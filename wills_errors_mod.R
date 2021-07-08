source("power_paper_base.R")
source("cdn_weather.R")
source("aeso_scrapes.R")

update_forecasts()
load("data/forecast_data.Rdata")

load("data/ab_power_temps.Rdata")


data_set<-forecast_data %>% 
  filter(time>=ymd("2010-01-1")) %>%
  assign_date_time_days() %>%
  assign_peaks() %>% 
  left_join(temps_power) %>%
  filter(!is.na(hdd_YEG),!is.na(cdd_YEG),!is.na(actual_posted_pool_price),he!="02*") %>%
  mutate(wday_fac = ifelse(wday %in% c("Sat","Sun"),"Weekend","Weekday"))


#adjust for both data and forecasts for observeables

# Build data table of only predictor variables
lm_data <- data_set %>%
  mutate(                      he = factor(he),
                         wday_fac = factor(wday_fac),
                             year = factor(year),
                          on_peak = factor(on_peak),
                             stat = factor(stat),
                             lag1 = lag(actual_posted_pool_price,1),
                             #lag2 = lag(actual_posted_pool_price,2),
         day_ahead_forecasted_ail = poly(day_ahead_forecasted_ail,3),
                          cdd_YYC = poly(cdd_YYC,2),
                          hdd_YYC = poly(hdd_YYC,2),
                         month_he = interaction(month_fac,he,sep=":"),
                       weekday_he = interaction(wday_fac,he,sep=":")) %>%
  select(actual_posted_pool_price,year,on_peak,stat,lag1,#lag2,
                    day_ahead_forecasted_ail,cdd_YYC,hdd_YYC,month_he,weekday_he)

# Build a full model then test if variables can be removed using partial F test
full_model <- lm(actual_posted_pool_price~., data = lm_data)
anova0 <- anova(full_model)
sum0 <- summary(full_model)

# Get the fitted price using first linear model and add it to a second regression
coeffs<-tidy(full_model)
resid<-augment(full_model) 
glanced<-glance(full_model) 


#get ar_term

ar_price<-coeffs$estimate[grep("lag",coeffs$term)]

sigma<-glanced$sigma


                 

#ar_fit<-arima(data_set$actual_posted_pool_price,order = c(2,0,0),xreg =x_reg)

#ar_fit<-arima(resid$resid,order = c(2,0,0))
#ar_model<-tidy(ar_fit)
#ar_glance<-glance(ar_fit)


ar_p<-arima.sim(list(ar = ar_price),n=nrow(resid)+50,sd = sigma) %>% tail(-50) %>% as_tibble()




resid<-resid %>% bind_cols(ar_p %>% rename(ar_p=value))%>% mutate(strip_ar=.fitted-ar_price*lag1, replace_ar=strip_ar+ar_p,ar_only=ar_price*lag1+.resid)

resid<-resid%>% mutate(hour=row_number())
  
ar_fit<-arima(resid$ar_only,order = c(1,0,0))
ar_model<-tidy(ar_fit)
ar_glance<-glance(ar_fit)
  
sim_ar<-arima.sim(list(ar = ar_model$estimate[1]),n=NROW(resid),sd = ar_glance$sigma) %>% as_tibble()%>%
  mutate(hour=row_number())

ggplot(resid) +geom_line(aes(hour,ar_only,color="original"))+
  geom_line(aes(hour,.fitted,color="ar1 fits"))+
  geom_line(data=sim_ar,aes(hour,x,color="ar1 sims"))
  


coeffs2<-tidy(test_model)

  
  

df<-resid %>% select(actual_posted_pool_price)%>% as_tibble() %>% mutate(hour=row_number()) 


new_data<-lm_data %>% bind_cols(resid %>% select(replace_ar) %>% add_row(replace_ar=NA,.before=1))%>%
  select(-actual_posted_pool_price) %>% mutate(lag1=lag(replace_ar,1))


#now let's check to see if the sim data ives the same results

test_model <- lm(replace_ar~., data = new_data)

coeffs2<-tidy(test_model)

coeff_compare<-bind_cols(coeffs%>% select(est_orig=estimate),coeffs2%>% select(est_sim=estimate))


df<-resid %>% head(100) %>% ungroup() %>% as_tibble() %>% mutate(hour=row_number())

ggplot(df) +geom_line(aes(hour,actual_posted_pool_price,color="original"))+
  geom_line(aes(hour,.fitted,color="ar1 fits"))+
  geom_line(aes(hour,strip_ar,color="stripped ar out"))+
  geom_line(aes(hour,replace_ar,color="replace ar"))




resid %>% ggplot() +
  geom_line(aes(y=actual_posted_pool_price),color="dodgerblue")
#set.seed(456)
#tgs.sim<-resid$.fitted[1:1000]+arima.sim(list(ar = ar_model$estimate[1:2]),n=1000,sd = ar_glance$sigma)#

#ts.sim<-resid$.fitted[1:1000]+arima.sim(list(ar = c(0.00000000000001,0.000000000000001)),n=1000,sd = ar_glance$sigma)

ts_test<-as_tibble(rnorm(nrow(resid1)+500,0,sd=sum1$sigma)) #use 500 iterations to seed the processs

ts_test<-tail(ts_test,nrow(resid1)) #trim those 500 seed values
ts_test<-ts_test %>% mutate(ar_val=resid1$.fitted+value) #add in the fitted price values

# loop forward accounting for shocks
for (timestep in 2:nrow(ts_test)){
  ts_test$ar_val[timestep] <- ts_test$ar_val[timestep]
                            + ts_test$ar_val[timestep-1] 
                            + ts_test$value[timestep]
}

ts_test <- ts_test %>% mutate(ar_val = pmax(ar_val,0),
                              ar_val = pmin(ar_val,1000))

comb.ts <- cbind(resid$actual_posted_pool_price[1000:1168],ts_test$ar_val[1000:1168])

ts.plot(comb.ts, gpars = list(col = c("black","green","red")))
legend("topleft",
       bty="n",
       lty=c(1, 1, 1, 1),
       lwd=c(2, 2, 2, 2),
       c("actual posted price","linear model","linear model + ARIMA error"),
       col = c("black","green","red"))
