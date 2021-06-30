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
                             lag2 = lag(actual_posted_pool_price,2),
         day_ahead_forecasted_ail = poly(day_ahead_forecasted_ail,3),
                          cdd_YYC = poly(cdd_YYC,2),
                          hdd_YYC = poly(hdd_YYC,2),
                         month_he = interaction(month_fac,he,sep=":"),
                       weekday_he = interaction(wday_fac,he,sep=":")) %>%
  subset(select = c(actual_posted_pool_price,year,on_peak,stat,lag1,lag2,
                    day_ahead_forecasted_ail,cdd_YYC,hdd_YYC,month_he,weekday_he))

# Build a full model then test if variables can be removed using partial F test
full_model <- lm(actual_posted_pool_price~., data = lm_data)
anova0 <- anova(full_model)

# Reduced model 1: remove 1 hour lag of pool price
red_model1 <- lm(actual_posted_pool_price ~. -lag1, data = lm_data)
anova1 <- anova(red_model1,full_model)

# Reduced model 2: remove 2 hour lag of pool price
red_model2 <- lm(actual_posted_pool_price ~. -lag2, data = lm_data)
anova2 <- anova(red_model2,full_model)

# Reduced model 3: remove day ahead forecasted ail
red_model3 <- lm(actual_posted_pool_price ~. -day_ahead_forecasted_ail, data = lm_data)
anova3 <- anova(red_model3,full_model)

# Reduced model 4: remove stat holiday factor
red_model4 <- lm(actual_posted_pool_price ~. -stat, data = lm_data)
anova4 <- anova(red_model4,full_model)

# Reduced model 5: remove on/off peak factor
red_model5 <- lm(actual_posted_pool_price ~. -on_peak, data = lm_data)
anova5 <- anova(red_model5,full_model)

# Reduced model 6: remove cooling degree days
red_model6 <- lm(actual_posted_pool_price ~. -cdd_YYC, data = lm_data)
anova6 <- anova(red_model6,full_model)

# Reduced model 7: remove heating degree days
red_model7 <- lm(actual_posted_pool_price ~. -hdd_YYC, data = lm_data)
anova7 <- anova(red_model7,full_model)

# Reduced model 8: remove year
red_model8 <- lm(actual_posted_pool_price ~. -year, data = lm_data)
anova8 <- anova(red_model8,full_model)

# Reduced model 9: remove month-hour interaction
red_model9 <- lm(actual_posted_pool_price ~. -month_he, data = lm_data)
anova9 <- anova(red_model9,full_model)

# Reduced model 10: remove weekday/weekend-hour interaction
red_model10 <- lm(actual_posted_pool_price ~. -weekday_he, data = lm_data)
anova10 <- anova(red_model10,full_model)


coeffs<-tidy(full_model)
resid<-augment(full_model)


#resid<-resid%>% mutate(resid=actual_posted_pool_price-.fitted)

#x_reg<-data_set %>% select(day_ahead_forecasted_ail,year,month_fac,he,hdd_YEG,cdd_YEG)%>%
#  mutate(load2=day_ahead_forecasted_ail^2,year=factor(year),hdd2=hdd_YEG^2,cdd2=cdd_YEG^2)
#x_reg<-fastDummies::dummy_cols(x_reg)%>% select(-year,-month_fac,-year_2010,-month_fac_Jan,-he_01,-he)

#ar_fit<-arima(data_set$actual_posted_pool_price,order = c(2,0,0),xreg =x_reg)

#ar_fit<-arima(resid$resid,order = c(2,0,0))
#ar_model<-tidy(ar_fit)
#ar_glance<-glance(ar_fit)


#set.seed(456)
#ts.sim<-resid$.fitted[1:1000]+arima.sim(list(ar = ar_model$estimate[1:2]),n=1000,sd = ar_glance$sigma)#

#ts.sim<-resid$.fitted[1:1000]+arima.sim(list(ar = c(0.00000000000001,0.000000000000001)),n=1000,sd = ar_glance$sigma)


ts_test<-as_tibble(rnorm(nrow(resid)+500,0,sd=resid$.sigma[1])) #use 500 iterations to seed the processs
#ts_test<-ts_test %>% mutate(ar_val=lag(value,1)*ar_model$estimate[1]+lag(value,2)*ar_model$estimate[2]+value)

ts_test<-tail(ts_test,nrow(resid)) #trim those 500 seed values
ts_test<-ts_test %>% mutate(ar_val=resid$.fitted+value,
                            ar_val=pmax(ar_val,0),
                            ar_val=pmin(ar_val,1000)
                            ) #add in the fitted price values


ts.plot(ts_test$ar_val)
