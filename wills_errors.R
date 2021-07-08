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
  mutate(wday_fac = ifelse(wday %in% c("Sat","Sun"),"Weekend","Weekday")) %>%
  group_by(month,he) %>%
  mutate(mean_price = mean(actual_posted_pool_price))

# Take hourly price time series and decompose into trend, season, and random error

# Original data
price_observed <- data_set$actual_posted_pool_price

# Original data transformed - log(x+1) root transformation  
price_observed_transformed <- log(price_observed+1)

# Trend as moving average (8760 hours)
lags <- 8760
fltr <- c(0.5,rep(1,times=lags-1),0.5)/lags
price_trend_transformed <- stats::filter(price_observed_transformed,filter=fltr,method="convolution",sides=2)
price_trend <- exp(price_trend_transformed)-1

# Sesonality using monthly mean price
mean_price_transformed <- log(data_set$mean_price+1)
price_season_transformed <- mean_price_transformed - mean(mean_price_transformed)
price_season <- exp(mean_price_transformed)-1

# Random errors over time
price_error_transformed <- price_observed_transformed - price_trend_transformed - price_season_transformed
price_error <- exp(price_error_transformed)-1

# plot observed, trend, season, and error (transformed)
plot(cbind(price_observed_transformed,price_trend_transformed,
           price_season_transformed,price_error_transformed),
     main = "", yax.flip = TRUE)

# plot observed, trend, season, and error ($/MWh)
plot(cbind(price_observed,price_trend,price_season,price_error),
     main = "", yax.flip = TRUE)

# use auto arima to generate arima model of errors
error_arima <- forecast::auto.arima(price_error_transformed)

# simulate errors using arima.sim
random_error <- arima.sim(n = 8760, list(ar = c(error_arima$coef[["ar1"]],
                                               error_arima$coef[["ar2"]]),
                                        ma = c(error_arima$coef[["ma1"]],
                                               error_arima$coef[["ma2"]],
                                               error_arima$coef[["ma3"]],
                                               error_arima$coef[["ma4"]]),
                                        sd = error_arima$sigma2)) %>% exp()-1
ts.plot(random_error)

ts.plot(price_season[1:1000])

#adjust for both data and forecasts for observeables

# Build data table of only predictor variables
lm_data <- data_set %>%
  mutate(                      he = factor(he),
                         wday_fac = factor(wday_fac),
                             year = factor(year),
                          on_peak = factor(on_peak),
                             stat = factor(stat),
                             #lag1 = lag(actual_posted_pool_price,1),
                             #lag2 = lag(actual_posted_pool_price,2),
         day_ahead_forecasted_ail = poly(day_ahead_forecasted_ail,3),
                          cdd_YYC = poly(cdd_YYC,2),
                          hdd_YYC = poly(hdd_YYC,2),
                         month_he = interaction(month_fac,he,sep=":"),
                       weekday_he = interaction(wday_fac,he,sep=":")) %>%
  subset(select = c(actual_posted_pool_price,year,on_peak,stat,#lag1,lag2,
                    day_ahead_forecasted_ail,cdd_YYC,hdd_YYC,month_he,weekday_he))

# Build a full model then test if variables can be removed using partial F test
full_model <- lm(actual_posted_pool_price~., data = lm_data)
anova0 <- anova(full_model)
sum0 <- summary(full_model)

# Reduced model 1: remove 1 hour lag of pool price
#red_model1 <- lm(actual_posted_pool_price ~. -lag1, data = lm_data)
#anova1 <- anova(red_model1,full_model)
#sum1 <- summary(red_model1)

# Reduced model 2: remove 2 hour lag of pool price
#red_model2 <- lm(actual_posted_pool_price ~. -lag2, data = lm_data)
#anova2 <- anova(red_model2,full_model)
#sum2 <- summary(red_model2)

# Reduced model 3: remove day ahead forecasted ail
#red_model3 <- lm(actual_posted_pool_price ~. -day_ahead_forecasted_ail, data = lm_data)
#anova3 <- anova(red_model3,full_model)
#sum3 <- summary(red_model3)

# Reduced model 4: remove stat holiday factor
#red_model4 <- lm(actual_posted_pool_price ~. -stat, data = lm_data)
#anova4 <- anova(red_model4,full_model)
#sum4 <- summary(red_model4)

# Reduced model 5: remove on/off peak factor
#red_model5 <- lm(actual_posted_pool_price ~. -on_peak, data = lm_data)
#anova5 <- anova(red_model5,full_model)
#sum5 <- summary(red_model5)

# Reduced model 6: remove cooling degree days
#red_model6 <- lm(actual_posted_pool_price ~. -cdd_YYC, data = lm_data)
#anova6 <- anova(red_model6,full_model)
#sum6 <- summary(red_model6)

# Reduced model 7: remove heating degree days
#red_model7 <- lm(actual_posted_pool_price ~. -hdd_YYC, data = lm_data)
#anova7 <- anova(red_model7,full_model)
#sum7 <- summary(red_model7)

# Reduced model 8: remove year
#red_model8 <- lm(actual_posted_pool_price ~. -year, data = lm_data)
#anova8 <- anova(red_model8,full_model)
#sum8 <- summary(red_model8)

# Reduced model 9: remove month-hour interaction
#red_model9 <- lm(actual_posted_pool_price ~. -month_he, data = lm_data)
#anova9 <- anova(red_model9,full_model)
#sum9 <- summary(red_model9)

# Reduced model 10: remove weekday/weekend-hour interaction
#red_model10 <- lm(actual_posted_pool_price ~. -weekday_he, data = lm_data)
#anova10 <- anova(red_model10,full_model)
#sum10 <- summary(red_model10)

# Reduced model 11: remove stat holiday & on/off peak
#red_model11 <- lm(actual_posted_pool_price ~. -on_peak -stat, data = lm_data)
#anova11 <- anova(red_model11,full_model)
#sum11 <- summary(red_model11)

# Reduced model 12: remove stat holiday, on/off peak, & lag2
#red_model12 <- lm(actual_posted_pool_price ~. -on_peak -stat -lag2, data = lm_data)
#anova12 <- anova(red_model12,full_model)
#sum12 <- summary(red_model12)

# Reduced model 13: remove stat holiday, on/off peak, lag2, & day ahead forecasted ail
#red_model13 <- lm(actual_posted_pool_price ~. -on_peak -stat -lag2 -day_ahead_forecasted_ail, data = lm_data)
#anova13 <- anova(red_model13,full_model)
#sum13 <- summary(red_model13)

# Reduced model 14: remove stat holiday, on/off peak, lag2, day ahead forecasted ail, weekday/weekend - he interaction
#red_model14 <- lm(actual_posted_pool_price ~. -on_peak -stat -lag2 -day_ahead_forecasted_ail -weekday_he, data = lm_data)
#anova14 <- anova(red_model14,full_model)
#sum14 <- summary(red_model14)

# Get the fitted price using first linear model and add it to a second regression
#coeffs<-tidy(full_model)
resid<-augment(full_model) 

lm_data2 <- as_tibble(resid$.fitted) %>% mutate(lag1 = lag(resid$.fitted))

full_model2 <- lm(value ~., data = lm_data2)
anova1 <- anova(full_model2)
sum1 <- summary(full_model2)
resid1 <- augment(full_model2)
coeffs1 <-tidy(full_model2)

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

comb.ts <- cbind(resid$actual_posted_pool_price,ts_test$ar_val)

ts.plot(comb.ts, gpars = list(col = c("black","green","red")))
legend("topleft",
       bty="n",
       lty=c(1, 1, 1, 1),
       lwd=c(2, 2, 2, 2),
       c("actual posted price","linear model","linear model + ARIMA error"),
       col = c("black","green","red"))
