source("power_paper_base.R")
source("cdn_weather.R")
source("aeso_scrapes.R")

update_forecasts()
load("data/forecast_data.Rdata")

load("data/ab_power_temps.Rdata")


data_set<-forecast_data %>% filter(time>=ymd("2010-01-1"))%>%assign_date_time_days() %>% assign_peaks() %>% left_join(temps_power)%>%
  filter(!is.na(hdd_YEG),!is.na(cdd_YEG),!is.na(actual_posted_pool_price),he!="02*")


#adjust for both data and forecasts for observeables

price_model <- lm(actual_posted_pool_price ~ poly(day_ahead_forecasted_ail,3) +factor(he)*month_fac + factor(year)+factor(on_peak)+poly(cdd_YEG,2)+poly(hdd_YEG,2), data = data_set)
coeffs<-tidy(price_model)
resid<-augment(price_model)

resid<-resid%>% mutate(resid=actual_posted_pool_price-.fitted)

#x_reg<-data_set %>% select(day_ahead_forecasted_ail,year,month_fac,he,hdd_YEG,cdd_YEG)%>%
#  mutate(load2=day_ahead_forecasted_ail^2,year=factor(year),hdd2=hdd_YEG^2,cdd2=cdd_YEG^2)
#x_reg<-fastDummies::dummy_cols(x_reg)%>% select(-year,-month_fac,-year_2010,-month_fac_Jan,-he_01,-he)

#ar_fit<-arima(data_set$actual_posted_pool_price,order = c(2,0,0),xreg =x_reg)

ar_fit<-arima(resid$resid,order = c(2,0,0))
tidy(ar_fit)
glance(ar_fit)

