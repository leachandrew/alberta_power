#library(plyr)
library(dplyr)
library(openxlsx)
library(reshape2)
library(lubridate)

library(tidyr)
library(ggplot2)
library(httr)

library(viridis)
library(scales)

library(stringi)
library(janitor)


#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive")
print(getwd())


set_png<-function(file_sent,w_sent=1400,h_sent=750,res_sent=130){
  #MAC
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file=file_sent, width = w_sent, height = h_sent,res=res_sent)
  #PC
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file=file_sent, width = w_sent, height = h_sent,res=res_sent,type='cairo')
}

set_pdf<-function(file_sent,w_sent=1400,h_sent=750,res_sent=130){
  #MAC
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    pdf(file=file_sent, width = w_sent, height = h_sent,res=res_sent)
  #PC
  if(R.version$platform ==  "x86_64-w64-mingw32")
    pdf(file=file_sent, width = w_sent, height = h_sent,res=res_sent)
}



## Make breaks from a starting date at a given hour, occuring by interval,
## length.out is days
make_breaks <- function(strt, hour, interval="day", length.out=31) {
  strt <- as.POSIXlt(strt - 60*60*24)  # start back one day
  strt <- ISOdatetime(strt$year+1900L, strt$mon+1L, strt$mday, hour=hour, min=0, sec=0, tz="UTC")
  seq.POSIXt(strt, strt+(1+length.out)*60*60*24, by=interval)
}


if(!exists("get_metered_volumes_report", mode="function")) source("aeso_scrapes.R")


#METERED VOLUMES

update_vols()

#all_vols<-data_frame()
years<-seq(2004,2018)
for(year_id in years){
  filename<-paste("measured_vols_",year_id,".RData",sep = "")
  load(file= filename) 
  assign(paste("vols_",year_id,sep=""),data_store)
}

all_vols<-rbind(vols_2004,vols_2005,vols_2006,vols_2007,vols_2008,vols_2009,vols_2010,vols_2011,vols_2012,vols_2013,vols_2014,vols_2015,vols_2016,vols_2017,vols_2018)



#FORECASTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#forecast_data$time<-as.POSIXct(paste(forecast_data$date," ",as.numeric(forecast_data$he),":00",sep=""),format="%Y-%m-%d %H:%M")
filename<-paste("forecast_data",".RData",sep = "")
load(filename)
update_forecasts()
#hourly prices  
forecast_data$day_ahead_forecasted_ail<-gsub(",","",forecast_data$day_ahead_forecasted_ail)
forecast_data$actual_ail<-gsub(",","",forecast_data$actual_ail)
forecast_data$forecasted_actual_ail_difference<-gsub(",","",forecast_data$forecasted_actual_ail_difference)
forecast_data$Year<-year(forecast_data$date)
forecast_data$actual_posted_pool_price<-as.numeric(forecast_data$actual_posted_pool_price)
forecast_data$forecast_pool_price<-as.numeric(forecast_data$forecast_pool_price)
forecast_data$actual_ail<-as.numeric(forecast_data$actual_ail)
forecast_data$day_ahead_forecasted_ail<-as.numeric(forecast_data$day_ahead_forecasted_ail)
save(forecast_data, file= filename) 


if(!exists("get_merit_report", mode="function")) source("aeso_scrapes.R")

#MERITS

load("all_merit.RData")  
merit_data<-rbind(merit_data,update_merit(merit_data))
save(merit_data, file="all_merit.RData")  

