#adding a test line
#library(plyr)
library(reshape2)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(viridis)
library(janitor)
library(lubridate)
library(httr)
library(stringi)
library(purrr)
library(scales)
library(zoo)
library(ggrepel)



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



 
#aeso site scraper

 get_metered_volumes_report <- function(start_date, end_date) {
   
   start_date <- as.Date(start_date)
   end_date <- as.Date(start_date)
   
   GET(
     url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/PublicSummaryAllReportServlet",
     query = list(
       beginDate = format(start_date, "%m%d%Y"),
       endDate = format(end_date, "%m%d%Y"),
       contentType = "csv"
     )
   ) -> res
   
   stop_for_status(res)
   
   # Neither the CSV nor HTML output is all that great but the CSV
   # can be made to work with (IMO) less effort than the HTML. You may
   # need to do some extra checks for data format (for either CSV or
   # HTML), though, in "production" mode.
   
   # From what I saw in the output, you likely need to modify 
   # this attempt at munging since the "hours" seem off, but you
   # at least now have the data.
   
   test<- content(res, as="text") %>% 
     stri_split_lines() %>% 
     flatten_chr()
   #headers<-paste(c(paste(test[8:9], collapse=",")), collapse="\n")
   #headers<- gsub("\"-\",", "", headers)
   data<-gsub("\"-\",", "",paste(c(paste(test[8:9], collapse=","), test[13:length(test)]), collapse="\n"))
   clean_data<-read.csv(text=data,header = TRUE, stringsAsFactors=FALSE)
   clean_data<-janitor::clean_names(clean_data)%>%   tbl_df()
   clean_data$date<-start_date
   
   #clean_data$he<-hour(clean_data$date)
   #clean_data$time<-clean_data$date
   #clean_data$date<-as.Date(clean_data$time)
   
   #names(clean_data)<-c(read.csv(text=headers,header = FALSE, stringsAsFactors=FALSE),"date")
   return(clean_data)
 }
 
get_all_data<-function() {
  years<-seq(2018,2018)
  
  for(year_id in years){
   days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
   days<-days[days<Sys.Date()-days(3)]
   data_store <- data.frame()
   list_item<-1
   for(day in days){
     print(as.Date(day))
     if(as.Date(day)<Sys.Date()-days(3))
       {
       xdf<-get_metered_volumes_report(as.Date(day), as.Date(day)+days(1))
       xdf<-melt(xdf,id.vars = c("pool_participant_id","asset_type","asset_id","date"),variable.name = "hour",value.name = "vol" )
       xdf$hour<-stri_pad(gsub("hour_","",xdf$hour), 2, pad = "0")
       xdf$time<-as.POSIXct(paste(xdf$date,xdf$hour,sep=" "),format="%Y-%m-%d %H")
       xdf$date<-as.Date(xdf$time,tz="America/Denver")
       xdf$hour<-hour(xdf$time)
       xdf$vol<-as.numeric(xdf$vol)
       data_store<-rbind(data_store,xdf)
       list_item<-list_item+1
       }
     }
   filename<-paste("measured_vols_",year_id,".RData",sep = "")
   save(data_store, file= filename) 
   #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
   #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
  }
}

#get_all_data()

#get and stack all available data

update_vols <- function() {
  filename<-paste("measured_vols_",year(Sys.Date()),".RData",sep = "")
  load(file= filename) 
  days<-seq.Date(as.Date(paste(year(Sys.Date()),"-01-01",sep="")),as.Date(paste(year(Sys.Date()),"-12-31",sep="")),by="1 day")
  days<-days[days<Sys.Date()-days(3)]
  days<-as.Date(days[days>max(data_store$date)])
  list_item<-1
  for(day in days){
    print(as.Date(day))
    if(as.Date(day)<as.Date(Sys.Date()-days(3)))
    {
      xdf<-get_metered_volumes_report(as.Date(day), as.Date(day)+days(1))
      xdf<-melt(xdf,id.vars = c("pool_participant_id","asset_type","asset_id","date"),variable.name = "hour",value.name = "vol" )
      xdf$hour<-stri_pad(gsub("hour_","",xdf$hour), 2, pad = "0")
      xdf$time<-as.POSIXct(paste(xdf$date,xdf$hour,sep=" "),format="%Y-%m-%d %H")
      xdf$date<-as.Date(xdf$time,tz="America/Denver")
      xdf$hour<-hour(xdf$time)
      xdf$vol<-as.numeric(xdf$vol)
      data_store<-rbind(data_store,xdf)
      list_item<-list_item+1
    }
  }
  save(data_store, file= filename) 
  }




#years<-seq(2004,2017)
#for(year_id in years){
#  print(paste("working on",year_id,sep=" "))
#  filename<-paste("measured_vols_",year_id,".RData",sep = "")
#  load(file= filename) 
#  data_store<-data_store[year(data_store$date)==year_id,]
  #xdf<-data_store
  #xdf$hour<-stri_pad(gsub("hour_","",xdf$hour), 2, pad = "0")
  #xdf$time<-as.POSIXlt(paste(xdf$date,xdf$hour,sep=" "),format="%Y-%m-%d %H")
  #xdf$date<-as.Date(xdf$time)
  #xdf$hour<-hour(xdf$time)
  #data_store<-xdf
#  data_store$time<-as.POSIXct(data_store$time)
#  save(data_store, file= filename) 
#  }



process_data <- function(data_sent) {
#function to process all AESO data into useful load and trade volumes
  #data_sent<-data_store
  include_list<-c("IMPORTER","IPP","EXPORTER","GENCO","SPP")
  clean<-filter(data_sent,asset_type %in% include_list)
  #load plant data
  aeso_ids <- read.xlsx(xlsxFile = "aeso_assets.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  #fix trade ids
  trade_list<-c("IMPORTER","EXPORTER")
  trade<-filter(clean,asset_type %in% trade_list)
  trade<-merge(trade,aeso_ids,by.x="asset_id",by.y="Asset.ID")
  trade$dest<-NA
  trade<-trade %>% mutate(dest = ifelse(grepl("BC",Asset.Name),"AB_BC",dest)) %>%
    mutate(dest = ifelse(grepl("MT",Asset.Name),"AB_MON",dest)) %>% 
    mutate(dest = ifelse(grepl("SK",Asset.Name),"AB_SK",dest)) %>% 
    mutate(dest = ifelse(grepl("Sask",Asset.Name),"AB_SK",dest))%>% 
    mutate(dest = ifelse(grepl("SPC",Asset.Name),"AB_SK",dest))%>% 
    mutate(dest = ifelse(grepl("XB",Asset.Name),"AB_BC",dest))%>% 
    mutate(dest = ifelse(grepl("PWX",Asset.Name),"AB_BC",dest))%>%
    mutate(vol = ifelse(grepl("IMPORTER",asset_type),vol,-vol))
  trade<- trade %>% group_by(time,date,hour,dest) %>% summarise(vol = sum(vol)) %>% ungroup()
  names(trade)[names(trade) == "dest"] <- "asset_id"
  trade$asset_type<-"TRADE"
  trade$pool_participant_id<-"AESO"
  trade<-data.frame(trade)
  clean2<-filter(clean,!asset_type %in% trade_list)
  clean2<-rbind(clean2,trade)
  #fix IDs
  clean2<-clean2 %>% mutate(asset_id = ifelse(grepl("CES1",asset_id),"CAL1",asset_id)) %>%
                    mutate(asset_id = ifelse(grepl("CES2",asset_id),"CAL1",asset_id)) %>%
                    mutate(asset_id = ifelse(grepl("DOW1",asset_id),"DOWG",asset_id))%>%
                    mutate(asset_id = ifelse(grepl("GEN1",asset_id),"GEN6",asset_id))%>%
                    mutate(asset_id = ifelse(grepl("GEN3",asset_id),"WCD1",asset_id))%>%
                    mutate(asset_id = ifelse(grepl("SCTG",asset_id),"APS1",asset_id))
  clean2<-merge(clean2,aeso_ids,by.x="asset_id",by.y="Asset.ID",all.x = TRUE)
  
  #bring in plant data
  plant_data <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  plant_info<-data.frame(t(plant_data[(1:12),-1]))
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:12),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  plant_info<-plant_info %>%
    mutate(NRG_Stream = ifelse(grepl("AB - H R Milner Hr Avg MW",NRG_Stream),"AB Milner Hr Avg MW",NRG_Stream))%>%
    mutate(NRG_Stream = ifelse(grepl("AB - NPC1 Denis St Pierre Hr Avg MW",NRG_Stream),"AB - NPC1 Denis St  Pierre Hr Avg MW",NRG_Stream))  
  plant_info<-arrange(plant_info,NRG_Stream)
  
  
  #bring in ghg data
  ghg_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "GHG_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  ghg_rates<-dcast(ghg_rates, formula = GHG_ID~...,value.var = "Poln_rate")
  #bring in heat rates
  heat_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Heat_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  #combine all plant info, heat rates, and GHGs by plant ID
  combined<-merge(plant_info,heat_rates, by = c("Aurora_ID"),suffixes = c(".aeso",".aurora"),all.x=TRUE) # NA's match
  combined<-merge(combined,ghg_rates, by = c("GHG_ID"),suffixes = c(".aeso",".aurora"),all.x=TRUE) # NA's match
  coal_co2_btu<-100.4  #coal fuel factor GHGs/MMBTU
  gas_co2_btu<-53.077752 #gas fuel factor GHGs/MMBTU
  combined$co2_est<-combined$CO2/2.20462*combined$Heat.Rate #convert to kg/mmbtu
  combined$co2_est<-ifelse(combined$Plant_Fuel=="COAL",coal_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-ifelse(combined$Plant_Fuel=="GAS",gas_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-combined$co2_est/1000 #adjust from kg to tonnes
  
  #now, deal with micro_generators and others
  combined_new<-merge(combined,clean2,by.x="ID",by.y="asset_id",all.y = TRUE)
  
  #need to fix this - the factor replacement isn't working here.
  combined_new<-combined_new %>% mutate(co2_est = ifelse(is.na(co2_est),0,co2_est)) 
  
  combined_new$Plant_Type<-as.character(combined_new$Plant_Type)
  combined_new$Plant_Type[is.na(combined_new$Plant_Type)]<-"MICRO"
  combined_new$Plant_Fuel<-as.character(combined_new$Plant_Fuel)
  combined_new$Plant_Fuel[is.na(combined_new$Plant_Fuel)]<-"OTHER"
  combined_new$Plant_Fuel<-as.factor(combined_new$Plant_Fuel)
  combined_new$Plant_Type<-as.factor(combined_new$Plant_Type)
  
  combined_new<-arrange(combined_new,time)
  
  #bring in prices and load information
  
  load("forecast_data.Rdata")
  
  forecast_data$day_ahead_forecasted_ail<- gsub("\\,", "", forecast_data$day_ahead_forecasted_ail)
  forecast_data$actual_ail<- gsub("\\,", "", forecast_data$actual_ail)
  forecast_data$day_ahead_forecasted_ail<-as.numeric(forecast_data$day_ahead_forecasted_ail)
  forecast_data$actual_ail<-as.numeric(forecast_data$actual_ail)
  
  
  #FIX FIX FIX FIX
  combined_new<-merge(combined_new,forecast_data,by=c("time","date"),all.x = TRUE)
  
  combined_new$month<-as.factor(month(combined_new$date))
  combined_new$year<-as.factor(year(combined_new$date))
  combined_new <- transform(combined_new, MonthAbb = month.abb[month])
  combined_new$MonthAbb<-factor(combined_new$MonthAbb,levels = month.abb)
  
  combined_new$ghg_hr<-combined_new$co2_est*combined_new$vol
  #                       mutate(Plant_Type = revalue(Plant_Type, c(NA = "OTHER")))
  return(combined_new)
}



 #load and price data
 
 
 get_forecast_report <- function(start_date, end_date) {
   
   start_date <- as.Date(start_date)
   end_date <- min(as.Date(start_date+months(1)),as.Date(end_date)) #31 days of data
   GET(
     url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/ActualForecastWMRQHReportServlet",
     query = list(
       beginDate = format(start_date, "%m%d%Y"),
       endDate = format(end_date, "%m%d%Y"),
       contentType = "csv"
     )
   ) -> res
   stop_for_status(res)
   test<- content(res, as="text") %>% 
     stri_split_lines() %>% 
     flatten_chr()
   test<-paste(test[5:length(test)], collapse="\n")
   forecast_data<-read.csv(text=test,header = TRUE, stringsAsFactors=FALSE)
   clean_data<-janitor::clean_names(forecast_data) %>% 
     tbl_df()
   #fixing here: need to retain the 02* hours
   clean_data$time<-as.POSIXct(clean_data$date, format="%m/%d/%Y %H")
   #clean_data$he<-hour(clean_data$date)
   #clean_data$time<-clean_data$date
   date_he<- do.call(rbind,strsplit(as.character(clean_data$date),' '))
   clean_data$he<-date_he[,2]
   clean_data$date<-as.Date(clean_data$time-hours(1),tz="America/Denver")
   #clean missing data out
   #clean_data<-clean_data[(clean_data$time<=Sys.time()+hours(3)),]
   
   #clean_data$date<-as.Date(strptime(clean_data$date, "%m/%d/%Y %h"))
   #build in R30 prices and loads?
   
   return(clean_data)
 }

 #day<-as.Date("2011-11-06",format="%Y-%m-%d")
 #xdf<-get_forecast_report(as.Date(day), as.Date(day)+months(1))
 
  

  
all_forecasts<-function() {
  years<-seq(2000,2018)
  data_store <- data.frame()
  for(year_id in years){
    days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 month")
    list_item<-1
    for(day in days){
      print(as.Date(day))
      if(as.Date(day)<Sys.Date()-days(2))
      {
        xdf<-get_forecast_report(as.Date(day), as.Date(day)+months(1))
        data_store<-rbind(data_store,xdf)
        list_item<-list_item+1
      }
    }
  }
  filename<-paste("forecast_data",".RData",sep = "")
  forecast_data<-data_store
  save(forecast_data, file= filename) 
}

#all_forecasts()
#load("forecast_data.Rdata")
#forecast_data$date<-as.Date(forecast_data$time,tz="America/Denver")
#save(forecast_data,file="forecast_data.Rdata")



update_forecasts<-function() {
  
  load("forecast_data.Rdata")
  start_date<-as.Date(paste(year(max(na.omit(forecast_data$date))),month(max(na.omit(forecast_data$date))),"01",sep ="-"))
  forecast_data<-forecast_data[forecast_data$date<start_date,] #trim data on record to since forecast data is in full days
  days<-seq.Date(start_date,as.Date(paste(year(Sys.Date()),"-12-31",sep="")),by="1 month")
    list_item<-1
    for(day in days){
      print(as.Date(day))
      if(as.Date(day)<Sys.Date()-days(2))
      {
        xdf<-get_forecast_report(as.Date(day), as.Date(day)+months(1))
        xdf$time<-as.POSIXct(paste(xdf$date," ",as.numeric(xdf$he),":00",sep=""),format="%Y-%m-%d %H:%M")
        forecast_data<-rbind(forecast_data,xdf)
        list_item<-list_item+1
      }
    }
    
  filename<-paste("forecast_data",".RData",sep = "")
  save(forecast_data, file= filename) 
}


#update_forecasts()

#load("forecast_data.Rdata")

#merit order stuff


get_merit_report <- function(start_date, end_date,key_firms=NULL) {
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(start_date) #it only takes a start_date so force it like this
  GET(
    url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/MeritOrderSnapshotEnergyReportServlet",
    query = list(
      beginDate = format(start_date, "%m%d%Y"),
      endDate = format(end_date, "%m%d%Y"),
      contentType = "csv"
    )
  ) -> res
  stop_for_status(res)
  test<- content(res, as="text") %>% 
    stri_split_lines() %>% 
    flatten_chr()
  test<-paste(test[3:length(test)], collapse="\n")
  merit_data<-read.csv(text=test,header = TRUE, stringsAsFactors=FALSE)
  clean_data<-janitor::clean_names(merit_data) %>% 
    tbl_df()
  if(!is.null(key_firms))
  {
    clean_data$key_firm<-grepl(paste(key_firms, collapse="|"), clean_data$offer_control)
    clean_data$offer_sum<-ifelse(clean_data$key_firm,clean_data$offer_control,"Other")
    for(firm in key_firms)
      clean_data$offer_sum[grep(firm,clean_data$offer_sum)]<-firm
    clean_data$offer_sum<-ordered(clean_data$offer_sum,levels=c(key_firms,"Other"))
  }
  else
    clean_data$offer_sum<-clean_data$offer_control
  clean_data$date<-as.Date(strptime(clean_data$date, "%m/%d/%Y"))
  clean_data<-arrange(clean_data,he,price,offer_sum)
  clean_data<-within(clean_data, {
    merit <- ave(size, clean_data$he, FUN = cumsum)})
  clean_data<-arrange(clean_data,he,-price,offer_sum)
  return(clean_data)
}

firms<-function(){
  return(c("Balancing Pool",
             "TransAlta",
             "ATCO",
             "ENMAX",
             "TransCanada",
             "Capital Power"))
}




all_merit<- function(){
years<-seq(2009,2009)
for(year_id in years){
  days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
  if(year_id=="2009")
      days<-seq.Date(as.Date(paste(year_id,"-09-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
  data_store <- data.frame()
  list_item<-1
  for(day in days){
    print(as.Date(day))
    if(as.Date(day)<Sys.Date()-days(60))
    {
      xdf<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms)
      data_store<-rbind(data_store,xdf)
      list_item<-list_item+1
    }
  }
  filename<-paste("merit_orders_",year_id,".RData",sep = "")
  save(data_store, file= filename) 
  #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
  #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
}


key_firms<-firms()

merit_data<-data.frame()
years<-seq(2009,2017)
for(year_id in years){
  filename<-paste("merit_orders_",year_id,".RData",sep = "")
  load(filename) ## which is here *equivalent* to
  merit_data<-rbind(merit_data,data_store)
  }
#add leading zeros to all he
singles<-seq(1,9)
for(hour in singles){
  merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
  }
}



update_merit <- function(data_sent) {
  data_sent<-merit_data
  days<-seq.Date(max(data_sent$date)+days(1),Sys.Date()-days(60),by="1 day")
  data_store <- data.frame()
  key_firms<-firms()
  list_item<-1
  for(day in days){
    print(as.Date(day))
    if(as.Date(day)<Sys.Date()-days(60))
    {
      xdf<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms)
      data_store<-rbind(data_store,xdf)
      list_item<-list_item+1
    }
  }
  return(data_store)
}

#xdf<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms)



wind_forecast<-function(){
  wind_fcast<-read.csv("http://ets.aeso.ca/Market/Reports/Manual/Operations/prodweb_reports/wind_power_forecast/WPF_LongTerm.csv",skip=4)
  wind_fcast<- wind_fcast[-seq(nrow(wind_fcast),nrow(wind_fcast)-1),]
  wind_fcast[,2]<-as.numeric(wind_fcast[,2])
  wind_fcast[,1]<-as.POSIXct(as.character(wind_fcast[,1]))
  names(wind_fcast)[1]<-"Date"
  wind_fcast
}
  
  


#AS merit order 

get_AS_merit_report <- function(start_date, end_date,key_firms=NULL) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(start_date) #it only takes a start_date so force it like this
  GET(
    url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/OperatingReserveOfferControlReportServlet",
    query = list(
      beginDate = format(start_date, "%m%d%Y"),
      endDate = format(end_date, "%m%d%Y"),
      contentType = "csv"
    )
  ) -> res
  stop_for_status(res)
  test<- content(res, as="text") %>% 
    stri_split_lines() %>% 
    flatten_chr()
  test<-paste(test[3:length(test)], collapse="\n")
  merit_data<-read.csv(text=test,header = TRUE, stringsAsFactors=FALSE)
  clean_data<-janitor::clean_names(merit_data) %>% 
    tbl_df()
  #key_firms<-firms()
  #if(!is.null(key_firms))
  #{
  #  clean_data$key_firm<-grepl(paste(key_firms, collapse="|"), clean_data$offer_control)
  #  clean_data$offer_sum<-ifelse(clean_data$key_firm,clean_data$offer_control,"Other")
  #  for(firm in key_firms)
  #    clean_data$offer_sum[grep(firm,clean_data$offer_sum)]<-firm
  #  clean_data$offer_sum<-ordered(clean_data$offer_sum,levels=c(key_firms,"Other"))
  #}
  #if(is.null(key_firms))
  #  clean_data$offer_sum<-clean_data$offer_control
  clean_data$date<-mdy(clean_data$date)
  return(clean_data)
}

#clean_data<-get_AS_merit_report(start_date,end_date)

all_as_merit<- function(){
  years<-seq(2013,2018)
  for(year_id in years){
    days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
    if(year_id=="2012")
      days<-seq.Date(as.Date(paste(year_id,"-10-04",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
    data_store <- data.frame()
    list_item<-1
    for(day in days){
      print(as.Date(day))
      if(as.Date(day)<Sys.Date()-days(60))
      {
        xdf<-get_AS_merit_report(as.Date(day),as.Date(day)+days(1))
        data_store<-rbind(data_store,xdf)
        list_item<-list_item+1
      }
    }
    filename<-paste("merit_AS_orders_",year_id,".RData",sep = "")
    save(data_store, file= filename) 
    #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
    #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
  }
}




year_as_merit<- function(year_id){
    year_id<-as.character(year_id)
    days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
    if(year_id=="2012")
      days<-seq.Date(as.Date(paste(year_id,"-10-04",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
    data_store <- data.frame()
    list_item<-1
    for(day in days){
      print(as.Date(day))
      if(as.Date(day)<Sys.Date()-days(60))
      {
        xdf<-get_AS_merit_report(as.Date(day),as.Date(day)+days(1))
        data_store<-rbind(data_store,xdf)
        list_item<-list_item+1
      }
    }
    filename<-paste("merit_AS_orders_",year_id,".RData",sep = "")
    save(data_store, file= filename) 
    #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
    #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
  }




merge_AS_merit<-function(){
merit_AS_data<-data.frame()
years<-seq(2013,2018)
for(year_id in years){
  filename<-paste("merit_AS_orders_",year_id,".RData",sep = "")
  load(filename) ## which is here *equivalent* to
  merit_AS_data<-rbind(merit_AS_data,data_store)
}
#add leading zeros to all he
singles<-seq(1,9)
for(hour in singles){
  merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
}
save(merit_AS_data, file= "merit_AS.RData") 

#filename<-paste("merit_AS.xlsx",sep = "")
#write.xlsx(merit_AS_data, file = filename, colNames = TRUE, borders = "columns") 
}

load(file="merit_AS.RData")


