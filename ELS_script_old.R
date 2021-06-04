#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/Alberta Power Data")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/Alberta Power Data")
print(getwd())

#base templates
if(!exists("ajl_hourly", mode="function")) source("../andrew_base.R")


#FORECASTS AND OTHER AESO SCRIPTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#seasons
if(!exists("getSeason", mode="function")) source("get_season.R")



#Load MERITS
update<-0
load("all_merit.RData")  
if(update!=0){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  save(merit_data, file="all_merit.RData")  
}



update_forecasts()

load("forecast_data.Rdata")

 #Load AS MERIT 
load(file="merit_AS.RData")
if(update!=0){
  merit_AS_data<-rbind(merit_AS_data,update_AS_merit(merit_AS_data))
  save(merit_AS_data, file="merit_AS.RData")  
}

#check to make sure singles are consistent
singles<-seq(1,9)
for(hour in singles){
  merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
  merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
}



#open AESO workbook
tightest_hours<-function(){
  data_list<-list()
  #read excel workbook
  for(year in seq(2013,2017)){
  hours<-read_excel("HistoricPerformance.xlsx", sheet = as.character(year),n_max=250)
  hours<- hours %>% clean_names() %>% select(local_date,capacity_year,calendar_year,supply_cushion_mw,rank)
  data_list[[year]]<-hours
  }
  hours<-data.frame(do.call(rbind,data_list))
  hours
}


augment_data<-function(merit_sent){
  
  #testing below this line
  #merit_sent<-merit_filter
  #bring in plant data
  plant_data <- read.xlsx(xlsxFile = "AB_Plant_Info.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  plant_info<-data.frame(t(plant_data[(1:12),-1]))
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:12),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  plant_info<-plant_info %>%
    mutate(NRG_Stream = ifelse(grepl("AB - H R Milner Hr Avg MW",NRG_Stream),"AB Milner Hr Avg MW",NRG_Stream))%>%
    mutate(NRG_Stream = ifelse(grepl("AB - NPC1 Denis St Pierre Hr Avg MW",NRG_Stream),"AB - NPC1 Denis St  Pierre Hr Avg MW",NRG_Stream))  
  plant_info<-arrange(plant_info,NRG_Stream)
  
  
  
  merit_sent<-merit_sent %>% group_by(date,he) %>%
    mutate(hourly_exports=sum((import_export=="E")*size),hourly_imports=sum((import_export=="I")*size))  %>% ungroup()
  
  merit_combo<-merge(merit_sent,plant_info,by.x="asset_id",by.y="ID",all.x = T)
  
  #ids I don't match are APXB CWXS EEXB EMXB EMXM MGXB MOXB MOXM PW20 SHXB SHXM SHXS SPBC SPXA TEE1
  
  #merit_combo<-arrange(merit_combo,time,merit)
  
  load("forecast_data.RData")
  forecast_data$day_ahead_forecasted_ail<-gsub(",","",forecast_data$day_ahead_forecasted_ail)
  forecast_data$actual_ail<-gsub(",","",forecast_data$actual_ail)
  forecast_data$forecasted_actual_ail_difference<-gsub(",","",forecast_data$forecasted_actual_ail_difference)
  forecast_data$Year<-year(forecast_data$date)
  forecast_data$actual_posted_pool_price<-as.numeric(forecast_data$actual_posted_pool_price)
  forecast_data$forecast_pool_price<-as.numeric(forecast_data$forecast_pool_price)
  forecast_data$actual_ail<-as.numeric(forecast_data$actual_ail)
  forecast_data$day_ahead_forecasted_ail<-as.numeric(forecast_data$day_ahead_forecasted_ail)
  
  merit_combo<-merge(merit_combo,forecast_data,by=c("date","he"))
  
  return(merit_combo)
}



augment_as_data<-function(merit_sent){
  
  #testing below this line
  #merit_sent<-merit_AS_filter
  #bring in plant data
  plant_data <- read.xlsx(xlsxFile = "AB_Plant_Info.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  plant_info<-data.frame(t(plant_data[(1:12),-1]))
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:12),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  plant_info<-plant_info %>%
    mutate(NRG_Stream = ifelse(grepl("AB - H R Milner Hr Avg MW",NRG_Stream),"AB Milner Hr Avg MW",NRG_Stream))%>%
    mutate(NRG_Stream = ifelse(grepl("AB - NPC1 Denis St Pierre Hr Avg MW",NRG_Stream),"AB - NPC1 Denis St  Pierre Hr Avg MW",NRG_Stream))  
  plant_info<-arrange(plant_info,NRG_Stream)
  
  
  merit_combo<-merge(merit_sent,plant_info,by.x="asset_id",by.y="ID",all.x = T)
  
  #ids I don't match are APXB CWXS EEXB EMXB EMXM MGXB MOXB MOXM PW20 SHXB SHXM SHXS SPBC SPXA TEE1
  
  #merit_combo<-arrange(merit_combo,time,merit)
  
  load("forecast_data.RData")
  forecast_data$day_ahead_forecasted_ail<-gsub(",","",forecast_data$day_ahead_forecasted_ail)
  forecast_data$actual_ail<-gsub(",","",forecast_data$actual_ail)
  forecast_data$forecasted_actual_ail_difference<-gsub(",","",forecast_data$forecasted_actual_ail_difference)
  forecast_data$Year<-year(forecast_data$date)
  forecast_data$actual_posted_pool_price<-as.numeric(forecast_data$actual_posted_pool_price)
  forecast_data$forecast_pool_price<-as.numeric(forecast_data$forecast_pool_price)
  forecast_data$actual_ail<-as.numeric(forecast_data$actual_ail)
  forecast_data$day_ahead_forecasted_ail<-as.numeric(forecast_data$day_ahead_forecasted_ail)
  
  merit_combo<-merge(merit_combo,forecast_data,by=c("date","he"))
  
  return(merit_combo)
}



hours<-tightest_hours()

#check to make sure leading zeros are in place consistently



merit_merge<-function(hours){
#get relevant merit order stuff

merit_filter<-merit_data %>%
    mutate(local_date=ymd_hm(paste(date," ",he,":00",sep=""))) %>% #create a new time
    filter(local_date %in% unique(hours$local_date)) %>%  #filter by times in tightest hours
    group_by(local_date,date,he,asset_id,import_export) %>% summarise(available_mw=sum(available_mw),dispatched_mw=sum(dispatched_mw),size=sum(size)) %>%
    ungroup() 

#merge in plant information
test<-augment_data(merit_filter)

#same thing with AS Merits
merit_AS_filter<-merit_AS_data %>%
  mutate(local_date=ymd_hm(paste(date," ",he,":00",sep=""))) %>% #create a new time
  filter(local_date %in% unique(hours$local_date) & commodity == "ACTIVE") %>%  #filter by times in tightest hours
  group_by(local_date,date,he,asset_id,product) %>% summarise(volume_mw=sum(volume_mw)) %>%
  ungroup() 
  
#the SUPL products are not linked to my asset types
#ids to update
#PWSR PWX Spinning Reserve	PWSR	Source	Active	Powerex Corp.	PWX
#"RESK" "PWSK" "REMT" "CAXB" "CAXS" "TEMT" "CRW1" "EPXM" "CWMT" "PWXM" "MOBC" "ECSK" "MGXM" "APMT"

test_AS<-augment_as_data(merit_AS_filter)

#write.xlsx(test, file = "calder_energy.xlsx", colNames = TRUE, borders = "columns")
#write.xlsx(test_AS, file = "calder_AS.xlsx", colNames = TRUE, borders = "columns")

merit_asset<-test %>% select(local_date,asset_id,Plant_Type,available_mw,dispatched_mw,size,Capacity)

#cast different AS variables into columns 

merit_as_asset<-test_AS %>% select(local_date,asset_id,Plant_Type,volume_mw,product) %>% 
  group_by(local_date,asset_id,Plant_Type) %>% mutate(as_total_mw=sum(volume_mw))%>% ungroup() %>%
  dcast(.,local_date+Plant_Type+asset_id~product,value.var = "volume_mw",sum)


#merged_test<-merge(merit_as_asset,merit_asset,by=c("local_date","asset_id","Plant_Type"),all = T)
merged_test<-full_join(merit_as_asset,merit_asset,by=c("local_date","asset_id","Plant_Type"))

#write.xlsx(merged_test, file = "calder_merged.xlsx", colNames = TRUE, borders = "columns")
merged_test
}

#tight_hours_merit<-merit_merge(hours)


merit_combine<-function(merit_AS_sent,forecast_sent,start_date=as.Date("2001-01-01"),end_date=as.Date("2001-01-01")){
  #for each hour, get energy price and each of the A/S prices from the A/S merit
  #testing lines
  #merit_AS_sent<-merit_AS_data %>% filter(date >= as.Date("2017-01-01") & date<= as.Date("2017-02-01"))
  #forecast_sent<-forecast_data %>% filter(date >= as.Date("2017-01-01") & date<= as.Date("2017-02-01"))
  #end testing lines
  
  #filter by date range
  merit_AS_sent<-merit_AS_sent %>% filter(date >= start_date & date<= end_date)
  #join forecast and actual energy prices to A/S merit by hour and asset
  merit_AS_sent<-left_join(merit_AS_sent,forecast_sent,c("date"="date","he"="he"))
  
  merit_AS_sent<-merit_AS_sent %>% 
    group_by(date,he,time,product,commodity,actual_ail,actual_posted_pool_price) %>% 
    summarize(price_mw=mean(price_mw),
              premium_price_mw=mean(premium_price_mw),
              activation_price_mw=mean(activation_price_mw))%>% ungroup()  %>%
    mutate(as_price=ifelse(commodity=="ACTIVE",price_mw+actual_posted_pool_price,
                           premium_price_mw))%>%
    dcast(.,time+date+he+actual_ail+actual_posted_pool_price~product+commodity,value.var = "as_price",mean) %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) 
  
  merit_AS_sent
}

#open ELS workbook
ELS<-function(){
  ELS_data<-read_excel("ELS_AJL_data.xlsx", sheet = 1)
  #ELS_data<- ELS_data %>% mutate(date=ymd) %>%
  #  group_by(date,month) %>% 
  #  summarise(gen=sum(generationEnergy/12),imps=sum(importedEnergy/12),cons=sum(consumptionEnergy/12),net=sum(net_to_grid/12)) %>%
  #  mutate(month=factor(month.abb[month], levels = month.abb))
  ELS_data
}


test<-merit_combine(merit_AS_data,forecast_data,start_date=as.Date("2017-01-01"),end_date=as.Date("2017-12-31"))
test<-test %>% mutate(Month=as.numeric(month(time)),Day=as.numeric(day(time)),`Hour Ending`=as.numeric(hour(time)+1))
#take out time change hours, add in month and day indicators
#test<-test %>% filter(he!="02*") %>% mutate(he=as.numeric(he),month=month(date),day=day(date))

#read in ELS data, take out second november 2am hour, merge
ELS_data<-ELS()
ELS_data$date_pair<-paste(ELS_data$Month,ELS_data$Day,ELS_data$`Hour Ending`,sep="-")
ELS_data<-ELS_data %>% filter(!duplicated(date_pair)) %>% select(-date_pair)
ELS_data<-left_join(test,ELS_data,by=c("Day","Month","Hour Ending"))

ELS_data<-ELS_data %>% mutate(net_from_grid=ELS_ALL-PVSyst)
#take out the time change hour in 2017

#code the ELS_battery


summary_data_store<-data.frame()


#call on battery
batt_size<-15000 #15MWh Battery
#for(batt_size in c(seq(0,15000,by=15000))){  #if you're going to comment this in, close the bracket at the end
  batt_size<-15000 #15MWh Battery
  batt_in<- batt_size/2  #max inflow
  batt_out<-batt_size/2  #max outflow
  init_charge<-0 #initial charge
  batt_eff<-.9 #round trip efficiency
  
  
  elec_price<-.05
  t_d_price<-.055
  
  ELS_data$batt_charge<-0
  ELS_data$batt_pwr<-0
  ELS_data$net_imp<-0
  
  
  
    ELS_data$batt_call<-ELS_data$net_from_grid
    
    ELS_data$batt_call<- ifelse(ELS_data$batt_call>batt_out,batt_out,ELS_data$batt_call)
    ELS_data$batt_call<- ifelse(ELS_data$batt_call< -1* batt_in,-batt_in,ELS_data$batt_call)
    
    
    # 
    rows_data<-nrow(ELS_data)
    for(row_num in seq(1,rows_data)){
      if(row_num==1)
      {
        ELS_data$batt_charge[row_num]<-max(0,min(init_charge-ELS_data$batt_call[row_num],batt_size))
        ELS_data$batt_pwr[row_num]<-ELS_data$batt_charge[row_num]-init_charge
        ELS_data$batt_pwr[row_num]<- ifelse(ELS_data$batt_pwr[row_num]<0,ELS_data$batt_pwr[row_num]*batt_eff,ELS_data$batt_pwr[row_num]/batt_eff)
      }
      else
      {
        ELS_data$batt_charge[row_num]<-max(0,min(ELS_data$batt_charge[row_num-1]-ELS_data$batt_call[row_num],batt_size))
        ELS_data$batt_pwr[row_num]<-ELS_data$batt_charge[row_num]-ELS_data$batt_charge[row_num-1]
        ELS_data$batt_pwr[row_num]<- ifelse(ELS_data$batt_pwr[row_num]<0,ELS_data$batt_pwr[row_num]*batt_eff,ELS_data$batt_pwr[row_num]/batt_eff)
      }
      ELS_data$net_imp[row_num]<-ELS_data$net_from_grid[row_num]+ELS_data$batt_pwr[row_num]
    }
    
    ELS_data$charges<-ifelse(ELS_data$net_imp>0,ELS_data$net_imp*(elec_price+t_d_price),ELS_data$net_imp*(elec_price))
    ELS_data$net_meter<-ELS_data$net_imp*(elec_price+t_d_price)
    ELS_data$bill<-ELS_data$ELS_ALL*(elec_price+t_d_price) #total ELS load times price
    ELS_data$td_paid<-ifelse(ELS_data$net_imp>0,ELS_data$net_imp*(t_d_price),0)
    ELS_data$td_nb<-ELS_data$net_imp*(t_d_price)
    ELS_data$td_base<-ELS_data$ELS_ALL*(t_d_price)
    #ELS_data$charges<-ifelse(ELS_data$net_imp>0,ELS_data$net_imp*(elec_price+t_d_price),ELS_data$net_imp*(elec_price))
    
    
    #check constraints
    #are any inflows or outflows above constraints?
    ELS_data$outflow_cons<-ifelse(ELS_data$batt_call>batt_out,batt_call-batt_out,0)
    ELS_data$inflow_cons<-ifelse(ELS_data$batt_call<-batt_in,batt_call-batt_out,0)
    
      ELS_data$batt_call<- ifelse(ELS_data$batt_call>batt_out,batt_out,ELS_data$batt_call)
    ELS_data$batt_call<- ifelse(ELS_data$batt_call< -1* batt_in,-batt_in,ELS_data$batt_call)
    
    
    
    
    
    print(paste("Battery size is ",batt_size," and annual imports are ",round(sum(ELS_data$net_imp[ELS_data$net_imp>=0]), digits=2)," Total billable variable costs are ",round(sum(ELS_data$charges),digits=2)))
    
    
    
    #batt size, imports, exports,bill w/o battery, bill w net bill, bill w net meter
    summary<-c(batt_size,sum(ELS_data$net_imp[ELS_data$net_imp>=0]),-sum(ELS_data$net_imp[ELS_data$net_imp<=0]),
               sum(ELS_data$charges),sum(ELS_data$net_meter),sum(ELS_data$bill),max(abs(ELS_data$net_imp)),
               max(abs(ELS_data$ELS_ALL)),max(abs(ELS_data$net_from_grid)),sum(ELS_data$td_paid),sum(ELS_data$td_nb),sum(ELS_data$td_base))
    summary_data_store<-rbind(summary_data_store,summary)
    names(summary_data_store)<-c("batt_size"," imports", "exports","bill_net_bill","bill_net_meter","bill_base",
                                 "peak_flow","peak_load","peak_flow_w_solar","t_d_net_bill","t_d_net_meter","t_d_base")
    #write.xlsx(ELS_data, file = paste("battery_size_",batt_size,".xlsx",sep=""), colNames = TRUE, borders = "columns")
    
#  }    
    
 





   #write.xlsx(ELS_data, file = paste("battery_size_",batt_size,".xlsx",sep=""), colNames = TRUE, borders = "columns")
    if(batt_size==13.5)
    {
      home_battery_graph(ymd("2018-05-01",tz="America/Denver"),batt_size)
      home_battery_graph(ymd("2017-12-01",tz="America/Denver"),batt_size)
    }
    









write.xlsx(test2, file = "values.xlsx", colNames = TRUE, borders = "columns")







#ELS_data$Date_test<-seq.POSIXt(as.POSIXct("2015-01-01 00:00"),as.POSIXct("2015-12-31 23:00"),by="1 hour")


#merge tight hours with ELSmith Data



