#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")

#PC ***Change user before running***
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/sdegroot/Google Drive/alberta_power")
print(getwd())

#base templates
if(!exists("ajl_hourly", mode="function")) source("../andrew_base.R")


#FORECASTS AND OTHER AESO SCRIPTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#seasons
if(!exists("getSeason", mode="function")) source("get_season.R")



#Load MERITS
update<-1
load("all_merit.RData")  
if(update!=0){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  save(merit_data, file="all_merit.RData")  
}




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

tight_hours_merit<-merit_merge(hours)








#open ELS workbook
ELS<-function(){
  ELS_data<-read_excel("ELS_AJL_data.xlsx", sheet = 1)
  #ELS_data<- ELS_data %>% mutate(date=ymd) %>%
  #  group_by(date,month) %>% 
  #  summarise(gen=sum(generationEnergy/12),imps=sum(importedEnergy/12),cons=sum(consumptionEnergy/12),net=sum(net_to_grid/12)) %>%
  #  mutate(month=factor(month.abb[month], levels = month.abb))
  ELS_data
}

ELS_data<-ELS()



