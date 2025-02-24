#get weather info for power papers
source("power_paper_base.R")
hourly_data_pull<-function(stn_id,min_yr,max_yr,stn_string="NA"){
#hourly data
  #year_index<-2022
  #month_index<-12
  #testing
  #min_yr<-2019
  #max_yr<-2020
  #EIA
  #target_lat<-53.31
  #target_long<-113.61
  #stn_id<-closest_stn(stn_ids,min_yr,max_yr,target_lat,target_long,freq="hourly")
  #if(freq=="daily")
  #stn_id<-27793
  #stn_string<-"YYC"
  #print(paste("Getting EIA data"))
data_store <- list()
index_id<-1
for(year_index in seq(min_yr,max_yr)) {
  for(month_index in seq(1,12)) {
  
  url = paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=",stn_id,"&Year=",year_index,"&Month=",sprintf("%02d",month_index),"&timeframe=1",sep="")
  test<-read.csv(url, skip = 0)%>% clean_names() %>% 
                                          # select(long=longitude_x,lat=latitude_y,
                                          # stn="station_name",climate_id,
                                          # date_time="date_time_lst",
                                          # year,                
                                          # month,
                                          # day,
                                          # time="time_lst",
                                          # temp="temp_c")%>%
    I()
  data_store[[paste(month.abb[month_index],"-",year_index,sep = "")]]<-test
  #index_id<-index_id+1
}}
temp_data<-do.call("rbind", data_store)
temp_data$date_time<-as.POSIXct(as.character(temp_data$date_time),format='%Y-%m-%d %H:%M')
#names(temp_data)[grep("Dewpoint.Temp..?.C.",names(temp_data))]<-"Dew.Point.Deg.C"
#names(temp_data)[grep("Temp..?.C.",names(temp_data))]<-"Temp.Deg.C"
temp_data$stn_id<-stn_id
temp_data$stn<-stn_string
temp_data
}

blatch<-hourly_data_pull(27214,min_yr = 2019,max_yr = 2023)
save(blatch,file="blatch.Rdata")
#blatch<-hourly_data_pull(27214,min_yr = 2024,max_yr = 2024)


all_weather_data<-function(min_yr=2009,max_yr=2021){
  #min_yr<-2009
  #max_yr<-year(Sys.Date())
  
  #EIA
  target_lat<-53.31
  target_long<-113.61
  
  
  #stn_id<-closest_stn(stn_ids,min_yr,max_yr,target_lat,target_long,freq="hourly")
  #if(freq=="daily")
  stn_id<-27793
  print(paste("Getting EIA data"))
  eia_data<-hourly_data_pull(stn_id,min_yr,max_yr,"YEG")

  #blatchford
  stn_id<-27214
  print(paste("Getting Blatchford data"))
  blatchford_data<-hourly_data_pull(stn_id,min_yr,max_yr,"YEG")
  
    
  
  #yyc
  target_lat<-51.1215
  target_long<-114.0076
  #stn_id<-closest_stn(stn_ids,min_yr,max_yr,target_lat,target_long,freq)
  stn_id<-27211
  #if(freq=="daily")
  print(paste("Getting YYC data"))
  yyc_data<-hourly_data_pull(stn_id,min_yr,max_yr,"YYC")

  
  #yyc springbank
  target_lat<-51.1215
  target_long<-114.0076
  #stn_id<-closest_stn(stn_ids,min_yr,max_yr,target_lat,target_long,freq)
  stn_id<-52200
  #if(freq=="daily")
  print(paste("Getting YYC data"))
  yyc_sb_data<-hourly_data_pull(stn_id,min_yr,max_yr,"YYC")
  
    
  
  print(paste("Getting YMM data"))
  #ymm 56.6492? N, 111.2300?
  target_lat<-56.6492
  target_long<-111.2300
  #stn_id<-closest_stn(stn_ids,min_yr,max_yr,target_lat,target_long,freq)
  stn_id<-27216
  #if(freq=="daily")
  ymm_data<-hourly_data_pull(stn_id,min_yr,max_yr,"YMM")
  
  print(paste("Getting Cold Lake data"))
  stn_id<-2832
  #if(freq=="daily")
  cold_lake_data<-hourly_data_pull(stn_id,min_yr,max_yr,"YMM")
  
  
  
  #print(paste("Getting YQL data"))
  ##YQL 49.6300? N, 112.7892? W
  #target_lat<-49.63
  #target_long<-112.7892
  #stn_id<-closest_stn(stn_ids,min_yr,max_yr,target_lat,target_long,freq)
  #stn_id<-2265
  #if(freq=="daily")
  #yql_data<-hourly_data_pull(stn_id,min_yr,max_yr,"YQL")
  
  print(paste("Saving data"))
  temp_data<-rbind(eia_data,blatchford_data,yyc_data,yyc_sb_data,ymm_data,cold_lake_data)
  #AB_temps_data<-temp_data
  temp_data
}

#temp_data<-all_weather_data()
#save(temp_data, file="all_AB_temps.RData")  
make_power_data<-function(){
df1<-temp_data %>% group_by(date_time,year,month,day,time,stn) %>% summarize(temp=mean(temp,na.rm = T),
                                                                             #lat=mean(lat, na.rm = T),
                                                                             #long=mean(long,na.rm = T),
)%>%
  group_by(date_time,year,month,day,time,stn) %>%
  mutate(hdd=pmax(18-temp,0)/24,cdd=pmax(temp-18,0)/24)%>% ungroup()%>%
  filter(!is.na(temp))#%>% #take out if index temps are NA
#mutate(id=row_number())


temps_power<-df1 %>% pivot_wider(names_from = stn, values_from = c(temp,hdd,cdd))%>%
  filter(!is.na(temp_YYC))%>%
  filter(!is.na(temp_YEG))%>%
  filter(!is.na(temp_YMM))%>%
  left_join(forecast_data,by=c("date_time"="time")) %>%
  select(date,he,temp_YEG,temp_YMM,temp_YYC,hdd_YEG,hdd_YMM,hdd_YYC,cdd_YEG,cdd_YMM,cdd_YYC)%>%
  filter(!is.na(date))
save(temps_power,file="data/ab_power_temps.RData")  
}



update_weather_data<-function(data_sent){
  #testing data_sent<-temps_power
  #last year for which we have any data
  min_yr<-year(max(na.omit(data_sent$date)))
  #current year
  max_yr<-year(Sys.Date())
  data_sent<-data_sent%>% filter(year(date)<min_yr)%>% #filter out partial year
    bind_rows(
      all_weather_data(min_yr=min_yr,max_yr = max_yr)%>%
      group_by(date_time,year,month,day,time,stn) %>% summarize(temp=mean(temp,na.rm = T))%>%
      group_by(date_time,year,month,day,time,stn) %>%
      mutate(hdd=pmax(18-temp,0)/24,cdd=pmax(temp-18,0)/24)%>% ungroup()%>%
      filter(!is.na(temp))%>% #take out if index temps are NA
      pivot_wider(names_from = stn, values_from = c(temp,hdd,cdd))%>%
      filter(!is.na(temp_YYC))%>%
      filter(!is.na(temp_YEG))%>%
      filter(!is.na(temp_YMM))%>%
      left_join(forecast_data,by=c("date_time"="time")) %>% #use forecast data to get times, etc
      select(date,he,temp_YEG,temp_YMM,temp_YYC,hdd_YEG,hdd_YMM,hdd_YYC,cdd_YEG,cdd_YMM,cdd_YYC)%>%
      filter(!is.na(date)))
      #return updated data
      data_sent
  #testing
  # test<-all_weather_data(min_yr=min_yr,max_yr = max_yr)
}

#save(data_sent,file="ab_power_temps.RData")  





#testing_dates<-test %>% filter(is.na(date_time))%>% mutate(date_test=paste(date_test=year,"-",month,"-",day," ",time,sep=""))

#all_weather_data()
#temp_data<-update_weather_data()
#save(temp_data, file="all_AB_temps.RData")  

#load("all_AB_temps.RData")

