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
   
   test<- content(res, as="text") %>% 
     stri_split_lines() %>% 
     flatten_chr()
   #headers<-paste(c(paste(test[8:9], collapse=",")), collapse="\n")
   #headers<- gsub("\"-\",", "", headers)
   data<-gsub("\"-\",", "",paste(c(paste(test[8:9], collapse=","), test[13:length(test)]), collapse="\n"))
   clean_data<-read.csv(text=data,header = TRUE, stringsAsFactors=FALSE)
   clean_data<-janitor::clean_names(clean_data)%>%   as_tibble()
   clean_data$date<-start_date
   
   #clean_data$he<-hour(clean_data$date)
   #clean_data$time<-clean_data$date
   #clean_data$date<-as.Date(clean_data$time)
   
   #names(clean_data)<-c(read.csv(text=headers,header = FALSE, stringsAsFactors=FALSE),"date")
   return(clean_data)
 }
 

 clean_volume_data<-function(xdf){
   #tool to clean data from the metered volumes
   xdf<-xdf %>% pivot_longer(cols = - c("pool_participant_id","asset_type","asset_id","date"),names_to = "hour",values_to = "vol" )
   xdf$he<-stri_pad(gsub("hour_","",xdf$hour), 2, pad = "0")
   xdf$he<-gsub("2_2","02*",xdf$he) #match time change hour notation for forecast data he fields
   xdf$hour<-as.numeric(gsub("02\\*","02",xdf$he)) #create an actual numeric hour
   xdf$vol<-as.numeric(xdf$vol) 
   xdf
 }
 
get_all_data<-function() {
  years<-seq(2020,2020)
  for(year_id in years){
   print(paste("Starting year ",year_id))
   days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
   days<-days[days<Sys.Date()-days(3)]
   data_store <- data.frame()
   list_item<-1
   for(day in days){
     print(as.Date(day))
     if(as.Date(day)<Sys.Date()-days(3))
       {
       xdf<-get_metered_volumes_report(as.Date(day), as.Date(day)+days(1))
       data_store<-rbind(data_store,clean_volume_data(xdf))
       list_item<-list_item+1
       }
     }
   filename<-paste("data/measured_vols_",year_id,".RData",sep = "")
   save(data_store, file= filename) 
   #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
   #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
  }
}
#get_all_data()

#rebuild the data
build_all<-function(){
years<-seq(2004,2020)
data_list<-list()
i<-1
load("data/forecast_data.Rdata")
for(year_id in years){
  filename<-paste("measured_vols_",year_id,".RData",sep = "")
  print(filename)
  load(file= filename)
  #Need to process time and dates to have date,he,hour,dest
  data_list[[i]]<-process_data(data_store)
  i<-i+1
}
all_vols<-data.frame(do.call(rbind,data_list))
save(all_vols, file="data/metered_vols_data.Rdata" ) 
#save(all_vols, file="new_metered_vols_data.Rdata" ) 
}


#if you want to add the latest metered volumes data to the existing data set
update_vols <- function(data_sent) {
  #testing
  #  data_sent<-tail(all_vols,10000)
  #update forecast data and load into memory
  update_forecasts()  
  load("data/forecast_data.Rdata")
  #figure out where current data ends
  max_date<-max(data_sent$date)
  #truncate current data to include last full day
  data_sent<-data_sent %>% filter(date<max_date)
  #list of days since last full day
  days<-seq.Date(max_date,Sys.Date()-days(4),by="1 day")
  #testing
  #  days<-seq.Date(max_date,max_date+days(1),by="1 day")
  #shifted to 4 days lag here to correct potential for early day errors
  data_store<-data.frame()
  #list_item<-1
  #days<-head(days,3)
  for(day in days){
      print(as.Date(day))
      xdf<-get_metered_volumes_report(as.Date(day), as.Date(day)+days(1))
      data_store<-rbind(data_store,clean_volume_data(xdf))
      #list_item<-list_item+1
    }
  #return the data
  rbind(data_sent,process_data(data_store))
}

process_data <- function(data_sent) {
  #function to process all AESO data into useful load and trade volumes
  #testing
  #day<-ymd("2019-02-01")
  #xdf<-get_metered_volumes_report(as.Date(day), as.Date(day)+days(1)) %>% clean_volume_data()
  #data_sent<-xdf
  #data_sent<-get_metered_volumes_report(Sys.Date()-days(5), Sys.Date()-days(5)+days(1))%>% clean_volume_data()
  
  include_list<-c("IMPORTER","IPP","EXPORTER","GENCO","SPP")
  #take out retailers, microgen, but leave zero volume data to avoid errors later
  clean<-filter(data_sent,asset_type %in% include_list)
  #update the list of assets if you have an internet connection
  if(has_internet()){
    zz =
      readHTMLTable("http://ets.aeso.ca/ets_web/ip/Market/Reports/AssetListReportServlet?contentType=html",colClasses = "character",stringsAsFactors = FALSE)
    aeso_assets<-as.data.frame(rbind(zz)[2])
    save(aeso_assets,file="data/aeso_assets.Rdata")
  }
  #if you don't have internet, load the archived file
  if(-has_internet())
    load(file="data/aeso_assets.Rdata")
  
  colnames(aeso_assets)<- aeso_assets[1, ] # the first row will be the header
  aeso_assets<-clean_names(aeso_assets[-1,])
  zz<-NULL
  #fix trade ids
  trade_list<-c("IMPORTER","EXPORTER")
  #grab the trade assets
  trade<-filter(clean,asset_type %in% trade_list)
  trade<-trade %>% left_join(select(aeso_assets,-operating_status,-asset_type),by=c("asset_id","pool_participant_id"))
  trade$dest<-NA
  #combine by destination
  trade<-trade %>% mutate(dest = ifelse(grepl("BC",asset_name),"AB_BC",dest)) %>%
    mutate(dest = ifelse(grepl("MT",asset_name),"AB_MON",dest)) %>% 
    mutate(dest = ifelse(grepl("SK",asset_name),"AB_SK",dest)) %>% 
    mutate(dest = ifelse(grepl("Sask",asset_name),"AB_SK",dest))%>% 
    mutate(dest = ifelse(grepl("SPC",asset_name),"AB_SK",dest))%>% 
    mutate(dest = ifelse(grepl("XB",asset_name),"AB_BC",dest))%>% 
    mutate(dest = ifelse(grepl("PWX",asset_name),"AB_BC",dest))%>%
    mutate(vol = ifelse(grepl("IMPORTER",asset_type),vol,-vol))
  #group exports and imports by destination
  trade<- trade %>% group_by(date,he,hour,dest) %>% summarise(vol = sum(vol)) %>% filter(vol>0)%>%ungroup()
  #change the name of the "dest" column to be "asset_id, since it will then merge based on NRGStream data
  names(trade)[names(trade) == "dest"] <- "asset_id"
  trade$asset_type<-"TRADE"
  trade$pool_participant_id<-"AESO"
  #grab the non-trade assets and stack them onto the trade assets
  clean2<-filter(clean,!asset_type %in% trade_list)
  clean2<-rbind(clean2,trade)
  #fix IDs so that they match with AESO Supply and demand data.
  #Calgary Energy Centre, Dow, Scotford and Judy Creek
  #DOW - we have DOW1 and DOWG in the MV data and DOWG in supply and demand
  clean2<-clean2 %>% mutate(asset_id = ifelse(grepl("CES1",asset_id),"CAL1",asset_id)) %>% #calgrary energy center
    mutate(asset_id = ifelse(grepl("CES2",asset_id),"CAL1",asset_id)) %>% #calgrary energy center
    mutate(asset_id = ifelse(grepl("DOW1",asset_id),"DOWG",asset_id))%>% #dow
    mutate(asset_id = ifelse(grepl("GEN1",asset_id),"GEN6",asset_id))%>% #gen1 in asset id is judy creek (gen6)
    mutate(asset_id = ifelse(grepl("GEN3",asset_id),"WCD1",asset_id))%>% #gen3 is cadotte
    mutate(asset_id = ifelse(grepl("SCTG",asset_id),"APS1",asset_id)) #SCTG is sctoford, but it's shell-offered power
  #merge in asset names and pool participant ID combos
  clean2<-clean2 %>% left_join(select(aeso_assets,asset_name,asset_id,pool_participant_id),by=c("asset_id","pool_participant_id"))%>%
    #now we need to fix some of the joint marketing of power from assets
    group_by_at(vars(-vol,-pool_participant_id)) %>% arrange(pool_participant_id,-vol)%>% summarize(vol=sum(vol),pool_participant_id=first(pool_participant_id)) %>% ungroup()
  #bring in plant data
  plant_data <- read.xlsx(xlsxFile = "data/AB_Plant_Info_New.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  plant_info<-data.frame(t(plant_data[(1:10),-1]),stringsAsFactors = F)
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:10),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  
  plant_info<-plant_info %>%
    mutate(NRG_Stream = ifelse(grepl("AB - H R Milner Hr Avg MW",NRG_Stream),"AB Milner Hr Avg MW",NRG_Stream))%>%
    mutate(NRG_Stream = ifelse(grepl("AB - NPC1 Denis St Pierre Hr Avg MW",NRG_Stream),"AB - NPC1 Denis St  Pierre Hr Avg MW",NRG_Stream))  
  plant_info<-arrange(plant_info,NRG_Stream)
  
  #ids<-clean2%>% select(asset_id)%>%unique() %>% left_join(plant_info%>%select(ID,AESO_Name),by=c("asset_id"="ID"))
  
  #bring in ghg data
  ghg_rates <- read.xlsx(xlsxFile = "data/AB_Plant_Info_New.xlsx", sheet = "GHG_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  #ghg_rates<-dcast(ghg_rates, formula = GHG_ID ~ ...,value.var = "Poln_rate")
  ghg_rates<-ghg_rates %>% spread(Pollutant,Poln_rate) %>% select(GHG_ID,CO2)
  
  
  #bring in heat rates
  heat_rates <- read.xlsx(xlsxFile = "data/AB_Plant_Info_New.xlsx", sheet = "Heat_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE) %>%
    select(GHG_ID,Aurora_ID,Heat.Rate)
  #combine all plant info, heat rates, and GHGs by plant ID
  combined<-merge(ghg_rates,heat_rates,by="GHG_ID",all.y = T) # NA's match
  coal_co2_btu<-100.4  #coal fuel factor GHGs/MMBTU
  gas_co2_btu<-53.077752 #gas fuel factor GHGs/MMBTU
  combined<-merge(plant_info,combined,suffixes = c(".info",".rates"),all.x=TRUE,by="Aurora_ID") # NA's match
  combined$co2_est<-combined$CO2/2.20462*combined$Heat.Rate #convert to kg/mmbtu
  combined$co2_est<-ifelse(combined$Plant_Fuel=="COAL",coal_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-ifelse(combined$Plant_Fuel=="GAS",gas_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-combined$co2_est/1000 #adjust from kg to tonnes
  
  combined_new<-clean2%>%left_join(combined,by=c("asset_id"="ID"))
  
  #set those without co2_estimates to zero for now
  combined_new<-combined_new %>% mutate(co2_est = ifelse(is.na(co2_est),0,co2_est)) 
  
  #here, we're going to roll-up all of the small plants into one
  
  combined_new$Plant_Type[is.na(combined_new$Plant_Type)]<-"MICRO"
  combined_new$Plant_Fuel[is.na(combined_new$Plant_Fuel)]<-"OTHER"
  
  #create a sub-sample of the other generators
  other_gen<-combined_new %>% filter(Plant_Type=="MICRO")
  #roll up by time
  other_gen <-other_gen %>% group_by(date,he,Plant_Type,Plant_Fuel) %>% summarize(
    vol=mean(vol,na.rm = T),
    pool_participant_id="Pooled",
    asset_type="IPP",
    asset_id="IPP",
    #Asset.Name="Generic IPP",
    AESO_Name="Generic IPP",
    Capacity=NA,
    Latitude=NA,
    Longitude=NA,
    Aurora_ID="Generic IPP",
    Aurora_Name="Generic IPP",
    #Utility="Generic IPP",
    Heat.Rate=NA,
    #Heat.Rate.At.Minimum=NA,
    #Nameplate.Capacity=NA,
    #Fuel=NA,
    #AESO.Class=NA,
    #ID.Check=NA,
    GHG_ID="0",
    #HG=NA,
    #NOX=NA,
    #SO2=NA,
    co2_est=0,
    NRG_Stream=NA,
    #CO2=NA
  )
  #stack the other gens back into combined new
  combined_new<-combined_new %>% filter(Plant_Type!="MICRO") %>% bind_rows(other_gen)
  #sort everthing by time
  combined_new<-arrange(combined_new,date,hour)
  
  #bring in prices and load information
  
  combined_new<-combined_new%>%left_join(forecast_data,by=c("date","he"))
  
  combined_new$month<-month(combined_new$date)
  combined_new$year<-year(combined_new$date)
  combined_new$MonthAbb<-factor(month.abb[combined_new$month])
  
  combined_new$ghg_hr<-combined_new$co2_est*combined_new$vol
  return(combined_new)
}

#load and price data

get_forecast_report <- function(start_date,end_date) {
  #testing below here
    #start_date<-as.Date("2022-01-11",format="%Y-%m-%d")
    #end_date <- min(as.Date(start_date+months(1)),Sys.Date()-days(1)) #31 days of data
    #end testing - above should be commented if you're not testing
  
  start_date <- as.Date(start_date)
  end_date <- min(as.Date(start_date+months(1)),as.Date(end_date)) #31 days of data
  print(start_date)
  print(end_date)
  
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
    as_tibble()
  
  globals<<-clean_data
  #date formats from AESO are date and he as : 03/11/2018 01
  #process dates
  clean_data$time<-as.POSIXct(clean_data$date, format="%m/%d/%Y %H",tz="America/Denver")
  
  date_he<- do.call(rbind,strsplit(as.character(clean_data$date),' '))
  clean_data$he<-date_he[,2]
  #start_date here is report_date
  clean_data$start_date<-mdy(date_he[,1])
  #date at end of hour
  clean_data$date<-as_date(clean_data$time)
  clean_data$forecast_ail<- gsub("\\,", "", clean_data$forecast_ail)
  clean_data$actual_ail<- gsub("\\,", "", clean_data$actual_ail)
  #clean_data<-clean_data %>% select(-he) %>% mutate_if(is.character,as.numeric)
  #clean_data<-clean_data %>% select(-he)%>% mutate_if(is.integer,as.numeric)
  #set numeric columns to numeric
  clean_data[,c(2:6)]<-lapply(clean_data[,c(2:6)],as.numeric)
  return(clean_data)
}


#sample code to use this scraper
day<-as.Date(Sys.Date()-years(1))
xdf<-get_forecast_report(as.Date(day), as.Date(day)+months(1))

all_forecasts<-function() {
  years<-seq(2000,year(Sys.Date()))
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
  filename<-paste("data/forecast_data",".RData",sep = "")
  forecast_data<-data_store
  save(forecast_data, file= filename) 
}

#all_forecasts()
#load("forecast_data.Rdata")
#save(forecast_data, file= filename) 

update_forecasts<-function() {
  #load the existing file
  load("data/forecast_data.Rdata")
  #start on the first day of the first month for which you have data
  start_date<-as.Date(paste(year(max(na.omit(forecast_data$start_date))),month(max(na.omit(forecast_data$start_date))),"01",sep ="-"))
  #this is where you get a problem with the hour-ending vs hour-beginning because the last entry on each day is an hour ending the next day
  #find the last date for which you have a full day
  forecast_data<-forecast_data[forecast_data$start_date<start_date,] #trim data on record to since forecast data is in full days
  days<-seq.Date(start_date,as.Date(paste(year(Sys.Date()),"-12-31",sep="")),by="1 month")
  list_item<-1
  for(day in days){
    day<-as.Date(day)
    print(day)
    if(as.Date(day)<Sys.Date()-days(2))
    {
      xdf<-get_forecast_report(day, day+months(1))
      forecast_data<-rbind(forecast_data,xdf)
      list_item<-list_item+1
    }
  }
  filename<-paste("data/forecast_data",".RData",sep = "")
  save(forecast_data, file= filename) 
}

#code to use this scraper

forecast_data<-forecast_data%>%rename(#forecast_ail=day_ahead_forecasted_ail,
  forecast_ail_actual_ail_difference= forecast_actual_ail_difference)
#filename<-paste("data/forecast_data",".RData",sep = "")
#save(forecast_data, file= filename) 
#update_forecasts()
load("data/forecast_data.Rdata")



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
    as_tibble()
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
  years<-seq(2009,year(Sys.Date()))
  for(year_id in years){
    days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
    #we only have part of 2009 available
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
    filename<-paste("data/merit_orders_",year_id,".RData",sep = "")
    save(data_store, file= filename) 
    #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
    #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
  }
  
  
  key_firms<-firms()
  
  merit_data<-data.frame()
  years<-seq(2009,2019)
  for(year_id in years){
    filename<-paste("data/merit_orders_",year_id,".RData",sep = "")
    load(filename) ## which is here *equivalent* to
    merit_data<-rbind(merit_data,data_store)
  }
  #add leading zeros to all he
  singles<-seq(1,9)
  for(hour in as.character(singles)){
    merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
  }
  save(merit_data, file="data/all_merit.RData")  
}


#update merit order data based on data_sent
#done this way so you can add to a subset or to the whole data

update_merit <- function(data_sent) {
  #for testing
  #data_sent<-merit_data
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


#code to update merits

replace_merit_day<-function(data_sent,day=ymd("2013-11-28"))
{
  xdf<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms=firms())
  data_sent %>% filter(date!=day) %>% rbind(xdf) %>% arrange(date,he)
}



# load(file="all_merit.RData")
# merit_data<-rbind(merit_data,update_merit(merit_data))
# save(merit_data, file="all_merit.RData")  


#DDS merit order 

get_DDS_merit_report <- function(start_date, end_date,key_firms=NULL) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(start_date) #it only takes a start_date so force it like this
  GET(
    url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/MeritOrderSnapshotDdsReportServlet",
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
    as_tibble()
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



#day=ymd("2017-01-01")
#xdf<-get_DDS_merit_report(as.Date(day), as.Date(day)+days(1))


all_dds_merit<- function(){
  years<-seq(2009,2019)
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
        xdf<-get_DDS_merit_report(as.Date(day),as.Date(day)+days(1))
        data_store<-rbind(data_store,xdf)
        list_item<-list_item+1
      }
    }
    filename<-paste("data/merit_DDS_orders_",year_id,".RData",sep = "")
    save(data_store, file= filename) 
    #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
    #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
  }
}
#all_dds_merit()

update_DDS_merit <- function(data_sent) {
  #next line for testing
  #data_sent<-merit_AS_data
  days<-seq.Date(max(data_sent$date)+days(1),Sys.Date()-days(60),by="1 day")
  data_store <- data.frame()
  list_item<-1
  for(day in days){
    print(as.Date(day))
    if(as.Date(day)<Sys.Date()-days(60))
    {
      xdf<-get_DDS_merit_report(as.Date(day), as.Date(day)+days(1))
      data_store<-rbind(data_store,xdf)
      list_item<-list_item+1
    }
  }
  return(data_store)
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
    as_tibble()
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
  years<-seq(2018,2021)
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
    filename<-paste("data/merit_AS_orders_",year_id,".RData",sep = "")
    save(data_store, file= filename) 
    #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
    #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
  }
}

#all_as_merit()


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
    filename<-paste("data/merit_AS_orders_",year_id,".RData",sep = "")
    save(data_store, file= filename) 
    #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
    #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
  }




merge_AS_merit<-function(){
merit_AS_data<-data.frame()
years<-seq(2012,2018)
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
save(merit_AS_data, file= "data/merit_AS.RData") 

#filename<-paste("merit_AS.xlsx",sep = "")
#write.xlsx(merit_AS_data, file = filename, colNames = TRUE, borders = "columns") 
}








update_AS_merit <- function(data_sent) {
  #next line for testing
    #data_sent<-merit_AS_data
  days<-seq.Date(max(data_sent$date)+days(1),Sys.Date()-days(60),by="1 day")
  data_store <- data.frame()
  list_item<-1
  for(day in days){
    print(as.Date(day))
    if(as.Date(day)<Sys.Date()-days(60))
    {
      xdf<-get_AS_merit_report(as.Date(day), as.Date(day)+days(1))
      data_store<-rbind(data_store,xdf)
      list_item<-list_item+1
    }
  }
  return(data_store)
}

#xdf<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms)
 

#Intertie capacities

get_intertie_capacity_report<-function(start_date,end_date){ 
  #testing  
  #start_date<-"2018-01-01"
  #end_date<-"2019-12-31"
  #max date is today
if(ymd(end_date)>today())
  end_date<-today()
#max number of days is 400 days
if(as.numeric(ymd(end_date)-ymd(start_date))>=400)
  end_date<-as.character(ymd(start_date)+days(399))
#build url
url<-paste("http://itc.aeso.ca/itc/public/queryHistoricalIntertieReport.do?availableEffectiveDate=943279200000+1999-11-22+07%3A00%3A00+MST+%281999-11-22+14%3A00%3A00+GMT%29&availableExpiryDate=1582354800000+2020-02-22+00%3A00%3A00+MST+%282020-02-22+07%3A00%3A00+GMT%29&fileFormat=CSV&startDate=",start_date,"&endDate=",end_date,sep = "")  
#download data
download_indic<-download.file(url,"data/itc_temp.csv",mode="wb")
stop_for_status(download_indic)
#process data to build capability by hour and data
itc_data<-read.csv("data/itc_temp.csv",skip = 2,stringsAsFactors = F) %>% clean_names() %>%
  mutate(date=ymd(date),hour_ending=as.character(hour_ending)) %>% 
  rename("he"="hour_ending") %>%
  select(date,he,sk_import_capability,sk_export_capability,bc_export_capability,bc_import_capability,matl_export_capability,matl_import_capability,bc_matl_export_capability,bc_matl_import_capability)
itc_data
}


get_all_itc_data<-function(){
itc_store <- list()
index<-1
for(year in seq(2000,year(today()))){
  start_date<-paste(year,"01","01",sep="-")
  end_date<-paste(year,"12","31",sep="-")
  itc_store[[index]]<-get_intertie_capacity_report(start_date,end_date)
  index<-index+1
}  
itc_data<-data.frame(do.call(rbind,itc_store))
singles<-seq(1,9)
for(hour in singles){
  itc_data$he[itc_data$he==hour]<-paste(0,hour,sep="")
}  
itc_data$he[itc_data$he=="2*"]<-"02*"
save(itc_data, file= "data/aeso_itc_data.RData") 
}  

#get_all_itc_data()


update_itc_data<-function(){
  load(file= "data/aeso_itc_data.RData") 
  #find max date in file
  start_date<-max(itc_data$date)
  end_date<-today()
  itc_update<-get_intertie_capacity_report(start_date,end_date)
  #fix he characters
  singles<-seq(1,9)
  for(hour in singles){
    itc_update$he[itc_update$he==hour]<-paste(0,hour,sep="")
  }  
  itc_data$he[itc_update$he=="2*"]<-"02*"
  #take out today's last day obs from itc data, append updated data
  itc_data<-itc_data %>% filter(date<ymd(start_date)) %>% bind_rows(itc_update)
  save(itc_data, file= "data/aeso_itc_data.RData") 
}  




wind_forecast<-function(){
  wind_fcast<-read.csv("http://ets.aeso.ca/Market/Reports/Manual/Operations/prodweb_reports/wind_power_forecast/WPF_LongTerm.csv",skip=4)
  wind_fcast<- wind_fcast[-seq(nrow(wind_fcast),nrow(wind_fcast)-1),]
  wind_fcast[,2]<-as.numeric(as.character(wind_fcast[,2]))
  wind_fcast[,1]<-as.POSIXct(as.character(wind_fcast[,1]))
  names(wind_fcast)[1]<-"Date"
  wind_fcast
}


