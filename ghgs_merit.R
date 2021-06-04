#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive")
print(getwd())

#FORECASTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#seasons
if(!exists("getSeason", mode="function")) source("get_season.R")

#seasons
if(!exists("ajl_hourly", mode="function")) source("andrew_base.R")


#Load MERITS
update<-1
load("all_merit.RData")  
if(update!=0){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  save(merit_data, file="all_merit.RData")  
}


#check to make sure singles are consistent
singles<-seq(1,9)
for(hour in singles){
  merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
}


#augment_data<-function(merit_sent){
  
  #testing below this line
  merit_sent<-merit_data %>% filter(year(date)==2018)
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
  aeso_ids <- read.xlsx(xlsxFile = "aeso_assets.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  
  merit_combo<-left_join(merit_sent,aeso_ids,by=c("asset_id"="Asset.ID"))

  merit_combo<-left_join(merit_combo,combined,by=c("asset_id"="ID"))
  #ids I don't match are APXB CWXS EEXB EMXB EMXM MGXB MOXB MOXM PW20 SHXB SHXM SHXS SPBC SPXA TEE1
  

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
#}



days<-seq.Date(ymd("2018-04-10"),ymd("2018-04-10"),"day")

merit_filter<-function(days){
#get relevant merit order stuff

merit_filter<-merit_data %>% filter(date %in% days & he=="06")%>%
    ungroup() 

#merge in plant information
test<-augment_data(merit_filter)

sger_rates <- read.xlsx(xlsxFile = "power_ghgs.xlsx", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
sger_combo<-merge(test,sger_rates,by.y="ID",by.x="asset_id")
#sger_combo$co2_est<-sger_combo$co2_est/1000
sger_combo<-sger_combo[,c("asset_id","price","from","to","size","AESO_Name","Plant_Type","GHG/MWh","SGER_EI","BEI.(in.CR)")]

sger_combo <-sger_combo %>% mutate(oba_fix=37,oba_80pct=.8*`GHG/MWh`,oba_type=ifelse(Plant_Type=="COAL",.800,.420)) %>%
  mutate(vc_net_oba=price-oba_fix,vc_oba_80=vc_net_oba+oba_80pct,vc_oba_type=vc_net_oba+oba_type)
   


df1<-sger_combo%>% arrange(price,Plant_Type) %>% mutate(merit=cumsum(size))

p<-ggplot(df1) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-20,ymax=price,fill=Plant_Type))+
  ggtitle("Alberta Merit Order")+
  scale_fill_manual("Plant\nType",values = colors_tableau10())+
  #scale_fill_viridis("Plant\nType",discrete = T)+
  scale_colour_manual("Market\noutcomes",values=colors_tableau10_light())+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,13750,3000),limits = c(0,13751))+
  #scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,250),limits=c(-20,1001))+
  #scale_y_continuous(expand=c(0,0),breaks=seq(0,150,50),limits=c(-20,151))+
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))+
  #scale_x_date() +
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  #theme_minimal()+
  theme(
    legend.position = "right",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, ",max(df1$date)," ",max(df1$he),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")






merit_asset<-test %>% select(local_date,asset_id,Plant_Type,available_mw,dispatched_mw,size,Capacity)

#write.xlsx(merged_test, file = "calder_merged.xlsx", colNames = TRUE, borders = "columns")
merged_asset
}

merit<-merit_filter(days)



test_merit<-tight_hours_merit %>% filter(as.Date(local_date)==ymd("2013-12-6"))

