#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())

library(grid)
library(gridExtra)
library(ggpubr)
library(gganimate)
library(timeDate)


#seasons
if(!exists("ajl_hourly", mode="function")) source("../andrew_base.R")

#FORECASTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#seasons
if(!exists("getSeason", mode="function")) source("get_season.R")


augment_data<-function(merit_sent){
  #testing
  #merit_sent<-filter(merit_data, year(date)==2019,month(date)==2)%>% clean_merit_trade()
  
  #bring in plant data
  plant_data <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  plant_info<-data.frame(t(plant_data[(1:10),-1]),stringsAsFactors = F)
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:10),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  
  plant_info<-plant_info %>%
    mutate(NRG_Stream = ifelse(grepl("AB - H R Milner Hr Avg MW",NRG_Stream),"AB Milner Hr Avg MW",NRG_Stream))%>%
    mutate(NRG_Stream = ifelse(grepl("AB - NPC1 Denis St Pierre Hr Avg MW",NRG_Stream),"AB - NPC1 Denis St  Pierre Hr Avg MW",NRG_Stream))  
  plant_info<-arrange(plant_info,NRG_Stream)
  
  #bring in ghg data
  ghg_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "GHG_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  #ghg_rates<-dcast(ghg_rates, formula = GHG_ID ~ ...,value.var = "Poln_rate")
  ghg_rates<-ghg_rates %>% spread(Pollutant,Poln_rate) %>% select(GHG_ID,CO2)
  
  
  #bring in heat rates
  heat_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Heat_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE) %>%
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
  
  merit_sent<-merit_sent %>% group_by(date,he) %>%
    mutate(hourly_exports=sum((import_export=="E")*size),hourly_imports=sum((import_export=="I")*size))  %>% ungroup()
  
  
  merit_sent %>% left_join(combined,by=c("asset_id"="ID")) %>%left_join(forecast_data,by=c("date","he")) %>%
    select(-Latitude,-Longitude,-Aurora_Name,-NRG_Stream,GHG_ID,start_date)
  
  
}



clean_merit_trade<-function(data_sent,id_tag){
  #for testing
  #data_sent<-head(merit_data,1000) #%>% assign_time %>% assign_date_time_days %>% assign_peaks
  
  #read in all AESO asset names so we're up-to-date
  zz =
    readHTMLTable("http://ets.aeso.ca/ets_web/ip/Market/Reports/AssetListReportServlet?contentType=html",colClasses = "character",stringsAsFactors = FALSE)
  aeso_assets<-as.data.frame(rbind(zz)[2])
  colnames(aeso_assets)<- aeso_assets[1, ] # the first row will be the header
  aeso_assets<-clean_names(aeso_assets[-1,])
  zz<-NULL
  trade<-data_sent%>%filter(import_export %in% c("E","I")) %>% #select the imports and exports from the merit data
    left_join(aeso_assets,by=c("asset_id"="asset_id"))
  trade$dest<-NA
  trade<-trade %>% mutate(dest = ifelse(grepl("BC",asset_name),"AB_BC",dest),
                          dest = ifelse(grepl("MT",asset_name),"AB_MON",dest),
                          dest = ifelse(grepl("SK",asset_name),"AB_SK",dest),
                          dest = ifelse(grepl("Sask",asset_name),"AB_SK",dest),
                          dest = ifelse(grepl("SPC",asset_name),"AB_SK",dest), 
                          dest = ifelse(grepl("XB",asset_name),"AB_BC",dest),
                          dest = ifelse(grepl("PWX",asset_name),"AB_BC",dest))
  
  
  trade<- trade %>% group_by(date,he,effective_date_time,import_export,dest,dispatched,flexible,price) %>% 
    summarise(size=sum(size),
              available_mw=sum(available_mw),
              dispatched_mw=sum(dispatched_mw)) %>%
    mutate(key_firm=TRUE,offer_control="TRADE",offer_sum="TRADE",merit=0,
           from=0,to=available_mw,block_number=0,) %>% 
    ungroup()%>%
    mutate(dest=ifelse(import_export=="I",paste(dest,"_IMP",sep = ""),paste(dest,"_EXP",sep = "")))
  names(trade)[names(trade) == "dest"] <- "asset_id"
  clean<-data_sent%>%filter(!import_export %in% c("E","I")) %>% #select the non-imports and non-exports from the merit data
    rbind(trade)
  clean
}



#need three sets of scraped AESO data - metered volumes for wind and solar plants, prices, merits


load("all_merit.RData")  
#check to make sure singles are consistent
singles<-seq(1,9)
for(hour in singles){
  merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
  #  merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
}

load("forecast_data.RData")


load(file="metered_vols_data.Rdata" ) 

#isolate to 2019 merit data, and then clean up the trade date and add plant info
merit_aug<-filter(merit_data, year(date)==2019)%>% clean_merit_trade() %>%
  augment_data() %>% 
  left_join(itc_data,by=c("date","he")) %>% # bring in import and export capabilities
  mutate(
    Capacity=ifelse(asset_id=="AB_BC_IMP",bc_import_capability,Capacity), #use hourly import capability
    Capacity=ifelse(asset_id=="AB_SK_IMP",sk_import_capability,Capacity),
    Capacity=ifelse(asset_id=="AB_MON_IMP",matl_import_capability,Capacity),
    Capacity=ifelse(asset_id=="AB_BC_EXP",bc_export_capability,Capacity), #use hourly import capability
    Capacity=ifelse(asset_id=="AB_SK_EXP",sk_export_capability,Capacity),
    Capacity=ifelse(asset_id=="AB_MON_EXP",matl_export_capability,Capacity)
  )


#put in an export capacity fix for the hours (time change days) that these don't line up

merit_aug<-merit_aug%>%mutate(
  Capacity=ifelse((is.na(merit_aug$Capacity)& asset_id=="AB_MON_EXP"),300,Capacity),
  Capacity=ifelse((is.na(merit_aug$Capacity)& asset_id=="AB_BC_EXP"),950,Capacity),
  Capacity=ifelse((is.na(merit_aug$Capacity)& asset_id=="AB_SK_EXP"),155,Capacity),
  Capacity=ifelse((is.na(merit_aug$Capacity)& asset_id=="AB_MON_IMP"),295,Capacity),
  Capacity=ifelse((is.na(merit_aug$Capacity)& asset_id=="AB_BC_IMP"),750,Capacity),
  Capacity=ifelse((is.na(merit_aug$Capacity)& asset_id=="AB_SK_IMP"),155,Capacity)
)



#isolate renewable and cogen volumes from metered volumes
renew_gen<- all_vols %>% filter(year(date)>=2016) %>%
  filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% select(date,he,asset_id,vol)%>%rename("renew_gen"="vol")

cogen_gen<- all_vols %>% filter(year(date)>=2016) %>%
  filter(Plant_Type=="COGEN") %>% select(date,he,asset_id,vol)%>%rename("cogen_net_vol"="vol")

#ALS_test<-cogen_gen %>% filter(asset_id=="ALS1")

#take the merit data and merge in the renew gen and apply it to the renewable part of the merit order

merit_aug<-merit_aug%>% 
  select(date,he,import_export,from,to,size,price,block_number,dispatched,dispatched_mw,flexible,asset_id,Capacity,offer_control,AESO_Name,Plant_Type,available_mw,co2_est,forecast_pool_price,actual_posted_pool_price,actual_ail)%>%
  left_join(renew_gen,by=c("date","he","asset_id")) %>%
  left_join(cogen_gen,by=c("date","he","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) %>%
  #get hourly summary data
  group_by(date,he) %>% mutate(hourly_avail=sum(available_mw),
                               hourly_dispatch=sum(available_mw*(dispatched=="Y")),
                               hourly_imports=sum(available_mw*(import_export=="I")),
                               hourly_exports=sum(available_mw*(import_export=="E")),
                               hourly_renewables=sum(renew_gen,na.rm = T),
  )%>%
  #recalculate merits and dispatched power
  arrange(price,Plant_Type) %>% mutate(merit=cumsum(available_mw),merit_dispatch=cumsum(available_mw*(dispatched=="Y"))) %>%
  #ungroup and then arrange
  ungroup() %>%arrange(date,he,merit)%>%
  #projection by capcity percentile
  mutate(block_pctl=to/Capacity)

#load NRG Stream scraped data




load(file="gen.RData")
#get cogen data from the scraped data
cogen_nrg<-gen_data %>% filter(year(date)>=2016) %>% 
  filter(Plant_Type=="COGEN") %>% select(date,he,ID,gen) 
cogen_nrg<-cogen_nrg%>% rename(asset_id=ID)

#cogen_nrg should be actual generation while cogen_gen should be net-to-grid from the facility.

cogen_test<-merit_all %>% filter(Plant_Type=="COGEN") %>% group_by(date,he,asset_id) %>% 
  summarize(Capacity=mean(Capacity),max_block=max(to),
            available_mw=sum(available_mw),dispatched_mw=sum(dispatched_mw),
            cogen_mv=mean(cogen_net_vol)) %>% ungroup()
cogen_test<-cogen_test %>% mutate(he=ifelse(he=="02*","02",he))  %>% #take out the 02* to allow matching to NRGStream Scraped
  left_join(cogen_nrg) %>% arrange(date,he,asset_id)

#now let's figure out who has big systemactic errors
#NAs are for NRGStream data in time change hours that are not scraped.
cogen_mv_test<-cogen_test %>% group_by(asset_id) %>%
  summarize(mean_mv=mean(cogen_mv),mean_disp=mean(dispatched_mw),mean_gen=mean(gen,na.rm=T),
            mv_diff=mean(dispatched_mw-cogen_mv),
            gen_diff=mean(dispatched_mw-gen,na.rm=T))



#let's see if we can create a data frame that has a function for each firm's block bids

#step function to map blocks to bids over x
generate_func<-function(x,y){
  stepfun(x, y, f = as.numeric(0), ties = "ordered",right = FALSE)
}

#if there's only one block, you'll need a single value function
single_value_function<-function(y){
  function(x){y}
}

bid_func<-function(x,y){
  if(length(y)>1)
    generate_func(x[-1],y)
  else
    single_value_function(y)
}

#df2<-filter(merit_aug,date==ymd("2019-01-15"),import_export=="")%>% 
#  arrange(date,he,asset_id,price) %>%
#  head(50) 

df3<-filter(merit_aug,import_export!="E")%>% #take out exports
  group_by(date,he,asset_id)%>%
  mutate(bid_capacity=max(to))%>%
  summarize(step_func=list(bid_func(from/bid_capacity*100,price)),total_mw=mean(bid_capacity)) %>%
  mutate(bid_10=map_dbl(step_func, ~ .x(10)),
         bid_20=map_dbl(step_func, ~ .x(20)),
         bid_30=map_dbl(step_func, ~ .x(30)),
         bid_40=map_dbl(step_func, ~ .x(40)),
         bid_50=map_dbl(step_func, ~ .x(50)),
         bid_60=map_dbl(step_func, ~ .x(60)),
         bid_70=map_dbl(step_func, ~ .x(70)),
         bid_80=map_dbl(step_func, ~ .x(80)),
         bid_90=map_dbl(step_func, ~ .x(90)),
         bid_95=map_dbl(step_func, ~ .x(95))) %>%
  melt(id=c("date","he","asset_id","step_func","total_mw"),variable.name="percentile",value.name = "price")%>%
  mutate(percentile=as.numeric(gsub("bid_","",percentile)))
  
ggplot(df3)+
  geom_line(aes(percentile*total_mw/100,price,colour=asset_id,group=asset_id), size=2)+
  #scale_fill_manual("",values=c(colors_tableau10(),colors_tableau10_light()))+  
  #scale_colour_manual("",values=c(colors_tableau10(),colors_tableau10_light()))+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  #scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,13000))+
  #scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_line()+theme(plot.subtitle = element_text(size = 14))+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Merit Order Offers by Plant, ",max(df3$date)," hour ending ",max(as.character(df3$he)),":00",sep = ""),
       #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: AESO Data, graph by Andrew Leach.")

ggplot(df3)+
  geom_line(aes(percentile,price,colour=asset_id,group=asset_id), size=2)+
  scale_fill_manual("",values=c(colors_tableau10(),colors_tableau10_light()))+  
  scale_colour_manual("",values=c(colors_tableau10(),colors_tableau10_light()))+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  #scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,13000))+
  #scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_line()+theme(plot.subtitle = element_text(size = 14))+
  labs(x=paste("Percentile of Offered Generation (%)"),y="Price ($/MWh)",
       title=paste("Merit Order Offers by Plant, ",max(df3$date)," hour ending ",max(as.character(df3$he)),":00",sep = ""),
       #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: AESO Data, graph by Andrew Leach.")



df_test<-NULL
library(dplyr)
df_test<-data.frame(rep(1:25, times=1, each=4))
names(df_test)[1]<-"asset"
df_test$block<-rep(1:4, times=25)
df_test$from<-rep(seq(0,150,50), times=25)
df_test$to<-df_test$from+50
df_test$index<-runif(100)*100

df_test<-df_test %>% group_by(asset) %>% mutate(price=cumsum(index))


df_func<-df_test %>%
  group_by(asset) %>% 
  summarize(step_func=list(generate_func(from[-1],price)))




map_dbl(df_func$step_func, ~ .x(50))

#now, what I would like to do is, for each asset, calculate a step function using 
#the from, to, and price blocks and store it in the data frame

#for example, using the first group I could do this

generate_func<-function(x,y){
  stepfun(x, y, f = as.numeric(0), ties = "ordered",right = FALSE)
}

eg_func<-generate_func(df_test$from[2:4],df_test$price[1:4])

#now, eg_func lets me find the implied price at any value x

eg_func(500)
[1] 43.10305

#what I'd like to do is group my data by asset and then store a version of
#eg_func for each asset in a second column of df_test

#so, what I want to do is something like:

df_sum<-df_test %>% group_by(asset) %>% summarize(
  step_func=generate_func(from[-1],price)
) 

#but I get:

Error: Column `step_func` is of unsupported type function





max_cap<-as.numeric(max(df2$to))
df3<-data.frame(seq(0,max_cap))
names(df3)[1]<-"MW"
for(date_count in unique(df2$date_id)) {
  print(ymd_h(date_count,tz="America/Denver"))
  hour_id<-as.character(hour(ymd_h(date_count,tz="America/Denver")))
  print(hour_id)
  df_test<-df2 %>% filter(date_id==date_count)
  if(nrow(df_test)>1){
    step_fun<-generate_func(df_test$from[-1],df_test$price)
    df3[,hour_id]<-step_fun(df3$MW)
  }
  else
    df3[,hour_id]<-df_test$price
}














day<-ymd("2015-04-23")
xdf<-get_merit_report(as.Date(day), as.Date(day)+days(1))




df1<-df1 %>% left_join(renew_gen,by=c("time","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) %>%
  group_by(time) %>% 
  arrange(time,price,offer_sum) %>% 
  mutate(merit=cumsum(available_mw))%>%
  ungroup()




merit_day<-merit_aug %>% filter(date==as.Date("2018-07-11"))%>% 
  select(date,he,from,to,size,price,block_number,dispatched,dispatched_mw,flexible,asset_id,offer_control,AESO_Name,Plant_Type,available_mw,co2_est,forecast_pool_price,actual_posted_pool_price,actual_ail)%>%
  left_join(renew_gen,by=c("date","he","asset_id")) %>%
  left_join(cogen_gen,by=c("date","he","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) %>%
  group_by(date,he) %>% mutate(hourly_avail=sum(available_mw),hourly_dispatch=sum(available_mw*(dispatched=="Y")))%>%
  arrange(price,Plant_Type,asset_id) %>% mutate(merit=cumsum(available_mw),merit_dispatch=cumsum(available_mw*(dispatched=="Y"))) %>%
  ungroup() %>%arrange(date,he,merit)



#geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
#geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
#geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3)])+  
  scale_colour_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3)])+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,13000))+
  scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_line()+theme(plot.subtitle = element_text(size = 14))+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Alberta Energy Merit Order by Plant Type, ",max(merit_2018_07_11$date)," ",max(as.character(merit_2018_07_11$he)),":00",sep = ""),
       subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: AESO Data, graph by Andrew Leach.")

dev.off()







merit_2018_07_11<-merit_aug %>% filter(date==as.Date("2018-07-11")&he=="04")%>% 
  mutate(oba_adj=case_when(Plant_Fuel=="COAL"~ (.800),
                           Plant_Fuel=="GAS"~ (.370),
                           TRUE~0),
         bid_adj=case_when(block_number==0~ price,
                           block_number>0~ price+.370*30-oba_adj*30)
  )%>% 
  group_by(date,he) %>% arrange(bid_adj,Plant_Type) %>% mutate(merit2=cumsum(size)) %>%
  ungroup()


ggplot(arrange(merit_2018,bid_adj,Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=bid_adj,fill=Plant_Type))+
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=price,colour=Plant_Type),fill=NA)+
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
  #geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
  #geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
  geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+  
  scale_colour_manual("",values=colors_tableau10())+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,13000))+
  scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_line()+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Alberta Energy Merit Order by Plant Type, ",max(merit_2018$date)," ",max(as.character(merit_2018$he)),":00",sep = ""),
       subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: AESO Data, graph by Andrew Leach.")





#start with merit data for a day
#clean up trade and add plant info
load("all_merit.RData")  
merit_day<-merit_data %>% filter(date==ymd("2018-08-01")) %>% clean_merit_trade() %>%
  augment_data()

merit_set<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","WIND")      
#take out the wind generators
df1<-arrange(filter(merit_day,Plant_Type %in% merit_set),he,price,Plant_Type)
#now we need wind metered volumes for the day in question
load(file="metered_vols_data.Rdata" ) 
renew_gen<- all_vols %>% filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% group_by(time,asset_id) %>% summarise(renew_gen=sum(vol,na.rm = T))
all_vols<-NULL
#merge the actual wind gen by unit
df1<-df1 %>% left_join(renew_gen,by=c("time","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) %>%
  group_by(time) %>% 
  arrange(time,price,offer_sum) %>% 
  mutate(merit=cumsum(available_mw))%>%
  ungroup()


df1<-df1%>% group_by(time) %>% arrange(time,he,price,offer_sum) %>% mutate(merit=cumsum(available_mw))


ggplot(subset(df1,he==18),aes(merit,price,fill=offer_sum)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price,frame=time))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price"),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+   
  scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,12300))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(df1$price)))+
  guides(fill=guide_legend(nrow=2))+
  ajl_line()+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")       



p<-ggplot(df1,aes(merit,price,fill=offer_sum)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price"),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+   
  scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,13000))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(df1$price)))+
  guides(fill=guide_legend(nrow=2))+
  guides(color=guide_legend(nrow=1))+
  ajl_line()+
  labs(title = 'Hour ending: {frame}', x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.") +
  transition_manual(time)+
  enter_fade() +
  exit_fade()


df1<-df1%>% group_by(time) %>% arrange(time,he,price,Plant_Type) %>% mutate(merit=cumsum(available_mw))

ggplot(filter(df1,he=="18"),aes(merit,price,fill=Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price,frame=time))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price",frame=time),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+   
  scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,12001))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(df1$price)))+
  guides(fill=guide_legend(nrow=2))+
  ajl_line()+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")


p<-ggplot(df1,aes(merit,price,fill=Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price"),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+   
  scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,13000))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(df1$price)))+
  guides(fill=guide_legend(nrow=2))+
  guides(color=guide_legend(nrow=1))+
  ajl_line()+
  labs(title = 'Hour ending: {frame}', x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.") +
  transition_manual(time)+
  enter_fade() +
  exit_fade()



#this is the section that converts the merit orders to a step function by MW

if(!exists("generate_func", mode="function")) source("generate_func.R")




df2<-subset(merit_aug,date==ymd("2019-01-15"))


#here, for example, we can create capacity decile bid blocks matrix for each day for each plant of bid values

df2<-df2%>% mutate(he=ifelse(he=="02*","02",he))%>%arrange(date,as.numeric(he),block_number)
df2$date_id<-paste(df2$date,df2$he,sep="_")

df2<-df2 %>% group_by(date,he,asset_id)%>%
  mutate()

max_cap<-as.numeric(max(df2$to))
df3<-data.frame(seq(0,max_cap))
names(df3)[1]<-"MW"
for(date_count in unique(df2$date_id)) {
  print(ymd_h(date_count,tz="America/Denver"))
  hour_id<-as.character(hour(ymd_h(date_count,tz="America/Denver")))
  print(hour_id)
  df_test<-df2 %>% filter(date_id==date_count)
  if(nrow(df_test)>1){
    step_fun<-generate_func(df_test$from[-1],df_test$price)
    df3[,hour_id]<-step_fun(df3$MW)
  }
  else
    df3[,hour_id]<-df_test$price
}



df3<-melt(df3,id=c("MW"),variable.name = "he",value.name = "price")  

ggplot(df3)+
  geom_line(aes(MW,price,group=he,colour=he))+
  scale_color_manual("Hour Ending",values=c(colors_tableau10(),colors_tableau10_light(),colors_tableau10_medium()))




#some specific merit order bids

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png(file="bids_alpac.png",width=1200,height=800)
ggplot(df3, aes(MW,price,group=time,colour=time)) +
  geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
  
  geom_line(size=1.25)+
  scale_color_viridis("",discrete=TRUE,labels=rev(unique(df3$time)))+   
  #guides(colour=FALSE)+
  slide_theme()+    labs(y="Offer Price ($/MWh)",x="MW",
                         title="Power Offer Blocks, Alberta-Pacific Mill ($/MWh)",
                         caption="Source: AESO Data, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



df1<-subset(merit_data, date==as.Date("2015-01-01") & asset_id=="SD5")
df1$year<-year(df1$date)
df1 <- df1 %>% group_by(year,block_number) %>% summarise(price=mean(price),to=mean(to),from=mean(from))
df1 <- arrange(df1,year,block_number)
ggplot(subset(df1),aes(to,price)) +
  #geom_step(mapping=aes(x=merit,y=price))
  geom_rect(mapping=aes(xmin=from,xmax=to,ymin=price-0.1,ymax=price),alpha=1,fill="firebrick")+
  facet_wrap(~year)+
  #geom_step(mapping=aes(x=to,y=price),
  #           linetype=1,color='#d95f02',alpha=1,size=3)
  #geom_line(size=2)+
  #scale_y_continuous()+
  #scale_color_viridis(NULL, labels=c("July 2015","Jan 2016","July 2016","Jan 2017", "July 2017"),discrete=TRUE)+   
  #scale_color_viridis(NULL,discrete = TRUE,guide = 'none')+
  scale_fill_viridis("Offer\nControl",discrete = TRUE))+   
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
       title=paste("Sundance 5 Average Offer Blocks, ",min(df1$year),"-",max(df1$year),sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")

df1<-subset(merit_data, date>as.Date("2015-01-01") & asset_id=="BR5")
df1$year<-year(df1$date)
df1 <- df1 %>% group_by(year,block_number) %>% summarise(price=mean(price),to=mean(to),from=mean(from))
df1 <- arrange(df1,year,block_number)
ggplot(subset(df1),aes(to,price)) +
  #geom_step(mapping=aes(x=merit,y=price))
  geom_rect(mapping=aes(xmin=from,xmax=to,ymin=-5,ymax=price),alpha=1,fill="firebrick")+
  facet_wrap(~year)+
  #geom_step(mapping=aes(x=to,y=price),
  #           linetype=1,color='#d95f02',alpha=1,size=3)
  #geom_line(size=2)+
  #scale_y_continuous()+
  #scale_color_viridis(NULL, labels=c("July 2015","Jan 2016","July 2016","Jan 2017", "July 2017"),discrete=TRUE)+   
  #scale_color_viridis(NULL,discrete = TRUE,guide = 'none')+
  scale_fill_viridis("Offer\nControl",discrete = TRUE)+   
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
       title=paste("Battle River 5 Average Offer Blocks, ",min(df1$year),"-",max(df1$year),sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.") 


df1<-subset(merit_data, he==18 & date==as.Date("2017-10-05"))

ggplot(df1,aes(merit,price,fill=offer_sum)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-20,ymax=price))+
  scale_fill_viridis("Offer\nControl",discrete = TRUE)+   
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
       title=paste("Alberta Energy Merit Order, ",max(df1$date)," ",max(df1$he),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")

sger_rates <- read.xlsx(xlsxFile = "power_ghgs.xlsx", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
sger_combo<-merge(combined,sger_rates,by.x="ID",by.y="ID",all.y = TRUE)
sger_combo$co2_est<-sger_combo$co2_est/1000
sger_combo<-sger_combo[,c("ID","AESO_Name","Plant_Type","CO2","co2_est","GHG/MWh","SGER_EI","BEI.(in.CR)")]
#sger_combo$err_BEI<-ifelse(sger_combo$Plant_Type=="COGEN",sger_combo$`BEI.(in.CR)`- sger_combo$co2_est,sger_combo$`GHG/MWh`- sger_combo$co2_est)
#sger_combo$err_SGER<-ifelse(sger_combo$Plant_Type=="COGEN",sger_combo$SGER_EI- sger_combo$co2_est,sger_combo$`GHG/MWh`- sger_combo$co2_est)
#sger_combo$err<-ifelse(sger_combo$Plant_Type=="COGEN",sger_combo$`GHG/MWh`- sger_combo$co2_est,sger_combo$`GHG/MWh`- sger_combo$co2_est)
#sger_combo<-arrange(sger_combo,-abs(err))
write.xlsx(sger_combo, file = "sger_combo.xlsx", colNames = TRUE, borders = "columns") 







#old coal animations


df1<-subset(merit_test,Plant_Type=="COAL")

df1<-df1%>% arrange(date,he,price,offer_sum) %>% group_by(date,he)%>% mutate(merit=cumsum(size))



df2 <- df1 %>% group_by(he,merit) %>% summarise(price=mean(price))

source("tableau.R")

p<-ggplot(df2) +
  geom_point(aes(merit,price))+
  facet_wrap(~he,ncol = 4)+
  ggtitle("Alberta Merit Order, Internal Load, Exports,\nand Power Prices ")+
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




p<-ggplot(df1) +
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-20,ymax=price,fill=Plant_Type,frame=time))+
  geom_vline(aes(xintercept=actual_ail+hourly_exports,colour="Actual AIL\nand Exports",frame=time),linetype=2,size=1)+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price",frame=time),linetype=2,size=1)+
  #geom_hline(aes(yintercept=min(df1$actual_posted_pool_price),colour="Min Price",frame=time),linetype=2,size=1)+
  #facet_wrap(~he,ncol = 4)+
  ggtitle("Alberta Merit Order, Internal Load, Exports,\nand Power Prices ")+
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
library(gganimate)
animation::ani.options(interval = 1)
gganimate(p, title_frame = TRUE,"merit.gif")




#check peaks for merit data
#



