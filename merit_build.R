source("power_paper_base.R")
source("cdn_weather.R")
source("aeso_scrapes.R")
source("merit_scripts.R")
source("cdn_weather.R")

start_time<-Sys.time()
#update merit order data

options(scipen=999)

update<-0 #add new data
save<-1 #save files at the end
synth<-1 #synthetic plants?
  synth_type<-4  #4 is facility,3 is offer control by type, 2 is by offer_control,1 is by plant_type, 0 is full merit as synthetic plant


load("data/all_merit.RData")  
if(update==1){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  #remove the 02* hours
  #  merit_data<-merit_data%>%filter(he!="02*")
  save(merit_data, file="data/all_merit.RData")  
}

#small_testing_sample 
#merit_small<-merit_data%>%filter(date==ymd("2019-10-05"))
#merit_data<-merit_small


#bring in market data
if(update==1){
  update_forecasts()
  }
load("data/forecast_data.RData")

  singles<-seq(1,9)
  for(hour in singles){
    merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
    #  merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
  }

  #clean up the trade date in the merit order
    
  merit_aug<-merit_data%>% 
    mutate(import_export=case_when(
    is.na(import_export) ~ "",
    TRUE                      ~  import_export
  ),
  effective_date_time=mdy_hm(effective_date_time,tz="America/Denver"))%>%
  clean_merit_trade() %>%
  ungroup() %>% 
  select(-merit) 
    
  rm(merit_data) #no longer need this object, so clean it out
  gc()
      
  #remove exports from the merit and store them - they're demand, not supply
  
  #store them
  exports<-merit_aug %>% group_by(date,he)%>%
    summarize(hourly_exports=sum(dispatched_mw*(import_export=="E")))
  #drop them
    merit_aug<-merit_aug %>% filter(import_export!="E")
    
  
  print(paste("Loaded merit data, fixed exports. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    
  #2 Fix renewable generation so that blocks reflect actual generation, not capacity
  
  #load volumes
  
  if(update==1)
   {
   load(file="data/metered_vols_data.Rdata" ) 
    #all_vols<-all_vols %>% filter(year<2019)
    all_vols<-update_vols(all_vols)
    save(all_vols,file="data/metered_vols_data.Rdata" ) 
    #isolate renewable (non-hydro and biomass) volumes from metered volumes - the ones that default bid to zero
    renew_vols<- all_vols %>% 
    filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% select(date,he,asset_id,dispatched_mw=vol,pool_participant_id,effective_date_time=time)%>%
    #switch so that we have same data format as merit data
    mutate(import_export="",
          block_number=1,
          price=0,
          from=0,
          to=dispatched_mw,
          size=dispatched_mw,
          available_mw=dispatched_mw,
          dispatched="Y",
          flexible="N",
          key_firm=FALSE,
          renew_gen=dispatched_mw
          )
    save(renew_vols, file="data/renew_vols.RData")  
  }
  load("data/renew_vols.RData")

  asset_list<-"http://ets.aeso.ca/ets_web/ip/Market/Reports/AssetListReportServlet"
  aeso_assets<-readHTMLTable(asset_list, trim=T, as.data.frame=T, header=T,skip.rows = 3)[[2]]
  names(aeso_assets)<-aeso_assets[1,]
  aeso_assets<-aeso_assets[-1,]%>% clean_names() %>% select(pool_participant_id,pool_participant_name,asset_id)
  #add VQML is TransAlta
  aeso_assets[NROW(aeso_assets)+1,1]<-"VQML"
  aeso_assets[NROW(aeso_assets),2]<-"TransAlta Corporation"
  aeso_assets[NROW(aeso_assets),3]<-"AKE1"
  #grab only the last name associated to an ID
  aeso_assets<-aeso_assets%>%group_by(pool_participant_id)%>%
    summarize(pool_participant_name=last(pool_participant_name))
  renew_vols<-renew_vols %>% left_join(aeso_assets,by="pool_participant_id")%>%
    rename(offer_control=pool_participant_name)%>% select(-pool_participant_id)
  renew_vols<-renew_vols %>% mutate(offer_sum=case_when(
   grepl("TransAlta",offer_control)~"TransAlta",
   grepl("TransCanada",offer_control)~"TransCanada",
   grepl("ENMAX",offer_control)~"ENMAX",
   grepl("Capital Power",offer_control)~'Capital Power',
   grepl("ATCO",offer_control)~"ATCO",
   grepl("Balancing Pool",offer_control)~"Balancing Pool",
   TRUE~"Other" #if it's not one of these, it's false
 ),key_firm=(offer_sum!="Other"))
 

  #storage objects for testing purposes
  #merit_store<-merit_aug
  
  
  #use this to revert to stored merit_aug so you don't have to re-load
  #merit_aug<-merit_store
  
  merit_aug<-merit_aug%>% #take out any asset that appears in the renewables data
    filter(! asset_id %in% unique(renew_vols$asset_id) )%>%
    #add the renewable plants
    bind_rows(renew_vols%>%filter(date %in% unique(merit_aug$date))) 
    #stack them all again
  
# test<-merit_aug %>% filter(renew_gen!=dispatched_mw) 
  
#drop renew_vols from memory
  rm(renew_vols) 
  gc()
 
#testing storage objects
#merit_store<-merit_aug  
#merit_small<-merit_aug%>%filter(year(date)==2019)
#merit_aug<-merit_small
  
#merit_aug<-merit_store  
#merit_aug<-merit_small

  
merit_aug<-merit_aug %>% left_join(plant_data(),by=c("asset_id"="ID"))%>%
  mutate(year=year(date))%>%
  left_join(ghg_data(),by=c("asset_id"="ID","year"))%>%
  rename(co2_est=ei)%>% #use old naming covention
  mutate(policy=case_when( year(date)<2016 ~ "SGER_15",
                           year(date)==2016 ~ "SGER_20",
                           year(date)==2017 ~ "SGER_30",
                           year(date)==2018 ~ "CCIR_30",
                           year(date)==2019 ~ "CCIR_30",
                           year(date)==2020 ~ "TIER_30",
                           year(date)==2021 ~ "TIER_40",
                           TRUE ~ "TIER_40")
    )%>%
  select(-year)# take out the year value since it will create duplication with older code snippets later for full data set



  
  fossils<-c("SCGT","NGCC","COAL")
  
  merit_aug <-merit_aug %>% mutate(
    co2_est=case_when(import_export!="" ~ 0, #no deemed emissions for imports and exports
                      asset_id=="TMR" ~ 0.550, #simple cycle gas ei
                      TRUE ~ co2_est),
    oba_rate=case_when(import_export!="" ~ 0, #no oba for imports and exports
                       asset_id=="TMR" ~ 0, #no oba for TMR assets
                  TRUE ~ oba_rate),
    ctax=case_when(import_export!="" ~ 0, #no carbon tax for imports and exports
                   asset_id=="TMR" ~ ctax_year(year(date)), #carbon tax for TMR
                   TRUE ~ ctax),
    compliance_cost=case_when(import_export!="" ~ 0, #no carbon tax for imports and exports
                    asset_id=="TMR" ~ ctax_year(year(date)*.550), #carbon tax for TMR
                    TRUE ~ compliance_cost),
    oba_val=oba_rate*ctax,
    ctax_cost=co2_est*ctax
    )
  
  
  
  
  print(paste("Filled Climate Policy. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
 

  #reset merit data with updated renewables, etc.
  merit_aug<-merit_aug %>%
    group_by(date,he) %>%
    arrange(price,Plant_Type) %>% mutate(merit=cumsum(available_mw),merit_dispatch=cumsum(dispatched_mw)) %>%
    ungroup() %>%arrange(date,he,merit) 
   #%>% select(date,he,asset_id,AESO_Name,Plant_Type,Plant_Fuel,price,merit,merit_dispatch,available_mw,flexible,dispatched_mw,co2_est,oba_val,ctax_cost,net,import_export)
  
  
#remove problematic date/times 
  merit_aug<-merit_aug%>%
  filter(date!=ymd("2013-11-28")) %>% #remove incomplete data from nov 13, 2013
    filter(!(date==ymd("2009-12-15")& he=="18")) %>% #remove duplicated data he 18
    filter(!(date==ymd("2010-6-1")& he=="13")) %>% #remove day with missing data he13
    filter(!(date==ymd("2012-4-6")& he=="16")) %>% #remove day with missing data he 16
    filter(!(date==ymd("2015-1-12")&(he=="19"|he=="20"))) #remove day with missing data he 19 and 20
  
    
  
  #3 build hourly summary data
  hourly_summary<-merit_aug%>%
    group_by(date,he) %>% summarize(hourly_avail=sum(available_mw),
                                    hourly_dispatch=sum(dispatched_mw),
                                    hourly_imports=sum(dispatched_mw*(import_export=="I")),
                                    hourly_renewables=sum(renew_gen,na.rm = T),
    ) %>%ungroup() %>% left_join(exports) %>%
    mutate(supply_cushion=hourly_avail-hourly_dispatch)
  
  rm(exports) #no longer need to store this
  
  #add updated temperature data
  load("data/ab_power_temps.RData")
  
  if(update==1){
      temps_power<-update_weather_data(temps_power)
      save(temps_power,file="data/ab_power_temps.RData")
  }
  
  hourly_summary<-hourly_summary%>%left_join(temps_power)
  
  
  #update and load intertie capacities
  if(update==1){
    update_itc_data()
    }
  load(file="data/aeso_itc_data.Rdata" ) 
  
  mkt_data<-forecast_data %>% left_join(itc_data,by=c("date","he")) %>%
    assign_peaks(time_var = time)%>%
    left_join(hourly_summary,by=c("date","he"))
  
  #create a total export and import capability column
  
  mkt_data <- mkt_data %>% mutate(
    total_export_capability=case_when(
      is.na(bc_matl_export_capability) ~ bc_export_capability+sk_export_capability,
      TRUE                      ~  bc_matl_export_capability+ sk_export_capability
    ),
    total_import_capability=case_when(
      is.na(bc_matl_import_capability) ~ bc_import_capability+sk_import_capability,
      TRUE                      ~  bc_matl_import_capability+ sk_import_capability
    )
    
  )%>%
    #trim columns
    select(-sk_export_capability,-bc_export_capability,-bc_import_capability,
           -matl_export_capability,-matl_import_capability,-bc_matl_export_capability,
           -bc_matl_import_capability,-start_date)%>%
    #assign peaks
    assign_date_time_days()
  
  #clean up data in memory
  rm(forecast_data,itc_data,hourly_summary)
  gc()
  print(paste("Built hourly summary. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
  
  
    
  #storage objects for testing purposes
  #merit_store<-merit_aug
  #use this to revert to stored merit_aug so you don't have to re-load
  
  #merit_aug<-merit_store%>% filter(year(date)==2017)
  # merit_aug<-merit_store%>% filter(date==ymd("2014-01-16"))
  
  
  
  #convert to synthetic plants here? 
  #small_testing_sample 
  #merit_small<-merit_aug%>%filter(date==ymd("2019-10-05"))
  #merit_aug<-merit_small
  #merit_small<-merit_small%>%mutate(facility=gsub(" #","_",AESO_Name))%>% separate(facility,into = c("facility","number"), sep="_(?=[^_]+$)")
z


ggplot(filter(merit_aug,time>=ymd_h("2019-06-21 01",tz="America/Denver") & time<=ymd_h("2019-06-21 23",tz="America/Denver") & Plant_Type %in% fossils))+
  geom_line(aes(as.numeric(percentile)*available_mw/100,bid,group=he,color=he),size=.25)+
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  expand_limits(y=1001)+
  facet_wrap(~Plant_Type,scales="free_x")+
  labs(x="Offered Capacity",y="Offer Price")


ggplot(filter(merit_aug))+
  geom_line(aes(as.numeric(percentile),bid,group=he,color=he),size=.25)+
  facet_wrap(~offer_gen)


#ggplot(filter(merit_aug,time>=ymd_h("2019-06-21 01",tz="America/Denver") & time<=ymd_h("2019-06-21 23",tz="America/Denver") & Plant_Type %in% fossils))+
ggplot(merit_aug %>% filter(Plant_Type %in% fossils))+
  geom_line(aes(as.numeric(percentile),bid,group=he,color=he),size=.25)+
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  expand_limits(y=1001)+
  facet_grid(cols = vars(Plant_Type),rows = vars(offer_gen),scales="free_x")+
  labs(x="Offered Capacity",y="Offer Price")

ggplot(merit_aug%>%filter(offer_gen!="Other"))+
  geom_line(aes(as.numeric(percentile),bid,group=paste(date,he),color=he),size=.25)+
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  scale_x_continuous(expand=c(0,0),breaks = pretty_breaks())+
  expand_limits(y=1001)+
  facet_wrap(~offer_gen,nrow=2,scales="free_x")+
  paper_theme()+
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  labs(x="Share of Offered Capacity (%)",y="Offer Price ($/MWh)",
       title="2017 AESO Power Offers by Controlling Entity")
ggsave(filename = "images/offer_control.png",dpi = 300,width=14,height=7)


ggplot(merit_aug%>%filter(offer_gen!="Other"))+
  geom_line(aes(as.numeric(percentile),bid,group=paste(date,he),color=he),size=.25)+
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  scale_x_continuous(expand=c(0,0),breaks = pretty_breaks())+
  expand_limits(y=1001)+
  facet_wrap(~offer_gen,nrow=2,scales="free_x")+
  paper_theme()+
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  labs(x="Share of Offered Capacity (%)",y="Offer Price ($/MWh)",
       title="2017 AESO Power Offers by Controlling Entity")


