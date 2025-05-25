source("power_paper_base.R")
source("cdn_weather.R")
source("aeso_scrapes.R")
source("merit_scripts.R")

#rm(merit_aug,merit_year)
gc()
start_time<-Sys.time()
#update merit order data

options(scipen=999)

update<-0 #add new data
save<-1 #save files at the end
synth<-0 #synthetic plants?
  synth_type<-2  #5 is a target facility, focus_id,4 is facility,3 is by Plant Type, 2 is by offer_control,1 is by plant_fuel, 0 is full merit as synthetic plant
  if(synth_type==5)
    focus_id<-c("EGC1")

  
load("data/all_merit.RData")  
if(update==1){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  #merit_data <-merit_data %>% mutate(asset_id=gsub("SLD1","SDL1",asset_id)) #fix saddlebrook error
  #all_vols <-all_vols %>% mutate(asset_name=gsub("SLD1 Saddlebrook Solar","SDL1 Saddlebrook Solar",asset_name)) #fix saddlebrook error
  #fix Heartland and Suncor Offer Control
  #  merit_data <- merit_data %>%
  #    mutate(offer_sum<-as.character(offer_sum),
  #           )
  #remove the 02* hours
  #  merit_data<-merit_data%>%filter(he!="02*")
  save(merit_data, file="data/all_merit.RData")  
}

#small_testing_sample 
#merit_small<-merit_data%>%filter(date==ymd("2019-10-05"))
#merit_data<-merit_small

#fix the ANC1 issue if it exists
merit_data<-merit_data %>% mutate(asset_id=gsub("ANCI","ANC1",asset_id))



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
    #filter(date<ymd("2025-01-01"))%>%
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
    summarize(hourly_exports=sum(dispatched_mw*(import_export=="E")))%>%
    ungroup()
  #drop them
    merit_aug<-merit_aug %>% filter(import_export!="E")
    
  
  print(paste("Loaded merit data, fixed exports. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    
  #2 Fix renewable generation so that blocks reflect actual generation, not capacity
  
  #load volumes
  #test<-all_vols%>% filter(date==ymd("2025-03-01"))
  #test<-all_vols%>% filter(is.na(Plant_Type))
  
  if(update==1)
   {
   load(file="data/metered_vols_data.Rdata" ) 
    #all_vols<-all_vols %>% filter(date<=ymd("2025-01-20"))
    #test<-all_vols %>% filter(asset_id=="SLD1")
    #anci<-all_vols %>% filter(is.na(Plant_Type))%>% 
    #  I()
    #all_vols <-all_vols %>% mutate(asset_id=gsub("SLD1","SDL1",asset_id)) #fix saddlebrook error
    #all_vols <-all_vols %>% mutate(asset_name=gsub("SLD1 Saddlebrook Solar","SDL1 Saddlebrook Solar",asset_name)) #fix saddlebrook error
    all_vols<-update_vols(all_vols)
    save(all_vols,file="data/metered_vols_data.Rdata" ) 
    #isolate renewable (non-hydro and biomass) volumes from metered volumes - the ones that default bid to zero
    renew_vols<- all_vols %>% 
    filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% select(date,he,asset_id,Plant_Type,dispatched_mw=vol,pool_participant_id,effective_date_time=time)%>%
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
    #anci<-renew_vols %>% filter(is.na(Plant_Type)) 
    save(renew_vols, file="data/renew_vols.RData")  
    
    hourly_renew<-renew_vols%>%
      group_by(date,he) %>% summarize(renew_gen=sum(dispatched_mw,na.rm = T)) %>%ungroup()
    save(hourly_renew, file="data/hourly_renew.RData")  
    
  }
  load("data/renew_vols.RData")
  load("data/hourly_renew.RData")
  
  
  #hourly_renew<-renew_vols%>%
  #  group_by(date,he) %>% summarize(renew_gen=sum(dispatched_mw,na.rm = T)) %>%ungroup()
  
  
  
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
   grepl("Heartland",offer_control)~"Heartland",
   grepl("ENMAX",offer_control)~"ENMAX",
   grepl("Suncor",offer_control)~"Suncor",
   grepl("Capital Power",offer_control)~'Capital Power',
   grepl("ATCO",offer_control)~"ATCO",
   grepl("Alberta Power",offer_control) & (date<ymd("2019-09-30")) ~"ATCO",
   grepl("Alberta Power",offer_control) & (date>=ymd("2019-09-30")) ~"Heartland",
   grepl("Balancing Pool",offer_control)~"Balancing Pool",
   TRUE~"Other" #if it's not one of these, it's false
 ),key_firm=(offer_sum!="Other"))
 

  #storage objects for testing purposes
  #merit_store<-merit_aug
  
  #save(hourly_renew_aug, file="data/hourly_renew_aug.RData")  
  
  #use this to revert to stored merit_aug so you don't have to re-load
  #merit_aug<-merit_store
  
  merit_aug<-merit_aug%>% #take out any asset that appears in the renewables data
    filter(! asset_id %in% unique(renew_vols$asset_id) )%>%
    #add the renewable plants
    bind_rows(renew_vols%>%select(-Plant_Type)%>%filter(date %in% unique(merit_aug$date))) 
    #stack them all again
  
# test<-merit_aug %>% filter(renew_gen!=dispatched_mw) 
  
#drop renew_vols from memory
  rm(renew_vols) 
  gc()
 
#testing storage objects
#merit_store<-merit_aug  
#merit_small<-merit_aug%>%filter(year(date)==2022)
#merit_aug<-merit_small
  
#merit_aug<-merit_store  
#merit_aug<-merit_small
#test<-plant_data()
  
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
                           year(date)==2022 ~ "TIER_50",
                           year(date)==2023 ~ "TIER_65",
                           year(date)==2024 ~ "TIER_80",
                           year(date)==2025 ~ "TIER_95",
                           TRUE ~ "TIER_50")
    )%>%
  select(-year)# take out the year value since it will create duplication with older code snippets later for full data set


#testing storage objects
#merit_store<-merit_aug  
#merit_small<-merit_aug%>%filter(year(date)==2020)
#merit_aug<-merit_small

#merit_aug<-merit_store  
#merit_aug<-merit_aug %>% select(-c(co2_est,oba_rate,ctax,sger_first_year,compliance_cost))

#test<-plant_data()


#Repair coal-to-gas-conversions with script in power_paper_base
merit_aug<-merit_aug %>% 
    mutate(Plant_Type=case_when( 
      (asset_id=="GN3") & (effective_date_time>=ymd("2024-06-18")) ~ "NGCONV",  #GN3 change to gas effective
      (asset_id=="HRM") & (effective_date_time>=ymd("2020-05-08")) ~ "SCGT",  #Milner change to gas effective
      (asset_id=="HRM") & (effective_date_time>=ymd("2023-09-21")) ~ "CCGT",  #Milner change to gas effective 
      #SH1 Sheerness #1 and SH2 Sheerness #2 -July 30, 2021.https://www.aeso.ca/market/market-upeffective_date_times/2021/sh1-sheerness-1-and-sh2-sheerness-2-change-in-fuel-type-notice/
      (asset_id %in% c("SH1","SH2")) & (effective_date_time>=ymd("2021-07-30")) ~ "NGCONV",
      #KH3 January 11, 2022
      (asset_id =="KH1") & (effective_date_time>=ymd("2022-01-11")) ~ "NGCONV",
      #KH2 July 27, 2021.
      (asset_id =="KH2") & (effective_date_time>=ymd("2021-07-21")) ~ "NGCONV",
      (asset_id =="KH3") & (effective_date_time>=ymd("2022-01-11")) ~ "NGCONV",
      #Battle River #4 (BR4)	March 8, 2022
      (asset_id =="BR4") & (effective_date_time>=ymd("2022-03-08")) ~ "NGCONV",
      #Battle River #5 (BR5)	 November 19, 2021
      (asset_id =="BR5") & (effective_date_time>=ymd("2021-11-19")) ~ "NGCONV",
      #Sundance #6 (SD6)	401	0	0 February 19, 2021
      (asset_id =="SD6") & (effective_date_time>=ymd("2021-02-19")) ~ "NGCONV",
      (asset_id =="SD4") & (effective_date_time>=ymd("2022-01-4")) ~ "NGCONV",
      
      # GN1 and GN2 are fine since they were new IDs
      TRUE ~ Plant_Type),
      Capacity=case_when(
        (asset_id=="HRM") & (effective_date_time>=ymd("2020-04-23"))&(effective_date_time<ymd("2020-05-08")) ~ 185,  #Milner change to gas effective 
        (asset_id=="HRM") & (effective_date_time>=ymd("2020-05-08"))&(effective_date_time<ymd("2021-12-09")) ~ 208,  #Milner change to gas effective 
        (asset_id=="HRM") & (effective_date_time>=ymd("2021-12-09")) ~ 300,  #Milner change to gas effective 
        TRUE~Capacity
      ),
      Plant_Fuel=ifelse(Plant_Type=="NGCONV","GAS",Plant_Fuel)
    )
  
  


  
  
#check
anci<-merit_aug %>% filter(is.na(Plant_Type))%>% 
  select(asset_id, Plant_Type) %>% 
  distinct()%>%
  I()
  

  
  fossils<-c("SCGT","NGCC","COAL","NGCONV")
  
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
    ),
    total_import_capability=case_when(
      is.na(system_import_capability)~total_import_capability,
      TRUE ~ system_import_capability),
    total_export_capability=case_when(
      is.na(system_export_capability)~total_export_capability,
      TRUE ~ system_export_capability)
    
  )%>%
    #trim columns
    select(-system_export_capability,-system_import_capability,
           -sk_import_capability,-sk_export_capability,-bc_export_capability,-bc_import_capability,
           -matl_export_capability,-matl_import_capability,-bc_matl_export_capability,
           -bc_matl_import_capability,-start_date)
    #assign peaks
    mkt_data <- mkt_data %>%assign_date_time_days()
  
  save(hourly_summary,file="data/hourly_summary.RData")
  save(mkt_data,file="data/market_data.RData")
  
  
  #load(file="data/hourly_summary.RData")
  
  #clean up data in memory
  rm(forecast_data,itc_data,hourly_summary)
  gc()
  print(paste("Built hourly summary. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
  
  
  #Repair offer control
  
  key_firms<-c("ATCO","TransAlta","TransCanada","ENMAX","Capital Power","Heartland","Balancing Pool","Suncor")
  # 
  merit_aug<-
  #   test<-
    merit_aug%>% #
    mutate(offer_control=ifelse(offer_control=="",NA,offer_control))%>%
    group_by(asset_id)%>%
        fill(offer_control,.direction="up")%>%  #carry offer control info backwards%>%
    ungroup()%>%
    mutate(offer_sum=case_when(
      grepl("TransAlta",offer_control)~"TransAlta",
      grepl("TransCanada",offer_control)~"TransCanada",
      grepl("ENMAX",offer_control)~"ENMAX",
      grepl("URICA",offer_control)~"URICA",
      grepl("Shepard",offer_control)~"ENMAX",
      grepl("Calgary Energy",offer_control)~"ENMAX", #calgary energy centre
      grepl("Capital Power",offer_control)~'Capital Power',
      grepl("ATCO",offer_control)~"ATCO",
      grepl("Alberta Power",offer_control) & (date<ymd("2019-09-30")) ~"ATCO",
      grepl("Alberta Power",offer_control) & (date>=ymd("2019-09-30")) ~"Heartland",
      grepl("Heartland",offer_control)~"Heartland",
      grepl("Balancing Pool",offer_control)~"Balancing Pool",
      grepl("Canadian Natural",offer_control)~"CNRL",
      grepl("Genalta",offer_control)~"Genalta",
      grepl("Imperial Oil",offer_control)~"Imperial Oil",
      grepl("Suncor",offer_control)~"Suncor",
      asset_id=="WB4"~"University of Alberta", #never appears so assigned
      asset_id=="TMR"~"Transmission Must Run", #never appears so assigned
      asset_id=="RB2"~"ATCO", #never appears so assigned
      asset_id=="BLS1"~"ATCO", #never appears so assigned
      TRUE~offer_control #if it's not one of these, leave it the same
    ))%>% 
    #filter(year>=2013)%>%
    #filter(!Plant_Type %in% c("WIND","SOLAR","COGEN"))%>%
    #group_by(month,year,offer_sum)%>% summarize(offer_mw=sum(available_mw))%>%
    #arrange(month,year,-offer_mw)%>%
    #summarize(firm_5=sum(offer_mw[1:5],na.rm=T)/sum(offer_mw,na.rm=T),
    #          firm_4=sum(offer_mw[1:4],na.rm=T)/sum(offer_mw,na.rm=T),
    #          firm_3=sum(offer_mw[1:3],na.rm=T)/sum(offer_mw,na.rm=T),
    #          hhi=10000*sum((offer_mw/sum(offer_mw,na.rm=T))^2),
    #          date=ymd(paste(year,month,1,sep="-")))%>%
    #  ungroup()
    
    
    #test%>% pivot_longer(-c(date,month,year),values_to = "value",names_to="measure")%>%
    #  filter(measure!="hhi")%>%
    #  ggplot()+
    #  geom_line(aes(date,value,group=measure,colour=measure))+
    #  scale_color_manual("",values=colors_ua10())
  group_by(asset_id)%>%
      fill(offer_sum,.direction="up")%>%  #carry offer control info backwards
      mutate(key_firm=offer_sum %in% key_firms,
           key_firm_no_bp=offer_sum %in%key_firms[(key_firms != "Balancing Pool")])%>%
    mutate(offer_sum=as_factor(offer_sum),
           offer_sum=fct_other(offer_sum,keep = key_firms),
           #offer_sum=as.character(offer_sum))
           NULL)%>%
  ungroup()


  
  
  #storage objects for testing purposes
  #merit_store<-merit_aug
  #use this to revert to stored merit_aug so you don't have to re-load
  
  #merit_aug<-merit_store%>% filter(year(date)==2017)
  # merit_aug<-merit_store%>% filter(date==ymd("2014-01-16"))
  
  large_plants<-merit_aug %>% 
    filter(Plant_Type %in% fossils,asset_id!="CMH1") %>%
    group_by(asset_id,Plant_Type,Capacity)%>% 
    summarise(min_date=min(date),max_date=max(date))%>% 
    filter(max_date>ymd("2022-01-01"),min_date<ymd("2015-01-01")) %>% 
    group_by(Plant_Type) %>% arrange(Plant_Type,-Capacity)%>% 
      mutate(rank=row_number())%>%
    ungroup() %>% 
    #  filter(rank<=6)%>%
    filter(Capacity>=20.5)%>%
    select(asset_id)



  
  #small_testing_sample 
  #merit_small<-merit_aug%>%filter(date==ymd("2019-10-05"))
  #merit_aug<-merit_small
  #merit_aug<-merit_small%>%filter(date==ymd("2019-10-05"))
  #merit_small<-merit_small%>%mutate(facility=gsub(" #","_",AESO_Name))%>% separate(facility,into = c("facility","number"), sep="_(?=[^_]+$)")

  #small_testing_sample 
  #merit_small<-merit_aug%>%filter(date==ymd("2019-10-05"))
  
  #merit_small<-merit_small%>%mutate(facility=gsub(" #","_",AESO_Name))%>% separate(facility,into = c("facility","number"), sep="_(?=[^_]+$)")
  
  #convert to synthetic plants here if synth==1

  #merit_aug<-merit_small
  
  #test<-merit_aug %>% filter(year(date)==2023)
  #test<-merit_aug %>% filter(Plant_Type=="NGCONV")
  
  #load("data/merit_data_proc_bak.RData")  
  
  save.image("merit_workspace.RData")
  save(merit_aug,file="data/merit_aug.RData")
  #load("data/merit_aug.RData") 
  #load("merit_workspace.RData")
  
  #load(file="data/merit_aug.RData")
  #synth<-1
  #trying to configure merit data for lazy loading
  if(synth==1){
      #here, we are going to use Plant_Type and offer_gen to be our core pieces of information that gets passed on
      #determined by synth_type, we will send different information through these two variables for the final analysis.
      #synth_type 4 is facility,3 is offer control by type, 2 is by offer_control, 1 is by plant_type, 0 is full merit as synthetic plant
      #synth_type<-0
    #merit_aug <- merit_aug%>% filter(date<=ymd("2020-03-01")) #for the pass-through paper, let's use this.
    #merit_aug <- merit_aug%>% filter(date>=max(merit_aug$date)-years(5)) 
      
    #mar 2025 adding year_by_year synth
      #save(merit_aug,file="data/merit_aug.RData") 
      #load("data/merit_aug.RData")
      #rm(merit_aug)
      #attach("data/merit_aug.RData")
      #gc()
     
      merit_aug <- merit_aug%>% 
      mutate(offer_store=factor(offer_sum),
             plant_store=factor(Plant_Type))
    
      offer_levels<-levels(merit_aug$offer_store)
      plant_levels<-levels(merit_aug$plant_store)
               #use offer-store as the carry-through for offer control at a given point
      if(synth_type==5){ #if we're doing a specific unit or PPA
        merit_aug <- merit_aug%>% filter(asset_id %in% focus_id)%>%
          mutate(offer_sum=toString(focus_id)) #do it this way so that you preserve the old offer
          #mutate(offer_sum=as_factor(asset_id),offer_old=offer_sum) #do it this way so that you preserve the old offer
      }
      if(synth_type==4){ #if we're doing by unit, we need the largest fossil units
        merit_aug <- merit_aug%>% #filter(asset_id %in% large_plants$asset_id)%>%
          mutate(offer_sum=as_factor(asset_id),
                 offer_sum=fct_other(offer_sum,keep=large_plants$asset_id)
                 )
      }
      
      if(synth_type==3){ #by plant type, but focus on fossils
        merit_aug <- merit_aug%>% 
          filter(!is.na(offer_control),
                  offer_sum!="Other",
                  offer_sum!="Suncor",
                  offer_sum!="TRADE",Plant_Type %in% fossils)%>%
        mutate(offer_sum=fct_recode(offer_sum,"ATCO/Heartland"="ATCO","ATCO/Heartland"="Heartland")
        )%>%
          #mutate(Plant_Type=fct_recode(Plant_Type,"LEGACY COAL"="COAL","LEGACY COAL"="NGCONV"))%>%
          I()
      }
      
      if(synth_type==2){ #offer control w dispatchable plants
        merit_aug <- merit_aug%>% 
          filter(year(date)>=2013)%>% #only use years for which we have AESO offer control
          filter(!is.na(offer_control),offer_sum!="Other",
                 offer_sum!="Suncor",
                 offer_sum!="TRADE",Plant_Type %in% fossils)%>%
          mutate(offer_sum=fct_recode(offer_sum,"ATCO/Heartland"="ATCO","ATCO/Heartland"="Heartland")
                 )
      }
      
            
      merit_aug<-merit_aug%>%
        mutate(year=year(date))%>%
        filter(year<=2023)
      years_data<-unique(year(merit_aug$date))
      
      #LOOK HERE
      #      years_data<-c(2023)
      #LOOK HERE
      for(y_index in years_data)
      {
      #y_index<-2023
      merit_year<-merit_aug %>%
        filter(year==y_index)%>%
        mutate(Plant_Type=case_when( 
          synth_type == 0 ~ "All",
          synth_type == 2 ~ "All",
          synth_type == 3 ~ Plant_Type,
          TRUE ~ Plant_Fuel),
          offer_gen=as.character(offer_sum),
          offer_gen=case_when( 
            synth_type == 0 ~ "All",
            synth_type == 1 ~ "All",
            synth_type == 3 ~ "All",
            TRUE ~ offer_gen),
          offer_gen=factor(offer_gen)
          )%>%
        filter(size>0)%>%  #don't include zero-sized blocks - this helps section out issues with zero wind and solar hours too.
        arrange(date,he,price) %>%
        group_by(date,he,Plant_Type,offer_gen)%>% 
        mutate(merit_type=cumsum(size)/sum(size),
               merit_co2=cumsum(co2_est*size/1000), #cumulative tonnes of emissions across the merit order
               merit_ctax=(ctax_cost), #marginal compliance costs, $ per mwh
               merit_oba=(oba_val),#marginal oba value, $ per mwh
               #merit_net_comp=(compliance_cost)
               )%>%
        summarize(
          #place offer percentiles and prices in lists of vectors
          offers=list(as.character(offer_store)),plants=list(as.character(plant_store)),
          total_offers=sum(size),available_mw=sum(available_mw),dispatched_mw=sum(dispatched_mw),renew_gen=sum(renew_gen,na.rm = T),
          merit=list(merit_type*100),price=list(price),co2_est=list(merit_co2),ctax_cost=list(merit_ctax),oba_val=list(merit_oba)
        )%>%
        #test_tibble<-tibble(merit_year$offers[1][[1]],merit_year$plants[1][[1]],merit_year$merit[1][[1]],merit_year$price[1][[1]])%>%
        #rename(offers=1,plants=2,merit=3,price=4)
        #test_func<-generate_offer_func(test_tibble$merit,test_tibble$offers)
        #test_tibble<-test_tibble%>%mutate(test=test_func(merit))
        #test_tibble<-test_tibble%>%mutate(check=(test==offers))
        
        group_by(date,he,Plant_Type,offer_gen) %>% #re-group the summarized data
        #get and store the bid function
        mutate(merit_func=list(bid_func(merit[[1]],price[[1]])), #bids
               ghg_func=list(bid_func(merit[[1]],co2_est[[1]])), #cumulative emissions in tonnes
               ctax_func=list(bid_func(merit[[1]],ctax_cost[[1]])),#marginal ctax
               oba_func=list(bid_func(merit[[1]],oba_val[[1]])), #marginal oba
               #offer_func=list(generate_offer_func(merit[[1]],offers[[1]])), #marginal oba
               offer_func = list({
                 x <- merit[[1]]
                 y <- offers[[1]]
                 generate_offer_func(x, y)
               }),
               plant_func = list({
                 x <- merit[[1]]
                 y <- plants[[1]]
                 generate_offer_func(x, y)
               }),
               #plant_func=list(generate_offer_func(merit[[1]],plants[[1]])), #marginal oba
               #net_comp_func=list(bid_func(merit[[1]],net_comp[[1]])),
               import_export=case_when(
                 Plant_Type=="IMPORT" ~ "I",
                 TRUE                      ~  "")
        )%>%
        ungroup()%>%
        select(-merit,-ctax_cost,-oba_val,-co2_est,-price,-offers,-plants)

      merit_year<-merit_year%>% group_by(date,he,Plant_Type,offer_gen) %>%
        mutate(bid_15=merit_func[[1]](15),
               bid_30=merit_func[[1]](30),
               bid_40=merit_func[[1]](40),
               bid_50=merit_func[[1]](50),
               bid_55=merit_func[[1]](55),
               bid_60=merit_func[[1]](60),
               bid_65=merit_func[[1]](65),
               bid_70=merit_func[[1]](70),
               bid_75=merit_func[[1]](75),
               bid_80=merit_func[[1]](80),
               bid_85=merit_func[[1]](85),
               bid_90=merit_func[[1]](90),
               bid_95=merit_func[[1]](95),
               bid_100=merit_func[[1]](100))%>%
        mutate(ghg_15=ghg_func[[1]](15),
               ghg_30=ghg_func[[1]](30),
               ghg_40=ghg_func[[1]](40),
               ghg_50=ghg_func[[1]](50),
               ghg_55=ghg_func[[1]](55),
               ghg_60=ghg_func[[1]](60),
               ghg_65=ghg_func[[1]](65),
               ghg_70=ghg_func[[1]](70),
               ghg_75=ghg_func[[1]](75),
               ghg_80=ghg_func[[1]](80),
               ghg_85=ghg_func[[1]](85),
               ghg_90=ghg_func[[1]](90),
               ghg_95=ghg_func[[1]](95),
               ghg_100=ghg_func[[1]](100)) %>%
        mutate(ctax_15=ctax_func[[1]](15),
               ctax_30=ctax_func[[1]](30),
               ctax_40=ctax_func[[1]](40),
               ctax_50=ctax_func[[1]](50),
               ctax_55=ctax_func[[1]](55),
               ctax_60=ctax_func[[1]](60),
               ctax_65=ctax_func[[1]](65),
               ctax_70=ctax_func[[1]](70),
               ctax_75=ctax_func[[1]](75),
               ctax_80=ctax_func[[1]](80),
               ctax_85=ctax_func[[1]](85),
               ctax_90=ctax_func[[1]](90),
               ctax_95=ctax_func[[1]](95),
               ctax_100=ctax_func[[1]](100)) %>%
        mutate(oba_15=oba_func[[1]](15),
               oba_30=oba_func[[1]](30),
               oba_40=oba_func[[1]](40),
               oba_50=oba_func[[1]](50),
               oba_55=oba_func[[1]](55),
               oba_60=oba_func[[1]](60),
               oba_65=oba_func[[1]](65),
               oba_70=oba_func[[1]](70),
               oba_75=oba_func[[1]](75),
               oba_80=oba_func[[1]](80),
               oba_85=oba_func[[1]](85),
               oba_90=oba_func[[1]](90),
               oba_95=oba_func[[1]](95),
               oba_100=oba_func[[1]](100)) %>%
        ungroup() 
    print(paste("Built bids, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
      
        
      merit_year<-merit_year%>%
        select(-merit_func,-ghg_func,
               -ctax_func,-oba_func)
      
      print(paste("Cleaned data frame, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
      
      #turn these into the appropriate format for later analysis
      #merit_year_bk<-merit_year
      #merit_year<-merit_year_bk
      merit_year<-merit_year %>% 
        pivot_longer(cols = -c(date,he,Plant_Type,offer_gen,available_mw,dispatched_mw,total_offers,renew_gen,import_export,
                               offer_func,plant_func
                               ))%>%
        #split name at underscore
        separate(name,"_",into = c("data_point","percentile"))%>%
        mutate(percentile=as.numeric(percentile))%>%
        
        mutate(plants = map2(plant_func, percentile, ~ .x(.y)),
               offer = map2(offer_func, percentile, ~ .x(.y)))%>%
        select(-plant_func,-offer_func)%>%
        #make it wider again
        pivot_wider(names_from = data_point,values_from=value)%>%
        #mutate(offer=as_factor(offer_levels[offer]),
        #       plant=as_factor(plant_levels[plant]))%>%
        I()
      
      print(paste("Pivoted data frame for year ",y_index,", elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
      file_name<-paste("data/synth_",synth_type,"_",y_index,".RData",sep = "")
      save(merit_year,file=file_name)
      }
      
      
      #now we need to read each file in and then stack them
      
      library(purrr)
      
      file_names <- paste0("data/synth_",synth_type,"_",years_data, ".RData")
      
      # Function to load a data frame from an RData file
      load_rdata <- function(file) {
        e <- new.env()  # Load into a temporary environment
        load(file, envir = e)
        df_name <- ls(e)  # Get the name of the loaded object
        e[[df_name]]  # Extract the data frame as return
        }
      
      # Load and combine all data frames - this will be the revised merit_aug
      merit_aug <- file_names %>%
        map(load_rdata) %>%  # Load each RData file
        bind_rows()  # Combine all data frames
      
      #now replicate the merit_aug format, but for these compressed data
      #testing: merit_bids<-merit_bids %>% left_join(mkt_data,by=c("date","he")) 
      
      #merit_aug<-merit_bids    
      #stack them all
      
      #detach("file:data/merit_aug.RData")
      
      
    } 
    # end of synth plants
    
  load(file="data/hourly_summary.RData")
  load(file="data/market_data.RData")
  
  mkt_data<-mkt_data%>%left_join(ngx_data_read(),by=c("date"))#%>%filter(year>=2009,year<2025)
     
      
      # merge in companion market data and NIT gas prices
      
  merit_aug<-merit_aug %>% left_join(mkt_data,by=c("date","he"))
      
      
      #test<-merit_aug%>%filter(is.na(nit_settle_cad_gj)
      #clean up memory
      #rm(mkt_data)
      gc()
      
      
      print(paste("Market Data Merged. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))


      
#test<-tail(merit_aug,1000)
      
      if(save==1)
      {
        if(synth==1)
        {
          print(paste("Saving synthetic output. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
          if(synth_type==0){
            save(merit_aug,file=format(Sys.time(),format="data/synth_all_%Y_%b_%d_%H_%M.RData"))
            print(paste("Saved synthetic merit file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
            }
          if(synth_type==1){
            save(merit_aug,file=format(Sys.time(),format="data/synth_type_%Y_%b_%d_%H_%M.RData"))
          print(paste("Saved synthetic merit by type file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
          }
          if(synth_type==2){
            save(merit_aug,file=format(Sys.time(),format="data/synth_offer_%Y_%b_%d_%H_%M.RData"))
          print(paste("Saved synthetic merit by offer file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
          }
          if(synth_type==3){
            save(merit_aug,file=format(Sys.time(),format="data/synth_offer_type_%Y_%b_%d_%H_%M.RData"))
          print(paste("Saved synthetic merit by offer and type file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
          }
          if(synth_type==4){
            save(merit_aug,file=format(Sys.time(),format="data/synth_unit_%Y_%b_%d_%H_%M.RData"))
          print(paste("Saved synthetic merit by unit file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
          }
          if(synth_type==5){
            save(merit_aug,file=format(Sys.time(),format=paste("data/synth_focus_",gsub(", ","_",toString(focus_id)),"_%Y_%b_%d_%H_%M.RData",sep="")))
            print(paste("Saved synthetic merit by unit file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
          }
        }
        if(synth==0) #saving the processed merit data
        {
          print(paste("Saving merit file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
          save(merit_aug,file=format(Sys.time(),format="data/merit_data_%Y_%b_%d_%H_%M.RData"))
          #student csv  
          print(paste("Saving student csv file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
          write_csv(merit_aug %>% 
                      select(date,he,month,day,hour,year,time,on_peak,import_export,asset_id,AESO_Name,Plant_Type,Plant_Fuel,co2_est,block_number,size,
                             flexible,price,available_mw,dispatched_mw,merit,actual_posted_pool_price,
                             actual_ail,temp_ymm=temp_YMM,temp_yeg=temp_YEG,temp_yyc=temp_YYC,
                             hourly_dispatch,hourly_imports,hourly_exports,hourly_renewables,offer_control,offer_sum,
                             nit_settle_cad_gj), 
                    file.path(format(Sys.time(),format="data/student_data_%Y_%b_%d_%H_%M.csv.gz",sep="")))
        }
      }
      
      
      paste("Built and saved merit data set, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")  
    
#assignment_data<-merit_aug %>% filter(date>=ymd("2024-01-10"),date<=ymd("2024-01-17"))%>%
#  select(date,he,alberta_internal_load=actual_ail,pool_price=actual_posted_pool_price,import_export,AESO_Name,Capacity,asset_id,block_number,price,from,to,size,available_mw,dispatched,dispatched_mw,flexible,Plant_Type,Plant_Fuel)
#assignment_data%>%write_csv("merit_data.csv")

# si_data<-merit_aug %>% 
#  select(date,he,time,on_peak,super_peak,hourly_avail,hourly_dispatch,hourly_renewables,hourly_imports,hourly_exports,
#         alberta_internal_load=actual_ail,pool_price=actual_posted_pool_price,import_export,AESO_Name,Capacity,asset_id,block_number,price,from,to,size,available_mw,dispatched,dispatched_mw,flexible,Plant_Type,Plant_Fuel,offer_sum)
# write_csv(si_data, 
#           file.path(format(Sys.time(),format="data/si_data_%Y_%b_%d_%H_%M.csv.gz",sep="")))

#assignment_data%>%write_csv("merit_data.csv")

      
      
# marco_data<-merit_aug %>% filter(date==ymd("2024-04-10"),size>0)%>%select(date,he,asset_id,size,price,dispatched,flexible)%>%
#   group_by(date,he)%>%
#   arrange(date,he,price,size,asset_id)%>%
#   mutate(block=row_number(),
#          merit=cumsum(size),
#          mkt_price=max(price*(dispatched=="Y")         ))%>%
#   #fix the estimated ail using the last block in each hour. Assume half the last flexible block is dispatched
#   mutate(
#     dispatched_mw=case_when(
#         block==which(block==max(block*(dispatched=="Y")*(flexible=="Y"))) ~ size*.5,
#         dispatched=="Y" ~ size,
#         dispatched =="N" ~ 0))%>%
#   mutate(ail=sum(dispatched_mw),
#   dispatched_merit=cumsum(dispatched_mw))
# 
# marco_data%>%write_csv("marco_data.csv")

      
#annuals<-merit_aug %>% group_by(year,month,offer_sum,Plant_Type)%>%summarize(offers=sum(size,na.rm=T)/10^6)%>%
#  ungroup()%>% mutate(date=ymd(paste(year,month,1,sep="-")))

#save(annuals,file="data/annual_offers.Rdata")