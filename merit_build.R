source("power_paper_base.R")
source("cdn_weather.R")
source("aeso_scrapes.R")
source("merit_scripts.R")

start_time<-Sys.time()
#update merit order data

update<-1
load("data/all_merit.RData")  
if(update==1){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  #remove the 02* hours
  #  merit_data<-merit_data%>%filter(he!="02*")
  save(merit_data, file="data/all_merit.RData")  
}

update_forecasts()
load("data/forecast_data.RData")

#merit_data<-replace_merit_day(merit_data,day=ymd("2009-12-15"))
#day=ymd("2009-12-15")
#xdf<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms=firms()) %>% filter(he=="18")


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
    
    
  #remove exports from the merit and store them - they're demand, not supply
  #store them
  exports<-merit_aug %>% group_by(date,he)%>%
    summarize(hourly_exports=sum(dispatched_mw*(import_export=="E")))
  #drop them
    merit_aug<-merit_aug %>% filter(import_export!="E")
    
  
  print(paste("Loaded merit data, fixed exports. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    
  #2 Fix renewable generation so that blocks reflect actual generation, not capacity
  
  #load volumes
  
  new_renew<-1
  if(new_renew==1){
    load(file="data/metered_vols_data.Rdata" ) 
    update_vols(all_vols)
    save(all_vols,file="data/metered_vols_data.Rdata" ) 
    #isolate renewable volumes from metered volumes
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
  if(new_renew==0){
    load("data/renew_vols.RData")
    }
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
 renew_vols<-renew_vols %>% mutate(key_firm=case_when(
   grepl("TransAlta",offer_control)~TRUE,
   grepl("TransCanada",offer_control)~TRUE,
   grepl("ENMAX",offer_control)~TRUE,
   grepl("Capital Power",offer_control)~TRUE,
   grepl("ATCO",offer_control)~TRUE,
   grepl("Balancing Pool",offer_control)~TRUE,
   TRUE~FALSE #if it's not one of these, it's false
 ))
 

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
 
 merit_aug<-merit_aug %>% 
  left_join(plant_data(),by=c("asset_id"="ID")) %>% #here, co2_est is in kg/MWh
  left_join(sger_emissions_data(),by=c("asset_id"))
  
 #sger data is t/mwh

  #build sger allocations for 2010-2015,2016,2017
  #allocations of zero and carbon tax of zero for pre-2017 firms outside of sger
  #allocations of 0.37 for all facilities in 2018 and 2019
  
  merit_aug<-merit_aug %>% #filter(year(date)>=2012) %>%
    mutate(oba=oba_type(Plant_Type,year(date),co2_est/1000),
           ctax=ctax_year(year(date)),
           policy=case_when(year(date)<2018 ~ "SGER",
                            TRUE ~ "CCIR")
    )
  
  
  fossils<-c("SCGT","NGCC","COAL")
  merit_aug <-merit_aug %>% mutate(
    #if we have SGER data (t/MWh), use that to fill in the values for the co2 emissions
    co2_est=case_when(!is.na(sger_2016_adj_ei) ~ sger_2016_adj_ei*1000, #if it's not NA, use SGER data
                          TRUE                      ~  co2_est #otherwise, use what we have from the plant data in t/MWh
    ),
    oba_sger=case_when(year(date)<2016 ~ SGER_baseline*0.88, #12% below benchmark
                       year(date)==2016 ~ SGER_baseline*0.85, #15% below benchmark
                       year(date)==2017 ~ SGER_baseline*0.8, #20% below
                       year(date)>=2018 ~ 0.37,#set to .37 for all regulated firms
                       TRUE                      ~  0 #otherwise no OBA
    ),
    oba=case_when((is.na(oba_sger)& (policy=="SGER") & (Plant_Type %in% fossils) )~ 0, #12% below benchmark
                  (is.na(oba_sger)& (policy=="SGER") & (!Plant_Type %in% fossils) )~ oba, #12% below benchmark
                  (is.na(oba_sger)& (policy=="CCIR"))~ oba, #12% below benchmark
                  Plant_Type=="COGEN"~oba,
                  TRUE ~ oba_sger),
    
    ctax=case_when((is.na(oba_sger)& (policy=="SGER") & (Plant_Type %in% fossils) )~ 0, #12% below benchmark
                   (is.na(oba_sger)& (policy=="CCIR"))~ ctax, #12% below benchmark
                   TRUE ~ ctax),
    
    oba=case_when(import_export!="" ~ 0, #no oba for imports and exports
                  TRUE ~ oba),
    
    ctax=case_when(import_export!="" ~ 0, #no carbon tax for imports and exports
                   TRUE ~ ctax),
    oba_val=oba*ctax,
    ctax_cost=ctax*co2_est/1000, #adjust co2est to tonnes
    net=ctax_cost-oba_val

  )# %>% select(-oba_sger)
  
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
  
  #add updated temperature data
  source("cdn_weather.R")
  load("ab_power_temps.RData")
  temps_power<-update_weather_data(temps_power)
  save(temps_power,file="ab_power_temps.RData")  
  
  hourly_summary<-hourly_summary%>%left_join(temps_power)
  
  
  #update and load intertie capacities
  update_itc_data()
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
  #merit_aug<-merit_store
  
  
  
  #convert to synthetic plants here? 
  #small_testing_sample 
  #merit_small<-merit_aug%>%filter(date==ymd("2019-10-05"))
  #merit_aug<-merit_small
  
  
  synth<-0
  synth_type<-1 #1 is by plant_type, 0 is full merit as synthetic plant
  if(synth==1){
    if(synth_type==1)
      {
      merit_bids<-merit_aug %>% select(date,he,price,available_mw,dispatched_mw,co2_est,ctax_cost,oba_val,Plant_Type,renew_gen,offer_sum)%>%arrange(date,he,Plant_Type,price) %>%
        group_by(date,he,Plant_Type)%>% 
        filter(available_mw>0)%>%
        mutate(merit_type=cumsum(available_mw)/sum(available_mw),
               merit_co2=cumsum(co2_est*available_mw/1000), #tonnes of emissions in the hour
               merit_ctax=cumsum(ctax_cost),
               merit_oba=cumsum(oba_val))%>%
        summarize(
          #place offer percentiles and prices in lists of vectors
          available_mw=sum(available_mw),dispatched_mw=sum(dispatched_mw),renew_gen=sum(renew_gen,na.rm = T),
          merit=list(merit_type*100),price=list(price),co2_est=list(merit_co2),ctax_cost=list(merit_ctax),oba_val=list(merit_oba)
        )%>%
        group_by(date,he,Plant_Type) %>% #re-group the summarized data
        #get and store the bid function
        mutate(merit_func=list(bid_func(merit[[1]],price[[1]])),
               ghg_func=list(bid_func(merit[[1]],co2_est[[1]])),
               ctax_func=list(bid_func(merit[[1]],ctax_cost[[1]])),
               oba_func=list(bid_func(merit[[1]],oba_val[[1]])),
               import_export=case_when(
                 Plant_Type=="IMPORT" ~ "I",
                 TRUE                      ~  "")
               )%>%
        ungroup()
    }
    if(synth_type==0) # full merit order as the synthetic plant
    {
      merit_bids<-merit_aug %>% select(date,he,price,available_mw,dispatched_mw,co2_est,ctax_cost,oba_val,Plant_Type,renew_gen,offer_sum)%>%arrange(date,he,Plant_Type,price) %>%
        group_by(date,he)%>%arrange(date,he,price)%>% 
        filter(available_mw>0)%>%
        mutate(merit_type=cumsum(available_mw)/sum(available_mw),
               merit_co2=cumsum(co2_est*available_mw),
               merit_ctax=cumsum(ctax_cost),
               merit_oba=cumsum(oba_val))%>%
        summarize(
          #place offer percentiles and prices in lists of vectors
          available_mw=sum(available_mw),dispatched_mw=sum(dispatched_mw),renew_gen=sum(renew_gen,na.rm = T),
          merit=list(merit_type*100),price=list(price),co2_est=list(merit_co2),ctax_cost=list(merit_ctax),oba_val=list(merit_oba)
        )%>%
        group_by(date,he) %>% #re-group the summarized data
        #get and store the bid function
        mutate(merit_func=list(bid_func(merit[[1]],price[[1]])),
               ghg_func=list(bid_func(merit[[1]],co2_est[[1]])),
               ctax_func=list(bid_func(merit[[1]],ctax_cost[[1]])),
               oba_func=list(bid_func(merit[[1]],oba_val[[1]])),
               Plant_Type="All",
               import_export="")%>%
        ungroup()
    }
    print(paste("Built step functions. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    merit_bids<-merit_bids%>% group_by(date,he,Plant_Type) %>%
      mutate(bid_10=merit_func[[1]](10),
             bid_20=merit_func[[1]](20),
             bid_30=merit_func[[1]](30),
             bid_40=merit_func[[1]](40),
             bid_50=merit_func[[1]](50),
             bid_60=merit_func[[1]](60),
             bid_70=merit_func[[1]](70),
             bid_80=merit_func[[1]](80),
             bid_90=merit_func[[1]](90),
             bid_100=merit_func[[1]](100))%>%
       mutate(ghg_10=ghg_func[[1]](10),
              ghg_20=ghg_func[[1]](20),
              ghg_30=ghg_func[[1]](30),
              ghg_40=ghg_func[[1]](40),
              ghg_50=ghg_func[[1]](50),
              ghg_60=ghg_func[[1]](60),
              ghg_70=ghg_func[[1]](70),
              ghg_80=ghg_func[[1]](80),
              ghg_90=ghg_func[[1]](90),
              ghg_100=ghg_func[[1]](100)) %>%
     mutate(ctax_10=ctax_func[[1]](10),
            ctax_20=ctax_func[[1]](20),
            ctax_30=ctax_func[[1]](30),
            ctax_40=ctax_func[[1]](40),
            ctax_50=ctax_func[[1]](50),
            ctax_60=ctax_func[[1]](60),
            ctax_70=ctax_func[[1]](70),
            ctax_80=ctax_func[[1]](80),
            ctax_90=ctax_func[[1]](90),
            ctax_100=ctax_func[[1]](100)) %>%
      mutate(oba_10=oba_func[[1]](10),
             oba_20=oba_func[[1]](20),
             oba_30=oba_func[[1]](30),
             oba_40=oba_func[[1]](40),
             oba_50=oba_func[[1]](50),
             oba_60=oba_func[[1]](60),
             oba_70=oba_func[[1]](70),
             oba_80=oba_func[[1]](80),
             oba_90=oba_func[[1]](90),
             oba_100=oba_func[[1]](100)) %>%
    ungroup() %>% select(-merit,-price,-co2_est,-merit_func,-ghg_func,-ctax_func,-oba_func,
                         -oba_val,-ctax_cost)
    print(paste("Build bids, cleaned data frame, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    #turn these into the appropriate format for later analysis
    
    
    merit_bids<-merit_bids %>% pivot_longer(cols = -c(date,he,Plant_Type,available_mw,dispatched_mw,renew_gen,import_export))
    
    #split name at underscore
    
    merit_bids<-merit_bids %>% separate(name,"_",into = c("data_point","percentile"))%>%
      mutate(percentile=as.numeric(percentile))
    
    #make it wider again
    
    merit_bids<-merit_bids %>% pivot_wider(names_from = data_point,values_from=value)
    
    #now replicate the merit_aug format, but for these compressed data
    
    merit_aug<-merit_bids    
  }
  
  
  
# merge in companion market data and NIT gas prices
  
  merit_aug<-merit_aug %>% left_join(mkt_data,by=c("date","he")) 
  
  merit_aug<-merit_aug %>% left_join(ngx_data_read(),by=c("date")) %>%
  mutate(nit_settle_cad_gj=na.locf(nit_settle_cad_gj))
  
#clean up memory
  rm(merit_bids,mkt_data)
  gc()
  
  
print(paste("Market Data Merged. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))

save<-0
#save<-1

if(save==1)
  {
  if(synth==1)
  {
  print(paste("Saving synthetic output. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
    if(synth_type==0)
      save(merit_aug,file=format(Sys.time(),format="data/synth_all_%Y_%b_%d_%H_%M.RData"))
    if(synth_type==1)
           save(merit_aug,file=format(Sys.time(),format="data/synth_type_%Y_%b_%d_%H_%M.RData"))
    print(paste("Saved synthetic merit file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  }
  if(synth==0) #saving the processed merit data
  {
  print(paste("Saving merit file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  save(merit_aug,file=format(Sys.time(),format="merit_data_%Y_%b_%d_%H_%M.RData"))
  #student csv  
  print(paste("Saving student csv file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  student_data<-merit_aug %>% select(date,he,asset_id,AESO_Name,Plant_Type,Plant_Fuel,co2_est,block_number,size,flexible,price,import_export,available_mw,dispatched_mw,merit,actual_posted_pool_price,actual_ail,month,day,hour,year,on_peak,temp_ymm=temp_YMM,temp_yeg=temp_YEG,temp_yyc=temp_YYC,hourly_dispatch,hourly_imports,hourly_exports,hourly_renewables)
  write_csv(student_data, file.path(format(Sys.time(),format="data/student_data_%Y_%b_%d_%H_%M.csv.gz",sep="")))
  }
}
paste("Built and saved merit data set, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")





ggplot(filter(merit_aug,year==2020))+
  geom_line(aes(as.numeric(percentile),ghg/10^6*365,group=time),size=.25)

ggplot(filter(merit_aug))+
  geom_line(aes(as.numeric(percentile),ghg/10^6*365,group=time),size=.25)+
  facet_grid(cols = vars(he),rows=vars(year))
  

ggplot(filter(merit_aug))+
  geom_line(aes(as.numeric(percentile),bid,group=time),size=.25)+
  facet_grid(cols = vars(he),rows=vars(year))

