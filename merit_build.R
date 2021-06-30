source("power_paper_base.R")
source("cdn_weather.R")
source("aeso_scrapes.R")
source("merit_scripts.R")
source("cdn_weather.R")

start_time<-Sys.time()
#update merit order data

update<-0 #add new data
save<-1 #save files at the end
synth<-1 #synthetic plants?
  synth_type<-0 #1 is by plant_type, 0 is full merit as synthetic plant


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
  
  #merit_aug<-merit_store
  
  
  
  #convert to synthetic plants here? 
  #small_testing_sample 
  #merit_small<-merit_aug%>%filter(date==ymd("2019-10-05"))
  #merit_aug<-merit_small
  #this is by plant type
  if(synth==1){
    merit_aug<-merit_aug %>% mutate(Plant_Type=case_when( #use synth_type 0 for a single merit order
      synth_type == 0 ~ "All",
      TRUE ~ Plant_Type
    ))%>%
        filter(date<ymd("2020-01-01"))%>% #sample for the Shaffer paper is pre-2020
        #select(date,he,price,available_mw,dispatched_mw,co2_est,ctax_cost,oba_val,Plant_Type,renew_gen,offer_sum)%>%
        arrange(date,he,Plant_Type,price) %>%
        group_by(date,he,Plant_Type)%>% 
        filter(available_mw>0)%>%
        mutate(merit_type=cumsum(available_mw)/sum(available_mw),
               merit_co2=cumsum(co2_est*available_mw/1000), #cumulative tonnes of emissions across the merit order
               merit_ctax=(ctax_cost), #marginal compliance costs, $ per mwh
               merit_oba=(oba_val),#marginal oba value, $ per mwh
               merit_net_comp=(net))%>%
        summarize(
          #place offer percentiles and prices in lists of vectors
          available_mw=sum(available_mw),dispatched_mw=sum(dispatched_mw),renew_gen=sum(renew_gen,na.rm = T),
          merit=list(merit_type*100),price=list(price),co2_est=list(merit_co2),ctax_cost=list(merit_ctax),oba_val=list(merit_oba),net_comp=list(merit_net_comp)
        )%>%
        group_by(date,he,Plant_Type) %>% #re-group the summarized data
        #get and store the bid function
        mutate(merit_func=list(bid_func(merit[[1]],price[[1]])), #bids
               ghg_func=list(bid_func(merit[[1]],co2_est[[1]])), #cumulative emissions in tonnes
               ctax_func=list(bid_func(merit[[1]],ctax_cost[[1]])),#marginal ctax
               oba_func=list(bid_func(merit[[1]],oba_val[[1]])), #marginal oba
               net_comp_func=list(bid_func(merit[[1]],net_comp[[1]])),
               import_export=case_when(
                 Plant_Type=="IMPORT" ~ "I",
                 TRUE                      ~  "")
               )%>%
        ungroup()
  
    print(paste("Built step functions. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    merit_aug<-merit_aug%>% group_by(date,he,Plant_Type) %>%
      mutate(bid_10=merit_func[[1]](10),
             bid_20=merit_func[[1]](20),
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
       mutate(ghg_10=ghg_func[[1]](10),
              ghg_20=ghg_func[[1]](20),
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
     mutate(ctax_10=ctax_func[[1]](10),
            ctax_20=ctax_func[[1]](20),
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
      mutate(oba_10=oba_func[[1]](10),
             oba_20=oba_func[[1]](20),
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
      mutate(net_10=net_comp_func[[1]](10),
             net_20=net_comp_func[[1]](20),
             net_30=net_comp_func[[1]](30),
             net_40=net_comp_func[[1]](40),
             net_50=net_comp_func[[1]](50),
             net_55=net_comp_func[[1]](55),
             net_60=net_comp_func[[1]](60),
             net_65=net_comp_func[[1]](65),
             net_70=net_comp_func[[1]](70),
             net_75=net_comp_func[[1]](75),
             net_80=net_comp_func[[1]](80),
             net_85=net_comp_func[[1]](85),
             net_90=net_comp_func[[1]](90),
             net_95=net_comp_func[[1]](95),
             net_100=net_comp_func[[1]](100)) %>%
    ungroup() %>% select(-merit,-price,-co2_est,-merit_func,-ghg_func,-ctax_func,-oba_func,
                         -oba_val,-ctax_cost,-net_comp_func,-net_comp)
    print(paste("Build bids, cleaned data frame, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    #turn these into the appropriate format for later analysis
    
    
    merit_aug<-merit_aug %>% pivot_longer(cols = -c(date,he,Plant_Type,available_mw,dispatched_mw,renew_gen,import_export))%>%
    #split name at underscore
    separate(name,"_",into = c("data_point","percentile"))%>%
    mutate(percentile=as.numeric(percentile))%>%
    #make it wider again
    pivot_wider(names_from = data_point,values_from=value)
    
    #now replicate the merit_aug format, but for these compressed data
    #testing: merit_bids<-merit_bids %>% left_join(mkt_data,by=c("date","he")) 
    
    #merit_aug<-merit_bids    
    
    
  } # end of synth plants
  
  
  
# merge in companion market data and NIT gas prices
  
  merit_aug<-merit_aug %>% left_join(mkt_data,by=c("date","he")) 
  
  merit_aug<-merit_aug %>% left_join(ngx_data_read(),by=c("date")) %>%
  mutate(nit_settle_cad_gj=na.locf(nit_settle_cad_gj))
  
#clean up memory
  rm(mkt_data)
  gc()
  
  
print(paste("Market Data Merged. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))


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
  save(merit_aug,file=format(Sys.time(),format="data/merit_data_%Y_%b_%d_%H_%M.RData"))
  #student csv  
  print(paste("Saving student csv file. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  write_csv(merit_aug %>% 
              select(date,he,asset_id,AESO_Name,Plant_Type,Plant_Fuel,co2_est,block_number,size,flexible,price,import_export,available_mw,dispatched_mw,merit,actual_posted_pool_price,actual_ail,month,day,hour,year,on_peak,temp_ymm=temp_YMM,temp_yeg=temp_YEG,temp_yyc=temp_YYC,hourly_dispatch,hourly_imports,hourly_exports,hourly_renewables), 
            file.path(format(Sys.time(),format="data/student_data_%Y_%b_%d_%H_%M.csv.gz",sep="")))
  }
}
paste("Built and saved merit data set, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")

ggplot(filter(merit_aug,time>=ymd_h("2019-06-21 18",tz="America/Denver") & time<=ymd_h("2019-06-21 23",tz="America/Denver")))+
  geom_line(aes(as.numeric(percentile)*available_mw/100,ghg,group=he,color=he),size=.25)+
  facet_wrap(~Plant_Type)


ggplot(filter(merit_aug,time>=ymd_h("2019-06-21 16",tz="America/Denver") & time<=ymd_h("2019-06-21 22",tz="America/Denver")))+
  geom_line(aes(as.numeric(percentile)*available_mw,bid,group=he,color=he),size=.25)+
  facet_wrap(~Plant_Type)



