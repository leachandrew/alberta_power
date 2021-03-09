source("aeso_scrapes.R")
source("merit_scripts.R")

start_time<-Sys.time()
#update merit order data

update<-0
load("all_merit.RData")  
if(update==1){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  #remove the 02* hours
  #  merit_data<-merit_data%>%filter(he!="02*")
  save(merit_data, file="all_merit.RData")  
}



  singles<-seq(1,9)
  for(hour in singles){
    merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
    #  merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
  }

    merit_aug<-merit_data%>% #filter(year(date)>=2015) %>% #testing just with 2019 data
      mutate(import_export=case_when(
    is.na(import_export) ~ "",
    TRUE                      ~  import_export
  ),
  effective_date_time=mdy_hm(effective_date_time,tz="America/Denver"))%>%
  clean_merit_trade() %>%
  ungroup() %>% 
  select(-merit) %>% filter(import_export!="E")
  #remove merit variable since we're going to re-calculate
  #remove exports from the merit - they're demand, not supply
    
    #checked here - all imports are zero.
    
  
  #merit_data<-NULL #drop merit data out of memory
  
  print(paste("Loaded Merit Data. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    
  
  
  
  
  
  #2 Fix renewable generation so that blocks reflect actual generation, not capacity
  
  #load volumes
  
  new_renew<-1
  if(new_renew==1){
    load(file="metered_vols_data.Rdata" ) 
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
    save(renew_vols, file="renew_vols.RData")  
  }
  if(new_renew==0){load("renew_vols.RData")}
  
  #pool_list<-"http://ets.aeso.ca/ets_web/ip/Market/Reports/ParticipantListReportServlet?contentType=html"
  #pool_part<-readHTMLTable(pool_list, trim=T, as.data.frame=T, header=T,skip.rows = 3)[[2]]
  #names(pool_part)<-pool_part[1,]
  #pool_part<-pool_part[-1,]%>% clean_names() %>% select(pool_participant_id,pool_participant_name)
  #
  #test<-renew_vols %>% left_join(pool_part) %>% filter(is.na(pool_participant_name))
  # unique(test$pool_participant_id)
  
  
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
  merit_store<-merit_aug
  
  
  #use this to revert to stored merit_aug so you don't have to re-load
  merit_aug<-merit_store
  
  merit_aug<-merit_aug%>% #take out any asset that appears in the renewables data
    filter(! asset_id %in% unique(renew_vols$asset_id) )%>%
    #add the renewable plants
    bind_rows(renew_vols%>%filter(date %in% unique(merit_aug$date))) %>% arrange(effective_date_time,price)
    #stack them all again
  
# test<-merit_aug %>% filter(renew_gen!=dispatched_mw) 
  
 
 
  
  
  #3 build hourly summary data
  hourly_summary<-merit_aug%>%
    group_by(date,he) %>% summarize(hourly_avail=sum(available_mw),
                                    hourly_dispatch=sum(dispatched_mw),
                                    hourly_imports=sum(dispatched_mw*(import_export=="I")),
                                    hourly_exports=sum(dispatched_mw*(import_export=="E")),
                                    hourly_renewables=sum(renew_gen,na.rm = T),
    ) %>%ungroup()
  

  print(paste("Built hourly summary. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    
  merit_aug<-merit_aug %>% 
    left_join(plant_data(),by=c("asset_id"="ID")) %>%
    
    left_join(sger_emissions_data(),by=c("asset_id"))
  
  
  
  merit_aug<-merit_aug %>% #filter(year(date)>=2012) %>%
    mutate(oba=oba_type(Plant_Type,year(date),co2_est/1000),
           ctax=ctax_year(year(date)),
           policy=case_when(year(date)<2018 ~ "SGER",
                            TRUE ~ "CCIR")
    )
  
  #build sger allocations for 2010-2015,2016,2017
  #allocations of zero and carbon tax of zero for pre-2017 firms outside of sger
  #allocations of 0.37 for all facilities in 2018 and 2019
  fossils<-c("SCGT","NGCC","COAL")
  merit_aug <-merit_aug %>% mutate(
    #if we have SGER data, use that to fill in the values for the co2 emissions
    co2_est=case_when(    !is.na(sger_2016_adj_ei) ~ sger_2016_adj_ei*1000,
                          TRUE                      ~  co2_est 
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
    ctax_cost=ctax*co2_est/1000,
    net=ctax_cost-oba_val

  )# %>% select(-oba_sger)
  
  print(paste("Filled Climate Policy. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
  
  
  
    merit_aug<-merit_aug %>% 
      group_by(date,he) %>%
    arrange(price,Plant_Type) %>% mutate(merit=cumsum(available_mw),merit_dispatch=cumsum(dispatched_mw)) %>%
    ungroup() %>%arrange(date,he,merit) %>% select(date,he,asset_id,AESO_Name,Aurora_ID,Plant_Type,Plant_Fuel,block_number,price,merit,size,flexible,available_mw,dispatched_mw,merit,co2_est,oba_val,ctax_cost,net,import_export)
  
  #5 build and merge in our companion market data
  
  load("forecast_data.RData")
  
  #update_itc_data()
  
  load(file="aeso_itc_data.Rdata" ) 
  
  mkt_data<-forecast_data %>% left_join(itc_data,by=c("date","he")) %>%
    assign_peaks(time_var = time)%>%
    left_join(hourly_summary,by=c("date","he")) 
  
  
  #clean up data in memory
  forecast_data<-NULL
  itc_data<-NULL
  hourly_summary<-NULL
  
  merit_aug<-merit_aug %>% left_join(mkt_data,by=c("date","he")) 
  merit_bids<-NULL #drop bid file out of memory
  mkt_data<-NULL
  
  
  merit_aug<-merit_aug %>% #select(-nit_settle_cad_gj.x,-nit_settle_cad_gj.y) %>%
    left_join(ngx_data_read(),by=c("date"))
  #carry forward missing gas price data
  merit_aug <-merit_aug %>% mutate(nit_settle_cad_gj=na.locf(nit_settle_cad_gj))
  
  
  print(paste("Market Data Merged. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
  #10 create a total export and import capability column
  
  merit_aug <- merit_aug %>% mutate(
    total_export_capability=case_when(
      is.na(bc_matl_export_capability) ~ bc_export_capability+sk_export_capability,
      TRUE                      ~  bc_matl_export_capability+ sk_export_capability
    ),
    total_import_capability=case_when(
      is.na(bc_matl_import_capability) ~ bc_import_capability+sk_import_capability,
      TRUE                      ~  bc_matl_import_capability+ sk_import_capability
    )
    
  )
  
  #11 supply cushion
  
  merit_aug <- merit_aug %>% mutate(
    supply_cushion=hourly_avail-hourly_dispatch,
    
  )
  
  #12 trim columns
  merit_aug <- merit_aug %>% select(-sk_export_capability,-bc_export_capability,-bc_import_capability,
                                  -matl_export_capability,-matl_import_capability,-bc_matl_export_capability,
                                  -bc_matl_import_capability)%>%
    assign_date_time_days()
  
  print(paste("Saving. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))

save(merit_aug,file="merit_aug_mar_5.RData")   

paste("Built merit data set, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")


#load file
#load(file="merit_aug_nov_20.RData")   
#add temps
load("ab_power_temps.RData")
temps_power<-temps_power %>% clean_names()
merit_aug<-merit_aug%>%left_join(temps_power)

gloria_data<-merit_aug %>% select(date,he,asset_id,AESO_Name,Plant_Type,Plant_Fuel,co2_est,block_number,size,flexible,price,import_export,available_mw,dispatched_mw,merit,actual_posted_pool_price,actual_ail,month,day,hour,year,on_peak,temp_ymm,temp_yeg,temp_yyc,hourly_dispatch,hourly_imports,hourly_exports,hourly_renewables)
write_csv((gloria_data), file.path("gloria.csv.gz"))




#testing




as.numeric(merit_aug %>% filter(import_export=="I") %>% summarize(max(price)))

as.numeric(merit_aug %>% filter(import_export=="E") %>% summarize(min(price)))

test_data<-merit_aug %>% group_by(date,he) %>% summarize(error=sum(dispatched_mw)-max(hourly_dispatch))
unique(test_data$error)


brooks_test<-merit_aug %>% filter(Plant_Type=="SOLAR") %>%
  group_by(hour,month)  %>% summarize(gen_max=max(dispatched_mw),gen_min=min(dispatched_mw),
                                      gen_mean=mean(dispatched_mw))


#wind_test<-renew_vols %>% 
#  group_by(asset_id,date)  %>% summarize(gen=mean(renew_gen))
#ggplot(wind_test) + geom_line(aes(date,gen, group=asset_id))+
#  geom_ribbon(aes(x=hour,ymin=gen_min,ymax=gen_max),alpha=0.3)+
#  facet_wrap(~month)
#ggsave("brooks_test.png")


merit_test<-merit_aug %>% filter(Plant_Type=="WIND") %>%
  group_by(month,year,asset_id)  %>% summarize(gen_max=max(dispatched_mw),gen_min=min(dispatched_mw),
                                         gen_mean=mean(dispatched_mw),
                                )%>%
  mutate(date=ymd(paste(year,month,15)))

ggplot(merit_test) + geom_line(aes(date,gen_max,group=asset_id,color=asset_id))

ggplot(merit_test) + geom_line(aes(date,renew_gen))


renew_test<-renew_vols %>% #filter(Plant_Type=="COAL") %>%
  group_by(date)  %>% summarize(renew_gen=mean(renew_gen,na.rm = T))

ggplot(renew_test) + geom_line(aes(date,renew_gen))



wind_test<-merit_aug %>% filter(Plant_Type=="WIND") %>%
  mutate(false_date=ymd(paste("2012",month(date),day(date),sep="-")))%>%
  group_by(asset_id,date)  %>% summarize(gen_max=max(dispatched_mw),gen_min=min(dispatched_mw),
                                      gen_mean=mean(dispatched_mw),
                                      false_date=last(false_date),
                                      year=year(date))
                                      

ggplot(wind_test) + geom_line(aes(false_date,gen_max, color=asset_id,group=asset_id))+
  #geom_ribbon(aes(x=hour,ymin=gen_min,ymax=gen_max),alpha=0.3)+
  facet_wrap(~year)+
  scale_x_date(date_labels = "%b\n%d")
#ggsave("brooks_test.png")



merit_test<-merit_aug %>% filter(he==18,day==15,month==1)
  ggplot(merit_test) + geom_line(aes(merit,price))+
  #geom_ribbon(aes(x=hour,ymin=gen_min,ymax=gen_max),alpha=0.3)+
  facet_wrap(~year)
  #scale_x_date(date_labels = "%b\n%d")

#take out time change days?
merit_dst<-merit_aug%>% group_by(date) %>% filter(NROW(unique(he))==24)




#write.csv(gloria_data,"gloria.csv")





