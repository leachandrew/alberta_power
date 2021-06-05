#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())


source("aeso_scrapes.R")
source("merit_scripts.R")




#analysing changes in the merit order placement by emissions

data_summary<-function(){
  #merit_sent<-head(merit_2018,10000)
  start_time<-Sys.time()
  
  load("all_merit.RData")

  singles<-seq(1,9)
  for(hour in singles){
    merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
    #  merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
  }

    merit_aug<-merit_data%>% filter(year(date)>=2012) %>%  #why only after 2012? Just sampling? Don't recall
      mutate(import_export=case_when(
    is.na(import_export) ~ "",
    TRUE                      ~  import_export
  ))%>%
  clean_merit_trade() 
  
  merit_data<-NULL #drop merit data out of memory
  
  print(paste("Loaded Merit Data. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
    
  #2 Fix renewable generation so that blocks reflect actual generation, not capacity
  
  #isolate renewable volumes from metered volumes
  #renew_gen<- all_vols %>% filter(year(date)>=2016) %>%
  #  filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% select(date,he,asset_id,vol)%>%
  #  rename("renew_gen"="vol")
  load("renew_vols.RData")
  
  
  merit_aug<-merit_aug%>%
    left_join(renew_vols,by=c("date","he","asset_id")) %>%
    mutate(
      to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
      size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
      available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
      dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
    ) 
  
  renew_vols<-NULL #drop it out of memory
  print(paste("Fixed renewable vols. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
  load("metered_vols_data")
  
  
  
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
  
  
  merit_aug<-merit_aug %>%
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
  
  #things should be exactly the same to here for synth portfolios and synth plants
  # as well as for GHG emissions merit order
  
  #here we need to deviate and make some changes to stack via GHG emissions intensity
  
  merit_aug<-merit_aug %>%
      group_by(date,he) %>%
    arrange(price) %>% mutate(merit=cumsum(available_mw),merit_dispatch=cumsum(dispatched_mw),block_ghgs=co2_est*size,merit_ghgs=cumsum(co2_est*available_mw)) %>%
    ungroup() %>%arrange(date,he,merit) %>% select(date,he,Plant_Type,offer_sum,block_number,price,from,to,size,available_mw,dispatched_mw,merit,co2_est,oba_val,ctax_cost,net,import_export,block_ghgs,merit_ghgs)
  
  #5 build and merge in our companion market data
  
  load("forecast_data.RData")
  
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
    supply_cushion=hourly_avail-hourly_dispatch
  )
  
  #12 trim columns
  merit_aug <- merit_aug %>% select(-sk_export_capability,-bc_export_capability,-bc_import_capability,
                                  -matl_export_capability,-matl_import_capability,-bc_matl_export_capability,
                                  -bc_matl_import_capability)%>%
    select(date,he,price,from,to,block_ghgs,merit_ghgs,Plant_Type,hourly_renewables,hourly_imports,nit_settle_cad_gj,total_import_capability,total_export_capability) %>%
    merit_aug <- merit_aug %>% assign_date_time_days()
  
  print(paste("Saving. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  #and you're done
  save(merit_aug, file="merit_ghgs.Rdata" ) 
  print(paste("Done. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  gc()
}



start_time<-Sys.time()
#need to load some functions from aeso_merits


#synth_portfolio()


gc()
paste("Built synthetic plants, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")
load(file="merit_ghgs.Rdata" ) 







merit_avg <-merit_aug %>% mutate(percentile=round(merit/hourly_avail*100))%>%
  #filter(offer_sum!="TRADE")%>%
  group_by(year,percentile,on_peak) %>%
  summarize(n=n(),p_mean=mean(price),min_price=min(price),max_price=max(price),
            ghg_mean=mean(merit_ghgs),min_ghg=min(merit_ghgs),max_ghg=max(merit_ghgs))
my_palette<-colors_tableau10()[c(3,5,7,8,4,1,6,2)]


set_png("merit_ghgs.png",height = 1200)
ggplot(filter(merit_avg,year>=2012,on_peak==TRUE))+#geom_line(aes(percentile,price,color=Plant_Type,group=Plant_Type))+
  geom_line(aes(x=percentile,y=ghg_mean/10^3, group=as.factor(year),colour=as.factor(year)),size=2)+
scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual("",values=my_palette)+
  scale_color_manual("",values=my_palette)+
  theme_bw()+theme(
    legend.position = "bottom",
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,vjust = 0.5),
    axis.title = element_text(size = 12,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Offered emissions (tonnes per hour)",
       title=paste("Merit orders in GHG terms by year",sep = ""),
       subtitle=paste("Average of all peak hour merit orders",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")

dev.off()

