#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())


source("aeso_scrapes.R")
source("merit_scripts.R")




#constructing the synthetic portfolio based on offer control

synth_portfolio<-function(){
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

  
  #here we need to deviate and make some changes to offer control
  
  merit_aug<-merit_aug %>%
      group_by(date,he) %>%
    arrange(price,offer_sum) %>% mutate(merit=cumsum(available_mw),merit_dispatch=cumsum(dispatched_mw)) %>%
    ungroup() %>%arrange(date,he,merit) %>% select(date,he,Plant_Type,offer_sum,block_number,price,size,available_mw,dispatched_mw,merit,co2_est,oba_val,ctax_cost,net,import_export)
  
  #built merit orders by offer control summary data
  
  merit_aug<-merit_aug %>%
  group_by(date,he,offer_sum) %>%
    arrange(price,offer_sum) %>% mutate(type_merit=cumsum(available_mw),
                                         type_dispatch=cumsum(dispatched_mw),
                                         type_block=row_number(),
                                         from=lag(type_merit,1),
                                         to=type_merit,
                                         from=ifelse(is.na(from),0,from)
                                         ) %>%
    ungroup()%>%arrange(date,he,offer_sum,type_merit)
 
  #convert all blocks to percentile blocks within offer controlled portfolio
  build<-1
  if(build==1){
    merit_bids<-merit_aug %>% group_by(date,he,offer_sum)%>% 
      arrange(date,he,price) %>%
      mutate(bid_capacity=sum(available_mw))%>% filter(bid_capacity>0)%>%
      summarize(
        #place offer percentiles and prices in lists of vectors
        from=list(from/bid_capacity*100),price=list(price),co2_est=list(co2_est),
        net=list(net),
        ctax_cost=list(ctax_cost)
      )%>% group_by(date,he,offer_sum) %>% #re-group the summarized data
      #get and store the bid function
      mutate(step_func=list(bid_func(from[[1]],price[[1]])),
             ghg_func=list(bid_func(from[[1]],co2_est[[1]])),
             ctax_func=list(bid_func(from[[1]],ctax_cost[[1]]))
             )%>%
      ungroup()
    
    print(paste("Built step functions. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
    
    #now make the bids
    merit_bids<-merit_bids%>% group_by(date,he,offer_sum) %>%
      mutate(bid_10=step_func[[1]](10),
             bid_20=step_func[[1]](20),
             bid_30=step_func[[1]](30),
             bid_40=step_func[[1]](40),
             bid_50=step_func[[1]](50),
             bid_60=step_func[[1]](60),
             bid_70=step_func[[1]](70),
             bid_80=step_func[[1]](80),
             bid_90=step_func[[1]](90),
             bid_100=step_func[[1]](100))%>%
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
      ungroup() %>% select(-from,-price,-step_func,-co2_est,-ghg_func,
                           -net,-ctax_cost,-ctax_func)
    print(paste("Build bids, cleaned data frame, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
    save(merit_bids,file= "synth_port.RData")
     }
  #bids data has date, he, asset_id, and bid_10 through bid_100 for all data.
  if(build!=1){
    load(file = "synth_port.RData") 
  }
  #melt and match
  merit_aug<-merit_bids %>% select(date,he,offer_sum,grep("bid_",names(merit_bids)))%>%
    melt(id=c("date","he","offer_sum"),variable.name="percentile",value.name = "price")%>%
    mutate(percentile=as.numeric(gsub("bid_","",as.character(percentile))))
  #ghgs<-merit_bids %>% select(date,he,Plant_Type,grep("ghg_",names(merit_bids)))%>%
  #  melt(id=c("date","he","Plant_Type"),variable.name="percentile",value.name = "ghg")%>%
  #  mutate(percentile=as.numeric(gsub("ghg_","",as.character(percentile))))
  #ctax<-merit_bids %>% select(date,he,Plant_Type,grep("ctax_",names(merit_bids)))%>%
  #  melt(id=c("date","he","Plant_Type"),variable.name="percentile",value.name = "ctax")%>%
  #  mutate(percentile=as.numeric(gsub("ctax_","",as.character(percentile))))
  #oba<-merit_bids %>% select(date,he,Plant_Type,grep("net_",names(merit_bids)))%>%
  #  melt(id=c("date","he","Plant_Type"),variable.name="percentile",value.name = "net")%>%
  #  mutate(percentile=as.numeric(gsub("net_","",as.character(percentile))))
  #merit_aug<-merit_aug %>% left_join(ghgs) %>% left_join(ctax) # %>% left_join(oba)
  #ctax<-NULL
  #oba<-NULL
  #ghgs<-NULL
  
  print(paste("Melt and Match done. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
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
    assign_date_time_days()
  
  print(paste("Saving. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  
  #and you're done
  save(merit_aug, file="synth_portfolios.Rdata" ) 
  print(paste("Done. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds"))
  gc()
}



start_time<-Sys.time()
#need to load some functions from aeso_merits


synth_portfolio()


gc()
paste("Built synthetic plants, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")
load(file="synth_portfolios.Rdata" ) 

merit_avg <-merit_aug %>% 
  filter(offer_sum!="TRADE")%>%
  group_by(offer_sum, year, percentile) %>%
  summarize(n=n(),p_mean=mean(price),min_price=min(price),max_price=max(price))
my_palette<-colors_tableau10()[c(8,4,1,6,2)]



set_png("synth_port.png",height = 1200)
ggplot(filter(merit_avg,year>=2012))+#geom_line(aes(percentile,price,color=Plant_Type,group=Plant_Type))+
  geom_ribbon(aes(x=percentile,ymin=min_price, ymax=max_price,),alpha=.3)+
  geom_line(aes(x=percentile,y=p_mean,group=offer_sum),size=2)+
  facet_grid(offer_sum~year)+

scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual("",values=my_palette)+
  scale_color_manual("",values=my_palette)+
  theme_bw()+theme(
    panel.spacing = unit(.75, "lines"),
    legend.position = "none",
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    #plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,vjust = 0.5),
    strip.text = element_text(size =8, colour = "black", angle = 0),
    axis.title = element_text(size = 12,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Synthetic Portfolios Average Offer Schedules and Ranges",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")
dev.off()



merit_aug<-merit_aug %>%
  mutate(policy=case_when(year(date)<2016 ~ "SGER 1.0",
                          year(date)==2017 ~ "SGER 2.0",
                          year(date)==2018 ~ "SGER 3.0",
                          TRUE ~ "CCIR"),
         policy=factor(policy,levels=c("SGER 1.0","SGER 2.0","SGER 3.0","CCIR"))
  )
         
library(broom)


# Split into pieces, fit model to each piece
by_offer <- merit_aug %>%  mutate(p2=percentile^2,p3=percentile^3,p4=percentile^4,gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
                                     year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
                                     tight=(supply_cushion<=500),really_tight=(supply_cushion<200),he_fac=as.factor(he))%>%
  #nest(-c(Plant_Type,percentile)) %>%
  nest(-c(offer_sum,percentile)) %>% 
  mutate(
    fit = map(data, ~ lm(price ~ 
                           #poly(percentile,4)+
                           #net+
                           #+p2+p3+p4+
                           poly(supply_cushion,2)+
                           poly(hourly_renewables,2)+
                           poly(hourly_imports,2)+
                           as.factor(on_peak)+
                           policy+
                           #percentile+
                           #on_peak+
                           #supply_cushion+
                           #supply_cushion*on_peak+
                           #day_ahead_forecasted_ail+
                           forecast_pool_price+
                           forecasted_actual_ail_difference+
                           total_import_capability+
                           #total_export_capability+
                           
                           nit_settle_cad_gj
                         +gas2+gas3+
                         #tight+really_tight#+
                         factor(year)
                         , data = .x)),
    #pred=map(fit,predict),
    tidied = map(fit,tidy),
    glanced = map(fit, glance)
    #augmented = map(fit, augment)
  )

test<-by_offer %>% 
  unnest(tidied, .drop = TRUE)  %>% filter(grepl("olicy",term),offer_sum %in% c("Capital Power","TransAlta","ATCO","ENMAX"))
ids<-names(test %>% select(-term))


test$upper_ci=test$estimate+1.96*test$std.error
test$lower_ci=test$estimate-1.96*test$std.error
test$term<-gsub("policy","",test$term)
test$term<-factor(test$term)
levels(test$term)<-factor(test$term)
levels(test$term)<-c("SGER 2.0","SGER 3.0","CCIR")

set_png("portfolio_offer_changes.png")
ggplot(filter(test,percentile!=100))+#geom_line(aes(percentile,price,color=Plant_Type,group=Plant_Type))+
  geom_errorbar(aes(x=percentile,ymax=upper_ci, ymin=lower_ci,group=offer_sum,color=offer_sum),size=2)+
  geom_point(aes(x=percentile,y=estimate,colour=offer_sum,group=offer_sum),size=2)+
  facet_grid(~term)+
 scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual("",values=my_palette)+
  scale_color_manual("",values=my_palette)+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(size = 15,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text = element_text(size = 14, colour = "black", angle = 0),
    axis.title = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Change in Price ($/MWh)",
       title=paste("Change in Synthetic Portfolio Average Offer Schedules Across Policies",sep = ""),
       subtitle=paste("Changes are relative to SGER 2015, adjusting for market factors and year fixed effects",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")
dev.off()


