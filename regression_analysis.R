library(skimr)

library(broom)
library(purrr)
library(margins)
source("power_paper_base.R")

#make a market power indicator
mkt_power_ind<-function(){
  load("data/all_merit.RData")
  update<-0
  if(update==1){
    merit_data<-rbind(merit_data,update_merit(merit_data))
    #remove the 02* hours
    #  merit_data<-merit_data%>%filter(he!="02*")
    save(merit_data, file="data/all_merit.RData")
  }
  
  #mkt_power<-merit_data %>%filter(year(date)==2018) %>% select(date,he,asset_id,size,available_mw,dispatched_mw,offer_control,dispatched,)%>%
  #  group_by(date,he,offer_control)%>%
  #  summarize(avail=sum(available_mw),dispatched=sum(dispatched_mw),id=list(asset_id))
  
  #checking major players
  
  offer_test<-merit_data %>%
    #filter(year(date)>=2013)%>%
    #mutate(year=year(date))%>%
    #select(asset_id,block=block_number,offer_control,year)%>%
    #filter(!is.na(offer_control),offer_control!="")%>% #take out NA and blank
    mutate(offer_sum=case_when(
      grepl("TransAlta",offer_control)~"TransAlta",
      grepl("TransCanada",offer_control)~"TransCanada",
      grepl("ENMAX",offer_control)~"ENMAX",
      grepl("URICA",offer_control)~"URICA",
      grepl("Shepard",offer_control)~"ENMAX",
      grepl("Calgary Energy",offer_control)~"ENMAX", #calgary energy centre
      grepl("Capital Power",offer_control)~'Capital Power',
      grepl("ATCO",offer_control)~"ATCO",
      grepl("Heartland",offer_control)~"Heartland",
      grepl("Balancing Pool",offer_control)~"Balancing Pool",
      grepl("Canadian Natural",offer_control)~"CNRL",
      grepl("Genalta",offer_control)~"Genalta",
      grepl("Imperial Oil",offer_control)~"Imperial Oil",
      grepl("Suncor",offer_control)~"Suncor",
      asset_id=="WB4"~"University of Alberta", #never appears so assigned
      asset_id=="TMR"~"Transmission Must Run", #never appears so assigned
      asset_id=="RB2"~"ATCO", #never appears so assigned
      TRUE~offer_control #if it's not one of these, leave it the same
    ))%>%
    group_by(asset_id)%>%
    fill(offer_sum,.direction="up")%>%  #carry offer control info backwards by asset id
    ungroup()%>%
    mutate(key_firm=offer_sum %in% c("ATCO","TransAlta","TransCanada","ENMAX","Capital Power","Heartland","Balancing Pool","Suncor"),
           key_firm_no_bp=offer_sum %in%c("ATCO","TransAlta","TransCanada","ENMAX","Capital Power","Heartland","Suncor"))%>%
    #distinct()#%>%
    group_by(date,he)%>%
    mutate(mkt_price=max(price*(dispatched=="Y")))%>%
    mutate(gap_price=(price-mkt_price)/mkt_price)%>% #pct from price (abs val)
    ungroup()%>%
    #mutate(offer_sum=as_factor(offer_sum))%>%
    group_by(date,he,offer_sum,key_firm,key_firm_no_bp)%>%
    summarize(gz=sum((price>0)*available_mw),avail=sum(available_mw),dispatch=sum(dispatched_mw))%>%
    #arrange(date,he,-avail)%>%
    group_by(date,he)%>%
    summarize(total=sum(avail),
              cr_key=sum(avail*(key_firm==TRUE))/sum(avail),
              cr_nbp=sum(avail*(key_firm_no_bp==TRUE))/sum(avail),
              cr_nz=sum(gz*(key_firm==TRUE))/sum(gz),
              cr_nz_nbp=sum(gz*(key_firm_no_bp==TRUE))/sum(gz))%>%
    mutate(time=ymd_h(paste(date,he)))
  
  
  offer_graph<-offer_test %>% pivot_longer(-c(date,he,time,total),names_to = "measure")%>%
    mutate(no_bp=grepl("nbp",measure)) %>% 
    mutate(no_bp=as_factor(no_bp),
           no_bp=fct_recode(no_bp,"Balancing Pool Omitted"="TRUE",
                            "Balancing Pool Included"="FALSE"))%>%
    mutate(non_zero=grepl("nz",measure),
           non_zero=as_factor(non_zero),
           non_zero=fct_recode(non_zero,"Share of Non-Zero Offers"="TRUE",
                               "Share of Offers"="FALSE"),
           non_zero=fct_relevel(non_zero,"Share of Offers")
    )
  
  
  
  ggplot(offer_graph%>%#filter(year(date)<2022)%>%
           mutate(year=year(date),month=month(date))%>%
           group_by(year,month,non_zero,no_bp)%>%
           #group_by(date,non_zero,no_bp)%>%
           summarize(date=min(date),
                     value=mean(value,na.rm=TRUE)))+
    geom_line(aes(date,value*100,color=non_zero,lty=non_zero),size=1.25)+
    facet_grid(cols = vars(no_bp))+
    scale_color_manual("",values=c("black","grey50","grey80"))+
    scale_linetype_manual("",values=c("solid","11"))+
    expand_limits(y=c(40,100))+
    #expand_limits(x=ymd("2022-05-01"))+
    #guides(color="none")+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    scale_x_date(expand = c(0,0),breaks=pretty_breaks(n=8))+
    #paper_theme()+
    theme_ps()+
    theme(panel.spacing = unit(2,"lines"),
          legend.key.width = unit(3,"lines"),
          strip.text = element_text(
            size = 14, color = "black",# face = "bold"
          ),
          NULL
    )+
    labs(x="",y=expression("Key Firms' Average Share of Hourly Offers"*phantom(1)*'(%)'),
         NULL
    )+
    annotate("rect",xmin=ymd("2016-01-01"),xmax=ymd("2017-1-1"),ymin=-Inf,ymax=93.5, alpha=0.1, fill="black")+
    annotate("text", x = ymd("2016-1-1")+days(round(365/2)), y = 94, label = "PPAs returned to\nBalancing Pool",size=rel(4.5),hjust = 0.5,vjust = 0)+  
    annotate("rect",xmin=ymd("2020-12-15"),xmax=ymd("2021-1-15"),ymin=-Inf,ymax=93.5, alpha=0.1, fill="black")+
    geom_segment(x =ymd("2021-1-1"),xend =ymd("2021-1-1"),y=0,yend=93.5,linewidth=0.85)+
    annotate("text", x = ymd("2020-12-31"), y = 94, label = "PPA expiration\nDec. 31, 2020",size=rel(4.5),hjust = 0.5,vjust = 0)+  
    annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2023-12-31"),ymin=40,ymax=50, alpha=0.1, fill="black")+
    annotate("text",x=ymd("2009-09-01")+((ymd("2017-12-31")-ymd("2009-09-01"))/2),y=45,label = "Sample period (2009-2023)",size=rel(4.5))+
    NULL  
  
  ggsave("images/mkt_power.png",dpi=300,width=16,height=9)
  
  save(offer_test,file=format(Sys.time(),format="data/offer_data_%Y_%b_%d_%H_%M.RData"))  

}

offer_files<- list.files("data/") %>% as_tibble() %>% filter(grepl("offer_data",value))%>%
  mutate(file_date=gsub("offer_data_","",value),
         file_date=ymd_hm(gsub(".RData","",file_date))
  )%>% filter(file_date==max(file_date))

load(file = paste("data/",offer_files$value,sep=""))
paste("loaded file=data/",offer_files$value,sep="")


#merit all ie Synth Type 0


bids_files<- list.files("data/") %>% as_tibble() %>% filter(grepl("synth_all",value))%>%
  mutate(file_date=gsub("synth_all_","",value),
         file_date=ymd_hm(gsub(".RData","",file_date))
  )%>% filter(file_date==max(file_date))

load(file = paste("data/",bids_files$value,sep=""))
paste("loaded file=data/",bids_files$value,sep="")
merit_bids_all<-merit_aug%>%ungroup()%>%
  mutate(plants=as.character(plants),
         offer=as.character(offer)
         )


#pick 10% of the date/he combos at random
#sample_share<-.5
#merit_bids_all<-merit_bids_all%>% sample_n(nrow(.)*sample_share, replace = F) #no replacement - don't duplicate


# regression on all the plants
all_plants <- merit_bids_all %>%
  select(bid,percentile,ctax,oba,supply_cushion,hourly_renewables,hour,on_peak,supply_cushion,day_ahead_forecasted_ail,forecast_pool_price,actual_ail,
         day_ahead_forecasted_ail,total_export_capability,total_import_capability,nit_settle_cad_gj,hdd_YEG,hdd_YMM,hdd_YYC,
         cdd_YEG,cdd_YMM,cdd_YYC,year,month_fac,date,he,
         offer,
         plants
         )%>%
  na.omit()%>%
  mutate(
    net=ctax-oba,
    gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
    year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
    tight=(supply_cushion<=500),really_tight=(supply_cushion<200),he_fac=as.factor(he),pctl_fac=factor(percentile),
    peak_fac=factor(on_peak),
    out_mkt=(bid>forecast_pool_price)*(bid-forecast_pool_price),
    policy=case_when(year(date)<2016 ~ "SGER_15",
                     year(date)==2016 ~ "SGER_20",
                     year(date)==2017 ~ "SGER_30",
                     year(date)==2018 ~ "CCIR_30",
                     year(date)==2019 ~ "CCIR_30",
                     year(date)==2020 ~ "TIER_30",
                     year(date)==2021 ~ "TIER_40",
                     year(date)==2022 ~ "TIER_50",
                     year(date)==2023 ~ "TIER_65",
                     year(date)==2024 ~ "TIER_80",
                     TRUE ~ "TIER_80")
  )


all_plants<-all_plants %>% left_join(offer_test %>% select(-total))%>%
    mutate(net=ctax-oba,
    #plant=fct_other(plant,keep=c("SCGT","NGCC","COAL")),
    #plant=fct_relevel(plant,"Other"),
    ail_error=actual_ail-day_ahead_forecasted_ail,
    #offer=fct_other(offer,keep=c("Balancing Pool","Other"),other_level = "Other key"),
    #offer=fct_relevel(offer,"Other")
    )


base_formula <- bid ~
  plants+
  offer+
  cr_key+
  cr_nbp+
  #cr_nz+
  #cr_nz_nbp+
  poly(hourly_renewables,2,raw=TRUE)+
  poly(out_mkt,2,raw=TRUE)+
  #as.factor(yearmonth)+
  poly(supply_cushion,2,raw=TRUE)+
  forecast_pool_price+
  day_ahead_forecasted_ail+
  nit_settle_cad_gj+
  total_import_capability+
  total_export_capability+
  poly(nit_settle_cad_gj,2,raw = TRUE)+
  hdd_YEG+hdd_YYC+hdd_YMM+cdd_YEG+cdd_YMM+cdd_YYC

peak_adj<-"peak_fac+
  #peak_fac/as.factor(yearmonth)+
  peak_fac/poly(supply_cushion,3,raw=TRUE)"
  

no_peak_adj<-"
  #as.factor(yearmonth)+
  poly(supply_cushion,3,raw=TRUE)+
  nit_settle_cad_gj"


#all effects, peak adjustment, separate ctax and oba
this_formula <- update(base_formula, paste(". ~ . +", peak_adj,"+ ctax + oba"))

#sample_share<-.15
all_plants_reg<-all_plants %>% #sample_n(nrow(.)*sample_share, replace = F)%>% #no replacement - don't duplicate
  #mutate(offer=fct_relevel(offer,"Other"))%>%
  nest(data = -c(percentile)) %>%
  mutate(fit = map(data, ~ lm(this_formula, data = .x)),
         #key_marginals = map2(fit, data, ~margins_summary(.x, data = .y,variables=c("ctax","oba"))),
         #tidied = map(fit, tidy,conf.int = T),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
         #augmented = map(fit, augment)
  )%>%unnest(tidied)%>%
  select(-data,-fit)

#test<-all_plants %>% group_by(percentile)%>% summarize(min=min(bid),max=max(bid))

ctax_all_no_peaks<-all_plants_reg %>%
  filter(grepl("ctax",term)|grepl("oba",term)) %>%
  mutate(peak="All Hours",
         Plant_Type="All Plants")


#library(hrbrthemes)
#hrbrthemes::import_roboto_condensed()

ctax_all_no_peaks%>% #filter(percentile<=90 & percentile>=40)%>%
  filter(grepl("ctax",term)|grepl("oba",term))%>%ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.75),linewidth=.85)+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks(10))+
  expand_limits(x=0)+
  expand_limits(y=c(-0.7,0.7))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  #facet_grid(cols=vars(Plant_Type),rows = vars(peak))+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh in cost or value)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  theme_ps()+
  scale_color_manual("",values=c("black","grey50"),labels=c("Marginal effect of carbon tax cost","Marginal effect of OBA value"))+
  guides(color= guide_legend(nrow = 1,byrow = F))+
  NULL
ggsave(filename = "images/all_plants_no_peaks.png",dpi=300,width = 14, height=8)


#separate pass-through in peak hours


#all effects, peak adjustment, separate ctax and oba
this_formula <- update(base_formula, paste(". ~ . +", peak_adj,"-peak_fac + peak_fac:ctax + peak_fac:oba"))

#sample_share<-.15
all_plants_peak<-all_plants %>% #sample_n(nrow(.)*sample_share, replace = F)%>% #no replacement - don't duplicate
  #mutate(offer=fct_relevel(offer,"Other"))%>%
  nest(data = -c(percentile)) %>%
  mutate(fit = map(data, ~ lm(this_formula, data = .x)),
         #key_marginals = map2(fit, data, ~margins_summary(.x, data = .y,variables=c("ctax","oba"))),
         #tidied = map(fit, tidy,conf.int = T),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
         #augmented = map(fit, augment)
  )%>%unnest(tidied)%>%
  select(-data,-fit)


ctax_peak<-all_plants_peak %>%
  filter(grepl("ctax",term)|grepl("oba",term)) %>%
  mutate(peak=ifelse(grepl("peak_facTRUE",term),"Peak Hours","Off-Peak Hours"),
         Plant_Type="All Plants")

#library(hrbrthemes)
#hrbrthemes::import_roboto_condensed()
ctax_peak%>% filter(percentile<=100 & percentile>=0)%>%
  filter(grepl("ctax",term)|grepl("oba",term))%>%
  mutate(term=gsub("peak_facFALSE:","",term))%>%
  mutate(term=gsub("peak_facTRUE:","",term))%>%
  mutate(peak2=fct_relevel(peak,"Peak Hours"))%>%
  ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point(size=1.25)+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.75),size=.85)+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=c(-.5,.5))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(x=c(0,100))+
  facet_grid(rows = vars(peak2))+
  #facet_grid(rows=fct_relevel(peak,"Peak Hours"))+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh in cost or value)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  theme_ps()+
  scale_color_manual("",values=c("black","grey50"),labels=c("Marginal effect of carbon tax cost","Marginal effect of OBA value"))+
  #scale_shape_manual("",values=c(6,7),labels=c("Marginal effect of carbon tax cost","Marginal effect of OBA value"))+
  guides(color= guide_legend(nrow = 1,byrow = F))+
  NULL
ggsave(filename = "images/all_plants_peaks.png",dpi=150,width = 14, height=9)



#net effect only

#all effects, peak adjustment, net compliance cost
this_formula <- update(base_formula, paste(". ~ . +", peak_adj,"-peak_fac + peak_fac:net"))

all_plants_net<-all_plants %>%
  nest(data = -c(percentile)) %>%
  mutate(fit = map(data, ~ lm(this_formula
                              , data = .x)),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
  )%>%
  unnest(tidied)%>%
  select(-data,-fit)
  

net_all<-all_plants_net %>%
  filter(grepl("net",term)) %>%
  mutate(peak=ifelse(grepl("peak_facTRUE",term),"Peak Hours","Off-Peak Hours"),
         Plant_Type="All Plants")

net_all%>% #filter(percentile<=95 & percentile>=40)%>%
  mutate(term=gsub("peak_facFALSE:","",term))%>%
  mutate(term=gsub("peak_facTRUE:","",term))%>%
  filter(grepl("net",term))%>%ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.75),linewidth=.85)+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  #expand_limits(x=0)+
  expand_limits(y=c(-.55,.55))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  facet_grid(rows = vars(peak))+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh in cost or value)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  theme_ps()+
  scale_color_manual("",values=c("black"),labels=c("Marginal effect of net carbon pricing cost, off-peak hours","Marginal effect of net carbon pricing cost, peak hours"))+
  #guides(color= guide_legend(nrow = 2,byrow = F))+
  guides(color= "none")+
  NULL
ggsave(filename = "images/all_plants_net_peak.png",dpi=150,width = 14, height=8)



# FLEX

#policy+
#peak_fac+
# plants+
#   offer+
#   net+
#   cr_key+
#   cr_nbp+
#   cr_nz+
#   cr_nz_nbp+
#   poly(hourly_renewables,2,raw=TRUE)+
#   #peak_fac/ctax+
#   #peak_fac/oba+
#   #net+
#   poly(out_mkt,2,raw=TRUE)+
#   as.factor(yearmonth)+
#   poly(supply_cushion,2,raw=TRUE)+
#   forecast_pool_price+
#   #day_ahead_forecasted_ail+
#   ail_error+
#   total_import_capability+
#   total_export_capability+
#   poly(nit_settle_cad_gj,2,raw = TRUE)+
#   hdd_YEG+hdd_YYC+hdd_YMM+cdd_YEG+cdd_YMM+cdd_YYC


this_formula <- update(base_formula, paste(". ~ . +", peak_adj,"-peak_fac + peak_fac:ctax + peak_fac:oba"))

#sample_share<-.15
all_plants_peak<-all_plants %>% #sample_n(nrow(.)*sample_share, replace = F)%>% #no replacement - don't duplicate
  #mutate(offer=fct_relevel(offer,"Other"))%>%
  nest(data = -c(percentile)) %>%
  mutate(fit = map(data, ~ lm(this_formula, data = .x)),
         #key_marginals = map2(fit, data, ~margins_summary(.x, data = .y,variables=c("ctax","oba"))),
         #tidied = map(fit, tidy,conf.int = T),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
         #augmented = map(fit, augment)
  )%>%unnest(tidied)%>%
  select(-data,-fit)




this_formula <- update(base_formula, paste(". ~ . +", no_peak_adj,"+ net"))

all_plants_flex<-all_plants %>% 
nest(data = -c(percentile,peak_fac)) %>%
  mutate(fit = map(data, ~ lm(this_formula, data = .x)),
         #key_marginals = map2(fit, data, ~margins_summary(.x, data = .y,variables=c("ctax","oba"))),
         #tidied = map(fit, tidy,conf.int = T),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
         #augmented = map(fit, augment)
  )%>%unnest(tidied)%>%
  select(-data,-fit)



flex_all<-all_plants_flex %>%
  filter(grepl("net",term)) %>%
  mutate(peak=ifelse(peak_fac==TRUE,"Peak Hours","Off-Peak Hours"),
         peak=as_factor(peak),peak=fct_relevel(peak,"Peak Hours"),
         Plant_Type="All Plants")


flex_all%>% #filter(percentile<=95 & percentile>=40)%>%
  mutate(term=gsub("offer","",term))%>%
  mutate(term=gsub(":net","",term))%>%
  #mutate(term=gsub("peak_facTRUE:","",term))%>%
  #filter(grepl("net",term))%>%
  ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.75),linewidth=.85)+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  #expand_limits(x=0)+
  expand_limits(y=c(-.7,.7))+
  expand_limits(x=c(0,100))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  facet_grid(rows = vars(peak))+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh in net carbon policy costs)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  #paper_theme()+
  theme_ps()+
  #theme_ipsum() + theme(legend.position = "bottom")+
  scale_color_manual("",values=c("black"),labels=c("Marginal effect of net carbon pricing cost, off-peak hours","Marginal effect of net carbon pricing cost, peak hours"))+
  #scale_color_manual("",values=colors_tableau10())+
  
  #guides(color= guide_legend(nrow = 2,byrow = F))+
  guides(color= "none")+
  NULL
ggsave(filename = "images/all_plants_net_peak_flex.png",dpi=150,width = 14, height=8)

rm(merit_bids_all)
rm(all_plants)

#by plant fuel: coal and gas

bids_files<- list.files("data/") %>% as_tibble() %>% filter(grepl("synth_type",value))%>%
  mutate(file_date=gsub("synth_type_","",value),
         file_date=ymd_hm(gsub(".RData","",file_date))
  )%>% filter(file_date==max(file_date))

load(file = paste("data/",bids_files$value,sep=""))
paste("loaded file=data/",bids_files$value,sep="")
by_type<-merit_aug%>%ungroup()%>%
  mutate(plants=as.character(plants),
         offer=as.character(offer)
  )
rm(merit_aug)

#pick 10% of the date/he combos at random
#sample_share<-.15

by_type <- by_type %>%
  na.omit()%>%
  mutate(
    net=ctax-oba,
    gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
    year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
    tight=(supply_cushion<=500),really_tight=(supply_cushion<200),he_fac=as.factor(he),pctl_fac=factor(percentile),
    peak_fac=factor(on_peak),
    out_mkt=(bid>forecast_pool_price)*(bid-forecast_pool_price),
    policy=case_when(year(date)<2016 ~ "SGER_15",
                     year(date)==2016 ~ "SGER_20",
                     year(date)==2017 ~ "SGER_30",
                     year(date)==2018 ~ "CCIR_30",
                     year(date)==2019 ~ "CCIR_30",
                     year(date)==2020 ~ "TIER_30",
                     year(date)==2021 ~ "TIER_40",
                     year(date)==2022 ~ "TIER_50",
                     year(date)==2023 ~ "TIER_65",
                     year(date)==2024 ~ "TIER_80",
                     TRUE ~ "TIER_80")
  )



by_type<-by_type %>% left_join(offer_test %>% select(-total,-time))%>%    
  mutate(net=ctax-oba,
           #plant=fct_other(plant,keep=c("SCGT","NGCC","COAL")),
           #plant=fct_relevel(plant,"Other"),
           ail_error=actual_ail-day_ahead_forecasted_ail,
           #offer=fct_other(offer,keep=c("Balancing Pool","Other"),other_level = "Other key"),
           #offer=fct_relevel(offer,"Other")
    )


#unique(by_type$Plant_Type)

fossils<-c("GAS","COAL")


#
this_formula <- update(base_formula, paste(". ~ . +", peak_adj,"+ net - plants"))
#this_formula <- base_formula


reg_by_type<-by_type %>% filter(Plant_Type %in% fossils)%>%
  nest(data = -c(percentile,Plant_Type)) %>%
  mutate(fit = map(data, ~ lm(this_formula, data = .x)),
         #key_marginals = map2(fit, data, ~margins_summary(.x, data = .y,variables=c("ctax","oba"))),
         #tidied = map(fit, tidy,conf.int = T),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
         #augmented = map(fit, augment)
  )%>%unnest(tidied)%>%
  select(-data,-fit)


ctax_type<-reg_by_type %>% 
  filter(grepl("net",term)) %>%
  mutate(peak="All Hours")




ctax_type%>% #filter(percentile<=95) %>%
  mutate(Plant_Type=case_when(                 Plant_Type=="GAS"~"Natural Gas",
                                               Plant_Type=="SCGT"~"Natural Gas, Simple Cycle",
                                               Plant_Type=="NGCC"~"Natural Gas, Combined Cycle",
                                               Plant_Type=="COAL"~"Coal",
  ))%>%
  ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  geom_errorbar(width=rel(.75),size=.85)+
  geom_hline(yintercept = 1,linetype="dotted")+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(x=c(0,100))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  #expand_limits(y=c(-6,6))+
  facet_grid(rows=vars(Plant_Type))+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh of carbon policy cost)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  theme_ps()+
  theme(panel.spacing = unit(3, "lines"))+
  #theme_ipsum() + theme(legend.position = "bottom")+
  scale_color_manual("",values=c("black"),labels=c("Marginal effect of net carbon policy cost"))+
  guides(color= "none")+
  NULL
ggsave(filename = "images/by_fuel_net.png",dpi=120,width = 14, height=14)

rm(merit_bids_type,by_type)


#by offer control

bids_files<- list.files("data/") %>% as_tibble() %>% filter(grepl("synth_offer",value))%>% filter(!grepl("type",value))%>%
  mutate(file_date=gsub("synth_offer_","",value),
         file_date=ymd_hm(gsub(".RData","",file_date))
  )%>% filter(file_date==max(file_date))

load(file = paste("data/",bids_files$value,sep=""))
paste("loaded file=data/",bids_files$value,sep="")
merit_bids_offer<-merit_aug%>%ungroup()%>%
  mutate(plants=as.character(plants),
         offer=as.character(offer)
  )
rm(merit_aug)


#pick 10% of the date/he combos at random
#sample_share<-.15
#merit_bids_offer<-merit_bids_offer%>% #sample_n(nrow(.)*sample_share, replace = F) #no replacement - don't duplicate

# regression on all the plants
by_offer <- merit_bids_offer %>%  filter(year>=2013)%>%
  mutate(net=ctax-oba)%>%
  #select(bid,percentile,offer_gen,ctax,oba,net,supply_cushion,hourly_renewables,hour,on_peak,supply_cushion,day_ahead_forecasted_ail,forecast_pool_price,
  #       day_ahead_forecasted_ail,total_export_capability,total_import_capability,nit_settle_cad_gj,hdd_YEG,hdd_YMM,hdd_YYC,
  #       cdd_YEG,cdd_YMM,cdd_YYC,year,month_fac,he)%>%
  na.omit()%>%
  mutate(gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
         year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
         tight=(supply_cushion<=500),really_tight=(supply_cushion<200),he_fac=as.factor(he),pctl_fac=factor(percentile),
         peak_fac=factor(on_peak),
         out_mkt=(bid>forecast_pool_price)*(bid-forecast_pool_price) )
by_offer<-by_offer %>% left_join(offer_test %>% select(-total,-time))%>%    
  mutate(net=ctax-oba,
         #plant=fct_other(plant,keep=c("SCGT","NGCC","COAL")),
         #plant=fct_relevel(plant,"Other"),
         ail_error=actual_ail-day_ahead_forecasted_ail,
         #offer=fct_other(offer,keep=c("Balancing Pool","Other"),other_level = "Other key"),
         #offer=fct_relevel(offer,"Other")
  )


this_formula <- update(base_formula, paste(". ~ . +", peak_adj,"+ net - plants - offer"))

offer_reg<-by_offer %>% #filter(Plant_Type %in%fossils) %>%
  #filter(percentile<=95)%>%
  nest(data = -c(percentile,offer_gen)) %>%
  mutate(fit = map(data, ~ lm(this_formula
                              , data = .x)),
         #key_marginals = map2(fit, data, ~margins_summary(.x, data = .y,variables=c("ctax","oba"))),
         #tidied = map(fit, tidy,conf.int = T),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
         #augmented = map(fit, augment)
  ) %>%
  unnest(tidied)%>%
  select(-data,-fit)


offer_co<-offer_reg%>%
  filter(grepl("net",term)) %>%
  mutate(peak=ifelse(grepl("peak_facTRUE",term),"Peak Hours","Off-Peak Hours"),
         Plant_Type="All Plants")

offer_co%>% #filter(percentile<95,offer_gen %in% c("ENMAX"))%>%
  ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  geom_hline(yintercept = 1,linetype="dotted")+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.5))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(x=0)+
  expand_limits(y=c(-.55,.55))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  facet_grid(row=vars(offer_gen),scales="free_y")+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh of net carbon policy cost)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  theme_ps()+
  theme(panel.spacing = unit(1, "lines"))+
  #theme_ipsum() + theme(legend.position = "bottom")+
  scale_color_manual("",values=c("black"),labels=c("Marginal effect of net carbon policy cost"))+
  guides(color= "none")+
  NULL
ggsave(filename = "images/offers.png",dpi=290,width = 14, height=10,bg = "transparent")


offer_co%>% filter(offer_gen %in% c("Balancing Pool"))%>%
  ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  geom_hline(yintercept = 1,linetype="dotted")+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.5))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(x=0)+
  expand_limits(y=c(-.55,.55))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  facet_grid(row=vars(offer_gen),scales="free_y")+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh of net carbon policy cost)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  theme_ps()+
  theme(panel.spacing = unit(1, "lines"))+
  #theme_ipsum() + theme(legend.position = "bottom")+
  scale_color_manual("",values=c("black"),labels=c("Marginal effect of net carbon policy cost"))+
  guides(color= "none")+
  NULL
ggsave(filename = "images/offers_bp.png",dpi=290,width = 14, height=6,bg = "transparent")





this_formula <- update(base_formula, paste(". ~ . +", peak_adj,"+ net - plants - offer"))

offer_reg_2020<-by_offer %>% filter(date<ymd("2020-03-01")) %>%
  #filter(percentile<=95)%>%
  nest(data = -c(percentile,offer_gen)) %>%
  mutate(fit = map(data, ~ lm(this_formula
                              , data = .x)),
         #key_marginals = map2(fit, data, ~margins_summary(.x, data = .y,variables=c("ctax","oba"))),
         #tidied = map(fit, tidy,conf.int = T),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
         #augmented = map(fit, augment)
  ) %>%
  unnest(tidied)%>%
  select(-data,-fit)


 offer_reg_2020%>%
  filter(grepl("net",term)) %>%
  mutate(peak=ifelse(grepl("peak_facTRUE",term),"Peak Hours","Off-Peak Hours"),
  Plant_Type="All Plants")%>%
  ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  geom_hline(yintercept = 1,linetype="dotted")+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.5))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(x=0)+
  expand_limits(y=c(-.55,.55))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  facet_grid(row=vars(offer_gen),scales="free_y")+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh of net carbon policy cost)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  theme_ps()+
  theme(panel.spacing = unit(1, "lines"))+
  #theme_ipsum() + theme(legend.position = "bottom")+
  scale_color_manual("",values=c("black"),labels=c("Marginal effect of net carbon policy cost"))+
  guides(color= "none")+
  NULL
ggsave(filename = "images/offers_pre_2020.png",dpi=290,width = 14, height=10,bg = "transparent")





#library(hrbrthemes)
#hrbrthemes::import_roboto_condensed()
offer_co%>% filter(percentile<100,
                   offer_gen %in% c("ENMAX","Capital Power","TransAlta","Heartland","Balancing Pool"),
                   TRUE)%>%
  filter(grepl("net",term))%>%ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  geom_hline(yintercept = 1,linetype="dotted")+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.5))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  #expand_limits(x=0)+
  #expand_limits(y=c(-.55,.55))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  facet_grid(row=vars(offer_gen))+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh of net carbon policy cost)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  theme_ps()+
  #theme(panel.spacing = unit(3, "lines"))+
  #theme_ipsum() + theme(legend.position = "bottom")+
  scale_color_manual("",values=c("black"),labels=c("Marginal effect of net carbon policy cost"))+
  guides(color= "none")+
  NULL
ggsave(filename = "images/offer.png",width = 14, height=15,dpi=120)



#by unit

bids_files<- list.files("data/") %>% as_tibble() %>% filter(grepl("synth_focus",value))%>%
  mutate(file_date=gsub("synth_focus_","",value),
         file_date=ymd_hm(gsub(".RData","",file_date))
  )%>% filter(file_date==max(file_date))

load(file = paste("data/",bids_files$value,sep=""))
paste("loaded file=data/",bids_files$value,sep="")
merit_bids_unit<-merit_aug

#pick 10% of the date/he combos at random
#sample_share<-.15
#merit_bids_unit<-merit_bids_unit%>% sample_n(nrow(.)*sample_share, replace = F) #no replacement - don't duplicate

# regression on all the plants
by_unit <- merit_bids_unit %>%  #filter(offer_gen %in% c("GN3"))%>%
  select(bid,percentile,offer_gen,ctax,oba,supply_cushion,hourly_renewables,hour,on_peak,supply_cushion,day_ahead_forecasted_ail,forecast_pool_price,
         day_ahead_forecasted_ail,total_export_capability,total_import_capability,nit_settle_cad_gj,hdd_YEG,hdd_YMM,hdd_YYC,
         cdd_YEG,cdd_YMM,cdd_YYC,year,month_fac,he,time)%>%
  na.omit()%>%
  mutate(net=ctax-oba,gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
         year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
         tight=(supply_cushion<=500),really_tight=(supply_cushion<200),he_fac=as.factor(he),pctl_fac=factor(percentile),
         peak_fac=factor(on_peak),
         out_mkt=(bid>forecast_pool_price)*(bid-forecast_pool_price) )





bids_br5<-by_unit%>%filter(time>ymd("2016-01-01")-months(1),time<ymd("2016-01-01")+months(1),on_peak==TRUE)%>%
  mutate(ppa=ifelse(time<ymd("2016-01-01"),"Offer Control with PPA Owner","Offer Control with Balancing Pool"),
         ppa=as_factor(ppa),ppa=fct_relevel(ppa,"Offer Control with PPA Owner"))%>%
  group_by(percentile,ppa)%>%
  summarize(avg_bid=mean(bid),
            max_bid=max(bid),
            min_bid=min(bid))


ggplot(bids_br5)+
  geom_line(aes(percentile,avg_bid,group=ppa),color="black",size=1.25)+
  geom_ribbon(aes(percentile,ymin=min_bid,ymax=max_bid,group=ppa),fill="grey20",alpha=0.5,color="black",size=1.25)+
  facet_grid(cols = vars(ppa))+
  scale_color_manual("",values=c("black","grey50","grey80"))+
  scale_linetype_manual("",values=c("solid","11"))+
  #expand_limits(y=c(0,100))+
  #expand_limits(x=ymd("2020-3-01"))+
  guides(color="none")+
  scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
  scale_x_continuous(expand = c(0,0),breaks=pretty_breaks())+
  paper_theme()+
  theme(panel.spacing = unit(2,"lines"),
        legend.key.width = unit(3,"lines"),
        strip.text = element_text(
          size = 14, color = "black",# face = "bold"
        ),
        
        title = element_text(
          size = 14, color = "black",# face = "bold"
        ),
        NULL
  )+
  labs(x="Percentile of Offered Power",y=expression("Average and Range of Peak-hour Offers"*phantom(1)*'($/MWh)'),
       title = "Battle River 5 (BR5) Power Offers, December 1, 2015 through January 31, 2016",
       NULL
  )+
  NULL
ggsave("images/bp_bids.png",dpi=300,width=14,height=7)



bids_sunc<-by_unit%>%filter(time>ymd("2016-03-1")-years(1),time<ymd("2016-3-31")+years(1),on_peak==TRUE)%>%
  mutate(ppa=ifelse(time<ymd("2016-03-7"),"Offer Control with PPA Owner","Offer Control with Balancing Pool"),
         ppa=as_factor(ppa),ppa=fct_relevel(ppa,"Offer Control with PPA Owner"))%>%
  group_by(percentile,ppa)%>%
  summarize(avg_bid=mean(bid),
            max_bid=max(bid),
            min_bid=min(bid))


ggplot(bids_sunc)+
  geom_line(aes(percentile,avg_bid,group=ppa),color="black",size=1.25)+
  geom_ribbon(aes(percentile,ymin=min_bid,ymax=max_bid,group=ppa),fill="grey20",alpha=0.5,color="black",size=1.25)+
  facet_grid(cols = vars(ppa))+
  scale_color_manual("",values=c("black","grey50","grey80"))+
  scale_linetype_manual("",values=c("solid","11"))+
  #expand_limits(y=c(0,100))+
  #expand_limits(x=ymd("2020-3-01"))+
  guides(color="none")+
  scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
  scale_x_continuous(expand = c(0,0),breaks=pretty_breaks())+
  paper_theme()+
  theme(panel.spacing = unit(2,"lines"),
        legend.key.width = unit(3,"lines"),
        strip.text = element_text(
          size = 14, color = "black",# face = "bold"
        ),
        
        title = element_text(
          size = 14, color = "black",# face = "bold"
        ),
        NULL
  )+
  labs(x="Percentile of Offered Power",y=expression("Average and Range of Peak-hour Offers"*phantom(1)*'($/MWh)'),
       title = "Sheerness PPA Power Offers, March, 2015 through March, 2017",
       NULL
  )+
  NULL
ggsave("images/bp_bids_sunc.png",dpi=300,width=14,height=7)




unit_reg<-by_unit %>% #filter(Plant_Type %in%fossils) %>%
  nest(data = -c(percentile)) %>%
  mutate(fit = map(data, ~ lm(bid ~
                                poly(hourly_renewables,3,raw=TRUE)+
                                #peak_fac/ctax+
                                #peak_fac/oba+
                                peak_fac+
                                net+
                                poly(out_mkt,3,raw=TRUE)+
                                peak_fac/as.factor(yearmonth)+
                                peak_fac/poly(supply_cushion,3,raw=TRUE)+
                                forecast_pool_price+
                                day_ahead_forecasted_ail+
                                total_import_capability+
                                total_export_capability+
                                peak_fac/nit_settle_cad_gj+
                                hdd_YEG+hdd_YYC+hdd_YMM+cdd_YEG+cdd_YMM+cdd_YYC
                              , data = .x)),
         #key_marginals = map2(fit, data, ~margins_summary(.x, data = .y,variables=c("ctax","oba"))),
         #tidied = map(fit, tidy,conf.int = T),
         tidied = map(fit, tidy,conf.int = T),
         glanced = map(fit, glance),
         #augmented = map(fit, augment)
  )

unit_reg_data<-unit_reg %>% unnest(tidied)%>%
  select(-data,-fit)%>%
  filter(grepl("net",term)) %>%
  mutate(peak="All Hours")



unit_reg_data%>% filter(percentile<=95)%>%
  ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  geom_errorbar(width=rel(.75),size=.85)+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(x=c(0,100))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  #expand_limits(y=c(-6,6))+
  #facet_grid(rows=vars(offer_gen))+
  geom_hline(yintercept = 0, col = "black") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh of carbon policy cost)",
    #title = "Marginal effect of carbon tax cost and output-based allocation values on power offers by plant type"
    #subtitle = "Conditional on plant type"
  ) +
  paper_theme()+
  theme(panel.spacing = unit(3, "lines"))+
  #theme_ipsum() + theme(legend.position = "bottom")+
  scale_color_manual("",values=c("black"),labels=c("Marginal effect of net carbon policy cost"))+
  guides(color= "none")+
  NULL
ggsave(filename = "images/by_unit.png",dpi=120,width = 14, height=14)




unit_co_gas<-unit_reg %>%
  unnest(tidied)%>%
  select(-data,-fit)%>%
  filter(grepl("gj",term)) %>%
  mutate(peak="All Hours",
         Plant_Type="All Plants"
  )



unit_co<-unit_reg %>%
  unnest(tidied)%>%
  select(-data,-fit)%>%
  filter(grepl("net",term)) %>%
  mutate(peak="All Hours",
         Plant_Type="All Plants"
  )

#library(hrbrthemes)
#hrbrthemes::import_roboto_condensed()
unit_co%>% filter(percentile<100)%>%
  filter(grepl("net",term))%>%ggplot(aes(x=percentile, y=estimate, ymin=conf.low, ymax=conf.high,group=term,color=term)) +
  #geom_pointrange() +
  #geom_line(size=1.25)+
  geom_point()+
  #geom_errorbar(width=2.85)+
  geom_errorbar(width=rel(.5))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks())+
  #expand_limits(x=0)+
  expand_limits(y=c(-.55,.55))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  #facet_grid(cols=vars(offer_gen),rows = vars(peak))+
  facet_wrap(~offer_gen)+
  geom_hline(yintercept = 0, col = "orange") +
  labs(
    x = "Percentile of total offered power (%)", y = "Marginal effect (Δ in offer : Δ in $/MWh in cost or value)",
    title = "Marginal effect of carbon tax cost and output-based allocation value on power offers by controlling entity"
    #subtitle = "Conditional on plant type"
  ) +
  paper_theme()+
  #theme_ipsum() + theme(legend.position = "bottom")+
  scale_color_manual("",values=c(colors_tableau10_light()[1:2],colors_tableau10()[1:2]),
                     labels=c("Marginal effect of carbon tax cost, off-peak hours","Marginal effect of OBA value, off-peak hours",
                              "Marginal effect of carbon tax cost, peak hours","Marginal effect of OBA value, peak hours"))+
  guides(color= guide_legend(nrow = 2,byrow = F))+
  NULL
ggsave(filename = "images/enmax.png",dpi=150,width = 14, height=8)






library(hrbrthemes) ## theme(s) I like

tidy(fit2, conf.int = T) %>%
  filter(grepl("wt", term)) %>%
  ## Optional regexp work to make plot look nicier
  mutate(
    am = ifelse(grepl("am1", term), "Automatic", "Manual"),
    vs = ifelse(grepl("vs1", term), "V-shaped", "Straight"),
    x_lab = paste(am, vs, sep="\n")
  ) %>%
  ggplot(aes(x=x_lab, y=estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, col = "orange") +
  labs(
    x = NULL, y = "Marginal effect (Δ in MPG : Δ in '000 lbs)",
    title = " Marginal effect of vehicle weight on MPG",
    subtitle = "Conditional on transmission type and engine shape"
  ) +
  theme_ipsum()




margins_summary(model=lm_fit$fit[[1]], data = lm_fit$data[[1]])



list_data <- split(mtcars%>%mutate(am=as.factor(am)), mtcars$gear)

list_mod <- lapply(list_data, function(x) lm(mpg ~ am/wt, data = x))


mapply(margins_summary, model = list_mod, data = list_data, SIMPLIFY = FALSE)


#myworkflow

library(magrittr)
# toy data
test_data <- data.frame(region = sample(letters[1:3], 1000, replace = TRUE),
                        x = sample(0:1, 1000, replace = TRUE),
                        y = sample(1:100, 1000, replace = TRUE),
                        z = sample(0:1, 1000, replace = TRUE)) %>%
  arrange(region)

# nest
by_region <-
  test_data %>% nest(data=-region)%>%
  mutate(mod_rat= map(data, ~ lm(x ~ y + z,data=.x)))%>%
  mutate(marginals      = map2(mod_rat, data, ~margins_summary(.x, data = .y)))


by_region <-
  test_data %>% nest(data=-region)%>%
  mutate(mod_rat= map(data, ~ lm(x ~ z + poly(y,2),data=.x)))%>%
  mutate(marginals = map2(mod_rat, data, ~margins_summary(.x, data = .y)))


by_region <-
  test_data %>% nest(data=-region)%>%
  mutate(mod_rat= map(data, ~ lm(x ~ z + stats::poly(y,2),data=.x)))%>%
  mutate(marginals = map2(mod_rat, data, ~margins_summary(.x, data = .y)),
         marginal_key = map2(mod_rat, data, ~margins_summary(.x, data = .y,variables=c("z"))))



#mcdermott's method


summary(lm(mpg ~ factor(am) / wt, data = mtcars))




lm_fit<-all_plants %>%
  nest(data = -c(pctl_fac))









mapply(margins_summary, model = list_mod, data = list_data, SIMPLIFY = FALSE)









options(digits=2)
options(scipen = 10)
tax_impact<-lm_fit %>% #filter(as.numeric(as.character(pctl_fac))>50)%>%
  filter(grepl("ctax",term) | grepl("oba",term)) %>% mutate(p.value=round(as.numeric(p.value),3))%>%
  select(-statistic)%>%
  pivot_wider(id_cols = pctl_fac,names_from = term,values_from = c(estimate,std.error,p.value))






lmfit<-all_plants%>% nest(data = everything()) %>%
  mutate(
    fit = map(data, ~ lm(bid ~ pctl_fac+ctax+
                           oba +
                           pctl_fac/ctax+
                           pctl_fac/oba+
                           #poly(percentile,2)+
                           #poly(percentile,2)*ctax+
                           #poly(percentile,2)*oba+
                           poly(supply_cushion,2)+
                           poly(hourly_renewables,2)+
                           as.factor(hour)+
                           as.factor(yearmonth)+
                           on_peak+
                           #supply_cushion+
                           poly(supply_cushion,2)*on_peak+
                           day_ahead_forecasted_ail+
                           forecast_pool_price+
                           day_ahead_forecasted_ail+
                           total_import_capability+
                           total_export_capability+
                           nit_settle_cad_gj+hdd_YEG+hdd_YYC+hdd_YMM+cdd_YEG+cdd_YMM+cdd_YYC+
                           gas2+gas3, data = .x)),
    tidied = map(fit, tidy)
  ) %>%
  unnest(tidied)




lmfit <-lm(bid ~ pctl_fac/ctax, data = all_plants)
test<-tidy(lmfit)





lmfit <-lm(bid ~ ctax+
             oba+
             poly(percentile,4)+
             poly(percentile,4)*ctax+
             poly(percentile,4)*oba+
             #poly(supply_cushion,2)+
             poly(hourly_renewables,2)+
             #as.factor(hour)+
             as.factor(yearmonth)+
             #as.factor(yearmonth)*as.factor(hour)+
             on_peak+
             #supply_cushion+
             supply_cushion*on_peak+
             day_ahead_forecasted_ail+
             forecast_pool_price+
             day_ahead_forecasted_ail+
             total_import_capability+
             total_export_capability+
             nit_settle_cad_gj+hdd_YEG+hdd_YYC+hdd_YMM+cdd_YEG+cdd_YMM+cdd_YYC+
             gas2+gas3, data = all_plants)


coefs_1<-tidy(lmfit)


test<-all_plants %>%
  unnest(tidied)

test2<-all_plants %>%
  unnest(glanced)


test4<-by_type %>%
  unnest(pred, .drop = TRUE) %>% select(pred)%>% cbind(merit_fossils) %>% group_by(Plant_Type,percentile)%>%
  summarize(price=mean(price),pred=mean(pred))







merit_fossils<-merit_fossils %>%
  melt(id=id_names,variable.name="percentile",value.name = "price")%>%
  mutate(percentile=as.numeric(gsub("bid_","",as.character(percentile))))

merit_fossils<-merit_fossils %>% mutate(he=ifelse(he=="02*","02",he))


# Split into pieces, fit model to each piece
by_type <- merit_fossils %>%  mutate(p2=percentile^2,p3=percentile^3,p4=percentile^4,gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
                                     year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
                                     tight=(supply_cushion<=500),really_tight=(supply_cushion<200),he_fac=as.factor(he))%>%
  #nest(-c(Plant_Type,percentile)) %>%
  nest(-c(Plant_Type,offer_control)) %>%
  mutate(
    fit = map(data, ~ lm(price ~
                           poly(percentile,4)+
                           percentile*net+
                           #+p2+p3+p4+
                           poly(supply_cushion,2)+
                           poly(hourly_renewables,2)+
                           as.factor(hour)+
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
                         +gas2+gas3
                         #tight+really_tight#+
                         #yearmonth
                         , data = .x)),
    #pred=map(fit,predict),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
    #augmented = map(fit, augment)
  )

test<-by_type %>%
  unnest(tidied, .drop = TRUE)

test2<-by_type %>%
  unnest(glanced, .drop = TRUE)


test4<-by_type %>%
  unnest(pred, .drop = TRUE) %>% select(pred)%>% cbind(merit_fossils) %>% group_by(Plant_Type,percentile)%>%
  summarize(price=mean(price),pred=mean(pred))


test2<-by_type %>%
  unnest(augmented, .drop = TRUE)

data<-by_type %>%
  unnest(data, .drop = TRUE) %>% cbind(by_type %>% unnest(augmented, .drop = TRUE) %>%
                                         rename("Plant_Type_2"="Plant_Type") %>% select(.fitted,Plant_Type_2))


data2<-by_type %>%
  unnest(data, .drop = TRUE) %>% cbind(by_type %>% unnest(pred, .drop = TRUE) %>%
                                         rename("Plant_Type_2"="Plant_Type") %>% select(pred,Plant_Type_2))


averages<-ggplot(data)+
  geom_line(aes(percentile,price,color="Data"),fill=NA,size=1.5)+
  geom_line(aes(percentile,.fitted,color="Fixed Effects Model"),fill=NA,size=1.5)+
  facet_grid(~Plant_Type)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")
set_png(file="average_offers.png",width=1400,height = 850)
averages
dev.off()

df2<-merit_fossils %>% group_by(percentile,offer_control,year) %>% summarize(price=mean(price),
                                                                             min=min(price),
                                                                             max=max(price),
                                                                             sd_price=sd(price))

df3<-merit_fossils %>% group_by(percentile,offer_control,year,asset_id) %>% summarize(price=mean(price),
                                                                                      min=min(price),
                                                                                      max=max(price),
                                                                                      sd_price=sd(price))

df4<-merit_fossils %>% group_by(percentile,offer_control,year,he) %>% summarize(price=mean(price),
                                                                                min=min(price),
                                                                                max=max(price),
                                                                                sd_price=sd(price))


ggplot(df2)+
  geom_line(aes(percentile,price,color=offer_control),fill=NA,size=1.5)+
  facet_grid(~year)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")



ggplot(df3)+
  geom_line(aes(percentile,price,color=asset_id),fill=NA,size=1.5)+
  facet_grid(offer_control~year)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")



ggplot(df4)+
  geom_line(aes(percentile,price,color=offer_control),fill=NA,size=1.5)+
  facet_grid(year~he)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")





ggplot(filter(data2,asset_id=="EGC1",date==max(date),he=="15"))+
  geom_line(aes(percentile,price,color="Data"),fill=NA,size=1.5)+
  geom_line(aes(percentile,pred,color="Fixed Effects Model"),fill=NA,size=1.5)

+
  facet_grid(~Plant_Type)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")
set_png(file="average_offers.png",width=1400,height = 850)
averages
dev.off()


test3<-by_type %>%
  unnest(glanced, .drop = TRUE)


library(ggpubr)
ggerrorplot(test, x = "percentile", y = "estimate",
            desc_stat = "mean_ci", color = "Plant_Type")+
  facet_grid(~policy)


#ggplot(filter(test,term=="net", Plant_Type%in% c("COAL","COGEN")))+
ggplot(filter(test,Plant_Type %in% c("COAL","NGCC","SCGT")))+
  geom_point(aes(percentile,estimate,group=policy,color=policy),size=2)+
  geom_hline(aes(yintercept=0))+
  facet_grid(~Plant_Type)
scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values =colors_tableau10(),guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_bw()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x="Percentile of Capacity",y="Change in Average Offer ($/MWh per $ of carbon cost)",
       title="Impact of GHG Policies (2014-2019) on Average Offers of Capacity",
       #subtitle="Outline shows raw carbon tax cost, fill shows costs net OBA values",
       caption="Source: Estimated coefficients for net carbon cost on power price")






mods <- by_type %>% map(~ lm(price ~ supply_cushion+hourly_renewables, data = .))
map2(mods, by_type, predict)


#playing with peak, off peak

#by(merit_aug, merit_aug$on_peak, summary)

#skim(merit_aug)


#trial #2


# Split into pieces, fit model to each piece
by_type <- merit_aug %>% filter(Plant_Type %in% c("COAL","SCGT","NGCC","COGEN")) %>%
  mutate(gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
         year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
         tight=(supply_cushion<=500),really_tight=(supply_cushion<200))%>%
  nest(-c(Plant_Type)) %>%
  #nest(-c(Plant_Type)) %>%
  mutate(
    fit = map(data, ~ lm(price ~
                           poly(percentile, 3)+
                           poly(percentile, 3)*ctax_cost+
                           #asset_id+
                           #poly(percentile, 3)*oba_val+
                           poly(supply_cushion,2)+
                           poly(hourly_renewables,2)+
                           #on_peak+
                           #supply_cushion+
                           #supply_cushion*on_peak+
                           #day_ahead_forecasted_ail+
                           #forecasted_actual_ail_difference+
                           #total_import_capability+
                           #total_export_capability+
                           poly(nit_settle_cad_gj,3)+
                           net+tight+really_tight+
                           year_fac
                         , data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    #augmented = map(fit, augment)
  )

test<-by_type %>%
  unnest(tidied, .drop = TRUE)

test3<-by_type %>%
  unnest(glanced, .drop = TRUE)










merit_2018_07_11<-merit_aug %>% filter(date==as.Date("2018-07-11")&he=="04")%>%
  mutate(oba_adj=case_when(Plant_Fuel=="COAL"~ (.800),
                           Plant_Fuel=="GAS"~ (.370),
                           TRUE~0),
         bid_adj=case_when(block_number==0~ price,
                           block_number>0~ price+.370*30-oba_adj*30)
  )%>%
  group_by(date,he) %>% arrange(bid_adj,Plant_Type) %>% mutate(merit2=cumsum(size)) %>%
  ungroup()



#create a single-hour merit order file for analysis


day<-ymd("2018-04-26")
merit_day<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms=c(""))
singles<-seq(1,9)
for(hour in singles){
  merit_day$he[merit_day$he==hour]<-paste(0,hour,sep="")
  #  merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
}
merit_day$import_export<-as.character(merit_day$import_export)
merit_day<-merit_day %>%
  mutate(
    import_export=case_when(
      is.na(import_export) ~ "",
      TRUE~  import_export
    )
  ) %>% clean_merit_trade()




load("renew_vols.RData")

#merge renewable gen
merit_day<-merit_day%>%
  left_join(renew_vols,by=c("date","he","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  )

renew_vols<-NULL #drop it out of memory

#3 build hourly summary data
load("forecast_data.RData")


#load(file="metered_vols_data.Rdata" )

load(file="aeso_itc_data.Rdata" )


hourly_summary<-merit_day%>%
  group_by(date,he) %>% summarize(hourly_avail=sum(available_mw),
                                  hourly_dispatch=sum(available_mw*(dispatched=="Y")),
                                  hourly_imports=sum(available_mw*(import_export=="I")),
                                  hourly_exports=sum(available_mw*(import_export=="E")),
                                  hourly_renewables=sum(renew_gen,na.rm = T),
  ) %>%ungroup()


merit_day<-merit_day %>%
  left_join(itc_data,by=c("date","he")) %>% left_join(forecast_data,by=c("date","he")) %>%
  assign_peaks(time_var = time)%>%
  left_join(hourly_summary,by=c("date","he"))

merit_day<-merit_day %>% left_join(plant_data(),by=c("asset_id"="ID"))




merit_day_graph<-merit_day %>% filter(he=="18",import_export!="E")%>%
  mutate(oba_adj=case_when(Plant_Fuel=="COAL"~ (.800),
                           Plant_Fuel=="GAS"~ (.370),
                           TRUE~0),
         bid_adj=case_when(block_number==0~ price,
                           Plant_Type=="IMPORT"~price,
                           block_number>0~ price-co2_est/1000*30+oba_adj*30)
  )%>%
  group_by(date,he) %>% arrange(bid_adj,Plant_Type) %>% mutate(merit2=cumsum(size)) %>%
  ungroup()



set_png("merit_oba_ctax.png")
ggplot(arrange(merit_day_graph,bid_adj,Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=bid_adj,fill=Plant_Type))+
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=price,colour=Plant_Type),fill=NA)+
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
  #geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
  #geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
  geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3,10)])+
  scale_colour_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3,10)])+
  #scale_color_manual("",values=c("black","firebrick","blue"))+
  #scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,9500))+
  scale_y_continuous(expand=c(0,0),limits=c(-20,350))+
  ajl_line()+theme(plot.subtitle = element_text(size = 14))+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Alberta Energy Merit Order by Plant Type, ",max(merit_day_graph$date)," ",max(as.character(merit_day_graph$he)),":00",sep = ""),
       subtitle="Difference between solid fill and outline is net cost of carbon pricing net 370kg/MWh output-based allocation.",
       caption="Source: AESO Data, graph by Andrew Leach.")

dev.off()





#use bid data


# Split into pieces, fit model to each piece
by_type <- bids_aug %>%
  mutate(gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
         year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
         tight=(supply_cushion<=500),really_tight=(supply_cushion<200))%>%
  #nest(-c(Plant_Type,percentile)) %>%
  nest(-c(Plant_Type)) %>%
  mutate(
    fit = map(data, ~ lm(bid_50 ~
                           #+p2+p3+p4+
                           poly(supply_cushion,2)+
                           poly(hourly_renewables,2)+
                           net+ctax_cost+
                           #percentile+
                           #on_peak+
                           #supply_cushion+
                           #supply_cushion*on_peak+
                           #day_ahead_forecasted_ail+
                           #forecasted_actual_ail_difference+
                           #total_import_capability+
                           #total_export_capability+
                           nit_settle_cad_gj
                         +gas2+gas3+
                           tight+really_tight#+
                         #yearmonth
                         , data = .x)),
    #pred=map(fit,predict),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    #augmented = map(fit, augment)
  )

test<-by_type %>%
  unnest(tidied, .drop = TRUE)

test2<-by_type %>%
  unnest(glanced, .drop = TRUE)


test4<-by_type %>%
  unnest(pred, .drop = TRUE) %>% select(pred)%>% cbind(merit_fossils) %>% group_by(Plant_Type,percentile)%>%
  summarize(price=mean(price),pred=mean(pred))


# use half the iris data
ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))

library(dummies)


df5<-filter(merit_fossils,year>=2017,asset_id=="KH3")
df6<-filter(merit_fossils,year==2020,asset_id=="KH3",price>750)

test<-sample_n(ungroup(df5), 20000,replace=T)
test<-test %>% select(price,year,percentile,supply_cushion,hourly_renewables,hourly_imports,nit_settle_cad_gj)

test<-test %>% select(price,year,percentile,hourly_renewables,hourly_imports)
test<-cbind(test, dummy(test$year))


targets<-test %>% select(-price,-year)
goal<-test %>% select(price) %>% mutate(price=price/1000)
#ir1 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,
#            decay = 5e-4, maxit = 200)

ir1 <- nnet(targets, goal, size = 20 , rang = 0.8,
            decay = 5e-4, maxit = 2000)

test<-test %>% cbind(ir1$fitted.values)
names(test)[length(names(test))]<-"fitted_price"
test<-test %>% cbind(ir1$residuals)
names(test)[length(names(test))]<-"residuals"

ggplot(test)+geom_point(aes(percentile,residuals))

ggplot(test %>% group_by(percentile,year) %>% summarize(price=mean(price),fitted_price=mean(fitted_price)))+
  geom_line(aes(percentile,price,group=as.factor(year),colour=as.factor(year),linetype="Data"))+
  geom_line(aes(percentile,fitted_price*1000,group=as.factor(year),color=as.factor(year),
                linetype="ML Estimate"))




test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}
test.cl(targets[-samp,], predict(ir1, ir[-samp,]))



#Duration curves
load("renew_vols.RData")
renew_vols<-renew_vols %>% filter(year(date)>=2012) %>% mutate(Year=year(date))
renew_vols <- renew_vols %>% group_by(Year,date,he) %>% summarise(renew_gen=sum(renew_gen))
# Histogram of Generation Densities
set_png(file="wind_cdf.png")
ggplot(filter(renew_vols,Year<2019),aes(renew_gen))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=as.factor(Year),colour=as.factor(Year)),size=1.5)+
  scale_x_continuous(limits=range(renew_vols$renew_gen),expand=c(0,0))+
  scale_y_continuous()+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Wind Generation (MW)",y="Share of time wind power less than X MW",
       title="Cumulative Density Function, Wind Energy (2010-2018 Avg)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()


#Duration curves

hourly_summary<-merit_aug%>%
  group_by(date,he) %>% summarize(hourly_avail=sum(available_mw*(import_export=="")),
                                  hourly_dispatch=sum(dispatched_mw*(import_export=="")),
                                  hourly_imports=sum(dispatched_mw*(import_export=="I")),
                                  hourly_exports=sum(dispatched_mw*(import_export=="E")),
  ) %>%
  ungroup() %>% mutate(Year=year(date),net_imports=hourly_imports-hourly_exports,
                       supply_cushion=hourly_avail-hourly_dispatch)



supply_cushion<-hourly_summary %>% mutate(Month=month(date)) %>%
  group_by(Month,Year) %>% arrange(supply_cushion) %>%
  summarize(hours=max(row_number()),n_250=sum((supply_cushion<=250)),
            n_500=sum((supply_cushion<=500)),date=min(date)+days(14))

ggplot(data=supply_cushion) +
  geom_col(aes(date,n_500),size=1.5,fill="grey50",colour="grey50") +
  geom_col(aes(date,n_250),size=1.5,fill="black",colour="black") +
  
  scale_linetype_manual("",values = c(3,1))+
  scale_x_date(date_breaks = "6 months",labels = date_format("%b\n%Y", tz="America/Denver"),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  slide_theme()+theme(legend.key.width=unit(4,"line"))+
  labs(y="Number of hours with tight supply cushion",x="",
       title="Monthly Incidents of Tight Supply Cushions, 2012-2019)",
       caption="Source: AESO Data, Graph by Andrew Leach")
