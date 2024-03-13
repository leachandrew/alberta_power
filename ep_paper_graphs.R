source("power_paper_base.R")
source("aeso_scrapes.R")

library(directlabels)
library(cowplot)



#load(file="data/metered_vols_data.Rdata")
load(file="nrgstream/nrgstream_gen.Rdata") 
nrgstream_gen <- nrgstream_gen %>% clean_names()
  #ng_conv(id_sent=ID)%>%
  #rename(time=time)

renew_gen<-nrgstream_gen%>%filter(plant_type%in%c("WIND","SOLAR"))%>%
  select(time,plant_type,gen)%>%
  group_by(time,plant_type)%>%
  summarize(gen=sum(gen,na.rm = T))%>%
  group_by(time)%>%
  mutate(renew_gen=sum(gen,na.rm = T))%>%
  ungroup()%>%
  pivot_wider(names_from = plant_type,values_from=gen)



update_forecasts()
load(file="data/forecast_data.Rdata") 
forecast_data <- forecast_data %>% filter (he!="02*")

hourly_patterns<-forecast_data %>% 
  filter(!is.na(actual_posted_pool_price),!is.na(actual_ail))%>% 
  assign_date_time_days()%>%
  group_by(hour,year)%>%
  summarize(ail=mean(actual_ail),price=mean(actual_posted_pool_price))%>%
  mutate(hour=paste(hour,":00",sep=""),hour=factor(hour))

library(gghighlight)

hourly_graph<-  ggplot(hourly_patterns%>%filter(year>=2004,year<=2023)) +
  geom_line(aes(factor(hour),price,group=factor(year),color=factor(year)),size=.85)+
  geom_line(data=hourly_patterns %>% filter(year==2023),aes(factor(hour),price,group=factor(year),color=factor(year)),size=2.5)+
  scale_color_viridis("",option = "C",discrete = T,direction=-1)+
  scale_y_continuous(expand=c(0,0))+
  paper_theme()+
  scale_x_discrete(breaks=c("1:00","4:00","7:00","10:00",
                            "13:00","16:00","19:00","22:00"))+
  expand_limits(y=0)+ #make sure you get the zero line
  #guides(color = guide_legend())+
  theme(#legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Pool Prices ($/MWh)",x="Hour ending",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
hourly_graph 
ggsave(file=paste("images/hourly_prices.png",sep=""),width = 14,,height=7,dpi=300,bg="white")

hourly_graph<-  ggplot(hourly_patterns%>%filter(year>=2004)) +
  geom_line(aes(factor(hour),ail,group=factor(year),color=factor(year)),size=.85)+
  geom_line(data=hourly_patterns %>% filter(year==2023),aes(factor(hour),ail,group=factor(year),color=factor(year)),size=2.5)+
  scale_color_viridis("",option = "C",discrete = T,direction=-1)+
  scale_y_continuous(expand=c(0,0))+
  paper_theme()+
  scale_x_discrete(breaks=c("1:00","4:00","7:00","10:00",
                            "13:00","16:00","19:00","22:00"))+
  expand_limits(y=c(6000,11200))+ #make sure you get the zero line
  #guides(color = guide_legend())+
  theme(#legend.position="bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm")
    #legend.text = element_text(colour="black", size = 12, face = "bold")
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Internal Load (MW)",x="Hour ending",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
hourly_graph 
ggsave(file=paste("images/hourly_loads.png",sep=""),width = 14,height=7,dpi=300,bg="white")


  
#sample period prices and loads

peak_data<-forecast_data %>%filter(!is.na(actual_posted_pool_price),!is.na(actual_ail))%>% 
  assign_date_time_days()%>%
  filter(year>=2004)%>%
  assign_peaks()%>%
  left_join(renew_gen)%>%
  ungroup()%>%
  mutate(high_renew=(renew_gen>=1000),load_net_renew=actual_ail-renew_gen)%>%
  group_by(year,month) %>% 
  summarize(ail=mean(actual_ail,na.rm = T),peak_ail=max(actual_ail),trough_ail=min(actual_ail),
            q75_price=quantile(actual_posted_pool_price, probs=c(.95)),
            q25_price=quantile(actual_posted_pool_price, probs=c(.05)),
            q75_ail=quantile(actual_ail, probs=c(.95)),
            q25_ail=quantile(actual_ail, probs=c(.05)),
            mean_net_load=mean(load_net_renew,na.rm=T),
            q95_net=quantile(load_net_renew, probs=c(.95),na.rm = T),
            q05_net=quantile(load_net_renew, probs=c(.05),na.rm = T),
            renew_price=sum(actual_posted_pool_price*actual_ail*(high_renew==TRUE),na.rm = T)/sum(actual_ail*(high_renew==TRUE),na.rm = T),
            mean_peak_price=sum(actual_posted_pool_price*actual_ail*(on_peak==TRUE),na.rm = T)/sum(actual_ail*(on_peak==TRUE),na.rm = T),
            mean_off_peak_price=sum(actual_posted_pool_price*actual_ail*(on_peak==FALSE),na.rm = T)/sum(actual_ail*(on_peak==FALSE),na.rm = T),
            mean_peak_ail=sum(actual_ail*(on_peak==TRUE),na.rm = T)/sum((on_peak==TRUE),na.rm = T),
            mean_off_peak_ail=sum(actual_ail*(on_peak==FALSE),na.rm = T)/sum((on_peak==FALSE),na.rm = T),
            mean_price=sum(actual_posted_pool_price*actual_ail,na.rm = T)/sum(actual_ail,na.rm = T),peak_price=max(actual_posted_pool_price),trough_price=min(actual_posted_pool_price)
  )%>% 
  ungroup()%>%
  mutate(date=ymd(paste(year,month,1,sep="-"))) 
  

max_price=max(peak_data$peak_price)
# prices

top_panel<-ggplot(peak_data%>%filter(year>=2004)) +
  geom_line(aes(date,mean_price,linetype="A"),size=.85,color="black")+
  geom_line(aes(date,mean_off_peak_price,linetype="B"),size=.85,color="black")+
  geom_line(aes(date,renew_price,linetype="C"),size=.85,color="green")+
  geom_ribbon(aes(date,ymax=q75_price,ymin=q25_price,fill="Two-tailed 90th percentile range"),alpha=.5)+
  #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
  #scale_color_manual("",values = c("black"),labels=c("Average Monthly Price"))+
  scale_fill_manual("",values = c("grey50"))+
  scale_linetype_manual("",values = c("solid","11","22"),labels=c("Peak period average","Off-peak period average","High renewables average"))+
  scale_x_date(expand=c(0,0),breaks="2 year",labels = date_format("%b\n%Y",tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0))+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","black","green"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Pool Prices ($/MWh)",x="",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
  NULL)

top_panel


top_panel_short<-ggplot(peak_data%>%filter(year>=2015)) +
  geom_line(aes(date,mean_price,linetype="A"),size=.85,color="black")+
  geom_line(aes(date,mean_off_peak_price,linetype="B"),size=.85,color="black")+
  geom_line(aes(date,renew_price,linetype="C"),size=.85,color="green")+
  geom_ribbon(aes(date,ymax=q75_price,ymin=q25_price,fill="Two-tailed 90th percentile range"),alpha=.5)+
  #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
  #scale_color_manual("",values = c("black"),labels=c("Average Monthly Price"))+
  scale_fill_manual("",values = c("grey50"))+
  scale_linetype_manual("",values = c("solid","11","22"),labels=c("Peak periods","Off-peak periods","High renewables"))+
  scale_x_date(expand=c(0,0),breaks="2 year",labels = date_format("%b\n%Y",tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0))+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue","green"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Pool Prices ($/MWh)",x="",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)

top_panel_short

top_panel_short+
  geom_vline(aes(xintercept=ymd("2020-12-31")),linewidth=1.25,lty="11")+
  annotate("text",x=ymd("2020-12-05"),y=950,label="End of PPAs\nDec 31, 2020",hjust=1)
ggsave("images/ppa.png",height = 7,width=14,dpi=250,bg="white")



top_panel+
  geom_vline(aes(xintercept=ymd("2020-12-31")),linewidth=1.25,lty="11")+
  annotate("text",x=ymd("2020-12-05"),y=950,label="End of PPAs\nDec 31, 2020",hjust=1)
ggsave("images/ppa.png",height = 7,width=14,dpi=250,bg="white")



bottom_panel<-
  ggplot(peak_data%>%filter(year>=2004)) +
  #geom_line(aes(date,ail,colour="basic"),size=1.5)+
  geom_line(aes(date,mean_peak_ail,linetype="A"),size=.85,color="black")+
  geom_line(aes(date,mean_off_peak_ail,linetype="B"),size=.85,color="blue")+
  #geom_line(aes(date,load_net_renew,linetype="C"),size=.85,color="darkgreen")+
  #geom_ribbon(aes(date,ymax=peak_ail,ymin=trough_ail,fill="Range of Monthly Values"),alpha=.5)+
  geom_ribbon(aes(date,ymax=q75_ail,ymin=q25_ail,fill="Two-tailed 90th percentile range"),alpha=.5)+
  #scale_color_manual("",values = c("black"),labels=c("Average Monthly Price"))+
  scale_fill_manual("",values = c("grey50"))+
  scale_linetype_manual("",values = c("solid","11","3111"),labels=c("Peak hours average","Off-peak hours average","Internal Load Net of Renewables"))+
  
  scale_x_date(expand=c(0,0),breaks="2 year",labels = date_format("%b\n%Y",tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  expand_limits(y=12000)+
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  paper_theme()+
  
  theme(legend.position="bottom",
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.key.width = unit(1.3,"cm"),
        #legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="Internal Load (MW)",x="")
bottom_panel
#caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")

renew_panel<-
  ggplot(peak_data%>%filter(year>=2004)) +
  #geom_line(aes(date,ail,colour="basic"),size=1.5)+
  geom_line(aes(date,mean_net_load,linetype="A"),size=.85,color="darkgreen")+
  #geom_line(aes(date,mean_off_peak_ail,linetype="B"),size=.85,color="blue")+
  #geom_line(aes(date,load_net_renew,linetype="C"),size=.85,color="darkgreen")+
  #geom_ribbon(aes(date,ymax=peak_ail,ymin=trough_ail,fill="Range of Monthly Values"),alpha=.5)+
  geom_ribbon(aes(date,ymax=q95_net,ymin=q05_net,fill="Two-tailed 90th percentile range of hourly data"),alpha=.5)+
  #scale_color_manual("",values = c("black"),labels=c("Average Monthly Price"))+
  scale_fill_manual("",values = c("grey50"))+
  scale_linetype_manual("",values = c("solid","11","3111"),labels=c("Average internal load net of renewables"))+
  
  scale_x_date(expand=c(0,0),breaks="2 year",labels = date_format("%b\n%Y",tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  expand_limits(y=11200)+
  guides(linetype = guide_legend(override.aes = list(color = c("darkgreen")),order=1),color="none")+
  paper_theme()+
  
  theme(legend.position="bottom",
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.key.width = unit(1.3,"cm"),
        #legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="Internal Load net of Renewable Generation (MW)",x="")
renew_panel
#caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")



# sample period September 1, 2009 - December 31, 2019


top_panel+
  expand_limits(y=900)+
  #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=480,label = "Sample period")+
  coord_cartesian(clip = 'off')+    # This keeps the labels from disappearing
  theme(plot.margin = margin(t = 0.5, r = 1, b = 0, l = 1,unit= "cm"),
        axis.text.x = element_text(vjust=-0.5))
ggsave(file=paste("images/peak_prices.png",sep=""),width = 14,dpi=300,bg="white")

top_panel+
  expand_limits(y=500)+
  #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=480,label = "Sample period")+
  coord_cartesian(clip = 'off')+    # This keeps the labels from disappearing
  theme(plot.margin = margin(t = 0.5, r = 1, b = 0, l = 1,unit= "cm"),
        axis.text.x = element_text(vjust=-0.5))
ggsave(file=paste("images/peak_prices.png",sep=""),width = 14,height=7,dpi=300,bg="white")


bottom_panel+
  #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=11800,label = "Sample period")+
  theme(plot.margin = margin(t = 0.5, r = 1, b = 0, l = 1,unit= "cm"),
        axis.text.x = element_text(vjust=-0.5))
ggsave(file=paste("images/loads_clean.png",sep=""),width = 14,,height=7,dpi=300,bg="white")


bottom_panel+
  #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=11800,label = "Sample period")+
  theme(plot.margin = margin(t = 0.5, r = 1, b = 0, l = 1,unit= "cm"),
        axis.text.x = element_text(vjust=-0.5))
ggsave(file=paste("images/loads_clean.png",sep=""),width = 14,,height=7,dpi=300,bg="white")


renew_panel+
  #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=11800,label = "Sample period")+
  theme(plot.margin = margin(t = 0.5, r = 1, b = 0, l = 1,unit= "cm"),
        axis.text.x = element_text(vjust=-0.5))
ggsave(file=paste("images/renew_clean.png",sep=""),width = 14,,height=7,dpi=300,bg="white")


p_grid<-plot_grid(
  top_panel+
    expand_limits(y=1060)+
    annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
    annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=1030,label = "Sample period")+
    coord_cartesian(clip = 'off')+    # This keeps the labels from disappearing
    theme(
      plot.margin = margin(t = .25, r = 1, b = .05, l = 1,unit= "cm"),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      NULL
    )+
    NULL,
  bottom_panel+
    annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
    annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=11800,label = "Sample period")+
    theme(plot.margin = margin(t = -1.5, r = 1, b = .05, l = 1,unit= "cm"))+
    NULL,
  align = TRUE,axis="b", ncol = 1, rel_heights = c(1,.75)
)
p_grid
ggsave("images/prices_and_loads.png",width = 14,height=8,bg="white")


p_grid<-plot_grid(
  top_panel+
    expand_limits(y=500)+
    #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
    #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=1030,label = "Sample period")+
    coord_cartesian(clip = 'off')+    # This keeps the labels from disappearing
    theme(
      plot.margin = margin(t = .25, r = 1, b = .05, l = 1,unit= "cm"),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      NULL
    )+
    NULL,
  bottom_panel+
    #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
    #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=11800,label = "Sample period")+
    theme(plot.margin = margin(t = -1.5, r = 1, b = .05, l = 1,unit= "cm"))+
    NULL,
  align = TRUE,axis="b", ncol = 1, rel_heights = c(1,.75)
)
p_grid
ggsave("images/prices_and_loads_ep.png",width = 14,height=8,bg="white",dpi = 300)



#direct label graph with AESO data

#align variable names
df2 <- nrgstream_gen %>% filter(date<floor_date(max(date),"month"))%>% #trim to last full month of data
  select(time,vol=gen,id,aeso_name,plant_type,plant_fuel,capacity) %>%
  filter(plant_type %in% c("COAL","COGEN","NGCC","WIND","SCGT","NGCONV","TRADE","HYDRO","SOLAR","OTHER"))%>%
  #filter(year(time)>=2010) %>% 
  left_join(forecast_data) %>% filter(!is.na(date))%>%
  #strip the AB-WECC tie since it's duplicate of AB-MT and AB-BC
  filter(!id %in% c("AB_WECC","AB_WECC_Exp","AB_WECC_Imp"))%>%
  #mutate(plant_type=as.character(plant_type))%>%
  #mutate(Plant_Fuel=as.character(Plant_Fuel))%>%
  mutate(hour=hour(time))%>%
  select(-c("forecast_pool_price","day_ahead_forecasted_ail",        
                "forecasted_actual_ail_difference","start_date"))%>%
  #filter(date>Sys.Date()-years(10))%>%
  mutate(month=month(time),year=year(time))%>%   
  mutate(plant_type=as_factor(plant_type),
         # Relevel to the end
         plant_type=fct_other(plant_type,keep = c("COAL","COGEN","SCGT","NGCC","NGCONV","WIND","SOLAR","HYDRO","TRADE"),other_level = "OTHER"),
         plant_type=fct_relevel(plant_type, "OTHER", after = Inf),
         plant_type=fct_recode(plant_type, "NET IMPORTS"="TRADE"),
         plant_type=fct_relevel(plant_type, "NET IMPORTS", after = Inf),
         NULL
  )%>%
  #summarize by hour to get total gen and revenue by fuel, 
  group_by(year,month,date,hour,plant_type) %>% summarise(capacity=sum(capacity,na.rm = T),gen=sum(vol,na.rm = T),rev=sum(vol*actual_posted_pool_price),price=mean(actual_posted_pool_price)) %>% 
  #summarize by year and month to get mean gen and capture price by fuel, 
  group_by(year,month) %>% mutate(mkt_price=sum(price*gen)/sum(gen))%>%
  group_by(year,month,plant_type,mkt_price) %>% summarise(capacity=mean(capacity,na.rm = T),capture = sum(rev,na.rm=T)/sum(gen,na.rm = T),gen=mean(gen,na.rm = T))%>% 
  ungroup() %>%
  mutate(date=ymd(paste(year,month,15,sep="-")))
  
  
  AB_palette<- c("black","black","grey60","grey60","grey30","grey30","grey30","grey30")
  

  
  
    
  gen_fuel <- df2 %>% mutate(plant_type=factor(plant_type,levels=AB_plant_order))%>%
    filter(date>ymd("2005-01-01"))%>%
    mutate(plant_type=fct_collapse(plant_type,
     "OTHER"=c("OTHER","HYDRO"),
     "NATURAL GAS"=c("SCGT","NGCC","NGCONV","COGEN"),
     #"COGENERATION"=c("COGEN"),
     "NET IMPORTS"="TRADE"
     ),
     plant_type=fct_relevel(plant_type,"COGENERATION",after = 1))%>% 
    group_by(date,month,year,plant_type) %>% summarise(gen=sum(gen,na.rm = T),
                                                      )%>%
    ungroup() %>%
    group_by(plant_type) %>%
    mutate(gen12m=zoo::rollmean(gen,12,fill=NA))%>%
    #filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,gen, col = plant_type,lty=plant_type)) +
    geom_line(size=1.25)+
    #geom_line(aes(y=gen12m),size=1.25)+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=plant_type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_color_manual("",values= AB_palette)+
    #scale_fill_manual("",values= AB_palette)+
    scale_linetype_manual("",values= c("solid","21","solid","21","solid","21","solid"))+
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    paper_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "12 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2005-01-01", "2023-1-30")))+
    expand_limits(y =-500)+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks(6))+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(3.1,"line"))+
    labs(x="",y="Monthly Average Hourly Generation or Net Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  gen_fuel
  ggsave(file="images/gen_fuel.png", width = 14, height=8,dpi = 300,bg="white")
  
  
  
  rev_fuel <- 
    df2 %>% #mutate(plant_type=factor(plant_type,levels=AB_plant_order))%>%
    filter(date>=ymd("2015-01-01"))%>%
    filter(plant_type%in%c("WIND","SOLAR"))%>%
    ungroup()%>%
    rename(MARKET=mkt_price)%>%
    select(date,MARKET,plant_type,capture)%>%
    pivot_wider(names_from = plant_type,values_from = capture)%>%
    pivot_longer(-date,names_to = "plant_type",values_to = "capture")%>%
    group_by(plant_type)%>%
    mutate(rev12m=zoo::rollmean(capture,12,fill=NA))%>%
    
    ggplot() +
    geom_line(aes(date,capture, col = plant_type,lty=plant_type),size=1.25)+
    #geom_line(aes(date,rev12m, col = plant_type,lty=plant_type),size=1.25)+
    #geom_line(aes(date,mkt_price,col="MARKET",lty="MARKET"),size=1.25)+
    
    #geom_line(aes(y=gen12m),size=1.25)+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=plant_type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_color_manual("",values= AB_palette)+
    #scale_fill_manual("",values= AB_palette)+
    scale_linetype_manual("",values= c("solid","21","solid","21","solid","21","solid"))+
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    paper_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "12 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2015-01-01", "2023-1-30")))+
    #expand_limits(y =-500)+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks(6))+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(3.1,"line"))+
    labs(x="",y="Monthly Average Hourly Generation or Net Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  rev_fuel
  ggsave(file="images/rev_fuel.png", width = 14, height=8,dpi = 300,bg="white")
  
  
  
  
  color_palette<- c("black","grey30",colors_tableau10()[3],colors_tableau10()[2],"grey60","grey60")
  gen_fuel+
    scale_color_manual("",values= color_palette)
  ggsave(file="images/gen_fuel_color.png", width = 14, height=8,dpi = 300,bg="white")
  
  gen_trimmed <- df2 %>% mutate(plant_type=factor(plant_type,levels=AB_plant_order))%>%
    filter(date>ymd("2005-01-01"))%>%
    mutate(plant_type=fct_collapse(plant_type,
                                   "OTHER"=c("OTHER","HYDRO"),
                                   "NATURAL GAS"=c("SCGT","NGCC","NGCONV","COGEN"),
                                   #"COGENERATION"=c("COGEN"),
                                   "NET IMPORTS"="TRADE"
    ),
    plant_type=fct_relevel(plant_type,"COGENERATION",after = 1))%>% 
    group_by(date,month,year,plant_type) %>% summarise(gen=sum(gen,na.rm = T),
    )%>%
    ungroup() %>%
    filter(!plant_type %in% c("NET IMPORTS","OTHER"))%>%
    group_by(plant_type) %>%
    mutate(gen12m=zoo::rollmean(gen,12,fill=NA))%>%
    #filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,gen, col = plant_type,lty=plant_type)) +
    geom_line(size=1.25)+
    #geom_line(aes(y=gen12m),size=1.25)+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=plant_type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_color_manual("",values= AB_palette)+
    #scale_fill_manual("",values= AB_palette)+
    scale_linetype_manual("",values= c("solid","21","solid","21","solid","21","solid"))+
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    paper_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "12 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2005-01-01", "2023-1-30")))+
    expand_limits(y =-500)+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks(6))+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(3.1,"line"))+
    labs(x="",y="Monthly Average Hourly Generation or Net Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  color_palette<- c("black","grey50",colors_tableau10()[3],colors_tableau10()[2],"grey60","grey60")
    gen_trimmed+
      guides(color=guide_legend(nrow = 1))+
      labs(x="",y="Monthly Average Hourly Generation(MW)",
           #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
           #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
           #caption="Source: AESO data, authors' calculations."
           NULL)+
    scale_color_manual("",values= color_palette)
  ggsave(file="images/gen_trimmed_color.png", width = 14, height=8,dpi = 300,bg="white")
  
  
  area_fuel <- df2 %>% mutate(plant_type=factor(plant_type,levels=AB_plant_order))%>%
    filter(date>ymd("2005-01-01"))%>%
    mutate(plant_type=fct_collapse(plant_type,
                                   "RENEWABLES"=c("WIND","HYDRO","SOLAR","OTHER"),
                                   "NATURAL GAS"=c("SCGT","NGCC","NGCONV","COGEN"),
                                   #"COGENERATION"=c("COGEN"),
                                   "NET IMPORTS"="TRADE"
    ),
    plant_type=fct_relevel(plant_type,"COGENERATION",after = 1))%>% 
    group_by(date,month,year,plant_type) %>% summarise(gen=sum(gen,na.rm = T),
    )%>%
    ungroup() %>%
    #filter(!plant_type %in% c("NET IMPORTS","OTHER"))%>%
    group_by(plant_type) %>%
    mutate(gen12m=zoo::rollmean(gen,12,fill=NA))%>%
    #filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,gen,fill = plant_type)) +
    geom_area(size=.5,position="stack",color="black")+
    #geom_line(aes(y=gen12m),size=1.25)+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=plant_type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_fill_manual("",values=c("black","grey50",colors_tableau10()[3],colors_tableau10()[6],"grey60","grey60"))+
    #scale_fill_manual("",values= AB_palette)+
    scale_linetype_manual("",values= c("solid","21","solid","21","solid","21","solid"))+
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    paper_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "12 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2005-01-01", "2023-1-30")))+
    expand_limits(y =-500)+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks(6))+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(3.1,"line"))+
    labs(x="",y="Monthly Average Hourly Generation or Net Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  area_fuel
  area_fuel+
    guides(fill=guide_legend(nrow = 1))+
    theme(legend.key.width = unit(1.5,"line"))+
    labs(x="",y="Monthly Average Hourly Generation (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    scale_color_manual("",values= color_palette)
  ggsave(file="images/gen_trimmed_area.png", width = 14, height=8,dpi = 300,bg="white")
  
  
  
  share_fuel <- df2 %>% mutate(plant_type=factor(plant_type,levels=AB_plant_order))%>%
    filter(date>ymd("2005-01-01"))%>%
    filter(plant_type!="NET IMPORTS")%>%
    mutate(plant_type=fct_collapse(plant_type,
                                   "WWS"=c("WIND","HYDRO","SOLAR"),
                                   #"RENEWABLES"=c("WIND","HYDRO","SOLAR","OTHER"),
                                   "NATURAL GAS"=c("SCGT","NGCC","NGCONV","COGEN"),
                                   #"COGENERATION"=c("COGEN"),
                                   "NET IMPORTS"="TRADE"
    ),
    plant_type=fct_relevel(plant_type,"COGENERATION",after = 1))%>% 
    group_by(date,month,year,plant_type) %>% summarise(gen=sum(gen,na.rm = T),
    )%>%
    ungroup() %>%
    #filter(!plant_type %in% c("NET IMPORTS","OTHER"))%>%
    group_by(date)%>%
    mutate(total_gen=sum(gen),
           share_gen=gen/total_gen)%>%
    #filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,share_gen*100,fill = plant_type)) +
    geom_area(size=.5,position="stack",color="black")+
    #geom_line(aes(y=gen12m),size=1.25)+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=plant_type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_fill_manual("",values=c("black","grey50",colors_tableau10()[3],colors_tableau10()[6],"grey60","grey60"))+
    #scale_fill_manual("",values= AB_palette)+
    scale_linetype_manual("",values= c("solid","21","solid","21","solid","21","solid"))+
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    paper_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "12 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2005-01-01", "2023-1-30")))+
    #expand_limits(y =-500)+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks(6))+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(3.1,"line"))+
    labs(x="",y="Monthly Average Hourly Generation or Net Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  share_fuel
  share_fuel+
    guides(fill=guide_legend(nrow = 1))+
    theme(legend.key.width = unit(1.5,"line"))+
    labs(x="",y="Share of Total Generation (%)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    scale_color_manual("",values= color_palette)
  ggsave(file="images/share_area.png", width = 14, height=8,dpi = 300,bg="white")
  
  share_ws <- df2 %>% mutate(plant_type=factor(plant_type,levels=AB_plant_order))%>%
    filter(date>ymd("2005-01-01"))%>%
    filter(plant_type!="NET IMPORTS")%>%
    mutate(plant_type=fct_collapse(plant_type,
                                   #"WS"=c("WIND","SOLAR"),
                                   #"RENEWABLES"=c("WIND","HYDRO","SOLAR","OTHER"),
                                   "NATURAL GAS"=c("SCGT","NGCC","NGCONV","COGEN"),
                                   #"COGENERATION"=c("COGEN"),
                                   "NET IMPORTS"="TRADE"
    ),
    plant_type=fct_relevel(plant_type,"COGENERATION",after = 1))%>% 
    group_by(date,month,year,plant_type) %>% summarise(gen=sum(gen,na.rm = T),
    )%>%
    ungroup() %>%
    #filter(!plant_type %in% c("NET IMPORTS","OTHER"))%>%
    group_by(date)%>%
    mutate(total_gen=sum(gen),
           share_gen=gen/total_gen)%>%
    
    filter(plant_type%in% c("WIND","SOLAR"))%>%
    ggplot(aes(date,share_gen*100,fill = plant_type)) +
    geom_area(size=.5,position="stack",color="black")+
    #geom_line(aes(y=gen12m),size=1.25)+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=plant_type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_fill_manual("",values=c(colors_tableau10()[3],"yellow","grey60","grey60"))+
    #scale_fill_manual("",values= AB_palette)+
    scale_linetype_manual("",values= c("solid","21","solid","21","solid","21","solid"))+
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    paper_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "12 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2005-01-01", "2023-1-30")))+
    #expand_limits(y =-500)+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks(6))+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(3.1,"line"))+
    labs(x="",y="Monthly Average Hourly Generation or Net Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  
  share_ws
  share_ws+
    guides(fill=guide_legend(nrow = 1))+
    theme(legend.key.width = unit(1.5,"line"))+
    labs(x="",y="Share of Total Generation (%)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    scale_color_manual("",values= color_palette)
  ggsave(file="images/share_ws.png", width = 14, height=8,dpi = 300,bg="white")
  
  
  
  
  
    label_level<-5700
  test<-gen_fuel+
    annotate("text", x = ymd("2007-7-1"), y = label_level, label = "SGER introduced\n OBA=88%\np=$15/t",size=3.5,hjust = 0.5)+  
    annotate("segment",x=ymd("2007-7-1"),xend=ymd("2007-7-1"),y=-Inf,yend=label_level-350, colour="black",size=1,lty="11") +
    annotate("text", x = ymd("2016-1-1"), y = label_level, label = "SGER\nOBA=85%\np=$20/t",size=3.5,hjust = 0.5)+  
    annotate("segment",x=ymd("2016-1-1"),xend=ymd("2016-1-1"),y=-Inf,yend=label_level-350, colour="black",size=1,lty="11") +
    annotate("text", x = ymd("2017-1-1"), y = label_level, label = "SGER\nOBA=80%\np=$30/t",size=3.5,hjust = 0.5)+  
    annotate("segment",x=ymd("2017-1-1"),xend=ymd("2017-1-1"),y=-Inf,yend=label_level-350, colour="black",size=1,lty="11") +
    annotate("text", x = ymd("2018-1-1"), y = label_level, label = "CCIR\nOBA=0.37t\np=$30/t",size=3.5,hjust = 0.5)+  
    annotate("segment",x=ymd("2018-1-1"),xend=ymd("2018-1-1"),y=-Inf,yend=label_level-350, colour="black",size=1,lty="11") +
    #annotate("text", x = ymd("2019-1-1"), y = 6600, label = "CCIR\nOBA=0.37t/MWh\np=$30/t",size=3.8,hjust = 0.5)+  
    #annotate("segment",x=ymd("2019-1-1"),xend=ymd("2019-1-1"),y=-Inf,yend=6000, colour="black",size=1,lty="11") +
    #annotate("text", x = ymd("2020-1-1"), y = 6600, label = "CCIR\nOBA=0.37t/MWh\np=$30/t",size=3.8,hjust = 0.5)+  
    #annotate("segment",x=ymd("2020-1-1"),xend=ymd("2020-1-1"),y=-Inf,yend=6000, colour="black",size=1,lty="11") +
    annotate("text", x = ymd("2021-1-1"), y = label_level, label = "TIER\nOBA=0.37t\np=$40/t",size=3.5,hjust = 0.5)+  
    annotate("segment",x=ymd("2021-1-1"),xend=ymd("2021-1-1"),y=-Inf,yend=label_level-350, colour="black",size=1,lty="11") +
    annotate("text", x = ymd("2022-1-1"), y = label_level, label = "TIER\nOBA=0.37t\np=$50/t",size=3.5,hjust = 0.5)+  
    annotate("segment",x=ymd("2022-1-1"),xend=ymd("2022-1-1"),y=-Inf,yend=label_level-350, colour="black",size=1,lty="11") +
    #annotate("text", x = ymd("2010-1-1"), y = 6600, label = policy_exp("SGER",15,"88%"),size=3.5,hjust = 0.5)+  
    annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=label_level+380, alpha=0.1, fill="black")+
    annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=label_level+500,label = "Sample period")+
    expand_limits(y =label_level+300)+
    expand_limits(x =ymd("2022-09-01"))+
    coord_cartesian(clip = 'off')+    # This keeps the labels from disappearing
    NULL
  
    test
    ggsave(file="images/gen_fuel_policies.png", width = 16, height=8,dpi = 300)
  
  
  #carve out REP projects
  
  REP_projects<-c("RIV1","CRR2","WHT1","WRW1")
                             
  gen_rep <- nrgstream_gen %>% filter(date<floor_date(max(date),"month"))%>% #trim to last full month of data
    select(time=time,vol=gen,ID,AESO_Name,plant_type,Plant_Fuel,Capacity) %>%
    filter(plant_type %in% c("WIND"),!is.na(vol))%>%
    #filter(year(time)>=2010) %>% 
    left_join(forecast_data) %>% filter(!is.na(date))%>%
    mutate(hour=hour(time))%>%
    select(-c("forecast_pool_price","day_ahead_forecasted_ail",        
              "forecasted_actual_ail_difference","start_date"))%>%
    #filter(date>Sys.Date()-years(10))%>%
    mutate(month=month(time),year=year(time))%>%   
    mutate(plant_type=case_when(
      ID == "RIV1" ~ "REP_WIND",
      ID == "CRR2" ~ "REP_WIND",
      ID == "WHT1" ~ "REP_WIND",
      ID == "WRW1" ~ "REP_WIND",
      TRUE ~ plant_type
    ),
    plant_type=case_when(
      ID == "RTL1" ~ "PPA_WIND",
      ID == "WHT2" ~ "PPA_WIND",
      TRUE ~ plant_type
    ),           NULL  ) %>%
    #summarize by hour to get total gen and revenue by fuel, 
    group_by(year,month,date,hour,plant_type) %>% 
    summarise(capacity=sum(Capacity,na.rm = T),
              gen=sum(vol,na.rm = T),
              rev=sum(vol*actual_posted_pool_price)) %>%
    #summarize by year and month to get mean gen and capture price by fuel, 
    group_by(year,month,plant_type) %>% 
    summarise(Capacity=mean(capacity,na.rm = T),
              rev = sum(rev),
              avg_gen=mean(gen,na.rm = T),
              tot_gen=sum(gen))%>% 
    ungroup()%>%
    #create zeros where needed
    complete(year,month,plant_type)%>%
    mutate(Capacity=ifelse(is.na(Capacity),0,Capacity),
           tot_gen=ifelse(is.na(tot_gen),0,tot_gen),
           rev=ifelse(is.na(rev),0,rev),
           avg_gen=ifelse(is.na(avg_gen),0,avg_gen))%>%
    group_by(year,month,plant_type,Capacity) %>% 
    summarise(gen=avg_gen,capture=rev/tot_gen)%>%
    ungroup() %>%
    mutate(date=ymd(paste(year,month,15,sep="-")))%>%
    filter(date<=ymd("2022-07-31"))%>%
    mutate(
           plant_type=factor(plant_type),
           plant_type=fct_recode(plant_type,"Pre-REP Projects"= "WIND"),
           plant_type=fct_recode(plant_type,"REP Projects"= "REP_WIND"),
           plant_type=fct_recode(plant_type,"Post-REP Projects"= "PPA_WIND"),
           plant_type=fct_relevel(plant_type,"REP Projects",after =  Inf),
           plant_type=fct_relevel(plant_type,"Post-REP Projects",after =  Inf)
           )
  
  


rep_colors=c(colors_tableau10_light()[3],colors_tableau10()[8],colors_tableau10()[3])
rep_gen<-gen_rep %>% mutate(plant_type=fct_rev(plant_type))%>%
    ggplot()+
    geom_area(aes(date,gen,group=plant_type,fill=plant_type),position="stack",color="black",size=0.5)+
    scale_fill_manual("",values = rep_colors)+
    scale_x_date(expand=c(0,0),breaks="2 year",labels = date_format("%b\n%Y",tz="America/Denver"))+
    expand_limits(x=ymd("2004-01-01"))+
    scale_y_continuous(expand=c(0,0))+
    paper_theme()+
    expand_limits(y=c(0,1250))+ #make sure you get the zero line
    guides(fill = guide_legend(reverse=TRUE))+
    theme(legend.position="bottom",
          legend.margin=margin(c(0,0,0,0),unit="cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          #legend.text = element_text(colour="black", size = 12, face = "bold")
          #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
    )+
    labs(y="Monthly Average Hourly Generation (MW)",x="",
         #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
         #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
         NULL)
  rep_gen
  ggsave(file="images/gen_rep.png", width = 14,height=6,dpi = 600,bg="white")
  
  
  
    
rep_cap<-gen_rep %>% mutate(plant_type=fct_rev(plant_type))%>%
  ggplot()+
  geom_area(aes(date,Capacity,group=plant_type,fill=plant_type),position="stack",color="black",size=0.5)+
  scale_fill_manual("",values = rep_colors)+
  scale_x_date(expand=c(0,0),breaks="2 year",labels = date_format("%b\n%Y",tz="America/Denver"))+
  expand_limits(x=ymd("2004-01-01"))+
  scale_y_continuous(expand=c(0,0))+
  paper_theme()+
  expand_limits(y=c(0,2500))+ #make sure you get the zero line
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Installed Capacity (MW)",x="",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
rep_cap
  ggsave(file="images/capacity_rep.png", width = 14,height=4,dpi = 600,bg="white")


  rep_grid<-plot_grid(
    rep_gen+
      #expand_limits(y=500)+
      #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
      #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=1030,label = "Sample period")+
      coord_cartesian(clip = 'off')+    # This keeps the labels from disappearing
      theme(
        plot.margin = margin(t = .25, r = 1, b = .05, l = 1,unit= "cm"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        NULL
      )+
      NULL,
    rep_cap+
      #annotate("rect",xmin=ymd("2009-09-01"),xmax=ymd("2019-12-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
      #annotate("text",x=ymd("2009-09-01")+((ymd("2019-12-31")-ymd("2009-09-01"))/2),y=11800,label = "Sample period")+
      theme(plot.margin = margin(t = -1.5, r = 1, b = .05, l = 1,unit= "cm"))+
      NULL,
    align = TRUE,axis="b", ncol = 1, rel_heights = c(1,.5)
  )
  rep_grid
  ggsave("images/wind_cap_gen.png",width = 14,height=9,bg="white",dpi = 300)
  
  
  
#price_capture
    
    trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")
    
    ep_capture<- nrgstream_gen %>%
      select(time,ID,gen,plant_type)%>%
      
      
      filter(year(time) >= 2004,plant_type %in% c("WIND","SOLAR"))%>% 
      left_join(forecast_data%>%select(time,price=actual_posted_pool_price,ail=actual_ail))%>%
      group_by(plant_type,time,price,ail) %>% 
      summarise(total_gen=sum(gen,na.rm = T),total_rev=sum(gen*price,na.rm = T)) %>%
      ungroup()%>%
      mutate(year=as.factor(year(time)))%>%
      group_by(plant_type,year) %>% 
      summarize(avg_rev=sum(total_rev)/sum(total_gen,na.rm=TRUE))%>%
      ungroup()%>%
      complete(year,plant_type,fill = list(avg_rev = NA))%>%
      bind_rows(forecast_data%>%select(time,price=actual_posted_pool_price,ail=actual_ail)%>%
                  filter(year(time)>=2004)%>%
                  mutate(year=as.factor(year(time)))%>%
                  group_by(year)%>%
                  summarize(plant_type="Market average",avg_rev=sum(ail*price,na.rm = T)/sum(ail,na.rm=T)))
      #mutate(premium=ifelse(avg_rev!=0,avg_rev-mkt_rev,0))
      
    
    
    my_palette<-c("black",colors_tableau10()[3],"yellow")

    ep_capture_data <- ep_capture %>% 
      mutate(year=fct_recode(year,"2023\n(YTD)"="2023"),
             plant_type=fct_recode(plant_type,"Solar"="SOLAR"),
             plant_type=fct_recode(plant_type,"Wind"="WIND"),
             plant_type=fct_relevel(plant_type,"Solar",after=Inf),
             )
      ep_capture_plot<-ggplot(ep_capture_data)+
      geom_col(aes(year,avg_rev,fill=plant_type),position = position_dodge(width = .9),width = .6,color="black",size=.5,alpha=0.5)+
      #geom_col(aes(Year,p_mean),position = "identity",fill=NA,color="black")+
      #geom_line(dataaes(Year,capture,fill=plant_type),position = position_dodge(width = .9),width = .6,color="black",size=.5)+
      #geom_text(aes(y=-10,label=plant_type),angle=90,size=2)+
      #  scale_color_viridis("Plant Type",discrete=TRUE)+
      #  scale_fill_viridis("Plant Type",discrete=TRUE)+
      #scale_color_manual("",values=colors_tableau10())+
      #scale_fill_manual("",values=colors_tableau10())+
      scale_color_manual("",values=my_palette)+
      scale_fill_manual("",values=my_palette)+
      scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
      expand_limits(y=200)+
      paper_theme()+theme(legend.position = "bottom",
                          legend.margin=margin(c(-1.2,0,0,0),unit="cm"))+
      guides(fill=guide_legend(nrow = 1,keywidth = 1.25,keyheight = 1.25))+
      labs(x="",y="Generation-Weighted Average Revenue ($/MWh)",
           #title="Energy Price Capture ($/MWh, 2010-2021)",
           #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
      )
      ep_capture_plot+
      geom_col(data=ep_capture_data %>% filter (year!="2023\n(YTD)"),
               aes(year,avg_rev,fill=plant_type),position = position_dodge(width = .9),width = .6,color="black",size=.5,alpha=1)
      
    ggsave("images/price_capture_renew.png",width=14,height=7,dpi=300,bg="white")
# 0.97801* 1.00000 * 1.00733 *  1.05572 
    
    cpi<-function(year,REP="REP1"){
      ret<-case_when(
        (as.character(year)==2019) ~ 0.97801,
        (as.character(year)==2020) ~ 1.00000, 
        (as.character(year)==2021) ~  1.00733,
        (as.character(year)==2022) &  (REP=="REP1") ~  1.05572,
        (as.character(year)=="2022 (YTD)") &  (REP=="REP1")~ 1.05572,
        TRUE ~ 1)
      ret
    }
    
    REP_projects<-c("RIV1","CRR2","WHT1","WRW1","CYP1")
    
    rep_cf<- nrgstream_gen %>%
      select(time=time,ID,gen,Capacity)%>%
      filter(year(time) >= 2019,ID %in% REP_projects)%>%
      filter(!is.na(gen))%>%
      group_by(ID)%>%
      summarize(cap_fac=sum(gen,na.rm = T)/sum(Capacity,na.rm = T))
    
    
    plant_capture<- nrgstream_gen %>%
      select(time=time,ID,gen)%>%
      filter(year(time) >= 2019,ID %in% REP_projects)%>% 
      #filter(time<=ymd("2022-04-30"))%>% #check for initial submission values
      #filter(time<=ymd("2022-03-31"))%>% #check vs annual report
      #filter(time<=ymd("2022-03-31"))%>% #check vs AESO data
      left_join(forecast_data%>%select(time,price=actual_posted_pool_price))%>%
      mutate(year=as.factor(year(time)))%>%
      group_by(ID,year) %>% 
      summarize(avg_rev=sum(gen*price,na.rm=TRUE)/sum(gen,na.rm=TRUE),gen=sum(gen,na.rm=TRUE))%>%
      ungroup()%>%
      complete(year,ID,fill = list(avg_rev = NA))%>%
      bind_rows(forecast_data%>%select(time,price=actual_posted_pool_price,ail=actual_ail)%>%
                  filter(year(time)>=2019)%>%
                  mutate(year=as.factor(year(time)))%>%
                  group_by(year)%>%
                  summarize(ID="Market average",avg_rev=sum(ail*price)/sum(ail),gen=sum(ail)))%>%
      mutate(env_attr=case_when(
        (ID %in% REP_projects)& year==2019 ~30*.37,
        (ID %in% REP_projects)& year==2020 ~30*.37,
        (ID %in% REP_projects)& year==2021 ~40*.37,
        (ID %in% REP_projects)& year==2022 ~50*.37,
        TRUE~0
      ))%>%
        mutate(rep=case_when(
                     (ID=="WHT1") ~ "REP1",
                     (ID=="WRW1") ~ "REP3",
                     (ID=="RIV1") ~ "REP1",
                     (ID=="CRR2") ~ "REP1",
                     (ID=="CYP1") ~ "REP2",
                     TRUE ~ "Not in REP"))%>%
      mutate(raw_strike=case_when(
        (rep=="REP1") ~ 41.36,
        (rep=="REP2") ~ 38.69,
        (rep=="REP3") ~ 40.14,
        TRUE ~ 0))%>%
    mutate(
        cpi_adj=cpi(year,rep),
        strike=raw_strike*(.8+.2*cpi(year,rep)),
        net_govt_power=(avg_rev-strike)*gen*(ID!="Market average"),
        env_ttl=env_attr*gen*(ID!="Market average"),
        net_govt_ttl=net_govt_power+env_ttl
        )%>%
      group_by(year)%>%
      mutate(rep_gen=gen*(ID!="Market average"),
             rep_gen_year=sum(rep_gen,na.rm=T),
             rep_gen_share=round(rep_gen/rep_gen_year*100,1),
             year_mod=fct_recode(year,"2022 (YTD)"="2022"),
             label=paste(as.character(year_mod),"\n","(",round(rep_gen_year/10^3,0)," GWh)",sep=""))%>%
      ungroup()%>%
      filter(gen>0)
    

    #use updated AESO MV data to calculate REP contract values
    
    aeso_file<-read.csv("https://www.aeso.ca/assets/Uploads/data-requests/Hourly_Metered_Volumes_and_Pool_Price_and_AIL.csv")         
    load("aeso_plants.RData")
    
    
        
    aeso_rep<-aeso_file %>% 
      select(-Date_Begin_GMT)%>%
      rename("Market"=ACTUAL_AIL)%>%
      pivot_longer(-c(Date_Begin_Local,"ACTUAL_POOL_PRICE","DAY_AHEAD_POOL_PRICE",
                   "EXPORT_BC","EXPORT_MT","EXPORT_SK","IMPORT_BC",
                   "IMPORT_MT","IMPORT_SK"),
                   names_to = "ID", values_to = "gen")%>%
      filter(ID %in% c(REP_projects,"Market"))%>%
      mutate(time=mdy_hm(Date_Begin_Local))%>%
      select(-Date_Begin_Local)%>%
      filter(!is.na(gen))%>%
      select(time,price=ACTUAL_POOL_PRICE,ID,gen)%>%
      mutate(year=year(time),
             power_value=gen*price)%>%
      filter(as.numeric(as.character(year))>=2019)%>%
      #value carbon credits
      mutate(env_attr=case_when(
        (ID %in% REP_projects)& year==2019 ~30*.37,
        (ID %in% REP_projects)& year==2020 ~30*.37,
        (ID %in% REP_projects)& year==2021 ~40*.37,
        (ID %in% REP_projects)& year==2022 ~50*.37,
        TRUE~0
      ))%>%
      #assign rep series
      mutate(rep=case_when(
        (ID=="WHT1") ~ "REP1",
        (ID=="WRW1") ~ "REP3",
        (ID=="RIV1") ~ "REP1",
        (ID=="CRR2") ~ "REP1",
        (ID=="CYP1") ~ "REP2",
        TRUE ~ "Not in REP"))%>%
      #assign REP strike prices
      mutate(raw_strike=case_when(
        (rep=="REP1") ~ 41.36,
        (rep=="REP2") ~ 38.69,
        (rep=="REP3") ~ 40.14,
        TRUE ~ 0))%>%
      #adjust strikes for inflation, then calculate net positions
      group_by(year)%>%
      mutate(
       rep_gen_year=sum(gen*(ID %in% REP_projects))
       )%>%
        ungroup()%>%
      mutate(
        strike=raw_strike*(.8+.2*cpi(year,rep)),
        cfd_val=strike*gen,
        net_govt_power=(price-strike)*gen,
        env_ttl=env_attr*gen,
        net_govt_ttl=net_govt_power+env_ttl,
        year=as.factor(year),
        year_mod=fct_recode(year,"2022 (YTD)"="2022"),
        label=paste(as.character(year_mod),"\n\n","(",round(rep_gen_year/10^3,0)," GWh of REP-\nsupported generation)",sep="")
     )%>%
      group_by(year,ID,label)%>%
      summarize(net_govt_power=sum(net_govt_power,na.rm = T)/sum(gen,na.rm = T),
                env_ttl=sum(env_ttl,na.rm = T)/sum(gen,na.rm = T),
                net_govt_ttl=net_govt_power+env_ttl,
                power_val=sum(price*gen)/sum(gen),
                total_val=power_val+env_ttl
                )%>%
      ungroup()%>%
      mutate(
        ID=fct_relevel(ID,"WHT1","CRR2","RIV1","WRW1"),
        ID=fct_relevel(ID,"Market"),
        ID=fct_recode(ID,"Market Generation-Weighted Average Price"="Market"),
        ID=fct_recode(ID,"Whitla 1 (REP1)"="WHT1","Castle Rock Ridge 2 (REP1)"="CRR2","Riverview Wind (REP1)"="RIV1","Windrise Wind (REP3)"="WRW1"),
        #plant_type=fct_reorder(plant_type,startup,min),
      )
      
    
    
    
    
    aeso_rep_plant_cpi_graph<-ggplot(aeso_rep)+
      geom_col(aes(ID,power_val,fill=ID),size=.25,position=position_dodge(width=0.9),color="black",alpha=0.5)+
      geom_col(aes(ID,y=total_val,color="Deemed value of foregone emissions credits (0.37t/MWh, valued at Alberta TIER compliance prices)"),fill=NA,alpha=0.5,size=.5,position=position_dodge(width=0.9))+
      facet_wrap(~label,nrow=1)+
      #scale_fill_grey("")+
      scale_fill_viridis("",option = "B",discrete = T,direction = 1)+
      scale_color_manual("",values=c("black"))+
      scale_linetype_manual("",values=c("solid","21","22"))+
      scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
      expand_limits(y=0)+
      paper_theme()+
      theme(legend.position = "bottom",legend.box="vertical", legend.margin=margin(),
            legend.text=element_text(size=rel(2)),
            axis.text.x = element_blank(),
            NULL)+
      expand_limits(y=120)+
      guides(#fill="none",
        fill=guide_legend(order = 1,nrow = 1,label.position = "right",keywidth = 1,keyheight = 1),
        linetype=guide_legend(order = 3,nrow = 1,keywidth = 3),
        color=guide_legend(order = 2,ncol = 1,label.position = "right",keywidth = 1,keyheight = 1))+
      labs(x="",y="Value of generated electricity and environmental attributes ($/MWh)",
           #title="Energy Price Capture ($/MWh, 2010-2021)",
           #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
      )
    
    aeso_rep_plant_cpi_graph+
      geom_col(data=aeso_rep%>% filter(year!="2022"),aes(ID,power_val,fill=ID),size=.25,position=position_dodge(width=0.9),
               color="black",alpha=.95)+
      geom_col(data=aeso_rep%>% filter(year!="2022"),aes(ID,total_val,
               color="Deemed value of foregone emissions credits (0.37t/MWh, valued at Alberta TIER compliance prices)"),
               fill=NA,alpha=0.25,size=.5,position=position_dodge(width=0.9))+
      geom_hline(aes(yintercept=37*(.8+.2*cpi(year)),lty="REP1 Average CfD Strike Price"),color="black",size=1.5)+
      #geom_hline(aes(yintercept=38.69*(.8+.2*cpi(year)),lty="REP2 Average CfD Strike Price"),color="black",size=1.5)+
      geom_hline(aes(yintercept=40.14*(.8+.2*cpi(year)),lty="REP3 Average CfD Strike Price"),color="black",size=1.5)+
      NULL
    ggsave("images/aeso_rep_price_capture_cpi.png",width=14,height=8.5,dpi=300,bg="white")
    
    
    
    
    
    
    
    aeso_rep %>% 
      #group_by(year)%>%
      summarize(gen=sum(gen,na.rm=T),
                net_govt_power=sum(net_govt_power,na.rm = T),
                env_ttl=sum(env_ttl,na.rm = T),
                net_govt_ttl=sum(net_govt_ttl,na.rm=T),
                net_govt_check=net_govt_power+env_ttl
                )
    
    
    
    
    plant_capture %>% 
      filter(ID %in% REP_projects)%>% 
      #group_by(year)%>%
      summarize(gen=sum(gen,na.rm=T),
                net_govt_power=sum(net_govt_power,na.rm = T),
                env_ttl=sum(env_ttl,na.rm = T),
                net_govt_ttl=sum(net_govt_ttl,na.rm=T),
                net_govt_check=net_govt_power+env_ttl
                )
    
    
    
    aeso_rep %>% group_by(ID,year)%>%
      summarize(gen=sum(gen,na.rm=T))
    
    
    
    
    plant_capture %>% group_by(ID)%>%
      summarize(wtd_price=sum(avg_rev*gen,na.rm = T)/sum(gen,na.rm=T),
                gen=sum(gen,na.rm=T))
    
    
    
           
    plant_capture %>% filter(ID!="Market average")%>%
        summarize(total_power=sum(net_govt_power,na.rm = TRUE)/10^6,
                total=sum(net_govt_ttl,na.rm = TRUE)/10^6,
                total_env=sum(env_ttl,na.rm = TRUE)/10^6)
                
      
               
    #mutate(premium=ifelse(avg_rev!=0,avg_rev-mkt_rev,0))
    
    
    

    rep_plant_cpi_graph_data<-plant_capture %>%
      filter(as.numeric(as.character(year))>=2019)%>%
      #group_by(plant_type)%>%
      #mutate(startup=as.numeric(as.character(Year[min(which(!is.na(capture)))]))
      #)      %>%
      ungroup()  %>%
      #reset factor levels
      mutate(year=fct_recode(year,"2022 (YTD)"="2022"),
             ID=fct_relevel(ID,"WHT1","CRR2","RIV1","WRW1"),
             ID=fct_relevel(ID,"Market average"),
             #ID=fct_recode(ID,"Market Generation-Weighted Average"="MARKET"),
             ID=fct_recode(ID,"Whitla 1 (REP1)"="WHT1","Castle Rock Ridge 2 (REP1)"="CRR2","Riverview Wind (REP1)"="RIV1","Windrise Wind (REP3)"="WRW1"),
             #plant_type=fct_reorder(plant_type,startup,min),
      )
    
    rep_plant_cpi_graph<-ggplot(rep_plant_cpi_graph_data)+
      geom_col(aes(ID,avg_rev,fill=ID),size=.25,position=position_dodge(width=0.9),color="black",alpha=0.5)+
      geom_col(aes(ID,y=avg_rev+env_attr,color="Deemed value of foregone emissions credits (0.37t/MWh, valued at Alberta TIER compliance prices)"),fill=NA,alpha=0.5,size=.5,position=position_dodge(width=0.9))+
      facet_wrap(~label,nrow=1)+
      #scale_fill_grey("")+
      scale_fill_viridis("",option = "B",discrete = T,direction = 1)+
      scale_color_manual("",values=c("black"))+
      scale_linetype_manual("",values=c("solid","21","22"))+
      scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
      expand_limits(y=0)+
      paper_theme()+
      theme(legend.position = "bottom",legend.box="vertical", legend.margin=margin(),
            legend.text=element_text(size=rel(2)),
            axis.text.x = element_blank(),
            NULL)+
      expand_limits(y=120)+
      guides(#fill="none",
        fill=guide_legend(order = 1,nrow = 1,label.position = "right",keywidth = 1,keyheight = 1),
        linetype=guide_legend(order = 3,nrow = 1,keywidth = 3),
        color=guide_legend(order = 2,ncol = 1,label.position = "right",keywidth = 1,keyheight = 1))+
      labs(x="",y="Value of generated electricity and environmental attributes ($/MWh)",
           #title="Energy Price Capture ($/MWh, 2010-2021)",
           #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
      )
    rep_plant_cpi_graph+
      geom_col(data=rep_plant_cpi_graph_data%>% filter(year!="2022 (YTD)"),aes(ID,avg_rev,fill=ID),size=.25,position=position_dodge(width=0.9),
               color="black",alpha=.95)+
      geom_col(data=rep_plant_cpi_graph_data%>% filter(year=="2022 (YTD)"),aes(ID,y=avg_rev+env_attr,
                                                          color="Deemed value of foregone emissions credits (0.37t/MWh, valued at Alberta TIER compliance prices)"),
                                                          fill=NA,alpha=0.25,size=.5,position=position_dodge(width=0.9))+
      geom_hline(aes(yintercept=37*(.8+.2*cpi(year)),lty="REP1 Average CfD Strike Price"),color="black",size=1.5)+
      #geom_hline(aes(yintercept=38.69*(.8+.2*cpi(year)),lty="REP2 Average CfD Strike Price"),color="black",size=1.5)+
      geom_hline(aes(yintercept=40.14*(.8+.2*cpi(year)),lty="REP3 Average CfD Strike Price"),color="black",size=1.5)+
      NULL
    ggsave("images/rep_price_capture_cpi.png",width=14,height=8.5,dpi=300,bg="white")
    
    
     
    
    

    
    
    
    
    
    
    
    
    
    
    
    

    my_palette<-c("black",grey.colors(2,start=0.4,end = .7))
    df2 %>% filter(plant_type %in% c("MARKET","WIND","SOLAR"))%>%
      mutate(Year=fct_recode(Year,"2022*"="2022"),
             plant_type=fct_recode(plant_type,"Market"="MARKET","Wind"="WIND","Solar"="SOLAR"
                                    ))%>%
        ggplot(aes(Year,capture,group=plant_type,color=plant_type))+
      geom_errorbar(aes(ymin = q05, ymax = q95), size=1.25,width = .5,position=position_dodge(width=0.7))+
      geom_point(aes(shape="Mean"),size=3.25,position=position_dodge(width=0.7))+
      #geom_errorbar(aes(ymin = q50, ymax = q50), width = .75,position=position_dodge(width=0.9),size=1)+
      geom_point(aes(Year,q50,shape="Median"),size=3.25,position=position_dodge(width=0.7))+
  
      scale_color_manual("",values=my_palette)+
      scale_shape_manual("",values=c(1,19))+
      scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
      expand_limits(y=600)+
      blake_theme()+theme(plot.margin =unit(c(1,1,1,1),"cm"),
                          legend.position = "bottom")+
      guides(color=guide_legend(nrow = 1,label.position = "bottom",keywidth = 5),
             shape=guide_legend(nrow = 1,label.position = "bottom",keywidth = 5))+
      labs(x="",y="Capture Price ($/MWh)",
           #title="Energy Price Capture ($/MWh, 2010-2021)",
           #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
      )
    ggsave("images/price_capture_error.png",width=14,height=7)
    

   #wind_plants
    df5 <- nrgstream_gen %>%rename(time=time)%>% 
      filter(year(time) >= 2010,plant_type=="WIND",year(time)<=2022)%>%
      filter(gen!=0)%>%
      group_by(ID,time) %>% 
      summarise(#in_service=min(time),
                total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),p_mean=mean(Price)) %>%
      ungroup()%>%
      mutate(Year = as.factor(year(time)))%>%
      rename(plant_type=ID)%>%
      group_by(plant_type,Year) %>%
      mutate(avg_rev=total_rev/total_gen)%>%
      summarise(gen=sum(total_gen),
                capture = sum(total_rev)/sum(total_gen),
                q50=quantile(avg_rev, probs=c(.5)),
                q75=quantile(avg_rev, probs=c(.75)),
                q25=quantile(avg_rev, probs=c(.25)),
                q05=quantile(avg_rev, probs=c(.05)),
                q95=quantile(avg_rev, probs=c(.95)),
      )
    df5<-df5 %>% bind_rows(df3) #add the market prices
    
    #create all combos
    combos<- df5 %>% ungroup()%>%expand(plant_type,Year)
    
    plant_capture<-combos %>% left_join(df5)
      #mutate(capture=replace_na(capture,0))

    #REP_projects<-c("RIV1","CRR2","WHT1","WRW1")
    
    
 #27 plants plus market
    
     plant_cap_graph<-plant_capture %>% 
       filter(plant_type!="TAY2")%>%
      group_by(plant_type)%>%
       mutate(startup=as.numeric(as.character(Year[min(which(!is.na(capture)))]))
       )%>%
       ungroup()  %>%
      mutate(Year=fct_recode(Year,"2022\n(YTD)"="2022"),
             plant_type=fct_recode(plant_type,"Market"="MARKET"),
             plant_type=fct_recode(plant_type,"RIV1*"="RIV1"),
             plant_type=fct_recode(plant_type,"WRW1*"="WRW1"),
             plant_type=fct_recode(plant_type,"WHT1*"="WHT1"),
             plant_type=fct_recode(plant_type,"CRR2*"="CRR2"),
             plant_type=fct_reorder(plant_type,startup,min),
             plant_type=fct_relevel(plant_type,"Market"))%>%
      ggplot(aes(Year,capture,group=plant_type,fill=plant_type))+
      #geom_errorbar(aes(ymin = q05, ymax = q95), size=.75,width = .25,position=position_dodge(width=0.8))+
      #geom_point(aes(shape="Mean"),size=2.25,position=position_dodge(width=0.9))+
      geom_col(aes(shape="Mean"),size=.25,position=position_dodge(width=0.9),color="black")+
      #geom_errorbar(aes(ymin = q50, ymax = q50), width = .75,position=position_dodge(width=0.9),size=1)+
      #geom_point(aes(Year,q50,shape="Median"),size=3.25,position=position_dodge(width=0.7))+
      #coord_flip()+
      scale_fill_manual("",values=c("black",grey.colors(27,start = 0.2, end = 0.8)))+
      #scale_color_grey()+
      #scale_fill_grey("")+
      scale_shape_manual("",values=c(19))+
      scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
      expand_limits(y=0)+
      paper_theme()+theme(legend.position = "right",
                          legend.key.height= unit(.6, 'cm'),
                          legend.key.width= unit(.6, 'cm'),
                          legend.text=element_text(size=10.5))+
      guides(fill=guide_legend(ncol = 1))+
             #shape=guide_legend(nrow = 1,label.position = "right",keywidth = 1))+
      labs(x="",y="Volume-Weighted Average Capture Price ($/MWh)",
           #title="Energy Price Capture ($/MWh, 2010-2021)",
           #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
      )
    plant_cap_graph
    ggsave("images/price_capture_plant.png",width=14,height=7,dpi=300)
    
    guide_fill <- get_legend(plant_cap_graph)
    
    
    plot_grid(plant_cap_graph+
      geom_hline(aes(yintercept=37,lty="REP1 RESA Strike Price"),color="black",size=1.5)+
      geom_hline(aes(yintercept=38.69,lty="REP2 RESA Strike Price"),color="black",size=1.5)+
      geom_hline(aes(yintercept=40.14,lty="REP3 RESA Strike Price"),color="black",size=1.5)+
      scale_linetype_manual("",values=c("solid","11","22"))+
      theme(legend.position = "bottom")+
      guides(fill="none",linetype=guide_legend(nrow = 1,keywidth = 3)),
      guide_fill,ncol=2,rel_widths = c(.85, .15))
    ggsave("images/price_capture_error_plant_rep.png",width=14,height=7)
    
     
  #just the REP plants
    rep_plant_cap_graph<-plant_capture %>% 
      filter(plant_type%in% c("MARKET",REP_projects),as.numeric(as.character(Year))>=2019)%>%
      group_by(plant_type)%>%
      mutate(startup=as.numeric(as.character(Year[min(which(!is.na(capture)))]))
      )%>%
      ungroup()  %>%
      #add environmental attributes
      mutate(env_attr=case_when(
        (plant_type %in% REP_projects)& Year==2019 ~30*.37,
        (plant_type %in% REP_projects)& Year==2020 ~30*.37,
        (plant_type %in% REP_projects)& Year==2021 ~40*.37,
        (plant_type %in% REP_projects)& Year==2022 ~50*.37,
        TRUE~0
      ))%>%
      #reset factor levels
      mutate(Year=fct_recode(Year,"2022 (YTD)"="2022"),
             plant_type=fct_relevel(plant_type,"WHT1","CRR2","RIV1","WRW1"),
             plant_type=fct_relevel(plant_type,"MARKET"),
             plant_type=fct_recode(plant_type,"Market Generation-Weighted Average"="MARKET"),
             plant_type=fct_recode(plant_type,"Whitla 1 (WHT1)"="WHT1","Castle Rock Ridge 2 (CRR2)"="CRR2","Riverview Wind (RIV1)"="RIV1","Windrise Wind (WRW1)"="WRW1"),
             #plant_type=fct_reorder(plant_type,startup,min),
             )%>%
      ggplot(aes(Year,capture,group=plant_type))+
      #geom_errorbar(aes(ymin = q05, ymax = q95), size=.75,width = .25,position=position_dodge(width=0.8))+
      #geom_point(aes(shape="Mean"),size=2.25,position=position_dodge(width=0.9))+
      geom_col(aes(fill=plant_type),size=.25,position=position_dodge(width=0.9),color="black")+
      geom_col(aes(y=capture+env_attr,color="Environmental Attribute Value (TIER output-based allocation of emissions credits)"),fill=NA,alpha=0.5,size=.5,position=position_dodge(width=0.9))+
      #geom_errorbar(aes(ymin = q50, ymax = q50), width = .75,position=position_dodge(width=0.9),size=1)+
      #geom_point(aes(Year,q50,shape="Median"),size=3.25,position=position_dodge(width=0.7))+
      #coord_flip()+
      scale_color_manual("",values=my_palette)+
      scale_fill_grey("")+
      #scale_fill_manual("")+
      scale_shape_manual("",values=c(19))+
      scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
      expand_limits(y=0)+
      blake_theme()+theme(plot.margin =unit(c(1,1,1,1),"cm"),
                          legend.position = "bottom")+
      guides(#color=guide_legend(ncol = 1,label.position = "right",keywidth = 1,keyheight = 1)
        color="none",fill=guide_legend(nrow = 1,label.position = "right",keywidth = 1))+
      #shape=guide_legend(nrow = 1,label.position = "right",keywidth = 1))+
      labs(x="",y="Value of Electricity and Environmental Attributes ($/MWh)",
           #title="Energy Price Capture ($/MWh, 2010-2021)",
           #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
      )
    rep_plant_cap_graph
    ggsave("images/price_capture_error_plant.png",width=14,height=7)
    
    guide_right <- get_legend(rep_plant_cap_graph)
    
    
    plot_grid(rep_plant_cap_graph+
                geom_hline(aes(yintercept=37,lty="REP1 RESA Strike Price"),color="black",size=1.5)+
                geom_hline(aes(yintercept=38.69,lty="REP2 RESA Strike Price"),color="black",size=1.5)+
                geom_hline(aes(yintercept=40.14,lty="REP3 RESA Strike Price"),color="black",size=1.5)+
                scale_linetype_manual("",values=c("solid","11","22"))+
                theme(legend.position = "bottom",legend.box="vertical", legend.margin=margin())+
                guides(#fill="none",
                       fill=guide_legend(nrow = 1,label.position = "right",keywidth = 1,keyheight = 1),
                       linetype=guide_legend(nrow = 1,keywidth = 3),
                       color=guide_legend(ncol = 1,label.position = "right",keywidth = 1,keyheight = 1)),
              guide_right,ncol=1,rel_heights =  c(.85, .15))
    
    rep_plant_cap_graph+
      geom_hline(aes(yintercept=37,lty="REP1 RESA Strike Price"),color="black",size=1.5)+
      geom_hline(aes(yintercept=38.69,lty="REP2 RESA Strike Price"),color="black",size=1.5)+
      geom_hline(aes(yintercept=40.14,lty="REP3 RESA Strike Price"),color="black",size=1.5)+
      scale_linetype_manual("",values=c("solid","11","22"))+
      theme(legend.position = "bottom",legend.box="vertical", legend.margin=margin())+
      expand_limits(y=120)+
      guides(#fill="none",
        fill=guide_legend(nrow = 1,label.position = "right",keywidth = 1,keyheight = 1),
        linetype=guide_legend(nrow = 1,keywidth = 3),
        color=guide_legend(ncol = 1,label.position = "right",keywidth = 1,keyheight = 1))
    
    ggsave("images/rep_price_capture_bar.png",width=14,height=8,dpi=300)
    
    
    #check wind correlation with tight hours
    # 19-20 1.1125%
    #20-21 3.1857%
    #21-22* 4.8459%

    
    rep<-function(id_sent){
      ret<-case_when(
        (id_sent=="WHT1") ~ "REP1",
        (id_sent=="WRW1") ~ "REP3",
        (id_sent=="RIV1") ~ "REP1",
        (id_sent=="CRR2") ~ "REP1",
        TRUE ~ "Not in REP")
      ret
    }
    
        
    cpi<-function(year){
      ret<-case_when(
        (as.character(year)==2019) ~ 1,
        (as.character(year)==2020) ~ 1.011125,
        (as.character(year)==2021) ~ 1.011125*1.031857,
        (as.character(year)==2022) ~ 1.011125*1.031857*1.048459,
        (as.character(year)=="2022 (YTD)") ~ 1.011125*1.031857*1.048459,
        TRUE ~ 1)
      ret
    }
    
    rep_plant_cpi_graph<-plant_capture %>%
      filter(plant_type%in% c("MARKET",REP_projects),as.numeric(as.character(Year))>=2019)%>%
      group_by(plant_type)%>%
      mutate(startup=as.numeric(as.character(Year[min(which(!is.na(capture)))]))
      )%>%
      ungroup()  %>%
      #add environmental attributes
      mutate(env_attr=case_when(
        (plant_type %in% REP_projects)& Year==2019 ~30*.37,
        (plant_type %in% REP_projects)& Year==2020 ~30*.37,
        (plant_type %in% REP_projects)& Year==2021 ~40*.37,
        (plant_type %in% REP_projects)& Year==2022 ~50*.37,
        TRUE~0
      ))%>%
      #reset factor levels
      mutate(Year=fct_recode(Year,"2022 (YTD)"="2022"),
             plant_type=fct_relevel(plant_type,"WHT1","CRR2","RIV1","WRW1"),
             plant_type=fct_relevel(plant_type,"MARKET"),
             plant_type=fct_recode(plant_type,"Market Generation-Weighted Average"="MARKET"),
             plant_type=fct_recode(plant_type,"Whitla 1 (WHT1)"="WHT1","Castle Rock Ridge 2 (CRR2)"="CRR2","Riverview Wind (RIV1)"="RIV1","Windrise Wind (WRW1)"="WRW1"),
             #plant_type=fct_reorder(plant_type,startup,min),
      )%>%
      ggplot()+
      geom_col(aes(plant_type,capture,fill=plant_type),size=.25,position=position_dodge(width=0.9),color="black")+
      geom_col(aes(plant_type,y=capture+env_attr,color="Deemed environmental attribute value (TIER output-based allocation of emissions credits (0.37t/MWh) at annual carbon prices)"),fill=NA,alpha=0.5,size=.5,position=position_dodge(width=0.9))+
      geom_hline(aes(yintercept=37*(.8+.2*cpi(Year)),lty="REP1 RESA Strike Price"),color="black",size=1.5)+
      geom_hline(aes(yintercept=38.69*(.8+.2*cpi(Year)),lty="REP2 RESA Strike Price"),color="black",size=1.5)+
      geom_hline(aes(yintercept=40.14*(.8+.2*cpi(Year)),lty="REP3 RESA Strike Price"),color="black",size=1.5)+
      facet_wrap(~Year,nrow=1)+
      scale_fill_grey("")+
      scale_color_manual("",values=c("black"))+
      scale_linetype_manual("",values=c("solid","11","22"))+
      scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
      expand_limits(y=0)+
      paper_theme()+
      theme(legend.position = "bottom",legend.box="vertical", legend.margin=margin(),
            legend.text=element_text(size=rel(2)),
            axis.text.x = element_blank(),
            NULL)+
      expand_limits(y=120)+
      guides(#fill="none",
        fill=guide_legend(order = 1,nrow = 1,label.position = "right",keywidth = 1,keyheight = 1),
        linetype=guide_legend(order = 3,nrow = 1,keywidth = 3),
        color=guide_legend(order = 2,ncol = 1,label.position = "right",keywidth = 1,keyheight = 1))+
      labs(x="",y="Value of Electricity and Environmental Attributes ($/MWh)",
           #title="Energy Price Capture ($/MWh, 2010-2021)",
           #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
      )
    rep_plant_cpi_graph
    ggsave("images/rep_price_capture_cpi.png",width=14,height=8,dpi=300)
    
    
    
    rep_plant_cpi_totals<-plant_capture %>%
      filter(plant_type%in% c("MARKET",REP_projects),as.numeric(as.character(Year))>=2019)%>%
      group_by(plant_type)%>%
      mutate(startup=as.numeric(as.character(Year[min(which(!is.na(capture)))]))
      )%>%
      ungroup()  %>%
      #add environmental attributes
      mutate(env_attr=case_when(
        (plant_type %in% REP_projects)& Year==2019 ~30*.37,
        (plant_type %in% REP_projects)& Year==2020 ~30*.37,
        (plant_type %in% REP_projects)& Year==2021 ~40*.37,
        (plant_type %in% REP_projects)& Year==2022 ~50*.37,
        TRUE~0
      ))%>%
      mutate(REP=rep(plant_type))%>%
      mutate(strike=case_when(
        (REP == "REP1") ~41.36*cpi(as.character(Year)),
        (REP == "REP2") ~38.69*cpi(as.character(Year)),
        (REP == "REP3") ~40.14*cpi(as.character(Year)),
        TRUE~0
      ))%>%
      #reset factor levels
      mutate(Year=fct_recode(Year,"2022 (YTD)"="2022"),
             plant_type=fct_relevel(plant_type,"WHT1","CRR2","RIV1","WRW1"),
             plant_type=fct_relevel(plant_type,"MARKET"),
             plant_type=fct_recode(plant_type,"Market Generation-Weighted Average"="MARKET"),
             plant_type=fct_recode(plant_type,"Whitla 1 (WHT1)"="WHT1","Castle Rock Ridge 2 (CRR2)"="CRR2","Riverview Wind (RIV1)"="RIV1","Windrise Wind (WRW1)"="WRW1"),
             #plant_type=fct_reorder(plant_type,startup,min),
      )%>%
      filter(REP != "Not in REP",!is.na(gen))%>%
      select(plant_type,Year,gen,capture,startup,env_attr,strike)%>%
      mutate(AESO_net=(capture-strike)*gen/10^6,
             govt_net=env_attr*gen/10^6,
             total_net=AESO_net+govt_net)
   
    
    rep_plant_cpi_totals %>% ungroup()%>%
      summarize(AESO_total=sum(AESO_net),
                GOA_total=sum(govt_net),
                total=AESO_total+GOA_total)
    
    
    
    
     

    
  #price_capture plant level table
    
    #REP_projects<-c("RIV1","CRR2","WHT1","WRW1")
    
    
    #trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")
    rep_plant_capture <- nrgstream_gen %>%rename(time=time)%>% 
      filter(year(time) >= 2010,! NRG_Stream %in% trade_excl)%>% 
      filter(ID %in% REP_projects)%>%
      group_by(ID,time) %>% 
      summarise(total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),avg_rev=sum(total_rev/total_gen,na.rm = T),p_mean=mean(Price)) %>%
      ungroup()%>%
      filter(total_rev>0)
  
    
    market_capture=nrgstream_gen %>%rename(time=time)%>%
      filter(year(time) >= 2010,
            plant_type %in% c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","WIND")
      )%>% 
      select(time,Price,ID,gen)%>%
      mutate(ID="AESO",Revenue=Price*gen)%>%
      group_by(ID,time)%>%
      summarise(total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),avg_rev=sum(total_rev/total_gen,na.rm = T),p_mean=mean(Price)) 
  
    renew_capture=nrgstream_gen %>%rename(time=time)%>%
      filter(year(time) >= 2010,
             plant_type %in% c("SOLAR","WIND")
      )%>% 
      select(time,Price,ID,gen,plant_type)%>%
      mutate(ID=plant_type,Revenue=Price*gen)%>%
      mutate(Revenue=Price*gen)%>%
      group_by(ID,time)%>%
      summarise(total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),avg_rev=sum(total_rev/total_gen,na.rm = T),p_mean=mean(Price)) %>%
      filter(total_rev>0)
    
  
   paper_capture=bind_rows(market_capture,rep_plant_capture,renew_capture)%>%
     filter(time<ymd("2022-05-01"))%>%
     filter(time>ymd("2019-11-30"))%>%
     mutate(year=year(time))%>%
     group_by(year,ID)%>%
     summarize(total_gen=sum(total_gen),total_rev=sum(total_rev),avg_rev=total_rev/total_gen) %>%
     ungroup()
     #mutate(time=ymd(paste(year,month,1,sep="-")))%>%
     
   library(gt)
   library(gtExtras)
  
   
   order <- c("Market", 
              "Renewable facilities' volume-weighted average revenue ($/MWh)",
              "REP facilities' volume-weighted average revenue ($/MWh)")
   
gt1<-paper_capture%>% select(ID,year,avg_rev)%>%
  #mutate(ID=as_factor(ID))%>%
  #mutate(ID=fct_relevel(ID,"WIND",after=1),
  #       ID=fct_relevel(ID,"SOLAR",after=2))%>%
  mutate(
    group=case_when(
      ID == "AESO" ~ "Market",
      ID == "SOLAR" ~ "Renewable facilities' volume-weighted average revenue ($/MWh)",
      ID == "WIND" ~ "Renewable facilities' volume-weighted average revenue ($/MWh)",
      ID == "CRR2" ~ "REP facilities' volume-weighted average revenue ($/MWh)",
      ID == "WHT1" ~ "REP facilities' volume-weighted average revenue ($/MWh)",
      ID == "RIV1" ~ "REP facilities' volume-weighted average revenue ($/MWh)",
      ID == "WRW1" ~ "REP facilities' volume-weighted average revenue ($/MWh)",
    ),
    ID=case_when(
        ID == "AESO" ~ "Market volume-weighted market average price ($/MWh)",
        ID == "SOLAR" ~ "Solar",
        ID == "WIND" ~ "Wind",
        ID == "CRR2" ~ "Castle Rock Ridge Phase 2 (CRR2)",
        ID == "WHT1" ~ "Whitla Wind Phase 1 (WHT1)",
        ID == "RIV1" ~ "Riverview Wind Farm (RIV1)",
        ID == "WRW1" ~ "Windrise Wind (WRW1)",
      )
  )%>%
  pivot_wider(names_from = c(year),values_from = avg_rev)%>%
  arrange(
    factor(ID, levels = order)
  )%>%
  gt(    groupname_col = "group",
         rowname_col = "ID") %>%
  #tab_header(
  #  title = "AESO Market Price Capture"
  # ) %>%
  tab_spanner(
    label = "Year",
    columns = c("2019","2020","2021","2022")
  )%>%
  cols_label(
    ID = "",
  ) %>%
  row_group_order(groups = order)%>%
  tab_options(table.width = pct(100))%>%
  tab_source_note(source_note = "Data: AESO")%>%
  tab_style(
    style = cell_text(align = "left", indent = px(20)),
    locations = cells_stub()
  )%>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(1)),
      #Make text bold
      cell_text(weight = "bold")
    )
  )%>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold")
    )
  )%>%
  tab_style(
    locations = cells_column_spanners(spanners = "Year"),
    style = 
      cell_text(weight = "bold")
    )%>%
  tab_style(
    locations = cells_row_groups(),
    style = 
      cell_text(weight = "bold")
  )


  gt1%>% gtsave(
    "images/tab_1.png"
  )  

  
  
  #wind and temps
  
  load(file="data/market_data.RData")
  
  load("data/renew_vols.RData")
  
  renew_gen<-renew_vols%>% group_by(date,he,plant_type)%>%
    summarize(gen=sum(dispatched_mw))%>%pivot_wider(names_from = plant_type, values_from = gen)%>%
    left_join(mkt_data %>% select(date,he,time))%>%
    select(time,WIND,SOLAR)
  
    
  
  
  temp_corr<-mkt_data %>% filter(year==2021)%>%
    filter(!is.na(hourly_avail))%>%
    group_by(date,time,he)%>%mutate(hdd=mean(hdd_YEG,hdd_YMM,hdd_YYC,na.rm=TRUE),
                                    cdd=mean(cdd_YEG,cdd_YMM,cdd_YYC,na.rm=TRUE),
                                    temp=mean(temp_YEG,temp_YMM,temp_YYC,na.rm=TRUE))%>%
    ungroup()%>%
    select(time,hourly_renewables,temp,hdd,cdd,actual_ail,actual_posted_pool_price)%>%
    left_join(renew_gen)
    
    temp_quants<-temp_corr %>% filter(!is.na(temp))%>%
    mutate(quantile10 = cut(temp, quantile(temp, seq(0, 1, 0.1),na.rm=T), include.lowest=TRUE,labels = FALSE))
  
  quant_levels=temp_quants %>% summarize(quantiles=round(quantile(temp, seq(0, 1, 0.1),na.rm=T),1))  
  
  
  test<-temp_quants %>% mutate(low_wind=(WIND<500)*1)%>%
    #group_by(quantile10)%>% summarize(wind=mean(WIND))
  I()
  table(test$low_wind,test$quantile10)  
  table(test$low_wind)  
  
  
  
  temp_quants %>% mutate(
    quantile10=factor(quantile10,
                      labels=paste("(",quant_levels$quantiles[-11],"\u00B0C,\n",quant_levels$quantiles[-1],"\u00B0C)",sep=""))
  )%>%
  
    I()%>%ggplot(aes(group=quantile10))+
    geom_boxplot(aes(quantile10,WIND),outlier.shape = NA)+
    scale_x_discrete(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
    expand_limits(y=2000)+
    blake_theme()+theme(plot.margin =unit(c(1,1,1,1),"cm"),
                        legend.position = "bottom")+
    guides(#color=guide_legend(ncol = 1,label.position = "right",keywidth = 1,keyheight = 1)
      color="none",fill=guide_legend(nrow = 1,label.position = "right",keywidth = 1))+
    #shape=guide_legend(nrow = 1,label.position = "right",keywidth = 1))+
    labs(x="",y="Hourly Wind Generation (MW)",
         #title="Energy Price Capture ($/MWh, 2010-2021)",
         #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
    )
  
  ggsave("images/wind_temps.png",width=14,height=7)
  
  temp_quants %>% mutate(
    quantile10=factor(quantile10,
                      labels=paste("(",quant_levels$quantiles[-11],"\u00B0C,\n",quant_levels$quantiles[-1],"\u00B0C)",sep=""))
  )%>%
    
    I()%>%ggplot(aes(group=quantile10))+
    geom_boxplot(aes(quantile10,SOLAR),outlier.shape = NA)+
    scale_x_discrete(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
    expand_limits(y=300)+
    blake_theme()+theme(plot.margin =unit(c(1,1,1,1),"cm"),
                        legend.position = "bottom")+
    guides(#color=guide_legend(ncol = 1,label.position = "right",keywidth = 1,keyheight = 1)
      color="none",fill=guide_legend(nrow = 1,label.position = "right",keywidth = 1))+
    #shape=guide_legend(nrow = 1,label.position = "right",keywidth = 1))+
    labs(x="",y="Hourly Solar Generation (MW)",
         #title="Energy Price Capture ($/MWh, 2010-2021)",
         #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
    )
  
  ggsave("images/solar_temps.png",width=14,height=7)
  
  
  

    top_panel<-temp_corr %>% filter(cdd>0)%>%
    mutate(quantile10 = cut(cdd, quantile(cdd, seq(0, 1, 0.1)), include.lowest=TRUE,labels = FALSE))%>%
    mutate(quantile10=factor(quantile10))%>%
    group_by(quantile10)%>%
    #summarize(mean=mean(hourly_renewables,na.rm=T),
    #                                 min=min(hourly_renewables,na.rm=T),
    #                                 med=median(hourly_renewables,na.rm=T),
    #                                 max=max(hourly_renewables,na.rm=T)
    #                                 )%>%
    I()%>%ggplot(aes(group=quantile10))+
     geom_boxplot(aes(quantile10,hourly_renewables),outlier.shape = NA,notchwidth = 0.25)+
    scale_x_discrete(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())
    
    bottom_panel<-temp_corr %>% filter(hdd>0)%>%
      mutate(quantile10 = cut(hdd, quantile(hdd, seq(0, 1, 0.1)), include.lowest=TRUE,labels = FALSE))%>%
      mutate(quantile10=factor(quantile10))%>%
      group_by(quantile10)%>%
      #summarize(mean=mean(hourly_renewables,na.rm=T),
      #                                 min=min(hourly_renewables,na.rm=T),
      #                                 med=median(hourly_renewables,na.rm=T),
      #                                 max=max(hourly_renewables,na.rm=T)
      #                                 )%>%
      I()%>%ggplot(aes(group=quantile10))+
      geom_boxplot(aes(quantile10,hourly_renewables),outlier.shape = NA)+
      scale_x_discrete(expand=c(0,0))+
      scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())
    
    
    p_grid<-plot_grid(
      top_panel+
        expand_limits(y=1200)+
        theme(
          #legend.position = "none",
          #axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          NULL
        )+
        NULL,
      bottom_panel+ 
        NULL,
      align = TRUE,axis="b", ncol = 1, rel_heights = c(1,1)
    )
    p_grid
    ggsave("images/temps_and_wind.png",width = 15,height=12)
    
    
    
day<-floor_date(ymd("2020-01-01"),"month")
#get last month of data

whitla_gen<-nrgstream_gen %>% filter(ID=="WHT1")%>%
  filter(date>=day,date<=day+months(1))

mkt_snapshot<-get_forecast_report(day,day+months(1))%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  mutate(strike=(actual_posted_pool_price>=40)*1,
         pay_to_aeso=strike*actual_posted_pool_price,
         pay_to_gen=(1-strike)*actual_posted_pool_price,
         )%>%
  left_join(whitla_gen %>% select(time=time,whitla_gen=gen))%>%
  mutate(power_value=whitla_gen*actual_posted_pool_price,
         whitla_flow=(pay_to_gen-pay_to_aeso)*whitla_gen,
         whitla_rev=40*whitla_gen,
         above_strike=ifelse((actual_posted_pool_price>=40),actual_posted_pool_price,NA)
         )  


mkt_snapshot %>% summarize(gen=sum(whitla_gen),value=sum(power_value)/10^6,
                           rev=sum(whitla_rev)/10^6,aeso_net=value-rev,
                           net_ea=aeso_net+30*0.37*gen/10^6,
                           pwr_ea=value+30*0.37*gen/10^6)


bottom_panel<-ggplot(mkt_snapshot) +
  geom_line(aes(time,power_value/1000-whitla_rev/1000,colour="Net Payments to/from AESO",lty="Net Payments to/from AESO"),size=.75)+
  geom_line(aes(time,whitla_rev/1000,colour="Cash flow to Generator",lty="Cash flow to Generator"),size=.75)+
  #geom_line(aes(time,power_value/1000,colour="Value of Generated Electricity",lty="Value of Generated Electricity"),size=.75)+
  #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
  scale_color_manual("",values = c("black","grey40","grey70"))+
  scale_linetype_manual("",values = c("solid","31","11"))+
  scale_x_datetime(expand=c(0,0),breaks="7 days",labels = date_format("%b %d",tz="America/Denver"))+
  expand_limits(x=min(mkt_snapshot$time)-days(1))+
  expand_limits(y=c(-20,60))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  paper_theme()+
  labs(y="Cash flows ($ Thousands)",x="")





top_panel<-ggplot(mkt_snapshot) +
  geom_line(aes(time,actual_posted_pool_price,colour="A"),size=.75)+
  geom_line(aes(time,above_strike,colour="B"),size=.75)+
  #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
  scale_color_manual("",values = c("black","grey60"),labels=c("Price below $40/MWh strike","Price above $40/MWh strike"))+
  scale_x_datetime(expand=c(0,0),breaks="7 days",labels = date_format("%b %d",tz="America/Denver"))+
  expand_limits(x=min(mkt_snapshot$time)-days(1))+
  expand_limits(y=1001)+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  paper_theme()+
  labs(y="Pool Price ($/MWh)",x="")


mid_panel<-ggplot(mkt_snapshot) +
  geom_line(aes(time,whitla_gen,colour="A"),size=.75)+
  #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
  scale_color_manual("",values = c("black","grey60"),labels=c("Whitla Hourly Generation"))+
  scale_x_datetime(expand=c(0,0),breaks="7 days",labels = date_format("%b %d",tz="America/Denver"))+
  expand_limits(x=min(mkt_snapshot$time)-days(1))+
  expand_limits(y=200)+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  paper_theme()+
  labs(y="Generation (MW)",x="")


p_grid<-plot_grid(
  mid_panel+ 
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      plot.margin = margin(t = .5, r = 1, b = .05, l = 1,unit= "cm"))+
    NULL,
  top_panel+
    expand_limits(y=c(0,1001))+
    theme(
      legend.position = c(0.85, 1.04),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      plot.margin = margin(t = .05, r = 1, b = .05, l = 1,unit= "cm"),
      NULL)+
    NULL,
  bottom_panel+
    theme(
      legend.position = c(0.85, 1.02),
      plot.margin = margin(t = .05, r = 1, b = .05, l = 1,unit= "cm"),
      #axis.title.x = element_blank(),
      #axis.text.x = element_blank(),
      NULL
    )+
    NULL,
  align = TRUE,axis="b", ncol = 1, rel_heights = c(1.3,1,1)
)
p_grid
ggsave("images/cfd.png",width = 14,height=10,dpi=300)

lto_cap<-read_excel("data/2021LTO.xlsx",sheet = "Generation capacity by region")

lto_gen<-read_excel("data/2021LTO.xlsx",sheet = "Generation data by fuel type")%>%
  clean_names() %>%select(-x6)%>%
  mutate(category=gsub("Output \\(GWh\\)","output",category))%>%
  mutate(category=gsub("Capacity End Year \\(MW\\)","capacity",category))%>%
  filter(category%in% c("output","capacity"))%>%
  pivot_wider(names_from = category,values_from=value)%>%
  group_by(calendar_year,scenario)%>%mutate(total_cap=sum(capacity,na.rm=T),
                                            total_output=sum(output,na.rm=T),
                                            cap_share=capacity/total_cap,
                                            output_share=output/total_output)
renew<-c("Hydro","Other","Solar","Solar/Storage","Wind")

lto_renew<-lto_gen %>% filter(calendar_year==2023,fuel_type %in% renew) %>%
  group_by(scenario)%>%
  mutate(renew_cap=sum(capacity),
         renew_gen=sum(output))%>%
  group_by(scenario)%>%
  mutate(renew_cap_share=sum(cap_share),
            renew_gen_share=sum(output_share))


#             
#   
# 
# 
# gen_rep_graph<-gen_rep%>% filter(year>=2010)%>%
#   filter(plant_type%in% c("WIND","REP_WIND","PPA_WIND"))%>%
#   mutate(plant_type=fct_recode(plant_type,"Wind excl. REP Projects"= "WIND"))%>%
#   mutate(plant_type=fct_recode(plant_type,"Total wind incl. REP Projects"= "REP_WIND"))%>%
#   mutate(plant_type=fct_relevel(plant_type,"Wind excl. REP Projects",after =  Inf))%>%
#   group_by(plant_type) %>%
#   mutate(gen6m=zoo::rollmean(gen,6,fill=NA))%>%
#   ggplot(aes(date,gen, col = plant_type,lty=plant_type,fill=plant_type,group=plant_type)) +
#   #geom_line(size=1.25,position = "stack")+
#   #geom_line(size=1.25,position = "identity")+
#   geom_line(aes(y=gen6m),size=1.25,position = "stack")+
#   #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
#   #geom_dl(aes(label=plant_type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
#   #scale_color_manual("",values= AB_palette)+
#   
#   #scale_fill_manual("",values= AB_palette)+
#   scale_linetype_manual("",values= c("11","solid"),guide = guide_legend(reverse = TRUE))+
#   scale_color_manual("",values=grey.colors(9,start=0,end=.8),guide = guide_legend(reverse = TRUE))+
#   #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
#   #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
#   paper_theme()+
#   scale_x_date(date_labels = "%b\n%Y",date_breaks = "24 months",expand=c(0,0))+
#   expand_limits(x = as.Date(c("2010-01-01", "2022-1-30")))+
#   expand_limits(y =1000)+
#   scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
#   theme(legend.position = "bottom",
#         legend.key.width = unit(3.7,"line"),
#         axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
#   
#   labs(x="",y="6 Month Moving Average Generation (MW)",
#        #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
#        #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
#        #caption="Source: AESO data, authors' calculations."
#        NULL)+
#   
#   # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
#   #  annotate("rect", fill = "grey70", alpha = .3, 
#   #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
#   #           ymin = -Inf, ymax = Inf)+
#   # annotate("rect", fill = "grey70", alpha = .3, 
#   #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
#   #        ymin = -Inf, ymax = Inf)+
#   #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
#   NULL
# gen_rep_graph
# ggsave(file="images/gen_rep.png", width = 14,height=9,dpi = 600,bg="white")




ggplot(renew_gen%>%mutate(Year_ID=as_factor(year(time))),aes(renew_gen))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_x_continuous(limits=range(df1$total_gen),expand=c(0,0),breaks = pretty_breaks())+
  scale_y_continuous(expand=c(0,0),labels = scales::percent)+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Wind Generation (MW)",y="% of hours generation < X MW",
       title="Cumulative Density Function, Wind Energy (2010-2017 Avg)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
ggsave(file="images/wind_cdf.png")





ggplot(
  renew_gen%>%mutate(Year_ID=as_factor(year(time)),month=as_factor(month.abb[month(time)]),h24mean=rollmean(renew_gen,24,na.pad = T))%>%
  filter(time>max(time)-years(5),!is.na(h24mean)),
  aes(h24mean))+
  facet_wrap(~month)+
  #geom_density(aes(fill="Wind Power Generation",colour=year(time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  #scale_x_continuous(limits=range(df1$total_gen),expand=c(0,0),breaks = pretty_breaks())+
  #scale_y_continuous(expand=c(0,0),labels = scales::percent)+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="24-hour trailing average wind and solar generation (MW)",y="% of hours where previous 24hr average generation < X MW",
       title="Cumulative Density Function, Wind and Solar Generation (2022-23)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
ggsave(file="images/wind_cdf.png")


ggplot(
  renew_gen%>%mutate(Year_ID=as_factor(year(time)),month=as_factor(month.abb[month(time)]),h24mean=roll::roll_min(renew_gen,24))%>%
    filter(time>max(time)-years(5),!is.na(h24mean)),
  aes(h24mean))+
  facet_wrap(~month)+
  #geom_density(aes(fill="Wind Power Generation",colour=year(time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  #scale_x_continuous(limits=range(df1$total_gen),expand=c(0,0),breaks = pretty_breaks())+
  #scale_y_continuous(expand=c(0,0),labels = scales::percent)+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="24-hour trailing average wind and solar generation (MW)",y="% of hours where previous 24hr average generation < X MW",
       title="Cumulative Density Function, Wind and Solar Generation (2022-23)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")


