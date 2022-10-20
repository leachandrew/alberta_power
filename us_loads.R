source("power_paper_base.R")

library(directlabels)
library(cowplot)


download.file("https://www.eia.gov/electricity/gridmonitor/sixMonthFiles/EIA930_SUBREGION_2022_Jul_Dec.csv","eia_demand.csv")

eia_data<-read_csv("eia_demand.csv")

eia_data<-eia_data %>% clean_names()%>%mutate(
  balancing_authority=as.factor(balancing_authority))%>%
  #balancing_authority=fct_other(balancing_authority,drop=c("ISNE","CISO","PNM")
  rename(he_utc=utc_time_at_end_of_hour,
       he_local=local_time_at_end_of_hour)%>%
  mutate(he_utc=mdy_hms(he_utc),
         he_local=mdy_hms(he_local))%>%
  group_by(balancing_authority,he_local,he_utc)%>%
  summarize(demand_mw=sum(demand_mw,na.omit=T))%>%
  ungroup()%>%
  identity()
  
hourly_data<-eia_data%>%filter(he_utc>=ymd("2022-07-02"),he_utc<=ymd("2022-07-09"))

hourly_graph<-
  ggplot(hourly_data) +
  geom_line(aes(he_local-hours(1),demand_mw,group=balancing_authority,color=balancing_authority),size=.85)+
  scale_color_viridis("",option = "C",discrete = T,direction=-1)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_datetime(labels = date_format("%d %b\n%H:00"),breaks = "12 hours", expand=c(0,0))+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  #guides(color = guide_legend())+
  theme(#legend.position="bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm")
    #legend.text = element_text(colour="black", size = 12, face = "bold")
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Hourly System Load (MW)",x="Hour ending",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
hourly_graph 
ggsave("images/us_regions.png", width=14, height=7, dpi=300,bg="white")


hourly_data<-hourly_data%>% group_by(balancing_authority)%>%
  mutate(avg_load=mean(demand_mw))%>% ungroup()

hourly_graph<-
  ggplot(hourly_data) +
  geom_line(aes(he_local-hours(1),demand_mw/avg_load*100,group=balancing_authority,color=balancing_authority),size=.85)+
  scale_color_viridis("",option = "C",discrete = T,direction=-1)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_datetime(labels = date_format("%d %b\n%H:00"),breaks = "12 hours", expand=c(0,0))+
  paper_theme()+
  #expand_limits(y=0)+ #make sure you get the zero line
  #guides(color = guide_legend())+
  theme(#legend.position="bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm")
    #legend.text = element_text(colour="black", size = 12, face = "bold")
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Hourly System Load (MW)",x="Local Time",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
hourly_graph 
ggsave("images/us_regions_index.png", width=14, height=7, dpi=300,bg="white")


hourly_graph<-
  ggplot(hourly_data) +
  geom_line(aes(he_utc+hours(7),demand_mw/avg_load*100,group=balancing_authority,color=balancing_authority),size=.85)+
  scale_color_viridis("",option = "C",discrete = T,direction=-1)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_datetime(labels = date_format("%d %b\n%H:00"),breaks = "12 hours", expand=c(0,0))+
  paper_theme()+
  #expand_limits(y=0)+ #make sure you get the zero line
  #guides(color = guide_legend())+
  theme(#legend.position="bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm")
    #legend.text = element_text(colour="black", size = 12, face = "bold")
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Hourly System Load (MW)",x="Eastern Time",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
hourly_graph 

ggsave("images/us_regions_index_utc.png", width=14, height=7, dpi=300,bg="white")

hourly_graph<-
  ggplot(hourly_data) +
  geom_line(aes(he_utc+hours(7),demand_mw/avg_load*100,group=balancing_authority),colour=blakes_blue,alpha=.5, size=.5)+
  geom_line(
    data=hourly_data%>% group_by(he_utc)%>% summarise(balancing_authority="US L48",demand_mw=sum(demand_mw,na.rm=T))%>%
      ungroup()%>%mutate(avg_load=mean(demand_mw)),
    aes(he_utc+hours(7),demand_mw/avg_load*100,group=balancing_authority,color=balancing_authority),size=1.25)+
  scale_color_manual("",values=c(blakes_blue),labels="US Lower 48 Average" )+
  scale_y_continuous(expand=c(0,0))+
  scale_x_datetime(labels = date_format("%d %b\n%H:00"),breaks = "12 hours", expand=c(0,0))+
  paper_theme()+
  #expand_limits(y=0)+ #make sure you get the zero line
  #guides(color = guide_legend())+
  theme(#legend.position="bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm")
    #legend.text = element_text(colour="black", size = 12, face = "bold")
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Hourly System Load (MW)",x="Eastern Time",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
hourly_graph 

ggsave("images/us_regions_index_l48.png", width=14, height=7, dpi=300,bg="white")



hourly_graph<-
  ggplot(hourly_data) +
  geom_area(aes(he_utc+hours(7),demand_mw,group=balancing_authority,fill=balancing_authority),color="black", size=.5,position = "stack")+
  geom_line(
    data=hourly_data%>% group_by(he_utc)%>% summarise(balancing_authority="US L48",demand_mw=sum(demand_mw,na.rm=T))%>%
      ungroup()%>%mutate(avg_load=mean(demand_mw)),
    aes(he_utc+hours(7),demand_mw,group=balancing_authority,color=balancing_authority,lty=balancing_authority),size=1.25)+
  scale_color_manual("",values=c("black"),labels="US Lower 48 Total" )+
  scale_linetype_manual("",values=c("21"),labels="US Lower 48 Total" )+
  scale_fill_viridis("",option = "C",discrete = T,direction=-1)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_datetime(labels = date_format("%d %b\n%H:00"),breaks = "12 hours", expand=c(0,0))+
  paper_theme()+
  #expand_limits(y=0)+ #make sure you get the zero line
  #guides(color = guide_legend())+
  theme(#legend.position="bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm")
    #legend.text = element_text(colour="black", size = 12, face = "bold")
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Hourly System Load (MW)",x="Eastern Time",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
hourly_graph 
ggsave("images/us_regions_area.png", width=14, height=7, dpi=300,bg="white")




