library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(scales)


brooks_vols<-read_csv("brooks_data.csv",col_names = c("date","pool_price","brooks_vols"),skip = 3)%>%
      mutate(date=dmy_hms(date),hour_start=hour(date),month=factor(month.abb[month(date)],levels = month.abb),
             year=year(date),day=as.Date(date))%>%filter(!is.na(brooks_vols))

graph_data<-brooks_vols %>% group_by(month,hour_start)%>% 
  summarize(min_vol=min(brooks_vols),max_vol=max(brooks_vols),mean_vol=mean(brooks_vols),
            sd_vol=sd(brooks_vols) )
start_date<-format(min(brooks_vols$date),format = "%b %d, %Y")
end_date<-format(max(brooks_vols$date),format = "%b %d, %Y")
target_months<-month.abb

ggplot(filter(graph_data,month %in% target_months)) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+

  geom_line(data=filter(brooks_vols,month %in% target_months),aes(hour_start,brooks_vols/15,group=day,colour="Daily patterns"),
            size=.05)+
  geom_line(size=1.5,aes(hour_start,mean_vol/15,color="Monthly mean pattern"))+
  facet_wrap(~month,ncol = 4)+
  scale_x_continuous(breaks=seq(0,23,3))+
  scale_y_continuous(breaks=pretty_breaks(),limits = c(0,1.05),labels=percent)+
  scale_colour_manual("",values = c("grey30","dodgerblue"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Hourly Capacity Factor",x="Hour",
             title="Brooks Solar Plant Hourly Capacity Factor by Month",
             subtitle=paste("Monthly mean and range of hourly capacity factors from ",start_date," to ",end_date,sep=""),
             caption="Source: Generation data via NRGStream\nGraph by Andrew Leach")
ggsave("brooks_ajl.png",width = 16,height = 10)

ggplot(filter(graph_data,month %in% target_months)) +
  geom_ribbon(aes(x = hour_start, ymin = min_vol/15, ymax = max_vol/15,fill="A"),alpha=.25, linetype = 0)+
  #geom_line(data=filter(brooks_vols,month %in% target_months),aes(hour_start,brooks_vols/15,group=day,colour="Daily patterns"),
  #          size=.05)+
  geom_line(size=1.5,aes(hour_start,mean_vol/15,color="Monthly mean pattern"))+
  facet_wrap(~month,ncol = 4)+
  scale_x_continuous(breaks=seq(0,23,3))+
  scale_y_continuous(breaks=pretty_breaks(),limits = c(0,1.05),labels=percent)+
  scale_fill_manual("",values = c("grey30","dodgerblue"),labels="Monthly Range")+
  scale_colour_manual("",values = c("grey30","dodgerblue"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Hourly Capacity Factor",x="Hour",
             title="Brooks Solar Plant Hourly Capacity Factor by Month",
             subtitle=paste("Monthly mean and range of hourly capacity factors from ",start_date," to ",end_date,sep=""),
             caption="Source: Generation data via NRGStream\nGraph by Andrew Leach")
ggsave("brooks_ribbon.png",width = 16,height = 10)

ggplot(filter(graph_data,month %in% target_months)) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_errorbar(aes(x=hour_start,ymin=min_vol-2*sd_vol, ymax=max_vol+2*sd_vol,color="95% confidence interval"), width=1) +
  #geom_line(data=filter(brooks_vols,month %in% target_months),aes(hour_start,brooks_vols/15,group=day,colour="Daily patterns"),
  #          size=.05)+
  geom_line(size=1.5,aes(hour_start,mean_vol,color="Monthly mean pattern"))+
  facet_wrap(~month,ncol = 4)+
  scale_x_continuous(breaks=seq(0,23,3))+
  scale_y_continuous(breaks=pretty_breaks())+
  scale_colour_manual("",values = c("grey30","dodgerblue"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Hourly Capacity Factor",x="Hour",
             title="Brooks Solar Plant Hourly Capacity Factor by Month",
             subtitle=paste("Monthly mean and range of hourly capacity factors from ",start_date," to ",end_date,sep=""),
             caption="Source: Generation data via NRGStream\nGraph by Andrew Leach")
ggsave("brooks_box.png",width = 16,height = 10)



ggplot(filter(graph_data,month %in% target_months)) +
  geom_ribbon(aes(x = hour_start, ymin = min_vol/15, ymax = max_vol/15,fill="A"),alpha=.25, linetype = 0)+
  geom_line(data=filter(brooks_vols,month %in% target_months),aes(hour_start,brooks_vols/15,group=day,colour="Daily patterns"),
            size=.05)+
  geom_line(size=1.5,aes(hour_start,mean_vol/15,color="Monthly mean pattern"))+
  facet_wrap(~month,ncol = 4)+
  scale_x_continuous(breaks=seq(0,23,3))+
  scale_y_continuous(breaks=pretty_breaks(),limits = c(0,1.05),labels=percent)+
  scale_fill_manual("",values = c("grey30","dodgerblue"),labels="Monthly Range")+
  scale_colour_manual("",values = c("grey30","dodgerblue"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Hourly Capacity Factor",x="Hour",
             title="Brooks Solar Plant Hourly Capacity Factor by Month",
             subtitle=paste("Monthly mean and range of hourly capacity factors from ",start_date," to ",end_date,sep=""),
             caption="Source: Generation data via NRGStream\nGraph by Andrew Leach")
ggsave("brooks_ribbon_2.png",width = 16,height = 10)

