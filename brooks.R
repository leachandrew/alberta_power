library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(scales)


brooks_vols<-read_csv("brooks_data.csv",col_names = c("date","pool_price","brooks_vols"),skip = 3)%>%
      mutate(date=dmy_hms(date),hour_start=hour(date),month=factor(month.abb[month(date)],levels = month.abb),
             year=year(date))

graph_data<-brooks_vols %>% group_by(month,hour_start)%>% 
  summarize(min_vol=min(brooks_vols),max_vol=max(brooks_vols),mean_vol=mean(brooks_vols))

ggplot(filter(graph_data,month %in% c("Jan","Jun"))) +
  geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_line(size=.8,aes(hour_start,mean_vol))+
  facet_wrap(~month,ncol = 2)+
  scale_x_continuous(breaks=seq(0,23,3))+
  scale_fill_manual("",values = "grey40",labels="Range of Hourly Generation")+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Metered Volumes (MW)",x="Hour",
             title="Brooks Solar Plant Generation In January and June",
             subtitle="Mean and range of values",
             caption="Source: Generation data via NRGStream\nGraph by Andrew Leach")
ggsave("brooks_ajl.png",width = 16,height = 10)
