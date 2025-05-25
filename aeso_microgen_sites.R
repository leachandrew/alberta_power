library(tidyverse)
library(readxl)
library(cowplot)
library(janitor)
library(scales)
#if(!file.exists("mg_data.xlsx"))
  download.file("https://www.aeso.ca/assets/Small-DERS-Microgen-Monthly-by-Type-v3.xlsx", destfile = "mg_data.xlsx",mode = "wb")

mg_data<-read_xlsx("mg_data.xlsx",sheet="Microgen")%>%clean_names()%>%
mutate(microgen_fuel_type=as_factor(microgen_fuel_type),
       microgen_fuel_type=fct_other(microgen_fuel_type,keep="Solar")
)%>%
  group_by(month,microgen_fuel_type)%>%
  summarize(microgen_count=sum(microgen_count,na.rm=T),
            microgen_capacity_mw=sum(microgen_capacity_mw,na.rm=T))%>%
  ungroup()


capacity<-
  ggplot(mg_data)+
  geom_area(aes(month,microgen_capacity_mw,group=microgen_fuel_type,fill=microgen_fuel_type))+
  labs(y="Microgen Capacity (MW)",x="")+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y",expand = c(0,0))+
  expand_limits(x=Sys.time())+
  scale_fill_manual("",values = colors_ua10()[c(2,1)])+
  theme_classic() +
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=0)+
  theme(text=element_text(size=18),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(margin = margin(l = 5)),
        legend.key.width=unit(1,"line"),
        legend.position = "bottom",
        legend.box = "vertical",
  )
capacity

sites<-
  ggplot(mg_data)+
  geom_area(aes(month,microgen_count,group=microgen_fuel_type,fill=microgen_fuel_type))+
  labs(y="Microgen Sites (#)",x="")+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y",expand = c(0,0))+
  expand_limits(x=Sys.time(),y=15000)+
  scale_fill_manual("",values = colors_ua10()[c(2,1)])+
  theme_classic() +
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=0)+
  theme(text=element_text(size=18),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(margin = margin(l = 5)),
        legend.key.width=unit(1,"line"),
        legend.position = "bottom",
        legend.box = "vertical",
  )

plot_grid(capacity,sites)