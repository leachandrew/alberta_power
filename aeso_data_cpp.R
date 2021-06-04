#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")

#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())
source("../andrew_base.R")

library(grid)
library(gridExtra)
library(ggpubr)
#library(gganimate)
library(timeDate)
library(broom)
library(ggthemes)
library(directlabels)

library(directlabels)
blakes_blue<-"#4477AA"


graph_grey<-"grey20"
graph_black<-"black"


blake_theme<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.7)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.5)),
          legend.title = element_text(color="grey10",size=rel(.5)),
          legend.text = element_text(color="grey10",size=rel(.5)),
          axis.title = element_text(size=rel(.8)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          legend.position = "none",
          plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}

#load(file="metered_vols_data.Rdata")
load(file="gen.Rdata") 

load(file="forecast_data.Rdata") 
forecast_data <- forecast_data %>% filter (he!="02*")




#direct label graph with AESO data

#align variable names
df2 <- gen_data %>% 
  select(time=Time,vol=gen,ID,AESO_Name,Plant_Type,Plant_Fuel) %>%
  filter(Plant_Type %in% c("COAL","COGEN","NGCC","WIND","SCGT","TRADE","HYDRO","SOLAR","OTHER"))%>%
  filter(year(time)>=2010) %>% left_join(forecast_data) %>% filter(!is.na(date))%>%
  #strip the AB-WECC tie since it's duplicate of AB-MT and AB-BC
  filter(!ID %in% c("AB_WECC","AB_WECC_Exp","AB_WECC_Imp"))%>%
  mutate(Plant_Type=as.character(Plant_Type))%>%
  mutate(Plant_Fuel=as.character(Plant_Fuel))%>%
  mutate(hour=hour(time))%>%
  select(-c("forecast_pool_price","day_ahead_forecasted_ail",        
                "forecasted_actual_ail_difference","start_date"))%>%
  filter(date>Sys.Date()-years(10))%>%
  mutate(month=month(time),year=year(time))%>%   
  mutate(Plant_Type=as_factor(Plant_Type),
         # Relevel to the end
         Plant_Type=fct_other(Plant_Type,keep = c("COAL","COGEN","SCGT","NGCC","WIND","HYDRO","TRADE"),other_level = "OTHER"),
         Plant_Type=fct_relevel(Plant_Type, "OTHER", after = Inf),
         Plant_Type=fct_relevel(Plant_Type, "TRADE", after = Inf),
         NULL
  )%>%
  #summarize by hour to get total gen and revenue by fuel, 
  group_by(year,month,date,hour,Plant_Type) %>% summarise(gen=sum(vol,na.rm = T),rev=gen*actual_posted_pool_price) %>% 
  #summarize by year and month to get mean gen and capture price by fuel, 
  group_by(year,month,Plant_Type) %>% summarise(gen=mean(gen,na.rm = T),capture = sum(rev)/sum(gen))%>% 
  ungroup() %>%
  mutate(date=ymd(paste(year,month,15,sep="-")))
  
  



AB_plant_order<-c("COAL","COGEN","NGCC","WIND","HYDRO","SCGT","OTHER","TRADE")
AB_palette<- c("black","grey80","grey50",ptol_pal()(6)[3],ptol_pal()(6)[1],"grey50",ptol_pal()(6)[5],ptol_pal()(6)[6])



#covid_mid<-ymd("2020-03-11")+days(round((ymd("2020-07-01")-ymd("2020-03-11"))/2))
#covid_mid_lag<-ymd("2020-03-11")+days(round((ymd("2020-07-01")-ymd("2020-03-11"))/2))-years(1)


gen_plain <- df2 %>% mutate(Plant_Type=factor(Plant_Type,levels=AB_plant_order))%>%
  #filter(date>ymd("2014-12-31"))%>%
  ggplot(aes(date,gen, col = Plant_Type,fill = Plant_Type,shape=Plant_Type)) +
  geom_line(size=1.25)+
  geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
  geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = 1.05))+
  scale_color_manual("",values= AB_palette)+
  scale_fill_manual("",values= AB_palette)+
  
  #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
  #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
  blake_theme()+
  scale_x_date(date_labels = "%b %d\n%Y",date_breaks = "6 months",expand=c(0,0))+
  expand_limits(x = as.Date(c("2015-01-01", "2020-12-31")))+
  scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
  #theme(legend.position = "right")+
  labs(x="",y="Monthly Average Hourly Generation or Imports (MW)",
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
  gen_plain
  ggsave(file="gen_plain.png", width = 16, height = 9,dpi = 600)
  

  
  
  AB_palette<- c("black","grey80","grey30","grey40","grey20","grey50","grey70","grey60")
  
  gen_plain <- 
    df2 %>% mutate(Plant_Type=factor(Plant_Type,levels=AB_plant_order))%>%
    filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,gen, col = Plant_Type,fill = Plant_Type,shape=Plant_Type)) +
    geom_line(size=1.25)+
    geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = 1.05))+
    scale_color_manual("",values= AB_palette)+
    scale_fill_manual("",values= AB_palette)+
    
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    blake_theme()+
    scale_x_date(date_labels = "%b %d\n%Y",date_breaks = "6 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2015-01-01", "2020-12-31")))+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    #theme(legend.position = "right")+
    labs(x="",y="Monthly Average Hourly Generation or Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    annotate("rect", fill = "grey70", alpha = .3, 
             xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
             ymin = -Inf, ymax = Inf)+
    annotate("rect", fill = "grey70", alpha = .3, 
             xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
             ymin = -Inf, ymax = Inf)+
    annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
  
  gen_plain
  ggsave(file="gen_grey.png", width = 16, height = 9,dpi = 600)
  
  
  
  gen_fuel <- df2 %>% mutate(Plant_Type=factor(Plant_Type,levels=AB_plant_order))%>%
    filter(date<ymd("2020-09-01"))%>%
    mutate(Plant_Type=fct_collapse(Plant_Type,
     "OTHER"=c("WIND","OTHER","HYDRO"),
     "NATURAL\nGAS"=c("SCGT","COGEN","NGCC"),
     "NET\nIMPORTS"="TRADE"
     ))%>% 
    group_by(date,month,year,Plant_Type) %>% summarise(gen=sum(gen,na.rm = T))%>% 
    ungroup() %>%
    #filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,gen, col = Plant_Type,fill = Plant_Type,shape=Plant_Type)) +
    geom_line(size=1.25)+
    geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = 1.05))+
    scale_color_manual("",values= AB_palette)+
    scale_fill_manual("",values= AB_palette)+
    
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    blake_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "12 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2015-01-01", "2021-6-30")))+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    #theme(legend.position = "right")+
    labs(x="",y="Monthly Average Hourly Generation or Imports (MW)",
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
  ggsave(file="gen_fuel.png", width = 16, height = 9,dpi = 600)
  
  
  
  
  
  #checking trade
  covid_mid<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)
  covid_mid_lag<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)-years(1)
  
  
  
  trade_flows <- 
    gen_data %>% 
  filter(Plant_Type %in% c("TRADE"))%>% filter(year(time)>=2017)%>% 
    group_by(date,Plant_Type,ID) %>% summarise(vol=mean(vol,na.rm = T))%>%
    ungroup %>% 
    group_by(ID)%>%
    mutate(value=roll::roll_mean(vol,7)) %>%ungroup()%>%
    mutate(d_index=ymd(paste(2020,month(date),day(date),sep="-"))) %>%  
    mutate(year=year(date)) %>%
    ggplot() +
    #geom_area(position = "stack")+
    geom_line(aes(d_index,value,group=factor(year),size=factor(year),color=factor(year)))+
    #geom_line(aes(d_index,value,group=year,colour="grey30"),size=.5)+
    #geom_line(aes(d_index,ifelse(year==2020,value,NA),group=year,colour="dodgerblue"),size=1)+
    #geom_ribbon(aes(date,ymax=max_flow,ymin=min_flow),alpha=.2,size=0)+
    blake_theme()+
    facet_wrap(~ID)+
    scale_x_date(date_labels = "%d\n%b",date_breaks = "2 months")+
    #expand_limits(x = as.Date(c("2019-12-15", "2020-12-31")))+
    scale_y_continuous(breaks=pretty_breaks())+
    scale_color_manual("",values=c(grey.colors(3,end=0.7),blakes_blue))+
    scale_size_manual("",values=c(rep(.5,3),1.5))+
    theme(legend.position = "bottom")+
    annotate("text", x = covid_mid, y =-500, label = "COVID\nperiod",size=4,hjust=0.5,vjust=0.5)+
    annotate("rect", fill = "grey80", alpha = .3, 
             xmin = as.Date("2020-03-11"), xmax =as.Date("2020-06-08"),
             ymin = -Inf, ymax = +Inf)+
    labs(x="",y="Average Daily Net Imports (MW)",
         #      #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #      title="Ontario Transmission-Connected Generation by Fuel (MW, 2019-2020)",
         #      caption="Source: IESO Data"
         NULL)+
    NULL
  trade_flows
  ggsave("aeso_trade_flows.png",width=16,height = 6,dpi = 600)
  
  
  

  
  
#supply-side regressions

#hourly data by plant type
df1 <- gen_data %>% 
  filter(year(date)>=2015,he!="02*")%>%
  #mutate(                         Plant_Type = case_when((Plant_Type=="TRADE" & vol<0)  ~ "NET IMPORTS",
  #                                                       (Plant_Type=="TRADE" & vol>=0)  ~ "NET IMPORTS",
  #                                                       TRUE~Plant_Type)
  #)%>%
  #filter(Plant_Type!="EXPORT")%>%
  group_by(time,Plant_Type) %>% summarise(gen=sum(vol,na.rm = T)) %>% ungroup()%>%
  mutate(Plant_Type=as_factor(Plant_Type),
         # Relevel to the end
         Plant_Type=fct_other(Plant_Type,keep = c("COAL","COGEN","SCGT","NGCC","WIND","TRADE"),other_level = "OTHER"),
         Plant_Type=fct_relevel(Plant_Type, "OTHER", after = Inf),
         Plant_Type=fct_relevel(Plant_Type, "TRADE", after = Inf)
  )%>%
  ungroup() 


  
#load temps

temp <- read_csv("../Demand analysis/full_weather_data_calgary_hourly_allstations.csv") %>% mutate(Province="AB")%>%
  filter(year>=2015) %>%
  mutate(date=paste(year,month,day,sep="-"),
         time=paste(date,time,sep=" "),
         time=ymd_hms(time),
         HD=ifelse(temp<=14,14-temp,0),
         CD=ifelse(temp>14,temp-14,0),
         HD2=HD*HD,
         CD2=CD*CD) %>%
  select(time,temp,CD,HD,CD2,HD2)


# Merge load and temperature
df <- left_join(df1,temp, by=c("time")) %>%
  drop_na() %>% #drop where we are missing temperatures
  mutate(year=year(time),month=month(time),Date=as.Date(time))%>%
  mutate(Date_Group = ifelse(year==2020,as.character(as.Date(time)),0))%>% assign_peaks()%>%
  rename(MW=gen)%>%
  mutate(log_MW=log(MW),
         HE=hour(time),
         DOY=wday(time),
         Weekday=ifelse(DOY %in% c(7,1),1,0),   #wday=7 is Saturday, wday=1 is Sunday
         WOY=week(time),
         DOY=yday(time),
         Year=year(time),
         Month=month(time),
         Season=ifelse((Month>=4 & Month<=10),"Summer","Winter"),
         Quarter=ifelse(Month<=3,"Q1",ifelse(Month<=6,"Q2",ifelse(Month<=9,"Q3","Q4")))) 

 
#rm(gen_data,df1,temp)

# Summary stat
stats <- df %>%
  #group_by(Province) %>%
  summarise(min_MW=min(MW),
            mean_MW=mean(MW),
            max_MW=max(MW),
            min_temp=min(temp),
            mean_temp=mean(temp),
            max_temp=max(temp))




#supply-side regressions

by_plant <-df %>%# mutate(sample=runif(n(),0, 1))%>% filter(sample<=.1) %>% #10% subsample
  mutate(covid=ifelse(Date>=as.Date("2020-03-11"),1,0))%>%
  mutate(Date_Group = ifelse(covid==1,as.character(as.Date(time)),0))%>%
  nest(-Plant_Type) %>% 
  mutate(
    fit = map(data, ~ lm(MW ~ factor(year)+factor(WOY)+factor(Month)+factor(Date_Group)+factor(HE)*factor(on_peak) + CD + HD + CD2 + HD2, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied) 

proc<-by_plant %>% select(-data,-fit,-glanced)%>%
  filter(str_detect(term,"Date_Group")) %>%
  mutate(date_label=str_remove(term,fixed("factor(Date_Group)")),
         date_label=ymd(date_label))%>%
      group_by(Plant_Type)%>%
         mutate(
           estimate.7ma=roll::roll_mean(estimate,7))
  

covid_mid<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)
covid_mid_lag<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)-years(1)

    proc %>% filter(Plant_Type %in% c("NGCC","SCGT","COGEN"))%>%
      mutate(stat_sig=(p.value<=.05))%>%
    ggplot(aes(date_label,estimate.7ma)) +
      geom_line(size=1.5,color=blakes_blue)+
      geom_point(aes(date_label,estimate,shape=stat_sig),size=1.25,color=blakes_blue)+
      scale_shape_manual("",values=c(1,16))+
      
      facet_wrap(~Plant_Type)+
      blake_theme()+
      scale_x_date(date_labels = "%d\n%b",date_breaks = "1 months",expand=c(0,0) )+
      expand_limits(x = as.Date(c("2020-02-28", "2020-06-01")))+
      scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
      #scale_fill_manual("",values=colors_tableau10())+
      guides(colour=guide_colorbar(barwidth = 10, barheight = 0.75),shape=FALSE)+
      theme(legend.position = "bottom")+
    labs(x="",y="Change in Average Hourly Generation (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Estimated Decreases in Natural Gas Generation by Plant Type",
         #subtitle="Shaded area represents the 95% confidence interval of daily COVID-period fixed effect estimates",
         #caption="Source: AESO Data, authors' calculations"
         NULL)+
    #annotate("text", x = covid_mid, y =0, label = "COVID\nperiod",size=3.25,hjust=0.5)+
    #annotate("rect", fill = "grey80", alpha = .3, 
    #         xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #         ymin = -Inf, ymax = Inf)
    NULL
    ggsave("covid_aeso_supply.png",width=16,height = 6,dpi = 600)
  
    proc %>% filter(Plant_Type %in% c("NGCC","SCGT","COGEN"))%>%
      mutate(stat_sig=(p.value<=.05))%>%
      ggplot(aes(date_label,estimate.7ma)) +
      geom_line(size=1.5,color=graph_black)+
      geom_point(aes(date_label,estimate,shape=stat_sig),size=1.25,color=graph_grey)+
      scale_shape_manual("",values=c(1,16))+
      facet_wrap(~Plant_Type)+
      blake_theme()+
      scale_x_date(date_labels = "%d\n%b",date_breaks = "1 months",expand=c(0,0) )+
      expand_limits(x = as.Date(c("2020-02-28", "2020-06-01")))+
      scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
      #scale_fill_manual("",values=colors_tableau10())+
      guides(colour=guide_colorbar(barwidth = 10, barheight = 0.75),shape=FALSE)+
      theme(legend.position = "bottom")+
      labs(x="",y="Change in Average Hourly Generation (MW)",
           #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
           #title="Estimated Decreases in Natural Gas Generation by Plant Type",
           #subtitle="Shaded area represents the 95% confidence interval of daily COVID-period fixed effect estimates",
           #caption="Source: AESO Data, authors' calculations"
           NULL)+
      #annotate("text", x = covid_mid, y =0, label = "COVID\nperiod",size=3.25,hjust=0.5)+
      #annotate("rect", fill = "grey80", alpha = .3, 
      #         xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
      #         ymin = -Inf, ymax = Inf)
      NULL
    ggsave("covid_aeso_supply_grey.png",width=16,height = 6,dpi = 600)
    
    
    #hourly data by plant fuel
    df2 <- gen_data %>% 
      filter(year(date)>=2015,he!="02*")%>%
      #mutate(                         Plant_Type = case_when((Plant_Type=="TRADE" & vol<0)  ~ "NET IMPORTS",
      #                                                       (Plant_Type=="TRADE" & vol>=0)  ~ "NET IMPORTS",
      #                                                       TRUE~Plant_Type)
      #)%>%
      #filter(Plant_Type!="EXPORT")%>%
      ungroup()%>% 
      mutate(Plant_Fuel=as_factor(Plant_Fuel),
             # Relevel to the end
             Plant_Fuel=fct_other(Plant_Fuel,keep = c("COAL","GAS","TRADE","WIND","HYDRO"),other_level = "OTHER"),
             #Plant_Type=fct_relevel(Plant_Type, "IMPORT", after = Inf),
             #Plant_Fuel=fct_relevel(Plant_Fuel, "OTHER", after = Inf)
             Plant_Fuel=fct_relevel(Plant_Fuel, "WIND", after = Inf),
             Plant_Fuel=fct_relevel(Plant_Fuel, "HYDRO", after = Inf),
             Plant_Fuel=fct_relevel(Plant_Fuel, "TRADE", after = Inf),
             Plant_Fuel=fct_relevel(Plant_Fuel, "OTHER", after = Inf)
      )%>%
      group_by(time,Plant_Fuel) %>% summarise(gen=sum(vol,na.rm = T))%>%ungroup()
      
      
    
    # Merge load and temperature
    df_fuel <- left_join(df2,temp, by=c("time")) %>%
      drop_na() %>%
      mutate(year=year(time),month=month(time),Date=as.Date(time))%>%
      mutate(Date_Group = ifelse(year==2020,as.character(as.Date(time)),0))%>% assign_peaks()%>%
      rename(MW=gen)%>%
      mutate(log_MW=log(MW),
             HE=hour(time),
             DOY=wday(time),
             Weekday=ifelse(DOY %in% c(7,1),1,0),   #wday=7 is Saturday, wday=1 is Sunday
             WOY=week(time),
             DOY=yday(time),
             Year=year(time),
             Month=month(time),
             Season=ifelse((Month>=4 & Month<=10),"Summer","Winter"),
             Quarter=ifelse(Month<=3,"Q1",ifelse(Month<=6,"Q2",ifelse(Month<=9,"Q3","Q4")))) 
    
    
    
    
    
    by_fuel <-df_fuel %>% #mutate(sample=runif(n(),0, 1))%>% filter(sample<=.1) %>% #10% subsample
      mutate(covid=ifelse(Date>=as.Date("2020-03-11"),1,0))%>%
      mutate(Date_Group = ifelse(covid==1,as.character(as.Date(time)),0))%>%
      nest(-Plant_Fuel) %>% 
      mutate(
        fit = map(data, ~ lm(MW ~ factor(year)+factor(WOY)+factor(Month)+factor(Date_Group)+factor(HE)*factor(on_peak) + CD + HD + CD2 + HD2, data = .x)),
        tidied = map(fit, tidy),
        glanced = map(fit, glance)
      ) %>% 
      unnest(tidied) 
    
    proc2<-by_fuel %>% select(-data,-fit,-glanced)%>%
      filter(str_detect(term,"Date_Group")) %>%
      mutate(date_label=str_remove(term,fixed("factor(Date_Group)")),
             date_label=ymd(date_label))%>%
      group_by(Plant_Fuel)%>%
      mutate(
        estimate.7ma=roll::roll_mean(estimate,7))
    
    
    covid_mid<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)
    covid_mid_lag<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)-years(1)
    
    proc2 %>% #filter(Plant_Fuel!="WIND",Plant_Fuel!="SOLAR")%>%
      mutate(stat_sig=(p.value<=.05),
             Plant_Fuel=fct_relevel(Plant_Fuel,"TRADE",after=Inf)
             )%>%
      ggplot(aes(date_label,estimate.7ma)) +
      geom_line(size=1.5,color="#4477AA")+
      geom_point(aes(date_label,estimate,shape=stat_sig),size=1.25,color="#4477AA")+
      scale_shape_manual("",values=c(1,16))+
      
      #scale_color_gradient("H0: Daily fixed effect=0 p-value:   \n",low = "#4477AA",high = "white")+
      #scale_color_manual(expression('H'[0]*': daily fixed effect=0 p-value'),low = "#4477AA",high = "white")+
      facet_wrap(~Plant_Fuel)+
      blake_theme()+
      scale_x_date(date_labels = "%d\n%b",date_breaks = "1 months",expand=c(0,0) )+
      expand_limits(x = as.Date(c("2020-02-28", "2020-06-01")))+
      scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
      #scale_fill_manual("",values=colors_tableau10())+
      guides(colour=guide_colorbar(barwidth = 10, barheight = 0.75),shape=FALSE)+
      theme(legend.position = "bottom")+
      labs(x="",y="Change in Average Hourly Generation (MW)",
           #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
           #title="Estimated Decreases in Natural Gas Generation by Plant Type",
           #subtitle="Shaded area represents the 95% confidence interval of daily COVID-period fixed effect estimates",
           #caption="Source: AESO Data, authors' calculations"
           NULL)+
      #annotate("text", x = covid_mid, y =0, label = "COVID\nperiod",size=3.25,hjust=0.5)+
      #annotate("rect", fill = "grey80", alpha = .3, 
      #         xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
      #         ymin = -Inf, ymax = Inf)
      NULL
    ggsave("covid_aeso_fuel.png",width=16,height = 9,dpi = 600)
    
    proc2 %>% #filter(Plant_Fuel!="WIND",Plant_Fuel!="SOLAR")%>%
      mutate(stat_sig=(p.value<=.05),
             Plant_Fuel=fct_relevel(Plant_Fuel,"TRADE",after=Inf)
      )%>%
      ggplot(aes(date_label,estimate.7ma)) +
      geom_line(size=1.5,color=graph_black)+
      geom_point(aes(date_label,estimate,shape=stat_sig),size=1.25,color=graph_grey)+
      scale_shape_manual("",values=c(1,16))+
      
      #scale_color_gradient("H0: Daily fixed effect=0 p-value:   \n",low = "#4477AA",high = "white")+
      #scale_color_manual(expression('H'[0]*': daily fixed effect=0 p-value'),low = "#4477AA",high = "white")+
      facet_wrap(~Plant_Fuel)+
      blake_theme()+
      scale_x_date(date_labels = "%d\n%b",date_breaks = "1 months",expand=c(0,0) )+
      expand_limits(x = as.Date(c("2020-02-28", "2020-06-01")))+
      scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
      #scale_fill_manual("",values=colors_tableau10())+
      guides(colour=guide_colorbar(barwidth = 10, barheight = 0.75),shape=FALSE)+
      theme(legend.position = "bottom")+
      labs(x="",y="Change in Average Hourly Generation (MW)",
           #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
           #title="Estimated Decreases in Natural Gas Generation by Plant Type",
           #subtitle="Shaded area represents the 95% confidence interval of daily COVID-period fixed effect estimates",
           #caption="Source: AESO Data, authors' calculations"
           NULL)+
      #annotate("text", x = covid_mid, y =0, label = "COVID\nperiod",size=3.25,hjust=0.5)+
      #annotate("rect", fill = "grey80", alpha = .3, 
      #         xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
      #         ymin = -Inf, ymax = Inf)
      NULL
    ggsave("covid_aeso_fuel_grey.png",width=16,height = 9,dpi = 600)
  
      
covid_only<-df_fuel %>% #mutate(sample=runif(n(),0, 1))%>% filter(sample<=.1) %>% #10% subsample
  mutate(covid=ifelse(Date>=as.Date("2020-03-11"),1,0))%>%
  mutate(Date_Group = ifelse(covid==1,as.character(as.Date(time)),0))%>%
  nest(-Plant_Fuel) %>% 
  mutate(
    fit = map(data, ~ lm(MW ~ factor(year)+factor(WOY)+factor(Month)+covid+factor(HE)*factor(on_peak) + CD + HD + CD2 + HD2, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied) 

proc3<-covid_only %>% select(-data,-fit,-glanced)%>%
  filter(str_detect(term,"covid"))%>%
  mutate(p.value=round(p.value,digits = 4))


covid_only_plant<-df %>% #mutate(sample=runif(n(),0, 1))%>% filter(sample<=.1) %>% #10% subsample
  mutate(covid=ifelse(Date>=as.Date("2020-03-11"),1,0))%>%
  mutate(Date_Group = ifelse(covid==1,as.character(as.Date(time)),0))%>%
  nest(-Plant_Type) %>% 
  mutate(
    fit = map(data, ~ lm(MW ~ factor(year)+factor(WOY)+factor(Month)+covid+factor(HE)*factor(on_peak) + CD + HD + CD2 + HD2, data = .x)),
    tidied = map(fit, tidy),
    #glanced = map(fit, glance)
  ) %>% 
  unnest(tidied) 

proc4<-covid_only_plant %>% select(-data,-fit)%>%
  filter(str_detect(term,"covid"))%>%
  mutate(p.value=round(p.value,digits = 4))


#oil sands net gen
download.file("https://www.aeso.ca/assets/Uploads/Hourly-Oilsand-Generation-and-Demand.xlsx",destfile = "aeso_os_net.xlsx",mode="wb")
os_net<-read_excel("aeso_os_net.xlsx")%>% clean_names() %>% rename(time=begin_date_local)%>%
  pivot_longer(-time,names_to="volume",values_to="MW")



os_flows <- 
  os_net%>% mutate(year=year(time),date=date(time))%>% filter(date>=ymd("2017-01-01")-days(6))%>%
  group_by(year,date,volume) %>% summarize(value=mean(MW)) %>% ungroup()%>%
  mutate(value.7ma=roll::roll_mean(value,7),
         d_index=ymd(paste(2020,month(date),day(date),sep="-"))) %>%
  mutate(
    labels=case_when(volume=="net_gw"  ~ "Net Oil Sands Generation",
                     volume=="load_gw"  ~ "Power Imports to Site",
                     volume=="gen_gw"  ~ "Power Exports from Site",
                     TRUE~volume),
  labels=factor(labels,levels = unique(labels)))%>%filter(year>=2017)%>%
  group_by(volume)%>% mutate(value=roll::roll_mean(value,7)) %>% ungroup()%>%
  ggplot() +
  #geom_area(position = "stack")+
  geom_line(aes(d_index,value,group=factor(year),colour=factor(year),size=factor(year)))+
  #geom_line(aes(d_index,value,group=year,colour="grey30"),size=.5)+
  #geom_line(aes(d_index,ifelse(year==2020,value,NA),group=year,colour="dodgerblue"),size=1)+
  #geom_ribbon(aes(date,ymax=max_flow,ymin=min_flow),alpha=.2,size=0)+
  blake_theme()+
  facet_wrap(~labels,scales="free_y")+
  scale_x_date(date_labels = "%d\n%b",date_breaks = "2 months")+
  #expand_limits(x = as.Date(c("2019-12-15", "2020-12-31")))+
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_manual("",values=c(grey.colors(3,end = .7),blakes_blue))+
  scale_size_manual("",values=c(rep(.5,3),1.5))+
  theme(legend.position = "bottom")+
  #annotate("text", x = covid_mid, y =1200, label = "COVID\nperiod",size=4,hjust=0.5,vjust=0.5)+
  annotate("rect", fill = "grey80", alpha = .3, 
           xmin = as.Date("2020-03-11"), xmax =as.Date("2020-06-08"),
           ymin = -Inf, ymax = +Inf)+
  labs(x="",y="Average Daily Metered Flows (MW)",
       #      #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
       #      title="Ontario Transmission-Connected Generation by Fuel (MW, 2019-2020)",
       #      caption="Source: IESO Data"
       NULL)+
  NULL
os_flows
ggsave("covid_os_flows.png",width=16,height = 6,dpi = 600)



ymm_temp <- read_csv("../Demand analysis/full_weather_data_Fort MacMurray_hourly_allstations.csv") %>% mutate(Province="AB")%>%
  filter(year>=2015) %>%
  mutate(date=paste(year,month,day,sep="-"),
         time=paste(date,time,sep=" "),
         time=ymd_hms(time),
         HD=ifelse(temp<=14,14-temp,0),
         CD=ifelse(temp>14,temp-14,0),
         HD2=HD*HD,
         CD2=CD*CD) %>%
  select(time,temp,CD,HD,CD2,HD2)


df_os <- left_join(os_net,ymm_temp, by=c("time")) %>%
  drop_na() %>%
  mutate(year=year(time),month=month(time),Date=as.Date(time))%>%
  mutate(Date_Group = ifelse(year==2020,as.character(as.Date(time)),0))%>% assign_peaks()%>%
  #rename(MW=gen)%>%
  mutate(
         HE=hour(time),
         DOY=wday(time),
         Weekday=ifelse(DOY %in% c(7,1),1,0),   #wday=7 is Saturday, wday=1 is Sunday
         WOY=week(time),
         DOY=yday(time),
         Year=year(time),
         Month=month(time),
         Season=ifelse((Month>=4 & Month<=10),"Summer","Winter"),
         Quarter=ifelse(Month<=3,"Q1",ifelse(Month<=6,"Q2",ifelse(Month<=9,"Q3","Q4")))) 


os_covid<-df_os %>% #mutate(sample=runif(n(),0, 1))%>% filter(sample<=.1) %>% #10% subsample
  mutate(covid=ifelse(Date>=as.Date("2020-03-11"),1,0))%>%
  mutate(syncrude_explosion=ifelse((Date>=as.Date("2017-03-01")& Date<=as.Date("2017-05-01")),1,0))%>%
  mutate(Date_Group = ifelse(covid==1,as.character(as.Date(time)),0))%>%
  nest(-volume) %>% 
  mutate(
    fit = map(data, ~ lm(MW ~ factor(year)+factor(WOY)+factor(Month)+covid+syncrude_explosion+factor(HE)*factor(on_peak) + CD + HD + CD2 + HD2, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied) 

proc_os<-os_covid %>% select(-data,-fit,-glanced)%>%
  filter(str_detect(term,"covid"))%>%
  mutate(p.value=round(p.value,digits = 4))

os_full<-df_os %>% #mutate(sample=runif(n(),0, 1))%>% filter(sample<=.1) %>% #10% subsample
  mutate(covid=ifelse(Date>=as.Date("2020-03-11"),1,0))%>%
  mutate(Date_Group = ifelse(covid==1,as.character(as.Date(time)),0))%>%
  mutate(syncrude_explosion=ifelse((Date>=as.Date("2017-03-01")& Date<=as.Date("2017-05-01")),1,0))%>%
  nest(-volume) %>% 
  mutate(
    fit = map(data, ~ lm(MW ~ factor(year)+factor(WOY)+factor(Month)+factor(Date_Group)+syncrude_explosion+factor(HE)*factor(on_peak) + CD + HD + CD2 + HD2, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied) 


proc_os_syncrude<-os_full %>% select(-data,-fit,-glanced)%>%
  filter(str_detect(term,"syncrude")) %>%
  mutate(date_label=str_remove(term,fixed("factor(Date_Group)")),
         date_label=ymd(date_label))%>%
  group_by(volume)%>%
  mutate(
    labels=case_when(volume=="net_gw"  ~ "Net Oil Sands Generation",
                     volume=="load_gw"  ~ "Power Imports to Site",
                     volume=="gen_gw"  ~ "Power Exports from Site",
                     TRUE~volume),
    labels=factor(labels,levels = unique(labels)),
    estimate.7ma=roll::roll_mean(estimate,7),
    SE.upper=estimate+1.96*std.error,
    SE.lower=estimate-1.96*std.error)


proc_os_full<-os_full %>% select(-data,-fit,-glanced)%>%
  filter(str_detect(term,"Date_Group")) %>%
  mutate(date_label=str_remove(term,fixed("factor(Date_Group)")),
         date_label=ymd(date_label))%>%
  group_by(volume)%>%
  mutate(
    labels=case_when(volume=="net_gw"  ~ "Net Oil Sands Generation",
                     volume=="load_gw"  ~ "Power Imports to Site",
                     volume=="gen_gw"  ~ "Power Exports from Site",
                     TRUE~volume),
    labels=factor(labels,levels = unique(labels)),
    estimate.7ma=roll::roll_mean(estimate,7),
    SE.upper=estimate+1.96*std.error,
    SE.lower=estimate-1.96*std.error)

proc_os_full %>% #filter(volume=="net_gw")%>%
  mutate(stat_sig=(p.value<=.05))%>%
  ggplot(aes(date_label,estimate.7ma)) +
  geom_line(size=1.5,color="#4477AA")+
  geom_point(aes(date_label,estimate,shape=stat_sig),size=1.25,color="#4477AA")+
  scale_shape_manual("",values=c(1,16))+
  #scale_color_gradient("H0: Daily fixed effect=0 p-value:   \n",low = "#4477AA",high = "white")+
  #scale_color_gradient(expression('H'[0]*': daily fixed effect=0 p-value'),low = "#4477AA",high = "white")+
  facet_wrap(~labels)+
  blake_theme()+
  scale_x_date(date_labels = "%d\n%b",date_breaks = "1 months",expand=c(0,0) )+
  expand_limits(x = as.Date(c("2020-03-10", "2020-06-01")))+
  scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
  #scale_shape_manual("",values=c(16,16))+
  #scale_fill_manual("",values=colors_tableau10())+
  guides(colour=guide_colorbar(barwidth = 10, barheight = 0.75),shape=FALSE)+
  theme(legend.position = "bottom")+
  labs(x="",y="Average Hourly Flow (MW)",
       #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
       #title="Estimated Decreases in Natural Gas Generation by Plant Type",
       #subtitle="Shaded area represents the 95% confidence interval of daily COVID-period fixed effect estimates",
       #caption="Source: AESO Data, authors' calculations"
       NULL)+
  #annotate("text", x = covid_mid, y =0, label = "COVID\nperiod",size=3.25,hjust=0.5)+
  #annotate("rect", fill = "grey80", alpha = .3, 
  #         xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
  #         ymin = -Inf, ymax = Inf)
  NULL
ggsave("covid_os_fuel.png",width=16,height = 6,dpi = 600)


proc_os_full %>% #filter(volume=="net_gw")%>%
  mutate(stat_sig=(p.value<=.05))%>%
  ggplot(aes(date_label,estimate.7ma)) +
  geom_line(size=1.5,color=graph_black)+
  geom_point(aes(date_label,estimate,shape=stat_sig),size=1.25,color=graph_grey)+
  scale_shape_manual("",values=c(1,16))+
  #scale_color_gradient("H0: Daily fixed effect=0 p-value:   \n",low = "#4477AA",high = "white")+
  #scale_color_gradient(expression('H'[0]*': daily fixed effect=0 p-value'),low = "#4477AA",high = "white")+
  facet_wrap(~labels)+
  blake_theme()+
  scale_x_date(date_labels = "%d\n%b",date_breaks = "1 months",expand=c(0,0) )+
  expand_limits(x = as.Date(c("2020-03-10", "2020-06-01")))+
  scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
  #scale_shape_manual("",values=c(16,16))+
  #scale_fill_manual("",values=colors_tableau10())+
  guides(colour=guide_colorbar(barwidth = 10, barheight = 0.75),shape=FALSE)+
  theme(legend.position = "bottom")+
  labs(x="",y="Average Hourly Flow (MW)",
       #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
       #title="Estimated Decreases in Natural Gas Generation by Plant Type",
       #subtitle="Shaded area represents the 95% confidence interval of daily COVID-period fixed effect estimates",
       #caption="Source: AESO Data, authors' calculations"
       NULL)+
  #annotate("text", x = covid_mid, y =0, label = "COVID\nperiod",size=3.25,hjust=0.5)+
  #annotate("rect", fill = "grey80", alpha = .3, 
  #         xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
  #         ymin = -Inf, ymax = Inf)
  NULL
ggsave("covid_os_grey.png",width=16,height = 6,dpi = 600)

