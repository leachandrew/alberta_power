#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")

#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())
source("../andrew_base.R")

library(stringi)


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


#rebuild the data
build_all<-function(){
  years<-seq(2015,2020)
  data_list<-list()
  i<-1
  load("forecast_data.Rdata")
  #year_id<-2019
  for(year_id in years){
    filename<-paste("measured_vols_",year_id,".RData",sep = "")
    print(filename)
    load(file= filename) 
    #Need to process time and dates to have date,he,hour,dest
    data_list[[i]]<-data_store%>% filter(asset_id=="TER1")
    i<-i+1
  }
  ter1_vols<-data.frame(do.call(rbind,data_list))
  save(ter1_vols, file="ter1.Rdata" ) 
  #save(all_vols, file="new_metered_vols_data.Rdata" ) 
}
build_all()


load(file="ter1.Rdata") 
load(file="forecast_data.Rdata") 
ter1_vols<-ter1_vols %>% left_join(forecast_data)

ter1_vols<-ter1_vols %>% mutate(rev=vol*actual_posted_pool_price) %>% assign_peaks()%>%
  assign_date_time_days()

df1<-ter1_vols%>%group_by(year,month_fac) %>% summarise(price=mean(actual_posted_pool_price),rev=sum(rev,na.rm=T)/sum(vol,na.rm = T),mw=sum(vol,na.rm = T))%>%
  mutate(date=ymd(paste(year,month_fac,1,sep="-")))

ggplot(df1) +
  geom_line(aes(date,rev,color="TER 1 Monthly Average Revenue"),size=1.25) +
  geom_line(aes(date,price,color="Average Hourly Pool Price"),size=1.15) +
  scale_x_date(labels = date_format("%h\n%Y"),breaks=seq.Date(as.Date("2001-01-01"),Sys.Date(),by="6 months"))+
  scale_color_manual("",values=colors_tableau10())+
  ajl_line()  +    
  labs(y="Monthly Average Revenue or Power Price ($/MWh)",x="\nMonth",
       title="TER 1 Average Revenue and AESO Pool Prices ($/MWh, 2015-Present)",
       caption="Source: AESO Data, Graph by Andrew Leach")

ggplot(df1) +
  geom_line(aes(date,mw,color="TER 1 Monthly Output (MW)"),size=1.25) +
  #geom_line(aes(date,price,color="Average Hourly Pool Price"),size=1.15) +
  scale_x_date(labels = date_format("%h\n%Y"),breaks=seq.Date(as.Date("2001-01-01"),Sys.Date(),by="6 months"))+
  scale_color_manual("",values=colors_tableau10())+
  ajl_line()  +    
  labs(y="Monthly Average Revenue or Power Price ($/MWh)",x="\nMonth",
       title="TER 1 Average Revenue and AESO Pool Prices ($/MWh, 2015-Present)",
       caption="Source: AESO Data, Graph by Andrew Leach")


df2<-ter1_vols%>%group_by(year,month_fac,he) %>% summarise(mw=mean(vol,na.rm = T))%>%
  mutate(date=ymd(paste(year,month_fac,1,sep="-"))) %>% filter(he!="02*")

ggplot(df2) +
  geom_line(aes(as.numeric(he),mw,group=interaction(year,month_fac),colour=factor(year)),size=.75) +
  scale_color_manual("",values=c(colors_tableau10(),colors_tableau10_light()))+
  facet_wrap(~month_fac)+
  blake_theme()+theme(legend.position = "bottom")+
  guides(color=guide_legend(nrow=1))+
  scale_x_continuous(breaks=pretty_breaks(),limits=c(1,24),expand=c(0,0))+
  labs(y="Average Hourly Output (MWh)",x="Hour",
       title="TER 1 Average Output",
       caption="Source: AESO Data, Graph by Andrew Leach")



write_excel_csv(ter1_vols,"ter1.csv")

#align variable names

gen_data<-gen_data %>% select(time=Time,vol=gen,ID,AESO_Name,Plant_Type,Plant_Fuel) %>%
filter(Plant_Type %in% c("COAL","COGEN","NGCC","WIND","SCGT","TRADE","HYDRO","SOLAR","OTHER"))

#take out pre-2010 data and the November time change hours that don't align AESO to NRGStream
gen_data<-gen_data %>% filter(year(time)>=2010) %>% left_join(forecast_data) %>% filter(!is.na(date))


gen_data<-gen_data %>% filter(date(time)<ymd("2020-06-14")) # take out any newer data than what was in the paper


#take out extra info from AESO forecasts
gen_data<-gen_data %>% select(-c("forecast_pool_price","day_ahead_forecasted_ail",        
                              "forecasted_actual_ail_difference","start_date"))

#strip the AB-WECC tie since it's duplicate of AB-MT and AB-BC
gen_data<-gen_data %>% filter(!ID %in% c("AB_WECC","AB_WECC_Exp","AB_WECC_Imp"))

#gen_data<-gen_data %>% rename(actual_posted_pool_price=Price)
gen_data<-gen_data %>% mutate(Plant_Type=as.character(Plant_Type))
gen_data<-gen_data %>% mutate(Plant_Fuel=as.character(Plant_Fuel))
gen_data<-gen_data %>% mutate(hour=hour(time))





#direct label graph with AESO data

df2 <- gen_data %>% filter(date>Sys.Date()-years(10))%>%
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

covid_mid<-ymd("2020-03-11")+days(round((ymd("2020-07-01")-ymd("2020-03-11"))/2))
covid_mid_lag<-ymd("2020-03-11")+days(round((ymd("2020-07-01")-ymd("2020-03-11"))/2))-years(1)


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
  ggsave(file="gen_plain.png", width = 16, height = 9)
  
  
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
  ggsave("aeso_trade_flows.png",width=16,height = 6)
  
  
  

  
  
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
      geom_line(size=1.5,color="#4477AA")+
      geom_point(aes(date_label,estimate,shape=stat_sig),size=1.25,color="#4477AA")+
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
    ggsave("covid_prelim_supply.png",width=16,height = 6)
  

    
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
    ggsave("covid_prelim_fuel.png",width=16,height = 9)
    
    
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


#merit_graph: not used

merits<-read_csv("merit_2019.csv") %>% mutate(date=ymd(date),week=week(date)) %>% filter(week(date)==8,wday(date)==3,he==18)%>%
  filter(Plant_Type!="IMPORT") %>% arrange(price) %>%  mutate(merit=cumsum(size))


set_png("AB_merit.png")
ggplot(arrange(merits,merit,Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price,fill=Plant_Type))+
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price,colour=Plant_Type),fill=NA)+
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
  #geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
  #geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
  geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3)])+  
  scale_colour_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3)])+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = pretty_breaks())+
  scale_y_continuous(expand=c(0,0),limits=c(-11,1001),breaks=seq(0,1000,200))+
  ajl_line()+theme(plot.subtitle = element_text(size = 14),
                   plot.margin = margin(1, 1, 1, 1, "cm"))+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Alberta Energy Merit Order by Plant Type, ",max(merits$date)," ",max(as.character(merits$he)),":00",sep = "")
       #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       #caption="Source: AESO Data."
       )

dev.off()



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
ggsave("os_flows.png",width=16,height = 6)



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
ggsave("covid_os_fuel.png",width=16,height = 6)

