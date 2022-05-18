source("power_paper_base.R")
source("aeso_scrapes.R")

library(directlabels)
library(cowplot)



load(file="data/metered_vols_data.Rdata")
#load(file="nrgstream/nrgstream_gen.Rdata") 

#update_forecasts()
load(file="data/forecast_data.Rdata") 
forecast_data <- forecast_data %>% filter (he!="02*")




#sample period prices and loads

peak_data<-forecast_data %>%filter(!is.na(actual_posted_pool_price),!is.na(actual_ail))%>% group_by(year,month) %>% summarize(ail=mean(actual_ail,na.rm = T),peak_ail=max(actual_ail),trough_ail=min(actual_ail),
                                                                                                                              mean_peak_price=sum(actual_posted_pool_price*actual_ail*(on_peak==TRUE),na.rm = T)/sum(actual_ail*(on_peak==TRUE),na.rm = T),
                                                                                                                              mean_off_peak_price=sum(actual_posted_pool_price*actual_ail*(on_peak==FALSE),na.rm = T)/sum(actual_ail*(on_peak==FALSE),na.rm = T),
                                                                                                                              mean_peak_ail=sum(actual_ail*(on_peak==TRUE),na.rm = T)/sum((on_peak==TRUE),na.rm = T),
                                                                                                                              mean_off_peak_ail=sum(actual_ail*(on_peak==FALSE),na.rm = T)/sum((on_peak==FALSE),na.rm = T),
                                                                                                                              mean_price=sum(actual_posted_pool_price*actual_ail,na.rm = T)/sum(actual_ail,na.rm = T),peak_price=max(actual_posted_pool_price),trough_price=min(actual_posted_pool_price)
)%>%
  mutate(date=ymd(paste(year,month,1,sep="-")))  

max_price=max(peak_data$peak_price)
# prices

top_panel<-ggplot(peak_data) +
  geom_line(aes(date,mean_peak_price,colour="Monthly Mean Price",linetype="A"),size=1.25)+
  geom_line(aes(date,mean_off_peak_price,colour="Monthly Mean Price",linetype="B"),size=1.25)+
  geom_ribbon(aes(date,ymax=peak_price,ymin=trough_price,fill="Range of Monthly Prices"),alpha=.5)+
  #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
  scale_color_manual("",values = c("black"),labels=c("Average Monthly Price"))+
  scale_fill_manual("",values = c("grey50"))+
  scale_linetype_manual("",values = c("solid","21"),labels=c("Peak period average","Off-peak period average"))+
  scale_x_date(expand=c(0,0),breaks="2 year",labels = date_format("%b\n%Y",tz="America/Denver"))+
  scale_y_continuous(limits = c(0,max_price),expand=c(0,0))+
  paper_theme()+
  guides(linetype = guide_legend(override.aes = list(color = c("dodgerblue","dodgerblue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Monthly Pool Prices ($/MWh)",x="",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
  NULL)
bottom_panel<-ggplot(peak_data) +
  #geom_line(aes(date,ail,colour="basic"),size=1.5)+
  geom_line(aes(date,mean_peak_ail,colour="Monthly Mean Price",linetype="A"),size=1.25)+
  geom_line(aes(date,mean_off_peak_ail,colour="Monthly Mean Price",linetype="B"),size=1.25)+
  geom_ribbon(aes(date,ymax=peak_ail,ymin=trough_ail,fill="Range of Monthly Values"),alpha=.5)+
  scale_color_manual("",values = c("black"),labels=c("Average Monthly Price"))+
  scale_fill_manual("",values = c("grey50"))+
  scale_linetype_manual("",values = c("solid","21"),labels=c("Peak hours average","Off-peak hours average"))+
  
  scale_x_date(expand=c(0,0),breaks="2 year",labels = date_format("%b\n%Y",tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  expand_limits(y=12000)+
  guides(linetype = guide_legend(override.aes = list(color = c("black","black"))),color="none")+
  paper_theme()+
  theme(legend.position="bottom",
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.key.width = unit(1.2,"cm"),
        #legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="Alberta Internal Load (MW)",x="")
#caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")



p_grid<-plot_grid(
  top_panel+
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      NULL
    )+
    NULL,
  bottom_panel+ 
    NULL,
  align = TRUE,axis="b", ncol = 1, rel_heights = c(1,.75)
)
p_grid
ggsave("images/prices_and_loads.png",width = 15,height=12)


#direct label graph with AESO data

#align variable names
df2 <- nrgstream_gen %>% filter(date<floor_date(max(date),"month"))%>% #trim to last full month of data
  select(time=Time,vol=gen,ID,AESO_Name,Plant_Type,Plant_Fuel,Capacity) %>%
  filter(Plant_Type %in% c("COAL","COGEN","NGCC","WIND","SCGT","TRADE","HYDRO","SOLAR","OTHER"))%>%
  #filter(year(time)>=2010) %>% 
  left_join(forecast_data) %>% filter(!is.na(date))%>%
  #strip the AB-WECC tie since it's duplicate of AB-MT and AB-BC
  filter(!ID %in% c("AB_WECC","AB_WECC_Exp","AB_WECC_Imp"))%>%
  #mutate(Plant_Type=as.character(Plant_Type))%>%
  #mutate(Plant_Fuel=as.character(Plant_Fuel))%>%
  mutate(hour=hour(time))%>%
  select(-c("forecast_pool_price","day_ahead_forecasted_ail",        
                "forecasted_actual_ail_difference","start_date"))%>%
  #filter(date>Sys.Date()-years(10))%>%
  mutate(month=month(time),year=year(time))%>%   
  mutate(Plant_Type=as_factor(Plant_Type),
         # Relevel to the end
         Plant_Type=fct_other(Plant_Type,keep = c("COAL","COGEN","SCGT","NGCC","WIND","HYDRO","TRADE"),other_level = "OTHER"),
         Plant_Type=fct_relevel(Plant_Type, "OTHER", after = Inf),
         Plant_Type=fct_recode(Plant_Type, "NET IMPORTS"="TRADE"),
         Plant_Type=fct_relevel(Plant_Type, "NET IMPORTS", after = Inf),
         NULL
  )%>%
  #summarize by hour to get total gen and revenue by fuel, 
  group_by(year,month,date,hour,Plant_Type) %>% summarise(capacity=sum(Capacity,na.rm = T),gen=sum(vol,na.rm = T),rev=gen*actual_posted_pool_price) %>% 
  #summarize by year and month to get mean gen and capture price by fuel, 
  group_by(year,month,Plant_Type) %>% summarise(gen=mean(gen,na.rm = T),Capacity=mean(capacity,na.rm = T),capture = sum(rev)/sum(gen))%>% 
  ungroup() %>%
  mutate(date=ymd(paste(year,month,15,sep="-")))
  
  
  AB_palette<- c("black","black","grey50","grey50")
  
    
  gen_fuel <- df2 %>% mutate(Plant_Type=factor(Plant_Type,levels=AB_plant_order))%>%
    #filter(date<ymd("2020-09-01"))%>%
    mutate(Plant_Type=fct_collapse(Plant_Type,
     "RENEWABLES"=c("WIND","OTHER","HYDRO"),
     "NATURAL GAS"=c("SCGT","COGEN","NGCC"),
     "NET IMPORTS"="TRADE"
     ))%>% 
    group_by(date,month,year,Plant_Type) %>% summarise(gen=sum(gen,na.rm = T),
                                                      )%>%
    ungroup() %>%
    group_by(Plant_Type) %>%
    mutate(gen12m=zoo::rollmean(gen,12,fill=NA))%>%
    #filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,gen, col = Plant_Type,lty=Plant_Type)) +
    geom_line(size=1.25)+
    #geom_line(aes(y=gen12m),size=1.25)+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_color_manual("",values= AB_palette)+
    #scale_fill_manual("",values= AB_palette)+
    scale_linetype_manual("",values= c("solid","1131","11","solid"))+
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    paper_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "24 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2004-01-01", "2022-1-30")))+
    expand_limits(y =-500)+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(3.7,"line"))+
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
  ggsave(file="images/gen_fuel.png", width = 14, height=8,dpi = 300)

  
  #carve out REP projects
  
  REP_projects<-c("RIV1","CRR2","WHT1","WRW1")
                             
  gen_rep <- nrgstream_gen %>% filter(date<floor_date(max(date),"month"))%>% #trim to last full month of data
    select(time=Time,vol=gen,ID,AESO_Name,Plant_Type,Plant_Fuel,Capacity) %>%
    filter(Plant_Type %in% c("COAL","COGEN","NGCC","WIND","SCGT","TRADE","HYDRO","SOLAR","OTHER"))%>%
    #filter(year(time)>=2010) %>% 
    left_join(forecast_data) %>% filter(!is.na(date))%>%
    #strip the AB-WECC tie since it's duplicate of AB-MT and AB-BC
    filter(!ID %in% c("AB_WECC","AB_WECC_Exp","AB_WECC_Imp"))%>%
    #mutate(Plant_Type=as.character(Plant_Type))%>%
    #mutate(Plant_Fuel=as.character(Plant_Fuel))%>%
    mutate(hour=hour(time))%>%
    select(-c("forecast_pool_price","day_ahead_forecasted_ail",        
              "forecasted_actual_ail_difference","start_date"))%>%
    #filter(date>Sys.Date()-years(10))%>%
    mutate(month=month(time),year=year(time))%>%   
    mutate(Plant_Type=case_when(
      ID == "RIV1" ~ "REP_WIND",
      ID == "CRR2" ~ "REP_WIND",
      ID == "WHT1" ~ "REP_WIND",
      ID == "WRW1" ~ "REP_WIND",
      TRUE ~ Plant_Type
    ),
    Plant_Type=as_factor(Plant_Type),
           # Relevel to the end
           Plant_Type=fct_other(Plant_Type,keep = c("COAL","COGEN","SCGT","NGCC","WIND","REP_WIND","HYDRO","TRADE","SOLAR"),other_level = "OTHER"),
           Plant_Type=fct_other(Plant_Type,drop = c("COGEN","SCGT","NGCC"),other_level = "NATURAL GAS"),
           Plant_Type=fct_other(Plant_Type,drop = c("TRADE"),other_level = "OTHER"),
           Plant_Type=fct_relevel(Plant_Type, "OTHER", after = Inf),
           Plant_Type=fct_recode(Plant_Type,"OTHER (INCL TRADE)"= "OTHER"),
           NULL
    )%>%
    #summarize by hour to get total gen and revenue by fuel, 
    group_by(year,month,date,hour,Plant_Type) %>% summarise(capacity=sum(Capacity,na.rm = T),gen=sum(vol,na.rm = T),rev=gen*actual_posted_pool_price) %>% 
    #summarize by year and month to get mean gen and capture price by fuel, 
    group_by(year,month,Plant_Type) %>% summarise(gen=mean(gen,na.rm = T),Capacity=mean(capacity,na.rm = T),capture = sum(rev)/sum(gen))%>% 
    ungroup() %>%
    mutate(date=ymd(paste(year,month,15,sep="-")))
  
    gen_rep_graph<-gen_rep%>% filter(year>=2010)%>%
      filter(Plant_Type%in% c("WIND","REP_WIND"))%>%
      mutate(Plant_Type=fct_recode(Plant_Type,"Wind excl. REP Projects"= "WIND"))%>%
      mutate(Plant_Type=fct_recode(Plant_Type,"Total wind incl. REP Projects"= "REP_WIND"))%>%
      mutate(Plant_Type=fct_relevel(Plant_Type,"Wind excl. REP Projects",after =  Inf))%>%
      group_by(Plant_Type) %>%
      mutate(gen6m=zoo::rollmean(gen,6,fill=NA))%>%
      ggplot(aes(date,gen, col = Plant_Type,lty=Plant_Type,fill=Plant_Type,group=Plant_Type)) +
    #geom_line(size=1.25,position = "stack")+
    #geom_line(size=1.25,position = "identity")+
    geom_line(aes(y=gen6m),size=1.25,position = "stack")+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    #scale_color_manual("",values= AB_palette)+
    
    #scale_fill_manual("",values= AB_palette)+
    scale_linetype_manual("",values= c("11","solid"),guide = guide_legend(reverse = TRUE))+
    scale_color_manual("",values=grey.colors(9,start=0,end=.8),guide = guide_legend(reverse = TRUE))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
     paper_theme()+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "24 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2010-01-01", "2022-1-30")))+
    expand_limits(y =1000)+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    theme(legend.position = "bottom",
          legend.key.width = unit(3.7,"line"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
      
    labs(x="",y="6 Month Moving Average Generation (MW)",
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
  gen_rep_graph
  ggsave(file="images/gen_rep.png", width = 14,height=9,dpi = 600)

#price_capture
    
    trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")
    
    
    
    df1 <- nrgstream_gen %>%rename(time=Time)%>% filter(year(time) >= 2010,! NRG_Stream %in% trade_excl)%>% 
      group_by(Plant_Type,time) %>% 
      summarise(total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),p_mean=mean(Price)) %>%
      ungroup()
    
    
    df1$Day <- date(df1$time)
    df1$Year <- as.factor(year(df1$time))
    #df1<-na.omit(df1)
    #df1$Revenue <- df1$total_gen*df1$p_mean
    
    gen_set<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND")
    
    
    #test_samp<-nrgstream_gen %>% filter(year(time)==2019)
    #test_samp2<-test_samp %>% filter(Plant_Type %in% gen_set,! NRG_Stream %in% trade_excl)
    
    
    df2 <- df1 %>% filter(Plant_Type %in% gen_set,year(time)<=2022) %>%
      filter(total_gen!=0)%>%
      group_by(Plant_Type,Year) %>%
      mutate(avg_rev=total_rev/total_gen)%>%
      summarise(capture = sum(total_rev)/sum(total_gen),
                q50=quantile(avg_rev, probs=c(.5)),
                q75=quantile(avg_rev, probs=c(.75)),
                q25=quantile(avg_rev, probs=c(.25)),
                q05=quantile(avg_rev, probs=c(.05)),
                q95=quantile(avg_rev, probs=c(.95)),
                )
    
    
    #make vol-weighted avg
    
    
    
    
    df2$Plant_Type<-fct_relevel(df2$Plant_Type, "OTHER",after=Inf)
    df2$Plant_Type<-fct_relevel(df2$Plant_Type, "HYDRO",after=Inf)
    df2$Plant_Type<-fct_relevel(df2$Plant_Type, "WIND",after=Inf)
    df2$Plant_Type<-fct_relevel(df2$Plant_Type, "SOLAR",after=Inf)
    df2$Plant_Type<-fct_relevel(df2$Plant_Type, "TRADE",after=Inf)
    df2$Plant_Type<-fct_relevel(df2$Plant_Type, "IMPORT",after=Inf)
    df2$Plant_Type<-fct_relevel(df2$Plant_Type, "EXPORT",after=Inf)
    
    #make market prices
    df3 <- df1 %>% filter(Plant_Type %in% gen_set,year(time)<2023,
                          !is.na(p_mean)) %>%
      group_by(Year) %>% 
      summarise(capture = sum(total_rev)/sum(total_gen),
                q50=quantile(p_mean, probs=c(.5)),
                q75=quantile(p_mean, probs=c(.75)),
                q25=quantile(p_mean, probs=c(.25)),
                q05=quantile(p_mean, probs=c(.05)),
                q95=quantile(p_mean, probs=c(.95)),
                )%>%
      mutate(Plant_Type="MARKET",Plant_Type=as_factor(Plant_Type))
    
    df2<-df2 %>% bind_rows(df3)
    df2$Plant_Type<-fct_relevel(df2$Plant_Type, "MARKET",after=0)
    
    df4<-tibble(Year=seq(2010,2016),Plant_Type="SOLAR",capture=0,
                q50=0,q05=0,q95=0,q75=0,q25=0)%>%
      mutate(Year=as_factor(Year),
             Plant_Type=as_factor(Plant_Type))
    df2<-df2 %>% bind_rows(df4)
    
    
    my_palette<-c("black",grey.colors(2,start=0.3,end = .8))
    df2 %>% filter(Plant_Type %in% c("MARKET","WIND","SOLAR"))%>%
      mutate(Year=fct_recode(Year,"2022\n(YTD)"="2022"))%>%
      ggplot(aes(Year,capture,fill=Plant_Type))+
      geom_col(aes(Year,capture,fill=Plant_Type),position = position_dodge(width = .9),width = .6,color="black",size=.5)+
      #geom_col(aes(Year,p_mean),position = "identity",fill=NA,color="black")+
      #geom_line(dataaes(Year,capture,fill=Plant_Type),position = position_dodge(width = .9),width = .6,color="black",size=.5)+
      #geom_text(aes(y=-10,label=Plant_Type),angle=90,size=2)+
      #  scale_color_viridis("Plant Type",discrete=TRUE)+
      #  scale_fill_viridis("Plant Type",discrete=TRUE)+
      #scale_color_manual("",values=colors_tableau10())+
      #scale_fill_manual("",values=colors_tableau10())+
      scale_color_manual("",values=my_palette)+
      scale_fill_manual("",values=my_palette)+
      scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
      expand_limits(y=150)+
      paper_theme()+theme(legend.position = "bottom")+
      guides(fill=guide_legend(nrow = 1,label.position = "bottom",keywidth = 5))+
      labs(x="",y="Volume-Weighted Average Capture Price ($/MWh)",
           #title="Energy Price Capture ($/MWh, 2010-2021)",
           #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
      )
    ggsave("images/price_capture_renew.png",width=14,height=7,dpi=300)


    my_palette<-c("black",grey.colors(2,start=0.4,end = .7))
    df2 %>% filter(Plant_Type %in% c("MARKET","WIND","SOLAR"))%>%
      mutate(Year=fct_recode(Year,"2022*"="2022"),
             Plant_Type=fct_recode(Plant_Type,"Market"="MARKET","Wind"="WIND","Solar"="SOLAR"
                                    ))%>%
        ggplot(aes(Year,capture,group=Plant_Type,color=Plant_Type))+
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
    df5 <- nrgstream_gen %>%rename(time=Time)%>% 
      filter(year(time) >= 2010,Plant_Type=="WIND",year(time)<=2022)%>%
      filter(gen!=0)%>%
      group_by(ID,time) %>% 
      summarise(#in_service=min(time),
                total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),p_mean=mean(Price)) %>%
      ungroup()%>%
      mutate(Year = as.factor(year(time)))%>%
      rename(Plant_Type=ID)%>%
      group_by(Plant_Type,Year) %>%
      mutate(avg_rev=total_rev/total_gen)%>%
      summarise(capture = sum(total_rev)/sum(total_gen),
                q50=quantile(avg_rev, probs=c(.5)),
                q75=quantile(avg_rev, probs=c(.75)),
                q25=quantile(avg_rev, probs=c(.25)),
                q05=quantile(avg_rev, probs=c(.05)),
                q95=quantile(avg_rev, probs=c(.95)),
      )
    df5<-df5 %>% bind_rows(df3) #add the market prices
    
    #create all combos
    combos<- df5 %>% ungroup()%>%expand(Plant_Type,Year)
    
    plant_capture<-combos %>% left_join(df5)
      #mutate(capture=replace_na(capture,0))

    #REP_projects<-c("RIV1","CRR2","WHT1","WRW1")
    
    
 #27 plants plus market
    
     plant_cap_graph<-plant_capture %>% 
       filter(Plant_Type!="TAY2")%>%
      group_by(Plant_Type)%>%
       mutate(startup=as.numeric(as.character(Year[min(which(!is.na(capture)))]))
       )%>%
       ungroup()  %>%
      mutate(Year=fct_recode(Year,"2022\n(YTD)"="2022"),
             Plant_Type=fct_recode(Plant_Type,"Market"="MARKET"),
             Plant_Type=fct_recode(Plant_Type,"RIV1*"="RIV1"),
             Plant_Type=fct_recode(Plant_Type,"WRW1*"="WRW1"),
             Plant_Type=fct_recode(Plant_Type,"WHT1*"="WHT1"),
             Plant_Type=fct_recode(Plant_Type,"CRR2*"="CRR2"),
             Plant_Type=fct_reorder(Plant_Type,startup,min),
             Plant_Type=fct_relevel(Plant_Type,"Market"))%>%
      ggplot(aes(Year,capture,group=Plant_Type,fill=Plant_Type))+
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
      filter(Plant_Type%in% c("MARKET",REP_projects),as.numeric(as.character(Year))>=2019)%>%
      group_by(Plant_Type)%>%
      mutate(startup=as.numeric(as.character(Year[min(which(!is.na(capture)))]))
      )%>%
      ungroup()  %>%
      #add environmental attributes
      mutate(env_attr=case_when(
        (Plant_Type %in% REP_projects)& Year==2019 ~30*.37,
        (Plant_Type %in% REP_projects)& Year==2020 ~30*.37,
        (Plant_Type %in% REP_projects)& Year==2021 ~40*.37,
        (Plant_Type %in% REP_projects)& Year==2022 ~50*.37,
        TRUE~0
      ))%>%
      #reset factor levels
      mutate(Year=fct_recode(Year,"2022 (YTD)"="2022"),
             Plant_Type=fct_relevel(Plant_Type,"WHT1","CRR2","RIV1","WRW1"),
             Plant_Type=fct_relevel(Plant_Type,"MARKET"),
             Plant_Type=fct_recode(Plant_Type,"Market Generation-Weighted Average"="MARKET"),
             Plant_Type=fct_recode(Plant_Type,"Whitla 1 (WHT1)"="WHT1","Castle Rock Ridge 2 (CRR2)"="CRR2","Riverview Wind (RIV1)"="RIV1","Windrise Wind (WRW1)"="WRW1"),
             #Plant_Type=fct_reorder(Plant_Type,startup,min),
             )%>%
      ggplot(aes(Year,capture,group=Plant_Type))+
      #geom_errorbar(aes(ymin = q05, ymax = q95), size=.75,width = .25,position=position_dodge(width=0.8))+
      #geom_point(aes(shape="Mean"),size=2.25,position=position_dodge(width=0.9))+
      geom_col(aes(fill=Plant_Type),size=.25,position=position_dodge(width=0.9),color="black")+
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
      filter(Plant_Type%in% c("MARKET",REP_projects),as.numeric(as.character(Year))>=2019)%>%
      group_by(Plant_Type)%>%
      mutate(startup=as.numeric(as.character(Year[min(which(!is.na(capture)))]))
      )%>%
      ungroup()  %>%
      #add environmental attributes
      mutate(env_attr=case_when(
        (Plant_Type %in% REP_projects)& Year==2019 ~30*.37,
        (Plant_Type %in% REP_projects)& Year==2020 ~30*.37,
        (Plant_Type %in% REP_projects)& Year==2021 ~40*.37,
        (Plant_Type %in% REP_projects)& Year==2022 ~50*.37,
        TRUE~0
      ))%>%
      #reset factor levels
      mutate(Year=fct_recode(Year,"2022 (YTD)"="2022"),
             Plant_Type=fct_relevel(Plant_Type,"WHT1","CRR2","RIV1","WRW1"),
             Plant_Type=fct_relevel(Plant_Type,"MARKET"),
             Plant_Type=fct_recode(Plant_Type,"Market Generation-Weighted Average"="MARKET"),
             Plant_Type=fct_recode(Plant_Type,"Whitla 1 (WHT1)"="WHT1","Castle Rock Ridge 2 (CRR2)"="CRR2","Riverview Wind (RIV1)"="RIV1","Windrise Wind (WRW1)"="WRW1"),
             #Plant_Type=fct_reorder(Plant_Type,startup,min),
      )%>%
      ggplot()+
      geom_col(aes(Plant_Type,capture,fill=Plant_Type),size=.25,position=position_dodge(width=0.9),color="black")+
      geom_col(aes(Plant_Type,y=capture+env_attr,color="Deemed environmental attribute value (TIER output-based allocation of emissions credits (0.37t/MWh) at annual carbon prices)"),fill=NA,alpha=0.5,size=.5,position=position_dodge(width=0.9))+
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
    
    
    
    
    
    

    
  #price_capture plant level table
    
    #REP_projects<-c("RIV1","CRR2","WHT1","WRW1")
    
    
    #trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")
    rep_plant_capture <- nrgstream_gen %>%rename(time=Time)%>% 
      filter(year(time) >= 2010,! NRG_Stream %in% trade_excl)%>% 
      filter(ID %in% REP_projects)%>%
      group_by(ID,time) %>% 
      summarise(total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),avg_rev=sum(total_rev/total_gen,na.rm = T),p_mean=mean(Price)) %>%
      ungroup()%>%
      filter(total_rev>0)
  
    
    market_capture=nrgstream_gen %>%rename(time=Time)%>%
      filter(year(time) >= 2010,
            Plant_Type %in% c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","WIND")
      )%>% 
      select(time,Price,ID,gen)%>%
      mutate(ID="AESO",Revenue=Price*gen)%>%
      group_by(ID,time)%>%
      summarise(total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),avg_rev=sum(total_rev/total_gen,na.rm = T),p_mean=mean(Price)) 
  
    renew_capture=nrgstream_gen %>%rename(time=Time)%>%
      filter(year(time) >= 2010,
             Plant_Type %in% c("SOLAR","WIND")
      )%>% 
      select(time,Price,ID,gen,Plant_Type)%>%
      mutate(ID=Plant_Type,Revenue=Price*gen)%>%
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
  
  renew_gen<-renew_vols%>% group_by(date,he,Plant_Type)%>%
    summarize(gen=sum(dispatched_mw))%>%pivot_wider(names_from = Plant_Type, values_from = gen)%>%
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
  left_join(whitla_gen %>% select(time=Time,whitla_gen=gen))%>%
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
            
  