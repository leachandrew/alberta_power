source("power_paper_base.R")
source("aeso_scrapes.R")



#seasons
if(!exists("ajl_hourly", mode="function")) source("power_paper_base.R")

#FORECASTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#seasons
if(!exists("getSeason", mode="function")) source("get_season.R")



#update prices and loads

update<-1
if(update==1)
  update_forecasts()
#re-load
load("data/forecast_data.Rdata")
forecast_data<-assign_date_time_days(forecast_data)
forecast_data<-assign_peaks(forecast_data)


#Update volumes

load(file="data/metered_vols_data.Rdata" ) 
if(update==1){
  all_vols<-all_vols %>% filter(date<=ymd("2021-12-31"))
  #test<-update_vols(all_vols)
  all_vols<-update_vols(all_vols)
  save(all_vols, file="data/metered_vols_data.Rdata" ) 
  }
#Update renewable volumes as used in merit paper

  renew_vols<- all_vols %>% 
  filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% select(date,he,asset_id,dispatched_mw=vol,pool_participant_id,effective_date_time=time)%>%
  #switch so that we have same data format as merit data
  mutate(import_export="",
         block_number=1,
         price=0,
         from=0,
         to=dispatched_mw,
         size=dispatched_mw,
         available_mw=dispatched_mw,
         dispatched="Y",
         flexible="N",
         key_firm=FALSE,
         renew_gen=dispatched_mw
  )
  if(update==1)
    save(renew_vols, file="data/renew_vols.RData")  
  



#calculate peak prices

peak_data<-forecast_data %>% group_by(year,month,on_peak) %>% summarize(pool_price=mean(actual_posted_pool_price,na.rm = T),
                                                                        ail=mean(actual_ail,na.rm = T)) %>%
  mutate(date=ymd(paste(year,month,1,sep="-"))) %>% ungroup()%>% 
  filter(date<max(date))


peaks<-
  ggplot(peak_data) +
  geom_line(aes(date,pool_price,colour=on_peak,linetype=on_peak),size=1.25)+
  #scale_color_viridis("",option="D",discrete = TRUE)+
  #scale_color_manual("",values = colors_ua10(),labels=c("Off-peak Prices","Peak prices"))+
  #scale_color_manual("",values = colors_ua10() ,labels=c("Off-peak Prices","Peak prices"))+
  scale_color_manual("",values = c("grey30","grey70") ,labels=c("Off-peak Prices","Peak prices"))+
  scale_linetype_manual("",values = c("solid","11") ,labels=c("Off-peak Prices","Peak prices"))+
  
  scale_x_date(expand=c(0,0),breaks="12 months",labels = date_format("%b\n%Y"))+
  scale_y_continuous(breaks=pretty_breaks(),expand = c(0,0))+
  expand_limits(y=0,x=max(peak_data$date+months(1)))+
  paper_theme()+
  labs(y="Average Monthly Pool Price ($/MWh)",x="")

  
peaks
ggsave(file=paste("images/peak_prices.png",sep=""),width = 14,dpi=300)


peaks+labs(
title=paste("Alberta Monthly Average Peak and Off-Peak Wholesale Power Prices",sep=""),
subtitle=paste(format(min(peak_data$date),"%B, %Y")," through ",format(max(peak_data$date),"%B, %Y"),sep=""),
caption = "Peak hours are 8am to 11pm other than on statutory holidays or Sundays.\n Data via AESO, graph by Andrew Leach.")
ggsave(file=paste("images/peak_prices_",year(period_start),"_",year(end_date),".png",sep=""),width = 14,dpi=300)


mid<-ymd("2015-05-24")+((ymd("2019-04-30")-ymd("2015-05-24"))/2)
 peaks-
   geom_text(
     aes(x = mid, y = max_price-150, label = "NDP in Power\nin Alberta"),
     size = 4, vjust = 0, hjust = 0.5, color = "black"
   )-
   geom_rect(mapping=aes(xmin=ymd("2015-05-24"),xmax=ymd("2019-04-30"),ymin=0,ymax=max_price,fill="NDP in office"),alpha=.25)+
  scale_fill_manual("",values = "grey90")+
  labs(
  title=paste("Alberta Monthly Average Peak and Off-Peak Wholesale Power Prices",sep=""),
  subtitle=paste(format(min(peak_data$date),"%B, %Y")," through ",format(max(peak_data$date),"%B, %Y"),sep=""),
  caption = "Peak hours are 8am to 11pm other than on statutory holidays or Sundays.\n Data via AESO, graph by Andrew Leach.")
ggsave(file=paste("images/peak_prices_ndp.png",sep=""),width = 14,dpi=300)

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
  guides(linetype = guide_legend(override.aes = list(color = c("dodgerblue","dodgerblue"))),color=FALSE)+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
        )+
  labs(y="Monthly Pool Prices ($/MWh)",x="",
       title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
  )
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
  guides(linetype = guide_legend(override.aes = list(color = c("black","black"))),color=FALSE)+
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

p_grid_blank<-plot_grid(
  top_panel+
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.text.x = element_blank(),
      NULL
    )+
    NULL,
  bottom_panel+ 
    NULL,
  align = TRUE,axis="b", ncol = 1, rel_heights = c(1,.75)
)
p_grid_blank
ggsave("images/prices_and_loads_clean.png",width = 15,height=12)


bottom_panel
ggsave("images/loads_clean.png",width = 14,dpi=300)



hourly_prices<-ggplot(forecast_data%>%filter(!is.na(actual_posted_pool_price),
                              he!="02*")%>%
  group_by(he,year) %>% 
  summarise(AIL=mean(actual_ail,na.rm=T),p_mean=mean(actual_posted_pool_price,na.rm=T))%>%         
  mutate(he=factor(he,levels = unique(he)),year=factor(year))%>%ungroup(),
  aes(he,p_mean,group=year,color=year)) +
  geom_line(size=1.5) +
  geom_line(data=forecast_data%>%filter(year==year(Sys.Date()),!is.na(actual_posted_pool_price),
                         he!="02*")%>%
  group_by(he,year) %>% 
  summarise(AIL=mean(actual_ail,na.rm=T),p_mean=mean(actual_posted_pool_price,na.rm=T))%>%         
  mutate(he=factor(he,levels = unique(he)),year=factor(year))%>%ungroup(),
  aes(he,p_mean),size=2.25)+
  #
  #geom_line(data=forecast_data%>%filter(year==year(Sys.Date()),month(date)%in%seq(3,5,1),!is.na(actual_posted_pool_price),
  #                                      he!="02*")%>%
  #            group_by(he,year) %>% 
  #            summarise(AIL=mean(actual_ail,na.rm=T),p_mean=mean(actual_posted_pool_price,na.rm=T))%>%         
  #            mutate(he=factor(he,levels = unique(he)),year=factor(year))%>%ungroup(),
  #          aes(he,p_mean),size=2.25,color="black",lty="dashed")+
  scale_color_grey("",start=.8,end=0)+
  guides(col = guide_legend(nrow=3))+
  paper_theme()+
  expand_limits(y=0)+
  labs(y="Hourly Average Pool Price ($/MWh)",x="Hour Ending")
hourly_prices
ggsave("images/hourly_prices_clean.png",width = 10,dpi=300)

hourly_prices+
  scale_color_viridis("",discrete=TRUE,option = "D")+
ggsave("images/hourly_prices_col_clean.png",width = 10,dpi=300)

hourly_prices+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  labs(
  title="Hourly Average Energy Prices ($/MWh, 2000-Present)",
  caption="Source: AESO Data, Graph by Andrew Leach")
ggsave("images/hourly_prices_col.png",width = 10,dpi=300)

hourly_prices+labs(
       title="Hourly Average Energy Prices ($/MWh, 2000-Present)",
       caption="Source: AESO Data, Graph by Andrew Leach")
ggsave("images/hourly_prices.png",width = 10,dpi=300)





duck<-ggplot(forecast_data%>%filter(year>=2021,!is.na(actual_posted_pool_price),
                                             he!="02*")%>%
                        group_by(he,year,month) %>% 
                        summarise(AIL=mean(actual_ail,na.rm=T),p_mean=mean(actual_posted_pool_price,na.rm=T))%>%         
                        mutate(he=factor(he,levels = unique(he)))%>%ungroup()%>%
                      mutate(year_mon=format(ymd(paste(year,month,1,sep="-")),"%b, %Y"),
                             year_mon=factor(year_mon,levels=unique(format(forecast_data$date,"%b, %Y")))
                             ),
                      aes(he,p_mean,group=year_mon,color=year_mon)) +
  geom_line(size=1.5) +
  scale_color_grey("",start=.8,end=0)+
  guides(col = guide_legend(nrow=2))+
  paper_theme()+
  expand_limits(y=0)+
  labs(y="Hourly Average Pool Price ($/MWh)",x="Hour Ending")
duck

duck+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  ggsave("images/duck_prices_clean.png",width = 10,dpi=300)




#direct label graph with AESO data

df2 <- all_vols %>% filter(date>Sys.Date()-years(10))%>%
  mutate(month=month(time),year=year(time),
                           Plant_Type = case_when((Plant_Type=="TRADE" & vol<0)  ~ "EXPORT",
                                                  (Plant_Type=="TRADE" & vol>=0)  ~ "IMPORT",
                                                  TRUE~Plant_Type)
                           )%>%   
  filter(Plant_Type!="EXPORT")%>%
  mutate(Plant_Type=as_factor(Plant_Type),
         # Relevel to the end
         Plant_Type=fct_other(Plant_Type,keep = c("COAL","COGEN","SCGT","NGCC","WIND","IMPORT"),other_level = "OTHER"),
         Plant_Type=fct_relevel(Plant_Type, "IMPORT", after = Inf),
         Plant_Type=fct_relevel(Plant_Type, "OTHER", after = Inf)
  )%>%
  #summarize by hour to get total gen and revenue by fuel, 
  group_by(year,month,date,hour,Plant_Type) %>% summarise(gen=sum(vol,na.rm = T),rev=gen*actual_posted_pool_price) %>% 
  #summarize by year and month to get mean gen and capture price by fuel, 
  group_by(year,month,Plant_Type) %>% summarise(gen=mean(gen,na.rm = T),capture = sum(rev)/sum(gen))%>% 
  mutate(policy=ifelse(as.character(year)>="2018","CCIR","SGER"))%>%
  ungroup() %>%
  mutate(date=ymd(paste(year,month,15,sep="-")))
  
  




#test<-df2 %>% filter(Plant_Fuel=="TRADE",gen>=0) %>% ungroup() %>%
#  #mutate(Plant_Fuel=as_factor(Plant_Fuel),
#  #       # Relevel to the end
  #       Plant_Fuel=fct_relevel(Plant_Fuel, "TRADE", after = Inf)) %>%
#  ggplot(aes(date,gen)) +
#  geom_area()+
#  geom_point()
  
covid_mid<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)
covid_mid_lag<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)-years(1)


gen_plain <- 
  df2 %>% 
  filter(date>ymd("2014-12-31"))%>%
           ggplot(aes(date,gen, col = Plant_Type,fill = Plant_Type)) +
  #geom_area(position = "stack")+
  geom_line()+
  #annotate("text", x = as.Date("2014-8-1")-days(20), y = 5600, label = "SGER in place at\n12% and $15/tonne\nsince July 2007",size=3.8)+  
  #annotate("text", x = as.Date("2016-1-1")-days(20), y = 5600, label = "SGER changed to\n15% and $20/tonne\nJan 2016",size=3.8)+  
  #annotate("text", x = as.Date("2017-1-1")-days(20), y = 5600, label = "SGER changed to\n20% and $30/tonne\nJan 2017",size=3.8)+  
  #annotate("text", x = as.Date("2018-1-1")-days(20), y = 5600, label = "SGER changed to\nCCIR at $30/tonne\nJan 2018",size=3.8)+  
  
  slide_theme()+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "6 months",expand=c(0,0) )+
  expand_limits(x = as.Date(c("2015-01-01", "2020-07-01")))+
  scale_y_continuous(limits=c(0,6001),expand = c(0,0),breaks=pretty_breaks())+
  scale_color_manual("",values=colors_tableau10())+
  theme(legend.position = "none")+
  labs(x="Year",y="Average Hourly Generation (MW)",
       #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
       title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
       caption="Source: AESO Data, accessed via NRGStream")+
  annotate("text", x = covid_mid, y =5000, label = "COVID\nperiod",size=3.25,hjust=0.5)+
  annotate("rect", fill = "grey80", alpha = .3, 
           xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
           ymin = 0, ymax = 5999)+
  annotate("rect", fill = "grey80", alpha = .3, 
           xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
           ymin = 0, ymax = 5999)+
  annotate("text", x = covid_mid_lag, y =5000, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5)  

  gen_plain

p<-gen_plain+
  annotate("text", x = as.Date("2014-10-1")-days(20), y =5600, label = "SGER in place. OBAs at 88% of\nhistoric emissions intensity and\n$15/tonne carbon price",size=2.5)+  
  annotate("text", x = as.Date("2016-1-1")-days(20), y = 5600, label = "SGER changed to 85% OBA\nand $20/tonne carbon price",size=2.5)+  
  annotate("text", x = as.Date("2017-1-1")-days(20), y = 5600, label = "SGER changed to 80% OBA\nand $30/tonne carbon price",size=2.5)+  
  annotate("text", x = as.Date("2018-1-1")-days(20), y = 5600, label = "SGER changed to CCIR\nwith fixed 0.37t/MWh OBA\nand $30/tonne carbon price",size=2.5)+  
  
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2015-12-29"), xmax =as.Date("2016-1-3"),
           ymin = 0, ymax = 5200) +
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2016-12-29"), xmax =as.Date("2017-1-3"),
           ymin = 0, ymax = 5200) +
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2017-12-29"), xmax =as.Date("2018-1-3"),
           ymin = 0, ymax = 5200) 
p


direct_labels <- df2 %>% 
  group_by(Plant_Type) %>%
  summarize(
    x = last(date), 
    y = last(gen)
  )
direct_labels <-direct_labels%>%
  mutate(y=case_when(
    Plant_Type=="COGEN" ~ y+0,
    Plant_Type=="OTHER" ~ y-50,
    Plant_Type=="IMPORT" ~ y-75,
    Plant_Type=="WIND" ~ y+75,
    Plant_Type=="NGCC" ~ y+50,
    TRUE~y),
    labels=case_when(
      Plant_Type=="SCGT" ~ "NGSC",
      Plant_Type=="COGEN" ~ "CHP",
      TRUE~as.character(Plant_Type)),
    #labels=str_to_title(labels)
      )


direct_labels_axis <- axis_canvas(p, axis = "y") +
  geom_text(
    data = direct_labels, 
    aes(y = y, label = labels), 
    x = 0.06, 
    hjust = 0, 
    size = 5, 
    col = colors_tableau10()[1:7]
  )


plain_direct_labels <- insert_yaxis_grob(gen_plain, direct_labels_axis)
ggdraw(plain_direct_labels)


set_png(file="gen_plain.png", width = 1600, height = 1000)
ggdraw(plain_direct_labels)
dev.off()


p_direct_labels <- insert_yaxis_grob(p, direct_labels_axis)


set_png(file="gen_ghg_price.png", width = 1600, height = 1000)
ggdraw(p_direct_labels)
dev.off()


set_png(file="gen_old.png", width = 1600, height = 1000)
ggdraw(p_direct_labels)
dev.off()





#oil sands net get
download.file("https://www.aeso.ca/assets/Uploads/Monthly-Oilsands-Net-Generation-and-Load-2014-2019.xlsx",destfile = "aeso_os_net.xlsx",mode="wb")
os_net<-read_excel("aeso_os_net.xlsx")%>% clean_names()




top_panel<-
  ggplot(os_net) +
  geom_line(aes(month,load_g_wh,color="Oil sands load"),size=1.7)+
  geom_line(aes(month,generation_g_wh,color="Oil sands generation"),size=1.7)+
  scale_colour_manual(NULL,values=c("#0D3692","#FEBA35"))+
  scale_x_datetime(date_breaks = "6 months", date_labels =  "%b\n%Y") +
  scale_y_continuous(expand = c(0, 0),breaks = pretty_breaks(),limits = c(0,800)) +
  guides(colour=guide_legend(nrow=1),fill=guide_legend(nrow=1))+
  labs(y="Monthly Load or Generation (GWh)",x="Date",
       title="Oil sands load and generation",
       caption="Data via AESO")+
  ajl_line()
  NULL
  
bottom_panel<-
  ggplot(os_net) +
  geom_area(aes(month,generation_g_wh-load_g_wh,fill="Oil sands net generation",color="Oil sands net generation"),size=1.7)+
  scale_colour_manual(NULL,values=c("#0D3692","#FEBA35"))+
  scale_fill_manual(NULL,values=c("#0D3692","#FEBA35"))+
  scale_x_datetime(date_breaks = "6 months", date_labels =  "%b\n%Y") +
  scale_y_continuous(expand = c(0, 0),breaks = pretty_breaks(),limits = c(0,800)) +
  guides(colour=guide_legend(nrow=1),fill=guide_legend(nrow=1))+
  labs(y="Monthly Net Generation (GWh)",x="Date",
       title="",
       caption="Data via AESO")+
  ajl_line()
  NULL

#ifelse(grepl("can",spread_name),
#       bottom_panel<-bottom_panel+labs(caption="Assumed NGL fractions are 26% ethane, 13% propane, 6% butane and 55% condensate. Ethane value approximated at 66% of Edmonton propane. Spread is relative to AECO/NIT gas."),
#       bottom_panel<-bottom_panel+labs(caption="Assumed NGL fractions are 26% ethane, 13% propane, 6% butane and 55% condensate. Spread is relative to Henry Hub gas."))
#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-arrangeGrob(g_legend(top_panel),g_legend(bottom_panel), nrow=2)

#grid_arrange_shared_legend(top_panel,gridExtra::arrangeGrob(bottom_panel, ncol=1), ncol=1, nrow=2)

set_png(file="os_net.png")
grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                           plot.caption = element_blank(),
                                           plot.title = element_blank(),
                                           plot.subtitle = element_text(size = 14, face = "italic"),
                                           panel.grid = element_blank(),
                                           text = element_text(size = 10,face = "bold"),
                                           axis.text = element_text(size = 10,face = "bold", colour="black"),
                                           axis.text.x = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.ticks.x =  element_blank()
),
bottom_panel +
  theme(legend.position="none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        plot.caption = element_text(size = 5, face = "italic"),
        panel.grid = element_blank(),
        text = element_text(size = 10,face = "bold"),
        axis.text = element_text(size = 10,face = "bold", colour="black"),
        axis.title.x = element_blank()),
ncol=1,heights=c(5,5)),
mylegend, 
nrow=2,heights=c(10, 2),
#bottom =text_grob(
#  "Source: Data via Bloomberg, graph by Andrew Leach",
#  face = "italic", color = "black",size=8,just="center",lineheight = 1
#),
top =text_grob(
  "Oil sands monthly load, generation and net generation",
  face = "bold", color = "black",size=14,just="center",lineheight = 1
)

)
dev.off()  





