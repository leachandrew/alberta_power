#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())

library(grid)
library(gridExtra)
library(ggpubr)
library(gganimate)
library(timeDate)


#seasons
if(!exists("ajl_hourly", mode="function")) source("../andrew_base.R")

#FORECASTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#seasons
if(!exists("getSeason", mode="function")) source("get_season.R")



#METERED VOLUMES

update_forecasts()
#re-load
load("forecast_data.Rdata")

load(file="metered_vols_data.Rdata" ) 
all_vols<-update_vols(all_vols)

save(all_vols, file="metered_vols_data.Rdata" ) 




renew_vols<-all_vols %>% filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% select(date,he,asset_id,vol)%>%
  rename("renew_gen"="vol")
save(renew_vols, file="renew_vols.Rdata" ) 






latest_vols<-all_vols %>% filter(year(date)>=2018)
brooks_vols<-latest_vols%>% filter(asset_id=="BSC1")
brooks_vols$time<-as.POSIXct(paste(brooks_vols$date," ",as.numeric(brooks_vols$hour),":00",sep=""),format="%Y-%m-%d %H:%M")


#FORECASTS



#forecast_data$time<-as.POSIXct(paste(forecast_data$date," ",as.numeric(forecast_data$he),":00",sep=""),format="%Y-%m-%d %H:%M")
filename<-paste("forecast_data",".RData",sep = "")
update_forecasts()
load(filename)
forecast_data<-assign_date_time_days(forecast_data)
forecast_data<-assign_peaks(forecast_data)



#calculate peak prices

peak_data<-forecast_data %>% group_by(year,month,on_peak) %>% summarize(pool_price=mean(actual_posted_pool_price,na.rm = T),
                                                                        ail=mean(actual_ail,na.rm = T)) %>%
  mutate(date=ymd(paste(year,month,1,sep="-"))) 

max_price<-max(max(peak_data$pool_price))
period_start<-min(peak_data$date)
end_date<-max(peak_data$date)

png<-1
if(png==1)
  set_png(file=paste("peak_prices_",year(period_start),"_",year(end_date),".png",sep=""))
peaks<-ggplot(peak_data) +
  geom_line(aes(date,pool_price,colour=on_peak),size=1.5)+
  #scale_color_viridis("",option="D",discrete = TRUE)+
  #scale_color_manual("",values = colors_ua10(),labels=c("Off-peak Prices","Peak prices"))+
  scale_color_manual("",values = colors_ua10() ,labels=c("Off-peak Prices","Peak prices"))+
  scale_x_date(expand=c(0,0),limits = c(period_start,end_date+months(2)), breaks="12 months",labels = date_format("%b\n%Y"))+
  scale_y_continuous(limits = c(0,max_price))+
  theme_minimal()+
  theme(legend.position="bottom",
        plot.margin = margin(c(0,1,0,0),unit="cm"),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+
  labs(y="Average Monthly Pool Price ($/MWh)",x="",
       title=paste("Alberta Monthly Average Peak and Off-Peak Wholesale Power Prices",sep=""),
       subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep=""),
       caption = "Peak hours are 8am to 11pm other than on statutory holidays or Sundays.\n Data via AESO, graph by Andrew Leach."
       )
peaks
if(png==1)
  dev.off()

png<-1
if(png==1)
  set_png(file=paste("peak_prices_ndp.png",sep=""))
mid<-ymd("2015-05-24")+((ymd("2019-04-30")-ymd("2015-05-24"))/2)
ggplot(peak_data) +
  geom_rect(mapping=aes(xmin=ymd("2015-05-24"),xmax=ymd("2019-04-30"),ymin=0,ymax=max_price,fill="NDP in office"),alpha=.25)+
  geom_text(
    aes(x = mid, y = max_price-150, label = "NDP in Power\nin Alberta"),
    size = 4, vjust = 0, hjust = 0.5, color = "black"
  )+
  
  scale_fill_manual("",values = "grey90")+
  geom_line(aes(date,pool_price,colour=on_peak),size=1.5)+
  #scale_color_viridis("",option="D",discrete = TRUE)+
  #scale_color_manual("",values = colors_ua10(),labels=c("Off-peak Prices","Peak prices"))+
  scale_color_manual("",values = colors_ua10() ,labels=c("Off-peak Prices","Peak prices"))+
  scale_x_date(expand=c(0,0),limits = c(period_start,end_date+months(2)), breaks="12 months",labels = date_format("%b\n%Y"))+
  scale_y_continuous(limits = c(0,max_price))+
  guides(fill=FALSE)+
  theme_minimal()+
  theme(legend.position="bottom",
        plot.margin = margin(c(.5,1,.5,1),unit="cm"),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+
  labs(y="Average Monthly Pool Price ($/MWh)",x="",
       title=paste("Alberta Monthly Average Peak and Off-Peak Wholesale Power Prices",sep=""),
       subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep=""),
       caption = "Peak hours are 8am to 11pm other than on statutory holidays or Sundays.\n Data via AESO, graph by Andrew Leach."
  )

if(png==1)
  dev.off()



period_start<-max(forecast_data$start_date)-days(21)
end_date<-period_start+weeks(1)

peak_data<-forecast_data %>% filter(date>=period_start,date<=end_date) %>%
  mutate(peak_ail=ifelse(on_peak,actual_ail,NA),
         peak_price=ifelse(on_peak,actual_posted_pool_price,NA),
         )

max_price<-max(max(peak_data$actual_posted_pool_price))


#previous week prices
png<-1

top_panel<-ggplot(peak_data) +
  geom_line(aes(time,actual_posted_pool_price,colour="basic"),size=1.5)+
  geom_line(aes(time,peak_price,colour="peak"),size=1.5)+
  #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
  scale_color_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_x_datetime(expand=c(0,0),breaks="1 day",labels = date_format("%H:00\n%b %d\n%Y",tz="America/Denver"))+
  scale_y_continuous(limits = c(0,max_price))+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt"))
  )+
  labs(y="Hourly Pool Price ($/MWh)",x="",
       title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       subtitle=paste(month.name[month(period_start)]," ",day(period_start),", ",year(period_start)," to ",month.name[month(end_date)]," ",day(end_date), ", ",year(end_date),sep="")
  )
bottom_panel<-ggplot(peak_data) +
  geom_line(aes(time,actual_ail,colour="basic"),size=1.5)+
  geom_line(aes(time,peak_ail,colour="peak"),size=1.5)+
  scale_color_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_x_datetime(expand=c(0,0),breaks="1 day",labels = date_format("%H:00\n%b %d\n%Y",tz="America/Denver"))+
  scale_y_continuous()+
  theme_minimal()+
  theme(legend.position="bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="Alberta Internal Load (MW)",x="")
#caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(top_panel)

if(png==1)
  #set_png(file=paste("peak_prices_",period_start,"_",end_date,".png",sep=""))
  set_png(file=paste("peak_prices_week.png",sep=""))
grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                           plot.caption = element_text(size = 12, face = "italic"),
                                           plot.title = element_text(size = 14,face = "bold"),
                                           plot.subtitle = element_text(size = 12, face = "italic"),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = 10,face = "bold"),
                                           axis.text = element_text(size = 10,face = "bold", colour="black"),
                                           axis.text.x = element_blank()
),
bottom_panel + theme(legend.position="none",
                     plot.caption = element_text(size = 12, face = "italic"),
                     plot.title = element_text(size = 14,face = "bold"),
                     plot.subtitle = element_text(size = 12, face = "italic"),
                     panel.grid.minor = element_blank(),
                     text = element_text(size = 10,face = "bold"),
                     axis.text = element_text(size = 10,face = "bold", colour="black")
),
ncol=1,heights=c(3,3)),
mylegend, nrow=2,heights=c(10, 1),bottom =text_grob(
  "Source: AESO data, graph by Andrew Leach. Peak periods are between 7am and 11pm other than on statutory holidays or Sundays.",
  face = "italic", color = "black",size=10,just="center",lineheight = 1
)
)
if(png==1)
  dev.off()

#ggplot(peak_data, aes(x=time, y=actual_ail, colour=actual_ail,fill=actual_ail)) +
#  geom_col(size=.8) +
#  scale_fill_gradient2(low = 'green', mid = 'yellow', high='red',
#                       labels=seq(0,12000,3000)) +
#  scale_y_continuous()




load_and_price_graph<-function(){
  #find the latest value we have in the forecast for a Sunday night
  #last_sunday<-max(forecast_data$start_date[forecast_data$wday=="Sun"])
  last_forecast<-max(forecast_data$date[!is.na(forecast_data$actual_posted_pool_price)])
  period_start<-ymd_hm(paste(last_forecast,"0:00",sep=" "))-weeks(1)
  end_date<-period_start+weeks(1)+hours(24)
  lims <- c(period_start,end_date)
  #lims <- c(min(work_data$start), max(work_data$start))
  break_start<-as.POSIXct(date(min(lims))+hours(12),tz="America/Denver")
  breaks<-seq.POSIXt(break_start, max(lims), by="1 day")
  
  #breaks<-seq.POSIXt(min(lims)+weeks(2), max(lims), by="1 month")
  
  df1<-forecast_data %>% filter(time>=period_start & time<=end_date)
  
  max_price<-max(max(df1$forecast_pool_price),max(df1$actual_posted_pool_price))
  max_price<-(trunc(max_price/10)+1)*10
  top_panel<-ggplot(df1) +
    geom_line(aes(time,forecast_pool_price,colour="Forecast Pool Price (top) or Alberta Internal Load (bottom)"),size=1)+
    geom_line(aes(time,actual_posted_pool_price,colour="Actual Pool Price (top) or Alberta Internal Load (bottom)"),size=1)+
    #scale_color_viridis("",option="D",discrete = TRUE)+
    scale_color_manual("",values = colors_tableau10())+
    scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
    scale_y_continuous(limits = c(0,max_price))+
    theme_minimal()+
    theme(legend.position="bottom",
          legend.margin=margin(c(0,0,0,0),unit="cm"),
          legend.text = element_text(colour="black", size = 12, face = "bold"),
    )+
    labs(y="Power Prices ($/MWh)",x="",
         title=paste("Alberta Wholesale Power Prices and Loads",sep=""),
         subtitle=paste(month.name[month(period_start)]," ",day(period_start),", ",year(period_start)," to ",month.name[month(end_date)]," ",day(end_date),", ",year(end_date),sep="")
    )
  print(top_panel)
  
  
  bottom_panel<-ggplot(df1) +
    geom_line(aes(time,day_ahead_forecasted_ail,colour="Day-ahead Load Forecast"),size=1)+
    geom_line(aes(time,actual_ail,colour="Alberta Internal Load"),size=1)+
    #scale_color_viridis("",option="D",discrete = TRUE)+
    scale_color_manual("",values = colors_tableau10())+
    scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
    #scale_y_continuous(limits = c(0,round(batt_size/10)*10))+
    scale_y_continuous()+
    theme_minimal()+theme(    
      legend.position = "bottom",
      plot.caption = element_text(size = 12, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 12,face = "bold", colour="black")
    )+    labs(y="Alberta Internal Load (MW)",x="")
  #caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
  
  print(bottom_panel)
  
  #extract legend
  #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(top_panel)
  
  png<-1
  if(png==1)
    set_png(file=paste("price_and_load.png",sep=""))
  grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                             legend.margin=margin(c(0,0,0,0),unit="cm"),
                                             legend.text = element_text(colour="black", size = 14, face = "bold"),
                                             plot.caption = element_text(size = 12, face = "italic"),
                                             plot.title = element_text(face = "bold"),
                                             plot.subtitle = element_text(size = 14, face = "italic"),
                                             panel.grid.minor = element_blank(),
                                             text = element_text(size = 12,face = "bold"),
                                             axis.text = element_text(size = 12,face = "bold", colour="black"),
                                             axis.text.x = element_blank()
  ),
  bottom_panel + theme(legend.position="none",
                       plot.caption = element_text(size = 12, face = "italic"),
                       plot.title = element_text(face = "bold"),
                       plot.subtitle = element_text(size = 16, face = "italic"),
                       panel.grid.minor = element_blank(),
                       text = element_text(size = 12,face = "bold"),
                       axis.text = element_text(size = 12,face = "bold", colour="black")
  ),
  ncol=1,heights=c(3,3)),
  mylegend, nrow=2,heights=c(10, 1),bottom =text_grob(
    "Source: AESO data, graph by Andrew Leach\n",
    face = "italic", color = "black",size=14,just="center",lineheight = 1
  )
  )
  
  if(png==1)#set these to only turn on if you're making PNG graphs
    dev.off()
}


#hourly prices  
load_and_price_graph()



sub_samp<-forecast_data
#take out the forecasts
sub_samp<-sub_samp[!is.na(sub_samp$actual_posted_pool_price),]
#take out the 02* if they are still there
sub_samp<-sub_samp[sub_samp$he!="02*",]


df1 <- sub_samp %>% group_by(he,year) %>% summarise(AIL=mean(actual_ail),p_mean=mean(actual_posted_pool_price))
df1$he<-factor(df1$he,levels = unique(df1$he))


set_png(file="hourly-prices-aeso.png")
ggplot(df1, aes(he,p_mean,group=as.factor(year),colour=as.factor(year))) +
  geom_line(data=subset(df1,year<2019),size=1.5) +
  geom_line(data=subset(df1,year==2019),size=3) +
  scale_color_viridis("",discrete=TRUE,option = "E")+
  guides(col = guide_legend(nrow=3))+
  ajl_hourly()+
  labs(y="Hourly Average Pool Price ($/MWh)",x="Hour Ending",
       title="Hourly Average Energy Prices ($/MWh, 2000-Present)",
       caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()

sub_samp$Date_ID = as.yearmon(sub_samp$date)
df1 <- sub_samp %>% group_by(Date_ID) %>% summarise(cons =sum(actual_ail)/1000,AIL =mean(actual_ail),Peak_AIL =max(actual_ail),Min_AIL =min(actual_ail), p_mean=sum(actual_posted_pool_price*actual_ail)/sum(actual_ail))
df1$Date<-as.Date(df1$Date_ID)
df1$m12_avg<-as.numeric(rollapply(df1$p_mean,12,mean,fill=NA,align = c("right")))
df1$m12_ail_avg<-as.numeric(rollapply(df1$AIL,12,mean,fill=NA,align = c("right")))

ggplot(df1, aes(month(Date_ID, label=TRUE, abbr=TRUE), 
                +                 AIL, group=factor(year(Date_ID)), colour=factor(year(Date_ID)))) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  labs(x="Month", colour="Year") +
  theme_classic()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    plot.caption = element_text(size = 18, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16)
  )+
  labs(y="Monthly Average Load (MW)",x="Month",
       title="Time Series of Monthly Average Load (MW, 2004-2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")


set_png(file="monthly-prices.png")
ggplot(data=subset(df1,Date>="2001-01-01"), aes(Date,p_mean,colour="A"),size=2.5) +
  geom_line(size=1.5) +
  geom_line(data=subset(df1,Date>="2001-01-01"),aes(Date,m12_avg,colour="B"),size=2.5) +
  scale_color_viridis("",labels=c("Monthly Average Prices","12 Month Rolling Average Prices"),discrete=TRUE)+   
  scale_x_date(labels = date_format("%h\n%Y"),breaks=seq.Date(as.Date("2001-01-01"),Sys.Date(),by="12 months"))+
  ajl_line()  +    
  labs(y="Monthly/Annual Average Power Price ($/MWh)",x="\nMonth",
       title="Load-Weighted Monthly Average Energy Prices ($/MWh, 2001-Present)",
       caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()

df1$m12_ail_avg<-as.numeric(rollapply(df1$AIL,12,mean,fill=NA,align = c("right")))
df1$m12_ail_max<-as.numeric(rollapply(df1$Peak_AIL,12,max,fill=NA,align = c("right")))
df1$m12_ail_min<-as.numeric(rollapply(df1$Min_AIL,12,min,fill=NA,align = c("right")))




set_png(file="load_time.png")
ggplot(data=subset(df1,Date>="2001-01-01")) +
  geom_ribbon(aes(x=Date,ymin = Min_AIL, ymax = Peak_AIL,fill="C")) +
  #geom_ribbon(aes(x=Date,ymin = m12_ail_min, ymax = m12_ail_max,fill="C")) +
  geom_line(aes(Date,AIL,colour="A"),size=1.5) +
  #geom_line(data=subset(df1,Date>="2001-01-01"),aes(Date,m12_avg,colour="B"),size=2.5) +
  scale_color_viridis("",labels=c("Monthly Average Internal Load"),discrete=TRUE)+
  scale_fill_manual("",values=c("grey70"),labels=c("Monthly range of internal load"))+   
  
  scale_x_date(labels = date_format("%h\n%Y"),breaks=seq.Date(as.Date("2001-01-01"),Sys.Date(),by="24 months"))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Alberta Internal Load (MW)",x="\nMonth",
             title="Alberta Internal Loads, 2001-Present",
             caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()



#seasons

df1$season<-getSeason(df1$Date)


png(file="monthly-loads.png", width = 1400, height = 750,type="cairo")
ggplot(data=subset(df1,Date>="2001-01-01"), aes(Date,AIL,colour="A"),size=2.5) +
  #geom_line(size=1.5) +
  geom_line(data=subset(df1,Date>="2001-01-01"),aes(Date,Peak_AIL,colour="B"),linetype = 1,size=1.5) +
  geom_line(data=subset(df1,Date>="2001-01-01"),aes(Date,m12_ail_avg,colour="C"),size=1.5) +
  scale_color_viridis("",labels=c("Monthly Peak Load","12 Month Rolling Average Load"),discrete=TRUE)+   
  scale_x_date(labels = date_format("%h\n%Y"),breaks=seq.Date(as.Date("2001-01-01"),Sys.Date(),by="12 months"))+
  scale_y_continuous(limits = c(6000,12000))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 18,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Alberta Internal Load (MW)",x="\nMonth",
             title="Monthly Peak and Annual Average Alberta Internal Load (MW, 2001-2017)",
             caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()

df1$m12_cons<-as.numeric(rollapply(df1$cons,12,mean,fill=NA,align = c("right")))
this_month<-ymd(paste(year(max(df1$Date)),month(max(df1$Date)),1,sep="-"))
last_month<-this_month-months(1)+days(days_in_month(this_month-months(1))-1)

png(file="monthly-cons.png", width = 1400, height = 750,type="cairo")
ggplot(data=subset(df1,Date>="2001-01-01" & Date<floor_date(Sys.Date(), "month")-days(1)), aes(Date,cons,colour="A"),size=2.5) +
  geom_line(size=1.5) +
  geom_line(aes(Date,m12_cons,colour="C"),size=1.5) +
  scale_color_viridis("",labels=c("Monthly Consumption","12 Month Rolling Average Consumption"),discrete=TRUE)+   
  scale_x_date(labels = date_format("%h\n%Y"),breaks=seq.Date(as.Date("2001-01-01"),Sys.Date(),by="12 months"))+
  #scale_y_continuous(limits = c(6000,12000))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 18,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Alberta Monthly Consumption (GWh)",x="\nMonth",
             title="Monthly and Annual Average Alberta Internal Consumption (GWh, 2001-2017)",
             caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()



#MERITS



augment_data<-function(merit_sent){
  #testing
  #merit_sent<-filter(merit_data, year(date)==2019,month(date)==2)%>% clean_merit_trade()
  
  #bring in plant data
  plant_data <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  plant_info<-data.frame(t(plant_data[(1:10),-1]),stringsAsFactors = F)
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:10),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  
  plant_info<-plant_info %>%
    mutate(NRG_Stream = ifelse(grepl("AB - H R Milner Hr Avg MW",NRG_Stream),"AB Milner Hr Avg MW",NRG_Stream))%>%
    mutate(NRG_Stream = ifelse(grepl("AB - NPC1 Denis St Pierre Hr Avg MW",NRG_Stream),"AB - NPC1 Denis St  Pierre Hr Avg MW",NRG_Stream))  
  plant_info<-arrange(plant_info,NRG_Stream)
  
  #bring in ghg data
  ghg_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "GHG_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  #ghg_rates<-dcast(ghg_rates, formula = GHG_ID ~ ...,value.var = "Poln_rate")
  ghg_rates<-ghg_rates %>% spread(Pollutant,Poln_rate) %>% select(GHG_ID,CO2)
  
  
  #bring in heat rates
  heat_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Heat_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE) %>%
    select(GHG_ID,Aurora_ID,Heat.Rate)
  #combine all plant info, heat rates, and GHGs by plant ID
  combined<-merge(ghg_rates,heat_rates,by="GHG_ID",all.y = T) # NA's match
  coal_co2_btu<-100.4  #coal fuel factor GHGs/MMBTU
  gas_co2_btu<-53.077752 #gas fuel factor GHGs/MMBTU
  combined<-merge(plant_info,combined,suffixes = c(".info",".rates"),all.x=TRUE,by="Aurora_ID") # NA's match
  combined$co2_est<-combined$CO2/2.20462*combined$Heat.Rate #convert to kg/mmbtu
  combined$co2_est<-ifelse(combined$Plant_Fuel=="COAL",coal_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-ifelse(combined$Plant_Fuel=="GAS",gas_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-combined$co2_est/1000 #adjust from kg to tonnes
  
  merit_sent<-merit_sent %>% group_by(date,he) %>%
    mutate(hourly_exports=sum((import_export=="E")*size),hourly_imports=sum((import_export=="I")*size))  %>% ungroup()
  
  merit_sent %>% left_join(combined,by=c("asset_id"="ID")) %>%left_join(forecast_data,by=c("date","he")) %>%
    select(-Latitude,-Longitude,-Aurora_Name,-NRG_Stream,GHG_ID,start_date)
  
  
}



clean_merit_trade<-function(data_sent,id_tag){
  #for testing
  #data_sent<-head(merit_data,1000) #%>% assign_time %>% assign_date_time_days %>% assign_peaks
  
  #read in all AESO asset names so we're up-to-date
  zz =
    readHTMLTable("http://ets.aeso.ca/ets_web/ip/Market/Reports/AssetListReportServlet?contentType=html",colClasses = "character",stringsAsFactors = FALSE)
  aeso_assets<-as.data.frame(rbind(zz)[2])
  colnames(aeso_assets)<- aeso_assets[1, ] # the first row will be the header
  aeso_assets<-clean_names(aeso_assets[-1,])
  zz<-NULL
  trade<-data_sent%>%filter(import_export %in% c("E","I")) %>% #select the imports and exports from the merit data
    left_join(aeso_assets,by=c("asset_id"="asset_id"))
  trade$dest<-NA
  trade<-trade %>% mutate(dest = ifelse(grepl("BC",asset_name),"AB_BC",dest),
                          dest = ifelse(grepl("MT",asset_name),"AB_MON",dest),
                          dest = ifelse(grepl("SK",asset_name),"AB_SK",dest),
                          dest = ifelse(grepl("Sask",asset_name),"AB_SK",dest),
                          dest = ifelse(grepl("SPC",asset_name),"AB_SK",dest), 
                          dest = ifelse(grepl("XB",asset_name),"AB_BC",dest),
                          dest = ifelse(grepl("PWX",asset_name),"AB_BC",dest))
  trade<- trade %>% group_by(date,he,effective_date_time,import_export,dest,dispatched,flexible,price) %>% 
    summarise(size=sum(size),
              available_mw=sum(available_mw),
              dispatched_mw=sum(dispatched_mw)) %>%
    mutate(key_firm=TRUE,offer_control="TRADE",offer_sum="TRADE",merit=0,
           from=0,to=available_mw,block_number=0,) %>% 
    ungroup()%>%
    mutate(dest=ifelse(import_export=="I",paste(dest,"_IMP",sep = ""),paste(dest,"_IMP",sep = "")))
  names(trade)[names(trade) == "dest"] <- "asset_id"
  clean<-data_sent%>%filter(!import_export %in% c("E","I")) %>% #select the non-imports and non-exports from the merit data
    rbind(trade)
  clean
}



#need three sets of data - metered volumes for wind and solar plants, prices, merits

#load and updated merit order data

update<-1
load("all_merit.RData")  
if(update!=0){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  #remove the 02* hours
  #  merit_data<-merit_data%>%filter(he!="02*")
  save(merit_data, file="all_merit.RData")  
  
  }


#AS MERIT 
#load(file="merit_AS.RData")
#if(update!=0){
#  merit_AS_data<-rbind(merit_AS_data,update_AS_merit(merit_AS_data))
##  save(merit_AS_data, file="merit_AS.RData")  
#}



#check to make sure singles are consistent
singles<-seq(1,9)
for(hour in singles){
  merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
  #merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
}


#update and load prices and load data
update_forecasts()
load("forecast_data.RData")
#assign weekdays, stats, peaks, etc., clean trade, add plant info!
#testing trade cleaning here
#merit_test<-head(merit_data,1000) %>% filter(import_export!="") %>% clean_merit_trade() %>% assign_time %>% assign_date_time_days %>% assign_peaks %>% 
#    augment_data()


#load metered volumes data
load(file="metered_vols_data.Rdata" ) 


#process some data


merit_aug<-filter(merit_data, year(date)>=2016)%>% clean_merit_trade() %>%
  augment_data()

#get net-to-grid from wind production

renew_gen<- all_vols %>% filter(year(date)>=2016) %>%
  filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% select(date,he,asset_id,vol)%>%rename("renew_gen"="vol")

#get net-to-grid from cogen

cogen_gen<- all_vols %>% filter(year(date)>=2016) %>%
  filter(Plant_Type=="COGEN") %>% select(date,he,asset_id,vol)%>%rename("cogen_gen"="vol")

#Problem is that my zeros are disappearing.  Why?!?


merit_adj<-merit_aug%>% 
  select(date,he,from,to,size,price,block_number,dispatched,dispatched_mw,flexible,asset_id,offer_control,AESO_Name,Plant_Type,available_mw,co2_est,forecast_pool_price,actual_posted_pool_price,actual_ail)%>%
  left_join(renew_gen,by=c("date","he","asset_id")) %>%
  left_join(cogen_gen,by=c("date","he","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) %>%
  group_by(date,he) %>% mutate(hourly_avail=sum(available_mw),hourly_dispatch=sum(available_mw*(dispatched=="Y")))%>%
  arrange(price,Plant_Type) %>% mutate(merit=cumsum(available_mw),merit_dispatch=cumsum(available_mw*(dispatched=="Y"))) %>%
  ungroup() %>%arrange(date,he,merit)


#write.csv(merit_tim,"merit_tim.csv")


cogen_test<-merit_adj %>% filter(Plant_Type=="COGEN") %>% group_by(date,he,asset_id) %>% summarize(available_mw=sum(available_mw),dispatched_mw=sum(dispatched_mw),cogen_gen=sum(cogen_gen))

merit_2019<- merit_tim %>% filter(year(date)>=2019) 
write.csv(merit_2019,"merit_2019.csv")


df1<-df1 %>% left_join(renew_gen,by=c("time","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) %>%
  group_by(time) %>% 
  arrange(time,price,offer_sum) %>% 
  mutate(merit=cumsum(available_mw))%>%
  ungroup()




merit_day<-merit_aug %>% filter(date==as.Date("2018-07-11"))%>% 
  select(date,he,from,to,size,price,block_number,dispatched,dispatched_mw,flexible,asset_id,offer_control,AESO_Name,Plant_Type,available_mw,co2_est,forecast_pool_price,actual_posted_pool_price,actual_ail)%>%
  left_join(renew_gen,by=c("date","he","asset_id")) %>%
  left_join(cogen_gen,by=c("date","he","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) %>%
  group_by(date,he) %>% mutate(hourly_avail=sum(available_mw),hourly_dispatch=sum(available_mw*(dispatched=="Y")))%>%
  arrange(price,Plant_Type,asset_id) %>% mutate(merit=cumsum(available_mw),merit_dispatch=cumsum(available_mw*(dispatched=="Y"))) %>%
  ungroup() %>%arrange(date,he,merit)



#geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
#geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
#geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3)])+  
  scale_colour_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3)])+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,13000))+
  scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_line()+theme(plot.subtitle = element_text(size = 14))+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Alberta Energy Merit Order by Plant Type, ",max(merit_2018_07_11$date)," ",max(as.character(merit_2018_07_11$he)),":00",sep = ""),
       subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: AESO Data, graph by Andrew Leach.")

dev.off()







merit_2018_07_11<-merit_aug %>% filter(date==as.Date("2018-07-11")&he=="04")%>% 
  mutate(oba_adj=case_when(Plant_Fuel=="COAL"~ (.800),
                           Plant_Fuel=="GAS"~ (.370),
                           TRUE~0),
         bid_adj=case_when(block_number==0~ price,
                           block_number>0~ price+.370*30-oba_adj*30)
  )%>% 
  group_by(date,he) %>% arrange(bid_adj,Plant_Type) %>% mutate(merit2=cumsum(size)) %>%
  ungroup()



set_png("merit_oba.png")
ggplot(arrange(merit_2018_07_11,bid_adj,Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=bid_adj,fill=Plant_Type))+
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=price,colour=Plant_Type),fill=NA)+
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
  #geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
  #geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
  geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3)])+  
  scale_colour_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3)])+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,13000))+
  scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_line()+theme(plot.subtitle = element_text(size = 14))+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Alberta Energy Merit Order by Plant Type, ",max(merit_2018_07_11$date)," ",max(as.character(merit_2018_07_11$he)),":00",sep = ""),
       subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: AESO Data, graph by Andrew Leach.")

dev.off()



#start with merit data for a day
#clean up trade and add plant info
load("all_merit.RData")  


merit_day<-merit_data %>% filter(date==ymd("2018-08-01")) %>% clean_merit_trade() %>%
  augment_data()

merit_set<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","WIND")      
#take out the wind generators
df1<-arrange(filter(merit_day,Plant_Type %in% merit_set),he,price,Plant_Type)
#now we need wind metered volumes for the day in question
load(file="metered_vols_data.Rdata" ) 
renew_gen<- all_vols %>% filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% group_by(time,asset_id) %>% summarise(renew_gen=sum(vol,na.rm = T))
#merge the actual wind gen by unit
df1<-df1 %>% left_join(renew_gen,by=c("time","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) %>%
  group_by(time) %>% 
  arrange(time,price,offer_sum) %>% 
  mutate(merit=cumsum(available_mw))%>%
  ungroup()


df1<-df1%>% group_by(time) %>% arrange(time,he,price,offer_sum) %>% mutate(merit=cumsum(available_mw))


ggplot(subset(df1,he==18),aes(merit,price,fill=offer_sum)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price"),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+   
  scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,12300))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(df1$price)))+
  guides(fill=guide_legend(nrow=2))+
  ajl_line()+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")       



p<-ggplot(df1,aes(merit,price,fill=offer_sum)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price"),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+   
  scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,13000))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(df1$price)))+
  guides(fill=guide_legend(nrow=2))+
  guides(color=guide_legend(nrow=1))+
  ajl_line()+
  labs(title = 'Hour ending: {frame}', x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.") +
  transition_manual(time)+
  enter_fade() +
  exit_fade()


df1<-df1%>% group_by(time) %>% arrange(time,he,price,Plant_Type) %>% mutate(merit=cumsum(available_mw))

ggplot(filter(df1,he=="18"),aes(merit,price,fill=Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price,frame=time))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price",frame=time),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+   
  scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,12001))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(df1$price)))+
  guides(fill=guide_legend(nrow=2))+
  ajl_line()+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")


p<-ggplot(df1,aes(merit,price,fill=Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price"),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+   
  scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,13000))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(df1$price)))+
  guides(fill=guide_legend(nrow=2))+
  guides(color=guide_legend(nrow=1))+
  ajl_line()+
  labs(title = 'Hour ending: {frame}', x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.") +
  transition_manual(time)+
  enter_fade() +
  exit_fade()



#this is the section that converts the merit orders to a step function by MW

if(!exists("generate_func", mode="function")) source("generate_func.R")

df2<-filter(merit_aug,asset_id=="GN3",date==ymd("2017-03-07"))


#here, for example, we can create a MWx24 matrix for each day for each plant of bid values


df2<-arrange(df2,date,as.numeric(he),block_number)
df2$date_id<-paste(df2$date," ",df2$he,sep="")
max_cap<-as.numeric(max(df2$to))
df3<-data.frame(seq(0,max_cap))
names(df3)[1]<-"MW"
for(date_count in unique(df2$date_id)) {
  print(ymd_h(date_count,tz="America/Denver"))
  hour_id<-as.character(hour(ymd_h(date_count,tz="America/Denver")))
  print(hour_id)
  df_test<-df2 %>% filter(date_id==date_count)
  if(nrow(df_test)>1){
    step_fun<-generate_func(df_test$from[-1],df_test$price)
    df3[,hour_id]<-step_fun(df3$MW)
  }
  else
    df3[,hour_id]<-df_test$price
}



df3<-melt(df3,id=c("MW"),variable.name = "he",value.name = "price")  

ggplot(df3)+
  geom_line(aes(MW,price,group=he,colour=he))+
  scale_color_manual("Hour Ending",values=c(colors_tableau10(),colors_tableau10_light(),colors_tableau10_medium()))




#some specific merit order bids

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png(file="bids_alpac.png",width=1200,height=800)
ggplot(df3, aes(MW,price,group=time,colour=time)) +
  geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
  
  geom_line(size=1.25)+
  scale_color_viridis("",discrete=TRUE,labels=rev(unique(df3$time)))+   
  #guides(colour=FALSE)+
  slide_theme()+    labs(y="Offer Price ($/MWh)",x="MW",
                         title="Power Offer Blocks, Alberta-Pacific Mill ($/MWh)",
                         caption="Source: AESO Data, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



df1<-subset(merit_data, date==as.Date("2015-01-01") & asset_id=="SD5")
df1$year<-year(df1$date)
df1 <- df1 %>% group_by(year,block_number) %>% summarise(price=mean(price),to=mean(to),from=mean(from))
df1 <- arrange(df1,year,block_number)
ggplot(subset(df1),aes(to,price)) +
  #geom_step(mapping=aes(x=merit,y=price))
  geom_rect(mapping=aes(xmin=from,xmax=to,ymin=price-0.1,ymax=price),alpha=1,fill="firebrick")+
  facet_wrap(~year)+
  #geom_step(mapping=aes(x=to,y=price),
  #           linetype=1,color='#d95f02',alpha=1,size=3)
  #geom_line(size=2)+
  #scale_y_continuous()+
  #scale_color_viridis(NULL, labels=c("July 2015","Jan 2016","July 2016","Jan 2017", "July 2017"),discrete=TRUE)+   
  #scale_color_viridis(NULL,discrete = TRUE,guide = 'none')+
  scale_fill_viridis("Offer\nControl",discrete = TRUE))+   
  #scale_x_date() +
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  #theme_minimal()+
  theme(
    legend.position = "right",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Sundance 5 Average Offer Blocks, ",min(df1$year),"-",max(df1$year),sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")

df1<-subset(merit_data, date>as.Date("2015-01-01") & asset_id=="BR5")
df1$year<-year(df1$date)
df1 <- df1 %>% group_by(year,block_number) %>% summarise(price=mean(price),to=mean(to),from=mean(from))
df1 <- arrange(df1,year,block_number)
ggplot(subset(df1),aes(to,price)) +
  #geom_step(mapping=aes(x=merit,y=price))
  geom_rect(mapping=aes(xmin=from,xmax=to,ymin=-5,ymax=price),alpha=1,fill="firebrick")+
  facet_wrap(~year)+
  #geom_step(mapping=aes(x=to,y=price),
  #           linetype=1,color='#d95f02',alpha=1,size=3)
  #geom_line(size=2)+
  #scale_y_continuous()+
  #scale_color_viridis(NULL, labels=c("July 2015","Jan 2016","July 2016","Jan 2017", "July 2017"),discrete=TRUE)+   
  #scale_color_viridis(NULL,discrete = TRUE,guide = 'none')+
  scale_fill_viridis("Offer\nControl",discrete = TRUE)+   
  #scale_x_date() +
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  #theme_minimal()+
  theme(
    legend.position = "right",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Battle River 5 Average Offer Blocks, ",min(df1$year),"-",max(df1$year),sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.") 


df1<-subset(merit_data, he==18 & date==as.Date("2017-10-05"))

ggplot(df1,aes(merit,price,fill=offer_sum)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-20,ymax=price))+
  scale_fill_viridis("Offer\nControl",discrete = TRUE)+   
  #scale_x_date() +
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  #theme_minimal()+
  theme(
    legend.position = "right",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Alberta Energy Merit Order, ",max(df1$date)," ",max(df1$he),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")

sger_rates <- read.xlsx(xlsxFile = "power_ghgs.xlsx", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
sger_combo<-merge(combined,sger_rates,by.x="ID",by.y="ID",all.y = TRUE)
sger_combo$co2_est<-sger_combo$co2_est/1000
sger_combo<-sger_combo[,c("ID","AESO_Name","Plant_Type","CO2","co2_est","GHG/MWh","SGER_EI","BEI.(in.CR)")]
#sger_combo$err_BEI<-ifelse(sger_combo$Plant_Type=="COGEN",sger_combo$`BEI.(in.CR)`- sger_combo$co2_est,sger_combo$`GHG/MWh`- sger_combo$co2_est)
#sger_combo$err_SGER<-ifelse(sger_combo$Plant_Type=="COGEN",sger_combo$SGER_EI- sger_combo$co2_est,sger_combo$`GHG/MWh`- sger_combo$co2_est)
#sger_combo$err<-ifelse(sger_combo$Plant_Type=="COGEN",sger_combo$`GHG/MWh`- sger_combo$co2_est,sger_combo$`GHG/MWh`- sger_combo$co2_est)
#sger_combo<-arrange(sger_combo,-abs(err))
write.xlsx(sger_combo, file = "sger_combo.xlsx", colNames = TRUE, borders = "columns") 







#old coal animations


df1<-subset(merit_test,Plant_Type=="COAL")

df1<-df1%>% arrange(date,he,price,offer_sum) %>% group_by(date,he)%>% mutate(merit=cumsum(size))



df2 <- df1 %>% group_by(he,merit) %>% summarise(price=mean(price))

source("tableau.R")

p<-ggplot(df2) +
  geom_point(aes(merit,price))+
  facet_wrap(~he,ncol = 4)+
  ggtitle("Alberta Merit Order, Internal Load, Exports,\nand Power Prices ")+
  scale_fill_manual("Plant\nType",values = colors_tableau10())+
  #scale_fill_viridis("Plant\nType",discrete = T)+
  scale_colour_manual("Market\noutcomes",values=colors_tableau10_light())+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,13750,3000),limits = c(0,13751))+
  #scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,250),limits=c(-20,1001))+
  #scale_y_continuous(expand=c(0,0),breaks=seq(0,150,50),limits=c(-20,151))+
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))+
  #scale_x_date() +
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  #theme_minimal()+
  theme(
    legend.position = "right",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, ",max(df1$date)," ",max(df1$he),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")




p<-ggplot(df1) +
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-20,ymax=price,fill=Plant_Type,frame=time))+
  geom_vline(aes(xintercept=actual_ail+hourly_exports,colour="Actual AIL\nand Exports",frame=time),linetype=2,size=1)+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price",frame=time),linetype=2,size=1)+
  #geom_hline(aes(yintercept=min(df1$actual_posted_pool_price),colour="Min Price",frame=time),linetype=2,size=1)+
  #facet_wrap(~he,ncol = 4)+
  ggtitle("Alberta Merit Order, Internal Load, Exports,\nand Power Prices ")+
  scale_fill_manual("Plant\nType",values = colors_tableau10())+
  #scale_fill_viridis("Plant\nType",discrete = T)+
  scale_colour_manual("Market\noutcomes",values=colors_tableau10_light())+   
  scale_x_continuous(expand=c(0,0),breaks = seq(0,13750,3000),limits = c(0,13751))+
  #scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,250),limits=c(-20,1001))+
  #scale_y_continuous(expand=c(0,0),breaks=seq(0,150,50),limits=c(-20,151))+
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))+
  #scale_x_date() +
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  #theme_minimal()+
  theme(
    legend.position = "right",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, ",max(df1$date)," ",max(df1$he),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")
library(gganimate)
animation::ani.options(interval = 1)
gganimate(p, title_frame = TRUE,"merit.gif")









#check peaks for merit data


test<-all_vols %>% filter(is.na(vol))
test<-NULL

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

library(patchwork)
library(cowplot)

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





