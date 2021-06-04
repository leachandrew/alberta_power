library(tidyverse)
library(lubridate)
require(reshape2)
library(openxlsx)
library(ggplot2)
library(zoo)
library(ggmap)
library(ggjoy)
library(viridis)
library(RColorBrewer)
library(scales) 
library(gganimate)
library(rvest)
library(httr)
library(stringi)


#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive")
print(getwd())


set_png<-function(file_sent,w_sent=1400,h_sent=750,res_sent=130){
  #MAC
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file=file_sent, width = w_sent, height = h_sent,res=res_sent)
  #PC
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file=file_sent, width = w_sent, height = h_sent,res=res_sent,type='cairo')
}


## Make breaks from a starting date at a given hour, occuring by interval,
## length.out is days
make_breaks <- function(strt, hour, interval="day", length.out=31) {
  strt <- as.POSIXlt(strt - 60*60*24)  # start back one day
  strt <- ISOdatetime(strt$year+1900L, strt$mon+1L, strt$mday, hour=hour, min=0, sec=0, tz="UTC")
  seq.POSIXt(strt, strt+(1+length.out)*60*60*24, by=interval)
}





load("gen.RData") ## which is here *equivalent* to


gen_data$gen[is.na(gen_data$gen)] <- 0

gen_data$AIL<-as.numeric(gen_data$AIL)
gen_data$Price<-as.numeric(gen_data$Price)
gen_data$Demand<-as.numeric(gen_data$Demand)



sub_samp<-subset(gen_data, Time > as.Date("2010-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-10-15"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
sub_samp<-na.omit(sub_samp)
sub_samp$Year<-year(sub_samp$Time)
df1 <- sub_samp %>% group_by(Plant_Type,Time,Year) %>% summarise(sumcap = sum(Capacity),total_gen=sum(gen),p_mean=mean(Price))
df1$Year_ID=as.character(df1$Year)
# Histogram of Generation Densities
png(file="wind_cdf.png", width = 1400, height = 750)
ggplot(df1,aes(total_gen))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_x_continuous(limits=range(df1$total_gen),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),labels = scales::percent)+
  scale_color_viridis(discrete=TRUE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Wind Generation (MW)",y="% of time wind power less than X MW",
       title="Cumulative Density Function, Wind Energy Generation by Hour (2010-2017 Avg)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()





sub_samp<-subset(gen_data, Time > as.Date("2010-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-10-15"))
sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
sub_samp<-na.omit(sub_samp)
sub_samp$Year<-year(sub_samp$Time)
sub_samp$month_id<-paste(month.abb[month(sub_samp$Time)],year(sub_samp$Time))
sub_samp$month<-month(sub_samp$Time)

df1 <- sub_samp %>% group_by(month_id,month,Year) %>% summarise(total_gen=sum(gen),p_mean=mean(Price))
df1$Year<-as.factor(df1$Year)
df1$month_id <-as.factor(df1$month_id)
df1$month = factor(month.abb[df1$month], levels = month.abb)
ggplot(df1)+
  geom_col(aes(month,total_gen/10^6,colour=Year,group=Year,fill=Year),position = position_dodge(width = 0.9),width = 1)+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_color_viridis(discrete=TRUE)+
  scale_fill_viridis(discrete=TRUE)+
  scale_x_discrete(expand=c(.05,.05))+
  scale_y_continuous(expand=c(.05,.05))+
    theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0.1,0.1,0.1,0.1),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Month",y="Total Generation (TWh)",
       title="Monthly Generation (2010-2017 Avg)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()



sub_samp<-gen_data[grep("WIND", gen_data$Plant_Fuel), ] #wind plants
sub_samp<-subset(sub_samp, Time > as.Date("2008-01-01"))
sub_samp<-subset(sub_samp, Time < as.Date("2010-12-31"))
sub_samp<-na.omit(sub_samp)

df2<-dcast(sub_samp, Time~AESO_Name,value.var = "gen")




png(file="wind_cap.png", width = 1400, height = 750)
ggplot(df1,aes(total_gen/sumcap*100))+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=3)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_x_continuous(limits=range(df1$total_gen/df1$sumcap*100))+
  scale_color_viridis(discrete=TRUE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Wind Generation Capacity Factor (%)",y="",
       title="Distribution of Wind Energy Hourly Capacity Factors (2014-2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()





sub_samp<-subset(gen_data, Time > as.Date("2005-01-1"))
sub_samp<-subset(gen_data, year(Time) %in% c(2008,2013,2016,2017))

#sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND" | sub_samp$`Plant_Fuel`=="GAS" |sub_samp$`Plant_Fuel`=="COAL")
sub_samp<-na.omit(sub_samp)



sub_samp<-subset(gen_data, Time >= as.Date("2007-01-1"))

#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

#sub_samp<-subset(gen_data, year(Time) %in% c(2008,2013,2016,2017))

df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
df1$Day <- date(df1$Time)
df1$Year <- as.factor(year(df1$Time))
df1<-na.omit(df1)
#df1$Revenue <- df1$total_gen*df1$p_mean

df2 <- df1 %>% group_by(Plant_Type,Year) %>% summarise(capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean))
write.xlsx(df2, file = "tim_wind.xlsx", colNames = TRUE, borders = "columns")

png(file="price_capture.png", width = 1400, height = 750)
ggplot(df2,aes(Year,capture,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture,colour=Plant_Type,fill=Plant_Type),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("Plant Type",discrete=TRUE)+
  scale_fill_viridis("Plant Type",discrete=TRUE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
labs(x="Year",y="Average Revenue ($/MWh)",
     title="Energy Price Capture ($/MWh, 2007-2017)",
     caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach")
dev.off()

png(file="price_capture_avg.png", width = 1400, height = 750)
ggplot(df2,aes(Year,capture-p_mean,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture-p_mean,colour=Plant_Type,fill=Plant_Type),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("Plant Type",discrete=TRUE)+
  scale_fill_viridis("Plant Type",discrete=TRUE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Year",y="Price Capture Relative to Average Price ($/MWh)",
       title="Energy Price Capture Differential ($/MWh, 2007-2017)",
       caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach")
dev.off()

png(file="price_capture_pct.png", width = 1400, height = 750)
ggplot(df2,aes(Year,capture/p_mean*100-100,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture/p_mean*100-100,colour=Plant_Type,fill=Plant_Type),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("Plant Type",discrete=TRUE)+
  scale_fill_viridis("Plant Type",discrete=TRUE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Year",y="Price Capture Relative to Average Price (%)",
       title="Energy Price Capture Differential (%, 2007-2017)",
       caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach")
dev.off()



df2 <- df1 %>% group_by(Day,Year) %>% summarise(capture = sum(total_rev)/sum(total_gen))
df2<-subset(df2, Year==2017 | Year==2014)
png(file="price_capture_all.png", width = 1400, height = 750)
ggplot(df2,aes(capture,group=Year,colour=Year),alpha=0.5)+
  geom_density(aes(),size=1.5)+
  scale_x_continuous(limits=c(0,500))+
  scale_color_viridis(discrete=TRUE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Daily Average Power Revenues ($/MWh)",y="Density",
       title="Distribution of Daily Energy Revenues ($/MWh, 2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()


sub_samp<-subset(gen_data, Time > as.Date("2017-05-20"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-5-23"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)
sub_samp$Month_Year <- decimal_date(sub_samp$Time)
sub_samp$Month <- month(sub_samp$Time)
sub_samp$Hour <- hour(sub_samp$Time)
sub_samp$Day <- day(sub_samp$Time)
#distribution of hourly prices - duration curves and density functions


#df1 <- sub_samp %>% group_by(Hour,Day) %>% summarise(total_gen=sum(gen))

#ggplot(df1, aes(Hour,total_gen,colour=Day)) + 
#  geom_line() + 
#  coord_polar(theta = "x")

sub_samp<-subset(gen_data, Time >= as.Date("2004-01-1"))
#sub_samp<-subset(sub_samp, Time < as.Date("2017-7-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND" | sub_samp$`Plant_Fuel`=="GAS" |sub_samp$`Plant_Fuel`=="COAL")
sub_samp<-na.omit(sub_samp)
sub_samp$Month_Year <- decimal_date(sub_samp$Time)
sub_samp$Year <- year(sub_samp$Time)
sub_samp$Month <- month(sub_samp$Time)
sub_samp$Hour <- hour(sub_samp$Time)
#distribution of hourly prices - duration curves and density functions


df1 <- sub_samp %>% group_by(Time,Year) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
df1$Year_ID=as.character(df1$Year)
#df1$Revenue <- df1$total_gen*df1$p_mean



png(file="price_dist.png", width = 1400, height = 750,type = "cairo")
ggplot(data=subset(df1,Year %in% c(2008,2013,2016,2017)),aes(total_rev/total_gen))+
  #ggplot(df1,aes(p_mean))+
  #geom_density(aes(group=Year_ID,colour=Year_ID),size=1.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  guides(fill = guide_legend(title = "LEFT", title.position = "left"))+
  labs(colour="Year") +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+
  scale_y_reverse(labels = scales::percent,expand=c(0,0))+
  scale_x_continuous(limits=c(0.5,1000),expand=c(.10,.10))+
  scale_color_viridis(discrete=TRUE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.5,.5,.5,.5),unit="cm"),
    legend.text = element_text(colour="black", size = 32, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 32, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 32,face = "bold"),
    axis.text = element_text(size = 32,face = "bold", colour="black")
  )+
  labs(x="Daily Average Power Prices ($/MWh)",y="Percentage of Time",
       title="Duration Curve of Hourly Energy Prices ($/MWh)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()



sub_samp<-subset(gen_data, Time >= as.Date("2004-01-1"))
#sub_samp<-subset(sub_samp, Time < as.Date("2017-7-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND" | sub_samp$`Plant_Fuel`=="GAS" |sub_samp$`Plant_Fuel`=="COAL")
#sub_samp<-na.omit(sub_samp)
#sub_samp<-sub_samp[-is.na(sub_samp)] 
#sub_samp<- sub_samp %>%  mutate_all(funs(ifelse(is.na(.), 0, .)))

sub_samp$Month_Year <- decimal_date(sub_samp$Time)
sub_samp$Year <- year(sub_samp$Time)
sub_samp$Month <- month(sub_samp$Time)
sub_samp$Hour <- hour(sub_samp$Time)
sub_samp$Date_ID = as.yearmon(sub_samp$Time)



df1 <- sub_samp %>% filter(!is.na(Price))%>% filter(!is.na(gen)) %>%
  group_by(Date_ID) %>% 
  summarise(,total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))


df1$Date<-as.Date(df1$Date_ID)
df1$avg_rev<-df1$total_rev/df1$total_gen
df1$m12_avg<-as.numeric(rollapply(df1$avg_rev,12,mean,fill=NA,align = c("right")))



lims <- c(min(df1$Date)+years(1),max(df1$Date))


breaks<-seq.Date(lims[1],lims[2], by="1 year")

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("monthly_prices.png")
ggplot(data=df1, aes(Date,total_rev/total_gen,colour="A"),size=2.5) +
  geom_line(size=1.5) +
  geom_line(data=subset(df1,Date>="2005-01-01"),aes(Date,m12_avg,colour="B"),size=2.5) +
  scale_color_viridis("",labels=c("Monthly      \nAverage\nPrices","12 Month     \nRolling Average\nPrices"),discrete=TRUE)+   
  scale_x_date(limits = lims,breaks=breaks,labels = date_format("%b\n%Y", tz="America/Denver"))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 14,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Monthly/Annual Average Power Price ($/MWh)",x="Month",
             title="Generation-Weighted Monthly Average Energy Prices ($/MWh, 2005-2017)",
             caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

  
#hourly prices  
  
  
  sub_samp$Date_ID = as.yearmon(sub_samp$Time)
  sub_samp$month = month(sub_samp$Time)
  sub_samp$week_num = week(sub_samp$Time)
  df1 <- sub_samp %>% group_by(Hour,Year) %>% summarise(AIL=mean(AIL),meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
  df1$Year=as.character(df1$Year)
  df1<-merge(df1,dfyear,by="Year")
  
  png(file="hourly-prices.png", width = 1400, height = 750)
  ggplot(df1, aes(Hour,total_rev/total_gen,group=Year,colour=Year)) +
    geom_line(data=subset(df1,Year<2017),size=1.5) +
    geom_line(data=subset(df1,Year==2017),size=3) +
    geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+   
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 20, face = "bold"),
      plot.caption = element_text(size = 24, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 24, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 24,face = "bold"),
      axis.text = element_text(size = 24,face = "bold", colour="black")
    )+    labs(y="Hourly Average Power Price ($/MWh)",x="Hour",
               title="Generation-Weighted Hourly Average Energy Prices ($/MWh, 2004-2018)",
               caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  
  png(file="hourly-loads.png", width = 1400, height = 750)
  ggplot(df1, aes(Hour,AIL,group=Year,colour=Year)) +
    #geom_line(data=subset(df1,Year<2017),size=1.5) +
    #geom_line(data=subset(df1,Year==2017),size=3) +
    geom_line(data=subset(df1,Year>=2007),size=1.5) +
    #geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+   
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 20, face = "bold"),
      plot.caption = element_text(size = 24, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 24, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 24,face = "bold"),
      axis.text = element_text(size = 24,face = "bold", colour="black")
    )+    labs(y="Hourly Load (MW)",x="Hour",
               title="Hourly Average Internal Load (MW, 2007-2017)",
               caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  #monthly loads  
  
  
  df1 <- sub_samp %>% group_by(week_num,Year) %>% summarise(peak_AIL=max(AIL),AIL=mean(AIL),meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
  year_vec<-c(2014,2015,2016,2017)
  df1$Year_ID=as.character(df1$Year)
  dfyear <- sub_samp %>% group_by(Year) %>% summarise(year_p_mean=mean(Price))
  df1<-merge(df1,dfyear,by="Year")
  df1<-subset(df1,df1$Year %in% year_vec)
  df1$week_num<-as.factor(df1$week_num)
  
  png(file="monthly-loads.png", width = 1400, height = 750)
  ggplot(df1, aes(week_num,peak_AIL,group=Year_ID,colour=Year_ID)) +
    #geom_line(data=subset(df1,Year<2017),size=1.5) +
    #geom_line(data=subset(df1,Year==2017),size=3) +
    geom_line(data=subset(df1,Year>=2007),size=1.5) +
    #geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+ 
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 20, face = "bold"),
      plot.caption = element_text(size = 24, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 24, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 24,face = "bold"),
      axis.text = element_text(size = 24,face = "bold", colour="black")
    )+    labs(y="Weekly Peak Load (MW)",x="Week",
               title="Weekly Peak Alberta Internal Load (MW, 2014-2017)",
               caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  
  
  
    
  png(file="hourly-prices-avg.png", width = 1400, height = 750)
  ggplot(df1, aes(Hour,total_rev/total_gen/year_p_mean,group=Year_ID,colour=Year_ID)) +
    geom_line(data=subset(df1,Year<2017),size=1.5) +
    geom_line(data=subset(df1,Year==2017),size=3) +
    geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+   
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 20, face = "bold"),
      plot.caption = element_text(size = 24, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 24, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 24,face = "bold"),
      axis.text = element_text(size = 24,face = "bold", colour="black")
    )+    labs(y="Deviation in Hourly Average Power Price ($/MWh)",x="Hour",
               title="Hourly Average Energy Prices Compared to Annual Average Price($/MWh, 2004-2017)",
               caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  

  
    
  
  sub_samp<-subset(gen_data, Time > as.Date("2016-01-01"))
  sub_samp<-subset(sub_samp, Time < as.Date("2017-07-28"))
  #sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
  sub_samp<-na.omit(sub_samp)
  sub_samp$month<-month(sub_samp$Time)
  sub_samp$year<-year(sub_samp$Time)
  sub_samp$yearmon<-paste(month(as.Date(gen_data$Time), label = TRUE, abbr = TRUE),year(sub_samp$Time),sep="-")
  
  df1 <- sub_samp %>% group_by(Plant_Type,yearmon) %>% summarise(newvar = sum(gen))
  df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("TRADE","WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))
  
  
  png(file="gen_month.png", width = 1400, height = 750)
  ggplot(df1, aes(Time,newvar))+
  geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
    scale_fill_brewer(palette="Spectral") +
    #scale_fill_viridis(discrete = FALSE,option="A")+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 20, face = "bold"),
      plot.caption = element_text(size = 24, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 24, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 24,face = "bold"),
      axis.text = element_text(size = 24,face = "bold", colour="black")
    )+
    labs(x="Date",y="Hourly Generation (MW)",
         title="Distribution of Hourly Energy Production (MWh, May, 2017)",
         subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  ggplot(data =df1, aes(yearmon,newvar/1000))+ 
    geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
    #facet_grid(. ~ Run_ID)+
    scale_fill_viridis("Generation Type",discrete = TRUE,option="viridis")+
    #scale_x_continuous(breaks=seq(1, 24, 1))+ # Ticks from 0-1200, every 200+
    #scale_fill_brewer(palette="Spectral") +
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(size = 18,face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 12,face = "bold", colour="black")
    )+
    labs(x="Hour",y="Forecast Annual Generation (GWh)",
         title="Forecast Annual Renewable Electricity Production",
         subtitle="Base Case and 30% by '30 Renewable Generation Constraint, 2016-2035",
         caption="Source: University of Alberta School of Business AuroraXMP implementation\nGraph by Andrew Leach")
  
    
  
  
  
  
  


sub_samp<-subset(gen_data, Time > as.Date("2017-06-28"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(newvar = sum(gen))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("TRADE","WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))


png(file="gen_month.png", width = 1400, height = 750)
p <- ggplot(df1, aes(Time,newvar)) 
p + geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
  scale_fill_brewer(palette="Spectral") +
  #scale_fill_viridis(discrete = FALSE,option="A")+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Date",y="Hourly Generation (MW)",
       title="Distribution of Hourly Energy Production (MWh, May, 2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()


sub_samp<-subset(gen_data, Time > as.Date("2017-07-21"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

png(file="gen_week.png", width = 1400, height = 750)
p <- ggplot(df1, aes(Time,newvar))
p + geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+scale_fill_brewer(palette="Spectral") +
  geom_line(aes(Time,ail),size=1.5) +
    theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Date",y="Hourly Generation (MW)",
       title="Distribution of Hourly Energy Production (MWh), July 21-28, 2017",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()

sub_samp<-subset(gen_data, Time > as.Date("2017-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))

#df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

png(file="load_ytd.png", width = 1400, height = 750)
p <- ggplot(df1, aes(Time,ail))
 p+  geom_line(size=1.5) +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Date",y="Alberta Internal Load (MW)",
       title="Hourly Energy Load (MW), 2017",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()


sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-06-19"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)
sub_samp$Date<-date(sub_samp$Time)


df1 <- sub_samp %>% group_by(Plant_Type,Date) %>% summarise(newvar = sum(gen))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

png(file="gen_all.png", width = 1400, height = 750)
p <- ggplot(df1, aes(Date,newvar/1000))
p + geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+scale_fill_brewer(palette="Spectral") +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Date",y="Daily Generation (GWh)",
       title="Daily Average Energy Production (GWh), 2014-present",
       subtitle="Source: AESO Data")
dev.off()




sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-06-19"))
sub_samp<-subset(sub_samp, Plant_Fuel=="GAS")
sub_samp<-subset(sub_samp, ID=="NX02")


sub_samp$Date_ID = as.yearmon(sub_samp$Time)
df1 <- sub_samp %>% group_by(Date_ID) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))

ggplot(df1, aes(month(Date_ID, label=TRUE, abbr=TRUE), 
                +                 total_gen, group=factor(year(Date_ID)), colour=factor(year(Date_ID)))) +
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
  labs(x="Monthly  Power Generation (MWh)",y="",
       title="Time Series of Monthly Total Generation (MWh, 2014-2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")



sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-06-19"))
sub_samp<-subset(sub_samp, Plant_Fuel=="WIND")
sub_samp<-subset(sub_samp, ID=="HAL1")


sub_samp$Date_ID <- as.yearmon(sub_samp$Time)
sub_samp$Week_ID <- factor(str_c(week(sub_samp$Time),year(sub_samp$Time), sep = "_", collapse = NULL))
sub_samp$Week <- week(sub_samp$Time)


df1 <- sub_samp %>% group_by(Week,Date_ID) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
ggplot(df1, aes(Week,total_gen, group=factor(year(Date_ID)), colour=factor(year(Date_ID)))) +
  geom_line(size=1.5)


ggplot(df1, aes(Week,total_rev/total_gen, group=factor(year(Date_ID)), colour=factor(year(Date_ID)))) +
  geom_line(size=1.5)



forwards <- read.xlsx(xlsxFile = "NGX_forwards.xlsx", sheet = 5, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
forwards<-forwards[,-c(3,4,5)]
colnames(forwards)<-c("Trade_Date","Inst_Date","Settle","Volume_MW","Open_Int_MW")
forwards$Inst_Year<-year(forwards$Inst_Date)
forwards$Inst_Month<-month(forwards$Inst_Date)
df1 <- forwards %>% group_by(Trade_Date,Inst_Year) %>% summarise(Settle = mean(Settle),interest=mean(Open_Int_MW),volume=mean(Volume_MW))

today_date<-as.Date("2017-12-31")
today_date<-max(forwards$Trade_Date)
dates<-c(today_date,today_date-years(1),today_date-years(2),today_date-years(3),today_date-years(4))

png<-1
if(png==1)
  png(file="forwards.png", width = 1400, height = 750,res=130,type='cairo')

ggplot(subset(df1,Trade_Date %in% dates)) +
  geom_line(aes(Inst_Year,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date)),size=2)+
  scale_color_brewer("Trade Date",palette = "Set1")+
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
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves (Calendar Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



png<-1
if(png==1)
  png(file="forwards_2017.png", width = 1400, height = 750,res=130,type='cairo')

ggplot(subset(df1,Inst_Year %in% c(2018,2019,2020)& Trade_Date> as.Date("2017-01-01"))) +
  geom_line(aes(Trade_Date,Settle,group=as.factor(Inst_Year),colour=as.factor(Inst_Year)),size=2)+
  #geom_vline(xintercept = as.Date("2017-04-19")+
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2017-04-19"), xmax =as.Date("2017-04-20"),
           ymin = 30, ymax = 60) +
  annotate("text", x = as.Date("2017-04-01"), y = 50, label = "TransAlta\n April 19th\nAnnouncement\n on Sundance\n Shutdowns",size=2.8)+
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2017-12-05"), xmax =as.Date("2017-12-06"),
           ymin = 30, ymax = 60) +
  annotate("text", x = as.Date("2017-11-9"), y = 35, label = "TransAlta\n December 6th\nAnnouncement\n on Sundance\n Shutdowns",size=2.8)+
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2018-1-12"), xmax =as.Date("2018-1-13"),
           ymin = 30, ymax = 60) +
  annotate("text", x = as.Date("2017-12-25"), y = 35, label = "Balancing Pool\n January 12th\nAnnouncement\n on BR5 PPA\nTermination",size=2.8)+
    #geom_vline(data=ev,aes(xtintercept=as.numeric(dt)))
  scale_color_brewer("Calendar Strip Year",palette = "Set1")+
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
  )+    labs(y="Settlement Price ($/MWh)",x="\nTrade Date",
             title="Alberta Power Forward Contract Values (Calendar Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()







  aurora_res <- read.xlsx(xlsxFile = "2014_2017_gen.xlsx", sheet = 6, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  wind_plants<-subset(aurora_res, Fuel== "WND" & Capacity>0)
  
  #hdf<-get_map(location = "alberta", zoom = 6)
  
  #hdf<-get_map(center = c(lon = -116, lat = 48), zoom = 6,maptype = "terrain")
  hdf<-get_googlemap(center = c(lon = -113, lat = 50.9), zoom = 7,size = c(600, 500),)
                
  #ggmap(hdf, extent = "normal")
  
  library(ggrepel)
  wind_plants<-wind_plants[which(is.na(wind_plants$Latitude)==FALSE),]
  wind_plants$Latitude <- as.numeric(as.character(wind_plants$Latitude))
  wind_plants$Longitude <- as.numeric(as.character(wind_plants$Longitude))
  wind_plants$Capacity <- as.numeric(as.character(wind_plants$Capacity))
  #Mac
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="wind_plants_new.png", width = 1400, height = 750,res=130)
  #PC
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file="wind_plants_new.png", width = 1400, height = 750,res=130,type='cairo')
  ggmap(hdf, extent = "normal")+  theme_bw() +
    geom_point(aes(x = Longitude, y = Latitude, size=Capacity),
               data = wind_plants, alpha = .5, color="darkblue")+
    scale_size(range=c(3,12),name="Capacity (MW)")+
    #geom_label_repel(data = wind_plants,
    #                 aes(x = Longitude, y = Latitude, size=4, label = ID.1),show.legend = FALSE) +
    
      theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 20, face = "bold"),
      plot.caption = element_text(size = 20, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 20, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 20,face = "bold"),
      axis.text = element_text(size = 20,face = "bold", colour="black"),
      axis.title.y=element_text(vjust=0)
    )+
    labs(x="Longitude",y="Latitude",
         title="Alberta Wind Power Production Facilities, June 2017",
         subtitle="Source: Various, Map by Andrew Leach")  
  dev.off()

  #I DIDN'T GET THE NEXT PART WORKING
      
  solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  res_data <- read.xlsx(xlsxFile = "YEG_Res_Load.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  sub_samp <- solar_data
  names(sub_samp) <- tolower(names(sub_samp))
  sub_samp$`beam.irradiance.(w/m^2)`<- NULL
  sub_samp$`diffuse.irradiance.(w/m^2)`<- NULL
  sub_samp$`wind.speed.(m/s)`<- NULL
  sub_samp$`plane.of.array.irradiance.(w/m^2)`<- NULL
  sub_samp$`cell.temperature.(c)`<- NULL
  names(sub_samp) <- c("month","day","hour","temperature","dc.output","ac.output")
  sub_samp$hour <- as.numeric(sub_samp$hour)
  sub_samp$hour <- as.factor(sub_samp$hour)
  sub_samp$month <- as.numeric(sub_samp$month)
  sub_samp$month <- as.factor(sub_samp$month)
  sub_samp<- arrange(sub_samp,month,day,hour)
  
  
  df1 <- sub_samp %>% group_by(month,hour) %>% summarise(hourly_gen=sum(ac.output)/1000)
  png(file="solar_gen.png", width = 1400, height = 750)
  ggplot(df1, aes(hour,hourly_gen,group=month,colour=month)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    labs(colour="Month") +
    scale_color_viridis(discrete=TRUE)+  
    theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Hourly Solar System Generation (Monthly total, kWh)",x="Hour of the Day",
             title="Time Series of Hourly Solar Generation (Monthly Total kWh, estimated)",
             subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()
  
  
  df1 <-sub_samp %>% group_by(month) %>% summarise(monthly_gen=sum(ac.output)/1000)

  png(file="solar_monthly_gen.png", width = 1400, height = 750)
  ggplot(df1,aes(month,monthly_gen,group = 1)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    scale_color_viridis(discrete=TRUE)+ 
    expand_limits(y = 0)+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Monthly Solar System Generation (total, kWh)",x="Month",
               title="Time Series of Monthly Solar Generation (Total kWh, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()

  
  
  #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
  sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
  sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
  sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
  sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
  sub_samp<-na.omit(sub_samp)
  sub_samp$month<-month(sub_samp$Time,label=TRUE)
  df1 <- sub_samp %>% group_by(Time,month,Plant_Fuel) %>% summarise(total_gen=sum(gen),test=3,avg_rev=3)
  df1$Date_ID<-df1$month
  df1<-arrange(df1,Date_ID)

  

  #png(file="wind_montly_gen.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
  ggplot(df1,aes(x =total_gen,y=Date_ID,fill=as.factor(test),height=..density..))+
    geom_joy(rel_min_height = 0.01)+
    scale_fill_viridis(discrete = TRUE) +   
    theme_minimal()+theme(
      legend.position = "none",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black"))+    
      labs(y="Density by Month",x="Hourly Power Generation (MW)",
               title="Distribution of Hourly Wind Power Generation, Alberta, 2014-2017",
               subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
   # dev.off()
    
    ggplot(df1,aes(x =total_gen,y=Date_ID))+
      geom_joy(fill=as.factor(month),height=..density..,rel_min_height = 0.01)+
      scale_colour_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black"))+ 
      labs(title="Alberta Electricity Demand",
           subtitle="Hourly load distribution, by year",
           caption="Source: AESO\nChart by Andrew Leach (@andrew_leach)\nR package (ggjoy) by Claus Wilke",
           x="Megawatts",
           y="")+
      theme(legend.position = "none")
    
    
    
    
    
    #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
    sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
    sub_samp<-subset(sub_samp, sub_samp$`Plant_Type`=="COGEN")
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-month(sub_samp$Time,label=TRUE)
    df1 <- sub_samp %>% group_by(Time,month) %>% summarise(total_gen=sum(gen))
    df1$Date_ID<-df1$month
    df1<-arrange(df1,-Date_ID)
    mins<-min(df1$total_gen)
    maxs<-max(df1$total_gen)
    
    png(file="cogen_montly_gen.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
    ggplot(df1,aes(x =total_gen,y=Date_ID,height=..density..,colour=3))+
      #theme_joy() +
      geom_joy(scale=1,fill=3)+
      scale_x_continuous(limits = c(mins,maxs))+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Density by Month",x="Hourly Power Generation (MW)",
                 title="Distribution of Hourly Generation from Cogeneration Units, Alberta, 2014-2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    dev.off()    
    
    #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
    sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
    sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL")
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-month(sub_samp$Time,label=TRUE)
    df1 <- sub_samp %>% group_by(Time,month) %>% summarise(total_gen=sum(gen))
    df1$Date_ID<-df1$month
    df1<-arrange(df1,-Date_ID)
    mins<-min(df1$total_gen)
    maxs<-max(df1$total_gen)
    
    png(file="coal_montly_gen.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
    ggplot(df1,aes(x =total_gen,y=Date_ID,height=..density..,colour=3))+
      #theme_joy() +
      geom_joy(scale=1,fill=3)+
      scale_x_continuous(limits = c(mins,maxs))+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Density by Month",x="Hourly Power Generation (MW)",
                 title="Distribution of Hourly Generation from Coal Units, Alberta, 2014-2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    dev.off() 
    

    #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
    sub_samp<-subset(gen_data, Time > as.Date("2016-01-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2016-12-31"))
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-as.factor(month(sub_samp$Time,label=TRUE))
    #sub_samp<-subset(sub_samp, sub_samp$month=="Jan" | sub_samp$month=="May")
    sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL"|sub_samp$`Plant_Fuel`=="GAS"|sub_samp$`Plant_Fuel`=="HYDRO"| sub_samp$`Plant_Fuel`=="WIND")
    
    
    df1 <- sub_samp %>% group_by(Time,month,Plant_Fuel) %>% summarise(total_gen=sum(gen),total_rev=sum(gen*Price),avg_rev=sum(gen*Price))
    df1$Date_ID<-df1$month

      
    
    png(file="all_montly_test.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
    ggplot(df1,aes(y =total_gen,x=Date_ID))+
      #theme_joy() +
      #geom_joy(rel_min_height = 0.01,aes(colour=Plant_Type,fill=Plant_Type))+
      geom_boxplot(aes(color=Plant_Fuel),fatten = 1.25,lwd=1.45)+coord_flip()+
      geom_text(data = p_meds, aes(x = TYPE, y = med, label = med), 
                size = 3, vjust = -1.5)
      #scale_x_continuous(limits = c(0,6000))+
      scale_colour_viridis(discrete = TRUE,option="viridis")+
      #scale_colour_viridis(discrete = TRUE,option="plasma")+
      #scale_colour_brewer(palette="Set1")+
      labs(colour = "Plant Fuel\nSource") +
      theme_minimal()+theme(
        legend.position = "right",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 16, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Hourly Power Generation (MW)",x="Month",
                 title="Distribution of Hourly Generation from All Units, Alberta, 2016",
                 caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
    
 
   
   #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
   sub_samp<-subset(gen_data, Time > as.Date("2016-01-1"))
   sub_samp<-subset(sub_samp, Time < as.Date("2016-12-31"))
   sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
   sub_samp<-na.omit(sub_samp)
   sub_samp$month<-as.factor(month(sub_samp$Time,label=TRUE))
   
   
   #sub_samp<-subset(sub_samp, sub_samp$month=="Jan" | sub_samp$month=="May")
   sub_samp$renew <- ifelse(sub_samp$Plant_Fuel =="WIND" |sub_samp$Plant_Fuel =="HYDRO"|sub_samp$Plant_Fuel =="OTHER" ,"Renewable", "Fossil Fuel")
   sub_samp$AIL <- as.numeric(sub_samp$AIL)
   
   df1 <- sub_samp %>% group_by(Time,month,renew,AIL) %>% summarise(total_gen=sum(gen),total_rev=sum(gen*Price),share=sum(gen)/mean(AIL)*100)
   df1$Date_ID<-factor(df1$month,levels=rev(levels(df1$month)))
   
   
   
   png(file="renew_montly_test.png", width = 1400, height = 750)
   #ggplot(df1,aes(x =total_gen,y=Date_ID))+
   ggplot(df1,aes(y=share,x=Date_ID))+
     #theme_joy() +
     #geom_joy(rel_min_height = 0.01,aes(colour=Plant_Type,fill=Plant_Type))+
     geom_boxplot(aes(color=renew),fatten = 1.25,lwd=1.45)+coord_flip()+
     scale_colour_viridis(discrete = TRUE,option="viridis")+
     #scale_colour_viridis(discrete = TRUE,option="plasma")+
     #scale_colour_brewer(palette="Set1")+
     labs(colour = "Plant Fuel\nSource") +
     scale_y_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     #scale_x_discrete(breaks=seq(0, 100, 20))+ # Ticks from 0-1200, every 200
     theme_minimal()+theme(
       legend.position = "right",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 16, face = "bold"),
       plot.caption = element_text(size = 16, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 16, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 16,face = "bold"),
       axis.text = element_text(size = 16,face = "bold", colour="black")
     )+    labs(y="Share of Hourly Power Generation (%)",x="Month",
                title="Distribution of Hourly Generation from Renewable vs Fossil Fuel Units, Alberta, 2016",
                caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
   
   png(file="renew_montly_test.png", width = 1400, height = 750)
   #ggplot(df1,aes(x =total_gen,y=Date_ID))+
   ggplot(df1,aes(x =total_gen,y=Date_ID,height=..density..))+
    #ggplot(df1,aes(y=share,x=Date_ID))+
     #theme_joy() +
     geom_joy(rel_min_height = 0.001,aes(fill=as.factor(renew)))+
     #scale_color_viridis()+
     #geom_boxplot(aes(color=renew),fatten = 1.25,lwd=1.45)+coord_flip()+
     scale_colour_viridis(discrete = TRUE,option="viridis")+
     scale_fill_viridis("Plant Fuel Source",discrete = TRUE,option="viridis")+
     #scale_colour_viridis(discrete = TRUE,option="plasma")+
     #scale_colour_brewer(palette="Set1")+
     labs(colour = "Plant Fuel\nSource") +
     #scale_y_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     #scale_x_discrete(breaks=seq(0, 100, 20))+ # Ticks from 0-1200, every 200
     theme_minimal()+theme(
       legend.position = "right",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 16, face = "bold"),
       plot.caption = element_text(size = 16, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 16, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 16,face = "bold"),
       axis.text = element_text(size = 16,face = "bold", colour="black")
     )+    labs(y="Density of Hourly Power Generation",x="Total Hourly Generation (MW)",
                title="Distribution of Hourly Generation from Renewable vs Fossil Fuel Units, Alberta, 2016",
                caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
   
 
   png(file="renew_montly_joy.png", width = 1400, height = 750)
   #ggplot(df1,aes(x =total_gen,y=Date_ID))+
   ggplot(df1,aes(x =share,y=Date_ID,height=..density..))+
     #ggplot(df1,aes(y=share,x=Date_ID))+
     #theme_joy() +
     geom_joy(rel_min_height = 0.001,aes(fill=as.factor(renew)))+
     #scale_color_viridis()+
     #geom_boxplot(aes(color=renew),fatten = 1.25,lwd=1.45)+coord_flip()+
     scale_colour_viridis(discrete = TRUE,option="viridis")+
     #scale_fill_viridis("Plant Fuel Source",discrete = TRUE,option="viridis")+
     scale_fill_viridis("Plant Fuel Source",discrete = TRUE,option="plasma")+
     #scale_colour_viridis(discrete = TRUE,option="plasma")+
     #scale_colour_brewer(palette="Set1")+
     labs(colour = "Plant Fuel\nSource") +
     #scale_y_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     scale_x_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     theme_minimal()+theme(
       legend.position = "right",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 18, face = "bold"),
       plot.caption = element_text(size = 18, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 18, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 18,face = "bold"),
       axis.text = element_text(size = 18,face = "bold", colour="black")
     )+    labs(y="Density of Hourly Power Generation Shares",x="Share of Hourly Total Internal Load (%)",
                title="Distribution of Hourly Generation from Renewable vs Fossil Fuel Units, Alberta, 2016",
                caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
   
     
   
   #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
   sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
   sub_samp<-subset(sub_samp, Time < as.Date("2016-12-31"))
   sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
   sub_samp<-na.omit(sub_samp)
   sub_samp$month<-as.factor(month(sub_samp$Time,label=TRUE))
   #sub_samp<-subset(sub_samp, sub_samp$month=="Jan" | sub_samp$month=="May")
   sub_samp$renew <- ifelse(sub_samp$Plant_Fuel =="WIND" |sub_samp$Plant_Fuel =="HYDRO"|sub_samp$Plant_Fuel =="OTHER" ,"Renewable", "Fossil Fuel")
   sub_samp$AIL <- as.numeric(sub_samp$AIL)
   
   df1 <- sub_samp %>% group_by(Time,Year,renew,AIL) %>% summarise(total_gen=sum(gen),total_rev=sum(gen*Price),share=sum(gen)/mean(AIL)*100)
   df1$Date_ID<-as.factor(df1$Year)
   
   
   
   png(file="renew_year_test.png", width = 1400, height = 750)
   #ggplot(df1,aes(x =total_gen,y=Date_ID))+
   ggplot(df1,aes(y=share,x=Date_ID))+
     #theme_joy() +
     #geom_joy(rel_min_height = 0.01,aes(colour=Plant_Type,fill=Plant_Type))+
     geom_boxplot(aes(color=renew),fatten = 1.25,lwd=1.45)+coord_flip()+
     scale_colour_viridis(discrete = TRUE,option="viridis")+
     #scale_colour_viridis(discrete = TRUE,option="plasma")+
     #scale_colour_brewer(palette="Set1")+
     labs(colour = "Plant Fuel\nSource") +
     scale_y_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     #scale_x_discrete(breaks=seq(0, 100, 20))+ # Ticks from 0-1200, every 200
     theme_minimal()+theme(
       legend.position = "right",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 16, face = "bold"),
       plot.caption = element_text(size = 16, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 16, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 16,face = "bold"),
       axis.text = element_text(size = 16,face = "bold", colour="black")
     )+    labs(y="Share of Hourly Power Generation (%)",x="Month",
                title="Distribution of Hourly Generation from Renewable vs Fossil Fuel Units, Alberta, 2016",
                caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
   
   
   
   
      
    #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
    sub_samp<-subset(gen_data, Time > as.Date("2016-01-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-month(sub_samp$Time,label=TRUE)
    sub_samp<- cbind(sub_samp,model.matrix( ~ Plant_Type, data = sub_samp))
    
    df1 <- sub_samp %>% group_by(Plant_Type,Time,month) %>% summarise(total_gen=sum(gen),avg_rev=sum(gen*Price)/sum(gen))
    df1$Date_ID<-df1$month

    
    
    
    
    #png(file="all_montly_gen.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
    ggplot(df1,aes(x =total_gen,y=Date_ID,fill=as.factor(month),height=..density..))+
      geom_joy(rel_min_height = 0.01)+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black"))+ 
      labs(title="Alberta Electricity Demand",
           subtitle="Hourly load distribution, by year",
           caption="Source: AESO\nChart by Andrew Leach (@andrew_leach)\nR package (ggjoy) by Claus Wilke",
           x="Megawatts",
           y="")+
      theme(legend.position = "none")
    
      
    
    ggplot(df1,aes(x =total_gen,y=Date_ID,height=..density..,colour=3))+
      theme_joy() +
      geom_joy(scale=1,fill=3)+
      scale_x_continuous(limits = c(mins,maxs))+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Density by Month",x="Hourly Power Generation (MW)",
                 title="Distribution of Hourly Generation from All Units, 2016-2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    #dev.off()
    
    mins<-0
    maxs<-60
    
    
    #png(file="all_montly_value.png", width = 1400, height = 750)
    ggplot(df1,aes(x =avg_rev,y=Date_ID,height=..density..,colour=3))+
      theme_joy() +
      geom_joy(scale=1,fill=3)+
      scale_x_continuous(limits = c(mins,maxs))+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Density by Month",x="Hourly Power Revenues ($/MWh)",
                 title="Distribution of Hourly Generation Values from all units, 2016-2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    #dev.off()
    
    
    
    
#duck-ish curves w wind
    #use March wind
    sub_samp<-subset(gen_data, Time > as.Date("2017-03-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2017-03-31"))
    sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-month(sub_samp$Time,label=TRUE)
    sub_samp$AIL<-as.numeric(sub_samp$AIL)
    df1 <- sub_samp %>% group_by(Time) %>% summarise(AIL=mean(AIL),AIL_net=mean(AIL)-sum(gen),AIL_2xnet=mean(AIL)-2*sum(gen),total_gen=sum(gen),avg_rev=(sum(gen*Price)/sum(gen))) 
    df1$avg_rev[df1$avg_rev==NA]<-0
    df1$Hour<-hour(df1$Time)
    df1$Date_ID<-day(df1$Time)
    df1<-arrange(df1,Time)
    
    mins<-min(0)
    maxs<-max(12000)
    

    myPalette <- colorRampPalette(rev(brewer.pal(3, "Set1")))
    sc <- scale_colour_gradientn(colours = myPalette(31), limits=c(1,31))
    myPalette <- colorRampPalette
    
    #Mac
    if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130)
    #PC
    if(R.version$platform ==  "x86_64-w64-mingw32")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130,type='cairo')
    ggplot(df1,aes(Hour,total_gen,group=Date_ID,colour = Date_ID)) +
      geom_line(size=1.5) +
      #sc+
      scale_colour_viridis(option="viridis")+
      scale_y_continuous(breaks=seq(0, 1400, 200))+ # Ticks from 0-1200, every 200
      scale_x_continuous(breaks=seq(0, 23, 2))+ # Ticks from 0-1200, every 200
      expand_limits(y=c(0,1400),x=c(1,24))+
      labs(colour = "Day") +
      theme_minimal()+theme(
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.position = "right",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 14,face = "bold", colour="black")
      )+    labs(y="Hourly wind generation (MW)",x="Hour",
                 title="Hourly Wind Generation Patterns, March 2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    dev.off()
    
    #Mac
    if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130)
    #PC
    if(R.version$platform ==  "x86_64-w64-mingw32")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130,type='cairo')
    ggplot(df1,aes(Hour,AIL_net,group=Date_ID,colour = Date_ID)) +
      geom_line(size=1.5) +
      #sc+
      scale_colour_viridis(option="viridis")+
      scale_y_continuous(breaks=seq(7000, 11000, 2000))+ # Ticks from 0-1200, every 200
      scale_x_continuous(breaks=seq(0, 23, 2))+ # Ticks from 0-1200, every 200
      expand_limits(y=c(6000,11000))+
      labs(colour = "Day") +
      theme_minimal()+theme(
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.position = "right",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 14,face = "bold", colour="black")
      )+    labs(y="Hourly wind generation (MW)",x="Hour",
                 title="Hourly AIL Net of Wind Generation, March 2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    
    dev.off()
  
    
    #Mac
    if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130)
    #PC
    if(R.version$platform ==  "x86_64-w64-mingw32")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130,type='cairo')
    
    ggplot(df1,aes(Hour,AIL,group=Date_ID,colour = Date_ID)) +
      geom_line(size=1.5) +
      #sc+
      scale_colour_viridis(option="viridis")+
      scale_y_continuous(breaks=seq(7000, 11000, 2000))+ # Ticks from 0-1200, every 200
      scale_x_continuous(breaks=seq(0, 23, 2))+ # Ticks from 0-1200, every 200
      expand_limits(y=c(6000,11000))+
      labs(colour = "Day") +
      theme_minimal()+theme(
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.position = "right",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 14,face = "bold", colour="black")
      )+    labs(y="Hourly AIL (MW)",x="Hour",
                 title="Hourly AIL, March 2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    dev.off()      


  load_data <- read.xlsx(xlsxFile = "aeso_load_data.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  load_data$Effective.Date <- as.POSIXct(load_data$Effective.Date * (60*60*24), origin="1899-12-30", tz="GMT")
  load_data <- setNames(load_data,c("Date","AIL"))
  load_data$month <- month(load_data$Date)
  load_data$day <- day(load_data$Date)
  load_data$year <- year(load_data$Date)
  load_data$Date_ID = as.yearmon(load_data$Date)
  load_data<-na.omit(load_data)
  
  df1 <-load_data %>% group_by(Date_ID) %>% summarise(peak_ail=max(AIL))
  
  #png(file="solar_monthly_gen.png", width = 1400, height = 750)
  ggplot(df1,aes(Date_ID,peak_ail,group = 1)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+   
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 20, face = "bold"),
      plot.caption = element_text(size = 24, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 24, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 24,face = "bold"),
      axis.text = element_text(size = 24,face = "bold", colour="black")
    )+    labs(y="Monthly Average Power Price ($/MWh)",x="Month",
               title="Time Series of Generation-Weighted Monthly Average Energy Prices ($/MWh, 2014-2017)",
               subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  
  #dev.off()
  
  df2 <-load_data %>% group_by(year) %>% summarise(peak_ail=max(AIL))
  
  
  
  forecast_data <- read.xlsx(xlsxFile = "aeso_forecasts.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  forecast_data$Date<-mdy_h(forecast_data$Date,tz=tz(now()))
  colnames(forecast_data) <- c("Date", "Forecast.Price.3Hr","Price","DA.Forecast.AIL","AIL","AIL.Diff")
  
  sub_samp<-subset(forecast_data, Date > mdy_h("07/25/2017 23",tz=tz(now())))
  sub_samp<-subset(sub_samp, Date <= mdy_h("07/29/2017 00",tz=tz(now())))
  sub_samp$hour <-hour(sub_samp$Date)
  
  lims <- c(min(sub_samp$Date),max(sub_samp$Date))
  breaks <- make_breaks(min(sub_samp$Date), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  df1<-melt(sub_samp,id=c("Date","hour"),measure.vars = c("Forecast.Price.3Hr","Price"),value.name = "data")  
  df2<-melt(sub_samp,id=c("Date"),measure.vars = c("DA.Forecast.AIL","AIL"),value.name = "data")  
  df1$Date_ID<-paste(day(df1$Date),hour(df1$Date))
  
  png(file="AESO_price.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(Date,data)) +
    geom_line(size=1,aes(group=variable,colour=variable)) +
    
    scale_color_viridis(discrete=TRUE,labels=c("3hr Ahead Forecast","Actual Hourly Pool Price"))+   
    scale_x_datetime(limits = lims,breaks = breaks,labels = date_format("%d/%m %H:00"))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+    labs(y="Energy Price ($/MWh)",x="Hour",
               title="Alberta hourly actual and 3 hour ahead forecast power prices\nJuly 26-29th, 2017",
               caption="Source: AESO Data, Accessed via NRGStream\n Graph by Andrew Leach")  
  dev.off()  
  
  
  png(file="AESO_load.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df2,aes(Date,data)) +
    geom_line(size=1,aes(group=variable,colour=variable)) +
    
    scale_color_viridis(discrete=TRUE,labels=c("Day Ahead Forecast","Actual Hourly Demand"))+   
    scale_x_datetime(limits = lims,breaks = breaks,labels = date_format("%d/%m %H:00"))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+    labs(y="Alberta Internal Load (MW)",x="Hour",
               title="Alberta hourly actual and day-ahead forecast power demand\nJuly 26-29th, 2017",
               caption="Source: AESO Data, Accessed via NRGStream\n Graph by Andrew Leach")  
  dev.off()  
  
  
  #Time Zone is Canada/Mountain
  sub_samp<-subset(gen_data, Time > mdy_h("07/22/2017 00",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("07/29/2017 00",tz="GMT"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)

  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  

    
  #take out totals, CR, FCASTS, etc
    sub_samp<-sub_samp[-grep("TOTAL", sub_samp$Plant_Type), ]
    sub_samp<-sub_samp[-grep("WIND_FCAST", sub_samp$Plant_Type), ]
    sub_samp<-sub_samp[-grep("CR", sub_samp$Plant_Type), ]
  
    #group gen into types
  #sub_samp<-na.omit(sub_samp)
  df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(gen.MWh = sum(gen),ail=mean(AIL),price=mean(Price))
  
  
  df1$Plant_Type <- factor(df1$Plant_Type, levels=c("COAL","COGEN", "HYDRO", "NGCC",  "OTHER" ,"SCGT"  ,"TRADE", "WIND"))
  #reorder
  df1$Plant_Type = factor(df1$Plant_Type,levels(df1$Plant_Type)[c(8,1,2,3,4,5,6,7)])
  
  #HERE  
  #png(file="gen_summer_peak.png", width = 1400, height = 750)
  ggplot(df1,aes(Time,gen.MWh)) + 
    #geom_area(data =subset(df1,!grepl("TRADE", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =subset(df1,grepl("TRADE", df1$Plant_Type)), aes(Time,-gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    geom_area(data =df1, aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type), position = 'stack',size=1.5)+
    geom_line(data =df1, aes(Time,ail,colour="AIL"),size=2)+
    
    scale_colour_viridis(discrete = TRUE,option="magma",labels=c("Alberta Demand"))+
    
    #scale_fill_brewer(type = "seq", palette = "Set1", direction = 1,labels=c(levels(df1$Plant_Type)))+
    scale_fill_viridis(discrete = TRUE,option="viridis",labels=c(levels(df1$Plant_Type)))+
    
    #scale_colour_brewer(type = "seq", palette = 1, direction = 1,labels=c("AIL"))+
    scale_x_datetime(limits = lims,breaks = breaks,labels = date_format("%d/%m %H:00"))+
    theme_minimal()+theme(
      legend.position = "right",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+
    labs(x="Date",y="Hourly Generation (MW)",
         title="Distribution of Hourly Energy Production, July 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  #dev.off()
  
 
  sub_samp<-subset(gen_data, Time > mdy_h("07/26/2017 08",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("07/26/2017 22",tz="GMT"))
  sub_samp<-arrange(sub_samp,AESO_Name)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='2 hour', length.out=length(sub_samp))
  
  #Grab just the coal units
  sub_samp<-sub_samp[grep("COAL", sub_samp$Plant_Type), ]
  #sub_samp<-sub_samp[-grep("GN3", sub_samp$ID), ]
  #sub_samp<-sub_samp[-grep("HRM", sub_samp$ID), ]
  #sub_samp<-sub_samp[-grep("BR3", sub_samp$ID), ]
  #sub_samp<-sub_samp[grep("Gen", sub_samp$AESO_Name), ]
  down<-"SD6|SD5|SD4|SD1|SD2|KH2"
  sub_samp<-sub_samp[grep(down, sub_samp$AESO_Name), ]
  
  
  #Down or off HRM BR5 SD6 SD5 SD4 SD1 #SD2 KH2
  #Up KH1 BR4 BR5
  #stable GN1,2,3
  
  sub_samp<-arrange(sub_samp,AESO_Name)
  
  #group gen into types
  #sub_samp<-na.omit(sub_samp)
  
  df1 <- sub_samp
  sub_samp<-subset(gen_data, Time > mdy_h("07/26/2017 08",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("07/26/2017 22",tz="GMT"))
  
  df2<-sub_samp %>% group_by(Plant_Type,Time) %>% summarise(gen.MWh = sum(gen),ail=mean(AIL),price=mean(Price))
  df2<-df2[grep("WIND", df2$Plant_Type), ]
  df2<-df2[-grep("FCAST", df2$Plant_Type), ]
  
  
  
  png(file="coal_summer_peak.png", width = 1400, height = 750)
  
  #lims <- c(mdy_h("07/27/2017 00",tz=tz(now())),mdy_h("07/27/2017 23",tz=tz(now())))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='2 hour', length.out=length(sub_samp))
  ggplot(df1,aes(Time,gen)) + 
    #geom_area(data =subset(df1,!grepl("TRADE", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =subset(df1,grepl("COAL", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =df1, aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type), position = 'stack',size=1.5)+
    geom_line(data =df1, aes(Time,gen,colour=AESO_Name,group=AESO_Name),size=1.5)+
    geom_line(data =df1, aes(Time,Price,colour="Alberta Power Price"),size=1.5)+
    geom_line(data =df2, aes(Time,gen.MWh,colour="Wind Generation"),size=1.5)+
    #geom_line(data =df1, aes(Time,AIL/100),size=1.5)+
    
    scale_colour_viridis(discrete = TRUE,option="viridis")+
    
    #scale_fill_brewer(type = "seq", palette = "Set1", direction = 1,labels=c("Coal"))+
    #scale_fill_viridis(discrete = TRUE,option="viridis",labels=c("Coal"))+
    
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+
    labs(x="Date",y="Hourly Generation (MW) or Price ($/MWh)",
         title="Drops in production during price-capped hours, July 26,2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
   
  png(file="ail_summer_peak.png", width = 1400, height = 750)
  
  #lims <- c(mdy_h("07/27/2017 00",tz=tz(now())),mdy_h("07/27/2017 23",tz=tz(now())))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='2 hour', length.out=length(sub_samp))
  ggplot(df1,aes(Time,gen)) + 
    #geom_area(data =subset(df1,!grepl("TRADE", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =subset(df1,grepl("COAL", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =df1, aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type), position = 'stack',size=1.5)+
    #geom_line(data =df1, aes(Time,gen,colour=AESO_Name,group=AESO_Name),size=1.5)+
    #geom_line(data =df1, aes(Time,Price,colour="Alberta Power Price"),size=1.5)+
    #geom_line(data =df2, aes(Time,gen.MWh,colour="Wind Generation"),size=1.5)+
    geom_line(data =df1, aes(Time,AIL,colour="Alberta Internal Load"),size=1.5)+
    
    scale_colour_viridis(discrete = TRUE,option="viridis")+
    
    #scale_fill_brewer(type = "seq", palette = "Set1", direction = 1,labels=c("Coal"))+
    #scale_fill_viridis(discrete = TRUE,option="viridis",labels=c("Coal"))+
    
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+
    labs(x="Date",y="Hourly Demand (MW)",
         title="Alberta Demand during price-capped hours, July 26,2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  
  
   
  
  
  
  sub_samp<-subset(gen_data, Time >= as.Date("2017-11-01"))
  sub_samp<-subset(sub_samp, Time <= as.Date("2017-11-15"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  #select wind forecasts
  #sub_samp<-sub_samp[grep("AB - Wind", sub_samp$NRG_Stream), ]
  #remove 7 day capacility
  #sub_samp<-sub_samp[-grep("Capability", sub_samp$NRG_Stream), ]
  #sub_samp<-sub_samp[-grep("Most Likely", sub_samp$NRG_Stream), ]
  #df1<-sub_samp[-grep("Wind Generation Unit Net", sub_samp$NRG_Stream), ]
  df2<-sub_samp[grep("WIND",sub_samp$Plant_Fuel), ]
  df2<-df2[-grep("WIND_", df2$ID), ]
  
  #reverse order
  df2$AESO_Name = with(df2, factor(AESO_Name, levels = rev(levels(AESO_Name))))
  
 
  #myPalette <- colorRampPalette(rev(brewer.pal(9, "Reds")))
  #df2<-head(df2,1000)
  #subset(sub_samp, grepl("12 Hr", sub_samp$NRG_Stream)) 
  p<-ggplot(df2, aes(AESO_Name,gen/Capacity*100,fill=(gen/Capacity*100))) +
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,gen/Capacity*100,fill=(gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100,fill=(66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100,fill=(33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100),frame = Time))+
    
      coord_flip()+
    ggtitle("Alberta Wind Power Capacity Factor\n")+
    scale_fill_gradient2("Capacity Factor",low = muted("green"), mid = "yellow",
                           high = "red", midpoint = 50)+
    theme_minimal()+theme(
      legend.position = "none",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 11, colour="black"),
      axis.text.x=element_text(angle=0)
    )+
    labs(x="",y="Hourly Capacity Factor (%)",
         #title="Alberta Wind Generation, October 1-15, 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  
  animation::ani.options(interval = .01)
  gganimate(p, title_frame = TRUE,"capacity.gif")
  
  #subset(sub_samp, grepl("12 Hr", sub_samp$NRG_Stream)) 
  p<-ggplot(df2, aes(AESO_Name,gen,fill=(gen/Capacity*100))) +
    #facet_wrap(~ dataset, nrow = 3) +
    #geom_bar(stat="identity", position="dodge",aes(frame = Time))+
    
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,gen,fill=(gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,Capacity*.66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen,fill=(66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,Capacity*.33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen,fill=(33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100),frame = Time))+
    ggtitle("Alberta Wind Power Generation and Capacity Factor\n")+
    
    
    #scale_colour_viridis(discrete = TRUE,option="viridis")+
    #scale_fill_brewer(type = "seq", palette = "Set1", direction = 1,labels=c("Coal"))+
    #scale_fill_viridis(discrete = TRUE,option="magma")+
    scale_fill_gradient2("Capacity\nFactor",low = muted("green"), mid = "yellow",
                         high = "red", midpoint = 50)+
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "right",
      legend.title=element_text(colour="black", size = 16, face = "bold"),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 11, colour="black"),
      axis.text.x=element_text(angle=90)
    )+
    labs(x="",y="Hourly Generation (MW)",
         #title="Alberta Wind Generation, November 1-15, 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  #dev.off()
  
  animation::ani.options(interval = .01)
  gganimate(p, title_frame = TRUE,"generation.gif")
  
  
  sub_samp<-subset(gen_data, Time >= as.Date("2017-07-26"))
  sub_samp<-subset(sub_samp, Time < as.Date("2017-07-27"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  #select wind forecasts
  #sub_samp<-sub_samp[grep("AB - Wind", sub_samp$NRG_Stream), ]
  #remove 7 day capacility
  #sub_samp<-sub_samp[-grep("Capability", sub_samp$NRG_Stream), ]
  #sub_samp<-sub_samp[-grep("Most Likely", sub_samp$NRG_Stream), ]
  #df1<-sub_samp[-grep("Wind Generation Unit Net", sub_samp$NRG_Stream), ]
  #sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="GAS")
  #df2<-sub_samp
  df2<-sub_samp[grep("COAL", sub_samp$Plant_Fuel), ] #coal plants
  #df2<-df2[-grep("WIND_", df2$ID), ] #take out the wind forecasts
  #df2<-df2[-grep("TOTAL", df2$Plant_Type), ]
  
  #reverse order
  df2$AESO_Name = with(df2, factor(AESO_Name, levels = rev(levels(AESO_Name))))
  
  p<-ggplot(df2, aes(AESO_Name,gen,fill=(gen/Capacity*100))) +
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,gen/Capacity*100,fill=(gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100,fill=(66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100,fill=(33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100),frame = Time))+
    coord_flip()+
    ggtitle("Alberta Coal Power Generation and Capacity Factor\n")+
    scale_fill_gradient2(low = muted("green"), mid = "yellow",
                         high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "right",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 12, colour="black"),
      axis.text.x=element_text(angle=0)
    )+
    labs(x="",y="Hourly Capacity Factor (%)",
         title="Alberta Coal Capacity Factors, July 26, 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  
  animation::ani.options(interval = .5,ani.width = 800, ani.height = 400)
  gganimate(p, title_frame = TRUE,"coal_gen.gif")
  
 
  sub_samp<-subset(gen_data, Time > mdy_h("1/1/2004 00",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("10/15/2017 23",tz="GMT"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  df2<-sub_samp[grep("COAL", sub_samp$Plant_Fuel), ] #coal plants
  df2<-na.omit(df2)
  df2$month<-month(df2$Time)
  df2$year<-year(df2$Time)
  df2$year_mon<-as.Date(as.yearmon(df2$Time))
  df2 <- df2 %>% group_by(year_mon,AESO_Name) %>% summarise(cap_fac=100*mean(gen/Capacity))
  
  #df2<-df2[-grep("WIND_", df2$ID), ] #take out the wind forecasts
  #df2<-df2[-grep("TOTAL", df2$Plant_Type), ]
  
  #reverse order
  df2$AESO_Name = with(df2, factor(AESO_Name, levels = rev(levels(AESO_Name))))
  
    ggplot(df2, aes(year_mon,cap_fac),group=AESO_Name) +
    geom_line(stat="identity",aes(group=AESO_Name))+
      #facet_grid(. ~ AESO_Name, margins = TRUE)+
      facet_wrap(~ AESO_Name)+
    ggtitle("Alberta Coal Power Generation and Capacity Factor\n")+
    scale_fill_gradient2(low = muted("green"), mid = "yellow",
                         high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "right",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 8,face = "bold"),
      axis.text = element_text(size = 8, colour="black"),
      axis.text.x=element_text(angle=0)
    )+
    labs(x="",y="Average Monthly Capacity Factor (%)",
         title="Alberta Coal Capacity Factors, 2004-2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  
    sub_samp<-subset(gen_data, Time > mdy_h("1/1/2004 00",tz="GMT"))
    sub_samp<-subset(sub_samp, Time <= mdy_h("10/15/2017 23",tz="GMT"))
    sub_samp<-arrange(sub_samp,Time,NRG_Stream)
    
    lims <- c(min(sub_samp$Time),max(sub_samp$Time))
    breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
    
    df2<-sub_samp
    df2<-na.omit(df2)
    df2$month<-month(df2$Time)
    df2$year<-year(df2$Time)
    df2$year_mon<-as.Date(as.yearmon(df2$Time))
    df2 <- df2 %>% group_by(year_mon,Plant_Type) %>% summarise(cap_fac=100*mean(gen/Capacity))
    
    #df2<-df2[-grep("WIND_", df2$ID), ] #take out the wind forecasts
    #df2<-df2[-grep("TOTAL", df2$Plant_Type), ]
    
    #reverse order
    #df2$AESO_Name = with(df2, factor(AESO_Name, levels = rev(levels(AESO_Name))))
    
    ggplot(df2, aes(year_mon,cap_fac),group=Plant_Type) +
      geom_line(stat="identity",aes(group=Plant_Type))+
      #facet_grid(. ~ AESO_Name, margins = TRUE)+
      facet_wrap(~ Plant_Type)+
      ggtitle("Alberta Power Generation Capacity Factor by Plant Type\n")+
      scale_fill_gradient2(low = muted("green"), mid = "yellow",
                           high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
      #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
      #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
      #expand_limits(x = lims)+
      theme_minimal()+theme(
        legend.position = "right",
        #legend.title=element_blank(),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 16, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 8,face = "bold"),
        axis.text = element_text(size = 8, colour="black"),
        axis.text.x=element_text(angle=0)
      )+
      labs(x="",y="Average Monthly Capacity Factor (%)",
           title="Alberta Coal Capacity Factors, 2004-2017",
           caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
    
  
  
   
  
  #Wind Capacity Factor by Plant, July
  
  sub_samp<-subset(gen_data, Time > mdy_h("07/1/2011 00",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("07/31/2011 23",tz="GMT"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  #select wind forecasts
  #sub_samp<-sub_samp[grep("AB - Wind", sub_samp$NRG_Stream), ]
  #remove 7 day capacility
  #sub_samp<-sub_samp[-grep("Capability", sub_samp$NRG_Stream), ]
  #sub_samp<-sub_samp[-grep("Most Likely", sub_samp$NRG_Stream), ]
  #df1<-sub_samp[-grep("Wind Generation Unit Net", sub_samp$NRG_Stream), ]
  #sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="GAS")
  #df2<-sub_samp
  df2<-sub_samp[grep("WIND", sub_samp$Plant_Fuel), ] #wind plants
  df2<-df2[-grep("WIND_FCAST", df2$Plant_Type), ] #wind plants
  df1 <- df2 %>% group_by(AESO_Name) %>% summarise(gen.MWh = sum(gen),avg_rev=sum(gen*Price)/sum(gen),cap_fac=100*mean(gen/Capacity))
  df1<-melt(df1,id=c("AESO_Name"),measure.vars = c("cap_fac","avg_rev"))
  df1$variable <- factor(df1$variable, labels = c("Capacity Factor (%)", "Average Revenue ($/MWh)"))
  
  png(file="july_wind.png", width = 1400, height = 750,res=130,type='cairo') 
  ggplot(df1, aes(AESO_Name,value,fill=variable)) +
    geom_bar(stat="identity", position="dodge")+
    coord_flip()+
    ggtitle("Alberta Wind Plant Capacity Factor\n")+
    facet_grid(. ~ variable,scales = "free")+
    #scale_fill_gradient2(low = muted("green"), mid = "yellow", high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
    #scale_fill_viridis(discrete=TRUE,option = "magma")+
    scale_fill_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "none",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 12, colour="black"),
      axis.text.x=element_text(angle=0)
    )+
    labs(x="",y="",
         title="Alberta Wind Plant Average Revenue and Capacity Factors, July 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()  
  
  
  
#AURORA WIND OUTAGE DATA
  
  sub_samp<-gen_data[grep("WIND", gen_data$Plant_Fuel), ] #wind plants
  sub_samp<-subset(sub_samp, Time > as.Date("2016-01-01"))
  sub_samp<-subset(sub_samp, Time < as.Date("2016-12-31"))
  sub_samp<-na.omit(sub_samp)
  
  #df2<-df2[-grep("WIND_FCAST", df2$Plant_Type), ] #wind plants
  
  df2<-sub_samp
  df2$month<-month(df2$Time)
  df2$hour<-hour(df2$Time)
  df2$week<-week(df2$Time)
  df2$Year<-year(df2$Time)
  df2$Day<-as.factor(format(as.Date(df2$Time), "%A"))
  
  #df1 <- df2  %>% group_by(AESO_Name,Year) %>% summarise(gen.MWh = sum(gen))
  df1 <- df2 %>% group_by(AESO_Name,ID,month,Day,hour) %>% summarise(gen.MWh = mean(gen),avg_rev=sum(gen*Price)/sum(gen),cap_fac=100*mean(gen/Capacity),outage=100-100*mean(gen/Capacity))
  df1$Day <- ordered(df1$Day,labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  
  #df1 <- df2
  


  
  #data set with monthly outage for 2016 for each of the plants in AB
  plants<-unique(df1$AESO_Name)
  months<-unique(df1$month)
  months<-month.abb[months]
  wind_data <-  as.data.frame(matrix(0, ncol = 170, nrow = 0))
  names(wind_data)<-c("ID","Name",paste("", 1:length(wind_data[,-(1:2)]), sep = ""))
  count<-1
  for(pname in plants){
     plant_data<-df1[grep(pname, df1$AESO_Name)[],c(1,3,9,2)] 
     plant_data$month<-month.abb[plant_data$month]
     for(m_data in months){
        #print(paste("WindShape_Area56_",df2[grep(pname, df2$AESO_Name)[1],7],"_",sep=""))
        v_name<-paste("WindShape_Area56_",count,"_",m_data,sep="")
        p_data<-plant_data[grep(m_data,plant_data$month)[],(3)]
        p_data_m<-(cbind(v_name,paste("Wind data based on ",pname," for ",m_data),t(p_data)))
        #need to write this to data frame
        wind_data[nrow(wind_data)+1,]<-p_data_m
        }
    count<-count+1
    }
  
  #df1<-melt(df1,id=c("AESO_Name"),measure.vars = c("cap_fac","avg_rev"))
  #df1$variable <- factor(df1$variable, labels = c("Capacity Factor (%)", "Average Revenue ($/MWh)"))
for(i in c(3:170)) 
    wind_data[,(i)]<-as.numeric(wind_data[,(i)])
  
 
  
  
   write.xlsx(wind_data, file = "wind_outages.xlsx", colNames = TRUE, borders = "columns")
   
  
   #sub_samp <-subset(gen_data, Time>="2015-01-01")
   #sub_samp[,][is.na(sub_samp[,])] <- 0
   #df1 <- sub_samp %>% group_by(Time,Plant_Type) %>% summarise(Price=mean(Price),Generation=sum(gen),Demand=mean(Demand))
   
   #write.xlsx(df1, file = "luft_data_1.xlsx", colNames = TRUE, borders = "columns")
   #df2<-dcast(df1,Time+Price+Demand~Plant_Type,value.var = c("Generation"))
   #df2$hour<-hour(df2$Time)
   #write.xlsx(df2, file = "luft_data_1.xlsx", colNames = TRUE, borders = "columns")
   
  
  df1 <- df2 %>% group_by(AESO_Name,ID,month) %>% summarise(gen.MWh = mean(gen),avg_rev=sum(gen*Price)/sum(gen),cap_fac=100*mean(gen/Capacity),outage=100-100*mean(gen/Capacity))
  df1$months = factor(month.abb[df1$month], levels = month.abb)
  ggplot(data=df1, aes(AESO_Name,gen.MWh,fill=(cap_fac))) +
    geom_bar(stat="identity", position="dodge")+
    coord_flip()+
    #ggtitle("Alberta Coal Power Generation and Capacity Factor\n")+
    scale_fill_gradient2(low = muted("green"), mid = "yellow",
                         high = "red", midpoint = 35,name="Capacity\nFactor (%)\n")+
    #facet_grid(. ~ Year,scales = "free")+
    facet_grid(. ~ months)+
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    scale_y_continuous(breaks=c(0,75,150))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "right",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 10, colour="black"),
      axis.text.x=element_text(size = 8,angle=0)
    )+
    labs(x="",y="Monthly Average Hourly Generation (MW)",
         title="Alberta Wind Power Generation, 2016",
         #subtitle="30% by 2030 Renewable Generation Constraint",
         caption="Source: AESO Data, Accessed via NRGStream\n Graph by Andrew Leach") 
  
  
  
  
  
   
  
  
  

  
  paired_viridis<-function(pairs){
   palette<-c(viridis(pairs)[1],viridis(pairs)[1])
    if(pairs>1)
      for(c in c(2:pairs))
        palette<-c(palette,viridis(pairs)[c],viridis(pairs)[c])
  return(palette)
  }
  
  sub_samp<-subset(gen_data, Time > mdy_h("07/01/2016 00",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("7/31/2016 23",tz="GMT"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='168 hour', length.out=length(sub_samp))
    
  
  palette_gen_data<-c(paired_viridis(2),"Black")
  png(file="july_wind.png", width = 1400, height = 1500,res=130,type='cairo') 
  ggplot(sub_samp,aes(Time,gen)) + 
      #geom_line(data =subset(sub_samp, (NRG_Stream %in% c("AB - Wind Generation Unit Net MW")),group=NRG_Stream,colour=NRG_Stream)) +
      geom_line(data =subset(sub_samp,(NRG_Stream %in% c("AB - Wind Generation Unit Net MW"))), aes(group=NRG_Stream,colour=NRG_Stream),size=3)+
      geom_line(data =subset(sub_samp, grepl("12 Hr", sub_samp$NRG_Stream)), aes(group=NRG_Stream,colour=NRG_Stream),size=1.5,linetype="dashed")+
      #geom_line(data =subset(sub_samp, grepl("7 Day", sub_samp$NRG_Stream)), aes(group=NRG_Stream,colour=NRG_Stream),size=1.5,linetype="dashed")+    
     scale_colour_manual(values = palette_gen_data)+
     scale_x_datetime(labels = date_format("%d/%m %H:00"))+
     guides(colour = guide_legend(nrow = 2))+
     theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(.1,.1,.1,.1),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+    labs(y="Alberta Wind Generation (MW)",x="Hour",
               title="Alberta hourly actual and forecast wind generation\nJuly 26-29th, 2017",
               caption="Source: AESO Data, Accessed via NRGStream\n Graph by Andrew Leach") 
  dev.off()  
  
  
  
  
  
  
  lto_data <- read.xlsx(xlsxFile = "aesoLTO.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(lto_data)[1] <- "Date"
  
  #lto_data$`2004.Forecast`[is.na(lto_data$`2004.Forecast`)] <- lto_data$Actual[is.na(lto_data$`2004.Forecast`)]
  #lto_data$`2005.Forecast`[is.na(lto_data$`2005.Forecast`)] <- lto_data$Actual[is.na(lto_data$`2005.Forecast`)]
  #for(cname in colnames(lto_data[,grep("20", colnames(lto_data))]))
  # lto_data[is.na(lto_data[,cname]),cname] <- lto_data[is.na(lto_data[,cname]),'Actual']
  #lto_data<-na.omit(lto_data)
  df1<-melt(lto_data,id=c("Date"),measure.vars = c("2004.Forecast","2006.Forecast", "2007.Forecast" ,"2008.Forecast","2009.Forecast","2012.Forecast" , "2014.Reference.Case", "2016.Reference.Case","2017.Reference","Actual"))
  colnames(df1)[2] <- "Forecast"
  colnames(df1)[3] <- "AIL_Est"
  
  png(file="AESO_LTO.png", width = 1400, height = 750,res=130,type='cairo')
  #jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
  ggplot(df1,aes(Date,AIL_Est,group = Forecast,colour=Forecast)) +
    geom_line(size=1.7) +
    #geom_point(size=1) +
    #scale_color_viridis(labels=c("2004 Forecast","2006 Forecast", "2007 Forecast" ,"2008 Forecast","2009 Forecast","2012 Forecast" , "2014 Reference Case", "2016 Reference Case","Actual AIL"),discrete=TRUE)+   
    #scale_colour_brewer(labels=c("2004 Forecast","2006 Forecast", "2007 Forecast" ,"2008 Forecast","2009 Forecast","2012 Forecast" , "2014 Reference Case", "2016 Reference Case","Actual AIL"),type = "seq", palette = "Greens", direction = 1)+
    scale_colour_manual(NULL,labels=c("2004 Forecast","2006 Forecast", "2007 Forecast" ,"2008 Forecast","2009 Forecast","2012 Forecast" , "2014 Reference Case", "2016 Reference Case","2017 Reference Case","Actual AIL"),
                        values=c("#e5f5f9","#ccece6","#99d8c9","#66c2a4","#41ae76","#238b45","#006d2c","#00441b","Black","Grey","Black","Black"))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Alberta Peak Internal Load (MW)",x="Year",
               title="AESO Outlook Forecasts of Alberta Internal Load (MW, 2004-2017)",
               subtitle="Source: AESO Reports. Research: Calder Watrich. Graph: Andrew Leach.")
  
 dev.off()
  
 myplotg<-function (data_sent,label_sent,colour_sent){
   g<-
     ggplot(data_sent,aes(Date,AIL_Est,group = Forecast,colour=Forecast)) +
     geom_line(aes(Date,AIL_Est,group = Forecast,colour=Forecast),size=1.7) +
     #geom_point(size=1) +
     #scale_color_viridis(labels=c("2004 Forecast","2006 Forecast", "2007 Forecast" ,"2008 Forecast","2009 Forecast","2012 Forecast" , "2014 Reference Case", "2016 Reference Case","Actual AIL"),discrete=TRUE)+   
     #scale_colour_brewer(labels=c("2004 Forecast","2006 Forecast", "2007 Forecast" ,"2008 Forecast","2009 Forecast","2012 Forecast" , "2014 Reference Case", "2016 Reference Case","Actual AIL"),type = "seq", palette = "Greens", direction = 1)+
     scale_colour_manual(NULL,labels=c(label_sent),
                         values=c(colour_sent))+
     theme_minimal()+theme(
       legend.position = "bottom",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 14, face = "bold"),
       plot.caption = element_text(size = 16, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 16, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 16,face = "bold"),
       axis.text = element_text(size = 16,face = "bold", colour="black")
     )+    
     labs(y="Alberta Peak Internal Load (MW)",x="Year",
                title="AESO Outlook Forecasts of Alberta Internal Load (MW, 2004-2017)",
                subtitle="Source: AESO Reports. Research: Calder Watrich. Graph: Andrew Leach.")
   
   return (g)
 }  
 

 
 

     
 
  
 
 
 
 #BLAKE CAISO CODE
 
 #set parameters
 queryname <- "SLD_REN_FCST"
 market_run_id <- "ACTUAL"
 startdatetime <- 20160601
 enddatetime <- 20160630
 
 #create URL
 apiURL<-paste("oasis.caiso.com/oasisapi/Singl.",queryname,"&market_run_id=",market_run_id,"&startdatetime=",startdatetime,"T07:00-0000","&enddatetime=",enddatetime,"T07:00-0000","&version=1",sep="")
 
 #download to temp file
 temp<-tempfile()
 download.file(apiURL,temp,method="curl")
 tempdata <- read.table(unzip(temp), sep = ",", header=TRUE)
 unlink(temp) #deletes 'temp' file
 
 
 
 caiso_mar <- read.xlsx(xlsxFile = "caiso_mar_11.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
 df_caiso <- melt(caiso_mar,id=c("Hour"),measure=c("GEOTHERMAL" ,"BIOMASS","BIOGAS","SMALL.HYDRO","WIND.TOTAL","SOLAR.PV","SOLAR.THERMAL"),value.name = "generation") 
 df2_caiso <- melt(caiso_mar,id=c("Hour"),measure=c("RENEWABLES.TOTAL", "NUCLEAR","THERMAL","IMPORTS","HYDRO"),value.name = "generation") 
 
 png(file="caiso_day.png", width = 1400, height = 750)
 ggplot(df_caiso, aes(Hour,generation))+ 
   geom_area(aes(fill=variable), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
   scale_fill_viridis("Generation Type",discrete = TRUE,option="viridis")+
   scale_x_continuous(breaks=seq(1, 24, 2))+ # Ticks from 0-1200, every 200+
   #scale_fill_brewer(palette="Spectral") +
   theme_minimal()+theme(
     legend.position = "right",
     legend.margin=margin(c(0,0,0,0),unit="cm"),
     legend.text = element_text(colour="black", size = 18, face = "bold"),
     plot.caption = element_text(size = 20, face = "italic"),
     plot.title = element_text(face = "bold"),
     plot.subtitle = element_text(size = 20, face = "italic"),
     panel.grid.minor = element_blank(),
     text = element_text(size = 20,face = "bold"),
     axis.text = element_text(size = 20,face = "bold", colour="black")
   )+
   labs(x="Hour",y="Hourly Generation (MWh)",
        title="Distribution of Hourly Renewable Energy Production (MWh), March 11, 2017",
        caption="Source: CAISO Data. Graph by @andrew_leach")
 dev.off()

 df2_caiso$variable <- ordered(df2_caiso$variable,labels = c("RENEWABLES","NUCLEAR","THERMAL","IMPORTS","HYDRO"))
 
 png(file="caiso_day_all.png", width = 1400, height = 750)
 ggplot(df2_caiso, aes(Hour,generation))+ 
   geom_area(aes(fill=variable), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
   scale_fill_viridis("Generation Type",discrete = TRUE,option="viridis")+
   scale_x_continuous(breaks=seq(1, 24, 1))+ # Ticks from 0-1200, every 200+
   #scale_fill_brewer(palette="Spectral") +
   theme_minimal()+theme(
     legend.position = "right",
     legend.margin=margin(c(0,0,0,0),unit="cm"),
     legend.text = element_text(colour="black", size = 18, face = "bold"),
     plot.caption = element_text(size = 20, face = "italic"),
     plot.title = element_text(face = "bold"),
     plot.subtitle = element_text(size = 20, face = "italic"),
     panel.grid.minor = element_blank(),
     text = element_text(size = 20,face = "bold"),
     axis.text = element_text(size = 20,face = "bold", colour="black")
   )+
   labs(x="Hour",y="Hourly Generation (MWh)",
        title="Distribution of Hourly Energy Production (MWh), March 11, 2017",
        caption="Source: CAISO Data. Graph by @andrew_leach")
 dev.off()
 
 
 
 
 
 
 
 #US Hourly Power Demand
 #Northwest EBA.NW-ALL.D.H
 #Bonneville EBA.BPAT-ALL.D.H
 #CAISO EBA.CISO-ALL.D.H
 #MISO EBA.MISO-ALL.D.H
 # PJM EBA.PJM-ALL.D.H
 # NY EBA.NYIS-ALL.D.H
 #NE EBA.ISNE-ALL.D.H
 #ERCOT EBA.ERCO-ALL.D.H
 #US L48 EBA.US48-ALL.D.H
 
 #Net Generation
 #CAISO EBA.CISO-ALL.NG.H
 
 #Net Interchange
 #CAISO EBA.CISO-ALL.TI.H
 
 
 
 grids <-c("NW","CISO","MISO","PJM","NYIS","ERCO","ISNE")
 series <-c("D","NG","TI")
 
 grids <-c("ERCO")
 series <-c("D","NG","TI")
 
 
 
 count<-1
 for(g in grids){
   for(s in series){
     id<-paste("EBA.",g,"-ALL.",s,".H",sep="")
     if(count == 1)
       ids <- id
     if(count > 1)
       ids <- cbind(ids,id)
     count<-count+1
   }
 }
 
 #install.packages("rjson")
 library(rjson)
 X <- read.csv()
 
 #download.file(set_url, "testfile.json", method="auto")
 
 
 series <-c("D","NG","TI")
 
 for(s in series){
   print(paste("Starting loop for",s,sep=" "))
   set_url<-paste("http://api.eia.gov/series/?api_key=",KEY,"&series_id=EBA.CAL-ALL.",s,".H&out=json",sep="")
   data1 <- fromJSON(set_url)
   df_data<-data.frame(data1$series$data)
   df_data$X1<-strptime(df_data$X1,"%Y%Om%dT%HZ",tz = "GMT")
   df_data$X1<-ymd_hms(df_data$X1)
   df_data$X1<-with_tz(df_data$X1, "US/Pacific")
   df_data$X2<-as.numeric(as.character(df_data$X2))
   colnames(df_data) <- c("Date",data1$series$name)
   assign(paste(s,"_data",sep=""),df_data)
 }
 
 #set up date variables
 all_data<-merge(D_data,NG_data,by="Date")
 all_data<-merge(all_data,TI_data,by="Date")
 
 
 #all_data$Date<-as.Date(all_data$Date)
 all_data$month<-month(all_data$Date)
 all_data$year<-year(all_data$Date)
 all_data$day<-day(all_data$Date)
 all_data$hour<-hour(all_data$Date)
 all_data <- transform(all_data, Date_ID = paste(month.abb[month]," ",year,sep=""))
 colnames(all_data)[grep("Demand", colnames(all_data))] <- "Demand"
 colnames(all_data)[grep("generation", colnames(all_data))] <- "NetGen"
 colnames(all_data)[grep("interchange", colnames(all_data))] <- "NetTrade"
 
 save(all_data, file= "caiso.RData")
 
 #load("caiso.RData") ## which is here *equivalent* to
 
 
 sub_samp <-subset(all_data, year==2016)
 sub_samp <-subset(sub_samp, month==5)
 sub_samp <- sub_samp %>% group_by(Date_ID,hour) %>% summarise(ti_avg=mean(NetTrade),gen_avg=mean(NetGen),d_avg=mean(Demand),tot_avg=mean(NetGen-NetTrade))
 sub_samp<-na.omit(sub_samp)
 df1<-melt(sub_samp,id=c("Date_ID","hour"),measure.vars = c("d_avg"))
 
 #png(file="caiso_ramps_july.png", width = 1400, height = 750,res=130,type='cairo')
 ggplot(df1,aes(hour,value,group = factor(Date_ID),colour=factor(Date_ID))) +
   geom_line(size=1.7)+
   scale_y_continuous()+
   #scale_color_viridis(NULL, labels=c("July 2015","Jan 2016","July 2016","Jan 2017", "July 2017"),discrete=TRUE)+   
   scale_color_viridis(NULL,discrete=TRUE)+   
   #scale_x_date() +
   #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
   theme_minimal()+theme(
     legend.position = "bottom",
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
   labs(y="Demand (MW)",x="Hour",
        title="California Demand (MW)",
        subtitle="Source: EIA Data, graph by Andrew Leach.")
 #dev.off()
 
 df1<-melt(sub_samp,id=c("Date_ID","hour"),measure.vars = c("gen_avg"))
 
c
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 # CAISO-oasisAPI-operations.R
 #infoEnergy Demo -- CAISO Data
 
 # 2014 Feb 28  Peter Alstone
 
 # STEP 1: Load Packages
 
 require(ggplot2)  # for nice plots
 require(plyr)     # for data processing
 require(reshape)  # for melting and casting data 
 #(and you actually have to use reshape not reshape2...melting issues...)
 
 #Mac
 if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
   setwd("/Users/aleach/Google Drive")
 #PC
 if(R.version$platform ==  "x86_64-w64-mingw32")
   setwd("C:/Users/aleach/Google Drive")
 print(getwd())
 
 source("CAISO-oasisAPI-operations.R")
 
 
 ISO_TOT_IMP_MW
 
 http://oasis.caiso.com/oasisapi/GroupZip?groupid=ISO_TOT_IMP_MW&
   startdatetime=20130919T07:00-0000&version=1
 
 # STEP 2: Grab Data
 
 # LOADS ~~~~~~~
 load2011 <- getCAISOload(startdate=20170101,enddate=20170102)
 # ...Add dates and melt...
 load2011 <- addDatesCAISO(load2011)
 load2011m <- meltCAISO(load2011,preserve.na=TRUE)
 
 # ...Add more date stuff
 load2011m$dhr<-as.numeric(substr(load2011m$variable,3,4))
 load2011m$yhr <- load2011m$yday*24+load2011m$dhr
 
 
 # STEP 3: Explore Data
 
 # DATAFRAME 1: sys8760------------------------------------------------
 sys8760 <- subset(load2011m,TAC_AREA_NAME=="CA ISO-TAC" & MARKET_RUN_ID =="ACTUAL" & XML_DATA_ITEM=="SYS_FCST_ACT_MW")
 sys8760 <- arrange(sys8760,yhr)
 sys8760$sys.load <- sys8760$value #move values to dedicated sys column
 # clean up superfluous columns
 sys8760$value <- NULL
 sys8760$HE25 <- NULL
 sys8760$variable <- NULL
 sys8760$LABEL <- NULL
 sys8760$XML_DATA_ITEM <- NULL
 
 sys8760 <- addTimeCats(sys8760) #add categories for slicing...
 
 # PLot the Load
 
 casys <- ggplot(sys8760, aes(x=dhr, y=sys.load))
 
 quartz()
 
 casys + geom_point()
 
 casys + geom_point() + geom_smooth()
 
 casys + geom_jitter() + geom_smooth()
 
 casys + geom_jitter(aes(color=season)) + geom_smooth()
 
 casys + geom_jitter() + geom_smooth() + facet_grid(wday ~ monnum)
 
 
 
# library(rgdal) # rgdal library
# drvs <- ogrDrivers() # ogr capabilities
# drvs[grep("Inf", drvs$name),] # test MapInfo reading capability of gdal
# 
# f <- "C:/Users/aleach/Google Drive/SON/11/26_30_we_dos.mid" # filename 
# l <- ogrListLayers(f) # layer name
# sp_object <- readOGR(f, layer = l[[1]]) # load!
# gis_data<-data.frame((sp_object))
 
 
#aeso site scraper

 get_metered_vols_report <- function(start_date, end_date) {
   
   start_date <- as.Date(start_date)
   end_date <- as.Date(end_date)
   
   GET(
     url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/PublicSummaryAllReportServlet",
     query = list(
       beginDate = format(start_date, "%m%d%Y"),
       endDate = format(end_date, "%m%d%Y"),
       contentType = "csv"
     )
   ) -> res
   
   stop_for_status(res)
   
   # Neither the CSV nor HTML output is all that great but the CSV
   # can be made to work with (IMO) less effort than the HTML. You may
   # need to do some extra checks for data format (for either CSV or
   # HTML), though, in "production" mode.
   
   # From what I saw in the output, you likely need to modify 
   # this attempt at munging since the "hours" seem off, but you
   # at least now have the data.
   
   content(res, as="text") %>% 
     stri_split_lines() %>% 
     flatten_chr() -> 
     read.csv(text = paste0(l[8:9], collapse=","),
              header = TRUE, stringsAsFactors=FALSE
     )  %>% janitor::clean_names() %>% 
     tbl_df()
   
 }
 
 
 get_metered_volumes_report <- function(start_date, end_date) {
   
   start_date <- as.Date(start_date)
   end_date <- as.Date(start_date)
   
   GET(
     url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/PublicSummaryAllReportServlet",
     query = list(
       beginDate = format(start_date, "%m%d%Y"),
       endDate = format(end_date, "%m%d%Y"),
       contentType = "csv"
     )
   ) -> res
   
   stop_for_status(res)
   
   # Neither the CSV nor HTML output is all that great but the CSV
   # can be made to work with (IMO) less effort than the HTML. You may
   # need to do some extra checks for data format (for either CSV or
   # HTML), though, in "production" mode.
   
   # From what I saw in the output, you likely need to modify 
   # this attempt at munging since the "hours" seem off, but you
   # at least now have the data.
   
   test<- content(res, as="text") %>% 
     stri_split_lines() %>% 
     flatten_chr()
   #headers<-paste(c(paste(test[8:9], collapse=",")), collapse="\n")
   #headers<- gsub("\"-\",", "", headers)
   data<-gsub("\"-\",", "",paste(c(paste(test[8:9], collapse=","), test[13:length(test)]), collapse="\n"))
   clean_data<-read.csv(text=data,header = TRUE, stringsAsFactors=FALSE)
   clean_data<-janitor::clean_names(clean_data)%>%   tbl_df()
   clean_data$date<-start_date
   #names(clean_data)<-c(read.csv(text=headers,header = FALSE, stringsAsFactors=FALSE),"date")
   return(clean_data)
 }
 
 #xdf<-get_metered_volumes_report(as.Date(day), as.Date(day)+days(1)) 
 
years<-seq(2017,2017)

for(year_id in years){
 days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
 data_store <- data.frame()
 list_item<-1
 for(day in days){
   print(as.Date(day))
   if(as.Date(day)<Sys.Date()-days(3))
     {
     xdf<-get_metered_volumes_report(as.Date(day), as.Date(day)+days(1))
     xdf<-melt(xdf,id.vars = c("pool_participant_id","asset_type","asset_id","date"),variable.name = "hour",value.name = "vol" )
     xdf$hour<-gsub("hour_","",xdf$hour)
     xdf$vol<-as.numeric(xdf$vol)
     data_store<-rbind(data_store,xdf)
     list_item<-list_item+1
     }
   }
 filename<-paste("measured_vols_",year_id,".RData",sep = "")
 save(data_store, file= filename) 
 #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
 #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
}


 sub_samp <- data_store %>% group_by(date,pool_participant_id,asset_type,asset_id) %>% summarise(vol = mean(vol))
 
 df1<-subset(sub_samp,asset_id=="BSR1")
 
 #png(file="caiso_ramps_july.png", width = 1400, height = 750,res=130,type='cairo')
 ggplot(df1,aes(date,vol)) +
   geom_line(size=1.7)+
   scale_y_continuous()+
   #scale_color_viridis(NULL, labels=c("July 2015","Jan 2016","July 2016","Jan 2017", "July 2017"),discrete=TRUE)+   
   scale_color_viridis(NULL,discrete=TRUE)+   
   #scale_x_date() +
   #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
   theme_minimal()+theme(
     legend.position = "bottom",
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
   labs(y="Generation (MW)",x="Hour",
        title="California Net Generation (MW)",
        subtitle="Source: EIA Data, graph by Andrew Leach.")
 #dev.off()
 
 
 
 
 
 
 
 
 
 
 
 
 
 get_merit_report <- function(start_date, end_date,key_firms=NULL) {
   
   start_date <- as.Date(start_date)
   end_date <- as.Date(start_date) #it only takes a start_date so force it like this
  GET(
   url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/MeritOrderSnapshotEnergyReportServlet",
   query = list(
     beginDate = format(start_date, "%m%d%Y"),
     endDate = format(end_date, "%m%d%Y"),
     contentType = "csv"
   )
   ) -> res
  stop_for_status(res)
  test<- content(res, as="text") %>% 
   stri_split_lines() %>% 
   flatten_chr()
  test<-paste(test[3:length(test)], collapse="\n")
  merit_data<-read.csv(text=test,header = TRUE, stringsAsFactors=FALSE)
  clean_data<-janitor::clean_names(merit_data) %>% 
   tbl_df()
  if(!is.null(key_firms))
    {
    clean_data$key_firm<-grepl(paste(key_firms, collapse="|"), clean_data$offer_control)
    clean_data$offer_sum<-ifelse(clean_data$key_firm,clean_data$offer_control,"Other")
    for(firm in key_firms)
      clean_data$offer_sum[grep(firm,clean_data$offer_sum)]<-firm
    clean_data$offer_sum<-ordered(clean_data$offer_sum,levels=c(key_firms,"Other"))
    }
  else
    clean_data$offer_sum<-clean_data$offer_control
  clean_data$date<-as.Date(strptime(clean_data$date, "%m/%d/%Y"))
  clean_data<-arrange(clean_data,he,price,offer_sum)
  clean_data<-within(clean_data, {
    merit <- ave(size, clean_data$he, FUN = cumsum)})
  clean_data<-arrange(clean_data,he,-price,offer_sum)
  return(clean_data)
 }
 
 key_firms<-c("Balancing Pool",
              "TransAlta",
              "ATCO",
              "ENMAX",
              "TransCanada",
              "Capital Power"
 )
 
 years<-seq(2010,2017)
 
 for(year_id in years){
   days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 day")
   data_store <- data.frame()
   list_item<-1
   for(day in days){
     print(as.Date(day))
     if(as.Date(day)<Sys.Date()-days(60))
     {
       xdf<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms)
       data_store<-rbind(data_store,xdf)
       list_item<-list_item+1
     }
   }
   filename<-paste("merit_orders_",year_id,".RData",sep = "")
   save(data_store, file= filename) 
   #filename<-paste("measured_vols_",year_id,".xlsx",sep = "")
   #write.xlsx(data_store, file = filename, colNames = TRUE, borders = "columns") 
 }
 
 merit_data<-data.frame()
 years<-seq(2010,2017)
  for(year_id in years){
    filename<-paste("merit_orders_",year_id,".RData",sep = "")
    load(filename) ## which is here *equivalent* to
    merit_data<-rbind(merit_data,data_store)
  }
 save(merit_data, file="all_merit.RData")  

 load("all_merit.RData")  
 
 
 df1<-subset(merit_data, date>as.Date("2015-01-01") & asset_id=="SD5")
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
 
 
 df1<-subset(merit_data, date>as.Date("2010-01-01") & asset_id=="BR5")
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
 
 
 df1<-subset(merit_data, date>as.Date("2015-01-01") & asset_id=="BR5" & he==18)
 df1$he<-as.numeric(df1$he)
 df1 <- arrange(df1,he,block_number)
 BR5<-data.frame(seq(1,385))
 names(BR5)[1]<-"MW"
  for(day in unique(df1$date)){
    for(asset in unique(subset(df1,date=day)$asset_id)){
      for(hour in subset(df1,date==day & asset_id==asset)$he){
        variable_name<-paste(as.Date(day),"-",hour,sep="")
        BR5[[variable_name]]<-999
        for(block in subset(df1,he==hour & asset_id==asset & date==day)$block_number){
          #print(paste("Asset :",asset,", bids on ",as.Date(day)," ",hour,":00"," block ",block))
          temp_to<-df1$to[df1$date==as.Date(day)&df1$he==hour&df1$asset_id==asset&df1$block_number==block]
          temp_from<-df1$from[df1$date==as.Date(day)&df1$he==hour&df1$asset_id==asset&df1$block_number==block]
          temp_price<-df1$price[df1$date==as.Date(day)&df1$he==hour&df1$asset_id==asset&df1$block_number==block]
          BR5[[variable_name]][temp_to:temp_from]<-temp_price
          }
          }
          }
          }
 
 
 
 
 
 
 
  
  df1<-subset(merit_data, he==18 & date==as.Date("2017-10-05"))
 
 ggplot(df1,aes(merit,price,fill=offer_sum)) +
   #geom_step(mapping=aes(x=merit,y=price))
   geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-20,ymax=price))+
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
        title=paste("Alberta Energy Merit Order, ",max(df1$date)," ",max(df1$he),":00",sep = ""),
        caption="Source: AESO Data, graph by Andrew Leach.")
 
 #load and price data
 
 
 get_forecast_report <- function(start_date, end_date) {
   
   start_date <- as.Date(start_date)
   end_date <- min(as.Date(start_date+months(1)),as.Date(end_date)) #31 days of data
   GET(
     url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/ActualForecastWMRQHReportServlet",
     query = list(
       beginDate = format(start_date, "%m%d%Y"),
       endDate = format(end_date, "%m%d%Y"),
       contentType = "csv"
     )
   ) -> res
   stop_for_status(res)
   test<- content(res, as="text") %>% 
     stri_split_lines() %>% 
     flatten_chr()
   test<-paste(test[5:length(test)], collapse="\n")
   forecast_data<-read.csv(text=test,header = TRUE, stringsAsFactors=FALSE)
   clean_data<-janitor::clean_names(forecast_data) %>% 
     tbl_df()
   clean_data$date<-as.POSIXlt(clean_data$date, format="%m/%d/%Y %H")
   clean_data$he<-hour(clean_data$date)
   clean_data$time<-clean_data$date
   clean_data$date<-as.Date(clean_data$time)
   #clean_data$date<-as.Date(strptime(clean_data$date, "%m/%d/%Y %h"))
   #build in R30 prices and loads?
   return(clean_data)
 }
 

  
xdf<-get_forecast_report("2017-11-01","2017-11-30")

years<-seq(2000,2017)
data_store <- data.frame()
for(year_id in years){
  days<-seq.Date(as.Date(paste(year_id,"-01-01",sep="")),as.Date(paste(year_id,"-12-31",sep="")),by="1 month")
  list_item<-1
  for(day in days){
    print(as.Date(day))
    if(as.Date(day)<Sys.Date()-days(2))
    {
      xdf<-get_forecast_report(as.Date(day), as.Date(day)+months(1))
      data_store<-rbind(data_store,xdf)
      list_item<-list_item+1
    }
  }
}
filename<-paste("forecast_data",".RData",sep = "")
save(data_store, file= filename) 
 
forecast_data<-data_store
#hourly prices  
forecast_data$day_ahead_forecasted_ail<-gsub(",","",forecast_data$day_ahead_forecasted_ail)
forecast_data$actual_ail<-gsub(",","",forecast_data$actual_ail)
forecast_data$forecasted_actual_ail_difference<-gsub(",","",forecast_data$forecasted_actual_ail_difference)
forecast_data$Year<-year(forecast_data$date)
forecast_data$actual_posted_pool_price<-as.numeric(forecast_data$actual_posted_pool_price)
forecast_data$forecast_pool_price<-as.numeric(forecast_data$forecast_pool_price)
forecast_data$actual_ail<-as.numeric(forecast_data$actual_ail)
forecast_data$day_ahead_forecasted_ail<-as.numeric(forecast_data$day_ahead_forecasted_ail)
sub_samp<-forecast_data
sub_samp$time<-NULL
sub_samp<-sub_samp[!is.na(sub_samp$actual_posted_pool_price),]
df1 <- sub_samp %>% group_by(he,Year) %>% summarise(AIL=mean(actual_ail),p_mean=mean(actual_posted_pool_price))

png(file="hourly-prices.png", width = 1400, height = 750)
ggplot(df1, aes(he,p_mean,group=as.factor(Year),colour=as.factor(Year))) +
  geom_line(data=subset(df1,Year<2017),size=1.5) +
  geom_line(data=subset(df1,Year==2017),size=3) +
  geom_point(size=1.5) +
  labs(colour="Year") +
  scale_color_viridis(discrete=TRUE)+   
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+    labs(y="Hourly Average Power Price ($/MWh)",x="Hour",
             title="Hourly Average Energy Prices ($/MWh, 2000-2017)",
             caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()

sub_samp$month<-month(sub_samp$date)
sub_samp$Date_ID = as.yearmon(sub_samp$date)
df1 <- sub_samp %>% group_by(Date_ID) %>% summarise(cons =sum(actual_ail)/1000,AIL =mean(actual_ail),Peak_AIL =max(actual_ail), p_mean=sum(actual_posted_pool_price*actual_ail)/sum(actual_ail))
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
  labs(y="Monthly Average Load (MW)",y="Month",
       title="Time Series of Monthly Average Load (MW, 2004-2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")





png(file="monthly-prices.png", width = 1400, height = 750,type="cairo")
ggplot(data=subset(df1,Date>="2001-01-01"), aes(Date,p_mean,colour="A"),size=2.5) +
  geom_line(size=1.5) +
  geom_line(data=subset(df1,Date>="2001-01-01"),aes(Date,m12_avg,colour="B"),size=2.5) +
  scale_color_viridis("",labels=c("Monthly Average Prices","12 Month Rolling Average Prices"),discrete=TRUE)+   
  scale_x_date(labels = date_format("%h\n%Y"),
               breaks = seq(from = min(as.Date(df1$Date_ID)), to = max(as.Date(df1$Date_ID)+months(12)), by = "18 months"))+
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
  )+    labs(y="Monthly/Annual Average Power Price ($/MWh)",x="\nMonth",
             title="Load-Weighted Monthly Average Energy Prices ($/MWh, 2005-2017)",
             caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()

df1$m12_ail_avg<-as.numeric(rollapply(df1$AIL,12,mean,fill=NA,align = c("right")))

png(file="monthly-loads.png", width = 1400, height = 750,type="cairo")
ggplot(data=subset(df1,Date>="2001-01-01"), aes(Date,AIL,colour="A"),size=2.5) +
  #geom_line(size=1.5) +
  geom_line(data=subset(df1,Date>="2001-01-01"),aes(Date,Peak_AIL,colour="B"),linetype = 1,size=1.5) +
  geom_line(data=subset(df1,Date>="2001-01-01"),aes(Date,m12_ail_avg,colour="C"),size=1.5) +
  scale_color_viridis("",labels=c("Monthly Peak Load","12 Month Rolling Average Load"),discrete=TRUE)+   
  scale_x_date(labels = date_format("%h\n%Y"),
               breaks = seq(from = min(as.Date(df1$Date_ID)), to = max(as.Date(df1$Date_ID)+months(12)), by = "18 months"))+
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

png(file="monthly-cons.png", width = 1400, height = 750,type="cairo")
ggplot(data=subset(df1,Date>="2001-01-01"), aes(Date,cons,colour="A"),size=2.5) +
  geom_line(size=1.5) +
  geom_line(data=subset(df1,Date>="2001-01-01"),aes(Date,m12_cons,colour="C"),size=1.5) +
  scale_color_viridis("",labels=c("Monthly Consumption","12 Month Rolling Average Consumption"),discrete=TRUE)+   
  scale_x_date(labels = date_format("%h\n%Y"),
               breaks = seq(from = min(as.Date(df1$Date_ID)), to = max(as.Date(df1$Date_ID)+months(12)), by = "18 months"))+
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

