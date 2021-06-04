library(plyr)
require(dplyr)
require(tidyr)
require(reshape2)
library(openxlsx)
library(ggplot2)
library(lubridate)
library(zoo)
library(ggmap)
library(ggjoy)
library(viridis)
library(RColorBrewer)


#create long data file

setwd("C:/Users/aleach/Google Drive")

load("gen.RData") ## which is here *equivalent* to

gen_data$AIL<-as.numeric(gen_data$AIL)
gen_data$Price<-as.numeric(gen_data$Price)
gen_data$Demand<-as.numeric(gen_data$Demand)




#merge capacity information


sub_samp<-subset(gen_data, Time > as.Date("2017-05-13"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-05-20"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Type`=="COGEN")

p <- ggplot(sub_samp, aes(Time,gen)) 
p + geom_area(aes(fill= ID), position = 'stack') + labs(y = "Hourly Generation (MW)")+ labs(x = "Date")


p <- ggplot(sub_samp, aes(Time,gen)) + geom_line(aes(colour= Plant_Type), position = 'stack') + labs(y = "Hourly Generation (MW)")+ labs(x = "Date")
p + facet_grid(. ~ Plant_Fuel)


sub_samp<-subset(gen_data, Time > as.Date("2017-05-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-05-31"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Type`=="WIND")

p <- ggplot(sub_samp, aes(Time,gen)) 
p + geom_area(aes(fill= ID), position = 'stack') + labs(y = "Hourly Generation (MW)")+ labs(x = "Date")


sub_samp<-subset(gen_data, Time > as.Date("2017-05-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-05-31"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Type`=="COAL")

p <- ggplot(sub_samp, aes(Time,gen)) 
#p + geom_point(position = 'stack')
p + geom_area(aes(fill= ID), position = 'stack') + labs(y = "Hourly Generation (MW)")+ labs(x = "Date")




sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,Time,Year) %>% summarise(sumcap = sum(Capacity),total_gen=sum(gen),p_mean=mean(Price))
df1$Year_ID=as.character(df1$Year)
# Histogram of Generation Densities
png(file="wind_cdf.png", width = 1400, height = 750)
ggplot(df1,aes(total_gen))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_x_continuous(limits=range(df1$total_gen),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
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
  labs(x="Wind Generation (MW)",y="Cumulative Density",
       title="Distribution of Wind Energy Production by Hour (2014-2017 Avg)",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()

png(file="wind_cap.png", width = 1400, height = 750)
ggplot(df1,aes(total_gen/sumcap*100))+
  stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=3)+
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





sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND" | sub_samp$`Plant_Fuel`=="GAS" |sub_samp$`Plant_Fuel`=="COAL")
sub_samp<-na.omit(sub_samp)




df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
df1$Day <- date(df1$Time)
#df1$Revenue <- df1$total_gen*df1$p_mean
df2 <- df1 %>% group_by(Plant_Type,Day) %>% summarise(capture = sum(total_rev)/sum(total_gen))


png(file="price_capture.png", width = 1400, height = 750)
ggplot(df2,aes(capture),alpha=0.5)+
  geom_density(aes(colour=Plant_Type),size=1.5)+
  scale_x_continuous(limits=c(0,100))+
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
     title="Distribution of Daily Energy Revenues ($/MWh, 2014-2017)",
     subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()




df2 <- df1 %>% group_by(Day) %>% summarise(capture = sum(total_rev)/sum(total_gen))
png(file="price_capture_all.png", width = 1400, height = 750)
ggplot(df2,aes(capture),alpha=0.5)+
  geom_density(aes(),size=1.5)+
  scale_x_continuous(limits=c(0,100))+
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
       title="Distribution of Daily Energy Revenues ($/MWh, 2014-2017)",
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

sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-7-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND" | sub_samp$`Plant_Fuel`=="GAS" |sub_samp$`Plant_Fuel`=="COAL")
sub_samp<-na.omit(sub_samp)
sub_samp$Month_Year <- decimal_date(sub_samp$Time)
sub_samp$Month <- month(sub_samp$Time)
sub_samp$Hour <- hour(sub_samp$Time)
#distribution of hourly prices - duration curves and density functions


df1 <- sub_samp %>% group_by(Time,Year) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
df1$Year_ID=as.character(df1$Year)
#df1$Revenue <- df1$total_gen*df1$p_mean



png(file="price_dist.png", width = 1400, height = 750)
ggplot(df1,aes(total_rev/total_gen))+
  #geom_density(aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  guides(fill = guide_legend(title = "LEFT", title.position = "left"))+
  labs(colour="Year") +
  scale_x_continuous(limits=c(0,75))+
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
  labs(x="Daily Average Power Prices ($/MWh)",y="",
       title="Distribution of Hourly Energy Prices ($/MWh, 2014-2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()


  sub_samp$Date_ID = as.yearmon(sub_samp$Time)
  df1 <- sub_samp %>% group_by(Date_ID) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
  
  png(file="monthly-prices.png", width = 1400, height = 750)
  ggplot(df1, aes(month(Date_ID, label=TRUE, abbr=TRUE), 
                    +                 total_rev/total_gen, group=factor(year(Date_ID)), colour=factor(year(Date_ID)))) +
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
  dev.off()

#hourly prices  
  sub_samp$Date_ID = as.yearmon(sub_samp$Time)
  df1 <- sub_samp %>% group_by(Hour,Year) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
  df1$Year_ID=as.character(df1$Year)
  
  png(file="hourly-prices.png", width = 1400, height = 750)
  ggplot(df1, aes(Hour,total_rev/total_gen,group=Year_ID,colour=Year_ID)) +
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
    )+    labs(y="Hourly Average Power Price ($/MWh)",x="Hour",
               title="Time Series of Generation-Weighted Monthly Average Energy Prices ($/MWh, 2014-2017)",
               subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  
  
  
  
  


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




  aurora_res <- read.xlsx(xlsxFile = "2014_2017_gen.xlsx", sheet = 5, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  wind_plants<-subset(aurora_res, Fuel== "WND" & Capacity>0)
  
  #hdf<-get_map(location = "alberta", zoom = 6)
  
  #hdf<-get_map(center = c(lon = -116, lat = 48), zoom = 6,maptype = "terrain")
  hdf<-get_googlemap(center = c(lon = -113, lat = 50.9), zoom = 7,size = c(600, 500),)
                
  #ggmap(hdf, extent = "normal")
  
  
  wind_plants<-wind_plants[which(is.na(wind_plants$Latitude)==FALSE),]
  wind_plants$Latitude <- as.numeric(as.character(wind_plants$Latitude))
  wind_plants$Longitude <- as.numeric(as.character(wind_plants$Longitude))
  wind_plants$Capacity <- as.numeric(as.character(wind_plants$Capacity))
  png(file="wind_plants.png", width = 1400, height = 750)
  ggmap(hdf, extent = "normal")+  theme_bw() +
  geom_point(aes(x = Longitude, y = Latitude, size=Capacity),
               data = wind_plants, alpha = .5, color="darkblue")+
  scale_size(range=c(3,12),name="Capacity (MW)")+
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
    
    png(file="hourly_wind_load_ramen.png", width = 1400, height = 750,res=130,type='cairo')
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
  
    
    png(file="hourly_AIL_ramen.png", width = 1400, height = 750,res=130,type='cairo')
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
 