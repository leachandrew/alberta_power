library(tidyverse)
library(pdfetch)

KEY <- "91b4dca0b858df64a2279d82f71af240"


eia_fix_dates<-function(data_sent)
{
  data_sent$date=ymd(rownames(data_sent))
  rownames(data_sent)<-NULL
  data_sent[]
}

steo_data_fetch<-function(date_sent){
  #for any month and year, get the STEO Price outlook
  #testing
  #date_sent<-ymd("2019-9-01")
  #month_sent should be lower case month.abb
  
  #convert date to month/year notation used by eia
  month_sent<-tolower(month.abb[month((date_sent))])
  year_sent<-sprintf('%02d', year(date_sent)%% 100)
  
  #before jun2013 they are xls files
  file_date<-ymd(paste(year_sent,month_sent,1,sep="-"))
  excel_file<-ifelse(file_date>ymd("2013-06-01"),
                     paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xlsx"),
                     paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xls"))
  temp_file<-ifelse(file_date>ymd("2013-06-01"),
                    paste0("steo_",month_sent,"_",year_sent,".xlsx"),
                    paste0("steo_",month_sent,"_",year_sent,".xls"))
  download.file(excel_file,mode = "wb",destfile = temp_file)
  dates<-read_excel(path=temp_file,sheet = "2tab",range = "C3:C4",col_names = F)
  names(dates)[1]<-"X__1"
  year_start<-dates$X__1[1]
  month_start<-grep(dates$X__1[2],month.abb)
  
  #process price outlook
  price_outlook<-read_excel(path=temp_file,sheet = "2tab",range = "A5:BV40",na="n/a")
  names(price_outlook)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
  #drop electricity and refined product headers
  price_outlook<-price_outlook[-c(4,26),]
  #rows which could be headers
  headers<-grep("TRUE",is.na(price_outlook[,1]))
  #for each header, the next x rows get a concatenated header
  price_outlook$Header<-NA
  price_outlook$Header[1]<-"Crude Oil"
  for(j in headers){
    #print(price_outlook$Region[j])
    price_outlook$Header[[j]]<-price_outlook$Region[[j]]
  }
  price_outlook<-price_outlook %>% fill(Header)
  price_outlook<-price_outlook[!is.na(price_outlook$code),]
  price_outlook<-melt(price_outlook,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
  price_outlook$table<-"2tab"
  price_outlook<-price_outlook %>% mutate(
    Date=ymd(Date),
    forecast=ifelse(Date>=file_date,1,0),
    version=file_date)
  #file ends up with columns code, Region, Header, Date, value, forecast, version
  
  #process non_opec_supply
  crude_supply_data<-read_excel(path=temp_file,sheet = "3atab",range = "A5:BV47",na="n/a")
  names(crude_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
  crude_supply_data<-crude_supply_data[rowSums(is.na(crude_supply_data)) != ncol(crude_supply_data),]
  headers<-grep("TRUE",is.na(crude_supply_data[,1]))
  #for each header, the next x rows get a concatenated header
  crude_supply_data$Header<-NA
  crude_supply_data$Header[1]<-"Supply (million barrels per day) (a)"
  for(j in headers){
    #print(price_outlook$Region[j])
    crude_supply_data$Header[[j]]<-crude_supply_data$Region[[j]]
  }
  crude_supply_data<-crude_supply_data %>% fill(Header)
  crude_supply_data<-crude_supply_data[!is.na(crude_supply_data$code),]
  crude_supply_data<-melt(crude_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
  crude_supply_data$table<-"3atab"
  crude_supply_data<-crude_supply_data %>% mutate(
    Date=ymd(Date),
    forecast=ifelse(Date>=file_date,1,0),
    version=file_date)
  #file ends up with columns code, Region, Header, Date, value, forecast, version
  
  #process non_opec_supply
  non_opec_supply_data<-read_excel(path=temp_file,sheet = "3btab",range = "A5:BV50",na="n/a")
  names(non_opec_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
  non_opec_supply_data<-non_opec_supply_data[rowSums(is.na(non_opec_supply_data)) != ncol(non_opec_supply_data),]
  non_opec_supply_data$Header<-"Petroleum Supply  (million barrels per day)"
  non_opec_supply_data<-melt(non_opec_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
  non_opec_supply_data$table<-"3btab"
  non_opec_supply_data<-non_opec_supply_data %>% mutate(
    Date=ymd(Date),
    forecast=ifelse(Date>=file_date,1,0),
    version=file_date)
  
  #process opec_data
  opec_supply_data<-read_excel(path=temp_file,sheet = "3ctab",range = "A4:BV55",na=c("n/a","-"))
  names(opec_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
  opec_supply_data<-opec_supply_data[rowSums(is.na(opec_supply_data)) != ncol(opec_supply_data),]
  opec_supply_data$Header<-NA
  headers<-grep("TRUE",is.na(opec_supply_data[,1]))
  #for each header, the next x rows get a concatenated header
  for(j in headers){
    #print(price_outlook$Region[j])
    opec_supply_data$Header[[j]]<-opec_supply_data$Region[[j]]
  }
  opec_supply_data<-opec_supply_data %>% fill(Header)
  opec_supply_data<-opec_supply_data[!is.na(opec_supply_data$code),]
  opec_supply_data<-melt(opec_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
  opec_supply_data$table<-"3ctab"
  opec_supply_data<-opec_supply_data %>% mutate(
    Date=ymd(Date),
    forecast=ifelse(Date>=file_date,1,0),
    version=file_date)
  
  
  #stack everthing
  
  steo_data<-rbind(price_outlook,crude_supply_data,non_opec_supply_data,opec_supply_data)
  
  steo_data
}


#steo_data<-steo_data_fetch(ymd("2018-12-1"))

#get historic data
#steo_data<-steo_data_fetch(ymd("2018-12-1"))

#find current issue of STEO - latest it could be out is the 12th, but let's use the 15th
steo_date<-as.Date(ifelse(day(Sys.Date())>=11,Sys.Date(),Sys.Date()-months(1)))
steo_data0<-filter(steo_data_fetch(steo_date),Date>=ymd("2019-1-01"),forecast==0)
get_history<-0
if(get_history==1)
{
  steo_data1<-filter(steo_data_fetch(ymd("2019-1-1")),Date>=ymd("2015-01-01"),forecast==0)
  #2011-2014 histories
  steo_data2<-filter(steo_data_fetch(ymd("2015-1-1")),forecast==0)
  #2007-2010 histories
  steo_data3<-filter(steo_data_fetch(ymd("2011-1-1")),forecast==0)
  #2004-2007 histories
  steo_data4<-filter(steo_data_fetch(ymd("2008-1-1")),Date<ymd("2007-01-01"),forecast==0)
  steo_history<-rbind(steo_data4,steo_data3,steo_data2,steo_data1)
  save(steo_history,file="steo_history.RData")
}

load("steo_history.RData")
steo_history<-rbind(steo_history,steo_data0)

#add forecasts

steo_forecast<-filter(steo_data_fetch(steo_date),forecast==1)
steo_data<-rbind(steo_history,steo_forecast)




#global supply and demand
#the brackets mess up filter, so this is a fix
supply_demand<-steo_data %>%filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")))
#find forecast dates
min_forecast<-min(supply_demand$Date[supply_demand$forecast==1])
max_forecast<-max(supply_demand$Date[supply_demand$forecast==1])

#historical demand forecasts
steo_old_sd_forecasts<-filter(steo_data_fetch(ymd("2020-1-1")),Date>=ymd("2015-01-01"),forecast==1) %>%
  rbind(filter(steo_data_fetch(ymd("2020-2-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-3-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-4-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))

graph_df<-supply_demand%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(format(max(supply_demand$version), "%b %Y"), "forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))%>%
  bind_rows(steo_old_sd_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")),
         version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                        levels=paste(month.abb[sort(month(unique(version)))],sort(year(unique(version))),"forecast")))


forecast_label<-paste(format(max(supply_demand$version), "%b %Y"), "forecast")
other_versions<-graph_df %>% filter(forecast==1,version!=forecast_label) %>% select(version) %>% unique()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("demand_new.png")
ggplot(filter(graph_df,Region=="Total World Consumption",forecast==0,Date>ymd("2018-01-01")))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.25)+
  geom_line(data=filter(graph_df,Region=="Total World Consumption",forecast==1),
            aes(Date,value,group=version,colour=version,linetype="STEO Forecast"),size=1.25)+
  geom_point(data=filter(graph_df,Region=="Total World Consumption",forecast==1),
             aes(Date,value,group=version,shape=version,colour=version,fill=version),size=2.5)+
  
  #geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y")+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  theme_minimal()+weekly_graphs()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  labs(y="Global Liquids Demand (million barrels per day)",x="",
       title=paste("Global Liquids Demand and EIA Forecasts"),
       subtitle=paste("Historic Values and Short Term Energy Outlook Forecasts"),
       caption="Source: Data via EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


