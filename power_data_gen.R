library(plyr)
require(dplyr)
require(tidyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(curlconverter)
library(curl)
library(lubridate)
library(viridis)
library(scales)
library(openxlsx)


#create long data file

setwd("C:/Users/aleach/Google Drive")

png<-0 #set to one to output to files, anything else to output to screen.

#curl -i -X POST --data "grant_type=client_credentials&client_id=P8EEG3rvRxyqWd_beUBCJA&client_secret=ki6M5J_hTtiXuMAcHHPmtw" https://api.neur.io/v1/oauth2/token

sensor_id <- "0x0000C47F510354AE"

#
#req <- make_req(straighten())[[1]]

#status check, neurio API
neurio_api <- GET("https://api.neur.io/v1/status")
neurio_r <- fromJSON(rawToChar(neurio_api$content))


#command line for token
#curl -i -X POST -H "Content-Type:application/x-www-form-urlencoded" -d "grant_type=client_credentials&client_id=P8EEG3rvRxyqWd_beUBCJA&client_secret=HWnpmXfqSymeG8Iqjp46Dg" "https://api.neur.io/v1/oauth2/token"
get_token<-function(){
  client_id <- c("P8EEG3rvRxyqWd_beUBCJA")
  client_secret <- c("HWnpmXfqSymeG8Iqjp46Dg")
  neurio.data <- data.frame(client_id,client_secret)
  
  
  # Use authorisation code to get (temporary) access token
  req <- POST(url = "https://api.neur.io/v1/oauth2/token", encode = "form",
              body = list(
                client_id = neurio.data$client_id,
                client_secret = neurio.data$client_secret,
                redirect_uri = "https://api.neur.io/v1/oauth2/token",
                grant_type = "client_credentials"))
  raise <- content(req, as="text")
  #convert to list using RJSONIO
  fromJSON(raise) -> token_data
  return(token_data$access_token)
  }


simon_token<-function(){
  client_id <- c("woE3Ank7Ty-zKp_QoT-1Yw")
  client_secret <- c("w6dldtcOSIS0QShj_ItL6g")
  neurio.data <- data.frame(client_id,client_secret)
  
  
  # Use authorisation code to get (temporary) access token
  req <- POST(url = "https://api.neur.io/v1/oauth2/token", encode = "form",
              body = list(
                client_id = neurio.data$client_id,
                client_secret = neurio.data$client_secret,
                redirect_uri = "https://api.neur.io/v1/oauth2/token",
                grant_type = "client_credentials"))
  raise <- content(req, as="text")
  #convert to list using RJSONIO
  fromJSON(raise) -> token_data
  return(token_data$access_token)
}




#token_id<-"AIOO-2mxvJ42jWrVuwmFKPzHNmcxady7V7gACklOFPGLXm-D1f4hRR9ECcYRD7vIBZyDhHeYtqw6R2gJZ8F1Gh0ysVyu8q05Xf_y4LlJmoU9q21n_0PtE-z7i3NbGFfYlDYMa-6fLw7YX-ogc61JTG3Lpq3M82SUm_EJKIvsfHIB1g7WNMvDW00MV5Qn6rZaPeDqJnsRVf9_qpry0AznNU_bGXLg5XWrjKky_VaRtJo0Zu2k2fRzlgmq7ZkLPTS6K2S6Wqf_E6b_"




# #call that works, stored in case I mess up the other ones.
 #test<-httr::VERB(verb = "GET", url = "https://api.neur.io/v1/samples/stats?sensorId=0x0000C47F510354AE&start=2017-08-16T10:00:00Z&end=2017-08-17T03:00:00Z&granularity=minutes&frequency=5&timezone=America/Edmonton", 
  #                      httr::add_headers(Authorization = paste("Bearer ",token_id,sep="")), 
  #                      encode = "json")
 #raise <- content(test, as="text")
 #convert to list using RJSONIO
 #fromJSON(raise) -> new
 #testing<-new

get_neurio<-function(start_string,end_string,time_unit,time_freq,sensor_id,token_id)
  {
  if(time_unit=="minutes") #don't allow more than 24 hours of 5 minute data
    end_string<-min(as.POSIXct(end_string),as.POSIXct(start_string)+24*60*60)
  if(time_unit=="hours") #don't allow more than 31 days for hourly data
    end_string<-min(as.POSIXct(end_string),as.POSIXct(start_string)+31*24*60*60)
  if(time_unit=="days") #don't allow more than 92 days for daily data
    end_string<-min(as.POSIXct(end_string),as.POSIXct(start_string)+92*24*60*60)
  
  start_time<-paste(format(with_tz(as.POSIXct(start_string),tz = "UTC"),"%Y-%m-%dT %H:%M:%SZ"),sep="")
  end_time<-paste(format(with_tz(as.POSIXct(end_string),tz = "UTC"),"%Y-%m-%dT %H:%M:%SZ"),sep="")
  api_url = paste("https://api.neur.io/v1/samples/stats?","sensorId=",sensor_id,"&start=",start_time,"&end=",end_time,"&granularity=",time_unit,"&frequency=",time_freq,"&timezone=America/Edmonton",sep="")
  
  test<-httr::VERB(verb = "GET", url = api_url, 
                   httr::add_headers(Authorization = paste("Bearer ",token_id,sep="")), 
                   encode = "json")
  neurio_rate<<-test$headers$`ratelimit-remaining`
  #test<-httr::VERB(verb = "GET", url = "https://api.neur.io/v1/samples/stats?sensorId=0x0000C47F510354AE&start=2017-08-16T19:35:00Z&end=2017-08-17T05:51:08Z&granularity=minutes&frequency=5&timezone=America/Edmonton", 
  #                 httr::add_headers(Authorization = "Bearer AIOO-2nAWDW89l_OJHK4wnuOKi_paoTLxKREWXZQuSLpefCSAhfxbeIslCQyQ8VXBiyvfRq3mlXOFLOu8clU31RN5l6B9YayQlP35VV12jtmrfueoWfabUfNtRl4HwNoNqeKxnxnlp7gl4zp-c4scD40OF-yZQgMZvf85Fzk7NXqycVBRC1EX7zjdKqjLyQsHRa2XhRzfgZSIxFMBcjZI-Rr_m6xq3dP3Tufp6rlaEVoViVyp24I3YCu9ndBSdg3fRUtyRN7nKgz"), 
  #                 encode = "json")
  #neurio_rate<<-test$headers$`ratelimit-remaining`
  raise <- content(test, as="text")
  #convert to list using RJSONIO
  fromJSON(raise) -> new
  testing<-new
  #convert from watt-seconds to kWh
  if(time_unit=="minutes")
    {
    new$consumptionEnergy=new$consumptionEnergy/1000/60/time_freq
    new$generationEnergy=new$generationEnergy/1000/60/time_freq
    new$importedEnergy=new$importedEnergy/1000/60/time_freq
    new$exportedEnergy=new$exportedEnergy/1000/60/time_freq
    }
  # watt-second * 1kW/1000W * 1 min/60 seconds * 1 hour/60 mins
  if(time_unit=="hours")
    {
    new$consumptionEnergy=new$consumptionEnergy/1000/60/60/time_freq
    new$generationEnergy=new$generationEnergy/1000/60/60/time_freq
    new$importedEnergy=new$importedEnergy/1000/60/60/time_freq
    new$exportedEnergy=new$exportedEnergy/1000/60/60/time_freq
    }
  
  # watt-second * 1kW/1000W * 1 min/60 seconds * 1 hour/60 mins * 1 day/24 hours
  if(time_unit=="days")
    {
    new$consumptionEnergy=new$consumptionEnergy/1000/60/60/24/time_freq
    new$generationEnergy=new$generationEnergy/1000/60/60/24/time_freq
    new$importedEnergy=new$importedEnergy/1000/60/60/24/time_freq
    new$exportedEnergy=new$exportedEnergy/1000/60/60/24/time_freq
    }
  
  new$net_to_grid<-new$importedEnergy-new$exportedEnergy
  new$start<-with_tz(as.POSIXct(new$start,format="%Y-%m-%dT%H:%M:%S.000",tz = "UTC"),"America/Denver")
  new$end<-with_tz(as.POSIXct(new$end,format="%Y-%m-%dT%H:%M:%S.000",tz = "UTC"),"America/Denver")
  return(new)
} #end data routine


make_breaks <- function(strt, hour, interval="day", length.out=31) {
  strt <- as.POSIXlt(strt - 60*60*24)  # start back one day
  strt <- ISOdatetime(strt$year+1900L, strt$mon+1L, strt$mday, hour=hour, min=0, sec=0, tz="America/Denver")
  seq.POSIXt(strt, strt+(1+length.out)*60*60*24, by=interval)
}


#get a token if you need one
token_id<- get_token()

#token_id<- simon_token



#get all available data

#set time units and frequencies
time_unit<-"hours"
time_freq<-12

start_string<-"2017-08-16 12:00:00 MDT"
#start_string<- Sys.time()-24*60*60
end_string<-Sys.time()
##end_string<- Sys.time()

day_start<- as.POSIXct(start_string)
day_end<-as.POSIXct(start_string)+hours(24)
#get initial data
sys_data<-get_neurio(day_start,day_end,time_unit,time_freq,sensor_id,token_id)
day_start<-as.POSIXct(day_start)+hours(24)
day_end<-as.POSIXct(day_end)+hours(24)
#collect other data and stack it.
while(day_start<as.POSIXct(end_string)) {
  new_data<-get_neurio(day_start,day_end,time_unit,time_freq,sensor_id,token_id)
  sys_data<-rbind(sys_data,new_data)
  day_start<-as.POSIXct(day_start)+hours(24)
  day_end<-min(as.POSIXct(end_string), as.POSIXct(day_end)+hours(24))
  }


solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

res_data <- read.xlsx(xlsxFile = "YEG_Res_Load.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

#solar_data$importedEnergy<-(solar_data$`750_load`-solar_data$`AC.System.Output.(W)`)*(solar_data$`750_load`-solar_data$`AC.System.Output.(W)`>0)
#solar_data$exportedEnergy<-(solar_data$`AC.System.Output.(W)`-solar_data$`750_load`)*(solar_data$`AC.System.Output.(W)`-solar_data$`750_load`>0)
solar_data$date<-paste("2017-",solar_data$Month,"-",solar_data$Day," ",solar_data$Hour,":00",sep="")
solar_data$date<-as.POSIXct(solar_data$date,format='%Y-%m-%d %H:%M')

solar_data<-solar_data[(solar_data$date>=start_string&solar_data$date<=end_string),]
test_data<-merge(sys_data,solar_data,by.x="end",by.y="date")





#set time units and frequencies
time_unit<-"hours"
time_freq<-1

start_string<-"2017-10-01 0:00:00 MDT"
#start_string<- Sys.time()-24*60*60
end_string<-"2017-10-22 20:00:00 MDT"
##end_string<- Sys.time()

#get data
sys_data<-get_neurio(start_string,end_string,time_unit,time_freq,sensor_id,token_id)

solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

res_data <- read.xlsx(xlsxFile = "YEG_Res_Load.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

#solar_data$importedEnergy<-(solar_data$`750_load`-solar_data$`AC.System.Output.(W)`)*(solar_data$`750_load`-solar_data$`AC.System.Output.(W)`>0)
#solar_data$exportedEnergy<-(solar_data$`AC.System.Output.(W)`-solar_data$`750_load`)*(solar_data$`AC.System.Output.(W)`-solar_data$`750_load`>0)
solar_data$date<-paste("2017-",solar_data$Month,"-",solar_data$Day," ",solar_data$Hour,":00",sep="")
solar_data$date<-as.POSIXct(solar_data$date,format='%Y-%m-%d %H:%M')

solar_data<-solar_data[(solar_data$date>=start_string&solar_data$date<=end_string),]
test_data<-merge(sys_data,solar_data,by.x="end",by.y="date")




lims <- c(min(sys_data$start),max(sys_data$start))
#breaks<- as.POSIXct(seq.POSIXt(min(sys_data$start),max(sys_data$start), by = "1 month") )

breaks<-seq.POSIXt(min(sys_data$start)+hours(12), max(sys_data$start), by="1 day")

png<-1
if(png==1)
  png(file="actual_vs_avg.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(test_data, aes(end,`750_load`,colour="Predicted Consumption")) +
  geom_line(size=2) +
  geom_line(aes(end,consumptionEnergy,colour="Actual Consumption"),size=2)+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("My House\nConsumption","Average EPCOR\nResidential Load"),discrete = TRUE,option="C")+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a, %m/%d\n%Hh", tz="America/Denver"))+
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
  )+    labs(y="Energy Consumption (kWh)",x="\nTime",
             title="Actual vs Average Household Load (170kWh/week)",
             caption="Source: Consumption data via Neurio API, Average load data via EPCOR\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






sys_data$batt_call<-sys_data$importedEnergy

sys_data$batt_charge<-sys_data$exportedEnergy
sys_data$batt_pwr<-sys_data$importedEnergy
sys_data$net_imp<-sys_data$importedEnergy
rows_data<-nrow(sys_data)

batt_size<-13
batt_in<-5
batt_out<-5
init_charge<-5
sys_data$batt_call[1]<- min(batt_out,sys_data$importedEnergy[1])
sys_data$batt_charge[1]<-init_charge
sys_data$batt_pwr[1]<-min(sys_data$batt_call[1],sys_data$batt_charge[1])
sys_data$net_imp[1]<-sys_data$importedEnergy[row_num]-sys_data$batt_pwr[row_num]
for(row_num in seq(1,rows_data)){
  sys_data$batt_call[row_num]<- min(batt_out,sys_data$importedEnergy[row_num])
  if(row_num==1)
      {
      sys_data$batt_charge[row_num]<-max(0,min(init_charge+min(sys_data$exportedEnergy[row_num],batt_in)-sys_data$batt_call[row_num],batt_size))
      sys_data$batt_pwr[row_num]<-min(sys_data$batt_call[row_num],init_charge)
      }
  else
    {
    sys_data$batt_charge[row_num]<-max(0,min(sys_data$batt_charge[row_num-1]+min(sys_data$exportedEnergy[row_num],batt_in)-sys_data$batt_call[row_num],batt_size))
    sys_data$batt_pwr[row_num]<-min(sys_data$batt_call[row_num],sys_data$batt_charge[row_num-1])
    }
  sys_data$net_imp[row_num]<-sys_data$importedEnergy[row_num]-sys_data$batt_pwr[row_num]
  }
#sys_data$start<-paste("2017-",sys_data$Month,"-",sys_data$Day," ",sys_data$Hour,":00",sep="")
#sys_data$start<-as.POSIXct(sys_data$start,format='%Y-%m-%d %H:%M')


#sys_data<-sys_data[1:1000,]

lims <- c(min(sys_data$start),max(sys_data$start))
#breaks<- as.POSIXct(seq.POSIXt(min(sys_data$start),max(sys_data$start), by = "1 month") )

breaks<-seq.POSIXt(min(sys_data$start)+days(15), max(sys_data$start)-days(15), by="1 month")

png<-1
if(png==1)
  png(file="home_system.png", width = 2800, height = 750,res=130,type='cairo')
ggplot(sys_data, aes(start,consumptionEnergy,colour="Consumption")) +
  geom_line(size=2) +
  geom_line(aes(start,batt_charge,colour="Battery Charge"),size=2)+
  geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  geom_line(aes(start,generationEnergy,colour="Solar/nGeneration"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("\n%m-%y", tz="America/Denver"))+
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
  )+    labs(y="Energy Generation and Purchases (kW)\nBattery Charge (kWh)",x="\nTime",
             title="Household Energy Shapes with a single 13 KW Tesla Powerwall",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





lims <- c(min(sys_data$start),max(sys_data$start))
#breaks<- as.POSIXct(seq.POSIXt(min(sys_data$start),max(sys_data$start), by = "1 month") )

breaks<-seq.POSIXt(min(sys_data$start)+hours(12), max(sys_data$start), by="1 day")


png<-1
if(png==1)
  png(file="delivery.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(sys_data, aes(start,consumptionEnergy-generationEnergy,colour="Consumption"),colour="Consumption") +
  geom_line(size=2) +
  geom_line(aes(start,importedEnergy,colour="Delivered Energy"),size=2)+
  #geom_line(aes(start,net_to_grid,colour="Electricity from grid"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("Net\nEnergy", "Delivered\nEnergy"),discrete = TRUE,option="D")+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
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
  )+    labs(y="Energy (kW)",x="\nTime",
             title="Leach Household Energy Monitor",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()








time_unit<-"minutes"
time_freq<-5

start_string<-"2017-08-21 6:00:00 MDT"
#start_string<- Sys.time()-24*60*60
end_string<-"2017-08-21 18:00:00 MDT"
##end_string<- Sys.time()

#get data
sys_data<-get_neurio(start_string,end_string,time_unit,time_freq,sensor_id,token_id)

day_data<-data.frame(seq(as.POSIXct("2017/8/21"), as.POSIXct("2017/8/22"), "5 mins"))
day_data<-setNames(day_data,c("Time"))

sys_data<-merge(sys_data,day_data,by.x="start", by.y="Time", all=TRUE)


lims <- c(as.POSIXct("2017-08-21 6:00:00 MDT"), as.POSIXct("2017-08-21 17:00:00 MDT"))


breaks <- make_breaks(min(sys_data$start), hour=0, interval='2 hour', length.out=length(sys_data))

png<-0
if(png==1)
  png(file="eclipse.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(sys_data) +
  geom_line(aes(start,generationEnergy,colour="Solar Generation"),size=2) +
  #geom_line(aes(start,generationEnergy,colour="Generation"),size=2)+
  #geom_line(aes(start,net_to_grid,colour="Electricity from grid"),size=2)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2017-08-21 10:24:00 MDT"), xmax =min(as.POSIXct("2017-08-21 12:49:00 MDT")),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.POSIXct("2017-08-21 11:36:00 MDT"), y = 0.5, label = "Solar eclipse\n2017",size=6)+
  scale_color_viridis("",labels = c("Solar Generation"),discrete = TRUE)+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
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
  )+    labs(y="Energy (kW)",x="\nTime",
             title="Andrew Leach's residential rooftop solar generation\nduring the 2017 solar eclipse",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





#load baseline hourly load data



#for each 5 minute block we need
  #probability of dryer
  #probability of toaster
  #probability of iron
  #probability of coffee maker
  #probability of hair dryer

#for each appliance we need a state and a transition matrix 
  #are the running?
  #how long have they been running?
  #what is the probability of them starting to run
  #probabilities could be based on time of day, day of week, or temperature/season

appliance <- function(name_sent,state_sent,date_sent,p_draw){
  #print(paste(name_sent,"at hour",hour(date_sent),"on a",weekdays(date_sent),"in",month.name[month(date_sent)],sep=" "))
  if(state_sent>0) #it's running
      if(state_sent<60) #it should stay running
        return(state_sent+5) #it's still running, it
      else
        return(0) #it's still running, it
  if(state_sent==0)
      return(p_draw*5) #if p_draw is 1, then it returns (1,5)
}



date_start<-as.POSIXct("2017-07-01 0:00",tz="Canada/Mountain")
dryer_state<-0
for(h in  seq(1,36*5)){ #loop over 8760 hours in the year
  dryer_state<-appliance("Dryer",dryer_state,date_start,rpois(1, .1))
  power<-3500*(dryer_state>0)
  date_start<-date_start+60*5
  print(paste(date_start,dryer_state,power,sep = " "))
  }









          
solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
load_data <- read.xlsx(xlsxFile = "minot_load.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
res_data <- read.xlsx(xlsxFile = "YEG_Res_Load.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)


