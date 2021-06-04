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
library(data.table)


#create long data file

setwd("C:/Users/aleach/Google Drive")

#df <- read.xlsx(xlsxFile = "simple_test2.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

time_data <- read.xlsx(xlsxFile = "2014_2017_gen.xlsx", sheet = 4, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
cnames<-time_data[1,]
cnames[1,1]<-"Time"
time_data <- setNames(time_data,cnames)
colnames(time_data)<-gsub("\\.", " ", colnames(time_data)) 

m<-time_data[-(1:8),]
cnames<-time_data[1,]
cnames[1,1]<-"Time"
m <- setNames(m,cnames)

m2 <- melt(m,id=c("Time","Demand","Price","AIL"),variable_name="Plant_ID") 
#m2 <- melt(m,id=c("Time"),variable_name="Plant_ID") 
m2 <- setNames(m2,c("Time","Demand","Price","AIL","ID","gen"))
m2$gen <- as.numeric(as.character(m2$gen))
m2$Time <- as.numeric(as.character(m2$Time))
m2[["Time"]] <- as.POSIXct(m2[["Time"]] * (60*60*24), origin="1899-12-30", tz="GMT")
m2$Year <- year(m2$Time)

m2$Price <- as.numeric(as.character(m2$Price))
m2<-arrange(m2,Time,ID)

m3<-data.frame(t(time_data[(1:8),-1]))
#m3<-setNames(m3,c("ID","AESO_ID","Capacity_MW","Plant_Type","Plant_Fuel","NRG_Stream_Code"))
m3<-setNames(m3,t(time_data[(1:8),1]))
m3$Capacity <- as.numeric(as.character(m3$Capacity))



new_data <- read.xlsx(xlsxFile = "hourly17_test.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE,colNames = TRUE)
#take out periods in column names
colnames(new_data)<-gsub("\\.", " ", colnames(new_data)) 
names(new_data)[names(new_data) == 'AB - Actual Demand Load MW'] <- 'Demand'
names(new_data)[names(new_data) == 'AB - Actual Hourly Pool Price Price CAD/MWh'] <- 'Price'
names(new_data)[names(new_data) == 'AB - Alberta Internal Load Hr Avg MW'] <- 'AIL'


#new_data$Price<-new_data$`AB - Actual Hourly Pool Price Price CAD/MWh`
#new_data$AIL<-new_data$`AB - Alberta Internal Load Hr Avg MW`
#new_data$Demand<-new_data$`AB - Actual Demand Load MW`


new_melt <- melt(new_data,id=c("Time","Demand","Price","AIL"),variable.name="NRG_Stream",value.name = "gen") 
new_melt[["Time"]] <- as.POSIXct(new_melt[["Time"]] * (60*60*24), origin="1899-12-30", tz="GMT")


#merge in AESO and other ID info to get ID tags
test_data<-merge.data.frame(new_melt,m3,by="NRG_Stream",all=TRUE)

test_data<-arrange(test_data,Time,ID)

#Here, you need to check to make sure all have actually merged
#Milner, St. pierre are potential issues with extra periods.
#Should work now but check.


test_data<-test_data[c("Time","gen","Demand","Price","AIL","ID")]
test_data<-arrange(test_data,Time,ID)
test_m2<-m2[c("Time","gen","Demand","Price","AIL","ID")]
test_m2<-arrange(test_m2,Time,ID)

#test_merge<-merge(test_data,test_m2,by=c("Time","ID"),all = TRUE)
test_merge<-rbind(test_m2,test_data)



gen_data<-merge(m3,test_merge)
gen_data$Cap_Fac<-gen_data$gen/gen_data$Capacity
gen_data$Revenue<-gen_data$gen*gen_data$Price
gen_data$Year <- as.numeric(as.character(year(gen_data$Time)))





remove(cnames,df,m,m2,m3)
save(gen_data, file= "gen.RData")

load("gen.RData") ## which is here *equivalent* to


#new data



