
#set your directories here
#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/safton/Documents/R/Merit Scripts")
print(getwd())

Sys.setenv("R_ZIPCMD" = "C:/Users/safton/Documents/R/Rtools/bin/zip.exe")

#makes sure these are in the same directory
source("andrew_base.R")
source("aeso_scrapes.R")
if(!exists("generate_func", mode="function")) source("generate_func.R")

library(dplyr)
library(openxlsx)

#load and update merits
update<-1
load("all_merit.RData")  
if(update!=0){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  save(merit_data, file="all_merit.RData")  
}

#check to make sure singles are consistent
singles<-seq(1,9)
for(hour in singles){
  merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
}



#Load temprature data  
temp_data <- read.table('C:/Users/safton/Documents/R/Merit Scripts/Temp Data.txt', header = TRUE)
  temp_data$hourly <- ifelse (temp_data$AM_PM == "PM", temp_data$Hour + 12, temp_data$Hour + 0)

  
#Load price data for hourly price and AECO-C NG prices  
price_data <- read.table('C:/Users/safton/Documents/R/Merit Scripts/historic_prices.txt', header = TRUE)
AECO_price <- read.table('C:/Users/safton/Documents/R/Merit Scripts/ATCOC.txt', header = TRUE)
asset_fuel <- read.table('C:/Users/safton/Documents/R/Merit Scripts/fuel_source.txt', header = TRUE)
fuel_price <- read.table('C:/Users/safton/Documents/R/Merit Scripts/fuel_price.txt', header = TRUE)

#add plant type data into merit data
merit_data <- merge(merit_data, asset_fuel)

#average temp data for Ft Mac, Cgy, Edm 
hour_temp <- aggregate(temp_data$Temp, by=list(temp_data$Month, temp_data$Day, temp_data$Year, temp_data$hourly ), FUN=mean)
  colnames(hour_temp) <- c("month", "day", "year", "hour", "temp")
  hour_temp$temp_dd <- abs(hour_temp$temp - 18) #calculate heating / cooling degree hours

  
#Filter data for undispatched MW, used to determine supply cushion    
weekdays<- c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
cush_data <- merit_data %>% filter(he != "02*" & dispatched == "N")
cush_data <- cush_data %>% mutate(day=day(date), weekday=factor(wday(date),labels=weekdays),he= as.numeric(as.character(he)))%>%
  mutate(month=month(date),year=year(date),weekhour=(wday(date)-1)*24+he)


#create full dates for tables so that table can be merged together
cush_data$full_date <- with(cush_data, paste(year, month, day, he , sep="-"))
hour_temp$full_date <- with(hour_temp, paste(year, month, day, hour , sep="-"))
price_data$full_date <- with(price_data, paste(year, month, day, hour , sep="-"))


#Calculate supply cushion by adding up undispatched MW hourly
sup_cush <- aggregate(cush_data$available_mw, by=list(cush_data$full_date), FUN=sum)
  colnames(sup_cush) <- c("full_date",'cushion')
  
#Calculate the year with the greastest average supply cushion  
year_cush <- aggregate(cush_data$available_mw, by=list(cush_data$year), FUN=sum)

  
#merger tables together and define other variables   
reg_table <- merge(sup_cush, hour_temp)
reg_table <- merge(reg_table, price_data)
reg_table <- merge(reg_table, AECO_price)


#Assign on peak hours as "1" and off peak as "0" for use as dummy variable
reg_table$on_peak <- ifelse(reg_table$hour %in% c(7:23), 1, 0)
#Remove broken down dates to clean up the table

#Remove the 1000 hours with the smallest supply cushion from the data, used for correctiong supply cushion in Aurora models
cut_1000_tight <- reg_table %>% filter(rank(cushion)>=1000)
cut_below_1000 <- reg_table %>% filter(cushion>=1000)


#Take squears of reg_table
reg_table$sqr_temp_dd <-(reg_table$temp_dd)^2
reg_table$sqr_price <-(reg_table$price)^2
reg_table$sqr_demand <-(reg_table$demand)^2
reg_table$sqr_cushion <-(reg_table$cushion)^2
reg_table$sqr_AECO <-(reg_table$AECOPrice)^2
#Take squear roots of reg_table
reg_table$sqrt_temp_dd <-(reg_table$temp_dd)^0.5
reg_table$sqrt_price <-(reg_table$price)^0.5
reg_table$sqrt_demand <-(reg_table$demand)^0.5
reg_table$sqrt_cushion <-(reg_table$cushion)^0.5
reg_table$sqrt_AECO <-(reg_table$AECOPrice)^0.5


#Create natural log filtered table
  ln_reg_table <- reg_table[!(reg_table$temp_dd==0),] #These lines remove the 0's so that taking the natural log doesn't equal -Inf
  ln_reg_table <- ln_reg_table[!(ln_reg_table$price==0),] 
  ln_reg_table <- ln_reg_table[!(ln_reg_table$demand==0),]
  ln_reg_table <- ln_reg_table[!(ln_reg_table$cushion==0),]
  ln_reg_table <- ln_reg_table[!(ln_reg_table$AECOPrice==0),]
ln_reg_table$ln_temp_dd <-log(ln_reg_table$temp_dd)
ln_reg_table$ln_price <-log(ln_reg_table$price)
ln_reg_table$ln_demand <-log(ln_reg_table$demand)
ln_reg_table$ln_cushion <-log(ln_reg_table$cushion)
ln_reg_table$ln_AECOPrice <-log(ln_reg_table$AECOPrice)


#preform regression to determine the segnificance of each variable
fit_dd_cush <- lm(price ~ temp_dd + cushion + demand + AECOPrice + on_peak, data = reg_table) #lin-lin regression
  print(summary(fit_dd_cush))
fit_dd_cush_ln <- lm(ln_price ~ ln_temp_dd + ln_cushion + ln_demand + ln_AECOPrice + on_peak, data = ln_reg_table) #log-log regression
  print(summary(fit_dd_cush_ln))
fit_dd_cush_sqrs <- lm(price ~ sqr_temp_dd + cushion + demand + sqr_AECO + on_peak, data = reg_table) #sqrt-sqrt (except temp_dd)
  print(summary(fit_dd_cush_sqrs))

#Predicted Values
ln_reg_table$fitted <- fitted(fit_dd_cush_ln)
write.xlsx(ln_reg_table, 'C:/Users/safton/Documents/R/Merit Scripts/fitted.xlsx')