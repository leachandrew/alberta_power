#set your directories here
#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/safton/Documents/R/Merit Scripts")
print(getwd())

#Maps location of zip file so that excel documents can be written
Sys.setenv("R_ZIPCMD" = "C:/Users/safton/Documents/R/Rtools/bin/zip.exe")

#makes sure these are in the same directory
source("andrew_base.R")
source("aeso_scrapes.R")
if(!exists("generate_func", mode="function")) source("generate_func.R")

#Needed library, make sure it is loaded
library(dplyr)

#load and update merits
update<-1
load("all_merit.RData")  
if(update!=0){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  save(merit_data, file="all_merit.RData")  
}

# add fuel type to merit data, will need to remap if working on a different computer
asset_fuel <- read.table('C:/Users/safton/Documents/R/Merit Scripts/fuel_source.txt', header = TRUE)
fuel_price <- read.table('C:/Users/safton/Documents/R/Merit Scripts/fuel_price.txt', header = TRUE)

#check to make sure singles are consistent
singles<-seq(1,9)
for(hour in singles){
  merit_data$he[merit_data$he==hour]<-paste(0,hour,sep="")
}

#add plant type data into merit data
merit_data <- merge(merit_data, asset_fuel)


#a new function to process plant/hour data into means by MW
data_proc<-function(data_sent,max_cap){    
  #send it a maximum capacity
  df2<-data.frame(seq(0,max_cap))
  names(df2)[1]<-"MW"
  data_sent<-arrange(data_sent,date,block_number)
  #start at 2 since your filling the second column
  date_count<-2  
  #fill columns for bids by MW
  for(date_id in unique(data_sent$date)) {
    df_test<-data_sent %>% filter(date==date_id)
    if(nrow(df_test)>1){
      step_fun<-generate_func(df_test$from[-1],df_test$price)
      df2[,date_count]<-step_fun(df2$MW)
      names(df2)[date_count]<-as.character(df_test$date[1])
    }
    else{
      df2[,date_count]<-df_test$price
      names(df2)[date_count]<-as.character(df_test$date[1])
    }
    date_count<-date_count+1
  }
  #calculate row means
  means<-rowMeans(df2[,-1])
  #return the row means
  return(means)
}


#set up your lists to store data
month_store <- list()
data_store <- list()

#Enter desired year and plant asset_id in quotes, do not need input_year or asset for general asset type shapes
input_year <- 2014
input_fuel <- "NG1AB"
#asset <- 'DRW1'

#Assignes hour in a week number in 1:168, starting with 12:00am on Sunday
weekdays<- c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
fuel_table <- merit_data %>% filter(he != "02*" & Bids != "na") #& asset_id != "GN2")
fuel_table <- fuel_table %>% mutate(day=day(date), weekday=factor(wday(date),labels=weekdays),he= as.numeric(as.character(he)))%>%
  mutate(month=month(date),year=year(date),weekhour=(wday(date)-1)*24+he)
fuel_table$full_date <- with(fuel_table, paste(date, he , sep="-"))

#create lists that are used to temporarily store data
asset_qar <-list()
month_list <- list()
asset_month_list <- list()
data_store <- list()

#loop through assets
for(asset in unique(fuel_table$asset_id)){
  plant_hr <- fuel_table %>% filter(asset_id==asset)
    #loop through hours
    for(time_hr in unique(plant_hr$full_date))
    {
      #tryCatch({ 
      #Select data based on input criteria
      plant_hr <- plant_hr %>% filter(full_date==time_hr)
      max_cap<-as.numeric(max(plant_hr$to))
      #Repeat rows method matrix
      data_store[time_hr]<-data.frame(c(time_hr,data_proc(plant_hr,max_cap)))
      #}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #prints any error message that exists in loop
    }
    #stack all the data from the hours to make a monthly data_frame
    time_data<-data.frame(c(0,seq(0,max_cap)),do.call("cbind", data_store))
    #fix the column names
    colnames(time_data) <- c("MW",as.character(unlist(time_data[1,-1])))
    time_data = time_data[-1,]
    #add year and month indicators
    hourly_data$full_date<-time_hr
    #store the monthly data in a list
    hourly_data[[input_month]]<-time_data    
}    



#High/low bid functions for different plant types
high_agg_list <- list()
low_agg_list <- list()
#This section creates individual shapes high and low bidding plants if the plant is cogen, high bidding plants are JOF1, RB5 and RL1, all else are low bidding
if  (input_fuel == "NGcogenAB_oilsands") {
  high_list <- asset_month_list[c("JOF1",'RL1','RB5')]
  high_shape <- data.frame(do.call("rbind", high_list))
  high_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(high_list))
  colnames(high_shape)<- c(1:168, "weight",'asset', 'quartile')
  
  power_agg_list <-list()
  for(quar in unique(high_shape$quartile)){
    select_per <- high_shape %>% filter(quartile == quar)
    select_per$month <- rep(c(1:12), times = length(high_list))
    for(mon in unique(select_per$month)){
      select_per_mon <- select_per %>% filter(month == mon)
      select_per_mon$asset <- NULL
      select_per_mon$quartile <- NULL
      select_per_mon$month <- NULL
      weight_mean <- lapply(select_per_mon[, -ncol(select_per_mon)], weighted.mean, w = select_per_mon$weight)
      weight_mean <- data.frame(do.call("cbind", weight_mean))
      entry_name <- paste(mon, quar, sep=" ")
      high_agg_list[[entry_name]] <- data.frame(weight_mean)
    }
  }  
  
  #merge components of list into single dataframe and implement correct format for Aroura
  high_all_shape <- data.frame(do.call("rbind", high_agg_list))
  high_all_shape <- high_all_shape[-169]
  high_all_shape <- high_all_shape[seq(dim(high_all_shape)[1],1),]
  
  
  low_list <- asset_month_list[c('APS1','CL01','EC04', 'IOR1', 'IOR2', 'MEG1', 'MKR1','MKRC','NX02','PR1','SCL1','SCR1','SCR5','SCR6')]
  low_shape <- data.frame(do.call("rbind", low_list))
  low_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(low_list))
  colnames(low_shape)<- c(1:168, "weight",'asset', 'quartile')
  
  power_agg_list <-list()
  for(quar in unique(low_shape$quartile)){
    select_per <- low_shape %>% filter(quartile == quar)
    select_per$month <- rep(c(1:12), times = length(low_list))
    for(mon in unique(select_per$month)){
      select_per_mon <- select_per %>% filter(month == mon)
      select_per_mon$asset <- NULL
      select_per_mon$quartile <- NULL
      select_per_mon$month <- NULL
      weight_mean <- lapply(select_per_mon[, -ncol(select_per_mon)], weighted.mean, w = select_per_mon$weight)
      weight_mean <- data.frame(do.call("cbind", weight_mean))
      entry_name <- paste(mon, quar, sep=" ")
      low_agg_list[[entry_name]] <- data.frame(weight_mean)
    }
  }
  
  #merge components of list into single dataframe and implement correct format for Aroura
  low_all_shape <- data.frame(do.call("rbind", low_agg_list))
  low_all_shape <- low_all_shape[-169]
  low_all_shape <- low_all_shape[seq(dim(low_all_shape)[1],1),]
}
