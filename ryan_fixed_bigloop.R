
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

#load and update merits
update<-1
load("all_merit.RData")  
if(update!=0){
  merit_data<-rbind(merit_data,update_merit(merit_data))
  save(merit_data, file="all_merit.RData")  
}

# add fuel type to merit data
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
    
    #print(paste(date_count,date_id))
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

#Above needs to be run when first opening R, below needs to be rerun for every subsequent plant type

#set a timer
start_time<-Sys.time()
print(paste("Start time is",Sys.time()))

#set up your lists to store data
month_store <- list()
data_store <- list()

#Enter desired year and plant asset_id in quotes, do not need input_year or asset for general asset type shapes
input_year <- 2014
input_fuel <- "NGAB"
#asset <- 'DRW1'

asset_qar <-list()

weekdays<- c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
fuel_table <- merit_data %>% filter(he != "02*" & Fuel == input_fuel) #& asset_id != "GN2")
fuel_table<- fuel_table %>% mutate(day=day(date), weekday=factor(wday(date),labels=weekdays),he= as.numeric(as.character(he)))%>%
mutate(month=month(date),year=year(date),weekhour=(wday(date)-1)*24+he)


for(asset in unique(fuel_table$asset_id)){
 #Tried to use tryCatch to skip over error, didnt work
 tryCatch({  
  plant <- fuel_table %>% filter(asset_id == asset)
  max_cap<-as.numeric(max(plant$to)) #set maximum capacity for the plant
  #Determine position of hours in week
    for(input_month in 1:12)
      {  
      plant_month <- plant %>% filter(month==input_month) #& year==input_year)
      #loop over hours in the week
        for(input_hour in 1:168)
          {
          #Select data based on input criteria
          plant_year <- plant_month %>% filter(weekhour==input_hour)
          #Repeat rows method matrix
          data_store[input_hour]<-data.frame(c(input_hour,data_proc(plant_year,max_cap)))
          }
      #stack all the data from the hours to make a monthly data_frame
      monthly_data<-data.frame(c(0,seq(0,max_cap)),do.call("cbind", data_store))
      #fix the column names
      colnames(monthly_data) <- c("MW",as.character(unlist(monthly_data[1,-1])))
      monthly_data = monthly_data[-1,]
      #add year and month indicators
      monthly_data$month<-input_month
      monthly_data$year<-input_year
      #store the monthly data in a list
      month_store[[input_month]]<-monthly_data    
    }
  #combine all the monthly data
  year_data<-data.frame(do.call("rbind", month_store))
  #fix column names
  colnames(year_data) <- c("MW",seq(1,length(year_data)-3),"Month","Year") #take out columns for MW (#1), month and year

  #write the excel

  #write.xlsx(year_data,"C:/Users/safton/Documents/R/Merit Scripts/Excel Exports/CoalWCA_shapes/all/bids_2017.xlsx")  

  #Create list for percentiles    
  per_list <- list()

  #Loop through hours to create percentiles
  for(hr in 4:ncol(year_data)-2)
  {
    qar <- quantile(year_data[,hr], c(.25, .50, .75, .90, .95, .99), na.rm = TRUE)
    per_list[hr]<-data.frame(qar)
  }

  #Create data.frame with percentiles
  hr_qar <- data.frame(do.call("cbind",per_list))
  colnames(hr_qar)<- c(1:168)
  rownames(hr_qar)<- c('.25', '.50', '.75', '.90', '.95', '.99')
  hr_qar$weight <- max_cap #sets weighting for each plant
  
  #Add plant Quartile to asset_qar list
  asset_qar[[asset]]<-data.frame(hr_qar)  
 }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #prints any error message that exists in loop
  print(asset)
}    

#Add weekly shape for all plants by quartile into single dataframe
type_shape <- data.frame(do.call("rbind", asset_qar))
  type_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(asset_qar))
  colnames(type_shape)<- c(1:168, "weight", 'quartile')

#Create list of weighted averages for hourly bids for every percentile, for single plant type  
agg_list <- list()
for(quar in unique(type_shape$quartile)){
  select_per <- type_shape %>% filter(quartile == quar)
  weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
  agg_list[[quar]] <- data.frame(weight_mean)
}    

#merge components of list into single dataframe and implement correct format for Aroura
all_shape <- data.frame(do.call("rbind", agg_list))
  all_shape <- all_shape[-169]
  all_shape <- all_shape[seq(dim(all_shape)[1],1),]

#This section creates individual shapes high and low bidding plants if the plant is cogen, high bidding plants are JOF1, RB5 and RL1, all else are low bidding
if  (input_fuel == "NGcogenAB_oilsands") {
  high_list <- asset_qar[c(6,12,13)]
  high_shape <- data.frame(do.call("rbind", high_list))
  high_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(high_list))
  colnames(high_shape)<- c(1:168, "weight", 'quartile')
  
  high_agg_list <- list()
  for(quar in unique(high_shape$quartile)){
    select_per <- high_shape %>% filter(quartile == quar)
    weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
    high_agg_list[[quar]] <- data.frame(weight_mean)
  }    
  
  #merge components of list into single dataframe and implement correct format for Aroura
  high_all_shape <- data.frame(do.call("rbind", high_agg_list))
  high_all_shape <- high_all_shape[-169]
  high_all_shape <- high_all_shape[seq(dim(high_all_shape)[1],1),]
  
  low_list <- asset_qar[-c(6,12,13)]
  low_shape <- data.frame(do.call("rbind", low_list))
  low_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(low_list))
  colnames(low_shape)<- c(1:168, "weight", 'quartile')
  
  low_agg_list <- list()
  for(quar in unique(low_shape$quartile)){
    select_per <- low_shape %>% filter(quartile == quar)
    weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
    low_agg_list[[quar]] <- data.frame(weight_mean)
  }    
  
  #merge components of list into single dataframe and implement correct format for Aroura
  low_all_shape <- data.frame(do.call("rbind", low_agg_list))
  low_all_shape <- low_all_shape[-169]
  low_all_shape <- low_all_shape[seq(dim(low_all_shape)[1],1),]
}

#Shapes for high market power and low market CoalWCA power plants 
if  (input_fuel == "CoalWCA") {
  #Plants owned by ATCO and TransAlta with high market power
  high_power <- asset_qar[c("BR3","BR4","BR5","SH1","SH2","KH1","KH2","SD1",'SD2',"SD3","SD4","SD5","SD6")]
  power_shape <- data.frame(do.call("rbind", high_power))
  power_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(high_power))
  colnames(power_shape)<- c(1:168, "weight", 'quartile')
  
  power_agg_list <-list()
  for(quar in unique(power_shape$quartile)){
    select_per <- power_shape %>% filter(quartile == quar)
    weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
    power_agg_list[[quar]] <- data.frame(weight_mean)
  }
  power_all_shape <- data.frame(do.call("rbind", power_agg_list))
  power_all_shape <- power_all_shape[-169]
  power_all_shape <- power_all_shape[seq(dim(power_all_shape)[1],1),]
  
  #Other plants with low makret power
  low_power <- asset_qar[c("GN1","GN2","GN3","HRM","KH3")]
  low_power_shape <- data.frame(do.call("rbind", low_power))
  low_power_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(low_power))
  colnames(low_power_shape)<- c(1:168, "weight", 'quartile')
  
  power_agg_list <-list()
  for(quar in unique(low_power_shape$quartile)){
    select_per <- low_power_shape %>% filter(quartile == quar)
    weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
    power_agg_list[[quar]] <- data.frame(weight_mean)
  }
  low_power_all_shape <- data.frame(do.call("rbind", power_agg_list))
  low_power_all_shape <- low_power_all_shape[-169]
  low_power_all_shape <- low_power_all_shape[seq(dim(low_power_all_shape)[1],1),]
}


#This section creates individual shapes high and low bidding plants if the plant is NG2AB
  if  (input_fuel == "NG2AB") {
    high_list <- asset_qar[c("ALP1", "NPC1", "DRW1", "VVW2", "ALP2", "VVW1", "PH1", "GEN5", "GEN6", "ME02", "ME03", "ME04", "NAT1", "NPC2")]
    high_shape <- data.frame(do.call("rbind", high_list))
    high_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(high_list))
    colnames(high_shape)<- c(1:168, "weight", 'quartile')
    
    high_agg_list <- list()
    for(quar in unique(high_shape$quartile)){
      select_per <- high_shape %>% filter(quartile == quar)
      weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
      high_agg_list[[quar]] <- data.frame(weight_mean)
    }    
    
    #merge components of list into single dataframe and implement correct format for Aroura
    high_all_shape <- data.frame(do.call("rbind", high_agg_list))
    high_all_shape <- high_all_shape[-169]
    high_all_shape <- high_all_shape[seq(dim(high_all_shape)[1],1),]
    
    low_list <- asset_qar[c("ENC1", "NPP1", "ENC3", "CRS3", "ENC2", "CRS2", "CRS1")]
    low_shape <- data.frame(do.call("rbind", low_list))
    low_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(low_list))
    colnames(low_shape)<- c(1:168, "weight", 'quartile')
    
    low_agg_list <- list()
    for(quar in unique(low_shape$quartile)){
      select_per <- low_shape %>% filter(quartile == quar)
      weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
      low_agg_list[[quar]] <- data.frame(weight_mean)
    }    
    
    #merge components of list into single dataframe and implement correct format for Aroura
    low_all_shape <- data.frame(do.call("rbind", low_agg_list))
    low_all_shape <- low_all_shape[-169]
    low_all_shape <- low_all_shape[seq(dim(low_all_shape)[1],1),]
}  
  
#This section creates individual shapes high and low bidding plants if the plant is NG1AB
  if  (input_fuel == "NG1AB") {
    high_list <- asset_qar[c("BCRK", "CAL1")]
    high_shape <- data.frame(do.call("rbind", high_list))
    high_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(high_list))
    colnames(high_shape)<- c(1:168, "weight", 'quartile')
    
    high_agg_list <- list()
    for(quar in unique(high_shape$quartile)){
      select_per <- high_shape %>% filter(quartile == quar)
      weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
      high_agg_list[[quar]] <- data.frame(weight_mean)
    }    
    
    #merge components of list into single dataframe and implement correct format for Aroura
    high_all_shape <- data.frame(do.call("rbind", high_agg_list))
    high_all_shape <- high_all_shape[-169]
    high_all_shape <- high_all_shape[seq(dim(high_all_shape)[1],1),]
    
    low_list <- asset_qar[c("ALS1", "CMH1", "DOWG", "EC01", "EGC1", "MFG1", "NX01", "TC01", "TLM2", "UOC1")]
    low_shape <- data.frame(do.call("rbind", low_list))
    low_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(low_list))
    colnames(low_shape)<- c(1:168, "weight", 'quartile')
    
    low_agg_list <- list()
    for(quar in unique(low_shape$quartile)){
      select_per <- low_shape %>% filter(quartile == quar)
      weight_mean <- lapply(select_per[, -ncol(select_per)], weighted.mean, w = select_per$weight)
      low_agg_list[[quar]] <- data.frame(weight_mean)
    }    
    
    #merge components of list into single dataframe and implement correct format for Aroura
    low_all_shape <- data.frame(do.call("rbind", low_agg_list))
    low_all_shape <- low_all_shape[-169]
    low_all_shape <- low_all_shape[seq(dim(low_all_shape)[1],1),]
}
  
#end your timer
int <- interval(start_time, Sys.time())
print(paste("End time is",Sys.time(),"and it took",time_length(int, "seconds"),"seconds"))  


