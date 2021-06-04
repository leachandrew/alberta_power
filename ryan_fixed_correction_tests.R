
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

#Above needs to be run when first opening R, below needs to be rerun for every subsequent plant type

#set a timer
start_time<-Sys.time()
print(paste("Start time is",Sys.time()))

#set up your lists to store data
month_store <- list()
data_store <- list()

#Enter desired year and plant asset_id in quotes, do not need input_year or asset for general asset type shapes
input_year <- 2016
input_fuel <- "NG1AB"
#asset <- 'DRW1'

#Assignes hour in a week number in 1:168, starting with 12:00am on Sunday
weekdays<- c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
fuel_table <- merit_data %>% filter(he != "02*" & Fuel == input_fuel) #& asset_id != "GN2")
fuel_table<- fuel_table %>% mutate(day=day(date), weekday=factor(wday(date),labels=weekdays),he= as.numeric(as.character(he)))%>%
  mutate(month=month(date),year=year(date),weekhour=(wday(date)-1)*24+he)
fuel_table$full_date <- with(fuel_table, paste(year, month, day, hour , sep="-"))

#Cuts out the 1000 tightest hours from calculations
fuel_table <- subset(fuel_table, full_date %in% cut_below_1000$full_date)

#create lists that are used to temporarily store data
asset_qar <-list()
month_list <- list()
asset_month_list <- list()


for(asset in unique(fuel_table$asset_id)){
  #use tryCatch to skip over errors
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
    
    #Insert monthly shape creation
    month_shape <-list()
    for (mon in unique(year_data$month)){
      per_list <- list()
      single_month <- year_data %>% filter(month == mon)
      #Loop through hours to create percentiles
      for(hr in 4:ncol(single_month)-2)
      {
        #Calculate values that represent the middle of percentiles for 0.25, 0.50, 0.75, 0.90, 0.95, 0.99 
        qar <- quantile(single_month[,hr], c(.125, .375, .625, .825, .925, .975), na.rm = TRUE)
        per_list[hr]<-data.frame(qar)
      }
      #Bind together percentiles into single dataframe
      hr_qar <- data.frame(do.call("cbind",per_list))
      colnames(hr_qar)<- c(1:168)
      rownames(hr_qar)<- c('.25', '.50', '.75', '.90', '.95', '.99')
      hr_qar$weight <- max_cap #sets weighting for each plant
      
      #Add plant Quartile to asset_qar list
      month_shape[[mon]]<-data.frame(hr_qar)
    }
    #Binds all the monthly percentiles gorether from the monthly shapes list 
    names(month_shape)<- c("1","2","3","4","5","6","7","8","9","10","11","12")
    month_shape_frame <- data.frame(do.call("rbind", month_shape))
    month_shape_frame$asset <- asset
    month_shape_frame$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(12))
    asset_month_list[[asset]] <- month_shape_frame
    #fix column names
    colnames(year_data) <- c("MW",seq(1,length(year_data)-3),"Month","Year") #take out columns for MW (#1), month and year
    month_list[[asset]]<-year_data
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #prints any error message that exists in loop
  print(asset)
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

#Shapes for high market power and low market CoalWCA power plants 
if  (input_fuel == "CoalWCA") {
  #Plants owned by ATCO and TransAlta with high market power
  high_list <- asset_month_list[c("BR3","BR4","BR5","SH1","SH2","KH1","KH2","SD1",'SD2',"SD3","SD4","SD5","SD6")]
  #Binds together plants with high power to get single dataframe
  high_shape <- data.frame(do.call("rbind", high_list))
  high_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(high_list))
  colnames(high_shape)<- c(1:168, "weight",'asset', 'quartile')
  
  #This function takes the weighted mean of high power plants based on how many MW of capacity they have. This same function is reused for every plant type and high vs low bidders
  power_agg_list <-list()
  for(quar in unique(high_shape$quartile)){
    select_per <- high_shape %>% filter(quartile == quar)
    select_per$month <- rep(c(1:12), times = length(high_list))
    for(mon in unique(select_per$month)){
      select_per_mon <- select_per %>% filter(month == mon)
      #These next 3 lines remove the asset name, quartile, and month to clean up dataframe
      select_per_mon$asset <- NULL
      select_per_mon$quartile <- NULL
      select_per_mon$month <- NULL
      #This takes the weighted average
      weight_mean <- lapply(select_per_mon[, -ncol(select_per_mon)], weighted.mean, w = select_per_mon$weight)
      weight_mean <- data.frame(do.call("cbind", weight_mean))
      entry_name <- paste(mon, quar, sep=" ")
      high_agg_list[[entry_name]] <- data.frame(weight_mean)
    }
  }
  #Creates a single shape for all high market power plants
  high_all_shape <- data.frame(do.call("rbind", high_agg_list))
  high_all_shape <- high_all_shape[-169]
  high_all_shape <- high_all_shape[seq(dim(high_all_shape)[1],1),]
  
  #Other plants with low makret power
  low_list <- asset_month_list[c("GN1","GN2","GN3","HRM","KH3")]
  low_power_shape <- data.frame(do.call("rbind", low_list))
  low_power_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(low_list))
  colnames(low_power_shape)<- c(1:168, "weight",'asset', 'quartile')
  
  power_agg_list <-list()
  for(quar in unique(low_power_shape$quartile)){
    select_per <- low_power_shape %>% filter(quartile == quar)
    select_per$month <- rep(c(1:12), times = length(low_list))
    for(mon in unique(select_per$month)){
      select_per_mon <- select_per %>% filter(month == mon)
      select_per_mon$asset <- NULL
      select_per_mon$quartile <- NULL
      select_per_mon$month <- NULL
      weight_mean <- lapply(select_per_mon[, -ncol(select_per_mon)], weighted.mean, w = select_per_mon$weight)
      weight_mean <- data.frame(do.call("cbind", weight_mean))
      entry_name <- paste(mon, quar, sep=" ")
      power_agg_list[[entry_name]] <- data.frame(weight_mean)
    }
  }
  low_all_shape <- data.frame(do.call("rbind", power_agg_list))
  low_all_shape <- low_all_shape[-169]
  low_all_shape <- low_all_shape[seq(dim(low_all_shape)[1],1),]
}


#This section creates individual shapes high and low bidding plants if the plant is NG2AB
if  (input_fuel == "NG2AB") {
  high_list <- asset_month_list[c("ALP1", "NPC1", "DRW1", "VVW2", "ALP2", "VVW1", "PH1", "GEN5", "GEN6", "ME02", "ME03", "ME04", "NAT1", "NPC2")]
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
  
  low_list <- asset_month_list[c("ENC1", "NPP1", "ENC3", "CRS3", "ENC2", "CRS2", "CRS1")]
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

#This section creates individual shapes high and low bidding plants if the plant is NG1AB
if  (input_fuel == "NG1AB") {
  high_list <- asset_month_list[c("BCRK", "CAL1")]
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
  
  low_list <- asset_month_list[c("ALS1", "CMH1", "DOWG", "EC01", "EGC1", "MFG1", "NX01", "TC01", "TLM2", "UOC1")]
  low_shape <- data.frame(do.call("rbind", low_list))
  low_shape$quartile <- rep(c('.25', '.50', '.75', '.90', '.95', '.99'), times = length(low_list))
  colnames(low_power_shape)<- c(1:168, "weight",'asset', 'quartile')
  
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

#end your timer
int <- interval(start_time, Sys.time())
print(paste("End time is",Sys.time(),"and it took",time_length(int, "seconds"),"seconds"))

#This section calacualtes the bid adder, based on the lowest bid above must run generation
dispatch_list_type <- list()
for(bid_type in unique(fuel_table$Bids_Fuel)){
  #This selects the value of block number 1 to be the dispatch cost of each plant
  bid_cat <- fuel_table %>% filter(Bids_Fuel == bid_type & block_number == 1)
  dispatch_list <- list()
  capacity_list <- list()
  #Calaculate a weigted average dispatch cost for all the plants within the bidding group
  for(asset in unique(bid_cat$asset_id)){
    bid_asset <- bid_cat %>% filter(asset_id == asset)
    capacity <- max(bid_asset$to)
    #Selecting this cost is important, but I am currently unshore what the best method would be for this (min, mean, median???)
    cost <- median(bid_asset$price)
    weighted_cost <- capacity * cost 
    capacity_list[[asset]] <- capacity
    dispatch_list[[asset]] <- weighted_cost
  }
  cap_frame <- data.frame(do.call("rbind", capacity_list))
  total_cap <- sum(cap_frame[1])
  cost_frame <- data.frame(do.call("rbind", dispatch_list)) / total_cap
  total_cost <- sum(cost_frame[1])
  dispatch_list_type[[bid_type]] <- total_cost
}
#Select the lowest dispatch cost of the bidding groups and apply it to all units. This helps to negate market power that the larger firms exert through higher bids
base_cost_frame <- data.frame(do.call("rbind", dispatch_list_type))
base_cost <- min(base_cost_frame)

#These sections actually calaculate the adder and set all negaive values to 0, which represents must run capacity
low_adder <- low_all_shape - base_cost
low_adder[low_adder < 0] = 0
high_adder <- high_all_shape - base_cost
high_adder[high_adder < 0] = 0