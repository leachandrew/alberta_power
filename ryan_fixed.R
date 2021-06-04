
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



#Enter desired year and plant asset_id in quotes  (I took out the character years - that's slowing you down)
input_year <- 2017
input_plant <- "GN3"

#Select data by plant
plant <- merit_data %>% filter(asset_id == input_plant & he != "02*")  #use filter here - way faster
weekdays<- c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

#Determine position of hours in week
plant<- plant %>% mutate(day=day(date), weekday=factor(wday(date),labels=weekdays),he= as.numeric(as.character(he)))%>%
                  mutate(month=month(date),year=year(date),weekhour=(wday(date)-1)*24+he)

#again, using mutate to add columns as opposed to subsetting and merging

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



#set a timer
start_time<-Sys.time()
print(paste("Start time is",Sys.time()))
#set up your lists to store data
month_store <- list()
data_store <- list()
#define max capacity for the plant
max_cap<-as.numeric(max(plant$to)) #set maximum capacity for the plant
#loop over months
for(input_month in 1:12)
{  
  plant_month <- plant %>% filter(month==input_month & year==input_year)
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

#end your timer
int <- interval(start_time, Sys.time())
print(paste("End time is",Sys.time(),"and it took",time_length(int, "seconds"),"seconds"))

#write the excel

#write.xlsx(year_data,"C:/Users/safton/Documents/R/Merit Scripts/Excel Exports/CoalWCA_shapes/all/bids_2017.xlsx")  




#Create list for percentiles    
per_list <- list()

#Loop through hours to create percentiles
for(hr in 4:ncol(year_data)-2){
  qar <- quantile(year_data[,hr], c(.25, .50, .75, .90, .95, .99))
  per_list[hr]<-data.frame(qar)
}

#Create data.frame with percentiles
hr_qar <- data.frame(do.call("cbind",per_list))
  colnames(hr_qar)<- c(1:168)
  rownames(hr_qar)<- c('.25', '.50', '.75', '.90', '.95', '.99')
    
    
    
    
    
