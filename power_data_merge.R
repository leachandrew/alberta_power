#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")

#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())
source("../andrew_base.R")



#routine to load plant info from Excel

get_plant_info <-function() {
  #load plant data
  #plant_data <- read.xlsx(xlsxFile = "2014_2017_gen.xlsx", sheet = 4, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE) 
  plant_data <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  
  plant_info<-data.frame(t(plant_data[(1:10),-1]))
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:10),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  
  #Some of the names had errors or inconsistencies so fixed here
  #plant_info$NRG_Stream<-revalue(plant_info$NRG_Stream, c("AB - H R Milner Hr Avg MW"="AB Milner Hr Avg MW"))
  #plant_info$NRG_Stream<-gsub("AB - H R Milner Hr Avg MW","AB - Milner Hr Avg MW",plant_info$NRG_Stream)
  #plant_info$NRG_Stream<-revalue(plant_info$NRG_Stream, c("AB - NPC1 Denis St Pierre Hr Avg MW"="AB - NPC1 Denis St  Pierre Hr Avg MW"))
  #plant_info$NRG_Stream<-gsub("AB - NPC1 Denis St Pierre Hr Avg MW","AB - NPC1 Denis St  Pierre Hr Avg MW",plant_info$NRG_Stream)
  
  plant_info<-arrange(plant_info,NRG_Stream)
  #put plant info in the Global Environment
  assign("plant_info", plant_info, envir = .GlobalEnv)
}

all_gen_data <- function() {
  #get all generation data
  years<-c(2004:year(Sys.Date()))
  count<-1
  get_plant_info()
  if(exists("stack"))
    rm(stack)
  for (y_data in years){
  #testing y_data<-2004
    file_name<-paste("hourly",sprintf('%02d', y_data %% 100),".csv",sep="")
    print(paste("data file is ",file_name))
    new_data <- read.csv(file_name,check.names = FALSE,stringsAsFactors = F)
    new_data[,-1] <- sapply(new_data[,-1], as.numeric )
    
    #here, we need to carry forward to fill any missing data points
    new_data[,-1] <- sapply(new_data[,-1], na.locf, na.rm = FALSE)
    
    #take out periods
    colnames(new_data)<-gsub("\\.", " ", colnames(new_data)) 
    #rename time
    names(new_data)[names(new_data) == "Effective Date"] <- 'Time'
    #add id variables - easier for revenue, capacity factor, and share of load calcs
    new_data$Demand<-new_data$`AB - Actual Demand Load MW`
    new_data$Price<-new_data$`AB - Actual Hourly Pool Price Price CAD/MWh`
    new_data$AIL<-new_data$`AB - Alberta Internal Load Hr Avg MW`
    #new_data$Time<- as.POSIXct(strptime(new_data$Time, "%m/%d/%Y %H:%M",tz="America/Denver"))
    #fix names in some older files  
    names(new_data)<-gsub("AB - H R  Milner Hr Avg MW","AB - H R Milner Hr Avg MW",names(new_data))
    names(new_data)<-gsub("AB - NPC1 Denis St  Pierre Hr Avg MW","AB - NPC1 Denis St Pierre Hr Avg MW",names(new_data))
    names(new_data)<-gsub("AB - BC Hydro  Imp/Exp Hr Avg MW","AB - BC Hydro Imp/Exp Hr Avg MW",names(new_data))
    names(new_data)<-gsub("AB - Encana Foster Creek  Hr Avg MW","AB - Encana Foster Creek Hr Avg MW",names(new_data))
    new_data$"AB - WECC Imp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"<0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW",0)
    new_data$"AB - WECC Exp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW">0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW",0)
    new_data$`AB - Saskpower Imp AB Hr Avg MW`<-ifelse(new_data$"AB - Saskpower Imp/Exp AB Hr Avg MW"<0,new_data$"AB - Saskpower Imp/Exp AB Hr Avg MW",0)
    new_data$"AB - Saskpower Exp AB Hr Avg MW"<-ifelse(new_data$"AB - Saskpower Imp/Exp AB Hr Avg MW">0,new_data$"AB - Saskpower Imp/Exp AB Hr Avg MW",0)
    new_data$"AB - BC Hydro Imp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"<0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW",0)
    new_data$"AB - BC Hydro Exp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW">0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW",0)
    #IS THERE AB-MONTANA TRADE?
    if(length(grep("AB - Montana Imp/Exp Hr Avg MW",names(new_data)))>0){
      new_data$"AB - WECC Imp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"+new_data$"AB - Montana Imp/Exp Hr Avg MW"<0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"+new_data$"AB - Montana Imp/Exp Hr Avg MW",0)
      new_data$"AB - WECC Exp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"+new_data$"AB - Montana Imp/Exp Hr Avg MW">0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"+new_data$"AB - Montana Imp/Exp Hr Avg MW",0)
      new_data$"AB - Montana Imp Hr Avg MW"<-ifelse(new_data$"AB - Montana Imp/Exp Hr Avg MW"<0,new_data$"AB - Montana Imp/Exp Hr Avg MW",0)
      new_data$"AB - Montana Exp Hr Avg MW"<-ifelse(new_data$"AB - Montana Imp/Exp Hr Avg MW">0,new_data$"AB - Montana Imp/Exp Hr Avg MW",0)
      }
    new_data$"AB - WECC Imp/Exp Hr Avg MW"<-new_data$"AB - WECC Imp Hr Avg MW"+new_data$"AB - WECC Exp Hr Avg MW"
    #if yes, set AB -WECC trade to be BC + Montant, otheriwise just BC
    #melt data into long form
    new_melt <- melt(new_data,id=c("Time","Demand","Price","AIL"),variable.name="NRG_Stream",value.name = "gen") 
    new_melt<-arrange(new_melt,Time)
    #bring in a local version of plant_info
    
    plant_info_mod<-plant_info
    #bring in ghg data
    ghg_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "GHG_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
    #ghg_rates<-dcast(ghg_rates, formula = GHG_ID ~ ...,value.var = "Poln_rate")
    ghg_rates<-ghg_rates %>% spread(Pollutant,Poln_rate) %>% select(GHG_ID,CO2)
    #bring in heat rates
    heat_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Heat_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE) %>%
      select(GHG_ID,Aurora_ID,Heat.Rate)
    #combine all plant info, heat rates, and GHGs by plant ID
    combined<-ghg_rates %>% left_join(heat_rates,by="GHG_ID")
    coal_co2_btu<-100.4  #coal fuel factor GHGs/MMBTU
    gas_co2_btu<-53.077752 #gas fuel factor GHGs/MMBTU
    plant_info_mod<-plant_info_mod %>% left_join(combined,by="Aurora_ID") %>% select(-Aurora_ID,-Aurora_Name)
    combined<-NULL
    plant_info_mod$co2_est<-plant_info_mod$CO2/2.20462*plant_info_mod$Heat.Rate #convert to kg/mmbtu
    plant_info_mod$co2_est<-ifelse(plant_info_mod$Plant_Fuel=="COAL",coal_co2_btu*plant_info_mod$Heat.Rate,plant_info_mod$co2_est)
    plant_info_mod$co2_est<-ifelse(plant_info_mod$Plant_Fuel=="GAS",gas_co2_btu*plant_info_mod$Heat.Rate,plant_info_mod$co2_est)
    plant_info_mod$co2_est<-plant_info_mod$co2_est/1000 #adjust from kg to tonnes
    
    #set capacities for the interties over time since Plant_info has them as a single cell
    if(y_data<2015) 
      plant_info_mod$Capacity[grep("AB_WECC_Exp",plant_info$ID)]<-735
    if(y_data==2016)
      plant_info_mod$Capacity[grep("AB_WECC_Imp",plant_info$ID)]<-975
    if(y_data==2015)
      plant_info_mod$Capacity[grep("AB_WECC_Imp",plant_info$ID)]<-950
    if(y_data==2014)
      plant_info_mod$Capacity[grep("AB_WECC_Imp",plant_info$ID)]<-765
    if(y_data==2013)
      plant_info_mod$Capacity[grep("AB_WECC_Imp",plant_info$ID)]<-700
    if(y_data==2012)
      plant_info_mod$Capacity[grep("AB_WECC_Imp",plant_info$ID)]<-700
    if(y_data==2011)
      plant_info_mod$Capacity[grep("AB_WECC_Imp",plant_info$ID)]<-625
    if(y_data==2010)
      plant_info_mod$Capacity[grep("AB_WECC_Imp",plant_info$ID)]<-650
    if(y_data<=2009)
      plant_info_mod$Capacity[grep("AB_WECC_Imp",plant_info$ID)]<-600
    #merge in the plant info
    new_melt<- new_melt%>%left_join(plant_info_mod,by="NRG_Stream")
    #print(paste("Number of NA Dates:",length(subset(new_melt[,1],is.na(new_melt[,"Time"])))))
    #stack each year of data
    if(count==1)
      stack<-new_melt
    else
      stack<-rbind(stack,new_melt)
    count<-count+1
    #check for data issues
    print(paste("Merged year ", y_data,". Number of NA Dates:",length(subset(stack[,1],is.na(stack[,"Time"])))))
  }
  #merge files into gen_data
  gen_data<-stack
  #convert all the times
  gen_data$Time<-parse_date_time(gen_data$Time, orders = c("ymd_HM", "dmy_HM", "mdy_HM","dby_HM","dby_HMS"),tz="America/Denver")
  #convert trade so that imports are positive, exports negative so same sign as gen
  gen_data[grep("TRADE", gen_data$Plant_Type),"gen"]<-gen_data[grep("TRADE", gen_data$Plant_Type),"gen"]*-1
  gen_data<-arrange(gen_data,Time,ID)
  #create some other variables in gen_data
  gen_data$Revenue<-gen_data$Price*gen_data$gen
  gen_data$Cap_Fac<-gen_data$gen/gen_data$Capacity
  
  #do date and he to match aeso
  gen_data$he<-as.character(hour(gen_data$Time)+1)
  singles<-seq(1,9)
  for(hour in singles){
    gen_data$he[gen_data$he==hour]<-paste(0,hour,sep="")
  }
  unique(gen_data$he)
  gen_data$date<-date(gen_data$Time)
  save(gen_data, file= "gen.RData")
}

#when you swich to a new year, may need to re-run #all_gen_data
get_plant_info()


#all_gen_data()

load(file="gen.RData")


#gen_data %>% filter(is.na(AESO_Name)) %>% select(NRG_Stream) %>% unique()



data_update <- function(data_sent,fix_year=0) {
  #build current year data and append to gen.Rdata
  #testing
  data_sent<-filter(gen_data,year(Time)==2018)
  #fix_year<-2019
  
  get_plant_info()
  stack<-NULL
  max_year<-max(year((na.omit(data_sent$Time))))
  #if switching to a new year, run once to get last of new data from previous year then fix year to new year and run again
  if(fix_year!=0)
    max_year<-fix_year
  file_name<-paste("hourly",sprintf('%02d', max_year %% 100),".csv",sep="")
  print(paste("data file is ",file_name))
  new_data <- read.csv(file_name,check.names = FALSE,stringsAsFactors = F)
  new_data[,-1] <- sapply(new_data[,-1], as.numeric )
  #take out periods
  colnames(new_data)<-gsub("\\.", " ", colnames(new_data)) 
  #rename time
  names(new_data)[names(new_data) == "Effective Date"] <- 'Time'
  #add id variables - easier for revenue, capacity factor, and share of load calcs
  new_data$Demand<-new_data$`AB - Actual Demand Load MW`
  new_data$Price<-new_data$`AB - Actual Hourly Pool Price Price CAD/MWh`
  new_data$AIL<-new_data$`AB - Alberta Internal Load Hr Avg MW`
  #new_data$Time<- as.POSIXct(strptime(new_data$Time, "%m/%d/%Y %H:%M",tz="America/Denver"))
  #fix names in some older files  
  names(new_data)<-gsub("AB - H R  Milner Hr Avg MW","AB - H R Milner Hr Avg MW",names(new_data))
  names(new_data)<-gsub("AB - NPC1 Denis St  Pierre Hr Avg MW","AB - NPC1 Denis St Pierre Hr Avg MW",names(new_data))
  names(new_data)<-gsub("AB - BC Hydro  Imp/Exp Hr Avg MW","AB - BC Hydro Imp/Exp Hr Avg MW",names(new_data))
  names(new_data)<-gsub("AB - Encana Foster Creek  Hr Avg MW","AB - Encana Foster Creek Hr Avg MW",names(new_data))
  new_data$"AB - WECC Imp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"<0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW",0)
  new_data$"AB - WECC Exp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW">0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW",0)
  new_data$`AB - Saskpower Imp AB Hr Avg MW`<-ifelse(new_data$"AB - Saskpower Imp/Exp AB Hr Avg MW"<0,new_data$"AB - Saskpower Imp/Exp AB Hr Avg MW",0)
  new_data$"AB - Saskpower Exp AB Hr Avg MW"<-ifelse(new_data$"AB - Saskpower Imp/Exp AB Hr Avg MW">0,new_data$"AB - Saskpower Imp/Exp AB Hr Avg MW",0)
  new_data$"AB - BC Hydro Imp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"<0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW",0)
  new_data$"AB - BC Hydro Exp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW">0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW",0)
  #IS THERE AB-MONTANA TRADE?
  if(length(grep("AB - Montana Imp/Exp Hr Avg MW",names(new_data)))>0){
    new_data$"AB - WECC Imp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"+new_data$"AB - Montana Imp/Exp Hr Avg MW"<0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"+new_data$"AB - Montana Imp/Exp Hr Avg MW",0)
    new_data$"AB - WECC Exp Hr Avg MW"<-ifelse(new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"+new_data$"AB - Montana Imp/Exp Hr Avg MW">0,new_data$"AB - BC Hydro Imp/Exp Hr Avg MW"+new_data$"AB - Montana Imp/Exp Hr Avg MW",0)
    new_data$"AB - Montana Imp Hr Avg MW"<-ifelse(new_data$"AB - Montana Imp/Exp Hr Avg MW"<0,new_data$"AB - Montana Imp/Exp Hr Avg MW",0)
    new_data$"AB - Montana Exp Hr Avg MW"<-ifelse(new_data$"AB - Montana Imp/Exp Hr Avg MW">0,new_data$"AB - Montana Imp/Exp Hr Avg MW",0)
  }
  new_data$"AB - WECC Imp/Exp Hr Avg MW"<-new_data$"AB - WECC Imp Hr Avg MW"+new_data$"AB - WECC Exp Hr Avg MW"
  #if yes, set AB -WECC trade to be BC + Montant, otheriwise just BC
  #melt data into long form
  new_melt <- melt(new_data,id=c("Time","Demand","Price","AIL"),variable.name="NRG_Stream",value.name = "gen") 
  new_melt<-arrange(new_melt,Time)
  #bring in a local version of plant_info
  plant_info_mod<-plant_info
  #merge in the plant info
  
  ghg_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "GHG_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  #ghg_rates<-dcast(ghg_rates, formula = GHG_ID ~ ...,value.var = "Poln_rate")
  ghg_rates<-ghg_rates %>% spread(Pollutant,Poln_rate) %>% select(GHG_ID,CO2)
  #bring in heat rates
  heat_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Heat_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE) %>%
    select(GHG_ID,Aurora_ID,Heat.Rate)
  #combine all plant info, heat rates, and GHGs by plant ID
  combined<-ghg_rates %>% left_join(heat_rates,by="GHG_ID")
  coal_co2_btu<-100.4  #coal fuel factor GHGs/MMBTU
  gas_co2_btu<-53.077752 #gas fuel factor GHGs/MMBTU
  plant_info_mod<-plant_info_mod %>% left_join(combined,by="Aurora_ID") %>% select(-Aurora_ID,-Aurora_Name)
  
  
  new_melt<-merge(new_melt,plant_info_mod,by="NRG_Stream",all.x =TRUE)
  #convert trade so that imports are positive, exports negative
  
  
  new_melt$Time<-parse_date_time(new_melt$Time, orders = c("ymd_HM", "dmy_HM", "mdy_HM","dby_HM","dby_HMS"),tz="America/Denver")
  #convert trade so that imports are positive, exports negative so same sign as gen
  new_melt[grep("TRADE", new_melt$Plant_Type),"gen"]<-new_melt[grep("TRADE", new_melt$Plant_Type),"gen"]*-1
  
  new_melt<-arrange(new_melt,Time,ID)
  #create some other variables in gen_data
  
  new_melt$Revenue<-new_melt$Price*new_melt$gen
  new_melt$Cap_Fac<-new_melt$gen/new_melt$Capacity
  new_melt$he<-as.character(hour(new_melt$Time)+1)
  singles<-seq(1,9)
  for(hour in singles){
    new_melt$he[new_melt$he==hour]<-paste(0,hour,sep="")
  }
  new_melt$date<-date(new_melt$Time)
  
  data_new<-rbind(data_sent,subset(new_melt,Time>max(data_sent$Time)))
  return(data_new)
}

#load the file
load(file="gen.RData")

#gen_data<-filter(gen_data,year(Time)<=2018)
#save(gen_data, file= "gen.RData")
#in the hourly csv, don't forget to fix the time change hours!

#if we're changing over to a new year, run both of these two lines of code:
gen_data<-data_update(gen_data,2020)
save(gen_data, file= "gen.RData")

#gen_data<-data_update(gen_data,fix_year = 2019)
#section out the forward-looking stuff with NA prices
#gen_data<-subset(gen_data,Time<=max(gen_data$Time[!is.na(gen_data$Price)]))

#fix solar cats
#gen_data$Plant_Type <- fct_collapse(gen_data$Plant_Type,
#                                    SOLAR = c("SOLAR", "SUN"))
#save the file
save(gen_data, file= "gen.RData")






