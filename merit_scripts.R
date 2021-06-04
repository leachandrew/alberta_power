oba_type<-function(plant_sent,year_sent,ei_sent){
  grid_avg<-0.65
  #nested case_when
  oba_12<-
    case_when(
      plant_sent == "HYDRO" ~ grid_avg,
      plant_sent == "COAL" ~ ei_sent*0.88,
      plant_sent == "COGEN" ~ 0.418,
      plant_sent == "NGCC" ~ ei_sent*.88,
      plant_sent == "SCGT" ~ ei_sent*.88,
      plant_sent == "SOLAR" ~ grid_avg,
      plant_sent == "WIND" ~ grid_avg,
      TRUE                      ~  0 
    )
  
  oba_15<-
    case_when(
      plant_sent == "HYDRO" ~ grid_avg,
      plant_sent == "COAL" ~ ei_sent*0.85,
      plant_sent == "COGEN" ~ 0.418,
      plant_sent == "NGCC" ~ ei_sent*.85,
      plant_sent == "SCGT" ~ ei_sent*.85,
      plant_sent == "SOLAR" ~ grid_avg,
      plant_sent == "WIND" ~ grid_avg,
      TRUE                      ~  0 
    )
  oba_20<-
    case_when(
      plant_sent == "HYDRO" ~ grid_avg,
      plant_sent == "COAL" ~ ei_sent*0.8,
      plant_sent == "COGEN" ~ 0.418,
      plant_sent == "NGCC" ~ ei_sent*.8,
      plant_sent == "SCGT" ~ ei_sent*.8,
      plant_sent == "SOLAR" ~ grid_avg,
      plant_sent == "WIND" ~ grid_avg,
      TRUE                      ~  0 
    )
  case_when(
    year_sent <=2015     ~ oba_12,
    year_sent %in% c(2016)    ~ oba_15,
    year_sent %in% c(2017)    ~ oba_20,
    TRUE                      ~  0.37 
  )
}


ctax_year<-function(year_sent){
  case_when(
    year_sent <= 2015 ~ 15,
    year_sent == 2016 ~ 20,
    year_sent == 2017 ~ 30,
    TRUE                      ~  30 
  )
}

#get gas spots
ngx_data_read<-function(){
  #testing stuff
  file_name<-"nit_gas_spot_old.csv"
  ngx_data_old <- read.csv(file_name,blank.lines.skip=T,stringsAsFactors=F,header=T)
  file_name<-"nit_gas_15_19.csv"
  ngx_data_15_19 <- read.csv(file_name,blank.lines.skip=T,stringsAsFactors=F,header=T)
  file_name<-"nit_gas_spot.csv"
  ngx_data <- read.csv(file_name,blank.lines.skip=T,stringsAsFactors=F,header=T)
  ngx_data<-rbind(ngx_data_old,ngx_data_15_19,ngx_data)
  
  
  names(ngx_data)<-c("date_time","low","WAvg", "High","Open","Settle", "Volume","Inst_Date","Drop") 
  ngx_data<-ngx_data %>% select(date_time,Settle,Inst_Date)
  ngx_data$date<-as.Date(parse_date_time(ngx_data$date_time,c("ymd HM","dmy HMS")))
  ngx_data<-ngx_data %>% select(date,Settle)
  names(ngx_data)<-c("date","nit_settle_cad_gj")
  ngx_data
}



sger_emissions_data<-function(){
  keep_columns<-
    c("Reporting.Company.Name",              "Sector",
      "Facility.Name",                        "asset_1",                          
      "asset_2",                           "asset_3",                          
      "asset_4",                           "asset_5",                          
      "asset_6",                           "Facility.ID",
      "Total.Production",                     "Cogen.Electricity.(MWh)",
      "BEI.(in.CR)",                          "NEIL.Limit",                          
      "EI.(incl.cogen.adjustment)")
  id_columns<-
    c("Reporting.Company.Name",               "Sector",
      "Facility.Name",                        "Facility.ID",
      "Total.Production",                     "Cogen.Electricity.(MWh)",
      "BEI.(in.CR)",                          "NEIL.Limit",                          
      "EI.(incl.cogen.adjustment)")
  file_name<-"SGER_data.xlsx"
  sger_data <- read.xlsx(file_name,sheet = "2016 CR Tracking")
  #spread the ids to new columns
  sger_data<-sger_data %>% separate(IDs.on.site, paste("asset",seq(1,6),sep="_"))%>%
    #keep the data we need
    select(keep_columns)  %>%
    melt(id=id_columns,variable.name="generating_unit",value.name = "asset_id") %>%
    filter(!is.na(asset_id),!(Sector=="Power Plant - Cogen")) %>%
    rename("SGER_baseline"="BEI.(in.CR)",
           "limit_2016"="NEIL.Limit",
           "sger_2016_adj_ei"="EI.(incl.cogen.adjustment)",
           "sger_facility"="Facility.Name",
           "sger_facility_id"="Facility.ID"
    ) %>%
    select(asset_id,SGER_baseline,limit_2016,sger_2016_adj_ei,sger_facility,sger_facility_id)%>%
    filter(!is.na(SGER_baseline))
  sger_data
}





augment_data<-function(merit_sent){
  #testing
  #merit_sent<-filter(merit_data, year(date)==2019,month(date)==2)%>% clean_merit_trade()
  
  #bring in plant data
  plant_data <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  plant_info<-data.frame(t(plant_data[(1:10),-1]),stringsAsFactors = F)
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:10),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  
  plant_info<-plant_info %>%
    mutate(NRG_Stream = ifelse(grepl("AB - H R Milner Hr Avg MW",NRG_Stream),"AB Milner Hr Avg MW",NRG_Stream))%>%
    mutate(NRG_Stream = ifelse(grepl("AB - NPC1 Denis St Pierre Hr Avg MW",NRG_Stream),"AB - NPC1 Denis St  Pierre Hr Avg MW",NRG_Stream))  
  plant_info<-arrange(plant_info,NRG_Stream)
  
  #bring in ghg data
  ghg_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "GHG_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  #ghg_rates<-dcast(ghg_rates, formula = GHG_ID ~ ...,value.var = "Poln_rate")
  ghg_rates<-ghg_rates %>% spread(Pollutant,Poln_rate) %>% select(GHG_ID,CO2)
  
  
  #bring in heat rates
  heat_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Heat_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE) %>%
    select(GHG_ID,Aurora_ID,Heat.Rate)
  #combine all plant info, heat rates, and GHGs by plant ID
  combined<-merge(ghg_rates,heat_rates,by="GHG_ID",all.y = T) # NA's match
  coal_co2_btu<-100.4  #coal fuel factor GHGs/MMBTU
  gas_co2_btu<-53.077752 #gas fuel factor GHGs/MMBTU
  combined<-merge(plant_info,combined,suffixes = c(".info",".rates"),all.x=TRUE,by="Aurora_ID") # NA's match
  combined$co2_est<-combined$CO2/2.20462*combined$Heat.Rate #convert to kg/mmbtu
  combined$co2_est<-ifelse(combined$Plant_Fuel=="COAL",coal_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-ifelse(combined$Plant_Fuel=="GAS",gas_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-combined$co2_est/1000 #adjust from kg to tonnes
  
  merit_sent<-merit_sent %>% group_by(date,he) %>%
    mutate(hourly_exports=sum((import_export=="E")*size),hourly_imports=sum((import_export=="I")*size))  %>% ungroup()
  
  
  merit_sent %>% left_join(combined,by=c("asset_id"="ID")) %>%left_join(forecast_data,by=c("date","he")) %>%
    select(-Latitude,-Longitude,-Aurora_Name,-NRG_Stream,GHG_ID,start_date)
  
  
}



plant_data<-function(){
  #testing
  #merit_sent<-filter(merit_data, year(date)==2019,month(date)==2)%>% clean_merit_trade()
  
  #bring in plant data
  plant_data <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
  plant_info<-data.frame(t(plant_data[(1:10),-1]),stringsAsFactors = F)
  #fix names
  plant_info<-setNames(plant_info,t(plant_data[(1:10),1]))
  plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
  
  plant_info<-plant_info %>%
    mutate(NRG_Stream = ifelse(grepl("AB - H R Milner Hr Avg MW",NRG_Stream),"AB Milner Hr Avg MW",NRG_Stream))%>%
    mutate(NRG_Stream = ifelse(grepl("AB - NPC1 Denis St Pierre Hr Avg MW",NRG_Stream),"AB - NPC1 Denis St  Pierre Hr Avg MW",NRG_Stream))  
  plant_info<-arrange(plant_info,NRG_Stream)
  
  #bring in ghg data
  ghg_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "GHG_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  #ghg_rates<-dcast(ghg_rates, formula = GHG_ID ~ ...,value.var = "Poln_rate")
  ghg_rates<-ghg_rates %>% spread(Pollutant,Poln_rate) %>% select(GHG_ID,CO2)
  
  
  #bring in heat rates
  heat_rates <- read.xlsx(xlsxFile = "AB_Plant_Info_New.xlsx", sheet = "Heat_Rates", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE) %>%
    select(GHG_ID,Aurora_ID,Heat.Rate)
  #combine all plant info, heat rates, and GHGs by plant ID
  combined<-merge(ghg_rates,heat_rates,by="GHG_ID",all.y = T) # NA's match
  coal_co2_btu<-100.4  #coal fuel factor GHGs/MMBTU
  gas_co2_btu<-53.077752 #gas fuel factor GHGs/MMBTU
  combined<-merge(plant_info,combined,suffixes = c(".info",".rates"),all.x=TRUE,by="Aurora_ID") # NA's match
  combined$co2_est<-combined$CO2/2.20462*combined$Heat.Rate #convert to kg/mmbtu
  combined$co2_est<-ifelse(combined$Plant_Fuel=="COAL",coal_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-ifelse(combined$Plant_Fuel=="GAS",gas_co2_btu*combined$Heat.Rate,combined$co2_est)
  combined$co2_est<-combined$co2_est/1000 #adjust from kg to tonnes
  
  #merit_sent<-merit_sent %>% group_by(date,he) %>%
  #  mutate(hourly_exports=sum((import_export=="E")*size),hourly_imports=sum((import_export=="I")*size))  %>% ungroup()
  
  
  combined %>%
    select(-Latitude,-Longitude,-Aurora_Name,-NRG_Stream,GHG_ID)
  
  
}




clean_merit_trade<-function(data_sent,id_tag){
  #for testing
  #data_sent<-head(merit_data,1000) #%>% assign_time %>% assign_date_time_days %>% assign_peaks
  #data_sent<-merit_day #%>% assign_time %>% assign_date_time_days %>% assign_peaks
  
  #read in all AESO asset names so we're up-to-date
  #check for internet
  if(has_internet()){
    zz =
      readHTMLTable("http://ets.aeso.ca/ets_web/ip/Market/Reports/AssetListReportServlet?contentType=html",colClasses = "character",stringsAsFactors = FALSE)
    aeso_assets<-as.data.frame(rbind(zz)[2])
    save(aeso_assets,file="aeso_assets.Rdata")
  }
  #if you don't have internet, load the archived file
  if(-has_internet())
    load(file="aeso_assets.Rdata")
  colnames(aeso_assets)<- aeso_assets[1, ] # the first row will be the header
  aeso_assets<-clean_names(aeso_assets[-1,])
  zz<-NULL
  trade<-data_sent%>%filter(import_export %in% c("E","I")) %>% #select the imports and exports from the merit data
    left_join(aeso_assets,by=c("asset_id"="asset_id"))
  trade$dest<-NA
  trade<-trade %>% mutate(dest = ifelse(grepl("BC",asset_name),"AB_BC",dest),
                          dest = ifelse(grepl("MT",asset_name),"AB_MON",dest),
                          dest = ifelse(grepl("SK",asset_name),"AB_SK",dest),
                          dest = ifelse(grepl("Sask",asset_name),"AB_SK",dest),
                          dest = ifelse(grepl("SPC",asset_name),"AB_SK",dest), 
                          dest = ifelse(grepl("XB",asset_name),"AB_BC",dest),
                          dest = ifelse(grepl("PWX",asset_name),"AB_BC",dest))
  
  
  trade<- trade %>% group_by(date,he,effective_date_time,import_export,dest,dispatched,flexible,price) %>% 
    summarise(size=sum(size),
              available_mw=sum(available_mw),
              dispatched_mw=sum(dispatched_mw)) %>%
    mutate(key_firm=TRUE,offer_control="TRADE",offer_sum="TRADE",merit=0,
           from=0,to=available_mw,block_number=0,) %>% 
    ungroup()%>%
    mutate(dest=ifelse(import_export=="I",paste(dest,"_IMP",sep = ""),paste(dest,"_EXP",sep = "")))
  names(trade)[names(trade) == "dest"] <- "asset_id"
  clean<-data_sent%>%filter(!import_export %in% c("E","I")) %>% #select the non-imports and non-exports from the merit data
    rbind(trade)
  clean
}


#functions to build each firms blocks by percentile instead of other stuff

#step function to map blocks to bids over x
generate_func<-function(x,y){
  stepfun(x, y, f = as.numeric(0), ties = "ordered",right = FALSE)
}

#if there's only one block, you'll need a single value function
single_value_function<-function(y){
  function(x){y}
}

bid_func<-function(x,y){
  if(length(y)>1)
    generate_func(x[-1],y)
  else
    single_value_function(y)
}


merit_bid_pctl<-function(merit_sent){
  merit_bids<-merit_sent %>% group_by(date,he,asset_id)%>% arrange(date,he,asset_id,price) %>%
    mutate(bid_capacity=max(to))%>%
    summarize(
      #place offer percentiles and prices in lists of vectors
      #from=list(from),
      
      #dispatched_mw=list(dispatched_mw/size),
      #available_mw=list(available_mw/size),
      from=list(from/bid_capacity*100),price=list(price),
    )%>% group_by(date,he,asset_id) %>% #re-group the summarized data
    #get and store the bid function
    mutate(step_func=list(bid_func(from[[1]],price[[1]])))%>%
    ungroup()
  paste("Built step functions. Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")
  
  
  start_time<-Sys.time()
  #now make the bids
  merit_bids<-merit_bids%>% group_by(date,he,asset_id) %>%
    mutate(bid_10=step_func[[1]](10),
           bid_20=step_func[[1]](20),
           bid_30=step_func[[1]](30),
           bid_40=step_func[[1]](40),
           bid_50=step_func[[1]](50),
           bid_60=step_func[[1]](60),
           bid_70=step_func[[1]](70),
           bid_80=step_func[[1]](80),
           bid_90=step_func[[1]](90),
           bid_100=step_func[[1]](100))%>%
    ungroup() %>% select(-from,-price,-step_func)
  paste("Build bids, cleaned data frame, elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")
  merit_bids
} 
