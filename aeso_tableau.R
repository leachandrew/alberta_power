source("power_paper_base.R")

source("aeso_scrapes.R")
options(scipen = 999)
library(directlabels)
library(cowplot)

aeso_gen<-read_csv("Gen Chart_Full Data_data.csv")%>%clean_names()

aeso_gen<-aeso_gen%>%mutate(time=mdy_hms(date_mst))%>%
  select(time,fuel_type,maximum_capacity,system_available,system_capacity,system_generation,total_generation)

aeso_monthly <-aeso_gen %>%
  mutate(month=month(time),year=year(time))%>%
  group_by(month,year,fuel_type)%>%
  summarize(capacity=max(maximum_capacity),
            system_generation=sum(system_generation,na.rm=T),
            total_generation=sum(total_generation,na.rm=T),
            )%>%
  ungroup()%>%
  mutate(date=ymd(paste(year,month,1,sep="-")))%>%
  arrange(date)
  
aeso_monthly%>%
  mutate(fuel_type=as_factor(fuel_type),
         fuel_type=fct_recode(fuel_type,"Natural Gas Combined Cycle"="Combined Cycle"),
         fuel_type=fct_recode(fuel_type,"Natural Gas Fired Steam"="Gas Fired Steam"),
         fuel_type=fct_recode(fuel_type,"Natural Gas Simple Cycle"="Simple Cycle"),
         fuel_type=fct_relevel(fuel_type,c("Natural Gas Combined Cycle","Natural Gas Simple Cycle",
                                           "Natural Gas Fired Steam"),after=2),
         fuel_type=fct_relevel(fuel_type,c("Dual Fuel"),after=1),
         fuel_type=fct_rev(fuel_type),
         fuel_type=fct_relevel(fuel_type,c("Storage","Other"),after=Inf)
  )%>%
ggplot()+
  geom_area(aes(date,capacity,fill=fuel_type),linewidth=0.5,colour="black")+
  scale_fill_manual(
    values = c(
      "Coal"                      = "black",
      "Dual Fuel"                 = "brown",
      "Cogeneration"              = "darkgoldenrod3",
      "Natural Gas Combined Cycle"= "dodgerblue3",
      "Natural Gas Simple Cycle"  = "deepskyblue4",
      "Natural Gas Fired Steam"   = "steelblue",
      "Hydro"                     = "blue",
      "Wind"                      = colors_ua10()[1],
      "Solar"                     = colors_ua10()[2],
      "Storage"                   = "orchid",
      "Other"                     = "grey60"
    )
  ) +
  theme_ps_grid()+
  guides(fill=guide_legend(nrow = 2))+
  scale_x_date(date_breaks = "1 years",expand=c(0,0),date_labels = "%Y")+
  scale_y_continuous(breaks = pretty_breaks(5),expand=c(0,0))+
  labs(x="",y="Installed Capacity (MW)")

ggsave("aeso_cap_plant.png",width=12,height = 7,dpi=300)
  



aeso_monthly%>%
  mutate(fuel_type=as_factor(fuel_type),
         fuel_type=fct_recode(fuel_type,"Natural Gas Combined Cycle"="Combined Cycle"),
         fuel_type=fct_recode(fuel_type,"Natural Gas Fired Steam"="Gas Fired Steam"),
         fuel_type=fct_recode(fuel_type,"Natural Gas Simple Cycle"="Simple Cycle"),
         fuel_type=fct_relevel(fuel_type,c("Natural Gas Combined Cycle","Natural Gas Simple Cycle",
                                           "Natural Gas Fired Steam"),after=2),
         fuel_type=fct_relevel(fuel_type,c("Dual Fuel"),after=1),
         fuel_type=fct_rev(fuel_type),
         fuel_type=fct_relevel(fuel_type,c("Storage","Other"),after=Inf)
  )%>%
ggplot()+
  geom_area(aes(date,total_generation/10^6,fill=fuel_type),linewidth=0.5,colour="black")+
  scale_fill_manual(
    values = c(
      "Coal"                      = "black",
      "Dual Fuel"                 = "brown",
      "Cogeneration"              = "darkgoldenrod3",
      "Natural Gas Combined Cycle"= "dodgerblue3",
      "Natural Gas Simple Cycle"  = "deepskyblue4",
      "Natural Gas Fired Steam"   = "steelblue",
      "Hydro"                     = "blue",
      "Wind"                      = colors_ua10()[1],
      "Solar"                     = colors_ua10()[2],
      "Storage"                   = "orchid",
      "Other"                     = "grey60"
    )
  ) +
  theme_ps_grid()+
  guides(fill=guide_legend(nrow = 2))+
  scale_x_date(date_breaks = "1 years",expand=c(0,0),date_labels = "%Y")+
  scale_y_continuous(breaks = pretty_breaks(5),expand=c(0,0))+
  labs(x="",y="Monthly Generation (TWh)")

ggsave("aeso_gen_plant.png",width=12,height = 7,dpi=300)

#Fuels

aeso_monthly_fuel<-
  aeso_monthly %>%
  mutate(fuel_type=as_factor(fuel_type),
         fuel_type=fct_other(fuel_type,drop=c("Cogeneration","Combined Cycle","Gas Fired Steam","Simple Cycle"),other_level = "Natural Gas")
         )%>%
  group_by(date,fuel_type)%>%
  summarize(capacity=sum(capacity,na.rm=T),
            system_generation=sum(system_generation,na.rm=T),
            total_generation=sum(total_generation,na.rm=T),
  )%>%
  ungroup()%>%
  mutate(fuel_type=as_factor(fuel_type),
         fuel_type=fct_relevel(fuel_type,"Other",after = Inf),
         fuel_type=fct_relevel(fuel_type,"Coal",after = Inf),
         fuel_type=fct_relevel(fuel_type,"Wind"),
         fuel_type=fct_relevel(fuel_type,"Solar"))

ggplot(aeso_monthly_fuel)+
  geom_area(aes(date,capacity,fill=fuel_type),linewidth=0.5,colour="black")+
scale_fill_manual("",
  values = c(
    "Coal"                      = "black",
    "Dual Fuel"                 = "brown",
    "Natural Gas"  = "deepskyblue4",
    "Hydro"                     = "blue",
    "Wind"                      = colors_ua10()[1],
    "Solar"                     = colors_ua10()[2],
    "Storage"                   = "orchid",
    "Other"                     = "grey60"
  )
) +
  guides(fill=guide_legend(nrow = 1))+
  theme_irpp()+
  scale_x_date(date_breaks = "1 years",expand=c(0,0),date_labels = "%Y")+
  scale_y_continuous(breaks = pretty_breaks(5),expand=c(0,0))+
  labs(x="",y="Installed Capacity (MW)")

ggsave("aeso_cap_fuel.png",width=7,height = 3.5,dpi=300)

ggplot(aeso_monthly_fuel)+
  geom_area(aes(date,total_generation/10^6,fill=fuel_type),linewidth=0.5,colour="black")+
  scale_fill_manual("",
  values = c(
    "Coal"                      = "black",
    "Dual Fuel"                 = "brown",
    "Natural Gas"  = "deepskyblue4",
    "Hydro"                     = "blue",
    "Wind"                      = colors_ua10()[1],
    "Solar"                     = colors_ua10()[2],
    "Storage"                   = "orchid",
    "Other"                     = "grey60"
  )
) +
  theme_irpp()+
  guides(fill=guide_legend(nrow = 1))+
  scale_x_date(date_breaks = "1 years",expand=c(0,0),date_labels = "%Y")+
  scale_y_continuous(breaks = pretty_breaks(5),expand=c(0,0))+
  labs(x="",y="Monthly Generation (TWh)")

ggsave("aeso_gen_fuel.png",width=7,height = 3.5,dpi=300)


lto_cap<-read_excel("LTO-2024-Data-File-08-19-2024.xlsx",sheet="Capacity Forecast",range="A9:U25")%>%clean_names()%>%
  rename("plant_type"=1)%>%
  pivot_longer(-plant_type,names_to = "year",values_to = "capacity")%>%
  mutate(year=as.numeric(gsub("x","",year)),
         plant_type=gsub("-"," ",plant_type),
         plant_type=gsub("wCCUS","w CCUS",plant_type))%>%
  filter(plant_type!="Coal")%>%
  I()

lto_cap%>%
  mutate(plant_type=factor(plant_type,levels = plant_types),
         plant_type=fct_relevel(plant_type,"Storage",after=0))%>%
  ggplot()+
  #geom_area(aes(year,capacity,fill=plant_type),linewidth=0.5,colour="black")+
  geom_col(aes(year,capacity,fill=plant_type),linewidth=0.5,colour="black",width = 1,
           position=position_stack(reverse = TRUE))+
  
  scale_fill_manual("",
    values = c(
      "Coal"                      = "black",
      "Coal to Gas"                 = "brown",
      "Cogeneration"              = "darkgoldenrod1",
      "Cogeneration w CCUS"              = "darkgoldenrod3",
      "Cogeneration H2"              = "darkgoldenrod4",
      "Combined Cycle"= "dodgerblue1",
      "Combined Cycle w CCUS"= "dodgerblue3",
      "Combined Cycle w CCUS Retrofit"= "dodgerblue4",
      "Simple Cycle"  = "deepskyblue4",
      "Simple Cycle H2"   = "steelblue1",
      "Hydro"                     = "blue",
      "Nuclear (SMR)" = "lightgoldenrod",
      "Wind"                      = colors_ua10()[1],
      "Solar"                     = colors_ua10()[2],
      "Storage"                   = "orchid",
      "Other"                     = "grey60"
    )
  ) +
  theme_irpp()+
  guides(fill=guide_legend(nrow = 3))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks(10))+
  scale_y_continuous(breaks = pretty_breaks(5),expand=c(0,0))+
  expand_limits(y=30000)+
  labs(x="",y="Installed Capacity (MW)")

ggsave("aeso_cap_lto.png",width=8,height = 4.5,dpi=300)

plant_types = c(
  "Coal",
  "Coal to Gas"  ,
  "Nuclear (SMR)" ,
  "Combined Cycle",
  "Combined Cycle w CCUS",
  "Combined Cycle w CCUS Retrofit",
  "Cogeneration",
  "Cogeneration w CCUS",
  "Cogeneration H2",
  "Simple Cycle" ,
  "Simple Cycle H2",
  "Other" ,
  "Hydro",
  "Wind",
  "Solar",
  "Storage"
  )


lto_gen<-read_excel("LTO-2024-Data-File-08-19-2024.xlsx",sheet="Generation Forecast",range="A9:U25")%>%clean_names()%>%
  rename("plant_type"=1)%>%
  pivot_longer(-plant_type,names_to = "year",values_to = "generation")%>%
  mutate(year=as.numeric(gsub("x","",year)),
         plant_type=gsub("-"," ",plant_type),
         plant_type=gsub("wCCUS","w CCUS",plant_type))%>%
  #filter(!grepl("Cogeneration",plant_type))
  I()

lto_gen%>%
  filter(!grepl("Storage",plant_type))%>%
  mutate(plant_type=factor(plant_type,levels = plant_types))%>%
  ggplot()+
  #geom_area(aes(year,generation/10^6,fill=plant_type),linewidth=0.25,colour="black",
  #          position=position_stack(reverse = TRUE))+
  geom_col(aes(year,generation/10^6,fill=plant_type),linewidth=0.5,colour="black",width = 1,
  position=position_stack(reverse = TRUE))+
  
  scale_fill_manual("",
                    values = c(
                      "Coal"                      = "black",
                      "Coal to Gas"                 = "brown",
                      "Cogeneration"              = "darkgoldenrod1",
                      "Cogeneration w CCUS"              = "darkgoldenrod3",
                      "Cogeneration H2"              = "darkgoldenrod4",
                      "Combined Cycle"= "dodgerblue1",
                      "Combined Cycle w CCUS"= "dodgerblue3",
                      "Combined Cycle w CCUS Retrofit"= "dodgerblue4",
                      "Simple Cycle"  = "deepskyblue4",
                      "Simple Cycle H2"   = "steelblue1",
                      "Hydro"                     = "blue",
                      "Nuclear (SMR)" = "lightgoldenrod",
                      "Wind"                      = colors_ua10()[1],
                      "Solar"                     = colors_ua10()[2],
                      "Storage"                   = "orchid",
                      "Other"                     = "grey60"
                    )
  ) +
  theme_irpp()+
  guides(fill=guide_legend(nrow = 3))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks(10))+
  scale_y_continuous(breaks = seq(0,125,25),expand=c(0,0))+
  expand_limits(y=125)+
  labs(x="",y="Generation (TWh)")

ggsave("aeso_gen_lto.png",width=8,height = 4.5,dpi=300)

# Ontario and Alberta peak comparison

#https://api.eia.gov/v2/electricity/rto/region-data/data/?frequency=hourly&data[0]=value&facets[respondent][]=ERCO&facets[respondent][]=MISO&facets[type][]=D&sort[0][column]=period&sort[0][direction]=asc&offset=119840&length=5000

ercot<-
GET(url=paste("https://api.eia.gov/v2/","electricity/rto/region-data/data/",sep=""),
    query = list(
      api_key=KEY,
      frequency="hourly",
      "data[0]"="value",
      "facets[respondent][]"="ERCO",
      "facets[type][]"="D",
      start=as.character(Sys.Date()-days(2)),
      end=as.character(Sys.Date()),
      "sort[0][column]"="period",
      "sort[0][direction]"="asc",
      #offset=119840,
      #offset=0,
      #length=5000,
      out="json"
    )
)%>% 
  httr::content() %>%
  pluck("response", "data") %>%
  enframe() %>%
  unnest_auto(value) %>%
  mutate(period_utc=ymd_h(gsub("T"," ",period)),
         value=as.numeric(value),
         period_local = with_tz(period_utc, tzone = "America/Chicago"))


library(httr)
library(dplyr)
library(lubridate)
library(tidyr)
library(jsonlite)

# build URL and parameters  
url <- "https://api.gridstatus.io/v1/datasets/ercot_net_load/query"

params <- list(
  start_time = "2022-01-01",
  end_time   = "2025-12-31",
  timezone   = "market",
  api_key    = grid_status_key
)

# send request  
resp <- GET(url, query = params)
stop_for_status(resp)

# parse JSON and extract data  
json_data <- content(resp, as = "text", encoding = "UTF-8") |> 
  fromJSON(flatten = TRUE)

# It depends on how the JSON is structured. Suppose it has something like json_data$data or json_data$results
ercot_all <- json_data$data  # adjust this path if needed

save(ercot_all,file="ercot_data.rdata")



update_forecasts()
load("data/forecast_data.RData")

ieso_load<-read_csv("https://reports-public.ieso.ca/public/Demand/PUB_Demand_2022.csv",skip=3)%>%
  bind_rows(read_csv("https://reports-public.ieso.ca/public/Demand/PUB_Demand_2023.csv",skip=3))%>%
  bind_rows(read_csv("https://reports-public.ieso.ca/public/Demand/PUB_Demand_2024.csv",skip=3))%>%
  bind_rows(read_csv("https://reports-public.ieso.ca/public/Demand/PUB_Demand_2025.csv",skip=3))%>%
  
  clean_names()

ieso_load_all<-ieso_load%>%
  mutate(time=ymd_h(paste(date,hour),tz=""),rto="IESO")%>%
  filter(time>=ymd("2022-01-01"))%>%
  select(time,load=market_demand,rto)%>%
  filter(!is.na(time))


# make a local folder to store downloads
download_folder <- "ercot_hourly_load"
if (!dir.exists(download_folder)) dir.create(download_folder)
download.file("https://www.ercot.com/files/docs/2025/02/11/Native_Load_2025.zip",mode="wb",destfile = "ercot_hourly_load/ercot_2025.zip")
download.file("https://www.ercot.com/files/docs/2024/02/06/Native_Load_2024.zip",mode="wb",destfile = "ercot_hourly_load/ercot_2024.zip")
download.file("https://www.ercot.com/files/docs/2023/02/09/Native_Load_2023.zip",mode="wb",destfile = "ercot_hourly_load/ercot_2023.zip")
download.file("https://www.ercot.com/files/docs/2022/02/08/Native_Load_2022.zip",mode="wb",destfile = "ercot_hourly_load/ercot_2022.zip")


# Folder where ZIPs are stored
zip_folder <- "ercot_hourly_load"

# 1. List all ZIP files
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)

# 2. Function to read a single ZIP file
read_ercot_zip <- function(zip_file) {
  #zip_file<-zip_files[1]
  # List contents of ZIP
  files_in_zip <- unzip(zip_file, list = TRUE)$Name
  
  # Pick the first Excel file (adjust if multiple)
  xls_file <- files_in_zip[str_detect(files_in_zip, "\\.xls|\\.xlsx|\\.csv")][1]
  
  # Temporary extraction
  temp_file <- tempfile(fileext = tools::file_ext(xls_file))
  unzip(zip_file, files = xls_file, exdir = tempdir())
  full_path <- file.path(tempdir(), xls_file)
  
  # Read depending on extension
  df <- if (str_detect(xls_file, "\\.xls|\\.xlsx")) {
    read_excel(full_path, skip = 0)  # may need to adjust skip
  } else {
    read.csv(full_path, stringsAsFactors = FALSE)
  }
  
  # Add a column for year (from filename)
  df <- df %>%
    mutate(year = str_extract(basename(zip_file), "\\d{4}"))
  
  return(df)
}

# 3. Read all ZIP files and combine
ercot_load_all <- map_dfr(zip_files, read_ercot_zip)%>%
  clean_names()

ercot_load_all<-ercot_load_all%>%
  mutate(time=mdy_hm(hour_ending,tz=""),rto="ERCOT")%>%
  filter(time>=ymd("2022-01-01"))%>%
  select(time,load=ercot,rto)%>%
  filter(!is.na(time))

aeso_load_all <-
  forecast_data %>% filter(time>=ymd("2022-01-01"))%>%
  mutate(time=as.POSIXct(time,tz=""),rto="AESO")%>%
  select(time,load=actual_ail,rto)

loads_all<-
  bind_rows(aeso_load_all,ieso_load_all,ercot_load_all)


test<-
loads_all %>% 
  mutate(year=year(time),hour=hour(time),month=month(time))%>%
  group_by(rto,year)%>%
  mutate(avg_load=mean(load,na.rm = T))%>%
  ungroup()%>%
  mutate(index_load=load/avg_load)

hour_load<-
  test%>%
  group_by(hour,rto)%>%
  summarize(n=n(),
            index_mean=mean(index_load),
            max_index=max(index_load,na.rm = T),
            min_index=min(index_load,na.rm = T),
            q95_index=quantile(index_load, probs=c(.95),na.rm = T),
            q05_index=quantile(index_load, probs=c(.05),na.rm = T))%>%
  ungroup()%>%
  mutate(hour_lab=paste(hour,":00",sep = ""))


month_load<-
  test%>%
  group_by(year,month,rto)%>%
  summarize(n=n(),
            index_mean=mean(index_load),
            q95_index=quantile(index_load, probs=c(.95),na.rm = T),
            q05_index=quantile(index_load, probs=c(.05),na.rm = T),
            max_index=max(index_load,na.rm = T),
            min_index=min(index_load,na.rm = T),
           )%>%
  ungroup()%>%
  mutate(date=ymd(paste(year,month,1,sep="-")))



  month_load%>%
  ggplot()+
  scale_y_continuous(labels = label_percent(),expand = c(0,0)) +
  scale_x_date(date_breaks = "1 years",date_labels = "%b\n%Y",expand = c(0,0)) +
  #geom_ribbon(aes(date,ymax=q95_index,ymin=q05_index,fill=rto),alpha=.5)+
  geom_ribbon(aes(date,ymax=max_index,ymin=min_index,fill="Range of Deviations"),alpha=.4)+
  geom_line(aes(date,index_mean,colour="Mean Deviation"),linewidth=1.05)+
  scale_fill_manual("",values="steelblue")+
  scale_colour_manual("",values="steelblue4")+
    
  facet_grid(rows=vars(rto))+
  expand_limits(x=ymd("2021-12-31"))+
  theme_irpp()+
  #theme(legend.position = "none")+
  labs(y="Deviation from annual mean load",x="")
ggsave("monthly_load_compare.png",width=5,height = 4)


hour_load%>%
  ggplot()+
  geom_ribbon(aes(hour+1,ymax=max_index,ymin=min_index,fill="Range of Deviations"),alpha=.4)+
  geom_line(aes(hour+1,index_mean,colour="Mean Deviation"),linewidth=1.05)+
  scale_y_continuous(labels = label_percent(),expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),breaks = pretty_breaks(24),
                     labels = paste0(1:24, ":00"))+
  scale_fill_manual("",values="steelblue")+
  scale_colour_manual("",values="steelblue4")+
  
  expand_limits(x=24)+
  facet_grid(rows=vars(rto))+
  theme_irpp()+
  theme(#legend.position = "none",
        axis.text.x = element_text(
          angle = 90,    # rotate 90 degrees
          vjust = 0.5,   # vertical justification
          hjust = 1      # horizontal justification
        )
        )+
  labs(y="Deviation from annual mean load",x="Time at end of Hour")
ggsave("hourly_load_compare.png",width=5,height = 4)

