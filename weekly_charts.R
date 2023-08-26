
nrg_folder<-"C:/Users/aleach/Google Drive/NRGStream"

assign_peaks<-function(data_orig,time_var=time){
  #default is that we're receiving a data_frame with time as the time variable
  #create temp_time with whatever the time variable might be, then use that to create stats and peaks
  #modify data_sent so you have a data-frame with only the time varible
  #data_mod<-data_orig %>% mutate_(temp_time=`time_var`) %>% select(temp_time)
  temp_time<- enquo(time_var)
  data_mod<-data_orig%>% select(!!!temp_time)
  #first, the holidays
  #Holidays:
  #xNew Year's Day  January 1
  #xAlberta Family Day   Third Monday in February
  #xGood Friday   Friday before Easter 
  #Victoria Day  Monday before May 25 	
  #xCanada Day July 1, except when it falls on a Sunday, then it is July 2
  #xLabour Day  First Monday in September
  #xThanksgiving Day  Second Monday in October 
  #xRemembrance Day   November 11 
  #xChristmas Day   December 25
  holiday_list<-c("Christmas","NYD","CDA_Day","Rem_Day","Labour_Day","Good_Friday","Family_Day",  
                  "Thanksgiving", "Victoria_Day")
  data_mod<-data_mod%>%mutate(
    Christmas=ifelse(month(!!temp_time)==12 & day(!!temp_time)==25,T,F),
    NYD=ifelse(month(!!temp_time)==1 & day(!!temp_time)==1,T,F),
    CDA_Day=ifelse(month(!!temp_time)==7 & day(!!temp_time)==1 & wday(!!temp_time,label = T)!="Sun" ,T,F), #Canada Day Holiday if it's not a Sunday
    CDA_Day=ifelse(month(!!temp_time)==7 & day(!!temp_time)==2 & wday(!!temp_time,label = T)=="Mon" ,T,F), #Canada Day Stat if the 2nd is a monday
    Rem_Day=ifelse(month(!!temp_time)==11 & day(!!temp_time)==11,T,F),
    Labour_Day=ifelse(month(!!temp_time)==9 & day(!!temp_time)<=7 & wday(!!temp_time,label = T)=="Mon",T,F), #first Monday in September
    Good_Friday=ifelse(date(!!temp_time)==as.Date(Easter(year(!!temp_time)))-days(2),T,F),
    #Family day - third monday in february so earliest it can be is day 15, latest is day 21
    Family_Day=ifelse(month(!!temp_time)==2 & day(!!temp_time)<=21 & day(!!temp_time)>=15 & wday(!!temp_time,label = T)=="Mon",T,F), #third Monday in Feb
    #Thanksgiving day - second monday in Oct so earliest it can be is day 8, latest is day 14
    Thanksgiving=ifelse(month(!!temp_time)==10 & day(!!temp_time)<=14 & day(!!temp_time)>=8 & wday(!!temp_time,label = T)=="Mon",T,F), #second Monday in Oct
    #Victoria day - monday before May 25, so earliest it can be is day 18, latest is day 24
    Victoria_Day=ifelse(month(!!temp_time)==5 & day(!!temp_time)<=24 & day(!!temp_time)>=18 & wday(!!temp_time,label = T)=="Mon",T,F) #Monday before May 25
  ) %>% mutate(
    stat = select(., holiday_list) %>% rowSums()>0
  )
  #On-Peak: hour ending HE8 to HE23 Monday through Saturday, excluding Sundays and NERC holidays
  #Off-Peak: HE1 to HE7 and HE24 Monday through Saturday, and all hours on Sundays and NERC holidays
  #Extended Peak: HE8 to HE23 every day in the contract period
  #Extended Off-Peak: HE1 to HE7 and HE24 every day in the contract period
  #Super Peak: HE17 to HE22 each day in the contract period
  #for AS, AESO does AM super peak HE 6, 7, 8 and a winter PM Super Peak (HE 17-34, in Nov, Dec, Jan)
  data_mod<-data_mod%>%mutate(
    on_peak=ifelse(wday(!!temp_time,label = T)!="Sun" & stat==F & hour(!!temp_time)>=8 & hour(!!temp_time)<=23,T,F), #Peak hours, not stat or Sunday
    off_peak=ifelse(wday(!!temp_time,label = T)=="Sun" | stat==T | hour(!!temp_time)>=24 | hour(!!temp_time)<=7,T,F), #Off-Peak hours, stat or Sunday
    ext_peak=ifelse(hour(!!temp_time)>=8 & hour(!!temp_time)<=23,T,F), #Ext Peak hours
    ext_off_peak=ifelse(hour(!!temp_time)<8 & hour(!!temp_time)>23,T,F), #Ext Off Peak hours
    super_peak=ifelse(hour(!!temp_time)>=17 & hour(!!temp_time)<=22,T,F), #Super Peak hours
  )
  #return indicators for stats and peaks - same # of rows as data sent
  data_mod<-data_mod %>% select(stat,on_peak,off_peak,ext_peak,ext_off_peak,super_peak)
  bind_cols(data_orig,data_mod)
}


assign_date_time_days<-function(data_sent,time_var=time){
  quo_time<- enquo(time_var)
  data_sent %>% 
    mutate(year=year(!!quo_time),
           month=month(!!quo_time), #month dummies
           month_fac=factor(month.abb[month],levels = month.abb),
           day=day(!!quo_time),
           wday=wday(!!quo_time,label=T),
           hour=hour(!!quo_time),
           temp_time=NULL
    )
}

assign_time <- function(data, date_var=date, he_var=he){
  quo_date <- enquo(date_var)
  quo_he <- enquo(he_var)
  data %>% 
    mutate(time = ymd_h(paste(year(!!quo_date), "-", 
                              month(!!quo_date), "-", 
                              day(!!quo_date), "-", 
                              !!quo_he, sep = "")))
}

get_forecast_report <- function(start_date, end_date) {
  #testing below here
  #start_date<- Sys.Date()-months(1)
  #end_date <- min(start_date+months(1),Sys.Date()-days(1)) #31 days of data
  #end testing - above should be commented if you're not testing
  
  start_date <- as.Date(start_date)
  end_date <- min(as.Date(start_date+months(1)),as.Date(end_date)) #31 days of data
  #print(start_date)
  #print(end_date)
  
  GET(
    url = "http://ets.aeso.ca/ets_web/ip/Market/Reports/ActualForecastWMRQHReportServlet",
    query = list(
      beginDate = format(start_date, "%m%d%Y"),
      endDate = format(end_date, "%m%d%Y"),
      contentType = "csv"
    )
  ) -> res
  stop_for_status(res)
  test<- content(res, as="text") %>% 
    stri_split_lines() %>% 
    flatten_chr()
  test<-paste(test[5:length(test)], collapse="\n")
  forecast_data<-read.csv(text=test,header = TRUE, stringsAsFactors=FALSE)
  clean_data<-janitor::clean_names(forecast_data) %>% 
    as_tibble()%>%
    mutate(time=as.POSIXct(date, format="%m/%d/%Y %H",tz="America/Denver"))
  
  date_he<- do.call(rbind,strsplit(as.character(clean_data$date),' '))
  clean_data$he<-date_he[,2]
  #start_date here is report_date
  clean_data$start_date<-mdy(date_he[,1])
  #date at end of hour
  clean_data$date<-as_date(clean_data$time)
  clean_data$forecast_ail<- gsub("\\,", "", clean_data$forecast_ail)
  clean_data$actual_ail<- gsub("\\,", "", clean_data$actual_ail)
  #clean_data<-clean_data %>% select(-he) %>% mutate_if(is.character,as.numeric)
  #clean_data<-clean_data %>% select(-he)%>% mutate_if(is.integer,as.numeric)
  #set numeric columns to numeric
  clean_data[,c(2:6)]<-lapply(clean_data[,c(2:6)],as.numeric)
  return(clean_data)
}

#set date to today
day<-Sys.Date()-days(30)
#get last month of data
price_ail<-get_forecast_report(as.Date(day), as.Date(Sys.Date())+days(1))
price_ail<-assign_date_time_days(price_ail)
price_ail<-assign_peaks(price_ail)


wind_forecast<-function(){
  wind_fcast<-read.csv("http://ets.aeso.ca/Market/Reports/Manual/Operations/prodweb_reports/wind_power_forecast/WPF_LongTerm.csv",skip=4)
  wind_fcast<- wind_fcast[-seq(nrow(wind_fcast),nrow(wind_fcast)-1),]
  wind_fcast[,2]<-as.numeric(as.character(wind_fcast[,2]))
  wind_fcast[,1]<-as.POSIXct(as.character(wind_fcast[,1]))
  names(wind_fcast)[1]<-"Date"
  wind_fcast
}
wind_fcast<-wind_forecast()

solar_forecast<-function(){
  read_csv("http://ets.aeso.ca/Market/Reports/Manual/Operations/prodweb_reports/wind_solar_forecast/solar_rpt_longterm.csv",skip=0)%>%clean_names()%>%rename(date=1)
  #solar_fcast
}

solar_fcast<-solar_forecast()

gen_mix <- read.csv(paste(nrg_folder,"AESO_Gen_Mix.csv",sep="/"),skip = 2,header = TRUE, stringsAsFactors=FALSE)
names(gen_mix)<-c("date","Coal","Hydro","Trade","Natural Gas","Other (incl. biomass)","Solar","Wind")
gen_mix<-gen_mix%>%mutate(Trade=-1*Trade)
gen_mix<-gen_mix%>%pivot_longer(-date,names_to="Source",values_to="gen")%>%
  mutate(date=dmy_hms(date),
         #change factor order to sort by avg gen
         Source=factor(Source, levels=c("Wind","Solar","Hydro","Other (incl. biomass)","Natural Gas","Coal","Trade")))
gen_mix_plot<-ggplot(gen_mix,aes(date,gen, group=Source, fill = Source)) +
  geom_area(position="stack")+
  weekly_small()+
  scale_x_datetime(expand = c(0,0),date_breaks="3 days",date_labels = "%b\n%d")+
  #scale_y_continuous(limits=c(0,5990),expand = c(0,0))+
  scale_fill_manual("",values=c(colors_ua10()[1],colors_ua10()[2],colors_ua10()[4],colors_ua10()[3],colors_ua10()[8],colors_ua10()[5],colors_ua10()[6]))+
  guides(fill = guide_legend(nrow = 1, keywidth = 1.25))+
  #theme(legend.position = "none")+
  labs(x="Year",y="Supply (MW)",
       title="Supply Mix, Past 30 Days",
       caption="Source: AESO Data, accessed via NRGStream")
#gen_mix_plot
gen_mix_plot_short<-ggplot(gen_mix%>%filter(date(date)==today()),aes(date,gen, group=Source, fill = Source)) +
  geom_area(position="stack")+
  weekly_small()+
  scale_x_datetime(expand = c(0,0),date_breaks="3 days",date_labels = "%b\n%d")+
  #scale_y_continuous(limits=c(0,5990),expand = c(0,0))+
  scale_fill_manual("",values=c(colors_ua10()[1],colors_ua10()[2],colors_ua10()[4],colors_ua10()[3],colors_ua10()[8],colors_ua10()[5],colors_ua10()[6]))+
  guides(fill = guide_legend(nrow = 1, keywidth = 1.25))+
  #theme(legend.position = "none")+
  labs(x="",y="Supply (MW)",
       title="Alberta Supply Mix",
       caption="Source: AESO Data, accessed via NRGStream")


renew_plot<-
  ggplot(gen_mix%>%filter(!Source %in% c("Natural Gas","Coal", "Trade")),aes(date,gen, group=Source, fill = Source)) +
  geom_area(position="stack")+
  weekly_small()+
  scale_x_datetime(expand = c(0,0),date_breaks="3 days",date_labels = "%b\n%d")+
  #scale_y_continuous(limits=c(0,5990),expand = c(0,0))+
  scale_fill_manual("",values=c(colors_ua10()[1],colors_ua10()[2],colors_ua10()[4],colors_ua10()[3],colors_ua10()[8],colors_ua10()[5],colors_ua10()[6]))+
  guides(fill = guide_legend(nrow = 1, keywidth = 1.25))+
  #theme(legend.position = "none")+
  labs(x="",y="Renewable Supply (MW)",
       title="Alberta Renewable Supply Mix",
       caption="Source: AESO Data, accessed via NRGStream")

wind_fcast_plot<-ggplot(wind_fcast) +
  geom_ribbon(aes(x = Date, ymin =MIN , ymax = MAX,fill="B"))+
  geom_line(aes(Date,MOST.LIKELY,colour="A"), size=1.25)+
  weekly_small()+
  scale_x_datetime(expand = c(0,0),labels = date_format("%b %d\n%H:00"),breaks = "1 day")+
  #scale_y_continuous(limits=c(0,5990),expand = c(0,0))+
  scale_color_manual("",labels=c("Most Likely Wind Generation"),values="#007C41")+
  scale_fill_manual("",labels=c("Range of Potential Wind Generation"),values=alpha("#007C41",.3))+
  guides(fill = guide_legend(nrow = 1))+
  #theme(legend.position = "none")+
  labs(x="",y="Average Hourly Generation (MW)",
       title="Wind Generation Forecast",
       caption="Source: AESO Data")



solar_fcast_plot<-ggplot(solar_fcast) +
  geom_ribbon(aes(x = date, ymin =min , ymax = max,fill="B"))+
  geom_line(aes(date,most_likely,colour="A"), linewidth=1.25)+
  weekly_small()+
  scale_x_datetime(expand = c(0,0),labels = date_format("%b %d\n%H:00"),breaks = "1 day")+
  #scale_y_continuous(limits=c(0,5990),expand = c(0,0))+
  scale_color_manual("",labels=c("Most Likely Solar Generation"),values="#007C41")+
  scale_fill_manual("",labels=c("Range of Potential Solar Generation"),values=alpha("#FFDB05",.8))+
  guides(fill = guide_legend(nrow = 1))+
  #theme(legend.position = "none")+
  labs(x="",y="Average Hourly Generation (MW)",
       title="Solar Generation Forecast",
       caption="Source: AESO Data")



#solar_fcast_plot

#graph of prices and loads from aeso
period_start<-max(price_ail$start_date)-weeks(1)
end_date<-max(price_ail$start_date)

peak_data<-price_ail %>% filter(date>=period_start,date<=end_date) %>%
  mutate(peak_ail=ifelse(on_peak,actual_ail,NA),
         peak_price=ifelse(on_peak,actual_posted_pool_price,NA),
  )

max_price<-max(max(peak_data$actual_posted_pool_price))


#previous week prices

top_panel<-ggplot(peak_data) +
  geom_line(aes(time,actual_posted_pool_price,colour="basic"),size=1.25)+
  geom_line(aes(time,peak_price,colour="peak"),size=1.25)+
  #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
  scale_color_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_fill_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_x_datetime(expand=c(0,0),breaks="1 day",labels = date_format("%H:00\n%b %d\n%Y",tz="America/Denver"))+
  scale_y_continuous(limits = c(0,max_price))+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt"))
  )+
  labs(y="Hourly Pool Price ($/MWh)",x="",
       title=paste("Alberta Hourly Wholesale Power Prices",sep=""),
       subtitle=paste(month.name[month(period_start)]," ",day(period_start),", ",year(period_start)," to ",month.name[month(end_date)]," ",day(end_date), ", ",year(end_date),sep="")
  )
bottom_panel<-ggplot(peak_data) +
  geom_line(aes(time,actual_ail,colour="basic"),size=1.25)+
  geom_line(aes(time,peak_ail,colour="peak"),size=1.25)+
  scale_color_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_fill_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_x_datetime(expand=c(0,0),breaks="1 day",labels = date_format("%H:00\n%b %d\n%Y",tz="America/Denver"))+
  scale_y_continuous()+
  theme_minimal()+
  theme(legend.position="bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="Internal Load (MW)",x="")
#caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(top_panel)



grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                           plot.caption = element_text(size = 12, face = "italic"),
                                           plot.title = element_text(size = 14,face = "bold"),
                                           plot.subtitle = element_text(size = 12, face = "italic"),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = 10,face = "bold"),
                                           axis.text = element_text(size = 10,face = "bold", colour="black"),
                                           axis.text.x = element_blank()
),
bottom_panel + theme(legend.position="none",
                     plot.caption = element_text(size = 12, face = "italic"),
                     plot.title = element_text(size = 14,face = "bold"),
                     plot.subtitle = element_text(size = 12, face = "italic"),
                     panel.grid.minor = element_blank(),
                     text = element_text(size = 10,face = "bold"),
                     axis.text = element_text(size = 10,face = "bold", colour="black")
),
ncol=1,heights=c(3,3)),
mylegend, nrow=2,heights=c(10, 1),bottom =text_grob(
  "Source: AESO data, graph by Andrew Leach. Peak periods are between 7am and 11pm other than on statutory holidays or Sundays.",
  face = "italic", color = "black",size=10,just="center",lineheight = 1
)
)


renew_mix <- read.csv(paste(nrg_folder,"AESO_Gen_Mix_coal.csv",sep="/"),skip = 2,header = TRUE, stringsAsFactors=FALSE)
names(renew_mix)<-c("date","AIL","BC_Exp","Coal","Hydro","AB_Dem","MT_Exp","AB_Net_Gen","SK_Exp","Solar","Total","Wind","BCH_AB","BCH_US","BPA_BCH")
renew_mix<-renew_mix%>%
  mutate(date=dmy_hms(date),
         ail_net_ws=AIL-Wind-Solar-Hydro)%>%
  #pivot_longer(-date,names_to="source",values_to="gen")%>%
  #mutate(
  #       #change factor order to sort by avg gen
  #       source=as_factor(source),
  #       source=fct_relevel(source,c("Wind","Solar","AIL","ail_net_ws"))
  #)
  I()

load("data/ab_power_temps.RData")
temps_power<-temps_power %>% 
  filter(he!="02*")%>%
  mutate(he=as.numeric(he),
         he=ifelse(he==24,0,he), #swap the 24s to zeros
         date=ymd_hm(paste(date," ",he,":00",sep="")))


renew_mix <-renew_mix %>% left_join(temps_power)%>%
  fill("temp_YEG","temp_YMM", "temp_YYC", "hdd_YEG"  ,"hdd_YMM",  "hdd_YYC" , "cdd_YEG",  "cdd_YMM" , "cdd_YYC" )

top_panel<-
  ggplot(renew_mix%>%filter(date>ymd("2022-12-18"),date<ymd("2022-12-18")+days(5))) +
  geom_line(aes(date,AIL,colour="Alberta Internal Load"),size=1.25)+
  geom_line(aes(date,ail_net_ws,colour="Alberta Internal Load Net WWS"),size=1.25)+
  scale_color_manual("",values = c("black",blakes_blue))+
  scale_fill_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_x_datetime(expand=c(0,0),breaks="12 hours",labels = date_format("%H:00\n%b %d\n%Y"))+
  scale_y_continuous()+
  theme_minimal()+
  theme(legend.position="bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="System Load (MW)",x="")

# 24s need to be 0 on the he


bottom_panel<-
  ggplot(renew_mix%>%filter(date>ymd("2022-12-18"),date<ymd("2022-12-18")+days(5))) +
  geom_line(aes(date,temp_YEG,colour="Edmonton International Airport"),size=1.25)+
  geom_line(aes(date,temp_YYC,colour="Calgary International Airport"),size=1.25)+
  scale_color_manual("",values = c("black",blakes_blue))+
  #scale_fill_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_x_datetime(expand=c(0,0),breaks="12 hours",labels = date_format("%H:00\n%b %d\n%Y"))+
  scale_y_continuous()+
  theme_minimal()+
  theme(legend.position="bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="Air Temperature",x="")


combo<-grid.arrange(arrangeGrob(top_panel + theme(#legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                           plot.caption = element_text(size = 12, face = "italic"),
                                           plot.title = element_text(size = 14,face = "bold"),
                                           plot.subtitle = element_text(size = 12, face = "italic"),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = 10,face = "bold"),
                                           axis.text = element_text(size = 10,face = "bold", colour="black"),
                                           #axis.text.x = element_blank()
),
bottom_panel + theme(#legend.position="none",
  legend.margin=margin(c(0,0,0,0),unit="cm"),
  legend.text = element_text(colour="black", size = 14, face = "bold"),
  plot.caption = element_text(size = 12, face = "italic"),
  plot.title = element_text(size = 14,face = "bold"),
  plot.subtitle = element_text(size = 12, face = "italic"),
  panel.grid.minor = element_blank(),
  text = element_text(size = 10,face = "bold"),
  axis.text = element_text(size = 10,face = "bold", colour="black"),
  axis.text.y = element_text(margin=margin(c(0,0,0,.5),unit="cm")),
  #axis.text.x = element_blank()
),
ncol=1,heights=c(3,3)))

ggsave("testing.png",plot = combo,width=14.5,height=7,dpi=200,bg="white")


#summer heat dome

top_panel<-
  ggplot(renew_mix%>%filter(date>ymd("2023-06-02"),date<ymd("2023-06-02")+days(6))) +
  geom_line(aes(date,AIL,colour="Alberta Internal Load"),size=1.25)+
  geom_line(aes(date,ail_net_ws,colour="Alberta Internal Load Net WWS"),size=1.25)+
  scale_color_manual("",values = c("black",blakes_blue))+
  scale_fill_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_x_datetime(expand=c(0,0),breaks="12 hours",labels = date_format("%H:00\n%b %d\n%Y"))+
  scale_y_continuous()+
  theme_minimal()+
  theme(legend.position="bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="System Load (MW)",x="")

# 24s need to be 0 on the he


bottom_panel<-
  ggplot(renew_mix%>%filter(date>ymd("2023-06-02"),date<ymd("2023-06-02")+days(6))) +
  geom_line(aes(date,temp_YEG,colour="Edmonton International Airport"),size=1.25)+
  geom_line(aes(date,temp_YYC,colour="Calgary International Airport"),size=1.25)+
  scale_color_manual("",values = c("black",blakes_blue))+
  #scale_fill_manual("",values = colors_ua10(),labels=c("Off-peak period","Peak period"))+
  scale_x_datetime(expand=c(0,0),breaks="12 hours",labels = date_format("%H:00\n%b %d\n%Y"))+
  scale_y_continuous()+
  theme_minimal()+
  theme(legend.position="bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+    labs(y="Air Temperature",x="")


combo<-grid.arrange(arrangeGrob(top_panel + theme(#legend.position="none",
  legend.margin=margin(c(0,0,0,0),unit="cm"),
  legend.text = element_text(colour="black", size = 14, face = "bold"),
  plot.caption = element_text(size = 12, face = "italic"),
  plot.title = element_text(size = 14,face = "bold"),
  plot.subtitle = element_text(size = 12, face = "italic"),
  panel.grid.minor = element_blank(),
  text = element_text(size = 10,face = "bold"),
  axis.text = element_text(size = 10,face = "bold", colour="black"),
  #axis.text.x = element_blank()
),
bottom_panel + theme(#legend.position="none",
  legend.margin=margin(c(0,0,0,0),unit="cm"),
  legend.text = element_text(colour="black", size = 14, face = "bold"),
  plot.caption = element_text(size = 12, face = "italic"),
  plot.title = element_text(size = 14,face = "bold"),
  plot.subtitle = element_text(size = 12, face = "italic"),
  panel.grid.minor = element_blank(),
  text = element_text(size = 10,face = "bold"),
  axis.text = element_text(size = 10,face = "bold", colour="black"),
  axis.text.y = element_text(margin=margin(c(0,0,0,.5),unit="cm")),
  #axis.text.x = element_blank()
),
ncol=1,heights=c(3,3)))

ggsave("heat.png",plot = combo,width=14.5,height=7,dpi=200,bg="white")
