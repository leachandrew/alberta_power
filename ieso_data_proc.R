#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())

library(grid)
library(gridExtra)
#library(ggpubr)
#library(gganimate)
library(timeDate)
library(broom)

#seasons
if(!exists("ajl_hourly", mode="function")) source("../andrew_base.R")

#seasons
#if(!exists("getSeason", mode="function")) source("get_season.R")



#METERED VOLUMES AND CAPABILITIES

get_ieso_reports<- function(start_date, end_date) {
  
  
#current data
  date_list<-as.character(seq.Date(ymd("2019-05-01"),Sys.Date(),"1 month"))
  ieso_scrapes<-list()
  count<-1
  for(date_start in date_list[1:2]){
    link<-paste("http://reports.ieso.ca/public/GenOutputCapabilityMonth/PUB_GenOutputCapabilityMonth_",format(ymd(date_start),"%Y"),format(ymd(date_start),'%m'),".csv",sep="")
    loc_file=paste("ieso_",date_start,".csv",sep="")
    download.file(link,destfile=loc_file,mode="wb")
    ieso_scrapes[[count]]<-read_csv(loc_file,skip=3,col_names = TRUE) %>% clean_names()
    count<-count+1
  }
  ieso_new<-bind_rows(ieso_scrapes)%>%pivot_longer(cols=-c(delivery_date,generator,fuel_type,measurement),values_drop_na = T)%>%
    pivot_wider(id_cols = -c(measurement, value), names_from = measurement, values_from = value) %>% clean_names() %>%
    mutate(name=gsub("hour_","",name),name=as.numeric(name))%>% rename(date=delivery_date,hour=name) 
  #older data
  
  year_seq<- c(2010,2018)
  links<-paste("http://ieso.ca/-/media/Files/IESO/Power-Data/data-directory/GOC-",year_seq,".xlsx?la=en",sep="")
  links<-c(links,"http://ieso.ca/-/media/Files/IESO/Power-Data/data-directory/GOC-2019-Jan-April.xlsx?la=en")
  old_ieso<-list()
  count<-1
  for(link_temp in links){
    download.file(links[1],destfile="temp.xlsx",mode="wb")
    ieso_output<-read_excel("temp.xlsx",sheet = "Output") %>% select(-grep("\\.\\.\\.",colnames(.))) %>% #read and drop missing
    clean_names()%>% pivot_longer(cols = -c(date,hour),names_to = "plant", values_to = "output",values_drop_na = T)  
    ieso_capability<-read_excel("temp.xlsx",sheet = "Capability") %>% select(-grep("\\.\\.\\.",colnames(.)))%>%  #read and drop missing
    clean_names()%>% pivot_longer(cols = -c(date,hour),names_to = "generator", values_to = "capability",values_drop_na = T)  
    ieso_xlsx<-merge(ieso_output,ieso_capability)
    old_ieso[[count]]<-ieso_xlsx
    count<-count+1
  }
  
  #headers<- gsub("\"-\",", "", headers)
  data<-gsub("\"-\",", "",paste(c(paste(test[8:9], collapse=","), test[13:length(test)]), collapse="\n"))
  clean_data<-read.csv(text=data,header = TRUE, stringsAsFactors=FALSE)
  clean_data<-janitor::clean_names(clean_data)%>%   tbl_df()
  clean_data$date<-start_date
  
  #clean_data$he<-hour(clean_data$date)
  #clean_data$time<-clean_data$date
  #clean_data$date<-as.Date(clean_data$time)
  
  #names(clean_data)<-c(read.csv(text=headers,header = FALSE, stringsAsFactors=FALSE),"date")
  return(clean_data)
}


years<-seq(2015,2020)
ieso_fuel<-list()
count<-1
for(year_id in years){
  ieso_fuel[[count]]<-read_excel(paste("ieso_",year_id,"_fuel.xlsx",sep=""))
  count<-count+1
  }
 ieso_fuel<-bind_rows(ieso_fuel) %>% clean_names()
 
 names(ieso_fuel)<-gsub("ns1_","",names(ieso_fuel))
 
 ieso_fuel <- ieso_fuel %>% select(year=delivery_year,day,hour,fuel,output) %>% mutate(fuel=str_to_title(fuel),
                                                                                       time=ymd_h(paste(year,"-",month(day),"-",day(day)," ",hour,sep="")))
 
 ieso_day <- ieso_fuel %>% group_by(year, day, fuel) %>% summarize(output=mean(output,na.rm = T),max=max(output,na.rm = T),min=min(output,na.rm = T))
 
 ieso_month <- ieso_fuel %>% mutate(month=month(day))%>%
   mutate(fuel=as_factor(fuel),fuel=fct_other(fuel,drop=c("Wind","Biofuel","Solar"))) %>% 
   group_by(year, month, fuel) %>% summarize(output=mean(output,na.rm = T),max=max(output,na.rm = T),min=min(output,na.rm = T))%>%
   mutate(day=ymd(paste(year,month,15,sep="-"))) 
 
 
 gen_plain <- 
   ieso_month %>% #filter(date<ymd(paste(year(max(date)),month(max(date)),1,sep="-"))) %>%
   mutate(fuel=as_factor(fuel))%>%#,
          # Relevel to the end
          #Plant_Fuel=fct_relevel(Plant_Fuel, "TRADE", after = Inf)) %>%
   ggplot(aes(day,output, col = fuel,fill = fuel)) +
   #geom_area(position = "stack")+
   geom_line()+
   #annotate("text", x = as.Date("2014-8-1")-days(20), y = 5600, label = "SGER in place at\n12% and $15/tonne\nsince July 2007",size=3.8)+  
   #annotate("text", x = as.Date("2016-1-1")-days(20), y = 5600, label = "SGER changed to\n15% and $20/tonne\nJan 2016",size=3.8)+  
   #annotate("text", x = as.Date("2017-1-1")-days(20), y = 5600, label = "SGER changed to\n20% and $30/tonne\nJan 2017",size=3.8)+  
   #annotate("text", x = as.Date("2018-1-1")-days(20), y = 5600, label = "SGER changed to\nCCIR at $30/tonne\nJan 2018",size=3.8)+  
   
   slide_theme()+
   scale_x_date(expand = c(0,0),date_labels = "%b\n%Y",date_breaks = "6 months")+
   scale_y_continuous(limits=c(0,12000),expand = c(0,0))+
   scale_color_manual("",values=colors_tableau10())+
   theme(legend.position = "none")+
   labs(x="",y="Average Hourly Generation (MW)",
        #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
        title="Ontario Generation by Fuel (MW, 2015-2020)",
        caption="Source: IESO Data")
 
 p<-gen_plain+
   annotate("text", x = as.Date("2020-03-11")-days(60), y =5600, label = "COVID\nCutoff",size=4)+  
   annotate("rect", fill = "black", alpha = 1, 
            xmin = as.Date("2020-03-11"), xmax =as.Date("2020-3-12"),
            ymin = 0, ymax = 11500)
 p
 
 
 ieso_zoom<-ieso_day %>% filter(day>=ymd("2019-01-01")) %>% ungroup() %>%
   mutate(day=as_date(day),fuel=as_factor(fuel),fuel=fct_other(fuel,drop=c("Wind","Biofuel","Solar"))) %>% 
   group_by(year, day, fuel) %>% summarize(output=mean(output,na.rm = T),max=max(output,na.rm = T),min=min(output,na.rm = T))
   
 
  covid_mid<-ymd("2020-03-11")+days((max(ieso_zoom$day)-ymd("2020-03-11"))/2)
 covid_mid_lag<-ymd("2020-03-11")+days((max(ieso_zoom$day)-ymd("2020-03-11"))/2)-years(1)
 
 
 p_zoom <- 
   ieso_zoom%>%
   # Relevel to the end
   #Plant_Fuel=fct_relevel(Plant_Fuel, "TRADE", after = Inf)) %>%
   ggplot(aes(day,output, col = fuel,fill = fuel)) +
   #geom_area(position = "stack")+
   geom_line()+
   #annotate("text", x = as.Date("2014-8-1")-days(20), y = 5600, label = "SGER in place at\n12% and $15/tonne\nsince July 2007",size=3.8)+  
   #annotate("text", x = as.Date("2016-1-1")-days(20), y = 5600, label = "SGER changed to\n15% and $20/tonne\nJan 2016",size=3.8)+  
   #annotate("text", x = as.Date("2017-1-1")-days(20), y = 5600, label = "SGER changed to\n20% and $30/tonne\nJan 2017",size=3.8)+  
   #annotate("text", x = as.Date("2018-1-1")-days(20), y = 5600, label = "SGER changed to\nCCIR at $30/tonne\nJan 2018",size=3.8)+  
   
   slide_theme()+
   scale_x_date(expand = c(0,0),date_labels = "%b\n%Y",date_breaks = "2 months")+
   scale_y_continuous(limits=c(0,12500),expand = c(0,0))+
   scale_color_manual("",values=colors_tableau10())+
   theme(legend.position = "none")+
   labs(x="",y="Average Hourly Generation (MW)",
        #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
        title="Ontario Transmission-Connected Generation by Fuel (MW, 2019-2020)",
        caption="Source: IESO Data")+
   annotate("text", x = covid_mid, y =6800, label = "COVID\nperiod",size=4,hjust=0.5)+
   annotate("rect", fill = "grey80", alpha = .3, 
            xmin = as.Date("2020-03-11"), xmax =as.Date("2020-06-08"),
            ymin = 0, ymax = 12499)+
   annotate("rect", fill = "grey80", alpha = .3, 
            xmin = as.Date("2019-03-11"), xmax =as.Date("2019-06-08"),
            ymin = 0, ymax = 12499)+
 annotate("text", x = covid_mid_lag, y =6800, label = "COVID\nperiod\nlast year",size=4,hjust=0.5)  
   
 direct_labels <- ieso_month %>% 
   group_by(fuel) %>%
   summarize(
     x = last(day)-months(1), 
     y = last(output)
   )
 #direct_labels$y<-c(4500,3000,1000,400)
 
 library(patchwork)
 library(cowplot)
 
 direct_labels_axis <- axis_canvas(p, axis = "y") +
   geom_text(
     data = direct_labels, 
     aes(y = y, label = fuel), 
     x = 0.06, 
     hjust = 0, 
     size = 5, 
     col = colors_tableau10()[1:4]
   )
 
 p_direct_labels <- insert_yaxis_grob(p, direct_labels_axis)
 ggdraw(p_direct_labels)
 ggsave("on_tc_supply.png",width=12,height = 8)
 
 
 
 direct_labels <- ieso_zoom %>% 
   group_by(fuel) %>%
   summarize(
     x = last(day)-months(1), 
     y = last(output)
   )
 direct_labels$y[1]<- 2000 #raise gas
 
 
 direct_labels_axis <- axis_canvas(p_zoom, axis = "y") +
   geom_text(
     data = direct_labels, 
     aes(y = y, label = fuel), 
     x = 0.06, 
     hjust = 0, 
     size = 5, 
     col = colors_tableau10()[1:4]
   )
  p_zoom_direct_labels <- insert_yaxis_grob(p_zoom, direct_labels_axis)
 ggdraw(p_zoom_direct_labels)
 ggsave("on_zoom.png",width=12,height = 8)
 
 
 
 
 
 #supply-side regressions
 
# ieso_fuel <- ieso_fuel %>% select(year=delivery_year,day,hour,fuel,output) %>% mutate(fuel=str_to_title(fuel),
 #                                                                                      time=ymd_h(paste(year,"-",month(day),"-",day(day)," ",hour,sep="")))
 
 df1<-ieso_fuel %>% select(time,fuel,MW=output)
 
 #load temps
 
 temp <- read_csv("full_weather_data_toronto_hourly_allstations.csv") %>% mutate(Province="AB")%>%
   filter(year>=2015) %>%
   mutate(date=paste(year,month,day,sep="-"),
          time=paste(date,time,sep=" "),
          time=ymd_hms(time),
          HD=ifelse(temp<=14,14-temp,0),
          CD=ifelse(temp>14,temp-14,0),
          HD2=HD*HD,
          CD2=CD*CD) %>%
   select(time,temp,CD,HD,CD2,HD2)
 
 
 # Merge load and temperature
 df <- left_join(df1,temp, by=c("time")) %>%
   drop_na() %>%
   mutate(year=year(time),month=month(time),Date=as.Date(time))%>%
   mutate(Date_Group = ifelse(year==2020,as.character(as.Date(time)),0))%>% assign_peaks()%>%
   rename(Plant_Fuel=fuel)%>%
   mutate(log_MW=log(MW),
          HE=hour(time),
          DOY=wday(time),
          Weekday=ifelse(DOY %in% c(7,1),1,0),   #wday=7 is Saturday, wday=1 is Sunday
          WOY=week(time),
          DOY=yday(time),
          Year=year(time),
          Month=month(time),
          Season=ifelse((Month>=4 & Month<=10),"Summer","Winter"),
          Quarter=ifelse(Month<=3,"Q1",ifelse(Month<=6,"Q2",ifelse(Month<=9,"Q3","Q4")))) 
 
 
 #rm(all_vols,df1,temp)
 
 # Summary stat
 stats <- df %>%
   #group_by(Province) %>%
   summarise(min_MW=min(MW),
             mean_MW=mean(MW),
             max_MW=max(MW),
             min_temp=min(temp),
             mean_temp=mean(temp),
             max_temp=max(temp))
 
 #supply-side regressions
 
 #blake's regression: m1 <- lm(log_MW ~ factor(Date_Group) + factor(WeekdayNotHol)*factor(HE) + factor(WOY) + CD + HD + CD2 + HD2, d)
 
 
 by_fuel <-df %>% mutate(sample=runif(n(),0, 1))%>% filter(sample<=.1) %>% #10% subsample
   mutate(covid=ifelse(Date>=as.Date("2020-03-11"),1,0))%>%
   mutate(Date_Group = ifelse(covid==1,as.character(as.Date(time)),0))%>% #regression will have zero as the base case factor
   nest(-Plant_Fuel) %>% 
   mutate(
     fit = map(data, ~ lm(MW ~ factor(year)+factor(WOY)+factor(Month)+factor(Date_Group)+factor(HE)*factor(on_peak) + CD + HD + CD2 + HD2, data = .x)),
     tidied = map(fit, tidy),
     glanced = map(fit, glance)
   ) %>% 
   unnest(tidied) 
 
 proc<-by_fuel %>% select(-data,-fit,-glanced)%>%
   filter(str_detect(term,"Date_Group")) %>%
   mutate(date_label=str_remove(term,fixed("factor(Date_Group)")),
          date_label=ymd(date_label))%>%
   group_by(Plant_Fuel)%>%
   mutate(
     #baseline=mean(estimate[date_label<ymd("2020-03-11")]),
     #norm.estimate=estimate-baseline,
     #norm.estimate.7ma=roll::roll_mean(norm.estimate,7),
     SE.upper=estimate+1.96*std.error,
     SE.lower=estimate-1.96*std.error)
 
 
 covid_mid<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)
 covid_mid_lag<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)-years(1)
 
 proc %>% #filter(Plant_Type!="WIND")%>%
   ggplot(aes(date_label,estimate, col = Plant_Fuel,fill = Plant_Fuel)) +
   #geom_area(position = "stack")+
   geom_line(size=1)+
   geom_ribbon(aes(date_label,ymax=SE.upper,ymin=SE.lower,fill = Plant_Fuel),alpha=.2,size=0)+
   #annotate("text", x = as.Date("2014-8-1")-days(20), y = 5600, label = "SGER in place at\n12% and $15/tonne\nsince July 2007",size=3.8)+  
   #annotate("text", x = as.Date("2016-1-1")-days(20), y = 5600, label = "SGER changed to\n15% and $20/tonne\nJan 2016",size=3.8)+  
   #annotate("text", x = as.Date("2017-1-1")-days(20), y = 5600, label = "SGER changed to\n20% and $30/tonne\nJan 2017",size=3.8)+  
   #annotate("text", x = as.Date("2018-1-1")-days(20), y = 5600, label = "SGER changed to\nCCIR at $30/tonne\nJan 2018",size=3.8)+  
   facet_wrap(~Plant_Fuel,scales = "free_y")+
   slide_theme()+
   scale_x_date(date_labels = "%d %b\n%Y",date_breaks = "1 months",expand=c(0,0) )+
   expand_limits(x = as.Date(c("2020-02-28", "2020-06-01")))+
   scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
   scale_color_manual("",values=colors_tableau10())+
   scale_fill_manual("",values=colors_tableau10())+
   #theme(legend.position = "none")+
   labs(x="Year",y="Change in Average Hourly Generation (MW)",
        #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
        title="Adjusted Drops in Generation by Plant Type (MW, 2020)",
        caption="Source: IESO Data, analysis by Andrew Leach")+
   #annotate("text", x = covid_mid, y =0, label = "COVID\nperiod",size=3.25,hjust=0.5)+
   #annotate("rect", fill = "grey80", alpha = .3, 
   #         xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
   #         ymin = -Inf, ymax = Inf)
   NULL
 ggsave("covid_ieso_prelim_supply.png",width=16,height = 9)
 
 
 covid_level <-df %>% mutate(sample=runif(n(),0, 1))%>% filter(sample<=.1) %>% #10% subsample
 mutate(covid=ifelse(Date>=as.Date("2020-03-11"),1,0))%>%
   #mutate(Date_Group = ifelse(covid==1,as.character(as.Date(time)),0))%>%
   nest(-Plant_Fuel) %>% 
   mutate(
     fit = map(data, ~ lm(MW ~ factor(year)+factor(WOY)+factor(Month)+covid+factor(HE)*factor(on_peak) + CD + HD + CD2 + HD2, data = .x)),
     tidied = map(fit, tidy),
     glanced = map(fit, glance)
   ) %>% 
   unnest(tidied) 
 
 proc<-covid_level %>% select(-data,-fit,-glanced)%>%
   filter(str_detect(term,"covid")) %>%
   mutate(date_label=str_remove(term,fixed("factor(Date_Group)")),
          date_label=ymd(date_label))%>%
   group_by(Plant_Fuel)%>%
   mutate(
     #baseline=mean(estimate[date_label<ymd("2020-03-11")]),
     #norm.estimate=estimate-baseline,
     #norm.estimate.7ma=roll::roll_mean(norm.estimate,7),
     SE.upper=estimate+1.96*std.error,
     SE.lower=estimate-1.96*std.error)
 
 
 covid_mid<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)
 covid_mid_lag<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)-years(1)
 
 proc %>% #filter(Plant_Type!="WIND")%>%
   ggplot(aes(date_label,estimate, col = Plant_Fuel,fill = Plant_Fuel)) +
   #geom_area(position = "stack")+
   geom_line(size=1)+
   geom_ribbon(aes(date_label,ymax=SE.upper,ymin=SE.lower,fill = Plant_Fuel),alpha=.2,size=0)+
   #annotate("text", x = as.Date("2014-8-1")-days(20), y = 5600, label = "SGER in place at\n12% and $15/tonne\nsince July 2007",size=3.8)+  
   #annotate("text", x = as.Date("2016-1-1")-days(20), y = 5600, label = "SGER changed to\n15% and $20/tonne\nJan 2016",size=3.8)+  
   #annotate("text", x = as.Date("2017-1-1")-days(20), y = 5600, label = "SGER changed to\n20% and $30/tonne\nJan 2017",size=3.8)+  
   #annotate("text", x = as.Date("2018-1-1")-days(20), y = 5600, label = "SGER changed to\nCCIR at $30/tonne\nJan 2018",size=3.8)+  
   facet_wrap(~Plant_Fuel,scales = "free_y")+
   slide_theme()+
   scale_x_date(date_labels = "%d %b\n%Y",date_breaks = "1 months",expand=c(0,0) )+
   expand_limits(x = as.Date(c("2020-02-28", "2020-06-01")))+
   scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
   scale_color_manual("",values=colors_tableau10())+
   scale_fill_manual("",values=colors_tableau10())+
   #theme(legend.position = "none")+
   labs(x="Year",y="Change in Average Hourly Generation (MW)",
        #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
        title="Adjusted Drops in Generation by Plant Type (MW, 2020)",
        caption="Source: IESO Data, analysis by Andrew Leach")+
   #annotate("text", x = covid_mid, y =0, label = "COVID\nperiod",size=3.25,hjust=0.5)+
   #annotate("rect", fill = "grey80", alpha = .3, 
   #         xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
   #         ymin = -Inf, ymax = Inf)
   NULL
 ggsave("covid_ieso_prelim_supply.png",width=16,height = 9)
 
 
 