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


#seasons
if(!exists("ajl_hourly", mode="function")) source("../andrew_base.R")

#seasons
if(!exists("getSeason", mode="function")) source("get_season.R")



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
 
 