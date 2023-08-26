source("power_paper_base.R")
source("cdn_weather.R")
source("aeso_scrapes.R")
source("merit_scripts.R")
source("cdn_weather.R")

load(file="data/metered_vols_data.Rdata" ) 
  #all_vols<-all_vols %>% filter(year<2022)
  #all_vols<-update_vols(all_vols)
  #save(all_vols,file="data/metered_vols_data.Rdata" ) 
  #isolate renewable (non-hydro and biomass) volumes from metered volumes - the ones that default bid to zero
  
  renewables<- all_vols %>% 
    filter(Plant_Type=="WIND"| Plant_Type=="SOLAR") %>% select(date,he,asset_id,Plant_Type,vol,time,Capacity)%>%
    #switch so that we have same data format as merit data
    I()
  
update_forecasts()  
load(file="data/forecast_data.Rdata") 
#forecast_data <- forecast_data %>% filter (he!="02*")

  #find first non-zero date
  start_dates<-renewables %>% group_by(asset_id)%>%
    filter(vol>0)%>%
    summarize(start_date=min(date))
  
  renewables <- renewables %>% left_join(start_dates) %>% group_by(asset_id)%>%
    mutate(in_mkt=(date>start_date))%>%
    filter(in_mkt)
  
  
  save(renewables, file="data/AB_renew.RData")  

  load(file="data/AB_renew.Rdata" ) 
  
  
  plant_sum<-renewables%>%
    filter(start_date<ymd("2022-12-31"))%>%
    left_join(forecast_data%>%select(date,he,price=actual_posted_pool_price))%>%
    mutate(year=year(date))%>%
    group_by(year,asset_id,Plant_Type,start_date) %>% summarize(rev=sum(vol*price,na.rm=T),
                                                     vol=sum(vol,na.rm = T),
                                                     avg_rev=rev/vol,
                                                     avail=sum(Capacity,na.rm = T),
                                    cap_fac=vol/avail ) %>%ungroup()%>%
    group_by(year,Plant_Type)%>%
    mutate(type_cf=sum(vol)/sum(avail),type_avg_rev=sum(rev)/sum(vol))%>%
    group_by(asset_id)%>%
    mutate(asset_cf=sum(vol)/sum(avail),asset_avg_rev=sum(rev)/sum(vol))
  
#  save(hourly_renew, file="data/hourly_renew.RData")  
  
  
  
    
  hourly_renew<-renewables%>%
    group_by(date,he) %>% summarize(renew_gen=sum(vol,na.rm = T),
                                    wind_gen=sum(vol*(Plant_Type=="WIND"),na.rm = T),
                                    solar_gen=sum(vol*(Plant_Type=="SOLAR"),na.rm = T),
                                    cap_fac=renew_gen/sum(Capacity,na.rm = T),
                                    solar_cap_fac=solar_gen/sum(Capacity*(Plant_Type=="SOLAR"),na.rm = T),
                                    wind_cap_fac=wind_gen/sum(Capacity*(Plant_Type=="WIND"),na.rm = T),
                                    solar_cap=sum(Capacity*(Plant_Type=="SOLAR"),na.rm = T),
                                    wind_cap=sum(Capacity*(Plant_Type=="WIND"),na.rm = T),
                                    ) %>%ungroup()
                              
  save(hourly_renew, file="data/hourly_renew.RData")  
  
load("data/renew_vols.RData")
load("data/hourly_renew.RData")


load("data/ab_power_temps.RData")


hourly_renew<-hourly_renew%>%left_join(temps_power)

hourly_renew %>% 
  mutate(year=factor(year(date)))%>%
  group_by(year)%>%
  arrange(renew_gen)%>%
  mutate(n=row_number()/n())%>%
  filter(year==2022)%>%
  ggplot()+
  geom_line(aes(n*100,renew_gen,group=year,color=year))

test<-
  hourly_renew %>% 
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #group_by(year)%>%
  #arrange(renew_gen)%>%
  #mutate(n=row_number()/n())%>%
  filter(year==2022)%>%
  ggplot()+
  geom_line(aes(index_time,renew_gen,group=year,color=year))


test<-
  hourly_renew %>% 
  filter(year(date)>=2021)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         temps=fct_recode(temps,"<-25"="(-35,-25]"),
         temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  ggplot(aes(temps,renew_gen))+
  geom_violin()+
  geom_boxplot(width=0.07,outlier.shape = NA)
#stat_summary(fun.data=mean_sdl, mult=1, 
#             geom="crossbar", color="red",width=0.2)

#REDO WITH CAPACITY FACTORS

hourly_renew %>% 
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                    
                      ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,wind_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Wind Fleet Capacity Factor (%)",x="\nTemperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_temps.png",height = 7,width=14,dpi=250,bg="white")


#smoothed wind


#test<-
  hourly_renew %>%
  mutate(wind_12hr=zoo::rollmean(wind_gen,12,fill=NA),
         wind_12hr_cap=zoo::rollmean(wind_cap,12,fill=NA),
         wind_12hr_cap_fac=wind_12hr/wind_12hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,wind_12hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Wind Fleet 12-hour Average Capacity Factor (%)",x="\nTemperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_smooth_24.png",height = 7,width=14,dpi=250,bg="white")


hourly_renew %>%
  mutate(wind_72hr=zoo::rollmean(wind_gen,72,fill=NA),
         wind_72hr_cap=zoo::rollmean(wind_cap,72,fill=NA),
         wind_72hr_cap_fac=wind_72hr/wind_72hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,wind_72hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Wind Fleet 72-hour Average Capacity Factor (%)",x="\nTemperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_smooth_72.png",height = 7,width=14,dpi=250,bg="white")

hourly_renew %>%
  mutate(wind_72hr=zoo::rollmean(wind_gen,72,fill=NA),
         wind_72hr_cap=zoo::rollmean(wind_cap,72,fill=NA),
         wind_72hr_cap_fac=wind_72hr/wind_72hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,wind_72hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Wind Fleet 72-hour Average Capacity Factor (%)",x="\nTemperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_smooth_72.png",height = 7,width=14,dpi=250,bg="white")



hourly_renew %>%
  mutate(wind_72hr=zoo::rollmean(wind_gen,72,fill=NA),
         wind_72hr_cap=zoo::rollmean(wind_cap,72,fill=NA),
         wind_72hr_cap_fac=wind_72hr/wind_72hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,wind_72hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Wind Fleet 72-hour Average Capacity Factor (%)",x="\nTemperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_smooth_72.png",height = 7,width=14,dpi=250,bg="white")



hourly_renew %>%
  mutate(wind_168hr=zoo::rollmean(wind_gen,168,fill=NA),
         wind_168hr_cap=zoo::rollmean(wind_cap,168,fill=NA),
         wind_168hr_cap_fac=wind_168hr/wind_168hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste("<",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste("<",seq(1,10)*10,"th\npercentile",sep = "")
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,wind_168hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA,notch = F)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Wind Fleet Weekly Average Capacity Factor (%)",x="\nHourly Temperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_smooth_168.png",height = 12,width=12,dpi=250,bg="white")

test<-
  hourly_renew %>%
  left_join(forecast_data)%>%
  mutate(year=year(date))%>%
  filter(year==2010)%>%
  group_by(year)%>%
    mutate(load_fac=actual_ail/max(actual_ail))%>%
  ungroup()%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  filter(!is.na(actual_ail))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  ggplot(aes(temps,load_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=50)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Hourly Load as a Share of Annual Peak Load (%)",x="\nHourly Temperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/loads.png",height = 12,width=12,dpi=250,bg="white")


library(ggridges)

hourly_renew %>%
  mutate(wind_72hr=zoo::rollmean(wind_gen,72,fill=NA),
         wind_72hr_cap=zoo::rollmean(wind_cap,72,fill=NA),
         wind_72hr_cap_fac=wind_72hr/wind_72hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste((as.numeric(temps)-1)*10,"-",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(0,9)*10,"-",seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         temps=fct_recode(temps,"Warmest 10%"="90-100th\npercentile"),
         temps=fct_recode(temps,"Coldest 10%"="0-10th\npercentile")
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = cap_fac*100, y = temps, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.8, rel_min_height = 0.01,alpha=.3) +
  scale_fill_viridis_c(name = "Wind Fleet Capacity Factor (%)", option = "B") +
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(3.5))
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(x="",y="\nHourly Temperature Percentiles",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)


hourly_renew %>%
  filter(year(date)>=2018)%>%
  mutate(solar_24hr=zoo::rollmean(solar_gen,24,fill=NA),
         solar_24hr_cap=zoo::rollmean(solar_cap,24,fill=NA),
         solar_24hr_cap_fac=solar_24hr/solar_24hr_cap)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste((as.numeric(temps)-1)*10,"-",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(0,9)*10,"-",seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         temps=fct_recode(temps,"Warmest 10%"="90-100th\npercentile"),
         temps=fct_recode(temps,"Coldest 10%"="0-10th\npercentile")
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = solar_24hr_cap_fac*24, y = temps, fill = stat(x))
  ) +
  geom_density_ridges_gradient(size = 0.8,scale = 0.95,from = 0, to = 100) +
  scale_fill_viridis_c(name = "Preceeding 24hr Solar Fleet Capacity Factor (%)", option = "B") +
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(3.5)),
        axis.title = element_text(size = rel(.85))
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(x="",y="\nHourly Temperature Percentiles",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/solar_ridges_day.png",height = 12,width=12,dpi=250,bg="white")

library(RColorBrewer)

#test<-
  hourly_renew %>%
  filter(year(date)>=2018)%>%
  group_by(date)%>%
  summarize(solar_24hr=sum(solar_gen,na.rm = T),
         solar_24hr_cap=sum(solar_cap,na.rm=T),
         solar_24hr_cap_fac=solar_24hr/solar_24hr_cap)%>%
  #filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(month=factor(month.abb[month(date)],levels=month.abb),
         month=fct_rev(month))%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = solar_24hr_cap_fac*24, y = month),fill="grey50") +
  geom_density_ridges_gradient(size = 0.8,scale = 0.95,from = 0, to = 24) +
  #scale_fill_gradientn(colours=brewer.pal(7,"Greys"))+
  scale_x_continuous(breaks=pretty_breaks(12),expand=c(0,0))+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(3.5)),
        axis.title = element_text(size = rel(.85))
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(x="Daily Generation (MWh) per MW of Installed Capacity",y="",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/solar_ridges_month.png",height = 8,width=8,dpi=600,bg="white")


#test<-
hourly_renew %>%
  filter(year(date)>=2018)%>%
  group_by(date)%>%
  summarize(
            solar_24hr=sum(solar_gen,na.rm = T),
            solar_24hr_cap=sum(solar_cap,na.rm=T),
            solar_24hr_cap_fac=solar_24hr/solar_24hr_cap,
            dd=sum((temp_YEG+temp_YYC)/2))%>%
  filter(!is.na(dd))%>%
  #filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(temps=cut_number(dd,10),
         temps=factor(paste((as.numeric(temps)-1)*10,"-",as.numeric(temps)*10,"th",sep = ""),
                      levels=paste(seq(0,9)*10,"-",seq(1,10)*10,"th",sep = "") ),
         temps=fct_recode(temps,"Warmest 10%"="90-100th"),
         temps=fct_recode(temps,"Coldest 10%"="0-10th")
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  
  
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = solar_24hr_cap_fac*24, y = temps),fill="grey50") +
  geom_density_ridges_gradient(size = 0.5,scale = 0.9,from = 0, to = 24) +
  #scale_fill_gradientn(colours=brewer.pal(7,"Greys"))+
  scale_x_continuous(breaks=pretty_breaks(12),expand=c(0,0))+
  paper_theme()+
  expand_limits(y=c(0))+ #make sure you get the zero line
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(text = element_text(color="black"),
        legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #plot.margin=margin(c(1,1,1,1),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        #legend.title = element_text(size = rel(3.5)),
        #axis.title = element_text(size = rel(.75)),
        axis.text.x = element_text(margin = margin(t = -30, r = 0, b = 0, l = 0, unit = "pt")),
        axis.title.y = element_text(margin = margin(t = 0, r = -14, b = 0, l = 0, unit = "pt"))
  )+
  labs(x="Daily Solar Generation (MWh) per MW of Installed Capacity",y="Daily Average Temperature Percentile",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)+coord_cartesian(clip = "off")
ggsave("images/solar_ridges_days.png",height = 6,width=12,dpi=600,bg="white")
ggsave("images/solar_ridges_days.tiff",height =6,width=12,dpi=600,bg="white")



#test<-
hourly_renew %>%
  filter(year(date)>=2018)%>%
  group_by(date)%>%
  summarize(
    wind_24hr=sum(wind_gen,na.rm = T),
    wind_24hr_cap=sum(wind_cap,na.rm=T),
    wind_24hr_cap_fac=wind_24hr/wind_24hr_cap,
    dd=sum((temp_YEG+temp_YYC)/2))%>%
  filter(!is.na(dd))%>%
  #filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(temps=cut_number(dd,10),
         temps=factor(paste((as.numeric(temps)-1)*10,"-",as.numeric(temps)*10,"th",sep = ""),
                      levels=paste(seq(0,9)*10,"-",seq(1,10)*10,"th",sep = "") ),
         temps=fct_recode(temps,"Warmest 10%"="90-100th"),
         temps=fct_recode(temps,"Coldest 10%"="0-10th")
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  
  
  #filter(wind_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = wind_24hr_cap_fac*24, y = temps),fill="grey50") +
  geom_density_ridges_gradient(size = 0.5,scale = 0.9,from = 0, to = 24) +
  #scale_fill_gradientn(colours=brewer.pal(7,"Greys"))+
  scale_x_continuous(breaks=pretty_breaks(12),expand=c(0,0))+
  paper_theme()+
  expand_limits(y=c(0,5))+ #make sure you get the zero line
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(#text = element_text(color="black"),
        legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #plot.margin=margin(c(1,1,1,1),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        #legend.title = element_text(size = rel(3.5)),
        #axis.title = element_text(size = rel(.75)),
        axis.text.x = element_text(margin = margin(t = -30, r = 0, b = 0, l = 0, unit = "pt")),
        axis.title.y = element_text(margin = margin(t = 0, r = -14, b = 0, l = 0, unit = "pt"))
  )+
  labs(x="Daily Wind Generation (MWh) per MW of Installed Capacity",y="Daily Average Temperature Percentile",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)+coord_cartesian(clip = "off")
ggsave("images/wind_ridges_days.png",height = 6,width=12,dpi=600,bg="white")
ggsave("images/wind_ridges_days.tiff",height = 6,width=12,dpi=600,bg="white")






hourly_renew %>%
  filter(year(date)>=2018)%>%
  group_by(date)%>%
  summarize(wind_24hr=sum(wind_gen,na.rm = T),
            wind_24hr_cap=sum(wind_cap,na.rm=T),
            wind_24hr_cap_fac=wind_24hr/wind_24hr_cap)%>%
  #filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(month=factor(month.abb[month(date)],levels=month.abb),
         month=fct_rev(month))%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = wind_24hr_cap_fac*24, y = month),fill="grey50") +
  geom_density_ridges_gradient(size = 0.8,scale = 0.95,from = 0, to = 24) +
  #scale_fill_gradientn(colours=brewer.pal(7,"Greys"))+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  scale_x_continuous(breaks=pretty_breaks(12),expand=c(0,0))+
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(3.5)),
        axis.title = element_text(size = rel(.85))
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(x="Daily Generation (MWh) per MW of Installed Capacity",y="",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_ridges_month.png",height = 8,width=8,dpi=600,bg="white")





hourly_renew %>%
  filter(year(date)>=2018)%>%
  mutate(wind_24hr=zoo::rollmean(wind_gen,24,fill=NA),
         wind_24hr_cap=zoo::rollmean(wind_cap,24,fill=NA),
         wind_24hr_cap_fac=wind_24hr/wind_24hr_cap)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste((as.numeric(temps)-1)*10,"-",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(0,9)*10,"-",seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         temps=fct_recode(temps,"Warmest 10%"="90-100th\npercentile"),
         temps=fct_recode(temps,"Coldest 10%"="0-10th\npercentile")
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(wind_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = wind_24hr_cap_fac*100, y = temps, fill = stat(x))
  ) +
  geom_density_ridges_gradient(size = 0.8,scale = 0.95,from = 0, to = 100) +
  scale_fill_viridis_c(name = "Preceeding 24hr Wind Fleet Capacity Factor (%)", option = "B") +
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(3.5)),
        axis.title = element_text(size = rel(.85))
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(x="",y="\nHourly Temperature Percentiles",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_ridges_day.png",height = 12,width=12,dpi=250,bg="white")


test<-
hourly_renew %>%
  filter(year(date)>=2018)%>%
  mutate(solar_24hr=zoo::rollmean(solar_gen,24,fill=NA),
         solar_24hr_cap=zoo::rollmean(solar_cap,24,fill=NA),
         solar_24hr_cap_fac=solar_24hr/solar_24hr_cap)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste((as.numeric(temps)-1)*10,"-",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(0,9)*10,"-",seq(1,10)*10,"th\npercentile",sep = ""),
                       ),
         year=year(time),
         temps=fct_recode(temps,"Warmest 10%"="90-100th\npercentile"),
         temps=fct_recode(temps,"Coldest 10%"="0-10th\npercentile"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         avg_cap_fac=sum(solar_gen/sum(solar_cap))
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  
  
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = solar_24hr_cap_fac*100, y = temps, fill = stat(x))
  ) +
  geom_density_ridges_gradient(size = 0.8,scale = 0.95,from = 0, to = 100) +
  scale_fill_viridis_c(name = "Preceeding Weekly Average Solar Fleet Capacity Factor (%)", option = "B") +
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(3.5)),
        axis.title = element_text(size = rel(.85))
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(x="",y="\nHourly Temperature Percentiles",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/solar_ridges.png",height = 12,width=12,dpi=250,bg="white")



  hourly_renew %>%
  filter(year(date)>=2018)%>%
  filter(solar_cap_fac>0.1)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste((as.numeric(temps)-1)*10,"-",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(0,9)*10,"-",seq(1,10)*10,"th\npercentile",sep = ""),
         ),
         year=year(time),
         temps=fct_recode(temps,"Warmest 10%"="90-100th\npercentile"),
         temps=fct_recode(temps,"Coldest 10%"="0-10th\npercentile"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         avg_cap_fac=sum(solar_gen/sum(solar_cap))
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  
  
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = solar_cap_fac*100, y = temps, fill = stat(x))
  ) +
  geom_density_ridges_gradient(size = 0.8,scale = 0.95,from = 0, to = 100) +
  scale_fill_viridis_c(name = "Preceeding Weekly Average Solar Fleet Capacity Factor (%)", option = "B") +
  theme_minimal()+
  expand_limits(y=0)+ #make sure you get the zero line
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(legend.position="none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        #legend.title = element_text(size = rel(3.5)),
        #axis.title = element_text(size = rel(.85))
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(x="Solar Capacity Factor",y="\nHourly Temperature Percentiles",
       title=paste("Distribution of Solar Capacity Factors by Hourly Temperature",sep=""),
       subtitle="Graph Produced Using the MacDonald-Laurier Method to Omit Values Close to or Below Zero",
       NULL)
ggsave("images/solar_ridges_mli.png",height = 12,width=12,dpi=250,bg="white")



hourly_renew %>%
  filter(year(date)>=2018)%>%
  mutate(wind_24hr=zoo::rollmean(wind_gen,24,fill=NA),
         wind_24hr_cap=zoo::rollmean(wind_cap,24,fill=NA),
         wind_24hr_cap_fac=wind_24hr/wind_24hr_cap)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste((as.numeric(temps)-1)*10,"-",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(0,9)*10,"-",seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         temps=fct_recode(temps,"Warmest 10%"="90-100th\npercentile"),
         temps=fct_recode(temps,"Coldest 10%"="0-10th\npercentile")
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(wind_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  #ggplot(aes(y=temps,x=wind_72hr_cap_fac*100,fill = stat(x)))+
  #geom_density_ridges(rel_min_height = 0.01)+
  #scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
  ggplot(aes(x = wind_24hr_cap_fac*100, y = temps, fill = stat(x))
  ) +
  geom_density_ridges_gradient(size = 0.8,scale = 0.95,from = 0, to = 100) +
  scale_fill_viridis_c(name = "Preceeding Weekly Average Wind Fleet Capacity Factor (%)", option = "B") +
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  #guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  guides(color=guide_colourbar(title.vjust=1))+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        #legend.text = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(3.5)),
        axis.title = element_text(size = rel(.85))
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(x="",y="\nHourly Temperature Percentiles",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_ridges.png",height = 12,width=12,dpi=250,bg="white")






hourly_renew %>%
  mutate(wind_672hr=zoo::rollmean(wind_gen,672,fill=NA),
         wind_672hr_cap=zoo::rollmean(wind_cap,672,fill=NA),
         wind_672hr_cap_fac=wind_672hr/wind_672hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,wind_672hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Wind Fleet Monthly Average Capacity Factor (%)",x="\nTemperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/wind_smooth_672.png",height = 12,width=12,dpi=250,bg="white")




hourly_renew %>%
  filter(date>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,wind_672hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Wind Fleet Monthly Average Capacity Factor (%)",x="\nTemperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)




hourly_renew %>%
  filter(year(date)>=2017)%>%
  mutate(month=month(date),year=year(date))%>%
  group_by(month,year)%>%
  mutate(
        date=ymd(paste(year,"-",month,"-",15,sep="")),
        solar_capacity=mean(solar_cap,na.rm=T),
        wind_capacity=mean(wind_cap,na.rm = T)
  )%>%
  ggplot()+
  geom_line(aes(date,solar_capacity))+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Installed Solar Capacity (MW)",x="",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)





hourly_renew %>%
  mutate(solar_24hr=zoo::rollmean(solar_gen,24,fill=NA),
         solar_24hr_cap=zoo::rollmean(solar_cap,24,fill=NA),
         solar_24hr_cap_fac=solar_24hr/solar_24hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,solar_24hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Solar Fleet 24-hour Average Capacity Factor (%)",x="\nHourly Temperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/solar_smooth_temps.png",height = 7,width=14,dpi=250,bg="white")



hourly_renew %>%
  mutate(solar_72hr=zoo::rollmean(solar_gen,72,fill=NA),
         solar_72hr_cap=zoo::rollmean(solar_cap,72,fill=NA),
         solar_72hr_cap_fac=solar_72hr/solar_72hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,solar_72hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Solar Fleet 72-hour Average Capacity Factor (%)",x="\nHourly Temperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/solar_smooth_temps.png",height = 7,width=14,dpi=250,bg="white")

hourly_renew %>%
  mutate(solar_168hr=zoo::rollmean(solar_gen,168,fill=NA),
         solar_168hr_cap=zoo::rollmean(solar_cap,168,fill=NA),
         solar_168hr_cap_fac=solar_168hr/solar_168hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste("<",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste("<",seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,solar_168hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Solar Fleet Weekly Average Capacity Factor (%)",x="\nHourly Temperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/solar_smooth_weekly.png",height = 12,width=12,dpi=250,bg="white")



hourly_renew %>%
  mutate(solar_672hr=zoo::rollmean(solar_gen,672,fill=NA),
         solar_672hr_cap=zoo::rollmean(solar_cap,672,fill=NA),
         solar_672hr_cap_fac=solar_672hr/solar_672hr_cap)%>%
  filter(year(date)>2018)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste("<",as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste("<",seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,solar_672hr_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Solar Fleet Monthly Average Capacity Factor (%)",x="\nHourly Temperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/solar_smooth_month.png",height = 7,width=14,dpi=250,bg="white")





hourly_renew %>% 
  left_join(forecast_data)%>%
  filter(year(date)>2018)%>%
  filter(solar_cap_fac>0)%>%
  filter(he!="02*")%>%
  filter(!((month(date)==2)&(day(date)==29)))%>%
  filter(!is.na((temp_YEG+temp_YYC)/2))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),
         index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)),
         temps=cut_number((temp_YEG+temp_YYC)/2,10),
         temps=factor(paste(as.numeric(temps)*10,"th\npercentile",sep = ""),
                      levels=paste(seq(1,10)*10,"th\npercentile",sep = "")
                      
         ),
         #temps=cut_width((temp_YEG+temp_YYC)/2,10,boundary=-35),
         #temps=fct_recode(temps,"(-35,-25]"="[-45,-35]"),
         #temps=fct_recode(temps,"<-25"="(-35,-25]"),
         #temps=fct_recode(temps,">25"="(25,35]")  ,
         
  )%>%
  assign_date_time_days()%>%
  assign_peaks()%>%
  #filter(solar_cap_fac>0)%>%
  #filter(on_peak==TRUE)%>%
  #mutate(index_time=ymd_hm(paste(2015,"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")),year=factor(year(date)))%>%
  #ggplot(aes(temps,cap_fac))+
  ggplot(aes(temps,solar_cap_fac*100))+
  geom_violin(fill=blakes_blue,alpha=0.05)+
  geom_boxplot(width=0.085,outlier.shape = NA)+
  paper_theme()+
  expand_limits(y=0)+ #make sure you get the zero line
  guides(linetype = guide_legend(override.aes = list(color = c("black","blue"))),color="none")+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm")
        #legend.text = element_text(colour="black", size = 12, face = "bold")
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
  )+
  labs(y="Solar Fleet Capacity Factor (%)",x="\nTemperature, lowest (left) to highest (right)",
       #title=paste("Alberta Hourly Wholesale Power Prices and Alberta Internal Load",sep=""),
       #subtitle=paste(month.name[month(period_start)],", ",year(period_start)," to ",month.name[month(end_date)],", ",year(end_date),sep="")
       NULL)
ggsave("images/solar_temps.png",height = 7,width=14,dpi=250,bg="white")




  

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
  hourly_renew %>% 
  left_join(forecast_data)%>%
  filter(date>ymd("2022-12-18"),date<ymd("2022-12-18")+days(5))%>%
  mutate(time=ymd_hm(paste(year(date),"-",month(date),"-",day(date)," ",as.numeric(he)-1,":00",sep="")))%>%
  ggplot()+
  geom_line(aes(time,temp_YEG,colour="Edmonton International Airport"),size=1.25)+
  
  
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


