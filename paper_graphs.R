source("power_paper_base.R")
library(directlabels)
#load(file="data/metered_vols_data.Rdata")
load(file="nrgstream/nrgstream_gen.Rdata") 

load(file="data/forecast_data.Rdata") 
forecast_data <- forecast_data %>% filter (he!="02*")




#direct label graph with AESO data

#align variable names
df2 <- nrgstream_gen %>% filter(date<floor_date(max(date),"month"))%>% #trim to last full month of data
  select(time=Time,vol=gen,ID,AESO_Name,Plant_Type,Plant_Fuel,Capacity) %>%
  filter(Plant_Type %in% c("COAL","COGEN","NGCC","WIND","SCGT","TRADE","HYDRO","SOLAR","OTHER"))%>%
  #filter(year(time)>=2010) %>% 
  left_join(forecast_data) %>% filter(!is.na(date))%>%
  #strip the AB-WECC tie since it's duplicate of AB-MT and AB-BC
  filter(!ID %in% c("AB_WECC","AB_WECC_Exp","AB_WECC_Imp"))%>%
  #mutate(Plant_Type=as.character(Plant_Type))%>%
  #mutate(Plant_Fuel=as.character(Plant_Fuel))%>%
  mutate(hour=hour(time))%>%
  select(-c("forecast_pool_price","day_ahead_forecasted_ail",        
                "forecasted_actual_ail_difference","start_date"))%>%
  #filter(date>Sys.Date()-years(10))%>%
  mutate(month=month(time),year=year(time))%>%   
  mutate(Plant_Type=as_factor(Plant_Type),
         # Relevel to the end
         Plant_Type=fct_other(Plant_Type,keep = c("COAL","COGEN","SCGT","NGCC","WIND","HYDRO","TRADE"),other_level = "OTHER"),
         Plant_Type=fct_relevel(Plant_Type, "OTHER", after = Inf),
         Plant_Type=fct_recode(Plant_Type, "NET IMPORTS"="TRADE"),
         Plant_Type=fct_relevel(Plant_Type, "NET IMPORTS", after = Inf),
         NULL
  )%>%
  #summarize by hour to get total gen and revenue by fuel, 
  group_by(year,month,date,hour,Plant_Type) %>% summarise(capacity=sum(Capacity,na.rm = T),gen=sum(vol,na.rm = T),rev=gen*actual_posted_pool_price) %>% 
  #summarize by year and month to get mean gen and capture price by fuel, 
  group_by(year,month,Plant_Type) %>% summarise(gen=mean(gen,na.rm = T),Capacity=mean(capacity,na.rm = T),capture = sum(rev)/sum(gen))%>% 
  ungroup() %>%
  mutate(date=ymd(paste(year,month,15,sep="-")))
  
  



AB_plant_order<-c("COAL","COGEN","NGCC","WIND","HYDRO","SCGT","OTHER","NET IMPORTS")
AB_palette<- c("black","grey80","grey50",ptol_pal()(6)[3],ptol_pal()(6)[1],"grey50",ptol_pal()(6)[5],ptol_pal()(6)[6])


gen_plain <- df2 %>% mutate(Plant_Type=factor(Plant_Type,levels=AB_plant_order))%>%
  #filter(date>ymd("2014-12-31"))%>%
  ggplot(aes(date,gen, col = Plant_Type,fill = Plant_Type,shape=Plant_Type)) +
  geom_line(size=1.25)+
  geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
  geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = 1.05))+
  scale_color_manual("",values= AB_palette)+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
  blake_theme()+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "24 months",expand=c(0,0))+
  expand_limits(x= ymd(c("2015-01-01", "2023-12-31")))+
  scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
  #theme(legend.position = "right")+
  labs(x="",y="Monthly Average Hourly Generation or Imports (MW)",
       #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
       #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
       #caption="Source: AESO data, authors' calculations."
       NULL)+
  
 # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
#  annotate("rect", fill = "grey70", alpha = .3, 
 #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
#           ymin = -Inf, ymax = Inf)+
 # annotate("rect", fill = "grey70", alpha = .3, 
  #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
   #        ymin = -Inf, ymax = Inf)+
  #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
  NULL
  gen_plain
  ggsave(file="images/gen_col.png", width = 16, height = 9,dpi = 600)

  AB_palette<- c("black","grey80","grey30","grey40","grey20","grey50","grey70","grey60")
  gen_plain+
    scale_color_manual("",values= AB_palette)
  ggsave(file="images/gen_grey.png", width = 16, height = 9,dpi = 600)
  

  ptol_green<-ptol_pal()(6)[3]
  ptol_blue<-ptol_pal()(6)[1]
  ptol_pink<-ptol_pal()(6)[5]
  ptol_purple<-ptol_pal()(6)[6]
  ptol_yellow<-ptol_pal()(6)[2]
  area_plant_order<-c("WIND","HYDRO","NGCC","SCGT","COGEN","COAL","OTHER","NET IMPORTS")
  AB_palette<- c(ptol_green,ptol_blue,"grey90","grey70","grey50","black",ptol_yellow,ptol_purple)
  
  
  
  gen_area <- df2 %>% mutate(Plant_Type=factor(Plant_Type,levels=area_plant_order))%>%
    #filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,gen,fill = Plant_Type,shape=Plant_Type)) +
    geom_area(position="stack",color="black",size=.5)+
    #geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    #geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = 1.05))+
    scale_fill_manual("",values= AB_palette)+
    scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    blake_theme()+
    scale_x_date(date_labels = "%Y",date_breaks = "12 months",expand=c(0,0))+
    #expand_limits(x=c(ymd("2022-1-31"))+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    theme(legend.position = "right")+
    labs(x="",y="Monthly Average Hourly Generation or Net Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  gen_area
  ggsave(file="images/gen_area_col.png", width = 16, height = 9,dpi = 600)
  
  BW_palette<- c("grey90","grey20","grey80","grey30","grey70","black","grey60","grey40")
  gen_area+
    scale_fill_manual("",values= BW_palette)
  ggsave(file="images/gen_area_grey.png", width = 16, height = 9,dpi = 600)
  
  
  
    
  gen_fuel <- df2 %>% mutate(Plant_Type=factor(Plant_Type,levels=AB_plant_order))%>%
    #filter(date<ymd("2020-09-01"))%>%
    mutate(Plant_Type=fct_collapse(Plant_Type,
     #"OTHER"=c("WIND","OTHER","HYDRO"),
     "NATURAL GAS"=c("SCGT","COGEN","NGCC"),
     "NET IMPORTS"="TRADE"
     ))%>% 
    group_by(date,month,year,Plant_Type) %>% summarise(gen=sum(gen,na.rm = T))%>% 
    ungroup() %>%
    #filter(date>ymd("2014-12-31"))%>%
    ggplot(aes(date,gen, col = Plant_Type,fill = Plant_Type,shape=Plant_Type)) +
    geom_line(size=1.25)+
    geom_point(aes(date,gen*ifelse(month%%2==0,1,NA)),size=2.5)+
    geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_color_manual("",values= AB_palette)+
    scale_fill_manual("",values= AB_palette)+
    
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    blake_theme()+theme(plot.margin =unit(c(1,1,1,1),"cm"))+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "24 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2004-01-01", "2023-11-30")))+
    expand_limits(y =c(-500,7000))+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    
    theme(legend.position = "none")+
    labs(x="",y="Monthly Average Hourly Generation or Net Imports (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  gen_fuel
  ggsave(file="images/gen_fuel.png", width = 14,dpi = 300)

  
  AB_palette<- c("black","grey50",ptol_pal()(6)[3],ptol_pal()(6)[1],ptol_pal()(6)[5],ptol_pal()(6)[6])
  gen_fuel+
    scale_color_manual("",values= AB_palette)
  ggsave(file="images/gen_fuel_col.png", width = 14,dpi = 600)
    
  
  AB_palette<- c("black","grey50",ptol_pal()(6)[3],ptol_pal()(6)[1],ptol_pal()(6)[5],ptol_pal()(6)[6])
  capacity_fuel <- df2 %>% mutate(Plant_Type=factor(Plant_Type,levels=AB_plant_order))%>%
    filter(Plant_Type!="TRADE")%>%
    mutate(Plant_Type=fct_collapse(Plant_Type,
                                   #"OTHER"=c("WIND","OTHER","HYDRO"),
                                   "NATURAL\nGAS"=c("SCGT","COGEN","NGCC"),
                                   "NET\nIMPORTS"="TRADE"),
           Plant_Type=fct_relevel(Plant_Type,"OTHER",after=Inf))%>% 
    group_by(date,month,year,Plant_Type) %>% summarise(capacity=sum(Capacity,na.rm=T),gen=sum(gen,na.rm = T))%>% 
    ungroup() %>%
    ggplot(aes(date,capacity, col = Plant_Type,fill = Plant_Type,shape=Plant_Type)) +
    geom_line(size=1.25)+
    geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    scale_color_manual("",values= AB_palette)+
    scale_fill_manual("",values= AB_palette)+
    
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    blake_theme()+theme(plot.margin =unit(c(1,1,1,1),"cm"))+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "24 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2004-01-01", "2023-11-30")))+
    expand_limits(y=c(0, 8000))+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    
    theme(legend.position = "none")+
    labs(x="",y="Monthly Average Capacity (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  capacity_fuel
  ggsave(file="images/cap_fuel_col.png", width = 14,dpi = 600)
  
  
  AB_palette<- c("black","grey80","grey30","grey40","grey20","grey50","grey70","grey60")
  capacity_fuel+
    scale_color_manual("",values= AB_palette)
  ggsave(file="images/cap_fuel.png", width = 14,dpi = 600)
  
  
  
  
  floor_date(last(time),"day")
  
  daily_gen <- nrgstream_gen %>% rename(time=Time)%>% 
    select(time,vol=gen,ID,AESO_Name,Plant_Type,Plant_Fuel,Capacity) %>%
    assign_date_time_days()%>%
    filter(Plant_Type %in% c("COAL","COGEN","NGCC","WIND","SCGT","TRADE","HYDRO","SOLAR","OTHER"))%>%
    filter(!is.na(vol))%>%
    filter(time<floor_date(Sys.time(),"month"))%>% #trim to last full month
    left_join(forecast_data%>%select(-c("forecast_pool_price","day_ahead_forecasted_ail",        
                                        "forecasted_actual_ail_difference","start_date")))%>% 
    #strip the AB-WECC tie since it's duplicate of AB-MT and AB-BC
    filter(!ID %in% c("AB_WECC","AB_WECC_Exp","AB_WECC_Imp"))%>%
    mutate(Plant_Type=as_factor(Plant_Type),
           # Relevel trade and others to the end
           Plant_Type=fct_other(Plant_Type,keep = c("COAL","COGEN","SCGT","NGCC","WIND","HYDRO","TRADE"),other_level = "OTHER"),
           Plant_Type=fct_relevel(Plant_Type, "OTHER", after = Inf),
           Plant_Type=fct_relevel(Plant_Type, "TRADE", after = Inf),
           NULL
    )%>%
    #summarize by day to get total gen and revenue by fuel, 
    group_by(year,month,date,AESO_Name,Plant_Type,Plant_Fuel,Capacity) %>%
    summarise(vol=mean(vol,na.rm = T),rev=sum(vol*actual_posted_pool_price,na.rm = T)) %>%
    ungroup()
  
  #plot plants or types over time
  
  df1<-daily_gen%>%filter(Plant_Type=="NGCC")%>%filter(date>=ymd('2021-04-01'))
  ggplot(df1)+
    geom_line(aes(date,vol,col=AESO_Name,fill=AESO_Name))+
    #geom_dl(aes(label=Plant_Type),method=list("last.bumpup",dl.trans(x=x+0.3),cex = .85))+
    #scale_color_manual("",values= AB_palette)+
    #scale_fill_manual("",values= AB_palette)+
    paper_theme()+
    theme(legend.position = "none")
    #scale_color_manual("",values=grey.colors(9,start=0,end=.8))+
    #scale_fill_manual("",values=grey.colors(9,start=0,end=.8))+
    scale_shape_manual("",values=c(15,16,17,18,0,1,2,3))+
    blake_theme()+theme(plot.margin =unit(c(1,1,1,1),"cm"))+
    scale_x_date(date_labels = "%b\n%Y",date_breaks = "24 months",expand=c(0,0))+
    expand_limits(x = as.Date(c("2004-01-01", "2023-11-30")))+
    expand_limits(y=c(0, 8000))+
    scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
    +
    labs(x="",y="Monthly Average Capacity (MW)",
         #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #title="Alberta Power Generation by Plant Type (MW, 2015-2020)",
         #caption="Source: AESO data, authors' calculations."
         NULL)+
    
    # annotate("text", x = covid_mid, y =4500, label = "COVID\nperiod",size=3.25,hjust=0.5,vjust=0.5)+
    #  annotate("rect", fill = "grey70", alpha = .3, 
    #          xmin = as.Date("2020-03-11"), xmax =as.Date("2020-07-01"),
    #           ymin = -Inf, ymax = Inf)+
    # annotate("rect", fill = "grey70", alpha = .3, 
    #         xmin = as.Date("2019-03-11"), xmax =as.Date("2019-07-01"),
    #        ymin = -Inf, ymax = Inf)+
    #annotate("text", x = covid_mid_lag, y =4500, label = "COVID\nperiod\nlast year",size=3.25,hjust=0.5,vjust=0.5)  
    NULL
  #capacity_fuel
  #ggsave(file="images/cap_fuel_col.png", width = 14,dpi = 600)
  
  
  
  
  
  
    
  
  
  #checking trade
  covid_mid<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)
  covid_mid_lag<-ymd("2020-03-11")+days((ymd("2020-07-01")-ymd("2020-03-11"))/2)-years(1)
  
  
  
  trade_flows <- 
    nrgstream_gen %>% 
  filter(Plant_Type %in% c("TRADE"))%>% filter(year(time)>=2017)%>% 
    group_by(date,Plant_Type,ID) %>% summarise(vol=mean(vol,na.rm = T))%>%
    ungroup %>% 
    group_by(ID)%>%
    mutate(value=roll::roll_mean(vol,7)) %>%ungroup()%>%
    mutate(d_index=ymd(paste(2020,month(date),day(date),sep="-"))) %>%  
    mutate(year=year(date)) %>%
    ggplot() +
    #geom_area(position = "stack")+
    geom_line(aes(d_index,value,group=factor(year),size=factor(year),color=factor(year)))+
    #geom_line(aes(d_index,value,group=year,colour="grey30"),size=.5)+
    #geom_line(aes(d_index,ifelse(year==2020,value,NA),group=year,colour="dodgerblue"),size=1)+
    #geom_ribbon(aes(date,ymax=max_flow,ymin=min_flow),alpha=.2,size=0)+
    blake_theme()+
    facet_wrap(~ID)+
    scale_x_date(date_labels = "%d\n%b",date_breaks = "2 months")+
    #expand_limits(x = as.Date(c("2019-12-15", "2020-12-31")))+
    scale_y_continuous(breaks=pretty_breaks())+
    scale_color_manual("",values=c(grey.colors(3,end=0.7),blakes_blue))+
    scale_size_manual("",values=c(rep(.5,3),1.5))+
    theme(legend.position = "bottom")+
    annotate("text", x = covid_mid, y =-500, label = "COVID\nperiod",size=4,hjust=0.5,vjust=0.5)+
    annotate("rect", fill = "grey80", alpha = .3, 
             xmin = as.Date("2020-03-11"), xmax =as.Date("2020-06-08"),
             ymin = -Inf, ymax = +Inf)+
    labs(x="",y="Average Daily Net Imports (MW)",
         #      #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
         #      title="Ontario Transmission-Connected Generation by Fuel (MW, 2019-2020)",
         #      caption="Source: IESO Data"
         NULL)+
    NULL
  trade_flows
  ggsave("aeso_trade_flows.png",width=16,height = 6,dpi = 600)
  
  
  
  
  
#merit stuff
  
  bids_files<- list.files("data/") %>% as_tibble() %>% filter(grepl("synth_type",value))%>% 
    mutate(file_date=gsub("synth_type_","",value),
           file_date=ymd_hm(gsub(".RData","",file_date))
    )%>% filter(file_date==max(file_date))
  
  load(file = paste("data/",bids_files$value,sep=""))
  merit_bids<-merit_aug
  
  
  
  #find latest merit_data file
  #data/merit_data_%Y_%b_%d_%H_%M.RData
data_files<- list.files("data/") %>% as_tibble() %>% filter(grepl("merit_data",value))%>% 
  mutate(file_date=gsub("merit_data_","",value),
         file_date=ymd_hm(gsub(".RData","",file_date))
         )%>% filter(file_date==max(file_date))

load(file = paste("data/",data_files$value,sep=""))

#merit_aug<-merit_aug %>% filter(year==2019,Plant_Type=="COAL")%>% filter(actual_posted_pool_price>200)

#test<-merit_aug %>% filter(date==ymd("2019-02-04"),he=="19",Plant_Type=="COAL")%>% select(actual_posted_pool_price)

merit_aug %>% filter(date==ymd("2019-02-04"),he=="19",Plant_Type=="COAL")%>% 
  mutate(facility=gsub(" #","_",AESO_Name))%>% separate(facility,into = c("facility","number"), sep="_(?=[^_]+$)")%>%
  group_by(facility,price,dispatched) %>% summarize(size=sum(size),dispatched_mw=sum(dispatched_mw))%>% ungroup()%>%
  arrange(price,facility)%>%
  mutate(merit=cumsum(size),total_offers=sum(size),total_dispatch=sum(dispatched_mw),dispatch_limit=max(merit*(dispatched=="Y")))%>%
  ggplot()+
  geom_rect(aes(xmin=merit-size,xmax=merit,ymin=-20,ymax=price,fill=facility),color="black")+
  geom_vline(aes(xintercept=max(dispatch_limit)))+
  annotate("text",3950,y=815,label="All offers to the\nleft of this line\nwere dispatched",size=4,hjust=1,vjust=0)+
  #geom_label_repel(aes(merit-size/2,y=price,label=facility))+
  scale_fill_grey("",end = 1,start=0)+   
  scale_x_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  paper_theme()+
  theme(
    legend.position = c(.1,.8),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, Feb 4, 2019 at hour ending 7pm"),
       #caption="Source: AESO Data, graph by Andrew Leach."
       )
ggsave("images/coal_merit.png",dpi=300,width = 14,height = 10)



merit_aug %>% filter(year(date)==2019,he=="19",Plant_Type=="COAL")%>% 
  mutate(facility=gsub(" #","_",AESO_Name))%>% separate(facility,into = c("facility","number"), sep="_(?=[^_]+$)")%>%
  group_by(facility,price,dispatched,date) %>% summarize(size=sum(size),dispatched_mw=sum(dispatched_mw))%>% ungroup()%>%
  group_by(date)%>%
  arrange(price,facility)%>%
  mutate(merit=cumsum(size),total_offers=sum(size),total_dispatch=sum(dispatched_mw),dispatch_limit=max(merit*(dispatched=="Y")))%>%
  ungroup()%>%
  ggplot()+
  #geom_rect(aes(xmin=merit-size,xmax=merit,ymin=-20,ymax=price,fill=facility),color="black")+
  #geom_vline(aes(xintercept=max(dispatch_limit)))+
  geom_line(aes(x=merit,y=price,group=date,color="2019 daily 7pm coal facility merit orders"))+
  #geom_point(aes(x=merit,y=price,group=date,color="2019 daily 7pm coal facility merit orders"))+
  #annotate("text",3950,y=815,label="All offers to the\nleft of this line\nwere dispatched",size=4,hjust=1,vjust=0)+
  #geom_label_repel(aes(merit-size/2,y=price,label=facility))+
  scale_color_grey("",end = 0.5,start=0.5)+   
  #scale_fill_grey("",end = 1,start=0)+   
  scale_x_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  expand_limits(y=1000,x=5000)+
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  paper_theme()+
  theme(
    legend.position = c(.15,.9),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, Feb 4, 2019 at hour ending 7pm"),
       #caption="Source: AESO Data, graph by Andrew Leach."
  )
ggsave("images/all_coal_merit.png",dpi=300,width = 14,height = 10)



merit_aug %>% filter(date==ymd("2019-02-04"),hour==19,asset_id=="BR5" )%>% arrange(price,AESO_Name)%>%
  mutate(merit=cumsum(size))%>%
  ggplot(aes(merit,price,color=AESO_Name))+
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-5,ymax=price),fill=NA)+
  scale_color_grey("Unit Name")+   
  scale_x_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  paper_theme()+
  theme(
    legend.position = "right",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, Feb 4, 2019 at hour ending 7pm"),
       #caption="Source: AESO Data, graph by Andrew Leach."
  )

#test_bids<-merit_bids %>% filter(date==ymd("2019-02-04"),he=="19",Plant_Type=="COAL")%>% rename(price=bid)

merit_bids %>% filter(date==ymd("2019-02-04"),he=="19",Plant_Type=="COAL")%>% rename(price=bid)%>%
  mutate(merit=percentile/100*total_offers,size=merit-lag(merit,n=1),size=ifelse(is.na(size),merit[1],size))%>%
  #group_by(price)%>% summarize(size=sum(size))%>% ungroup()%>%
  #mutate(merit=cumsum(size))%>%
  ggplot()+
  geom_rect(aes(xmin=merit-size,xmax=merit,ymin=-20,ymax=price,fill="Synthetic offer block"),color="black")+
  geom_point(aes(x=merit,y=price),color="black",size=2)+
  
  #geom_vline(aes(xintercept=max(dispatched_mw)))+
  #annotate("text",3950,y=815,label="All offers to the\nleft of this line\nwere dispatched",size=4,hjust=1,vjust=0)+
  #geom_label_repel(aes(merit-size/2,y=price,label=facility))+
  scale_fill_grey("",end = .5,start=.5)+   
  scale_x_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  paper_theme()+
  theme(
    legend.position = c(.1,.8),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, Feb 4, 2019 at hour ending 7pm"),
       #caption="Source: AESO Data, graph by Andrew Leach."
  )
ggsave("images/coal_synth_merit.png",dpi=300,width = 14,height = 10)





merit_bids <-merit_bids %>% 
  mutate(Plant_Type=factor(Plant_Type),
         Plant_Type=fct_relevel(Plant_Type,"WIND",after=Inf),
         Plant_Type=fct_relevel(Plant_Type,"HYDRO",after=Inf),
         Plant_Type=fct_relevel(Plant_Type,"SOLAR",after=Inf),
         Plant_Type=fct_relevel(Plant_Type,"OTHER",after=Inf),
         Plant_Type=fct_relevel(Plant_Type,"IMPORT",after=Inf))


bid_summary<-merit_bids %>% filter(year==2019,he=="19")%>% rename(price=bid)%>%
  group_by(percentile,Plant_Type) %>% summarize(price=mean(price),total_offers=mean(total_offers))%>% ungroup()%>%
  mutate(merit=percentile/100*total_offers,size=merit-lag(merit,n=1),size=ifelse(is.na(size),merit[1],size))

  

merit_bids %>% filter(year==2019,he=="19",Plant_Type=="COAL")%>% rename(price=bid)%>%
  #group_by(percentile) %>% mutate(mean_price=mean(price),mean_offer=mean(total_offers))%>% ungroup()%>%
  #group_by(date)%>%
  mutate(merit=percentile/100*total_offers,size=merit-lag(merit,n=1),size=ifelse(is.na(size),merit[1],size))%>%
  
  #group_by(price)%>% summarize(size=sum(size))%>% ungroup()%>%
  #mutate(merit=cumsum(size))%>%
  ggplot()+
  geom_line(aes(x=merit,y=price,color="2019 daily 7pm synthetic merit orders",group=date),size=.5)+
  geom_point(aes(x=merit,y=price,color="2019 daily 7pm synthetic merit orders",group=date),size=.85)+
  geom_point(data=bid_summary%>%filter(Plant_Type=="COAL"),aes(x=merit,y=price,color="2019 average 7pm synthetic merit order"),size=2)+
  geom_line(data=bid_summary%>%filter(Plant_Type=="COAL"),aes(x=merit,y=price,color="2019 average 7pm synthetic merit order"))+
  #geom_vline(aes(xintercept=max(dispatched_mw)))+
  #annotate("text",3950,y=815,label="All offers to the\nleft of this line\nwere dispatched",size=4,hjust=1,vjust=0)+
  #geom_label_repel(aes(merit-size/2,y=price,label=facility))+
  scale_color_manual("",values=c("black","grey80"))+   
  scale_x_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  expand_limits(y=1000,x=5000)+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  paper_theme()+
  theme(
    legend.position = c(.15,.9),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, Feb 4, 2019 at hour ending 7pm"),
       #caption="Source: AESO Data, graph by Andrew Leach."
  )
ggsave("images/coal_synth_2019.png",dpi=300,width = 14,height = 10)

merit_bids %>% filter(year==2019,he=="19",Plant_Type %in% c("COAL","NGCC","SCGT","COGEN","HYDRO","OTHER"))%>% 
  rename(price=bid)%>%
  #group_by(percentile) %>% mutate(mean_price=mean(price),mean_offer=mean(total_offers))%>% ungroup()%>%
  #group_by(date)%>%
  mutate(merit=percentile/100*total_offers,size=merit-lag(merit,n=1),size=ifelse(is.na(size),merit[1],size))%>%
  #group_by(price)%>% summarize(size=sum(size))%>% ungroup()%>%
  #mutate(merit=cumsum(size))%>%
  ggplot()+
  geom_line(aes(x=merit,y=price,color="2019 daily 7pm synthetic merit orders",group=date),size=.5)+
  geom_point(aes(x=merit,y=price,color="2019 daily 7pm synthetic merit orders",group=date),size=.85)+
  geom_point(data=bid_summary%>%filter(Plant_Type %in% c("COAL","NGCC","SCGT","COGEN","HYDRO","OTHER")),
                                       aes(x=merit,y=price,color="2019 average 7pm synthetic merit order"),size=2)+
  geom_line(data=bid_summary%>%filter(Plant_Type %in% c("COAL","NGCC","SCGT","COGEN","HYDRO","OTHER")),
                                      aes(x=merit,y=price,color="2019 average 7pm synthetic merit order"))+
  facet_wrap(~Plant_Type,scales="free_x")+
  #geom_vline(aes(xintercept=max(dispatched_mw)))+
  #annotate("text",3950,y=815,label="All offers to the\nleft of this line\nwere dispatched",size=4,hjust=1,vjust=0)+
  #geom_label_repel(aes(merit-size/2,y=price,label=facility))+
  scale_color_manual("",values=c("black","grey80"))+   
  scale_x_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  expand_limits(y=1000)+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
  paper_theme()+
  theme(
    legend.position = "bottom",#c(.15,.9),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=0, hjust=1),
  )+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, Feb 4, 2019 at hour ending 7pm"),
       #caption="Source: AESO Data, graph by Andrew Leach."
  )
ggsave("images/all_synth_2019.png",dpi=300,width = 14,height = 7)

  

  
  
  