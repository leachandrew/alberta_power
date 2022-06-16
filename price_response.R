#testing peaking indicator
load("data/merit_data_proc_bak.RData")  
#save(merit_aug,file="data/merit_data_proc_bak.RData")  

load("data/forecast_data.RData")

mkt_data<-forecast_data %>% assign_peaks()


merit_small<-  #small_testing_sample 
  merit_aug%>%filter(year(date)==2019)
  
merit_small<-merit_small%>%filter(Plant_Fuel=="GAS")

merit_small<-merit_small%>%filter(he=="18")

test<-merit_small %>% group_by(date,he,Plant_Type,asset_id)%>%
  arrange(price)%>%
  mutate(capability=sum(available_mw),pctl=to/capability,
         time=ymd_h(paste(date,he)))%>%
  select(time,Plant_Type,asset_id,size,to,capability,pctl,price)%>%
  
#contesting indicator  
test2<-merit_small %>% 
  #left_join(mkt_data)%>% ungroup()%>%
  #group_by(asset_id,date,he)%>%
  #summarize(share_over=sum((price>actual_posted_pool_price)*size/sum(size,na.rm=TRUE),na.rm=TRUE))%>%
  #summarize()%>%
  group_by(date,he,asset_id)%>%
  summarize(int=sum(size*price)/sum(size))%>%
  ungroup()%>%
  group_by(asset_id)%>%
  #summarize(min=min(int),q_25=quantile(int,.25),q_50=quantile(int,.5),q_75=quantile(int,.75),max=max(int),
  #          mean=mean(int,na.rm=TRUE))
  summarize(iqr=IQR(int))
  #ggplot()+
  

testing_iqr<-merit_aug%>%
  filter(Plant_Fuel=="GAS")%>%
  group_by(date,he,asset_id,Plant_Type)%>%
  summarize(int=sum(size*price)/sum(size))%>%
  ungroup()%>%
  group_by(asset_id)%>%
  mutate(iqr=IQR(int))%>%
  ungroup()

testing_iqr %>% #filter(Plant_Type%in%c("NGCC"))%>%
  mutate(contest=factor((iqr>20)),
         #contest=1,
         type_int=interaction(Plant_Type,contest),
         type_int=fct_recode(type_int,
                             "Non-responsive CHP"="COGEN.FALSE",
                             "Non-responsive SCGT"="SCGT.FALSE",
                             "Responsive NGCC"="NGCC.TRUE",
                             "Responsive CHP"="COGEN.TRUE",
                             "Responsive SCGT"="SCGT.TRUE",
                             ),
         type_int=fct_relevel(type_int,"Non-responsive SCGT"),
         type_int=fct_relevel(type_int,"Responsive CHP"),
         type_int=fct_relevel(type_int,"Responsive SCGT"),
         type_int=fct_relevel(type_int,"Responsive NGCC"),
         asset_id=factor(asset_id),
         asset_id=fct_reorder(asset_id,int)
         )%>%
  ggplot()+
   geom_boxplot(aes(asset_id,int,color=contest),outlier.shape = NA)+
  scale_color_manual("",values=c("black","black"))+
  facet_grid(~type_int,scales = "free_x", space = "free_x",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  paper_theme()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(angle = 90,size=10,vjust = 0.5,hjust=0.5),
        axis.text.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        axis.title.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        title = element_text(size=12),
        strip.text = element_text(size=10),
        legend.position = "none",
        
        )+
  labs(y=paste("Average Hourly Offer Value ($/MWh)"),x="",
       #title=paste("Sorting firms by price sensitivity"),
       #subtitle="Box plots show median (line),interquartile range (box) and confidence interval (whiskers) of hourly average offer price\nAverage offer price is the area under the facilty's offer function divided by the total offered MW"
       #caption="Source: AESO Data, graph by Andrew Leach."
       NULL
  )

ggsave("images/price_response.png",width=14,height=7,dpi=300)
  #group_by(asset_id)%>%
  #summarize(range=max(int)-min(int))


#check coal

testing_coal<-merit_aug%>%
  filter(Plant_Type=="COAL")%>%
  left_join(mkt_data)%>%
  filter(on_peak==TRUE)%>%
  group_by(date,he,asset_id,Plant_Type)%>%
  summarize(int=sum(size*price)/sum(size))%>%
  ungroup()%>%
  group_by(asset_id)%>%
  mutate(iqr=IQR(int))%>%
  ungroup()%>%
  mutate(contest=factor((iqr>20)),
         asset_id=factor(asset_id),
         asset_id=fct_reorder(asset_id,int),
         type_int=interaction(contest,Plant_Type)
  )%>%
  ggplot()+
  geom_boxplot(aes(asset_id,int,color=contest),outlier.shape = NA)+
  scale_color_manual("",values=c("black","black"))+
  facet_grid(~type_int,scales = "free_x", space = "free_x",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  paper_theme()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(angle = 90,size=10,vjust = 0.5,hjust=0.5),
        axis.text.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        axis.title.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        title = element_text(size=12),
        strip.text = element_text(size=10),
        legend.position = "none",
        
  )+
  labs(y=paste("Average Hourly Offer Value ($/MWh)"),x="",
       #title=paste("Sorting firms by price sensitivity"),
       #subtitle="Box plots show median (line),interquartile range (box) and confidence interval (whiskers) of hourly average offer price\nAverage offer price is the area under the facilty's offer function divided by the total offered MW"
       #caption="Source: AESO Data, graph by Andrew Leach."
       NULL
  )

testing_coal


#what about total value of non-zero offers?


testing_mids<-merit_aug%>%
  filter(date<=ymd("2020-03-01"))%>%
  filter(Plant_Fuel %in% c("COAL","GAS"),asset_id!="WB4")%>%
  left_join(mkt_data)%>%
  #filter(on_peak==TRUE)%>%
  arrange(date,he,Plant_Type,price) %>%
  group_by(date,he,Plant_Type,asset_id)%>% 
  mutate(merit_plant=cumsum(size)/sum(size)*100)%>%
  summarize(mid_merit=sum((price>0)*(price<900)*price*size),
            int=sum(size*price)/sum(size),
            nzp=sum(size*price)/sum(size*(price>0)), #average non-zero offer value
            nzp=ifelse(is.na(nzp),0,nzp),
            nzq=sum(size*(price>0))/sum(size), #non-zero offer share
            merit=list(merit_plant),
            price=list(price)
            )%>%
  #got plant_specific offer step function
  mutate(plant_offer_func=list(bid_func(merit[[1]],price[[1]])))%>%
  #now, are they shifting their marginal units around?
  mutate(#bid_95=plant_offer_func[[1]](95),
         bid_pt=plant_offer_func[[1]](75),
         bid_top=plant_offer_func[[1]](100)
         )%>%
  group_by(Plant_Type,asset_id)%>%
  mutate(iqr_bid=IQR(bid_pt),
          mean_bid_t=mean(bid_pt),
          iqr_offer=IQR(int),
         iqr_nzp=IQR(nzp),
         iqr_nzq=IQR(nzq),
            #top_iqr=IQR(bid_95),
            #top_mean=mean(bid_95)
            )%>%
  ungroup()%>%
  mutate(contest=factor((iqr_bid>0)|(iqr_offer>0)),
         contest_nz=factor((iqr_nzp>10)|(iqr_nzq>0.10)),
                  asset_id=factor(asset_id),
                  #asset_id=fct_reorder(asset_id,mean),
                  type_int=interaction(contest,Plant_Type)
  )


testing_mids<-testing_mids %>% 
  group_by(asset_id,Plant_Type)%>%
  mutate(med_int=median(int),
            iqr_int=IQR(int),
            iqr_nzp=IQR(nzp),
            comp_index=iqr_int+iqr_nzp,
            mid_merit_index=IQR(mid_merit)
            )%>%
  ungroup()
  

indeces<-testing_mids %>% group_by(asset_id,Plant_Type)%>%
  summarize(comp_index=first(comp_index),
            mid_merit=IQR(mid_merit))%>%
  ungroup()


ggplot(testing_mids%>% 
         mutate(contest=factor(comp_index>10),
                contest=factor(mid_merit_index>100),
                asset_id=factor(asset_id),
                asset_id=fct_reorder(asset_id,-comp_index)
                )%>%
         mutate(type_int=interaction(contest,Plant_Type))%>%
         pivot_longer(c(mid_merit),names_to = "hourly_indicator",values_to = "value"))+
  geom_boxplot(aes(asset_id,value,fill=hourly_indicator),outlier.shape = NA)+
  facet_grid(cols = vars(type_int),rows = vars(hourly_indicator),scales = "free", space = "free_x",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  scale_color_manual("",values=c("black","black"))+
  #facet_grid(~type_int,scales = "free_x", space = "free_x",
  #           labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  paper_theme()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks(n=10))+
  #expand_limits(y=1000)+
  theme(axis.text.x = element_text(angle = 90,size=10,vjust = 0.5,hjust=0.5),
        axis.text.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        axis.title.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        title = element_text(size=12),
        strip.text = element_text(size=10),
        legend.position = "none",
        
  )+
  labs(y=paste("Average Hourly Offer Value ($/MWh)"),x="",
       #title=paste("Sorting firms by price sensitivity"),
       #subtitle="Box plots show median (line),interquartile range (box) and confidence interval (whiskers) of hourly average offer price\nAverage offer price is the area under the facilty's offer function divided by the total offered MW"
       #caption="Source: AESO Data, graph by Andrew Leach."
       NULL
  )





ggplot(testing_mids%>% 
       pivot_longer(c(bid_pt,int),names_to = "hourly_indicator",values_to = "value"))+
       geom_boxplot(aes(asset_id,value,fill=hourly_indicator))+
  facet_grid(cols = vars(type_int),rows = vars(hourly_indicator),scales = "free", space = "free_x",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  scale_color_manual("",values=c("black","black"))+
  #facet_grid(~type_int,scales = "free_x", space = "free_x",
  #           labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  paper_theme()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks(n=10))+
  expand_limits(y=1000)+
  theme(axis.text.x = element_text(angle = 90,size=10,vjust = 0.5,hjust=0.5),
        axis.text.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        axis.title.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        title = element_text(size=12),
        strip.text = element_text(size=10),
        legend.position = "none",
        
  )+
  labs(y=paste("Average Hourly Offer Value ($/MWh)"),x="",
       #title=paste("Sorting firms by price sensitivity"),
       #subtitle="Box plots show median (line),interquartile range (box) and confidence interval (whiskers) of hourly average offer price\nAverage offer price is the area under the facilty's offer function divided by the total offered MW"
       #caption="Source: AESO Data, graph by Andrew Leach."
       NULL
  )



testing_mids%>%head(10000)%>%
  pivot_longer(c(bid_pt,int),names_to = "hourly_indicator",values_to = "value")%>%
  filter(!(asset_id=="HRM" & Plant_Type=="NGSC"))%>%
  ggplot()+
  geom_boxplot(aes(asset_id,value,group=asset_id))
  
  
  scale_color_manual("",values=c("black","black"))+
  facet_grid(~type_int,scales = "free_x", space = "free_x",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  paper_theme()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks(n=10))+
  expand_limits(y=1000)+
  theme(axis.text.x = element_text(angle = 90,size=10,vjust = 0.5,hjust=0.5),
        axis.text.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        axis.title.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        title = element_text(size=12),
        strip.text = element_text(size=10),
        legend.position = "none",
        
  )+
  labs(y=paste("Average Hourly Offer Value ($/MWh)"),x="",
       #title=paste("Sorting firms by price sensitivity"),
       #subtitle="Box plots show median (line),interquartile range (box) and confidence interval (whiskers) of hourly average offer price\nAverage offer price is the area under the facilty's offer function divided by the total offered MW"
       #caption="Source: AESO Data, graph by Andrew Leach."
       NULL
  )

testing_mids%>%
  filter(!(asset_id=="HRM" & Plant_Type=="NGSC"))%>%
  ggplot()+
  geom_boxplot(aes(asset_id,int,color=contest),outlier.size = 0.25)+
  scale_color_manual("",values=c("black","black"))+
  facet_grid(~type_int,scales = "free_x", space = "free_x",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  paper_theme()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks(n=10))+
  expand_limits(y=1000)+
  theme(axis.text.x = element_text(angle = 90,size=10,vjust = 0.5,hjust=0.5),
        axis.text.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        axis.title.y = element_text(size=10,vjust = 0.5,hjust=0.5),
        title = element_text(size=12),
        strip.text = element_text(size=10),
        legend.position = "none",
        
  )+
  labs(y=paste("Average Hourly Offer Value ($/MWh)"),x="",
       #title=paste("Sorting firms by price sensitivity"),
       #subtitle="Box plots show median (line),interquartile range (box) and confidence interval (whiskers) of hourly average offer price\nAverage offer price is the area under the facilty's offer function divided by the total offered MW"
       #caption="Source: AESO Data, graph by Andrew Leach."
       NULL
  )


summary_mids<-testing_mids %>% group_by(asset_id)%>%
  summarize(Plant_Type=first(Plant_Type),mean_pt_bid=first(mean_bid_t),
            iqr_bid=first(iqr_bid),
            iqr_offer=first(iqr_offer),
            contest=factor((iqr_bid>0)&(iqr_offer>0)))
  

ggsave("images/price_response)coal.png",width=14,height=7,dpi=300)
#group_by(asset_id)%>%
#summarize(range=max(int)-min(int))
