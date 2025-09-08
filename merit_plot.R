AB_palette<- c("black","grey50",ptol_pal()(6)[1],ptol_pal()(6)[4],ptol_pal()(6)[3],"orange",ptol_pal()(6)[5],ptol_pal()(6)[6])

load(file="data/hourly_summary.RData")
load(file="data/market_data.RData")

mkt_data<-mkt_data%>%left_join(ngx_data_read(),by=c("date"))%>%filter(year>=2009,year<2025)


# merge in companion market data and NIT gas prices


theme_ps <- function() {
  theme_cowplot() + 
    theme(
      text = element_text(size = 12, color = "black"), 
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11), 
      axis.line = element_line(color = "black", size = 0.8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom", 
      legend.title = element_blank(),
      legend.text = element_text(size = 11)
    ) + 
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
}

merits<-
  merit_aug%>%
  filter(he==19,size!=0,date>=ymd("2024-02-01"),date<=ymd("2024-02-04"))%>%
  left_join(mkt_data,by=c("date","he"))%>%
  group_by(date)%>%
  arrange(price,Plant_Fuel)%>%
  mutate(date_string=gsub(" 0", " ", format(date, "%B %d, %Y, HE 19 (6-7pm)")),
         date_string=paste0(date_string,"\nProvincial Demand ",actual_ail," MW\nPool Price $",actual_posted_pool_price,"/MWh"),
         date_string=as_factor(date_string),
         merit=cumsum(size),
         Plant_Fuel<-as_factor(Plant_Fuel),
         Plant_Fuel=fct_relevel(Plant_Fuel,"WIND",after=5),
         Plant_Fuel=fct_relevel(Plant_Fuel,"OTHER",after=Inf),
         )%>%
  ungroup()

  merits%>% 
    ggplot()+
  geom_rect(aes(xmin=(merit-size)/1000,xmax=merit/1000,ymin=-20,ymax=price,group=Plant_Fuel,fill=Plant_Fuel),linewidth = 0.01)+
  geom_vline(aes(xintercept=actual_ail/1000,color="Demand"),linewidth=1.25)+
  geom_hline(aes(yintercept=actual_posted_pool_price,color="Price"),linewidth=1.25,lty="21")+
  facet_wrap(~date_string,nrow = 1)+
  scale_color_manual("",values=c("black","firebrick"))+
  scale_fill_manual("",values=AB_palette)+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  expand_limits(y=1020,x=12)+
  guides(color = guide_legend(nrow = 1),order = 1,
         fill = guide_legend(nrow = 1,order = 2))+
  theme_ps()+
  theme(panel.spacing.x = unit(.5,"cm"), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")+
  labs(x=paste("Offered Generation (GW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Merit Order, select dates at hour ending 7pm"),
       caption="Source: AESO Data, graph by Andrew Leach."
  )+
  NULL
  
  ggsave(filename = "images/merit_examples.png",dpi=300,width = 11, height=6)
  

  
  merits%>% filter(date==ymd("2024-02-02"))%>%
    ggplot()+
    geom_rect(aes(xmin=(merit-size)/1000,xmax=merit/1000,ymin=-20,ymax=price,group=Plant_Fuel,fill=Plant_Fuel),linewidth = 0.31,colour="black")+
    geom_vline(aes(xintercept=actual_ail/1000,color="Demand"),linewidth=1.25)+
    geom_hline(aes(yintercept=actual_posted_pool_price+120,color="Price"),linewidth=1.25,lty="21")+
    facet_wrap(~date,nrow = 1)+
    scale_color_manual("",values=c("black","firebrick"))+
    scale_fill_manual("",values=AB_palette)+
    scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    expand_limits(y=1020,x=12)+
    guides(color = guide_legend(nrow = 1),order = 1,
           fill = guide_legend(nrow = 1,order = 2))+
    theme_ps()+
    theme(panel.spacing.x = unit(.5,"cm"), 
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal")+
    labs(x=paste("Offered Generation (GW)"),y="Price ($/MWh)",
         #title=paste("Alberta Energy Merit Order, select dates at hour ending 7pm"),
         caption="Source: AESO Data, graph by Andrew Leach."
    )+
    NULL
  
  ggsave(filename = "images/merit_one_day.png",dpi=300,width = 12, height=6)
  