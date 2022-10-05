source("power_paper_base.R")

oba_type<-function(plant_sent,year_sent){
  grid_avg<-0.65
  #nested case_when
  oba_12<-
    case_when(
      plant_sent == "HYDRO" ~ grid_avg,
      plant_sent == "COAL" ~ 1.07125*0.88,
      plant_sent == "COGEN" ~ 0.418,
      plant_sent == "NGCC" ~ 0.42*.88,
      plant_sent == "SCGT" ~ 0.6*.88,
      plant_sent == "SOLAR" ~ grid_avg,
      plant_sent == "WIND" ~ grid_avg,
      TRUE                      ~  0 
    )
  oba_20<-
    case_when(
      plant_sent == "HYDRO" ~ grid_avg,
      plant_sent == "COAL" ~ 1.07125*0.8,
      plant_sent == "COGEN" ~ 0.418,
      plant_sent == "NGCC" ~ 0.42*.8,
      plant_sent == "SCGT" ~ 0.6*.8,
      plant_sent == "SOLAR" ~ grid_avg,
      plant_sent == "WIND" ~ grid_avg,
      TRUE                      ~  0 
    )
  oba_15<-
    case_when(
      plant_sent == "HYDRO" ~ grid_avg,
      plant_sent == "COAL" ~ 1.07125*0.85,
      plant_sent == "COGEN" ~ 0.418,
      plant_sent == "NGCC" ~ 0.42*.85,
      plant_sent == "SCGT" ~ 0.6*.85,
      plant_sent == "SOLAR" ~ grid_avg,
      plant_sent == "WIND" ~ grid_avg,
      TRUE                      ~  0 
    )
  case_when(
    year_sent <=2015     ~ oba_12,
    year_sent ==2016     ~ oba_15,
    year_sent ==2017    ~ oba_20,
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

deemed_ei<-function(plant_sent,year_sent){
  case_when(
    plant_sent == "HYDRO" ~ 0,
    plant_sent == "COAL" ~ 1.07125,
    plant_sent == "COGEN" ~ 0.06,
    plant_sent == "NGCC" ~ 0.42,
    plant_sent == "SCGT" ~ 0.6,
    plant_sent == "SOLAR" ~ 0,
    plant_sent == "WIND" ~ 0,
    TRUE                      ~  0 
  )
}





load("nrgstream/nrgstream_gen.RData") ## which is here *equivalent* to

nrgstream_gen <- nrgstream_gen %>% rename(time=Time)

errors<-nrgstream_gen %>% filter(is.na(Price),date<Sys.Date())
gen_errors<-nrgstream_gen %>% filter(is.na(gen),date<Sys.Date())

#unique(gen_errors$AESO_Name)

nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 
#here, we have two sets of trade data - the AB-BC and AB-MON data and the grouped AB-WECC data

#take out AB-BC and AB-MON
#nrgstream_gen<-filter(nrgstream_gen, AESO_Name !="AB - BC Hydro  Imp/Exp Hr Avg MW",AESO_Name !="AB - Montana Imp/Exp Hr Avg MW")



sub_samp<-subset(nrgstream_gen, time > as.Date("2010-01-1"))
#sub_samp<-subset(sub_samp, time < as.Date("2017-12-31"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
sub_samp<-na.omit(sub_samp)
sub_samp$Year<-year(sub_samp$time)
df1 <- sub_samp %>% group_by(Plant_Type,time,Year) %>% summarise(sumcap = sum(Capacity),total_gen=sum(gen),p_mean=mean(Price))
df1$Year_ID=as.character(df1$Year)
# Histogram of Generation Densities
ggplot(df1,aes(total_gen))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_x_continuous(limits=range(df1$total_gen),expand=c(0,0),breaks = pretty_breaks())+
  scale_y_continuous(expand=c(0,0),labels = scales::percent)+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Wind Generation (MW)",y="% of hours generation < X MW",
       title="Cumulative Density Function, Wind Energy (2010-2017 Avg)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
ggsave(file="images/wind_cdf.png")




trade_set<-c("AB - BC Hydro Imp Hr Avg MW", "AB - Montana Imp Hr Avg MW",   "AB - Saskpower Imp AB Hr Avg MW",
             "AB - BC Hydro Exp Hr Avg MW", "AB - Montana Exp Hr Avg MW",   "AB - Saskpower Exp AB Hr Avg MW")

#exclude WECCs so that you don't double-count
trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")


sub_samp<-filter(nrgstream_gen, time >= as.Date("2010-01-1"))
#take out the double-counted trade series (WECC)

df_test<-filter(sub_samp,is.na(gen))

df1 <- sub_samp %>% filter(! NRG_Stream %in% trade_excl)%>% group_by(Plant_Type,time) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),p_mean=mean(Price)) %>% ungroup()
df1$Day <- date(df1$time)
df1$Year <- as.factor(year(df1$time))
#df1<-na.omit(df1)
#df1$Revenue <- df1$total_gen*df1$p_mean

gen_set<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND")


#test_samp<-nrgstream_gen %>% filter(year(time)==2019)
#test_samp2<-test_samp %>% filter(Plant_Type %in% gen_set,! NRG_Stream %in% trade_excl)


df2 <- df1 %>% filter(Plant_Type %in% gen_set,year(time)<2022) %>%
       group_by(Plant_Type,Year) %>% summarise(capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean))


#make vol-weighted avg




df2$Plant_Type<-fct_relevel(df2$Plant_Type, "OTHER",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "HYDRO",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "WIND",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "SOLAR",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "TRADE",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "IMPORT",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "EXPORT",after=Inf)



df2 <-df2 %>% mutate(ei=deemed_ei(Plant_Type,as.character(Year)), oba=oba_type(Plant_Type,as.character(Year)), ctax=ctax_year(as.character(Year)),ctax_net=(deemed_ei(Plant_Type,as.character(Year))-oba_type(Plant_Type,as.character(Year)))*ctax_year(as.character(Year)),
                     ctax_net_rev=avg_rev-ctax_net,
                     policy=ifelse(as.character(Year)>="2018","CCIR","SGER"))

#df3$Plant_Type_New<-factor(df3$Plant_Type_New)

#df3<-df3 %>% melt(id=c("Plant_Type_New","Year"),measure.vars=c("capture","ctax_net_rev"))





set_png(file="images/price_capture_avg.png", width = 1400, height = 750)
my_palette<-c(colors_tableau10()[8],colors_tableau10_medium()[4],colors_tableau10()[4],colors_tableau10_light()[4],colors_tableau10()[7],colors_tableau10()[1],colors_tableau10()[3],colors_tableau10()[2],colors_tableau10()[9],colors_tableau10_light()[9])

ggplot(df2,aes(Year,capture-p_mean,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture-p_mean,colour=Plant_Type,fill=Plant_Type),size=1.5,position = position_dodge(width = .9),width = .6)+
  #scale_color_viridis("Plant Type",discrete=TRUE)+
  #scale_fill_viridis("Plant Type",discrete=TRUE)+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  
  slide_theme()+
  labs(x="",y="Revenue Relative to Mean Price ($/MWh)",
       title="Energy Price Capture Differential ($/MWh, 2007-2018)",
       caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach")
dev.off()

set_png(file="images/price_capture_pct.png", width = 1400, height = 750)
my_palette<-c(colors_tableau10()[8],colors_tableau10_medium()[4],colors_tableau10()[4],colors_tableau10_light()[4],colors_tableau10()[7],colors_tableau10()[1],colors_tableau10()[3],colors_tableau10()[2],colors_tableau10()[9],colors_tableau10_light()[9])
ggplot(df2,aes(Year,capture/p_mean*100-100,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture/p_mean*100-100,colour=Plant_Type,fill=Plant_Type),size=1.5,position = position_dodge(width = .9),width = .6)+
  #scale_color_viridis("Plant Type",discrete=TRUE)+
  #scale_fill_viridis("Plant Type",discrete=TRUE)+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  
  slide_theme()+
  labs(x="",y="Revenue Relative to Average Price (%)",
       title="Energy Price Capture Differential (%, 2007-2019)",
       caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach")
dev.off()


df3 <- df1 %>% filter(Plant_Type %in% gen_set,year(time)<2022) %>%
  group_by(Year) %>% summarise(capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean))%>%
  mutate(Plant_Type="MARKET",Plant_Type=as_factor(Plant_Type))

df2<-df2 %>% bind_rows(df3)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "MARKET",after=0)


df4<-tibble(Year=seq(2010,2016),Plant_Type="SOLAR",capture=0)%>%mutate(Year=as_factor(Year),
                                                                       Plant_Type=as_factor(Plant_Type))
df2<-df2 %>% bind_rows(df4)


my_palette<-c("black",colors_tableau10()[8],colors_tableau10_medium()[4],colors_tableau10()[4],colors_tableau10_light()[4],colors_tableau10()[7],colors_tableau10()[1],colors_tableau10()[3],colors_tableau10()[2],colors_tableau10()[9],colors_tableau10_light()[9])
my_palette<-c("black",grey.colors(10,start=0.2,end = .95))

plot_a<-ggplot(df2,aes(Year,capture,fill=Plant_Type))+
  geom_col(aes(Year,capture,fill=Plant_Type),position = position_dodge(width = .9),width = .6,color="black",size=.5)+
  #geom_col(aes(Year,p_mean),position = "identity",fill=NA,color="black")+
  #geom_line(dataaes(Year,capture,fill=Plant_Type),position = position_dodge(width = .9),width = .6,color="black",size=.5)+
  #geom_text(aes(y=-10,label=Plant_Type),angle=90,size=2)+
  #  scale_color_viridis("Plant Type",discrete=TRUE)+
#  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  blake_theme()+theme(plot.margin =unit(c(1,1,1,1),"cm"),
                      legend.position = "bottom")+
  guides(fill=guide_legend(nrow = 1,label.position = "bottom",keywidth = 5))+
labs(x="",y="Average Revenue ($/MWh)",
     #title="Energy Price Capture ($/MWh, 2010-2021)",
     #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
     )
plot_a
ggsave(file="images/price_capture.png", width = 14, height = 8)



capture_new<-
  ggplot(df2,aes(y=capture,x=Plant_Type))+
  geom_bar(stat="identity",alpha=0.5,width=.9, position = "dodge",color="black")+
  facet_wrap(~Year,nrow = 1)+
  #coord_flip()+
  geom_text(aes(y=capture+.5,label=Plant_Type),angle=90,size=1.6,hjust=0,vjust=0.35)+
  #  scale_color_viridis("Plant Type",discrete=TRUE)+
  #  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  #scale_color_manual("",values=my_palette)+
  #scale_fill_manual("",values=my_palette)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank())+
  expand_limits(y=0)+
  labs(x="",y="Average Revenue ($/MWh)",
       #title="Energy Price Capture ($/MWh, 2010-2021)",
       #caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach"
       )
capture_new
ggsave(file="images/capture_test.png",width=14,height = 8,dpi=150)





df2 <-df2 %>% filter(!Plant_Type %in% c("OTHER", "TRADE","IMPORT","EXPORT","MARKET"))
#set_png(file="images/price_capture_tax.png", width = 1400, height = 750)

my_palette<-c(colors_tableau10()[8],colors_tableau10_medium()[4],colors_tableau10()[4],colors_tableau10_light()[4],colors_tableau10()[1],colors_tableau10()[3],colors_tableau10()[2],colors_tableau10()[9])
plot_a<-ggplot(filter(df2,as.character(Year)>=2014))+
  #geom_col(aes(Year,capture,fill=Plant_Type),size=1.5,alpha=0.75,position = position_dodge(width = .9),width = .6)+
  #geom_col(aes(Year,ctax_net_rev,fill=Plant_Type),alpha=0.75,size=1.5,position = position_dodge(width = .9),width = .6)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  geom_col(aes(Year,ctax_net_rev-avg_rev,fill=Plant_Type),alpha=0.75,size=1.5,position = position_dodge(width = .9),width = .6)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
    #  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  slide_theme()+
  labs(x="Year",y="Average Revenue ($/MWh)",
       title="Net Revenue from GHG Policies ($/MWh, 2014-2019)",
       caption="Source: AESO and SGER Data, with assumption that renewables capture full offset value pre-2018.\n AESO data accessed via NRGStream, graph by @andrew_leach")
plot_a
#dev.off()

plot_b<-ggplot(filter(df2,as.character(Year)>=2014))+
  geom_col(aes(Year,ctax_net_rev,fill=Plant_Type),colour=NA,alpha=0.5,position = position_dodge(width = .9),width = .75)+
  #geom_point(aes(Year,ctax_net_rev,group=Plant_Type,fill=Plant_Type),alpha=0.75,size=1.5,position = position_dodge(width = .9),shape=22)+
  geom_col(aes(Year,capture,colour=Plant_Type),fill=NA,alpha=1,position = position_dodge(width = .9),width = .7)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #geom_col(aes(Year,capture-ctax_net_rev,colour=Plant_Type,group=Plant_Type),fill=NA,alpha=0.75,width = .75)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #geom_col(aes(Year,ctax_net_rev-avg_rev,fill=Plant_Type),alpha=0.75,size=1.5,position = position_dodge(width = .9),width = .6)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  guides(fill=guide_legend(nrow=1,byrow=TRUE),colour=guide_legend(nrow=1,byrow=TRUE))+
  small_theme()+
  labs(x="Year",y="Average Revenue ($/MWh)",
       title="Change in Energy Price Capture Due to GHG Policies (2014-2019)",
       subtitle="Outline shows market revenues, fill shows market revenue plus OBA values less carbon pricing costs",
       caption="Source: AESO and SGER Data, with assumption that renewables capture full offset value pre-2018.\n AESO data accessed via NRGStream.")

plot_b



plot_c<-ggplot(filter(df2,as.character(Year)>=2014))+
  geom_col(aes(Year,oba*ctax,colour=Plant_Type,fill=Plant_Type),alpha=1,position = position_dodge(width = .9),width = .7)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #geom_col(aes(Year,capture-ctax_net_rev,colour=Plant_Type,group=Plant_Type),fill=NA,alpha=0.75,width = .75)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #geom_col(aes(Year,ctax_net_rev-avg_rev,fill=Plant_Type),alpha=0.75,size=1.5,position = position_dodge(width = .9),width = .6)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  guides(fill=guide_legend(nrow=1,byrow=TRUE),colour=guide_legend(nrow=1,byrow=TRUE))+
  small_theme()+
  labs(x="Year",y="Average Credit Value ($/MWh)",
       title="Value of Credit Allocations Under GHG Policies (2014-2019)",
       caption="Source: AESO and SGER Data, with assumption that renewables capture full offset value pre-2018.\n AESO data accessed via NRGStream, graph by @andrew_leach")
plot_c




plot_d<-ggplot(filter(df2,as.character(Year)>=2014))+
  geom_col(aes(Year,ei*ctax,colour=Plant_Type,fill=Plant_Type),alpha=1,position = position_dodge(width = .9),width = .7)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #geom_col(aes(Year,capture-ctax_net_rev,colour=Plant_Type,group=Plant_Type),fill=NA,alpha=0.75,width = .75)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #geom_col(aes(Year,ctax_net_rev-avg_rev,fill=Plant_Type),alpha=0.75,size=1.5,position = position_dodge(width = .9),width = .6)+#  scale_color_viridis("Plant Type",discrete=TRUE)+
  #  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  guides(fill=guide_legend(nrow=1,byrow=TRUE),colour=guide_legend(nrow=1,byrow=TRUE))+
  small_theme()+
  labs(x="Year",y="Average Hourly Carbon Cost ($/MWh)",
       title="Raw Cost of GHG Policies (2014-2019)",
       caption="Source: AESO and SGER Data, with assumption that renewables capture full offset value pre-2018.\n AESO data accessed via NRGStream, graph by @andrew_leach")
plot_d




set_png(file="images/price_capture_ctax.png",width=1200,height = 850)
plot_b
dev.off()


set_png(file="images/oba_value.png",width=1200,height = 850)
plot_c
dev.off()

set_png(file="images/cost_ctax.png",width=1200,height = 850)
print(plot_d)
dev.off()




#write.xlsx(df2, file = "cansia.xlsx", colNames = TRUE, borders = "columns")

plot_a<-ggplot(filter(df2,as.character(Year)>=2014),aes(Year,capture,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture,colour=Plant_Type,fill=Plant_Type),size=1.5,position = position_dodge(width = .9),width = .6)+
  #  scale_color_viridis("Plant Type",discrete=TRUE)+
  #  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  slide_theme()+
  labs(x="Year",y="Average Revenue ($/MWh)",
       title="Energy Price Capture ($/MWh, 2007-2019)")
plot_b<-ggplot(filter(df2,as.character(Year)>=2014),aes(Year,capture,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,ctax_net_rev,colour=Plant_Type,fill=Plant_Type),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  slide_theme()+
  labs(x="Year",y="Average Revenue net of GHG policies ($/MWh)",
       title="Energy Price Capture Net of Carbon Policies ($/MWh, 2007-2019)",
       caption="Source: AESO and SGER Data, with assumption that renewables capture full offset value pre-2018.\n AESO data accessed via NRGStream, graph by @andrew_leach")
library(patchwork)
library(cowplot)

set_png(file="images/price_diff_ctax.png",width=1600,height = 1200)
(plot_a + theme(legend.position = "none"))/(plot_b+guides(fill=guide_legend(nrow=1,byrow=TRUE),colour=guide_legend(nrow=1,byrow=TRUE)))
dev.off()



week_index <- function(x)format(x, '%Y.%W')

month_index <- function(x)format(x, '%b-%Y')



sub_samp<-subset(nrgstream_gen, time > as.Date("2007-01-1"))
#sub_samp<-subset(sub_samp, time < as.Date("2017-12-31"))
sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
sub_samp<-na.omit(sub_samp)
sub_samp$Year<-year(sub_samp$time)
df1 <- sub_samp %>% filter(! NRG_Stream %in% trade_excl)%>% group_by(Plant_Type,time) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),p_mean=mean(Price)) %>% ungroup()
df1$Day <- date(df1$time)
df1$Year <- as.factor(year(df1$time))


df2 <- df1 %>% mutate(month=month(time))%>% filter(Plant_Type %in% c("NGCC","SCGT","COGEN","COAL")) %>% 
  group_by(Year,month,Plant_Type) %>% summarise(gen=sum(total_gen),capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean)) %>% 
  mutate(ei=deemed_ei(Plant_Type,as.character(Year)), oba=oba_type(Plant_Type,as.character(Year)), ctax=ctax_year(as.character(Year)),ctax_net=(deemed_ei(Plant_Type,as.character(Year))-oba_type(Plant_Type,as.character(Year)))*ctax_year(as.character(Year)),
                     ctax_net_rev=avg_rev-ctax_net,
                     policy=ifelse(as.character(Year)>="2018","CCIR","SGER"),
                    date=ymd(paste(Year,month,15,sep="-")))




gen_plain <- df2 %>% filter(date>=ymd("2010-01-01")) %>%
  ggplot(aes(date,gen/days_in_month(month)/24, col = Plant_Type)) +
  geom_line(size = 1.2)+
  #annotate("text", x = as.Date("2014-8-1")-days(20), y = 5600, label = "SGER in place at\n12% and $15/tonne\nsince July 2007",size=3.8)+  
  #annotate("text", x = as.Date("2016-1-1")-days(20), y = 5600, label = "SGER changed to\n15% and $20/tonne\nJan 2016",size=3.8)+  
  #annotate("text", x = as.Date("2017-1-1")-days(20), y = 5600, label = "SGER changed to\n20% and $30/tonne\nJan 2017",size=3.8)+  
  #annotate("text", x = as.Date("2018-1-1")-days(20), y = 5600, label = "SGER changed to\nCCIR at $30/tonne\nJan 2018",size=3.8)+  
  
  slide_theme()+
  scale_x_date(expand = c(0,0),date_labels = "%b\n%Y",date_breaks = "12 months")+
  scale_y_continuous(limits=c(0,5990),expand = c(0,0))+
  scale_color_manual("",values=colors_tableau10()[1:4])+
  theme(legend.position = "none")+
  labs(x="Year",y="Average Hourly Generation (MW)",
       #title="Coal and Gas Generation and Carbon Prices (MW, 20010-2020)",
       title="Coal and Gas Generation (MW, 2010-2020)",
       caption="Source: AESO Data, accessed via NRGStream")
 

p<-gen_plain+
  annotate("text", x = as.Date("2012-1-1")-days(20), y =5600, label = str_wrap(width = 30,"SGER in place. OBAs at 88% of historic emissions intensity and $15/tonne carbon price"),size=2.5)+  
  annotate("text", x = as.Date("2016-1-1")-days(20), y = 5600, label = str_wrap(width = 17,"SGER changed to 85% OBA and $20/tonne carbon price"),size=2.5)+  
  annotate("text", x = as.Date("2017-1-1")-days(20), y = 5600, label = str_wrap(width = 17,"SGER changed to 80% OBA and $30/tonne carbon price"),size=2.5)+  
  annotate("text", x = as.Date("2018-1-1")-days(20), y = 5600, label = str_wrap(width = 17,"SGER changed to CCIR with fixed 0.37t/MWh OBA and $30/tonne carbon price"),size=2.5)+  
  
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2015-12-29"), xmax =as.Date("2016-1-3"),
           ymin = 0, ymax = 5200) +
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2016-12-29"), xmax =as.Date("2017-1-3"),
           ymin = 0, ymax = 5200) +
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2017-12-29"), xmax =as.Date("2018-1-3"),
           ymin = 0, ymax = 5200) 
  


direct_labels <- df2 %>% 
  group_by(Plant_Type) %>%
  summarize(
    x = last(date)-months(1), 
    y = 1000
  )
direct_labels$y<-c(3000,3400,1000,400)


direct_labels_axis <- axis_canvas(p, axis = "y") +
  geom_text(
    data = direct_labels, 
    aes(y = y, label = Plant_Type), 
    x = 0.06, 
    hjust = 0, 
    size = 5, 
    col = colors_tableau10()[1:4]
  )

p_direct_labels <- insert_yaxis_grob(p, direct_labels_axis)

plain_direct_labels <- insert_yaxis_grob(gen_plain, direct_labels_axis)
ggdraw(plain_direct_labels)

set_png(file="images/gen_ghg_price.png", width = 2000, height = 1000)
ggdraw(p_direct_labels)
dev.off()

set_png(file="images/gen_plain.png", width = 1600, height = 1000)
ggdraw(plain_direct_labels)
dev.off()

set_png(file="images/gen_old.png", width = 1600, height = 1000)
ggdraw(p_direct_labels)
dev.off()




df2 <- df1 %>% group_by(Day,Year) %>% summarise(capture = sum(total_rev)/sum(total_gen))
df2<-subset(df2, Year==2019 |Year==2018 | Year==2017 | Year==2014)
set_png(file="images/price_capture_all.png", width = 1400, height = 750)
ggplot(df2,aes(capture,group=Year,colour=Year),alpha=0.5)+
  geom_density(aes(),size=1.5)+
  scale_x_continuous(limits=c(0,100),expand = c(0,0))+
  scale_color_viridis(discrete=TRUE)+
  ajl_line()+
  geom_hline(aes(yintercept=0),size=1.5,colour="black")+
  labs(x="Daily Average Power Revenues ($/MWh)",y="Density",
       title="Distribution of Daily Energy Revenues ($/MWh)",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()


#weekly gen mix graph





df2 <- df1 %>% filter(time>=max(time)-days(21)-years(1) & time<=max(time)-days(7)) %>%
  mutate(weekday=wday(time, label=TRUE)) %>% filter(Plant_Type %in% gen_set,Plant_Type != "EXPORT",weekday!="Sun",weekday!="Sat") %>%
  mutate(day=day(time),month=month.abb[month(time)],hour=hour(time)) %>%   
  group_by(Year,month,hour,Plant_Type) %>% summarise(gen=mean(total_gen),capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean),day=mean(day)) %>% 
  mutate(date=ymd(paste(Year,month,round(day),sep="-")))


#write.xlsx(df2, file = "cansia.xlsx", colNames = TRUE, borders = "columns")

#emissions now built into the gen data by default


vol_test<-nrgstream_gen %>% filter(Plant_Type %in% gen_set) %>% mutate(month=month(time),year=year(time)) %>%
  group_by(month,year,Plant_Type) %>% 
  summarise(vol=sum(gen,na.rm = T),AIL=mean(AIL),price=mean(as.numeric(Price)),ghg=sum(co2_est/1000*gen)) %>% 
  ungroup()%>%
  mutate(date=ymd(paste(year,month,15,sep="-")))


vol_test$Plant_Type <- as_factor(vol_test$Plant_Type)

vol_test$Plant_Type<-fct_relevel(vol_test$Plant_Type, "OTHER",after=Inf)
vol_test$Plant_Type<-fct_relevel(vol_test$Plant_Type, "HYDRO",after=Inf)
vol_test$Plant_Type<-fct_relevel(vol_test$Plant_Type, "WIND",after=Inf)
vol_test$Plant_Type<-fct_relevel(vol_test$Plant_Type, "SOLAR",after=Inf)
vol_test$Plant_Type<-fct_relevel(vol_test$Plant_Type, "TRADE",after=Inf)
vol_test$Plant_Type<-fct_relevel(vol_test$Plant_Type, "IMPORT",after=Inf)
vol_test$Plant_Type<-fct_relevel(vol_test$Plant_Type, "EXPORT",after=Inf)


my_palette<-c(colors_tableau10()[8],colors_tableau10_medium()[4],colors_tableau10()[4],colors_tableau10_light()[4],colors_tableau10()[7],colors_tableau10()[1],colors_tableau10()[3],colors_tableau10()[2],colors_tableau10()[9],colors_tableau10_light()[9])

png<-1
if(png==1)
  set_png(file="images/monthly_ghgs.png")
ggplot(filter(vol_test,!Plant_Type %in% c("IMPORT","EXPORT"),date<=Sys.Date()-days(30)))+
  geom_area(aes(date,ghg*12/10^6,colour=Plant_Type,group=Plant_Type,fill=Plant_Type),size=2,position="stack")+
  #geom_line(aes(date,AIL))+
  small_theme()+
  scale_x_date(expand = c(0,0),date_labels = "%b\n%Y",date_breaks = "12 months")+
  scale_y_continuous(expand = expand_scale(mult=c(0,.05)))+
  geom_hline(aes(yintercept=0),color="black",size=1.25)+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  labs(x="Date",y="Annualized Emissions Rate (Mt/yr)",
       title="Monthly Emissions from Power Generation",
       caption="Source: AESO Data\nCalculations and graph by Andrew Leach")
if(png==1)
  dev.off()

avg_ghgs<-vol_test %>% group_by(date,year) %>% summarise(vol=sum(vol),price=mean(price),ghg=sum(ghg)) %>% ungroup() %>% 
  group_by(year) %>% mutate(annual_ghgs=sum(ghg))


png<-1
if(png==1)
  set_png(file="images/monthly_ghg_mwh.png")
ggplot(filter(avg_ghgs,year>=2007))+
  geom_line(aes(date,ghg/vol))+
  geom_point(aes(date,ghg/vol))+
  small_theme()+
  scale_x_date(expand = c(0,0))+
  #scale_y_continuous(limits=c(0.5,0.799),expand = c(0,0))+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  annotate("text", x = as.Date("2014-1-1")-days(20), y =.83, label = str_wrap(width = 30,"SGER in place. OBAs at 88% of historic emissions intensity and $15/tonne carbon price"),size=2.5)+  
  annotate("text", x = as.Date("2016-1-1")-days(20), y = .83, label = str_wrap(width = 17,"SGER changed to 85% OBA and $20/tonne carbon price"),size=2.5)+  
  annotate("text", x = as.Date("2017-1-1")-days(20), y = .83, label = str_wrap(width = 17,"SGER changed to 80% OBA and $30/tonne carbon price"),size=2.5)+  
  annotate("text", x = as.Date("2018-1-1")-days(20), y = .83, label = str_wrap(width = 17,"SGER changed to CCIR with fixed 0.37t/MWh OBA and $30/tonne carbon price"),size=2.5)+  
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2015-12-29"), xmax =as.Date("2016-1-3"),
           ymin = 0.5, ymax = 0.8) +
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2016-12-29"), xmax =as.Date("2017-1-3"),
           ymin = 0.5, ymax = 0.8) +
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2017-12-29"), xmax =as.Date("2018-1-3"),
           ymin = 0.5, ymax = 0.8) +
  labs(x="",y="Emissions Rate (tonnes/MWh)",
       title="Monthly Emissions Intensity of Alberta's Power Generation")
       #caption="Source: AESO data accessed via NRGStream. Calculations and graph by Andrew Leach")
if(png==1)
  dev.off()







sub_samp<-subset(nrgstream_gen, time >= as.Date("2004-01-1"))
sub_samp$Month_Year <- decimal_date(sub_samp$time)
sub_samp$Year <- year(sub_samp$time)
sub_samp$Month <- month(sub_samp$time)
sub_samp$Hour <- hour(sub_samp$time)
sub_samp$Date_ID = as.yearmon(sub_samp$time)



df1 <- sub_samp %>% filter(!is.na(Price))%>% filter(!is.na(gen)) %>%
  group_by(Date_ID) %>% 
  summarise(total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))


df1$Date<-as.Date(df1$Date_ID)
df1$avg_rev<-df1$total_rev/df1$total_gen
df1$m12_avg<-as.numeric(rollapply(df1$avg_rev,12,mean,fill=NA,align = c("right")))



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("monthly_prices.png")
ggplot(data=df1, aes(Date,total_rev/total_gen,colour="A"),size=2.5) +
  geom_line(size=1.5) +
  geom_line(data=subset(df1,Date>="2005-01-01"),aes(Date,m12_avg,colour="B"),size=2.5) +
  scale_color_viridis("",labels=c("Monthly Average Prices","12 Month Rolling Average Prices"),discrete=TRUE)+   
  scale_x_date(expand = c(0,0),date_labels = "%b\n%Y",date_breaks = "12 months")+
  scale_y_continuous(limits=c(0,200),expand=c(0,0))+
  small_theme()+
  labs(y="Power Price ($/MWh)",x="",
             title="Generation-Weighted Monthly Average Energy Prices ($/MWh, 2005-2018)",
             caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

  
#hourly prices  

  sub_samp<-subset(nrgstream_gen, as.Date(time) >= as.Date("2004-01-1"))
  sub_samp$month<-month(sub_samp$time)
  sub_samp$Year<-as.character(year(sub_samp$time))
  sub_samp$Hour<-as.factor(hour(sub_samp$time)+1)
  
  df1 <- sub_samp %>% group_by(Hour,Year) %>% summarise(AIL=mean(AIL, na.rm = TRUE),meancap = mean(Cap_Fac, na.rm = TRUE),total_gen=sum(gen, na.rm = TRUE),total_rev=sum(Revenue, na.rm = TRUE),p_mean=mean(Price, na.rm = TRUE))

  set_png(file="images/hourly-prices.png", width = 1200, height = 800)
  ggplot(df1, aes(Hour,total_rev/total_gen,group=Year,colour=Year)) +
    geom_line(data=subset(df1,Year<=2018),size=1.5) +
    geom_line(data=subset(df1,Year==2019),size=3) +
    geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis("",discrete=TRUE)+   
    ajl_hourly()+  
    labs(y="Hourly Average Power Price ($/MWh)",x="Hour",
               title="Generation-Weighted Hourly Average Energy Prices",
               caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  set_png(file="images/hourly-prices-recent.png", width = 1200, height = 800)
  ggplot(df1, aes(Hour,total_rev/total_gen,group=Year,colour=Year)) +
    geom_line(data=subset(df1,Year>2015),size=1.5) +
    geom_line(data=subset(df1,Year==2019),size=3) +
    labs(colour="Year") +
    scale_color_viridis("",discrete=TRUE)+   
    slide_theme()+  
    labs(y="Hourly Average Power Price ($/MWh)",x="Hour",
         title="Generation-Weighted Hourly Average Energy Prices",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  
  #df1<-filter(df1,as.numeric(Year)!=2018)
  set_png(file="images/hourly-loads.png", width = 1200, height = 800)
  ggplot(df1, aes(Hour,AIL,group=Year,colour=Year)) +
    #geom_line(data=subset(df1,Year<2017),size=1.5) +
    #geom_line(data=subset(df1,Year==2017),size=3) +
    geom_line(data=subset(df1,Year>=2007),size=1.5) +
    #geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+   
    ajl_hourly()+    labs(y="Hourly Load (MW)",x="Hour",
               title="Hourly Average Internal Load (MW, 2007-2019)",
               caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  #monthly loads  
  
  
  
  df1 <- sub_samp %>% group_by(month,Year) %>% summarise(peak_AIL=max(AIL, na.rm=TRUE),AIL=mean(AIL, na.rm = TRUE),meancap = mean(Cap_Fac, na.rm = TRUE),total_gen=sum(gen, na.rm = TRUE),total_rev=sum(Revenue, na.rm = TRUE),p_mean=mean(Price, na.rm = TRUE))
  df1$Year=as.character(df1$Year)
  df1$month<-factor(month.abb[df1$month],levels = month.abb)
  set_png(file="images/monthly-peak-loads.png")
  ggplot(df1, aes(month,peak_AIL,group=as.factor(Year),colour=as.factor(Year))) +
    #geom_line(data=subset(df1,Year<2017),size=1.5) +
    #geom_line(data=subset(df1,Year==2017),size=3) +
    geom_line(data=subset(df1,Year>=2007),size=1.5) +
    #geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+ 
    ajl_hourly()+    labs(y="Monthly Peak Load (MW)",x="Month",
               title="Monthly Peak Alberta Internal Load (MW, 2007-2019)",
               caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  set_png(file="images/monthly-avg-loads.png")
  ggplot(df1, aes(month,AIL,group=as.factor(Year),colour=as.factor(Year))) +
    #geom_line(data=subset(df1,Year<2017),size=1.5) +
    #geom_line(data=subset(df1,Year==2017),size=3) +
    geom_line(data=subset(df1,Year>=2007),size=1.5) +
    #geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+ 
    ajl_hourly()+    labs(y="Monthly Average Load (MWh)",x="Month",
                           title="Monthly Average Alberta Internal Load (MW, 2007-2019)",
                           caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  

  
strike_price<-37

df1 <-nrgstream_gen %>% filter(ID %in% "WHT1") %>%
  mutate(month=month(time),year=year(time))%>%
  group_by(month,year)%>% 
  summarize(price=mean(Price),total_gen=sum(gen,na.rm = T),rev=sum(gen*Price,na.rm = T),
          cfd=sum(gen*strike_price,na.rm = T),avg_rev=rev/total_gen)%>% filter(total_gen>0)%>%
  mutate(mid_mth=ymd(paste(year,month,15,sep="-")))%>%
  select(date=mid_mth,gen=total_gen,rev,cfd,avg_rev)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("images/wht_cfd_revs_equiv.png")
ggplot(df1) +
  geom_line(aes(date,avg_rev,colour="Average Revenue"),size=1.5) +
  geom_line(aes(date,strike_price,colour="CFD Strike Price"),size=1.5) +
  scale_color_viridis("",labels=c("Pool Price Revenues","CFD Strike Price"),discrete=TRUE)+   
  scale_x_date(expand = c(0,0),date_labels = "%b\n%Y",date_breaks = "2 month")+
  scale_y_continuous(breaks = pretty_breaks())+
  small_theme()+
  #theme(axis.text.x = element_text(margin = margin(t = 20)))+
  labs(y="Average Revenue (MWh)",x="",
       title=paste("Monthly Revenues at Market Prices and with CFD at $",strike_price,"/MWh",sep=""),
       caption=paste("Selling at pool prices would have provided revenues of $",
                 round(sum(df1$rev)/10^6,2)," million.\nRevenue from a CFD at $",
                     +       strike_price,"/MWh would have been $",round(sum(df1$cfd)/10^6,2)," million.\nSource: AESO data, accessed via NRGStream, calculations and graph by Andrew Leach.\n ",sep=""))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






sub_samp<-subset(nrgstream_gen, time > as.Date("2017-06-28"))
sub_samp<-subset(sub_samp, time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,time) %>% summarise(gen = sum(gen,na.rm = T))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("TRADE","WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))


png(file="gen_month.png", width = 1400, height = 750)
p <- ggplot(df1, aes(time,gen)) 
p + geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
  scale_fill_brewer(palette="Spectral") +
  #scale_fill_viridis(discrete = FALSE,option="A")+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Date",y="Hourly Generation (MW)",
       title="Distribution of Hourly Energy Production (MWh, May, 2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()


sub_samp<-subset(nrgstream_gen, time > as.Date("2017-07-21"))
sub_samp<-subset(sub_samp, time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

set_png(file="images/gen_week.png", width = 1400, height = 750)
ggplot(df1, aes(time,newvar))+
geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
  geom_line(aes(time,ail,colour="Alberta Internal Load"),size=1.5) +
  scale_fill_manual("",values = colors_tableau10())+
  scale_colour_manual("",values = c("black"))+ 
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Hourly Generation (MW)",
       title="Distribution of Hourly Energy Production (MWh), July 21-28, 2017",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()


sub_samp<-subset(nrgstream_gen, time > as.Date("2018-07-21"))
sub_samp<-subset(sub_samp, time < as.Date("2018-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

set_png(file="images/gen_week_2018.png", width = 1400, height = 750)
ggplot(df1, aes(time,newvar))+
  geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
  geom_line(aes(time,ail,colour="Alberta Internal Load"),size=1.5) +
  scale_fill_manual("",values = colors_tableau10())+
  scale_colour_manual("",values = c("black"))+ 
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Hourly Generation (MW)",
       title="Distribution of Hourly Energy Production (MWh), July 21-28, 2018",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()

sub_samp<-subset(nrgstream_gen, time > as.Date("2014-07-21"))
sub_samp<-subset(sub_samp, time < as.Date("2014-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))



set_png(file="images/gen_week_2014.png", width = 1400, height = 750)
ggplot(df1, aes(time,newvar))+
  geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
  geom_line(aes(time,ail,colour="Alberta Internal Load"),size=1.5) +
  scale_fill_manual("",values = colors_tableau10())+
  scale_colour_manual("",values = c("black"))+ 
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Hourly Generation (MW)",
       title="Distribution of Hourly Energy Production (MWh), July 21-28, 2014",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()




sub_samp<-subset(nrgstream_gen, time > as.Date("2017-01-1"))
sub_samp<-subset(sub_samp, time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))

#df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

png(file="load_ytd.png", width = 1400, height = 750)
p <- ggplot(df1, aes(time,ail))
 p+  geom_line(size=1.5) +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Date",y="Alberta Internal Load (MW)",
       title="Hourly Energy Load (MW), 2017",
       subtitle="Source: AESO Data, Accessed via NRGStream")
dev.off()


sub_samp<-subset(nrgstream_gen, time > as.Date("2013-10-01"))
sub_samp<-subset(sub_samp, time < as.Date("2018-10-01"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)
sub_samp$Date<-date(sub_samp$time)


df1 <- sub_samp %>% group_by(Plant_Type,Date) %>% summarise(newvar = sum(gen))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

png(file="gen_all.png", width = 1400, height = 750)
p <- ggplot(df1, aes(Date,newvar/1000))
p + geom_area(aes(fill=Plant_Type2), position = 'stack')+ 
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual("",values=colors_tableau10()) +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Date",y="Daily Generation (GWh)",
       title="Daily Average Energy Production (GWh), 2014-present",
       subtitle="Source: AESO Data")
dev.off()







sub_samp<-subset(nrgstream_gen, time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, time < as.Date("2017-06-19"))
sub_samp<-subset(sub_samp, Plant_Fuel=="GAS")
sub_samp<-subset(sub_samp, ID=="NX02")


sub_samp$Date_ID = as.yearmon(sub_samp$time)
df1 <- sub_samp %>% group_by(Date_ID) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))

ggplot(df1, aes(month(Date_ID, label=TRUE, abbr=TRUE), 
                +                 total_gen, group=factor(year(Date_ID)), colour=factor(year(Date_ID)))) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  labs(x="Month", colour="Year") +
  theme_classic()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    plot.caption = element_text(size = 18, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16)
  )+
  labs(x="Monthly  Power Generation (MWh)",y="",
       title="time Series of Monthly Total Generation (MWh, 2014-2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")



sub_samp<-subset(nrgstream_gen, time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, time < as.Date("2017-06-19"))
sub_samp<-subset(sub_samp, Plant_Fuel=="WIND")
sub_samp<-subset(sub_samp, ID=="HAL1")


sub_samp$Date_ID <- as.yearmon(sub_samp$time)
sub_samp$Week_ID <- factor(str_c(week(sub_samp$time),year(sub_samp$time), sep = "_", collapse = NULL))
sub_samp$Week <- week(sub_samp$time)


df1 <- sub_samp %>% group_by(Week,Date_ID) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
ggplot(df1, aes(Week,total_gen, group=factor(year(Date_ID)), colour=factor(year(Date_ID)))) +
  geom_line(size=1.5)


ggplot(df1, aes(Week,total_rev/total_gen, group=factor(year(Date_ID)), colour=factor(year(Date_ID)))) +
  geom_line(size=1.5)





daily_forwards <- read.csv("forwards_daily.csv",skip = 4,header = TRUE, stringsAsFactors=FALSE)
daily_forwards<-daily_forwards[,-c(3,4,5)]
colnames(daily_forwards)<-c("Trade_Date","Inst_Date","Settle","Volume_MW","Open_Int_MW")
daily_forwards$Trade_Date<-as.Date(daily_forwards$Trade_Date,format="%m/%d/%Y")
daily_forwards$Inst_Date<-as.Date(daily_forwards$Inst_Date,format="%m/%d/%Y")

df1 <- daily_forwards %>% group_by(Trade_Date,Inst_Date) %>% summarise(Settle = mean(Settle),n=n()) %>% ungroup()
#df1=melt(df1,id=c("Trade_Date","Inst_Year"),variable.name = "term",value.name = "settle")
today_date<-max(daily_forwards$Trade_Date)
dates<-c(today_date,today_date-weeks(1),today_date-weeks(2),today_date-weeks(3),today_date-weeks(4))

df1 <- df1 %>% filter(Inst_Date<=min(dates))

png<-1
if(png==1)
  set_png(file="images/daily_forwards.png")

ggplot(subset(df1,Trade_Date %in% dates)) +
  geom_line(data=subset(df1,Trade_Date %in% dates),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date)),size=2)+
  #geom_line(aes(Inst_Year,Peak_Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype="dashed"),size=2)+
  scale_color_brewer("Trade Date",palette = "Set1",guide=guide_legend(nrow=2,order=1))+
  scale_x_date(labels = date_format("%b\n%d"),
               breaks = seq(from = min(df1$Inst_Date), to = max(df1$Inst_Date)+months(2), by = "2 weeks"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.key.width=unit(3,"line"),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves (Daily Flat)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





## @knitr power_forwards_prelims



clean_forwards<-function(forwards_sent,type){
#forwards_sent<-read.csv(file="off_peak_forwards.csv",header = TRUE, stringsAsFactors=FALSE,skip=1)
#date_format<-guess_formats(forwards_sent[,1])
#forwards_sent[,1]<-as.Date(forwards_sent[,1],format=date_format)
#forwards_sent[,2]<-as.Date(forwards_sent[,2],format=date_format)

forwards_sent<-forwards_sent[,-c(3,4,5)]
colnames(forwards_sent)<-c("Trade_Date","Inst_Date","Settle","Volume_MW","Open_Int_MW")
forwards_sent$Inst_Year<-year(forwards_sent$Inst_Date)
forwards_sent$Inst_Month<-month(forwards_sent$Inst_Date)
forwards_sent$Type<-type
return(forwards_sent)

}


old_forwards<-function(){
forwards <- clean_forwards(read.xlsx(xlsxFile = "nrgstream/NGX_forwards.xlsx", sheet = "forwards_flat", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE),type="FLAT")
#forwards <- read.xlsx(xlsxFile = "NGX_forwards.xlsx", sheet = "forwards_flat", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
peak_forwards <- clean_forwards(read.xlsx(xlsxFile = "nrgstream/NGX_forwards.xlsx", sheet = "forwards_peak", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE),type="PEAK")
ext_peak_forwards <- clean_forwards(read.xlsx(xlsxFile = "nrgstream/NGX_forwards.xlsx", sheet = "forwards_ext_peak", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE),type="EXT_PEAK")
off_peak_forwards <- clean_forwards(read.xlsx(xlsxFile = "nrgstream/NGX_forwards.xlsx", sheet = "forwards_off_peak", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE),type="OFF_PEAK")

#all_forwards<-arrange(rbind(forwards,peak_forwards,ext_peak_forwards,off_peak_forwards),c("Trade_Date","Inst_Date","Type"))
all_forwards<-rbind(forwards,peak_forwards,ext_peak_forwards,off_peak_forwards)
all_forwards<-all_forwards%>%filter(Trade_Date<as.Date("2018-01-01"))
save(all_forwards,file="nrgstream/forwards.RData")
}

#old_forwards()


proc_forwards<-function(file_sent,type){
  forwards<-read.csv(file=file_sent,header = TRUE, stringsAsFactors=FALSE,skip=1)
  formats<-unique(guess_formats(forwards[,1],"mdY"))
  forwards[,1]<-as.Date(forwards[,1],format=formats)
  forwards[,2]<-as.Date(forwards[,2],format=formats)
  forwards<-forwards[,-c(3,4,5)]
  colnames(forwards)<-c("Trade_Date","Inst_Date","Settle","Volume_MW","Open_Int_MW")
  forwards$Inst_Year<-year(forwards$Inst_Date)
  forwards$Inst_Month<-month(forwards$Inst_Date)
  forwards$Type<-type
  return(forwards)
}



update_forwards<-function(forwards_sent){
  #testing
  #forwards_sent<-all_forwards
  #clip all_forwards
  clipped_forwards<-forwards_sent%>%filter(Trade_Date<as.Date("2018-01-01"))
  #load new ones
  forwards<-proc_forwards("nrgstream/forwards2018.csv","FLAT")
  peak_forwards<-proc_forwards("nrgstream/peak_forwards.csv","PEAK")
  ext_peak_forwards<-proc_forwards("nrgstream/ext_peak_forwards.csv","EXT_PEAK")
  off_peaks<-proc_forwards("nrgstream/off_peak_forwards.csv","OFF_PEAK")
  #stack new ones
  new_forwards<-rbind(clipped_forwards,forwards,peak_forwards,ext_peak_forwards,off_peaks)
  new_forwards<-arrange(new_forwards,Trade_Date,Inst_Date,Type)
  
}

load("nrgstream/forwards.Rdata")
all_forwards<-update_forwards(all_forwards)
save(all_forwards,file="nrgstream/forwards.RData")





## @knitr power_forwards_graphs


df1 <- all_forwards %>% group_by(Trade_Date,Inst_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))
#df1 <- test %>% group_by(Trade_Date,Inst_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))

#df1=melt(df1,id=c("Trade_Date","Inst_Year"),variable.name = "term",value.name = "settle")
df1$Type<-factor(df1$Type,levels = c("EXT_PEAK","OFF_PEAK","FLAT","PEAK"))
today_date<-as.Date("2017-12-31")

today_date<-max(all_forwards$Trade_Date)
#today_date<-max(test$Trade_Date)

df1$Inst_Year<-factor(df1$Inst_Year,levels = sort(unique(df1$Inst_Year)))


dates<-c(today_date,today_date-years(1),today_date-years(2),today_date-years(3),today_date-years(4))
dates<-c(today_date,today_date-years(1))

png<-1
if(png==1)
  set_png(file="images/forwards.png")

ggplot(subset(df1,Trade_Date %in% dates)) +
  #geom_line(data=subset(df1,Trade_Date %in% dates & Type=="OFF_PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  geom_line(data=subset(df1,Trade_Date %in% dates & Type=="FLAT"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  geom_line(data=subset(df1,Trade_Date %in% dates & Type=="PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  #geom_line(aes(Inst_Year,Peak_Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype="dashed"),size=2)+
  scale_y_continuous(limits=c(0,100))+
  scale_color_brewer("Trade Date",palette = "Set1",guide=guide_legend(nrow=2,order=1))+
  scale_linetype("Contract",guide=guide_legend(nrow=3,order=1))+ # Change linetypes
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.key.width=unit(3,"line"),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves (Calendar Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

dates<-c(today_date)

png<-1
if(png==1)
  set_png(file="images/forwards_all.png")
ggplot(subset(df1,Trade_Date %in% dates)) +
  geom_line(data=subset(df1,Trade_Date %in% dates & Type=="OFF_PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  geom_line(data=subset(df1,Trade_Date %in% dates & Type=="FLAT"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  geom_line(data=subset(df1,Trade_Date %in% dates & Type=="PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  #geom_line(aes(Inst_Year,Peak_Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype="dashed"),size=2)+
  scale_color_brewer("Trade Date",palette = "Set1",guide=guide_legend(nrow=1,order=1))+
  scale_y_continuous(limits=c(0,100))+
  scale_linetype("Contract",guide=guide_legend(nrow=2,order=1))+ # Change linetypes
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.key.width=unit(3,"line"),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves (Calendar Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



today_date<-max(all_forwards$Trade_Date)
dates<-c(today_date,today_date-weeks(1),today_date-weeks(2),today_date-weeks(3))


df1 <- all_forwards %>% filter(Trade_Date %in% dates) %>% group_by(Trade_Date,Inst_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))
#df1 <- test %>% group_by(Trade_Date,Inst_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))

df1$Type<-factor(df1$Type)
png<-1
if(png==1)
  set_png(file="images/forwards_monthly.png")
ggplot(subset(df1,Trade_Date %in% dates)) +
  geom_line(data=subset(df1,Trade_Date %in% dates & Type=="FLAT"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  geom_line(data=subset(df1,Trade_Date %in% dates & Type=="PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  scale_color_brewer("Trade Date",palette = "Set1",guide=guide_legend(nrow=2,order=1))+
  scale_linetype("Contract",guide=guide_legend(nrow=2,order=1))+ # Change linetypes
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = min(df1$Inst_Date), to = max(df1$Inst_Date)+months(2), by = "12 months"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.key.width=unit(3,"line"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves (Monthly Flat Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


today_date<-max(all_forwards$Trade_Date)
dates<-c(today_date,ymd("2019-04-16"),ymd("2015-05-05"),ymd("2015-11-22"))


df1 <- all_forwards %>% filter(Trade_Date %in% dates) %>% group_by(Trade_Date,Inst_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))
#df1 <- test %>% group_by(Trade_Date,Inst_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))

df1$Type<-factor(df1$Type)
png<-1
if(png==1)
  set_png(file="images/forwards_kenney.png")
ggplot(subset(df1,Trade_Date %in% dates)) +
  geom_line(data=subset(df1,Trade_Date %in% dates & Type=="FLAT"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  #geom_line(data=subset(df1,Trade_Date %in% dates & Type=="PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  scale_color_brewer("Trade Date",palette = "Set1",guide=guide_legend(nrow=2,order=1),labels=c("Notley elected","CLP Introduced","Kenney elected",as.character.Date(today_date)))+
  scale_linetype("Contract",guide=F)+ # Change linetypes
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = min(df1$Inst_Date), to = max(df1$Inst_Date)+months(2), by = "12 months"))+
  theme_minimal()+
  theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.key.width=unit(3,"line"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves (Monthly Flat Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)
  set_png(file="images/forwards_kenney_tax.png")
ggplot(subset(df1,Trade_Date %in% dates)) +
  geom_line(data=subset(df1,Trade_Date %in% dates[1:2] & Type=="FLAT"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  #geom_line(data=subset(df1,Trade_Date %in% dates & Type=="PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=2)+
  scale_color_brewer("Trade Date",palette = "Set1",guide=guide_legend(nrow=2,order=1),labels=c("Kenney elected",as.character.Date(today_date)))+
  scale_linetype("Contract",guide=F)+ # Change linetypes
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = min(df1$Inst_Date), to = max(df1$Inst_Date)+months(2), by = "12 months"))+
  theme_minimal()+
  theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.key.width=unit(3,"line"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves (Monthly Flat Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




df1 <- all_forwards %>% group_by(Trade_Date,Inst_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))

df2<-subset(df1,Inst_Date %in% c(as.Date("2018-04-01"))& Trade_Date> as.Date("2017-01-01") & Type=="PEAK")

png<-1
if(png==1)
  set_png(file="images/forwards_summer2017.png")
inst_dates<-c(as.Date("2018-04-01"),as.Date("2018-05-01"),as.Date("2018-06-01"),as.Date("2018-07-01"))

ggplot() +
  geom_line(data=subset(df1,Inst_Date %in% inst_dates & Trade_Date> as.Date("2017-01-01") & Type=="FLAT"),
            aes(Trade_Date,Settle,group=as.factor(Inst_Date),colour=as.factor(Inst_Date)),size=2)+
  geom_line(data=subset(df1,Inst_Date %in% inst_dates & Trade_Date> as.Date("2017-01-01") & Type=="PEAK"),
            aes(Trade_Date,Settle,group=as.factor(Inst_Date),colour=as.factor(Inst_Date)),size=2)+
  scale_x_date(date_breaks = "3 months",date_labels = "%b\n%Y")+
  #geom_vline(xintercept = as.Date("2017-04-19")+
  facet_wrap(~Type)+
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2017-04-19"), xmax =as.Date("2017-04-20"),
           ymin = 0, ymax = 100) +
  annotate("text", x = as.Date("2017-02-15"), y = 90, label = "TransAlta\n April 19th\nAnnouncement\n on Sundance\n Shutdowns",size=2.8)+
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2017-12-05"), xmax =as.Date("2017-12-06"),
           ymin = 0, ymax = 100) +
  annotate("text", x = as.Date("2017-10-5"), y = 90, label = "TransAlta\n December 6th\nAnnouncement\n on Sundance\n Shutdowns",size=2.8)+
  #annotate("rect", fill = "black", alpha = 0.8, 
  #         xmin = as.Date("2018-1-12"), xmax =as.Date("2018-1-13"),
  #         ymin = 30, ymax = 100) +
  #annotate("text", x = as.Date("2017-12-25"), y = 80, label = "Balancing Pool\n January 12th\nAnnouncement\n on BR5 PPA\nTermination",size=2.8)+
  #geom_vline(data=ev,aes(xtintercept=as.numeric(dt)))
  scale_color_brewer("Instrument Date",palette = "Set1",guide=guide_legend(nrow=2,order=1),labels=c("April-18","May-18","June-18","July-18"))+
  #scale_linetype("Contract",guide=guide_legend(nrow=2,order=1))+ # Change linetypes
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    legend.key.width=unit(3,"line"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nTrade Date",
             title="Alberta Power Forward Contract Values (Calendar Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





df1 <- all_forwards %>% group_by(Trade_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))
df1$Type<-factor(df1$Type)


png<-1
if(png==1)
  set_png(file="images/forwards_2017.png")

ggplot() +
  geom_line(data=subset(df1,Inst_Year %in% c(2018,2019,2020)& Trade_Date> as.Date("2017-01-01") & Type=="FLAT"),
    aes(Trade_Date,Settle,group=as.factor(Inst_Year),colour=as.factor(Inst_Year),linetype=Type),size=2)+
  geom_line(data=subset(df1,Inst_Year %in% c(2018,2019,2020)& Trade_Date> as.Date("2017-01-01") & Type=="PEAK"),
            aes(Trade_Date,Settle,group=as.factor(Inst_Year),colour=as.factor(Inst_Year),linetype=Type),size=2)+
  #geom_vline(xintercept = as.Date("2017-04-19")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b\n%Y")+
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2017-04-19"), xmax =as.Date("2017-04-20"),
           ymin = 30, ymax = 90) +
  annotate("text", x = as.Date("2017-02-15"), y = 70, label = "TransAlta\n April 19th\nAnnouncement\n on Sundance\n Shutdowns",size=2.8)+
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2017-12-05"), xmax =as.Date("2017-12-06"),
           ymin = 30, ymax = 90) +
  annotate("text", x = as.Date("2017-10-1"), y = 70, label = "TransAlta\n December 6th\nAnnouncement\n on Sundance\n Shutdowns",size=2.8)+
  #annotate("rect", fill = "black", alpha = 0.8, 
  #         xmin = as.Date("2018-1-12"), xmax =as.Date("2018-1-13"),
  #         ymin = 30, ymax = 100) +
  #annotate("text", x = as.Date("2017-12-25"), y = 80, label = "Balancing Pool\n January 12th\nAnnouncement\n on BR5 PPA\nTermination",size=2.8)+
    #geom_vline(data=ev,aes(xtintercept=as.numeric(dt)))
  scale_color_brewer("Calendar Strip Year",palette = "Set1",guide=guide_legend(nrow=2,order=1))+
  scale_linetype("Contract",guide=guide_legend(nrow=2,order=1))+ # Change linetypes
  facet_wrap(~Type)+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    legend.key.width=unit(3,"line"),
    plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nTrade Date",
             title="Alberta Power Forward Contract Values (Calendar Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






png<-1
if(png==1)
  set_png(file="images/forwards_long.png",height=1000)
ggplot() +
  geom_line(data=subset(df1,Inst_Year %in% c(2018,2019,2020,2021)& Trade_Date> as.Date("2014-01-01")& Trade_Date< as.Date("2019-05-01")&Type=="FLAT"),
            aes(Trade_Date,Settle,group=as.factor(Inst_Year),colour=as.factor(Inst_Year),linetype=Type),size=2)+
  geom_line(data=subset(df1,Inst_Year %in% c(2018,2019,2020,2021)& Trade_Date> as.Date("2014-01-01")& Trade_Date< as.Date("2019-05-01")&Type=="PEAK"),
            aes(Trade_Date,Settle,group=as.factor(Inst_Year),colour=as.factor(Inst_Year),linetype=Type),size=2)+
  scale_color_brewer("Calendar Strip Year",palette = "Set1",guide=guide_legend(nrow=2,order=1))+
  facet_wrap(~Type,nrow=2)+
  scale_linetype("Contract",guide=guide_legend(nrow=2,order=1))+ # Change linetypes
  scale_y_continuous(limits = c(0,110), breaks= seq(0,80,20))+
  scale_x_date(labels = date_format("%b\n%Y"),
               breaks = seq(from = min(df1$Trade_Date), to = max(df1$Trade_Date)+months(12), by = "6 months"))+
  annotate("rect", fill = "black", alpha = 1, 
           xmin = as.Date("2014-8-16"), xmax =as.Date("2014-8-18"),
           ymin = 0, ymax = 90) +
  annotate("text", x = as.Date("2014-8-17"), y = 100, label = "Brent\nLast Above\n $100/bbl",size=2.4)+  
  
    annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2015-05-14"), xmax =as.Date("2015-05-16"),
           ymin = 0, ymax = 90) +
  annotate("text", x = as.Date("2015-04-15"), y = 100, label = "NDP\nElected\nin Alberta",size=2.4)+
  
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2015-11-21"), xmax =as.Date("2015-11-23"),
           ymin = 0, ymax = 90) +
  annotate("text", x = as.Date("2015-11-22"), y = 100, label = "Climate\nLeadership\nPlan\nAnnounced",size=2.4)+
  
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2015-06-21"), xmax =as.Date("2015-06-23"),
           ymin = 0, ymax = 90) +
  annotate("text", x = as.Date("2015-07-23"), y = 100, label = "SGER\nPrice\nIncrease\n Announced",size=2.4)+
  
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2017-04-18"), xmax =as.Date("2017-04-20"),
           ymin = 0, ymax = 90) +
  annotate("text", x = as.Date("2017-4-19"), y = 100, label = "TransAlta\n April 19th\nAnnouncement\n on Sundance\n Shutdowns",size=2.4)+
 
   annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2017-12-4"), xmax =as.Date("2017-12-6"),
           ymin = 0, ymax = 90) +
  annotate("text", x = as.Date("2017-12-5"), y = 100, label = "TransAlta\n December 6th\nAnnouncement\n on Sundance\n Shutdowns",size=2.4)+
  
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2016-03-23"), xmax =as.Date("2016-03-25"),
           ymin = 0, ymax = 90) +
  annotate("text", x = as.Date("2016-3-24"), y = 100, label = "Enmax\nKH PPA\nReturned\nto BP",size=2.4)+
  
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2016-07-24"), xmax =as.Date("2016-07-26"),
           ymin = 0, ymax = 90) +
    annotate("text", x = as.Date("2016-7-25"), y = 100, label = "AB gov't\nPPA lawsuit\nAnnounced",size=2.4)+
      theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nTrade Date",
             title="Alberta Power Forward Contract Values (Calendar Strip)",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#get actual prices






  sub_samp<-subset(nrgstream_gen, time > mdy_h("07/26/2017 08",tz="GMT"))
  sub_samp<-subset(sub_samp, time <= mdy_h("07/26/2017 22",tz="GMT"))
  sub_samp<-arrange(sub_samp,AESO_Name)
  
  lims <- c(min(sub_samp$time),max(sub_samp$time))
  breaks <- make_breaks(min(sub_samp$time), hour=0, interval='2 hour', length.out=length(sub_samp))
  
  #Grab just the coal units
  sub_samp<-sub_samp[grep("COAL", sub_samp$Plant_Type), ]
  #sub_samp<-sub_samp[-grep("GN3", sub_samp$ID), ]
  #sub_samp<-sub_samp[-grep("HRM", sub_samp$ID), ]
  #sub_samp<-sub_samp[-grep("BR3", sub_samp$ID), ]
  #sub_samp<-sub_samp[grep("Gen", sub_samp$AESO_Name), ]
  down<-"SD6|SD5|SD4|SD1|SD2|KH2"
  sub_samp<-sub_samp[grep(down, sub_samp$AESO_Name), ]
  
  
  #Down or off HRM BR5 SD6 SD5 SD4 SD1 #SD2 KH2
  #Up KH1 BR4 BR5
  #stable GN1,2,3
  
  sub_samp<-arrange(sub_samp,AESO_Name)
  
  #group gen into types
  #sub_samp<-na.omit(sub_samp)
  
  df1 <- sub_samp
  sub_samp<-subset(nrgstream_gen, time > mdy_h("07/26/2017 08",tz="GMT"))
  sub_samp<-subset(sub_samp, time <= mdy_h("07/26/2017 22",tz="GMT"))
  
  df2<-sub_samp %>% group_by(Plant_Type,time) %>% summarise(gen.MWh = sum(gen),ail=mean(AIL),price=mean(Price))
  df2<-df2[grep("WIND", df2$Plant_Type), ]
  df2<-df2[-grep("FCAST", df2$Plant_Type), ]
  
  
  
  png(file="coal_summer_peak.png", width = 1400, height = 750)
  
  #lims <- c(mdy_h("07/27/2017 00",tz=tz(now())),mdy_h("07/27/2017 23",tz=tz(now())))
  breaks <- make_breaks(min(sub_samp$time), hour=0, interval='2 hour', length.out=length(sub_samp))
  ggplot(df1,aes(time,gen)) + 
    #geom_area(data =subset(df1,!grepl("TRADE", df1$Plant_Type)), aes(time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =subset(df1,grepl("COAL", df1$Plant_Type)), aes(time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =df1, aes(time,gen.MWh,group=Plant_Type,fill=Plant_Type), position = 'stack',size=1.5)+
    geom_line(data =df1, aes(time,gen,colour=AESO_Name,group=AESO_Name),size=1.5)+
    geom_line(data =df1, aes(time,Price,colour="Alberta Power Price"),size=1.5)+
    geom_line(data =df2, aes(time,gen.MWh,colour="Wind Generation"),size=1.5)+
    #geom_line(data =df1, aes(time,AIL/100),size=1.5)+
    
    scale_colour_viridis(discrete = TRUE,option="viridis")+
    
    #scale_fill_brewer(type = "seq", palette = "Set1", direction = 1,labels=c("Coal"))+
    #scale_fill_viridis(discrete = TRUE,option="viridis",labels=c("Coal"))+
    
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+
    labs(x="Date",y="Hourly Generation (MW) or Price ($/MWh)",
         title="Drops in production during price-capped hours, July 26,2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
   
  png(file="ail_summer_peak.png", width = 1400, height = 750)
  
  #lims <- c(mdy_h("07/27/2017 00",tz=tz(now())),mdy_h("07/27/2017 23",tz=tz(now())))
  breaks <- make_breaks(min(sub_samp$time), hour=0, interval='2 hour', length.out=length(sub_samp))
  ggplot(df1,aes(time,gen)) + 
    #geom_area(data =subset(df1,!grepl("TRADE", df1$Plant_Type)), aes(time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =subset(df1,grepl("COAL", df1$Plant_Type)), aes(time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =df1, aes(time,gen.MWh,group=Plant_Type,fill=Plant_Type), position = 'stack',size=1.5)+
    #geom_line(data =df1, aes(time,gen,colour=AESO_Name,group=AESO_Name),size=1.5)+
    #geom_line(data =df1, aes(time,Price,colour="Alberta Power Price"),size=1.5)+
    #geom_line(data =df2, aes(time,gen.MWh,colour="Wind Generation"),size=1.5)+
    geom_line(data =df1, aes(time,AIL,colour="Alberta Internal Load"),size=1.5)+
    
    scale_colour_viridis(discrete = TRUE,option="viridis")+
    
    #scale_fill_brewer(type = "seq", palette = "Set1", direction = 1,labels=c("Coal"))+
    #scale_fill_viridis(discrete = TRUE,option="viridis",labels=c("Coal"))+
    
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+
    labs(x="Date",y="Hourly Demand (MW)",
         title="Alberta Demand during price-capped hours, July 26,2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
  
  
   
  

  

  
#AURORA WIND OUTAGE DATA
  
  sub_samp<-nrgstream_gen[grep("WIND", nrgstream_gen$Plant_Fuel), ] #wind plants
  sub_samp<-subset(sub_samp, time > as.Date("2016-01-01"))
  sub_samp<-subset(sub_samp, time < as.Date("2016-12-31"))
  sub_samp<-na.omit(sub_samp)
  
  #df2<-df2[-grep("WIND_FCAST", df2$Plant_Type), ] #wind plants
  
  df2<-sub_samp
  df2$month<-month(df2$time)
  df2$hour<-hour(df2$time)
  df2$week<-week(df2$time)
  df2$Year<-year(df2$time)
  df2$Day<-as.factor(format(as.Date(df2$time), "%A"))
  
  #df1 <- df2  %>% group_by(AESO_Name,Year) %>% summarise(gen.MWh = sum(gen))
  df1 <- df2 %>% group_by(AESO_Name,ID,month,Day,hour) %>% summarise(gen.MWh = mean(gen),avg_rev=sum(gen*Price)/sum(gen),cap_fac=100*mean(gen/Capacity),outage=100-100*mean(gen/Capacity))
  df1$Day <- ordered(df1$Day,labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  
  #df1 <- df2
  


  
  #data set with monthly outage for 2016 for each of the plants in AB
  plants<-unique(df1$AESO_Name)
  months<-unique(df1$month)
  months<-month.abb[months]
  wind_data <-  as.data.frame(matrix(0, ncol = 170, nrow = 0))
  names(wind_data)<-c("ID","Name",paste("", 1:length(wind_data[,-(1:2)]), sep = ""))
  count<-1
  for(pname in plants){
     plant_data<-df1[grep(pname, df1$AESO_Name)[],c(1,3,9,2)] 
     plant_data$month<-month.abb[plant_data$month]
     for(m_data in months){
        #print(paste("WindShape_Area56_",df2[grep(pname, df2$AESO_Name)[1],7],"_",sep=""))
        v_name<-paste("WindShape_Area56_",count,"_",m_data,sep="")
        p_data<-plant_data[grep(m_data,plant_data$month)[],(3)]
        p_data_m<-(cbind(v_name,paste("Wind data based on ",pname," for ",m_data),t(p_data)))
        #need to write this to data frame
        wind_data[nrow(wind_data)+1,]<-p_data_m
        }
    count<-count+1
    }
  
  #df1<-melt(df1,id=c("AESO_Name"),measure.vars = c("cap_fac","avg_rev"))
  #df1$variable <- factor(df1$variable, labels = c("Capacity Factor (%)", "Average Revenue ($/MWh)"))
for(i in c(3:170)) 
    wind_data[,(i)]<-as.numeric(wind_data[,(i)])
  
 
  
  
   write.xlsx(wind_data, file = "wind_outages.xlsx", colNames = TRUE, borders = "columns")

  paired_viridis<-function(pairs){
   palette<-c(viridis(pairs)[1],viridis(pairs)[1])
    if(pairs>1)
      for(c in c(2:pairs))
        palette<-c(palette,viridis(pairs)[c],viridis(pairs)[c])
  return(palette)
  }
  
  sub_samp<-subset(nrgstream_gen, time > mdy_h("07/01/2016 00",tz="GMT"))
  sub_samp<-subset(sub_samp, time <= mdy_h("7/31/2016 23",tz="GMT"))
  sub_samp<-arrange(sub_samp,time,NRG_Stream)
  lims <- c(min(sub_samp$time),max(sub_samp$time))
  breaks <- make_breaks(min(sub_samp$time), hour=0, interval='168 hour', length.out=length(sub_samp))
    
  
  palette_nrgstream_gen<-c(paired_viridis(2),"Black")
  png(file="july_wind.png", width = 1400, height = 1500,res=130,type='cairo') 
  ggplot(sub_samp,aes(time,gen)) + 
      #geom_line(data =subset(sub_samp, (NRG_Stream %in% c("AB - Wind Generation Unit Net MW")),group=NRG_Stream,colour=NRG_Stream)) +
      geom_line(data =subset(sub_samp,(NRG_Stream %in% c("AB - Wind Generation Unit Net MW"))), aes(group=NRG_Stream,colour=NRG_Stream),size=3)+
      geom_line(data =subset(sub_samp, grepl("12 Hr", sub_samp$NRG_Stream)), aes(group=NRG_Stream,colour=NRG_Stream),size=1.5,linetype="dashed")+
      #geom_line(data =subset(sub_samp, grepl("7 Day", sub_samp$NRG_Stream)), aes(group=NRG_Stream,colour=NRG_Stream),size=1.5,linetype="dashed")+    
     scale_colour_manual(values = palette_nrgstream_gen)+
     scale_x_datetime(labels = date_format("%d/%m %H:00"))+
     guides(colour = guide_legend(nrow = 2))+
     theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(.1,.1,.1,.1),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+    labs(y="Alberta Wind Generation (MW)",x="Hour",
               title="Alberta hourly actual and forecast wind generation\nJuly 26-29th, 2017",
               caption="Source: AESO Data, Accessed via NRGStream\n Graph by Andrew Leach") 
  dev.off()  
  
  
  
  
  
  
  lto_data <- read.xlsx(xlsxFile = "data/aesoLTO.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  colnames(lto_data)[1] <- "Date"
  
  #lto_data$`2004.Forecast`[is.na(lto_data$`2004.Forecast`)] <- lto_data$Actual[is.na(lto_data$`2004.Forecast`)]
  #lto_data$`2005.Forecast`[is.na(lto_data$`2005.Forecast`)] <- lto_data$Actual[is.na(lto_data$`2005.Forecast`)]
  #for(cname in colnames(lto_data[,grep("20", colnames(lto_data))]))
  # lto_data[is.na(lto_data[,cname]),cname] <- lto_data[is.na(lto_data[,cname]),'Actual']
  #lto_data<-na.omit(lto_data)
  df1<-lto_data %>% pivot_longer(-Date, names_to = "Forecast",values_to = "AIL_Est") %>% na.omit()%>% filter(
    Forecast != "2019.High.Growth",
    Forecast != "2019.Low.Growth",
    Forecast != "2016H",
    Forecast != "2014L",
    Forecast != "2014.EnviroShift",Forecast != "2014.EnergyTrans",
    Forecast != "2016L",
  ) %>% mutate(Forecast=as_factor(Forecast),Forecast=fct_relevel(Forecast,"Actual",after = Inf))
  
  set_png(file="images/AESO_LTO.png", width = 1400, height = 750)
  #jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
  ggplot(filter(df1,Forecast!="Actual"),aes(Date,AIL_Est,group = Forecast,colour=Forecast)) +
    geom_line(size=1.7) +
    geom_line(data=filter(df1,Forecast=="Actual"),aes(Date,AIL_Est,group = Forecast,colour=Forecast),size=3) +
    
      #geom_point(size=1) +
    #scale_color_viridis(labels=c("2004 Forecast","2006 Forecast", "2007 Forecast" ,"2008 Forecast","2009 Forecast","2012 Forecast" , "2014 Reference Case", "2016 Reference Case","Actual AIL"),discrete=TRUE)+   
    #scale_colour_brewer(labels=c("2004 Forecast","2006 Forecast", "2007 Forecast" ,"2008 Forecast","2009 Forecast","2012 Forecast" , "2014 Reference Case", "2016 Reference Case","Actual AIL"),type = "seq", palette = "Greens", direction = 1)+
    #scale_colour_brewer(type = "seq", palette = "Greens", direction = 1)+
    scale_colour_manual(NULL,labels=c("2004 Forecast","2005 Forecast","2006 Forecast", "2007 Forecast" ,"2008 Forecast","2009 Forecast","2012 Forecast" , "2014 Reference Case", "2016 Reference Case","2017 Reference Case","2019 Reference Case","Actual AIL"),
                        values=c(brewer.pal(9,"Blues"),"grey70","grey40","Black"))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Alberta Peak Internal Load (MW)",x="Year",
               title="AESO Outlook Forecasts of Alberta Peak Internal Load (MW, 2004-2019)",
               caption="Source: AESO Reports as compiled by Andrew Leach and Calder Watrich. Graph: Andrew Leach.")
  
 dev.off()
 
 
 lto_capacity <- read.xlsx(xlsxFile = "Reports/AESO/2017-LTO-data-file.xlsx", sheet = "Generation Capacity by Type", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
 scenarios<-lto_capacity$`Capacity.by.Scenario.and.Fuel.Type.(MW)`[grep("Year",lto_capacity$`Capacity.by.Scenario.and.Fuel.Type.(MW)`)-1]
 lto_capacity<-lto_capacity[-(grep("Year",lto_capacity$`Capacity.by.Scenario.and.Fuel.Type.(MW)`)-1),]
 colnames(lto_capacity) <- lto_capacity[1,]
 lto_capacity<-lto_capacity[-(grep("Year",lto_capacity$Year)),]
 rownames(lto_capacity)<-NULL
 lto_capacity<-lto_capacity[-(36:37),]
 #create scenario index
 x<-matrix(scenarios,nrow = length(scenarios),ncol = 5)
 lto_capacity$scenario<-matrix(t(x),nrow = nrow(lto_capacity),ncol = 1)
 rm(x)
 lto_capacity$Year[grep("2017*",lto_capacity$Year)]<-"2017"
 lto_capacity$Year<-factor(lto_capacity$Year,levels = as.numeric(unique(lto_capacity$Year)))
 lto_capacity[,-c(1,12)]<-lapply(lto_capacity[,-c(1,12)], function(x) as.numeric(as.character(x)))
 
 df1<-melt(lto_capacity,id=c("Year","scenario"),value.name = "Capacity",variable.name = "Source")  
 df1$scenario<-factor(df1$scenario,levels = unique(df1$scenario))
 my_palette<-c("#313695",brewer.pal(9, "Set1"),"Black")
 set_png("aeso_capacity.png",width = 1400,height =800) 
 ggplot(filter(df1,Source!="Total"),group=Year)+
   geom_col(aes(Year,Capacity,colour=Source,fill=Source),size=.35,position = "stack")+
   facet_grid( ~ scenario)+
   #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
   #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
   #scale_fill_viridis("",discrete=TRUE)+
   scale_fill_manual("",values = my_palette,guide = "legend")+
   scale_colour_manual("",values=my_palette,guide = "legend")+
   guides(fill=guide_legend(nrow =2,byrow=FALSE))+
   theme(panel.border = element_blank(),
         panel.grid = element_blank(),
         panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
         axis.line.x = element_line(color = "gray"),
         axis.line.y = element_line(color = "gray"),
         axis.text = element_text(size = 16),
         axis.text.x = element_text(margin = margin(t=0),angle = 90,hjust = .50,vjust = .50),
         axis.title = element_text(size = 16),
         #axis.label.x = element_text(size=20,vjust=+5),
         plot.subtitle = element_text(size = 16,hjust=0.5),
         plot.caption = element_text(face="italic",size = 12,hjust=0),
         legend.key.width=unit(2,"line"),
         legend.position = "bottom",
         #legend.direction = "horizontal",
         #legend.box = "horizontal",
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         plot.title = element_text(hjust=0.5,size = 20),
         plot.margin=unit(c(1,1,1.5,1.2),"cm")
   )+
   labs(x=NULL,y=expression('Installed Capacity (MW)'),
        title="2017-2037 AESO Installed Capacity Scenarios",
        #subtitle="Excluding Electricity",
        caption="Source: AESO 2017 Long Term Outlook\nGraph by @andrew_leach")
 dev.off()
 

 
 monthly_solar<-nrgstream_gen %>% 
   filter(Plant_Type=="SOLAR",!is.na(gen),he=="13")%>%
   mutate(month=month(Time),year=year(Time))%>%
   group_by(month,year,ID,AESO_Name)%>%summarize(gen=mean(gen,na.rm = T))%>%
   mutate(date=ymd(paste(year,"-",month,"-1",sep="")))%>%
   ungroup()
 
 
monthly_solar %>%
  ggplot()+
  geom_col(aes(date,gen,group=ID,fill=ID),position = "stack")+
  scale_x_date(expand=c(0,0),breaks="3 months",labels = date_format("%b\n%Y",tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0))+
  expand_limits(x=c(ymd("2017-12-31"),Sys.Date()+months(2)),y=800)+
  scale_fill_viridis("",option="C",discrete = T)+
  guides(fill=guide_legend(ncol = 1))+
   theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(margin = margin(t=0),hjust = .50,vjust = .50),
        axis.title = element_text(size = 16),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 16,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "right",
        #legend.box = "horizontal",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust=0.5,size = 20),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")
  )+
  labs(x=NULL,y=expression('Mean Noon Hour Generation (MW)'),
       #title="2017-2037 AESO Installed Capacity Scenarios",
       #subtitle="Excluding Electricity",
       caption="Data via NRGStream, graph by @andrew_leach")

ggsave(file=paste("images/solar.png",sep=""),width = 14,height=7,dpi=300,bg="white")



proc_forwards<-function(file_sent,type){
  forwards<-read.csv(file=file_sent,header = TRUE, stringsAsFactors=FALSE,skip=1)
  formats<-unique(guess_formats(forwards[,1],"mdY"))
  forwards[,1]<-as.Date(forwards[,1],format=formats)
  forwards[,2]<-as.Date(forwards[,2],format=formats)
  forwards<-forwards[,-c(3,4,5)]
  colnames(forwards)<-c("Trade_Date","Inst_Date","Settle","Volume_MW","Open_Int_MW")
  forwards$Inst_Year<-year(forwards$Inst_Date)
  forwards$Inst_Month<-month(forwards$Inst_Date)
  forwards$Type<-type
  return(forwards)
}



new_forwards<-function(){
  #load existing ones up to Jan 2018
  #load("forwards.Rdata")
  #clip all_forwards
  #all_forwards<-all_forwards%>%filter(Trade_Date<as.Date("2018-01-01"))
  #import and proces new ones
  forwards<-proc_forwards(paste(nrg_folder,"forwards2018.csv",sep="/"),"FLAT")
  peak_forwards<-proc_forwards(paste(nrg_folder,"peak_forwards.csv",sep="/"),"PEAK")
  ext_peak_forwards<-proc_forwards(paste(nrg_folder,"ext_peak_forwards.csv",sep="/"),"EXT_PEAK")
  off_peaks<-proc_forwards(paste(nrg_folder,"off_peak_forwards.csv",sep="/"),"OFF_PEAK")
  #rro<-proc_forwards(paste(nrg_folder,"rro_forwards.csv",sep="/"),"RRO")
  
  #stack new ones
  new_forwards<-rbind(forwards,peak_forwards,ext_peak_forwards,off_peaks)
  new_forwards<-arrange(new_forwards,Trade_Date,Inst_Date,Type)
  new_forwards
  #save(all_forwards,file="forwards.RData")
}
nrg_folder<-"C:/Users/aleach/Google Drive/NRGStream"
test<-new_forwards()
df1 <- test %>% group_by(Trade_Date,Inst_Date,Inst_Year,Type) %>% summarise(Settle = mean(Settle))

today_date<-max(test$Trade_Date)
#today_date<-max(test$Trade_Date)

df1$Inst_Year<-factor(df1$Inst_Year,levels = sort(unique(df1$Inst_Year)))


dates<-c(today_date,today_date-years(1),today_date-years(2),today_date-years(3),today_date-years(4))
#dates<-c(today_date,today_date-years(1))
#dates<-c(today_date)


text_date<-function(date_sent=ymd("2020-01-01")){
  format(date_sent,"%B %d,%Y")}


format(dates,"%B %d,%Y")

select_fwds<-df1 %>% filter(Trade_Date %in% dates)%>%
  filter(Inst_Date<=Trade_Date+years(3),Type=="FLAT")%>%
  mutate(Trade_Date=format(Trade_Date,"%b %d, %Y"))%>%
mutate(Trade_Date=factor(as.character(Trade_Date)))
        


  


aeso_fwds<-ggplot(select_fwds) +
  #geom_line(data=subset(df1,Trade_Date %in% dates & Type=="OFF_PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=1.25)+
  geom_line(aes(Inst_Date,Settle,colour=Trade_Date,group=Trade_Date,linetype=Type),size=1.25)+
  #geom_line(data=subset(df1,Trade_Date %in% dates & Type=="PEAK"),aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype=Type),size=1.25)+
  #geom_line(aes(Inst_Year,Peak_Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date),linetype="dashed"),size=2)+
  scale_y_continuous(breaks=pretty_breaks())+
  expand_limits(x=Sys.Date()+months(38),y=200)+
  scale_color_manual("Trade Date",values=colors_ua10(),guide=guide_legend(nrow=1,order=1))+
  scale_linetype("Contract",guide=NULL)+ # Change linetypes
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.key.width=unit(3,"line"),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves (Calendar Strip)",
             caption="Source: Data via NRGStream")
aeso_fwds

ggsave(file=paste("images/forwards.png",sep=""),width = 14,height=7,dpi=300,bg="white")
