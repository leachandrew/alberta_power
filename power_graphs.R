#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")

#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())
source("../andrew_base.R")



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





load("gen.RData") ## which is here *equivalent* to

errors<-gen_data[is.na(gen_data$Price),] 
gen_errors<-gen_data[is.na(gen_data$gen),] 

gen_data<-gen_data[!is.na(gen_data$gen),] 
gen_data<-gen_data[!is.na(gen_data$Time),] 
#here, we have two sets of trade data - the AB-BC and AB-MON data and the grouped AB-WECC data

#take out AB-BC and AB-MON
#gen_data<-filter(gen_data, AESO_Name !="AB - BC Hydro  Imp/Exp Hr Avg MW",AESO_Name !="AB - Montana Imp/Exp Hr Avg MW")



sub_samp<-subset(gen_data, Time > as.Date("2010-01-1"))
#sub_samp<-subset(sub_samp, Time < as.Date("2017-12-31"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
sub_samp<-na.omit(sub_samp)
sub_samp$Year<-year(sub_samp$Time)
df1 <- sub_samp %>% group_by(Plant_Type,Time,Year) %>% summarise(sumcap = sum(Capacity),total_gen=sum(gen),p_mean=mean(Price))
df1$Year_ID=as.character(df1$Year)
# Histogram of Generation Densities
set_png(file="wind_cdf.png")
ggplot(df1,aes(total_gen))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_x_continuous(limits=range(df1$total_gen),expand=c(0,0),breaks = pretty_breaks())+
  scale_y_continuous(expand=c(0,0),labels = scales::percent)+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Wind Generation (MW)",y="% of hours generation < X MW",
       title="Cumulative Density Function, Wind Energy (2010-2017 Avg)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()




trade_set<-c("AB - BC Hydro Imp Hr Avg MW", "AB - Montana Imp Hr Avg MW",   "AB - Saskpower Imp AB Hr Avg MW",
             "AB - BC Hydro Exp Hr Avg MW", "AB - Montana Exp Hr Avg MW",   "AB - Saskpower Exp AB Hr Avg MW")

#exclude WECCs so that you don't double-count
trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")


sub_samp<-filter(gen_data, Time >= as.Date("2010-01-1"))
#take out the double-counted trade series (WECC)

df_test<-filter(sub_samp,is.na(gen))

df1 <- sub_samp %>% filter(! NRG_Stream %in% trade_excl)%>% group_by(Plant_Type,Time) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),p_mean=mean(Price)) %>% ungroup()
df1$Day <- date(df1$Time)
df1$Year <- as.factor(year(df1$Time))
#df1<-na.omit(df1)
#df1$Revenue <- df1$total_gen*df1$p_mean

gen_set<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND")


#test_samp<-gen_data %>% filter(year(Time)==2019)
#test_samp2<-test_samp %>% filter(Plant_Type %in% gen_set,! NRG_Stream %in% trade_excl)


df2 <- df1 %>% filter(Plant_Type %in% gen_set) %>%
       group_by(Plant_Type,Year) %>% summarise(capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean))

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





set_png(file="price_capture_avg.png", width = 1400, height = 750)
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

set_png(file="price_capture_pct.png", width = 1400, height = 750)
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


my_palette<-c(colors_tableau10()[8],colors_tableau10_medium()[4],colors_tableau10()[4],colors_tableau10_light()[4],colors_tableau10()[7],colors_tableau10()[1],colors_tableau10()[3],colors_tableau10()[2],colors_tableau10()[9],colors_tableau10_light()[9])
set_png(file="price_capture.png", width = 1400, height = 750)
plot_a<-ggplot(df2,aes(Year,capture,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture,colour=Plant_Type,fill=Plant_Type),size=1,position = position_dodge(width = .9),width = .6)+
  #  scale_color_viridis("Plant Type",discrete=TRUE)+
#  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
    slide_theme()+
labs(x="Year",y="Average Revenue ($/MWh)",
     title="Energy Price Capture ($/MWh, 2007-2019)",
     caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach")
plot_a
dev.off()

df2 <-df2 %>% filter(!Plant_Type %in% c("OTHER", "TRADE","IMPORT","EXPORT"))
#set_png(file="price_capture_tax.png", width = 1400, height = 750)

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




set_png(file="price_capture_ctax.png",width=1200,height = 850)
plot_b
dev.off()


set_png(file="oba_value.png",width=1200,height = 850)
plot_c
dev.off()

set_png(file="cost_ctax.png",width=1200,height = 850)
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

set_png(file="price_diff_ctax.png",width=1600,height = 1200)
(plot_a + theme(legend.position = "none"))/(plot_b+guides(fill=guide_legend(nrow=1,byrow=TRUE),colour=guide_legend(nrow=1,byrow=TRUE)))
dev.off()



week_index <- function(x)format(x, '%Y.%W')

month_index <- function(x)format(x, '%b-%Y')



sub_samp<-subset(gen_data, Time > as.Date("2007-01-1"))
#sub_samp<-subset(sub_samp, Time < as.Date("2017-12-31"))
sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
sub_samp<-na.omit(sub_samp)
sub_samp$Year<-year(sub_samp$Time)
df1 <- sub_samp %>% filter(! NRG_Stream %in% trade_excl)%>% group_by(Plant_Type,Time) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen,na.rm = T),total_rev=sum(Revenue,na.rm = T),p_mean=mean(Price)) %>% ungroup()
df1$Day <- date(df1$Time)
df1$Year <- as.factor(year(df1$Time))


df2 <- df1 %>% mutate(month=month(Time))%>% filter(Plant_Type %in% c("NGCC","SCGT","COGEN","COAL")) %>% 
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

set_png(file="gen_ghg_price.png", width = 2000, height = 1000)
ggdraw(p_direct_labels)
dev.off()

set_png(file="gen_plain.png", width = 1600, height = 1000)
ggdraw(plain_direct_labels)
dev.off()

set_png(file="gen_old.png", width = 1600, height = 1000)
ggdraw(p_direct_labels)
dev.off()




df2 <- df1 %>% group_by(Day,Year) %>% summarise(capture = sum(total_rev)/sum(total_gen))
df2<-subset(df2, Year==2019 |Year==2018 | Year==2017 | Year==2014)
set_png(file="price_capture_all.png", width = 1400, height = 750)
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





df2 <- df1 %>% filter(Time>=max(Time)-days(21)-years(1) & Time<=max(Time)-days(7)) %>%
  mutate(weekday=wday(Time, label=TRUE)) %>% filter(Plant_Type %in% gen_set,Plant_Type != "EXPORT",weekday!="Sun",weekday!="Sat") %>%
  mutate(day=day(Time),month=month.abb[month(Time)],hour=hour(Time)) %>%   
  group_by(Year,month,hour,Plant_Type) %>% summarise(gen=mean(total_gen),capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean),day=mean(day)) %>% 
  mutate(date=ymd(paste(Year,month,round(day),sep="-")))


#write.xlsx(df2, file = "cansia.xlsx", colNames = TRUE, borders = "columns")

#emissions now built into the gen data by default


vol_test<-gen_data %>% filter(Plant_Type %in% gen_set) %>% mutate(month=month(Time),year=year(Time)) %>%
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
  set_png(file="monthly_ghgs.png")
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
  set_png(file="monthly_ghg_mwh.png")
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








sub_samp<-subset(gen_data, Time >= as.Date("2004-01-1"))
sub_samp$Month_Year <- decimal_date(sub_samp$Time)
sub_samp$Year <- year(sub_samp$Time)
sub_samp$Month <- month(sub_samp$Time)
sub_samp$Hour <- hour(sub_samp$Time)
#distribution of hourly prices - duration curves and density functions


df1 <- sub_samp %>% group_by(Time,Year) %>% summarise(meancap = mean(Cap_Fac),total_gen=sum(gen),total_rev=sum(Revenue),p_mean=mean(Price))
df1$Year_ID=as.character(df1$Year)
#df1$Revenue <- df1$total_gen*df1$p_mean



set_png(file="price_dist.png")
ggplot(data=subset(df1,Year %in% c(2008,2013,2016,2017,2018,2019)),aes(total_rev/total_gen))+
  #ggplot(df1,aes(p_mean))+
  #geom_density(aes(group=Year_ID,colour=Year_ID),size=1.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  guides(fill = guide_legend(title = "LEFT", title.position = "left"))+
  labs(colour="Year") +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+
  scale_y_reverse(labels = scales::percent,expand=c(0,0))+
  scale_x_continuous(limits=c(0,1000),expand=c(0,0))+
  scale_color_viridis(discrete=TRUE)+
  ajl_line()+
  labs(x="Hourly Power Prices ($/MWh)",y="Percentage of Time Price < Y",
       title="Duration Curve of Hourly Energy Prices ($/MWh)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()

set_png(file="price_dist_recent.png")
ggplot(data=subset(df1,Year %in% c(2016,2017,2018,2019)),aes(total_rev/total_gen))+
  #ggplot(df1,aes(p_mean))+
  #geom_density(aes(group=Year_ID,colour=Year_ID),size=1.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  guides(fill = guide_legend(title = "LEFT", title.position = "left"))+
  labs(colour="Year") +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+
  scale_y_reverse(labels = scales::percent,expand=c(0,0))+
  scale_x_continuous(limits=c(0,50),expand=c(0,0))+
  scale_color_viridis(discrete=TRUE)+
  ajl_line()+
  labs(x="Hourly Power Prices ($/MWh)",y="Percentage of Time Price < Y",
       title="Duration Curve of Hourly Energy Prices ($/MWh)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()



sub_samp<-subset(gen_data, Time >= as.Date("2004-01-1"))
#sub_samp<-subset(sub_samp, Time < as.Date("2017-7-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND" | sub_samp$`Plant_Fuel`=="GAS" |sub_samp$`Plant_Fuel`=="COAL")
#sub_samp<-na.omit(sub_samp)
#sub_samp<-sub_samp[-is.na(sub_samp)] 
#sub_samp<- sub_samp %>%  mutate_all(funs(ifelse(is.na(.), 0, .)))

sub_samp$Month_Year <- decimal_date(sub_samp$Time)
sub_samp$Year <- year(sub_samp$Time)
sub_samp$Month <- month(sub_samp$Time)
sub_samp$Hour <- hour(sub_samp$Time)
sub_samp$Date_ID = as.yearmon(sub_samp$Time)



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

  sub_samp<-subset(gen_data, as.Date(Time) >= as.Date("2004-01-1"))
  sub_samp$month<-month(sub_samp$Time)
  sub_samp$Year<-as.character(year(sub_samp$Time))
  sub_samp$Hour<-as.factor(hour(sub_samp$Time)+1)
  
  df1 <- sub_samp %>% group_by(Hour,Year) %>% summarise(AIL=mean(AIL, na.rm = TRUE),meancap = mean(Cap_Fac, na.rm = TRUE),total_gen=sum(gen, na.rm = TRUE),total_rev=sum(Revenue, na.rm = TRUE),p_mean=mean(Price, na.rm = TRUE))

  set_png(file="hourly-prices.png", width = 1200, height = 800)
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
  
  set_png(file="hourly-prices-recent.png", width = 1200, height = 800)
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
  set_png(file="hourly-loads.png", width = 1200, height = 800)
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
  set_png(file="monthly-peak-loads.png")
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
  
  set_png(file="monthly-avg-loads.png")
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

df1 <-gen_data %>% filter(ID %in% "WHT1") %>%
  mutate(month=month(Time),year=year(Time))%>%
  group_by(month,year)%>% 
  summarize(price=mean(Price),total_gen=sum(gen,na.rm = T),rev=sum(gen*Price,na.rm = T),
          cfd=sum(gen*strike_price,na.rm = T),avg_rev=rev/total_gen)%>% filter(total_gen>0)%>%
  mutate(mid_mth=ymd(paste(year,month,15,sep="-")))%>%
  select(date=mid_mth,gen=total_gen,rev,cfd,avg_rev)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("wht_cfd_revs_equiv.png")
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





  df_mulligan <-gen_data %>% filter(ID =="MUL1",Time>ymd("2019-02-25"),Time<ymd("2019-03-09")) %>% assign_date_time_days(time_var="Time") %>%
    assign_peaks(time_var="Time")%>%
    mutate(peak_gen=ifelse(on_peak,gen,NA)
    )
    
  set_png(file="mulligan.png")
  p<-ggplot(df_mulligan) +
    geom_line(aes(Time,gen,colour="basic"),size=1.5)+
    geom_line(aes(Time,peak_gen,colour="peak"),size=1.5)+
    #geom_col(aes(time,actual_posted_pool_price,fill=on_peak,colour=on_peak),size=.8)+
    scale_color_manual("",values = colors_tableau10(),labels=c("Off-peak period","Peak period"))+
    scale_fill_manual("",values = colors_tableau10(),labels=c("Off-peak period","Peak period"))+
    scale_x_datetime(expand=c(0,0),breaks="1 day",labels = date_format("%H:00\n%a\n%b %d\n%Y",tz="America/Denver"))+
    scale_y_continuous(limits = c(0,5)) +
    theme_minimal()+
    theme(legend.position="bottom",
          legend.margin=margin(c(0,0,0,0),unit="cm"),
          legend.text = element_text(colour="black", size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt"))
    )+
    labs(y="Hourly Generation (MWh)",x="",
         title=paste("Mulligan 1 Cogeneration Plant",sep=""),
         subtitle = ("Welcome to the Grid, MUL1")
         #subtitle=paste(month.name[month(period_start)]," ",day(period_start),", ",year(period_start)," to ",month.name[month(end_date)]," ",day(end_date), ", ",year(end_date),sep="")
    )
  print(p)
   dev.off()


  
  ggplot(data =df1, aes(yearmon,newvar/1000))+ 
    geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
    #facet_grid(. ~ Run_ID)+
    scale_fill_viridis("Generation Type",discrete = TRUE,option="viridis")+
    #scale_x_continuous(breaks=seq(1, 24, 1))+ # Ticks from 0-1200, every 200+
    #scale_fill_brewer(palette="Spectral") +
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(size = 18,face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 12,face = "bold", colour="black")
    )+
    labs(x="Hour",y="Forecast Annual Generation (GWh)",
         title="Forecast Annual Renewable Electricity Production",
         subtitle="Base Case and 30% by '30 Renewable Generation Constraint, 2016-2035",
         caption="Source: University of Alberta School of Business AuroraXMP implementation\nGraph by Andrew Leach")
  
    
  
  
  
  

sub_samp<-subset(gen_data, Time > as.Date("2017-06-28"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(gen = sum(gen,na.rm = T))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("TRADE","WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))


png(file="gen_month.png", width = 1400, height = 750)
p <- ggplot(df1, aes(Time,gen)) 
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


sub_samp<-subset(gen_data, Time > as.Date("2017-07-21"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

set_png(file="gen_week.png", width = 1400, height = 750)
ggplot(df1, aes(Time,newvar))+
geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
  geom_line(aes(Time,ail,colour="Alberta Internal Load"),size=1.5) +
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


sub_samp<-subset(gen_data, Time > as.Date("2018-07-21"))
sub_samp<-subset(sub_samp, Time < as.Date("2018-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

set_png(file="gen_week_2018.png", width = 1400, height = 750)
ggplot(df1, aes(Time,newvar))+
  geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
  geom_line(aes(Time,ail,colour="Alberta Internal Load"),size=1.5) +
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

sub_samp<-subset(gen_data, Time > as.Date("2014-07-21"))
sub_samp<-subset(sub_samp, Time < as.Date("2014-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))
df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))



set_png(file="gen_week_2014.png", width = 1400, height = 750)
ggplot(df1, aes(Time,newvar))+
  geom_area(aes(fill=Plant_Type2), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
  geom_line(aes(Time,ail,colour="Alberta Internal Load"),size=1.5) +
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




sub_samp<-subset(gen_data, Time > as.Date("2017-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-07-28"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)

df1 <- sub_samp %>% group_by(Time) %>% summarise(newvar = sum(gen),ail=mean(AIL),price=mean(Price))

#df1$Plant_Type2 <- factor(df1$Plant_Type, levels=c("WIND","SCGT","NGCC","HYDRO","OTHER","COAL","COGEN"))

png(file="load_ytd.png", width = 1400, height = 750)
p <- ggplot(df1, aes(Time,ail))
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


sub_samp<-subset(gen_data, Time > as.Date("2013-10-01"))
sub_samp<-subset(sub_samp, Time < as.Date("2018-10-01"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-na.omit(sub_samp)
sub_samp$Date<-date(sub_samp$Time)


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







sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-06-19"))
sub_samp<-subset(sub_samp, Plant_Fuel=="GAS")
sub_samp<-subset(sub_samp, ID=="NX02")


sub_samp$Date_ID = as.yearmon(sub_samp$Time)
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
       title="Time Series of Monthly Total Generation (MWh, 2014-2017)",
       subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")



sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
sub_samp<-subset(sub_samp, Time < as.Date("2017-06-19"))
sub_samp<-subset(sub_samp, Plant_Fuel=="WIND")
sub_samp<-subset(sub_samp, ID=="HAL1")


sub_samp$Date_ID <- as.yearmon(sub_samp$Time)
sub_samp$Week_ID <- factor(str_c(week(sub_samp$Time),year(sub_samp$Time), sep = "_", collapse = NULL))
sub_samp$Week <- week(sub_samp$Time)


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
df1 <- df1 %>% filter(Inst_Date<=min(dates)+)

png<-1
if(png==1)
  set_png(file="daily_forwards.png")

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
forwards <- clean_forwards(read.xlsx(xlsxFile = "NGX_forwards.xlsx", sheet = "forwards_flat", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE),type="FLAT")
#forwards <- read.xlsx(xlsxFile = "NGX_forwards.xlsx", sheet = "forwards_flat", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
peak_forwards <- clean_forwards(read.xlsx(xlsxFile = "NGX_forwards.xlsx", sheet = "forwards_peak", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE),type="PEAK")
ext_peak_forwards <- clean_forwards(read.xlsx(xlsxFile = "NGX_forwards.xlsx", sheet = "forwards_ext_peak", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE),type="EXT_PEAK")
off_peak_forwards <- clean_forwards(read.xlsx(xlsxFile = "NGX_forwards.xlsx", sheet = "forwards_off_peak", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE),type="OFF_PEAK")

#all_forwards<-arrange(rbind(forwards,peak_forwards,ext_peak_forwards,off_peak_forwards),c("Trade_Date","Inst_Date","Type"))
all_forwards<-rbind(forwards,peak_forwards,ext_peak_forwards,off_peak_forwards)
all_forwards<-all_forwards%>%filter(Trade_Date<as.Date("2018-01-01"))
save(all_forwards,file="forwards.RData")
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
  forwards<-proc_forwards("forwards2018.csv","FLAT")
  peak_forwards<-proc_forwards("peak_forwards.csv","PEAK")
  ext_peak_forwards<-proc_forwards("ext_peak_forwards.csv","EXT_PEAK")
  off_peaks<-proc_forwards("off_peak_forwards.csv","OFF_PEAK")
  #stack new ones
  new_forwards<-rbind(clipped_forwards,forwards,peak_forwards,ext_peak_forwards,off_peaks)
  new_forwards<-arrange(new_forwards,Trade_Date,Inst_Date,Type)
  
}

load("forwards.Rdata")
all_forwards<-update_forwards(all_forwards)
save(all_forwards,file="forwards.RData")

load("forwards.Rdata")




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
  set_png(file="forwards.png")

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
  set_png(file="forwards_all.png")
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
  set_png(file="forwards_monthly.png")
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
  set_png(file="forwards_kenney.png")
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
  set_png(file="forwards_kenney_tax.png")
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
  set_png(file="forwards_summer2017.png")
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
  set_png(file="forwards_2017.png")

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
  set_png(file="forwards_long.png",height=1000)
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


#FORECASTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#forecast_data$time<-as.POSIXct(paste(forecast_data$date," ",as.numeric(forecast_data$he),":00",sep=""),format="%Y-%m-%d %H:%M")
filename<-paste("forecast_data",".RData",sep = "")
load(filename)
update_forecasts()
save(forecast_data, file= filename) 
#hourly prices  
forecast_data$day_ahead_forecasted_ail<-gsub(",","",forecast_data$day_ahead_forecasted_ail)
forecast_data$actual_ail<-gsub(",","",forecast_data$actual_ail)
forecast_data$forecasted_actual_ail_difference<-gsub(",","",forecast_data$forecasted_actual_ail_difference)
forecast_data$Year<-year(forecast_data$date)
forecast_data$actual_posted_pool_price<-as.numeric(forecast_data$actual_posted_pool_price)
forecast_data$forecast_pool_price<-as.numeric(forecast_data$forecast_pool_price)
forecast_data$actual_ail<-as.numeric(forecast_data$actual_ail)
forecast_data$day_ahead_forecasted_ail<-as.numeric(forecast_data$day_ahead_forecasted_ail)


forecast_data_merge<-forecast_data
forecast_data_merge$Inst_Year<-year(forecast_data_merge$date)
forecast_data_merge$Inst_Month<-month(forecast_data_merge$date)

forecast_data_merge<- na.omit(forecast_data_merge) %>% group_by(Inst_Year,Inst_Month) %>% summarise(pool_price = sum(actual_posted_pool_price*actual_ail)/sum(actual_ail))
df2<-merge(forecast_data_merge,df1,by=c("Inst_Year","Inst_Month"),all.y=TRUE)
df2<-arrange(df2,Trade_Date,Inst_Year,Inst_Month)
df2$Inst_Date<-as.Date(paste(df2$Inst_Year,"-",df2$Inst_Month,"-",days_in_month(df2$Inst_Month),sep=""),format="%Y-%m-%d")
today_date<-max(df2$Trade_Date)
dates<-c(today_date,today_date-months(3))

png<-1
if(png==1)
  png(file="forwards_pool.png", width = 1400, height = 750,res=130,type='cairo')

ggplot(subset(df2,Trade_Date %in% dates & Inst_Date<=as.Date("2020-12-31"))) +
  geom_line(aes(Inst_Date,Settle,colour=as.factor(Trade_Date),group=as.factor(Trade_Date)),size=2)+
  geom_line(data=subset(df2,Trade_Date==today_date-years(1)& Inst_Date<=as.Date("2020-12-31")),aes(Inst_Date,pool_price,colour="Actual Pool Price"),size=2)+
  scale_color_brewer("",palette = "Set1",labels=c(paste("Forward Curve\n",rev(dates),sep = ""),"Actual Monthly Average\nPool Prices"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Settlement Price ($/MWh)",x="\nInstrument Date",
             title="Alberta Power Forward Curves",
             caption="Source: Data via NRGStream\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





  aurora_res <- read.xlsx(xlsxFile = "2014_2017_gen.xlsx", sheet = 6, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  wind_plants<-subset(aurora_res, Fuel== "WND" & Capacity>0)
  
  #hdf<-get_map(location = "alberta", zoom = 6)
  
  #hdf<-get_map(center = c(lon = -116, lat = 48), zoom = 6,maptype = "terrain")
  hdf<-get_googlemap(center = c(lon = -113, lat = 50.9), zoom = 7,size = c(600, 500),)
                
  #ggmap(hdf, extent = "normal")
  
  library(ggrepel)
  wind_plants<-wind_plants[which(is.na(wind_plants$Latitude)==FALSE),]
  wind_plants$Latitude <- as.numeric(as.character(wind_plants$Latitude))
  wind_plants$Longitude <- as.numeric(as.character(wind_plants$Longitude))
  wind_plants$Capacity <- as.numeric(as.character(wind_plants$Capacity))
  #Mac
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="wind_plants_new.png", width = 1400, height = 750,res=130)
  #PC
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file="wind_plants_new.png", width = 1400, height = 750,res=130,type='cairo')
  ggmap(hdf, extent = "normal")+  theme_bw() +
    geom_point(aes(x = Longitude, y = Latitude, size=Capacity),
               data = wind_plants, alpha = .5, color="darkblue")+
    scale_size(range=c(3,12),name="Capacity (MW)")+
    #geom_label_repel(data = wind_plants,
    #                 aes(x = Longitude, y = Latitude, size=4, label = ID.1),show.legend = FALSE) +
    
      theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 20, face = "bold"),
      plot.caption = element_text(size = 20, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 20, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 20,face = "bold"),
      axis.text = element_text(size = 20,face = "bold", colour="black"),
      axis.title.y=element_text(vjust=0)
    )+
    labs(x="Longitude",y="Latitude",
         title="Alberta Wind Power Production Facilities, June 2017",
         subtitle="Source: Various, Map by Andrew Leach")  
  dev.off()

  #I DIDN'T GET THE NEXT PART WORKING
      
  solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  res_data <- read.xlsx(xlsxFile = "YEG_Res_Load.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  sub_samp <- solar_data
  names(sub_samp) <- tolower(names(sub_samp))
  sub_samp$`beam.irradiance.(w/m^2)`<- NULL
  sub_samp$`diffuse.irradiance.(w/m^2)`<- NULL
  sub_samp$`wind.speed.(m/s)`<- NULL
  sub_samp$`plane.of.array.irradiance.(w/m^2)`<- NULL
  sub_samp$`cell.temperature.(c)`<- NULL
  names(sub_samp) <- c("month","day","hour","temperature","dc.output","ac.output")
  sub_samp$hour <- as.numeric(sub_samp$hour)
  sub_samp$hour <- as.factor(sub_samp$hour)
  sub_samp$month <- as.numeric(sub_samp$month)
  sub_samp$month <- as.factor(sub_samp$month)
  sub_samp<- arrange(sub_samp,month,day,hour)
  
  
  df1 <- sub_samp %>% group_by(month,hour) %>% summarise(hourly_gen=sum(ac.output)/1000)
  png(file="solar_gen.png", width = 1400, height = 750)
  ggplot(df1, aes(hour,hourly_gen,group=month,colour=month)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    labs(colour="Month") +
    scale_color_viridis(discrete=TRUE)+  
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
  )+    labs(y="Hourly Solar System Generation (Monthly total, kWh)",x="Hour of the Day",
             title="Time Series of Hourly Solar Generation (Monthly Total kWh, estimated)",
             subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()
  
  
  df1 <-sub_samp %>% group_by(month) %>% summarise(monthly_gen=sum(ac.output)/1000)

  png(file="solar_monthly_gen.png", width = 1400, height = 750)
  ggplot(df1,aes(month,monthly_gen,group = 1)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    scale_color_viridis(discrete=TRUE)+ 
    expand_limits(y = 0)+
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
    )+    labs(y="Monthly Solar System Generation (total, kWh)",x="Month",
               title="Time Series of Monthly Solar Generation (Total kWh, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()

  
  
  #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
  sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
  sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
  sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
  sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
  sub_samp<-na.omit(sub_samp)
  sub_samp$month<-month(sub_samp$Time,label=TRUE)
  df1 <- sub_samp %>% group_by(Time,month,Plant_Fuel) %>% summarise(total_gen=sum(gen),test=3,avg_rev=3)
  df1$Date_ID<-df1$month
  df1<-arrange(df1,Date_ID)

  

  #png(file="wind_montly_gen.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
  ggplot(df1,aes(x =total_gen,y=Date_ID,fill=as.factor(test),height=..density..))+
    geom_joy(rel_min_height = 0.01)+
    scale_fill_viridis(discrete = TRUE) +   
    theme_minimal()+theme(
      legend.position = "none",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black"))+    
      labs(y="Density by Month",x="Hourly Power Generation (MW)",
               title="Distribution of Hourly Wind Power Generation, Alberta, 2014-2017",
               subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
   # dev.off()
    
    ggplot(df1,aes(x =total_gen,y=Date_ID))+
      geom_joy(fill=as.factor(month),height=..density..,rel_min_height = 0.01)+
      scale_colour_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black"))+ 
      labs(title="Alberta Electricity Demand",
           subtitle="Hourly load distribution, by year",
           caption="Source: AESO\nChart by Andrew Leach (@andrew_leach)\nR package (ggjoy) by Claus Wilke",
           x="Megawatts",
           y="")+
      theme(legend.position = "none")
    
    
    
    
    
    #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
    sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
    sub_samp<-subset(sub_samp, sub_samp$`Plant_Type`=="COGEN")
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-month(sub_samp$Time,label=TRUE)
    df1 <- sub_samp %>% group_by(Time,month) %>% summarise(total_gen=sum(gen))
    df1$Date_ID<-df1$month
    df1<-arrange(df1,-Date_ID)
    mins<-min(df1$total_gen)
    maxs<-max(df1$total_gen)
    
    png(file="cogen_montly_gen.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
    ggplot(df1,aes(x =total_gen,y=Date_ID,height=..density..,colour=3))+
      #theme_joy() +
      geom_joy(scale=1,fill=3)+
      scale_x_continuous(limits = c(mins,maxs))+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Density by Month",x="Hourly Power Generation (MW)",
                 title="Distribution of Hourly Generation from Cogeneration Units, Alberta, 2014-2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    dev.off()    
    
    #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
    sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
    sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL")
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-month(sub_samp$Time,label=TRUE)
    df1 <- sub_samp %>% group_by(Time,month) %>% summarise(total_gen=sum(gen))
    df1$Date_ID<-df1$month
    df1<-arrange(df1,-Date_ID)
    mins<-min(df1$total_gen)
    maxs<-max(df1$total_gen)
    
    png(file="coal_montly_gen.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
    ggplot(df1,aes(x =total_gen,y=Date_ID,height=..density..,colour=3))+
      #theme_joy() +
      geom_joy(scale=1,fill=3)+
      scale_x_continuous(limits = c(mins,maxs))+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Density by Month",x="Hourly Power Generation (MW)",
                 title="Distribution of Hourly Generation from Coal Units, Alberta, 2014-2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    dev.off() 
    

    #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
    sub_samp<-subset(gen_data, Time > as.Date("2016-01-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2016-12-31"))
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-as.factor(month(sub_samp$Time,label=TRUE))
    #sub_samp<-subset(sub_samp, sub_samp$month=="Jan" | sub_samp$month=="May")
    sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL"|sub_samp$`Plant_Fuel`=="GAS"|sub_samp$`Plant_Fuel`=="HYDRO"| sub_samp$`Plant_Fuel`=="WIND")
    
    
    df1 <- sub_samp %>% group_by(Time,month,Plant_Fuel) %>% summarise(total_gen=sum(gen),total_rev=sum(gen*Price),avg_rev=sum(gen*Price))
    df1$Date_ID<-df1$month

      
    
    png(file="all_montly_test.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
    ggplot(df1,aes(y =total_gen,x=Date_ID))+
      #theme_joy() +
      #geom_joy(rel_min_height = 0.01,aes(colour=Plant_Type,fill=Plant_Type))+
      geom_boxplot(aes(color=Plant_Fuel),fatten = 1.25,lwd=1.45)+coord_flip()+
      geom_text(data = p_meds, aes(x = TYPE, y = med, label = med), 
                size = 3, vjust = -1.5)
      #scale_x_continuous(limits = c(0,6000))+
      scale_colour_viridis(discrete = TRUE,option="viridis")+
      #scale_colour_viridis(discrete = TRUE,option="plasma")+
      #scale_colour_brewer(palette="Set1")+
      labs(colour = "Plant Fuel\nSource") +
      theme_minimal()+theme(
        legend.position = "right",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 16, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Hourly Power Generation (MW)",x="Month",
                 title="Distribution of Hourly Generation from All Units, Alberta, 2016",
                 caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
    
 
   
   #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
   sub_samp<-subset(gen_data, Time > as.Date("2016-01-1"))
   sub_samp<-subset(sub_samp, Time < as.Date("2016-12-31"))
   sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
   sub_samp<-na.omit(sub_samp)
   sub_samp$month<-as.factor(month(sub_samp$Time,label=TRUE))
   
   
   #sub_samp<-subset(sub_samp, sub_samp$month=="Jan" | sub_samp$month=="May")
   sub_samp$renew <- ifelse(sub_samp$Plant_Fuel =="WIND" |sub_samp$Plant_Fuel =="HYDRO"|sub_samp$Plant_Fuel =="OTHER" ,"Renewable", "Fossil Fuel")
   sub_samp$AIL <- as.numeric(sub_samp$AIL)
   
   df1 <- sub_samp %>% group_by(Time,month,renew,AIL) %>% summarise(total_gen=sum(gen),total_rev=sum(gen*Price),share=sum(gen)/mean(AIL)*100)
   df1$Date_ID<-factor(df1$month,levels=rev(levels(df1$month)))
   
   
   
   png(file="renew_montly_test.png", width = 1400, height = 750)
   #ggplot(df1,aes(x =total_gen,y=Date_ID))+
   ggplot(df1,aes(y=share,x=Date_ID))+
     #theme_joy() +
     #geom_joy(rel_min_height = 0.01,aes(colour=Plant_Type,fill=Plant_Type))+
     geom_boxplot(aes(color=renew),fatten = 1.25,lwd=1.45)+coord_flip()+
     scale_colour_viridis(discrete = TRUE,option="viridis")+
     #scale_colour_viridis(discrete = TRUE,option="plasma")+
     #scale_colour_brewer(palette="Set1")+
     labs(colour = "Plant Fuel\nSource") +
     scale_y_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     #scale_x_discrete(breaks=seq(0, 100, 20))+ # Ticks from 0-1200, every 200
     theme_minimal()+theme(
       legend.position = "right",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 16, face = "bold"),
       plot.caption = element_text(size = 16, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 16, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 16,face = "bold"),
       axis.text = element_text(size = 16,face = "bold", colour="black")
     )+    labs(y="Share of Hourly Power Generation (%)",x="Month",
                title="Distribution of Hourly Generation from Renewable vs Fossil Fuel Units, Alberta, 2016",
                caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
   
   png(file="renew_montly_test.png", width = 1400, height = 750)
   #ggplot(df1,aes(x =total_gen,y=Date_ID))+
   ggplot(df1,aes(x =total_gen,y=Date_ID,height=..density..))+
    #ggplot(df1,aes(y=share,x=Date_ID))+
     #theme_joy() +
     geom_joy(rel_min_height = 0.001,aes(fill=as.factor(renew)))+
     #scale_color_viridis()+
     #geom_boxplot(aes(color=renew),fatten = 1.25,lwd=1.45)+coord_flip()+
     scale_colour_viridis(discrete = TRUE,option="viridis")+
     scale_fill_viridis("Plant Fuel Source",discrete = TRUE,option="viridis")+
     #scale_colour_viridis(discrete = TRUE,option="plasma")+
     #scale_colour_brewer(palette="Set1")+
     labs(colour = "Plant Fuel\nSource") +
     #scale_y_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     #scale_x_discrete(breaks=seq(0, 100, 20))+ # Ticks from 0-1200, every 200
     theme_minimal()+theme(
       legend.position = "right",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 16, face = "bold"),
       plot.caption = element_text(size = 16, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 16, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 16,face = "bold"),
       axis.text = element_text(size = 16,face = "bold", colour="black")
     )+    labs(y="Density of Hourly Power Generation",x="Total Hourly Generation (MW)",
                title="Distribution of Hourly Generation from Renewable vs Fossil Fuel Units, Alberta, 2016",
                caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
   
 
   png(file="renew_montly_joy.png", width = 1400, height = 750)
   #ggplot(df1,aes(x =total_gen,y=Date_ID))+
   ggplot(df1,aes(x =share,y=Date_ID,height=..density..))+
     #ggplot(df1,aes(y=share,x=Date_ID))+
     #theme_joy() +
     geom_joy(rel_min_height = 0.001,aes(fill=as.factor(renew)))+
     #scale_color_viridis()+
     #geom_boxplot(aes(color=renew),fatten = 1.25,lwd=1.45)+coord_flip()+
     scale_colour_viridis(discrete = TRUE,option="viridis")+
     #scale_fill_viridis("Plant Fuel Source",discrete = TRUE,option="viridis")+
     scale_fill_viridis("Plant Fuel Source",discrete = TRUE,option="plasma")+
     #scale_colour_viridis(discrete = TRUE,option="plasma")+
     #scale_colour_brewer(palette="Set1")+
     labs(colour = "Plant Fuel\nSource") +
     #scale_y_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     scale_x_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     theme_minimal()+theme(
       legend.position = "right",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 18, face = "bold"),
       plot.caption = element_text(size = 18, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 18, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 18,face = "bold"),
       axis.text = element_text(size = 18,face = "bold", colour="black")
     )+    labs(y="Density of Hourly Power Generation Shares",x="Share of Hourly Total Internal Load (%)",
                title="Distribution of Hourly Generation from Renewable vs Fossil Fuel Units, Alberta, 2016",
                caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
   
     
   
   #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
   sub_samp<-subset(gen_data, Time > as.Date("2014-01-1"))
   sub_samp<-subset(sub_samp, Time < as.Date("2016-12-31"))
   sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
   sub_samp<-na.omit(sub_samp)
   sub_samp$month<-as.factor(month(sub_samp$Time,label=TRUE))
   #sub_samp<-subset(sub_samp, sub_samp$month=="Jan" | sub_samp$month=="May")
   sub_samp$renew <- ifelse(sub_samp$Plant_Fuel =="WIND" |sub_samp$Plant_Fuel =="HYDRO"|sub_samp$Plant_Fuel =="OTHER" ,"Renewable", "Fossil Fuel")
   sub_samp$AIL <- as.numeric(sub_samp$AIL)
   
   df1 <- sub_samp %>% group_by(Time,Year,renew,AIL) %>% summarise(total_gen=sum(gen),total_rev=sum(gen*Price),share=sum(gen)/mean(AIL)*100)
   df1$Date_ID<-as.factor(df1$Year)
   
   
   
   png(file="renew_year_test.png", width = 1400, height = 750)
   #ggplot(df1,aes(x =total_gen,y=Date_ID))+
   ggplot(df1,aes(y=share,x=Date_ID))+
     #theme_joy() +
     #geom_joy(rel_min_height = 0.01,aes(colour=Plant_Type,fill=Plant_Type))+
     geom_boxplot(aes(color=renew),fatten = 1.25,lwd=1.45)+coord_flip()+
     scale_colour_viridis(discrete = TRUE,option="viridis")+
     #scale_colour_viridis(discrete = TRUE,option="plasma")+
     #scale_colour_brewer(palette="Set1")+
     labs(colour = "Plant Fuel\nSource") +
     scale_y_continuous(breaks=seq(0, 100, 10))+ # Ticks from 0-1200, every 200
     #scale_x_discrete(breaks=seq(0, 100, 20))+ # Ticks from 0-1200, every 200
     theme_minimal()+theme(
       legend.position = "right",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 16, face = "bold"),
       plot.caption = element_text(size = 16, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 16, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 16,face = "bold"),
       axis.text = element_text(size = 16,face = "bold", colour="black")
     )+    labs(y="Share of Hourly Power Generation (%)",x="Month",
                title="Distribution of Hourly Generation from Renewable vs Fossil Fuel Units, Alberta, 2016",
                caption="Source: AESO Data via NRGStream, Graph by @Andrew_Leach")
   dev.off()
   
   
   
   
      
    #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/
    sub_samp<-subset(gen_data, Time > as.Date("2016-01-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2017-06-15"))
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-month(sub_samp$Time,label=TRUE)
    sub_samp<- cbind(sub_samp,model.matrix( ~ Plant_Type, data = sub_samp))
    
    df1 <- sub_samp %>% group_by(Plant_Type,Time,month) %>% summarise(total_gen=sum(gen),avg_rev=sum(gen*Price)/sum(gen))
    df1$Date_ID<-df1$month

    
    
    
    
    #png(file="all_montly_gen.png", width = 1400, height = 750)
    #ggplot(df1,aes(x =total_gen,y=Date_ID))+
    ggplot(df1,aes(x =total_gen,y=Date_ID,fill=as.factor(month),height=..density..))+
      geom_joy(rel_min_height = 0.01)+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black"))+ 
      labs(title="Alberta Electricity Demand",
           subtitle="Hourly load distribution, by year",
           caption="Source: AESO\nChart by Andrew Leach (@andrew_leach)\nR package (ggjoy) by Claus Wilke",
           x="Megawatts",
           y="")+
      theme(legend.position = "none")
    
      
    
    ggplot(df1,aes(x =total_gen,y=Date_ID,height=..density..,colour=3))+
      theme_joy() +
      geom_joy(scale=1,fill=3)+
      scale_x_continuous(limits = c(mins,maxs))+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Density by Month",x="Hourly Power Generation (MW)",
                 title="Distribution of Hourly Generation from All Units, 2016-2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    #dev.off()
    
    mins<-0
    maxs<-60
    
    
    #png(file="all_montly_value.png", width = 1400, height = 750)
    ggplot(df1,aes(x =avg_rev,y=Date_ID,height=..density..,colour=3))+
      theme_joy() +
      geom_joy(scale=1,fill=3)+
      scale_x_continuous(limits = c(mins,maxs))+
      scale_color_viridis()+   
      theme_minimal()+theme(
        legend.position = "none",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16,face = "bold"),
        axis.text = element_text(size = 16,face = "bold", colour="black")
      )+    labs(y="Density by Month",x="Hourly Power Revenues ($/MWh)",
                 title="Distribution of Hourly Generation Values from all units, 2016-2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    #dev.off()
    
    
    
    
#duck-ish curves w wind
    #use March wind
    sub_samp<-subset(gen_data, Time > as.Date("2017-03-1"))
    sub_samp<-subset(sub_samp, Time < as.Date("2017-03-31"))
    sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
    sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
    sub_samp<-na.omit(sub_samp)
    sub_samp$month<-month(sub_samp$Time,label=TRUE)
    sub_samp$AIL<-as.numeric(sub_samp$AIL)
    df1 <- sub_samp %>% group_by(Time) %>% summarise(AIL=mean(AIL),AIL_net=mean(AIL)-sum(gen),AIL_2xnet=mean(AIL)-2*sum(gen),total_gen=sum(gen),avg_rev=(sum(gen*Price)/sum(gen))) 
    df1$avg_rev[df1$avg_rev==NA]<-0
    df1$Hour<-hour(df1$Time)
    df1$Date_ID<-day(df1$Time)
    df1<-arrange(df1,Time)
    
    mins<-min(0)
    maxs<-max(12000)
    

    myPalette <- colorRampPalette(rev(brewer.pal(3, "Set1")))
    sc <- scale_colour_gradientn(colours = myPalette(31), limits=c(1,31))
    myPalette <- colorRampPalette
    
    #Mac
    if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130)
    #PC
    if(R.version$platform ==  "x86_64-w64-mingw32")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130,type='cairo')
    ggplot(df1,aes(Hour,total_gen,group=Date_ID,colour = Date_ID)) +
      geom_line(size=1.5) +
      #sc+
      scale_colour_viridis(option="viridis")+
      scale_y_continuous(breaks=seq(0, 1400, 200))+ # Ticks from 0-1200, every 200
      scale_x_continuous(breaks=seq(0, 23, 2))+ # Ticks from 0-1200, every 200
      expand_limits(y=c(0,1400),x=c(1,24))+
      labs(colour = "Day") +
      theme_minimal()+theme(
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.position = "right",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 14,face = "bold", colour="black")
      )+    labs(y="Hourly wind generation (MW)",x="Hour",
                 title="Hourly Wind Generation Patterns, March 2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    dev.off()
    
    #Mac
    if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130)
    #PC
    if(R.version$platform ==  "x86_64-w64-mingw32")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130,type='cairo')
    ggplot(df1,aes(Hour,AIL_net,group=Date_ID,colour = Date_ID)) +
      geom_line(size=1.5) +
      #sc+
      scale_colour_viridis(option="viridis")+
      scale_y_continuous(breaks=seq(7000, 11000, 2000))+ # Ticks from 0-1200, every 200
      scale_x_continuous(breaks=seq(0, 23, 2))+ # Ticks from 0-1200, every 200
      expand_limits(y=c(6000,11000))+
      labs(colour = "Day") +
      theme_minimal()+theme(
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.position = "right",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 14,face = "bold", colour="black")
      )+    labs(y="Hourly wind generation (MW)",x="Hour",
                 title="Hourly AIL Net of Wind Generation, March 2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    
    dev.off()
  
    
    #Mac
    if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130)
    #PC
    if(R.version$platform ==  "x86_64-w64-mingw32")
      png(file="hourly_wind_ramen.png", width = 1400, height = 750,res=130,type='cairo')
    
    ggplot(df1,aes(Hour,AIL,group=Date_ID,colour = Date_ID)) +
      geom_line(size=1.5) +
      #sc+
      scale_colour_viridis(option="viridis")+
      scale_y_continuous(breaks=seq(7000, 11000, 2000))+ # Ticks from 0-1200, every 200
      scale_x_continuous(breaks=seq(0, 23, 2))+ # Ticks from 0-1200, every 200
      expand_limits(y=c(6000,11000))+
      labs(colour = "Day") +
      theme_minimal()+theme(
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.position = "right",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 14,face = "bold", colour="black")
      )+    labs(y="Hourly AIL (MW)",x="Hour",
                 title="Hourly AIL, March 2017",
                 subtitle="Source: AESO Data via NRGStream, Graph by Andrew Leach")
    dev.off()      


  load_data <- read.xlsx(xlsxFile = "aeso_load_data.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  load_data$Effective.Date <- as.POSIXct(load_data$Effective.Date * (60*60*24), origin="1899-12-30", tz="GMT")
  load_data <- setNames(load_data,c("Date","AIL"))
  load_data$month <- month(load_data$Date)
  load_data$day <- day(load_data$Date)
  load_data$year <- year(load_data$Date)
  load_data$Date_ID = as.yearmon(load_data$Date)
  load_data<-na.omit(load_data)
  
  df1 <-load_data %>% group_by(Date_ID) %>% summarise(peak_ail=max(AIL))
  
  #png(file="solar_monthly_gen.png", width = 1400, height = 750)
  ggplot(df1,aes(Date_ID,peak_ail,group = 1)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    labs(colour="Year") +
    scale_color_viridis(discrete=TRUE)+   
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
    )+    labs(y="Monthly Average Power Price ($/MWh)",x="Month",
               title="Time Series of Generation-Weighted Monthly Average Energy Prices ($/MWh, 2014-2017)",
               subtitle="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  
  #dev.off()
  
  df2 <-load_data %>% group_by(year) %>% summarise(peak_ail=max(AIL))
  
  
  
  forecast_data <- read.xlsx(xlsxFile = "aeso_forecasts.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  forecast_data$Date<-mdy_h(forecast_data$Date,tz=tz(now()))
  colnames(forecast_data) <- c("Date", "Forecast.Price.3Hr","Price","DA.Forecast.AIL","AIL","AIL.Diff")
  
  sub_samp<-subset(forecast_data, Date > mdy_h("07/25/2017 23",tz=tz(now())))
  sub_samp<-subset(sub_samp, Date <= mdy_h("07/29/2017 00",tz=tz(now())))
  sub_samp$hour <-hour(sub_samp$Date)
  
  lims <- c(min(sub_samp$Date),max(sub_samp$Date))
  breaks <- make_breaks(min(sub_samp$Date), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  df1<-melt(sub_samp,id=c("Date","hour"),measure.vars = c("Forecast.Price.3Hr","Price"),value.name = "data")  
  df2<-melt(sub_samp,id=c("Date"),measure.vars = c("DA.Forecast.AIL","AIL"),value.name = "data")  
  df1$Date_ID<-paste(day(df1$Date),hour(df1$Date))
  
  png(file="AESO_price.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(Date,data)) +
    geom_line(size=1,aes(group=variable,colour=variable)) +
    
    scale_color_viridis(discrete=TRUE,labels=c("3hr Ahead Forecast","Actual Hourly Pool Price"))+   
    scale_x_datetime(limits = lims,breaks = breaks,labels = date_format("%d/%m %H:00"))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+    labs(y="Energy Price ($/MWh)",x="Hour",
               title="Alberta hourly actual and 3 hour ahead forecast power prices\nJuly 26-29th, 2017",
               caption="Source: AESO Data, Accessed via NRGStream\n Graph by Andrew Leach")  
  dev.off()  
  
  
  png(file="AESO_load.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df2,aes(Date,data)) +
    geom_line(size=1,aes(group=variable,colour=variable)) +
    
    scale_color_viridis(discrete=TRUE,labels=c("Day Ahead Forecast","Actual Hourly Demand"))+   
    scale_x_datetime(limits = lims,breaks = breaks,labels = date_format("%d/%m %H:00"))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(angle=-90)
    )+    labs(y="Alberta Internal Load (MW)",x="Hour",
               title="Alberta hourly actual and day-ahead forecast power demand\nJuly 26-29th, 2017",
               caption="Source: AESO Data, Accessed via NRGStream\n Graph by Andrew Leach")  
  dev.off()  
  
  
  #Time Zone is Canada/Mountain
  sub_samp<-subset(gen_data, Time > mdy_h("07/22/2017 00",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("07/29/2017 00",tz="GMT"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)

  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  

    
  #take out totals, CR, FCASTS, etc
    sub_samp<-sub_samp[-grep("TOTAL", sub_samp$Plant_Type), ]
    sub_samp<-sub_samp[-grep("WIND_FCAST", sub_samp$Plant_Type), ]
    sub_samp<-sub_samp[-grep("CR", sub_samp$Plant_Type), ]
  
    #group gen into types
  #sub_samp<-na.omit(sub_samp)
  df1 <- sub_samp %>% group_by(Plant_Type,Time) %>% summarise(gen.MWh = sum(gen),ail=mean(AIL),price=mean(Price))
  
  
  df1$Plant_Type <- factor(df1$Plant_Type, levels=c("COAL","COGEN", "HYDRO", "NGCC",  "OTHER" ,"SCGT"  ,"TRADE", "WIND"))
  #reorder
  df1$Plant_Type = factor(df1$Plant_Type,levels(df1$Plant_Type)[c(8,1,2,3,4,5,6,7)])
  
  #HERE  
  #png(file="gen_summer_peak.png", width = 1400, height = 750)
  ggplot(df1,aes(Time,gen.MWh)) + 
    #geom_area(data =subset(df1,!grepl("TRADE", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =subset(df1,grepl("TRADE", df1$Plant_Type)), aes(Time,-gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    geom_area(data =df1, aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type), position = 'stack',size=1.5)+
    geom_line(data =df1, aes(Time,ail,colour="AIL"),size=2)+
    
    scale_colour_viridis(discrete = TRUE,option="magma",labels=c("Alberta Demand"))+
    
    #scale_fill_brewer(type = "seq", palette = "Set1", direction = 1,labels=c(levels(df1$Plant_Type)))+
    scale_fill_viridis(discrete = TRUE,option="viridis",labels=c(levels(df1$Plant_Type)))+
    
    #scale_colour_brewer(type = "seq", palette = 1, direction = 1,labels=c("AIL"))+
    scale_x_datetime(limits = lims,breaks = breaks,labels = date_format("%d/%m %H:00"))+
    theme_minimal()+theme(
      legend.position = "right",
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
    labs(x="Date",y="Hourly Generation (MW)",
         title="Distribution of Hourly Energy Production, July 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  #dev.off()
  
 
  sub_samp<-subset(gen_data, Time > mdy_h("07/26/2017 08",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("07/26/2017 22",tz="GMT"))
  sub_samp<-arrange(sub_samp,AESO_Name)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='2 hour', length.out=length(sub_samp))
  
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
  sub_samp<-subset(gen_data, Time > mdy_h("07/26/2017 08",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("07/26/2017 22",tz="GMT"))
  
  df2<-sub_samp %>% group_by(Plant_Type,Time) %>% summarise(gen.MWh = sum(gen),ail=mean(AIL),price=mean(Price))
  df2<-df2[grep("WIND", df2$Plant_Type), ]
  df2<-df2[-grep("FCAST", df2$Plant_Type), ]
  
  
  
  png(file="coal_summer_peak.png", width = 1400, height = 750)
  
  #lims <- c(mdy_h("07/27/2017 00",tz=tz(now())),mdy_h("07/27/2017 23",tz=tz(now())))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='2 hour', length.out=length(sub_samp))
  ggplot(df1,aes(Time,gen)) + 
    #geom_area(data =subset(df1,!grepl("TRADE", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =subset(df1,grepl("COAL", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =df1, aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type), position = 'stack',size=1.5)+
    geom_line(data =df1, aes(Time,gen,colour=AESO_Name,group=AESO_Name),size=1.5)+
    geom_line(data =df1, aes(Time,Price,colour="Alberta Power Price"),size=1.5)+
    geom_line(data =df2, aes(Time,gen.MWh,colour="Wind Generation"),size=1.5)+
    #geom_line(data =df1, aes(Time,AIL/100),size=1.5)+
    
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
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='2 hour', length.out=length(sub_samp))
  ggplot(df1,aes(Time,gen)) + 
    #geom_area(data =subset(df1,!grepl("TRADE", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =subset(df1,grepl("COAL", df1$Plant_Type)), aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type),size=1.5,linetype="dashed")+
    #geom_area(data =df1, aes(Time,gen.MWh,group=Plant_Type,fill=Plant_Type), position = 'stack',size=1.5)+
    #geom_line(data =df1, aes(Time,gen,colour=AESO_Name,group=AESO_Name),size=1.5)+
    #geom_line(data =df1, aes(Time,Price,colour="Alberta Power Price"),size=1.5)+
    #geom_line(data =df2, aes(Time,gen.MWh,colour="Wind Generation"),size=1.5)+
    geom_line(data =df1, aes(Time,AIL,colour="Alberta Internal Load"),size=1.5)+
    
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
  
  
  
   
  
  
  
  sub_samp<-subset(gen_data, Time >= as.Date("2017-11-01"))
  sub_samp<-subset(sub_samp, Time <= as.Date("2017-11-15"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  #select wind forecasts
  #sub_samp<-sub_samp[grep("AB - Wind", sub_samp$NRG_Stream), ]
  #remove 7 day capacility
  #sub_samp<-sub_samp[-grep("Capability", sub_samp$NRG_Stream), ]
  #sub_samp<-sub_samp[-grep("Most Likely", sub_samp$NRG_Stream), ]
  #df1<-sub_samp[-grep("Wind Generation Unit Net", sub_samp$NRG_Stream), ]
  df2<-sub_samp[grep("WIND",sub_samp$Plant_Fuel), ]
  df2<-df2[-grep("WIND_", df2$ID), ]
  
  #reverse order
  df2$AESO_Name = with(df2, factor(AESO_Name, levels = rev(levels(AESO_Name))))
  
 
  #myPalette <- colorRampPalette(rev(brewer.pal(9, "Reds")))
  #df2<-head(df2,1000)
  #subset(sub_samp, grepl("12 Hr", sub_samp$NRG_Stream)) 
  p<-ggplot(df2, aes(AESO_Name,gen/Capacity*100,fill=(gen/Capacity*100))) +
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,gen/Capacity*100,fill=(gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100,fill=(66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100,fill=(33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100),frame = Time))+
    
      coord_flip()+
    ggtitle("Alberta Wind Power Capacity Factor\n")+
    scale_fill_gradient2("Capacity Factor",low = muted("green"), mid = "yellow",
                           high = "red", midpoint = 50)+
    theme_minimal()+theme(
      legend.position = "none",
      legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 11, colour="black"),
      axis.text.x=element_text(angle=0)
    )+
    labs(x="",y="Hourly Capacity Factor (%)",
         #title="Alberta Wind Generation, October 1-15, 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  
  animation::ani.options(interval = .01)
  gganimate(p, title_frame = TRUE,"capacity.gif")
  
  #subset(sub_samp, grepl("12 Hr", sub_samp$NRG_Stream)) 
  p<-ggplot(df2, aes(AESO_Name,gen,fill=(gen/Capacity*100))) +
    #facet_wrap(~ dataset, nrow = 3) +
    #geom_bar(stat="identity", position="dodge",aes(frame = Time))+
    
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,gen,fill=(gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,Capacity*.66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen,fill=(66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,Capacity*.33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen,fill=(33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100),frame = Time))+
    ggtitle("Alberta Wind Power Generation and Capacity Factor\n")+
    
    
    #scale_colour_viridis(discrete = TRUE,option="viridis")+
    #scale_fill_brewer(type = "seq", palette = "Set1", direction = 1,labels=c("Coal"))+
    #scale_fill_viridis(discrete = TRUE,option="magma")+
    scale_fill_gradient2("Capacity\nFactor",low = muted("green"), mid = "yellow",
                         high = "red", midpoint = 50)+
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "right",
      legend.title=element_text(colour="black", size = 16, face = "bold"),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 11, colour="black"),
      axis.text.x=element_text(angle=90)
    )+
    labs(x="",y="Hourly Generation (MW)",
         #title="Alberta Wind Generation, November 1-15, 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  #dev.off()
  
  animation::ani.options(interval = .01)
  gganimate(p, title_frame = TRUE,"generation.gif")
  
  
  sub_samp<-subset(gen_data, Time >= as.Date("2017-07-26"))
  sub_samp<-subset(sub_samp, Time < as.Date("2017-07-27"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  #select wind forecasts
  #sub_samp<-sub_samp[grep("AB - Wind", sub_samp$NRG_Stream), ]
  #remove 7 day capacility
  #sub_samp<-sub_samp[-grep("Capability", sub_samp$NRG_Stream), ]
  #sub_samp<-sub_samp[-grep("Most Likely", sub_samp$NRG_Stream), ]
  #df1<-sub_samp[-grep("Wind Generation Unit Net", sub_samp$NRG_Stream), ]
  #sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="GAS")
  #df2<-sub_samp
  df2<-sub_samp[grep("COAL", sub_samp$Plant_Fuel), ] #coal plants
  #df2<-df2[-grep("WIND_", df2$ID), ] #take out the wind forecasts
  #df2<-df2[-grep("TOTAL", df2$Plant_Type), ]
  
  #reverse order
  df2$AESO_Name = with(df2, factor(AESO_Name, levels = rev(levels(AESO_Name))))
  
  p<-ggplot(df2, aes(AESO_Name,gen,fill=(gen/Capacity*100))) +
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,gen/Capacity*100,fill=(gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100,fill=(66*(gen/Capacity*100>66)+(gen/Capacity*100<=66)*gen/Capacity*100),frame = Time))+
    geom_bar(data=df2,stat="identity", position="dodge",aes(AESO_Name,33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100,fill=(33*(gen/Capacity*100>33)+(gen/Capacity*100<=33)*gen/Capacity*100),frame = Time))+
    coord_flip()+
    ggtitle("Alberta Coal Power Generation and Capacity Factor\n")+
    scale_fill_gradient2(low = muted("green"), mid = "yellow",
                         high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "right",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 12, colour="black"),
      axis.text.x=element_text(angle=0)
    )+
    labs(x="",y="Hourly Capacity Factor (%)",
         title="Alberta Coal Capacity Factors, July 26, 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  
  animation::ani.options(interval = .5,ani.width = 800, ani.height = 400)
  gganimate(p, title_frame = TRUE,"coal_gen.gif")
  
 
  sub_samp<-subset(gen_data, Time > as.Date("2017-01-1"))
  sub_samp<-subset(sub_samp, Time < as.Date("2017-12-31"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  df2<-sub_samp[grep("COAL", sub_samp$Plant_Fuel), ] #coal plants
  df2<-na.omit(df2)
  df2$month<-month(df2$Time)
  df2$year<-year(df2$Time)
  df2$year_mon<-as.Date(as.yearmon(df2$Time))
  df2 <- df2 %>% group_by(year_mon,AESO_Name) %>% summarise(cap_fac=100*mean(gen/Capacity))
  
  
  
  sub_samp<-subset(gen_data, Time > as.Date("2008-10-1"))
  sub_samp<-subset(sub_samp, Time < as.Date("2018-10-1"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
    df3<-sub_samp[grep("COAL", sub_samp$Plant_Fuel), ] #coal plants
  df3<-na.omit(df3)
  df3 <- df3 %>% mutate(month=month(Time),year=year(Time)) %>% group_by(month,year) %>% 
                summarise(cap_fac=sum(gen)/sum(Capacity),Date=mean.POSIXct(Time))
  
  
  
  #df2<-df2[-grep("WIND_", df2$ID), ] #take out the wind forecasts
  #df2<-df2[-grep("TOTAL", df2$Plant_Type), ]
  
  #reverse order
  df2$AESO_Name = with(df2, factor(AESO_Name, levels = rev(levels(AESO_Name))))
  
    ggplot(df2, aes(year_mon,cap_fac),group=AESO_Name) +
    geom_line(stat="identity",aes(group=AESO_Name))+
      #facet_grid(. ~ AESO_Name, margins = TRUE)+
      facet_wrap(~ AESO_Name)+
    ggtitle("Alberta Coal Power Generation and Capacity Factor\n")+
    scale_fill_gradient2(low = muted("green"), mid = "yellow",
                         high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "right",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 8,face = "bold"),
      axis.text = element_text(size = 8, colour="black"),
      axis.text.x=element_text(angle=0)
    )+
    labs(x="",y="Average Monthly Capacity Factor (%)",
         title="Alberta Coal Capacity Factors, 2004-2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  
  set_png("coal_cap_fac.png")
    ggplot(df3, aes(as.Date(Date),cap_fac*100)) +
      geom_line()+
      #facet_grid(. ~ AESO_Name, margins = TRUE)+
            ggtitle("Alberta Coal Generation Capacity Factor\n")+
      scale_fill_gradient2(low = muted("green"), mid = "yellow",
                           high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
      #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
      #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
      #expand_limits(x = lims)+
      theme_minimal()+theme(
        legend.position = "right",
        #legend.title=element_blank(),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 16, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 8,face = "bold"),
        axis.text = element_text(size = 8, colour="black"),
        axis.text.x=element_text(angle=0)
      )+
      labs(x="",y="Average Monthly Capacity Factor (%)",
           title="Alberta Coal Capacity Factors, 2004-2017",
           caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()  
    
  sub_samp<-subset(gen_data, Time > as.Date("2008-10-1"))
  sub_samp<-subset(sub_samp, Time < as.Date("2018-10-1"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
    
    lims <- c(min(sub_samp$Time),max(sub_samp$Time))
    breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
    
    df2<-sub_samp
    df2<-na.omit(df2)
    df2$month<-month(df2$Time)
    df2$year<-year(df2$Time)
    df2$year_mon<-as.Date(as.yearmon(df2$Time))
    df2 <- df2 %>% filter(Plant_Type!="COGEN") %>%
      group_by(year_mon,Plant_Type) %>% summarise(cap_fac=100*mean(gen/Capacity))
    
    #df2<-df2[-grep("WIND_", df2$ID), ] #take out the wind forecasts
    #df2<-df2[-grep("TOTAL", df2$Plant_Type), ]
    
    #reverse order
    #df2$AESO_Name = with(df2, factor(AESO_Name, levels = rev(levels(AESO_Name))))
    set_png("facet_cap_fac")
    ggplot(df2, aes(year_mon,cap_fac),group=Plant_Type) +
      geom_line(stat="identity",aes(group=Plant_Type))+
      #facet_grid(. ~ AESO_Name, margins = TRUE)+
      facet_wrap(~ Plant_Type)+
      ggtitle("Alberta Power Generation Capacity Factor by Plant Type\n")+
      scale_fill_gradient2(low = muted("green"), mid = "yellow",
                           high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
      #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
      #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
      #expand_limits(x = lims)+
      theme_minimal()+theme(
        legend.position = "right",
        #legend.title=element_blank(),
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 16, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 8,face = "bold"),
        axis.text = element_text(size = 8, colour="black"),
        axis.text.x=element_text(angle=0)
      )+
      labs(x="",y="Average Monthly Capacity Factor (%)",
           title="Alberta Capacity Factors by Plant Type, 2004-2018",
           caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()
  
   
  
  #Wind Capacity Factor by Plant, July
  
  sub_samp<-subset(gen_data, Time > mdy_h("07/1/2011 00",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("07/31/2011 23",tz="GMT"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='4 hour', length.out=length(sub_samp))
  
  #select wind forecasts
  #sub_samp<-sub_samp[grep("AB - Wind", sub_samp$NRG_Stream), ]
  #remove 7 day capacility
  #sub_samp<-sub_samp[-grep("Capability", sub_samp$NRG_Stream), ]
  #sub_samp<-sub_samp[-grep("Most Likely", sub_samp$NRG_Stream), ]
  #df1<-sub_samp[-grep("Wind Generation Unit Net", sub_samp$NRG_Stream), ]
  #sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="COAL" | sub_samp$`Plant_Fuel`=="GAS")
  #df2<-sub_samp
  df2<-sub_samp[grep("WIND", sub_samp$Plant_Fuel), ] #wind plants
  df2<-df2[-grep("WIND_FCAST", df2$Plant_Type), ] #wind plants
  df1 <- df2 %>% group_by(AESO_Name) %>% summarise(gen.MWh = sum(gen),avg_rev=sum(gen*Price)/sum(gen),cap_fac=100*mean(gen/Capacity))
  df1<-melt(df1,id=c("AESO_Name"),measure.vars = c("cap_fac","avg_rev"))
  df1$variable <- factor(df1$variable, labels = c("Capacity Factor (%)", "Average Revenue ($/MWh)"))
  
  png(file="july_wind.png", width = 1400, height = 750,res=130,type='cairo') 
  ggplot(df1, aes(AESO_Name,value,fill=variable)) +
    geom_bar(stat="identity", position="dodge")+
    coord_flip()+
    ggtitle("Alberta Wind Plant Capacity Factor\n")+
    facet_grid(. ~ variable,scales = "free")+
    #scale_fill_gradient2(low = muted("green"), mid = "yellow", high = "red", midpoint = 50,name="Capacity\nFactor (%)\n")+
    #scale_fill_viridis(discrete=TRUE,option = "magma")+
    scale_fill_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "none",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 12, colour="black"),
      axis.text.x=element_text(angle=0)
    )+
    labs(x="",y="",
         title="Alberta Wind Plant Average Revenue and Capacity Factors, July 2017",
         caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
  dev.off()  
  
  #AURORA TRADE OUTAGE DATA
  
sub_samp<-gen_data[grep("WECC", gen_data$NRG_Stream), ]
sub_samp<-sub_samp[!is.na(sub_samp$gen),]
sub_samp<-subset(sub_samp, Time >= as.Date("2015-07-01"))
sub_samp<-subset(sub_samp, Time < as.Date("2015-07-31"))




imp_exp_data <- read.xlsx(xlsxFile = "Imports_and_Exports_Q3_2015.xlsx", sheet = 1, startRow = 5,skipEmptyRows = TRUE,detectDates = TRUE)
#imp_exp_data$Time<- as.POSIXct(paste(imp_exp_data$Date," ",imp_exp_data$Hour.Ending,":00",sep=""),format="%Y-%M-%d %H:%M",tz="America/Denver")
imp_exp_data$Time<- paste(imp_exp_data$Date," ",imp_exp_data$Hour.Ending,":00",sep="")
imp_exp_data$Time<-as.POSIXct(imp_exp_data$Time,format="%Y-%m-%d %H:%M",tz="America/Denver")
imp_exp_data$WECC<-imp_exp_data$BC.Interface.Imp+imp_exp_data$MATL.Interface.Imp-(imp_exp_data$BC.Interface.Exp+imp_exp_data$MATL.Interface.Exp)
new_melt <- melt(imp_exp_data,id=c("Time"),measure.vars = c("WECC"), variable.name="AESO_Data",value.name = "trade") 

test_trade<-merge(new_melt,sub_samp,by="Time")

set_png(file="AESO_trade.png", width = 1400, height = 750)
ggplot(test_trade)+
  geom_line(aes(Time,gen,group=NRG_Stream,colour="NRGStream"))+
  geom_line(aes(Time,trade,group=AESO_Data,colour="AESO\nScheduled"))+
  scale_color_brewer("Data Source",palette = "Set1")+
  #expand_limits(x = lims)+
  theme_minimal()+theme(
    legend.position = "bottom",
    #legend.title=element_blank(),
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    legend.title = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 10, colour="black"),
    axis.text.x=element_text(size = 8,angle=0)
  )+
  labs(x="",y="Monthly Average Hourly Net Imports (MW)",
       title="Alberta WECC Net Imports, Q3 2015",
       #subtitle="30% by 2030 Renewable Generation Constraint",
       caption="Source: AESO and NRGStream Data\n Graph by Andrew Leach") 
dev.off()


sub_samp<-gen_data[grep("WECC", gen_data$NRG_Stream), ]
sub_samp<-sub_samp[!is.na(sub_samp$gen),]
toMatch <- c("gen", "Revenue", "Cap_Fac")
cols=names(sub_samp)[-unique(grep(paste(toMatch,collapse="|"), 
                                  names(sub_samp)))]

if(!exists("getSeason", mode="function")) source("get_season.R")


cols=names(sub_samp)[2:4]
test_data<-sub_samp %>% 
  #group_by(Time,Demand,Price) %>%
  group_by_at(cols) %>%
  summarize(WECC_trade=sum(gen)) %>%
  ungroup() # if you use group_by, also use ungroup() to save heartache later

NAs<-test_data[is.na(test_data$WECC_trade),]

  test_data$Season<-getSeasonPOSIXct(test_data$Time)
  test_data$month<-month(test_data$Time)
  test_data$hour<-hour(test_data$Time)
  test_data$year<-year(test_data$Time)

  test_data<-test_data %>% 
    #group_by(Time,Demand,Price) %>%
    group_by(Season,hour,year,Price,Demand) %>%
    summarize(WECC_trade=mean(WECC_trade)) %>%
    ungroup() # if you use group_by, also use ungroup() to save heartache later
  
  
  set_png(file="WECC_trade.png", width = 1400, height = 750)
  ggplot(subset(test_data,year>2014))+
    geom_line(aes(hour,WECC_trade,group=Season,colour=Season),size=2)+
    scale_color_brewer("Season",palette = "Set1")+
    facet_grid(~year)+
    scale_x_continuous(breaks=seq(1,24,3))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "bottom",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      legend.title = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 10, colour="black"),
      axis.text.x=element_text(size = 8,angle=0)
    )+
    labs(x="",y="Average Hourly Net Imports (MW)",
         title="Alberta WECC Net Imports",
         #subtitle="30% by 2030 Renewable Generation Constraint",
         caption="Source: AESO and NRGStream Data\n Graph by Andrew Leach") 
  dev.off()
  
    
  set_png(file="WECC_import_price.png", width = 1400, height = 750)
  ggplot(subset(test_data,year>2014))+
    geom_point(aes(WECC_trade,Price,group=Season,colour=Season),size=2)+
    scale_color_brewer("Season",palette = "Set1")+
    facet_grid(~year)+
    scale_x_continuous(breaks=seq(1,24,3))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "bottom",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      legend.title = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 10, colour="black"),
      axis.text.x=element_text(size = 8,angle=0)
    )+
    labs(x="",y="Average Hourly Wholesale Price ($/MWh)",
         title="Alberta WECC Net Import Price Sensitivity",
         #subtitle="30% by 2030 Renewable Generation Constraint",
         caption="Source: NRGStream Data\n Graph by Andrew Leach") 
  dev.off()
  
  
  

  df2<-sub_samp
  df2$month<-month(df2$Time)
  df2$hour<-hour(df2$Time)
  df2$week<-week(df2$Time)
  df2$Year<-year(df2$Time)
  df2$Day<-as.factor(format(as.Date(df2$Time), "%A"))
  
  #df1 <- df2  %>% group_by(AESO_Name,Year) %>% summarise(gen.MWh = sum(gen))
  df1 <- df2 %>% group_by(AESO_Name,ID,month,Day,hour) %>% summarise(gen.MWh = mean(gen),avg_rev=sum(gen*Price)/sum(gen),cap_fac=100*mean(gen/Capacity),outage=100-100*mean(gen/Capacity))
  df1$Day <- ordered(df1$Day,labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  
  #df1 <- df2
  
  
  
  
  
  
  
  
  
#AURORA WIND OUTAGE DATA
  
  sub_samp<-gen_data[grep("WIND", gen_data$Plant_Fuel), ] #wind plants
  sub_samp<-subset(sub_samp, Time > as.Date("2016-01-01"))
  sub_samp<-subset(sub_samp, Time < as.Date("2016-12-31"))
  sub_samp<-na.omit(sub_samp)
  
  #df2<-df2[-grep("WIND_FCAST", df2$Plant_Type), ] #wind plants
  
  df2<-sub_samp
  df2$month<-month(df2$Time)
  df2$hour<-hour(df2$Time)
  df2$week<-week(df2$Time)
  df2$Year<-year(df2$Time)
  df2$Day<-as.factor(format(as.Date(df2$Time), "%A"))
  
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
   
  
   #sub_samp <-subset(gen_data, Time>="2015-01-01")
   #sub_samp[,][is.na(sub_samp[,])] <- 0
   #df1 <- sub_samp %>% group_by(Time,Plant_Type) %>% summarise(Price=mean(Price),Generation=sum(gen),Demand=mean(Demand))
   
   #write.xlsx(df1, file = "luft_data_1.xlsx", colNames = TRUE, borders = "columns")
   #df2<-dcast(df1,Time+Price+Demand~Plant_Type,value.var = c("Generation"))
   #df2$hour<-hour(df2$Time)
   #write.xlsx(df2, file = "luft_data_1.xlsx", colNames = TRUE, borders = "columns")
   
  
  df1 <- df2 %>% group_by(AESO_Name,ID,month) %>% summarise(gen.MWh = mean(gen),avg_rev=sum(gen*Price)/sum(gen),cap_fac=100*mean(gen/Capacity),outage=100-100*mean(gen/Capacity))
  df1$months = factor(month.abb[df1$month], levels = month.abb)
  ggplot(data=df1, aes(AESO_Name,gen.MWh,fill=(cap_fac))) +
    geom_bar(stat="identity", position="dodge")+
    coord_flip()+
    #ggtitle("Alberta Coal Power Generation and Capacity Factor\n")+
    scale_fill_gradient2(low = muted("green"), mid = "yellow",
                         high = "red", midpoint = 35,name="Capacity\nFactor (%)\n")+
    #facet_grid(. ~ Year,scales = "free")+
    facet_grid(. ~ months)+
    #scale_colour_brewer(type = "seq", palette = "Set1", direction = 1)+
    #scale_x_datetime(breaks = breaks,labels = date_format("%d/%m %H:00"))+
    scale_y_continuous(breaks=c(0,75,150))+
    #expand_limits(x = lims)+
    theme_minimal()+theme(
      legend.position = "right",
      #legend.title=element_blank(),
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 12,face = "bold"),
      axis.text = element_text(size = 10, colour="black"),
      axis.text.x=element_text(size = 8,angle=0)
    )+
    labs(x="",y="Monthly Average Hourly Generation (MW)",
         title="Alberta Wind Power Generation, 2016",
         #subtitle="30% by 2030 Renewable Generation Constraint",
         caption="Source: AESO Data, Accessed via NRGStream\n Graph by Andrew Leach") 
  
  
  
  
  
   
  
  
  

  
  paired_viridis<-function(pairs){
   palette<-c(viridis(pairs)[1],viridis(pairs)[1])
    if(pairs>1)
      for(c in c(2:pairs))
        palette<-c(palette,viridis(pairs)[c],viridis(pairs)[c])
  return(palette)
  }
  
  sub_samp<-subset(gen_data, Time > mdy_h("07/01/2016 00",tz="GMT"))
  sub_samp<-subset(sub_samp, Time <= mdy_h("7/31/2016 23",tz="GMT"))
  sub_samp<-arrange(sub_samp,Time,NRG_Stream)
  lims <- c(min(sub_samp$Time),max(sub_samp$Time))
  breaks <- make_breaks(min(sub_samp$Time), hour=0, interval='168 hour', length.out=length(sub_samp))
    
  
  palette_gen_data<-c(paired_viridis(2),"Black")
  png(file="july_wind.png", width = 1400, height = 1500,res=130,type='cairo') 
  ggplot(sub_samp,aes(Time,gen)) + 
      #geom_line(data =subset(sub_samp, (NRG_Stream %in% c("AB - Wind Generation Unit Net MW")),group=NRG_Stream,colour=NRG_Stream)) +
      geom_line(data =subset(sub_samp,(NRG_Stream %in% c("AB - Wind Generation Unit Net MW"))), aes(group=NRG_Stream,colour=NRG_Stream),size=3)+
      geom_line(data =subset(sub_samp, grepl("12 Hr", sub_samp$NRG_Stream)), aes(group=NRG_Stream,colour=NRG_Stream),size=1.5,linetype="dashed")+
      #geom_line(data =subset(sub_samp, grepl("7 Day", sub_samp$NRG_Stream)), aes(group=NRG_Stream,colour=NRG_Stream),size=1.5,linetype="dashed")+    
     scale_colour_manual(values = palette_gen_data)+
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
  
  
  
  
  
  
  lto_data <- read.xlsx(xlsxFile = "aesoLTO.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
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
  
  set_png(file="AESO_LTO.png", width = 1400, height = 750)
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
 #BLAKE CAISO CODE
 
 #set parameters
 queryname <- "SLD_REN_FCST"
 market_run_id <- "ACTUAL"
 startdatetime <- 20160601
 enddatetime <- 20160630
 
 #create URL
 apiURL<-paste("oasis.caiso.com/oasisapi/Singl.",queryname,"&market_run_id=",market_run_id,"&startdatetime=",startdatetime,"T07:00-0000","&enddatetime=",enddatetime,"T07:00-0000","&version=1",sep="")
 
 #download to temp file
 temp<-tempfile()
 download.file(apiURL,temp,method="curl")
 tempdata <- read.table(unzip(temp), sep = ",", header=TRUE)
 unlink(temp) #deletes 'temp' file
 
 
 
 caiso_mar <- read.xlsx(xlsxFile = "caiso_mar_11.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
 df_caiso <- melt(caiso_mar,id=c("Hour"),measure=c("GEOTHERMAL" ,"BIOMASS","BIOGAS","SMALL.HYDRO","WIND.TOTAL","SOLAR.PV","SOLAR.THERMAL"),value.name = "generation") 
 df2_caiso <- melt(caiso_mar,id=c("Hour"),measure=c("RENEWABLES.TOTAL", "NUCLEAR","THERMAL","IMPORTS","HYDRO"),value.name = "generation") 
 
 png(file="caiso_day.png", width = 1400, height = 750)
 ggplot(df_caiso, aes(Hour,generation))+ 
   geom_area(aes(fill=variable), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
   scale_fill_viridis("Generation Type",discrete = TRUE,option="viridis")+
   scale_x_continuous(breaks=seq(1, 24, 2))+ # Ticks from 0-1200, every 200+
   #scale_fill_brewer(palette="Spectral") +
   theme_minimal()+theme(
     legend.position = "right",
     legend.margin=margin(c(0,0,0,0),unit="cm"),
     legend.text = element_text(colour="black", size = 18, face = "bold"),
     plot.caption = element_text(size = 20, face = "italic"),
     plot.title = element_text(face = "bold"),
     plot.subtitle = element_text(size = 20, face = "italic"),
     panel.grid.minor = element_blank(),
     text = element_text(size = 20,face = "bold"),
     axis.text = element_text(size = 20,face = "bold", colour="black")
   )+
   labs(x="Hour",y="Hourly Generation (MWh)",
        title="Distribution of Hourly Renewable Energy Production (MWh), March 11, 2017",
        caption="Source: CAISO Data. Graph by @andrew_leach")
 dev.off()

 df2_caiso$variable <- ordered(df2_caiso$variable,labels = c("RENEWABLES","NUCLEAR","THERMAL","IMPORTS","HYDRO"))
 
 png(file="caiso_day_all.png", width = 1400, height = 750)
 ggplot(df2_caiso, aes(Hour,generation))+ 
   geom_area(aes(fill=variable), position = 'stack')+ guides(fill=guide_legend(title=NULL))+
   scale_fill_viridis("Generation Type",discrete = TRUE,option="viridis")+
   scale_x_continuous(breaks=seq(1, 24, 1))+ # Ticks from 0-1200, every 200+
   #scale_fill_brewer(palette="Spectral") +
   theme_minimal()+theme(
     legend.position = "right",
     legend.margin=margin(c(0,0,0,0),unit="cm"),
     legend.text = element_text(colour="black", size = 18, face = "bold"),
     plot.caption = element_text(size = 20, face = "italic"),
     plot.title = element_text(face = "bold"),
     plot.subtitle = element_text(size = 20, face = "italic"),
     panel.grid.minor = element_blank(),
     text = element_text(size = 20,face = "bold"),
     axis.text = element_text(size = 20,face = "bold", colour="black")
   )+
   labs(x="Hour",y="Hourly Generation (MWh)",
        title="Distribution of Hourly Energy Production (MWh), March 11, 2017",
        caption="Source: CAISO Data. Graph by @andrew_leach")
 dev.off()
 
 
 
 
 
 
 
 #US Hourly Power Demand
 #Northwest EBA.NW-ALL.D.H
 #Bonneville EBA.BPAT-ALL.D.H
 #CAISO EBA.CISO-ALL.D.H
 #MISO EBA.MISO-ALL.D.H
 # PJM EBA.PJM-ALL.D.H
 # NY EBA.NYIS-ALL.D.H
 #NE EBA.ISNE-ALL.D.H
 #ERCOT EBA.ERCO-ALL.D.H
 #US L48 EBA.US48-ALL.D.H
 
 #Net Generation
 #CAISO EBA.CISO-ALL.NG.H
 
 #Net Interchange
 #CAISO EBA.CISO-ALL.TI.H
 
 
 
 grids <-c("NW","CISO","MISO","PJM","NYIS","ERCO","ISNE")
 series <-c("D","NG","TI")
 
 grids <-c("ERCO")
 series <-c("D","NG","TI")
 
 
 
 count<-1
 for(g in grids){
   for(s in series){
     id<-paste("EBA.",g,"-ALL.",s,".H",sep="")
     if(count == 1)
       ids <- id
     if(count > 1)
       ids <- cbind(ids,id)
     count<-count+1
   }
 }
 
 #install.packages("rjson")
 library(rjson)
 X <- read.csv()
 
 #download.file(set_url, "testfile.json", method="auto")
 
 
 series <-c("D","NG","TI")
 
 for(s in series){
   print(paste("Starting loop for",s,sep=" "))
   set_url<-paste("http://api.eia.gov/series/?api_key=",KEY,"&series_id=EBA.CAL-ALL.",s,".H&out=json",sep="")
   data1 <- fromJSON(set_url)
   df_data<-data.frame(data1$series$data)
   df_data$X1<-strptime(df_data$X1,"%Y%Om%dT%HZ",tz = "GMT")
   df_data$X1<-ymd_hms(df_data$X1)
   df_data$X1<-with_tz(df_data$X1, "US/Pacific")
   df_data$X2<-as.numeric(as.character(df_data$X2))
   colnames(df_data) <- c("Date",data1$series$name)
   assign(paste(s,"_data",sep=""),df_data)
 }
 
 #set up date variables
 all_data<-merge(D_data,NG_data,by="Date")
 all_data<-merge(all_data,TI_data,by="Date")
 
 
 #all_data$Date<-as.Date(all_data$Date)
 all_data$month<-month(all_data$Date)
 all_data$year<-year(all_data$Date)
 all_data$day<-day(all_data$Date)
 all_data$hour<-hour(all_data$Date)
 all_data <- transform(all_data, Date_ID = paste(month.abb[month]," ",year,sep=""))
 colnames(all_data)[grep("Demand", colnames(all_data))] <- "Demand"
 colnames(all_data)[grep("generation", colnames(all_data))] <- "NetGen"
 colnames(all_data)[grep("interchange", colnames(all_data))] <- "NetTrade"
 
 save(all_data, file= "caiso.RData")
 
 #load("caiso.RData") ## which is here *equivalent* to
 
 
 sub_samp <-subset(all_data, year==2016)
 sub_samp <-subset(sub_samp, month==5)
 sub_samp <- sub_samp %>% group_by(Date_ID,hour) %>% summarise(ti_avg=mean(NetTrade),gen_avg=mean(NetGen),d_avg=mean(Demand),tot_avg=mean(NetGen-NetTrade))
 sub_samp<-na.omit(sub_samp)
 df1<-melt(sub_samp,id=c("Date_ID","hour"),measure.vars = c("d_avg"))
 
 #png(file="caiso_ramps_july.png", width = 1400, height = 750,res=130,type='cairo')
 ggplot(df1,aes(hour,value,group = factor(Date_ID),colour=factor(Date_ID))) +
   geom_line(size=1.7)+
   scale_y_continuous()+
   #scale_color_viridis(NULL, labels=c("July 2015","Jan 2016","July 2016","Jan 2017", "July 2017"),discrete=TRUE)+   
   scale_color_viridis(NULL,discrete=TRUE)+   
   #scale_x_date() +
   #scale_colour_manual(labe ls=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))
   theme_minimal()+theme(
     legend.position = "bottom",
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
   labs(y="Demand (MW)",x="Hour",
        title="California Demand (MW)",
        subtitle="Source: EIA Data, graph by Andrew Leach.")
 #dev.off()
 
 df1<-melt(sub_samp,id=c("Date_ID","hour"),measure.vars = c("gen_avg"))
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 