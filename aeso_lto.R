library(tidyverse)
library(readxl)
library(RColorBrewer)
library(stringr)

lto_data <- read_excel("data/aesoLTO.xlsx")
colnames(lto_data)[1] <- "date"
df1<-lto_data %>% pivot_longer(-c(date, Actual),names_to = "forecast",values_to = "peak") %>% 
  filter(!is.na(peak))%>%
  group_by(forecast)%>%mutate(split=forecast)%>%
  separate(split,into = c("year","case"),extra="merge")%>%
  #filter(
  #forecast != "2019.High.Growth",
  #forecast != "2019.Low.Growth",
  #forecast != "2016H",
  #forecast != "2014L",
  #forecast != "2014.EnviroShift",forecast != "2014.EnergyTrans",
  #forecast != "2016L",
#) %>%
  mutate(forecast=as_factor(forecast),
         forecast=fct_relevel(forecast,"2021 Reference Case",after = 18))

#set_png(file="images/AESO_LTO.png", width = 1400, height = 750)
#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
ggplot(df1) +
  geom_line(data=filter(df1,grepl("Reference",forecast)|grepl("Forecast",forecast))%>% filter(year!=2021),
            aes(date,peak,group = forecast,color="Previous AESO Reference Case Forecasts",linetype="Previous AESO Reference Case Forecasts"),size=.7) +
  geom_line(data=filter(df1,forecast=="Actual"),aes(date,peak,group = forecast,colour="Historic Peak Loads",linetype="Historic Peak Loads"),size=2) +
  geom_line(data=df1%>%filter(year==2021, forecast=="2021 Reference Case")%>%
              mutate(forecast=fct_relevel(forecast,"2021 Reference Case",after = Inf),
                     forecast=fct_recode(forecast,"Reference Case (2021)"="2021 Reference Case")),
              aes(date,peak,group = forecast,colour=forecast,linetype=forecast),size=2) +
  scale_colour_manual(NULL,values=c("Black","grey60","Black"),
                      guide=guide_legend(override.aes = list(lty = c("solid", "solid","21"),size=c(2,.7,2) ) ))+
  scale_linetype_manual(NULL,values=c("solid","solid","21")) +
  theme_minimal()+theme(
    legend.key.width=unit(2,"line"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic",hjust = 0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Alberta Peak Internal Load (MW)",x="",
             #title="AESO Long Term Outlook forecasts of Alberta peak internal load (MW, 2004-2021)",
             caption="Data via AESO, graph by Andrew Leach.")
ggsave("aeso_lto.png",width=14,height=6,dpi=300,bg="white")
ggsave("aeso_lto_small.png",width=16,height=9,dpi=150)

ggplot(df1) +
  geom_line(aes(date,peak,group = forecast,color="Previous AESO Forecasts",linetype="Previous AESO Forecasts"),size=.7) +
  geom_line(aes(date,Actual,colour="Historic Peak Loads",linetype="Historic Peak Loads"),size=2) +
  scale_colour_manual(NULL,values=c("Black","grey60","Black"))+
  #                    guide=guide_legend(override.aes = list(lty = c("solid", "solid","21"),size=c(2,.7,2) ) ))+
  scale_linetype_manual(NULL,values=c("solid","solid","21")) +
  theme_minimal()+theme(
    legend.key.width=unit(2,"line"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic",hjust = 0),
    plot.title = element_text(face = "bold",size=18),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Alberta Peak Internal Load (MW)",x="",
             title="The AESO is uniquely positioned and qualified to assess the future evolution of Alberta's electricity grid",
             caption="Data via AESO, graph by Andrew Leach.")
ggsave("aeso_lto_new.png",width=14,height=6,dpi=300,bg="white")








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
