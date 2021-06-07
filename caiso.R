
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
