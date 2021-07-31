#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/alberta_power")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/alberta_power")
print(getwd())

library(grid)
library(gridExtra)
library(ggpubr)
library(gganimate)
library(timeDate)


#seasons
if(!exists("ajl_hourly", mode="function")) source("../andrew_base.R")

#load merit data for the paper

load("merit_ghg_price.RData")  

library(broom)
library(purrr)
library(skimr)


merit_fossils<-bids_aug %>% filter(Plant_Fuel %in% c("COAL")) 
bids_aug<-NULL
merit_aug<-NULL
by_type<-NULL


save(merit_fossils,file="merit_fossils.RData")

load("merit_fossils.RData")  
#merit_fossils<-merit_fossils %>% select(-bid_10,-bid_30,-bid_40,-bid_60,-bid_70,-bid_90)
# Melt the data frame to get bid percentile data
id_names<-names(merit_fossils)[!grepl("bid_",names(merit_fossils))]

#pick 10% of the date/he combos at random  
sample_share<-.25
merit_fossils<-merit_fossils%>% sample_n(nrow(.)*sample_share, replace = F) #no replacement - don't duplicate



merit_fossils<-merit_fossils %>% 
  melt(id=id_names,variable.name="percentile",value.name = "price")%>%
  mutate(percentile=as.numeric(gsub("bid_","",as.character(percentile))))
         
         merit_fossils<-merit_fossils %>% mutate(he=ifelse(he=="02*","02",he))


# Split into pieces, fit model to each piece
by_type <- merit_fossils %>%  mutate(p2=percentile^2,p3=percentile^3,p4=percentile^4,gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
         year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
         tight=(supply_cushion<=500),really_tight=(supply_cushion<200),he_fac=as.factor(he))%>%
#nest(-c(Plant_Type,percentile)) %>%
nest(-c(Plant_Type,offer_control)) %>% 
  mutate(
    fit = map(data, ~ lm(price ~ 
                           poly(percentile,4)+
                           percentile*net+
                           #+p2+p3+p4+
                           poly(supply_cushion,2)+
                           poly(hourly_renewables,2)+
                           as.factor(hour)+
                           #percentile+
                           #on_peak+
                           #supply_cushion+
                           #supply_cushion*on_peak+
                           #day_ahead_forecasted_ail+
                           forecast_pool_price+
                           forecasted_actual_ail_difference+
                           total_import_capability+
                           #total_export_capability+
                           
                           nit_settle_cad_gj
                           +gas2+gas3
                           #tight+really_tight#+
                           #yearmonth
                           , data = .x)),
    #pred=map(fit,predict),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
    #augmented = map(fit, augment)
  )

test<-by_type %>% 
  unnest(tidied, .drop = TRUE) 

test2<-by_type %>% 
  unnest(glanced, .drop = TRUE) 


test4<-by_type %>% 
  unnest(pred, .drop = TRUE) %>% select(pred)%>% cbind(merit_fossils) %>% group_by(Plant_Type,percentile)%>%
  summarize(price=mean(price),pred=mean(pred))


test2<-by_type %>% 
  unnest(augmented, .drop = TRUE) 

data<-by_type %>% 
  unnest(data, .drop = TRUE) %>% cbind(by_type %>% unnest(augmented, .drop = TRUE) %>% 
                                         rename("Plant_Type_2"="Plant_Type") %>% select(.fitted,Plant_Type_2))


data2<-by_type %>% 
  unnest(data, .drop = TRUE) %>% cbind(by_type %>% unnest(pred, .drop = TRUE) %>% 
                                         rename("Plant_Type_2"="Plant_Type") %>% select(pred,Plant_Type_2))


averages<-ggplot(data)+
  geom_line(aes(percentile,price,color="Data"),fill=NA,size=1.5)+
  geom_line(aes(percentile,.fitted,color="Fixed Effects Model"),fill=NA,size=1.5)+
  facet_grid(~Plant_Type)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")
set_png(file="average_offers.png",width=1400,height = 850)
averages
dev.off()

df2<-merit_fossils %>% group_by(percentile,offer_control,year) %>% summarize(price=mean(price),
                                                                             min=min(price),
                                                                        max=max(price),
                                                                        sd_price=sd(price))

df3<-merit_fossils %>% group_by(percentile,offer_control,year,asset_id) %>% summarize(price=mean(price),
                                                                             min=min(price),
                                                                             max=max(price),
                                                                             sd_price=sd(price))

df4<-merit_fossils %>% group_by(percentile,offer_control,year,he) %>% summarize(price=mean(price),
                                                                                      min=min(price),
                                                                                      max=max(price),
                                                                                      sd_price=sd(price))


ggplot(df2)+
  geom_line(aes(percentile,price,color=offer_control),fill=NA,size=1.5)+
facet_grid(~year)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")



ggplot(df3)+
  geom_line(aes(percentile,price,color=asset_id),fill=NA,size=1.5)+
  facet_grid(offer_control~year)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")



ggplot(df4)+
  geom_line(aes(percentile,price,color=offer_control),fill=NA,size=1.5)+
  facet_grid(year~he)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")





ggplot(filter(data2,asset_id=="EGC1",date==max(date),he=="15"))+
  geom_line(aes(percentile,price,color="Data"),fill=NA,size=1.5)+
  geom_line(aes(percentile,pred,color="Fixed Effects Model"),fill=NA,size=1.5)

+
  facet_grid(~Plant_Type)+
  scale_x_continuous(expand=c(0,0),limits = c(0,100), breaks = c(0,25,50,75,100),labels = c(0,25,50,75,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual("",values=colors_tableau10())+
  theme_bw()+theme(
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=paste("Offered Generation (% of Capacity)"),y="Price ($/MWh)",
       title=paste("Observed and Estimated Average Offer Blocks",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")
set_png(file="average_offers.png",width=1400,height = 850)
averages
dev.off()


test3<-by_type %>% 
  unnest(glanced, .drop = TRUE)


library(ggpubr)
ggerrorplot(test, x = "percentile", y = "estimate",
            desc_stat = "mean_ci", color = "Plant_Type")+
facet_grid(~policy)


#ggplot(filter(test,term=="net", Plant_Type%in% c("COAL","COGEN")))+
ggplot(filter(test,Plant_Type %in% c("COAL","NGCC","SCGT")))+
  geom_point(aes(percentile,estimate,group=policy,color=policy),size=2)+
  geom_hline(aes(yintercept=0))+
  facet_grid(~Plant_Type)
scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values =colors_tableau10(),guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_bw()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x="Percentile of Capacity",y="Change in Average Offer ($/MWh per $ of carbon cost)",
       title="Impact of GHG Policies (2014-2019) on Average Offers of Capacity",
       #subtitle="Outline shows raw carbon tax cost, fill shows costs net OBA values",
       caption="Source: Estimated coefficients for net carbon cost on power price")


  



  mods <- by_type %>% map(~ lm(price ~ supply_cushion+hourly_renewables, data = .))
map2(mods, by_type, predict)


#playing with peak, off peak

#by(merit_aug, merit_aug$on_peak, summary)

#skim(merit_aug)


#trial #2


# Split into pieces, fit model to each piece
by_type <- merit_aug %>% filter(Plant_Type %in% c("COAL","SCGT","NGCC","COGEN")) %>%
  mutate(gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
         year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
         tight=(supply_cushion<=500),really_tight=(supply_cushion<200))%>%
  nest(-c(Plant_Type)) %>%
  #nest(-c(Plant_Type)) %>% 
  mutate(
    fit = map(data, ~ lm(price ~ 
                           poly(percentile, 3)+
                           poly(percentile, 3)*ctax_cost+
                           #asset_id+
                           #poly(percentile, 3)*oba_val+
                           poly(supply_cushion,2)+
                           poly(hourly_renewables,2)+
                           #on_peak+
                           #supply_cushion+
                           #supply_cushion*on_peak+
                           #day_ahead_forecasted_ail+
                           #forecasted_actual_ail_difference+
                           #total_import_capability+
                           #total_export_capability+
                           poly(nit_settle_cad_gj,3)+
                           net+tight+really_tight+
                           year_fac
                         , data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    #augmented = map(fit, augment)
  )

test<-by_type %>% 
  unnest(tidied, .drop = TRUE) 

test3<-by_type %>% 
  unnest(glanced, .drop = TRUE)










merit_2018_07_11<-merit_aug %>% filter(date==as.Date("2018-07-11")&he=="04")%>% 
  mutate(oba_adj=case_when(Plant_Fuel=="COAL"~ (.800),
                           Plant_Fuel=="GAS"~ (.370),
                           TRUE~0),
         bid_adj=case_when(block_number==0~ price,
                           block_number>0~ price+.370*30-oba_adj*30)
  )%>% 
  group_by(date,he) %>% arrange(bid_adj,Plant_Type) %>% mutate(merit2=cumsum(size)) %>%
  ungroup()



#create a single-hour merit order file for analysis


day<-ymd("2018-04-26")
merit_day<-get_merit_report(as.Date(day), as.Date(day)+days(1),key_firms=c(""))
singles<-seq(1,9)
for(hour in singles){
  merit_day$he[merit_day$he==hour]<-paste(0,hour,sep="")
  #  merit_AS_data$he[merit_AS_data$he==hour]<-paste(0,hour,sep="")
}
merit_day$import_export<-as.character(merit_day$import_export)
merit_day<-merit_day %>% 
  mutate(
    import_export=case_when(
        is.na(import_export) ~ "",
        TRUE~  import_export
)
) %>% clean_merit_trade() 




load("renew_vols.RData")

#merge renewable gen 
merit_day<-merit_day%>%
  left_join(renew_vols,by=c("date","he","asset_id")) %>%
  mutate(
    to=ifelse(!is.na(renew_gen),renew_gen,to), #use the renewable gen if you can
    size=ifelse(!is.na(renew_gen),renew_gen,size), #use the renewable gen if you can
    available_mw=ifelse(!is.na(renew_gen),renew_gen,available_mw), #use the renewable gen if you can
    dispatched_mw=ifelse(!is.na(renew_gen),renew_gen,dispatched_mw), #use the renewable gen if you can
  ) 

renew_vols<-NULL #drop it out of memory

#3 build hourly summary data
load("forecast_data.RData")


#load(file="metered_vols_data.Rdata" ) 

load(file="aeso_itc_data.Rdata" ) 


hourly_summary<-merit_day%>%
  group_by(date,he) %>% summarize(hourly_avail=sum(available_mw),
                                  hourly_dispatch=sum(available_mw*(dispatched=="Y")),
                                  hourly_imports=sum(available_mw*(import_export=="I")),
                                  hourly_exports=sum(available_mw*(import_export=="E")),
                                  hourly_renewables=sum(renew_gen,na.rm = T),
  ) %>%ungroup()


merit_day<-merit_day %>% 
  left_join(itc_data,by=c("date","he")) %>% left_join(forecast_data,by=c("date","he")) %>%
  assign_peaks(time_var = time)%>%
  left_join(hourly_summary,by=c("date","he")) 

merit_day<-merit_day %>% left_join(plant_data(),by=c("asset_id"="ID"))




merit_day_graph<-merit_day %>% filter(he=="18",import_export!="E")%>% 
  mutate(oba_adj=case_when(Plant_Fuel=="COAL"~ (.800),
                           Plant_Fuel=="GAS"~ (.370),
                           TRUE~0),
         bid_adj=case_when(block_number==0~ price,
                           Plant_Type=="IMPORT"~price,
                           block_number>0~ price-co2_est/1000*30+oba_adj*30)
  )%>% 
  group_by(date,he) %>% arrange(bid_adj,Plant_Type) %>% mutate(merit2=cumsum(size)) %>%
  ungroup()



set_png("merit_oba_ctax.png")
ggplot(arrange(merit_day_graph,bid_adj,Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=bid_adj,fill=Plant_Type))+
  geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=price,colour=Plant_Type),fill=NA)+
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
  #geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
  #geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
  geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3,10)])+  
  scale_colour_manual("",values=colors_tableau10()[c(8,2,1,4,5,6,7,9,3,10)])+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  #scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,9500))+
  scale_y_continuous(expand=c(0,0),limits=c(-20,350))+
  ajl_line()+theme(plot.subtitle = element_text(size = 14))+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       title=paste("Alberta Energy Merit Order by Plant Type, ",max(merit_day_graph$date)," ",max(as.character(merit_day_graph$he)),":00",sep = ""),
       subtitle="Difference between solid fill and outline is net cost of carbon pricing net 370kg/MWh output-based allocation.",
       caption="Source: AESO Data, graph by Andrew Leach.")

dev.off()





#use bid data


# Split into pieces, fit model to each piece
by_type <- bids_aug %>%
  mutate(gas2=nit_settle_cad_gj^2,gas3=nit_settle_cad_gj^3,
         year_fac=as.factor(year), yearmonth=interaction(month_fac,year_fac),
         tight=(supply_cushion<=500),really_tight=(supply_cushion<200))%>%
  #nest(-c(Plant_Type,percentile)) %>%
  nest(-c(Plant_Type)) %>% 
  mutate(
    fit = map(data, ~ lm(bid_50 ~ 
                           #+p2+p3+p4+
                           poly(supply_cushion,2)+
                           poly(hourly_renewables,2)+
                           net+ctax_cost+
                           #percentile+
                           #on_peak+
                           #supply_cushion+
                           #supply_cushion*on_peak+
                           #day_ahead_forecasted_ail+
                           #forecasted_actual_ail_difference+
                           #total_import_capability+
                           #total_export_capability+
                           nit_settle_cad_gj
                           +gas2+gas3+
                           tight+really_tight#+
                         #yearmonth
                         , data = .x)),
    #pred=map(fit,predict),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    #augmented = map(fit, augment)
  )

test<-by_type %>% 
  unnest(tidied, .drop = TRUE) 

test2<-by_type %>% 
  unnest(glanced, .drop = TRUE) 


test4<-by_type %>% 
  unnest(pred, .drop = TRUE) %>% select(pred)%>% cbind(merit_fossils) %>% group_by(Plant_Type,percentile)%>%
  summarize(price=mean(price),pred=mean(pred))


# use half the iris data
ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))

library(dummies)


df5<-filter(merit_fossils,year>=2017,asset_id=="KH3")
df6<-filter(merit_fossils,year==2020,asset_id=="KH3",price>750) 

test<-sample_n(ungroup(df5), 20000,replace=T)
test<-test %>% select(price,year,percentile,supply_cushion,hourly_renewables,hourly_imports,nit_settle_cad_gj)

test<-test %>% select(price,year,percentile,hourly_renewables,hourly_imports)
test<-cbind(test, dummy(test$year))

  
targets<-test %>% select(-price,-year)
goal<-test %>% select(price) %>% mutate(price=price/1000)
#ir1 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,
#            decay = 5e-4, maxit = 200)

ir1 <- nnet(targets, goal, size = 20 , rang = 0.8,
            decay = 5e-4, maxit = 2000)

test<-test %>% cbind(ir1$fitted.values) 
names(test)[length(names(test))]<-"fitted_price"
test<-test %>% cbind(ir1$residuals) 
names(test)[length(names(test))]<-"residuals"

ggplot(test)+geom_point(aes(percentile,residuals))

ggplot(test %>% group_by(percentile,year) %>% summarize(price=mean(price),fitted_price=mean(fitted_price)))+
  geom_line(aes(percentile,price,group=as.factor(year),colour=as.factor(year),linetype="Data"))+
  geom_line(aes(percentile,fitted_price*1000,group=as.factor(year),color=as.factor(year),
                linetype="ML Estimate"))




test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}
test.cl(targets[-samp,], predict(ir1, ir[-samp,]))



#Duration curves
load("renew_vols.RData")
renew_vols<-renew_vols %>% filter(year(date)>=2012) %>% mutate(Year=year(date))
renew_vols <- renew_vols %>% group_by(Year,date,he) %>% summarise(renew_gen=sum(renew_gen))
# Histogram of Generation Densities
set_png(file="wind_cdf.png")
ggplot(filter(renew_vols,Year<2019),aes(renew_gen))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=as.factor(Year),colour=as.factor(Year)),size=1.5)+
  scale_x_continuous(limits=range(renew_vols$renew_gen),expand=c(0,0))+
  scale_y_continuous()+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Wind Generation (MW)",y="Share of time wind power less than X MW",
       title="Cumulative Density Function, Wind Energy (2010-2018 Avg)",
       caption="Source: AESO Data, Accessed via NRGStream, Graph by Andrew Leach")
dev.off()


#Duration curves

hourly_summary<-merit_aug%>%
  group_by(date,he) %>% summarize(hourly_avail=sum(available_mw*(import_export=="")),
                                  hourly_dispatch=sum(dispatched_mw*(import_export=="")),
                                  hourly_imports=sum(dispatched_mw*(import_export=="I")),
                                  hourly_exports=sum(dispatched_mw*(import_export=="E")),
                                  ) %>%
          ungroup() %>% mutate(Year=year(date),net_imports=hourly_imports-hourly_exports,
                               supply_cushion=hourly_avail-hourly_dispatch)



supply_cushion<-hourly_summary %>% mutate(Month=month(date)) %>%
  group_by(Month,Year) %>% arrange(supply_cushion) %>%
  summarize(hours=max(row_number()),n_250=sum((supply_cushion<=250)),
            n_500=sum((supply_cushion<=500)),date=min(date)+days(14))

ggplot(data=supply_cushion) +
  geom_col(aes(date,n_500),size=1.5,fill="grey50",colour="grey50") +
  geom_col(aes(date,n_250),size=1.5,fill="black",colour="black") +
  
  scale_linetype_manual("",values = c(3,1))+
  scale_x_date(date_breaks = "6 months",labels = date_format("%b\n%Y", tz="America/Denver"),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  slide_theme()+theme(legend.key.width=unit(4,"line"))+
  labs(y="Number of hours with tight supply cushion",x="",
       title="Monthly Incidents of Tight Supply Cushions, 2012-2019)",
       caption="Source: AESO Data, Graph by Andrew Leach")


#geom_col(aes(date,n_250,linetype="Supply Cushion < 250 MW"),size=1.5,colour="grey50") +
#  geom_col(aes(date,n_500,linetype="Supply Cushion < 500 MW"),size=1.5,colour="black") +
  

# Histogram of Exports
set_png(file="exports_cdf.png")
ggplot(filter(hourly_summary,Year<2019),aes(hourly_exports))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=as.factor(Year),colour=as.factor(Year)),size=1.5)+
  scale_x_continuous(limits=range(renew_vols$renew_gen),expand=c(0,0))+
  scale_y_continuous()+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Exports (MW)",y="Share of time exports less than X MW",
       title="Cumulative Density Function, Hourly Exports (2010-2018 Avg)",
       caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()



# Histogram of Net Imports
set_png(file="net_imports_cdf.png")
ggplot(filter(hourly_summary,Year<2019),aes(hourly_imports))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=as.factor(Year),colour=as.factor(Year)),size=1.5)+
  scale_x_continuous(limits=range(renew_vols$renew_gen),expand=c(0,0))+
  scale_y_continuous()+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Imports (MW)",y="Share of time exports less than X MW",
       title="Cumulative Density Function, Hourly Imports (2010-2018 Avg)",
       caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()


# Histogram of Generation Densities
set_png(file="net_imports_cdf.png")
ggplot(filter(hourly_summary,Year<2019),aes(net_imports))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=as.factor(Year),colour=as.factor(Year)),size=1.5)+
  scale_x_continuous(limits=range(renew_vols$renew_gen),expand=c(0,0))+
  scale_y_continuous()+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Net Imports (MW)",y="Share of time net imports less than X MW",
       title="Cumulative Density Function, Hourly Net Imports (2010-2018 Avg)",
       caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()



# Histogram of Generation Densities
set_png(file="suuply_cushion_cdf.png")
ggplot(filter(hourly_summary,Year<2019),aes(supply_cushion))+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  stat_ecdf(geom = "step",aes(group=as.factor(Year),colour=as.factor(Year)),size=1.5)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous()+
  scale_color_viridis("",discrete=TRUE)+
  ajl_line()+
  labs(x="Imports (MW)",y="Share of time imports less than X MW",
       title="Cumulative Density Function, Hourly Imports (2010-2018 Avg)",
       caption="Source: AESO Data, Graph by Andrew Leach")
dev.off()




