

t<-20
supply_1<-function(x,tax=0){x^2+tax}

#y=x^2+t -> sqrt(y-t)=x

supply_2<-function(x,tax=0){.5*x^2+tax}
#y=.5x^2+t -> sqrt((y-t)/.5)=x



supply_mkt<-function(x,tax=0){x^2/(1+sqrt(2))^2+tax}
#x_mkt= sqrt(y-t)+sqrt((y-t)/.5)
#x_mkt=(1+sqrt(2))*sqrt(y-t)
#x/(1+sqrt(2))



#set up mock two-firm merit with and without ctax
#ifelse(row_number()==1, 0, time2)

d_1=tibble(size=c(200,100,100,0),
           price=c(0,200,500,500),
           ghgs=.3,
           firm="Firm 1"
           )


d_2=tibble(size=c(100,60,60,0),
           price=c(0,50,400,400),
           ghgs=1,
           firm="Firm 2")


d<-bind_rows(d_1,d_2)%>% mutate(scenario="Base",ctax=0)%>%
  bind_rows(bind_rows(d_1,d_2) %>% mutate(scenario="Low tax",ctax=50))%>%
  bind_rows(bind_rows(d_1,d_2) %>% mutate(scenario="High tax",ctax=180))%>%
  group_by(firm,scenario)%>%
  mutate(price=ifelse(row_number()==1, 0,price+ghgs*ctax))%>%
  mutate(
  start=ifelse(row_number()==1, 0,lag(cumsum(size))),                    
  end=cumsum(size))%>%ungroup()%>%
  group_by(scenario)%>%
  arrange(price)%>%
  mutate(merit_start=ifelse(row_number()==1, 0,lag(cumsum(size))),                    
  merit_end=cumsum(size))%>%
  arrange(scenario,merit_start)%>%
  mutate(scenario=as_factor(scenario),
         scenario=fct_relevel(scenario,"Base","Low tax","High tax"),
         )
  
  
ggplot(d) +
  geom_step(aes(y=price,x=merit_start/max(merit_start)*100,group=scenario,color=scenario,size=firm))+
  geom_point(data=tibble(x=c(55,55,55),y=c(50,100,230)),aes(x=x,y=y),size=5)+
  geom_point(data=tibble(x=c(80,80,80),y=c(400,450,554)),aes(x=x,y=y),size=5)+
geom_vline(aes(xintercept=55))+
  annotate("text",x=(80+55)/2,y=590,label="Illustrative sampling levels\nand points",vjust=1,size=5)+
  geom_segment(x=(80+55)/2,y=600,xend=55.5,yend=600,
               colour = "black", 
               arrow = arrow())+
  geom_segment(x=(80+55)/2,y=600,xend=79.5,yend=600,
               colour = "black", 
               arrow = arrow())+
  
  geom_vline(aes(xintercept=80))+
  scale_linetype_manual("",values=c("solid","11","32"))+
  scale_color_manual("",values=c("black","grey50","grey80"))+
  scale_size_manual("",values=c(1.5,2.5,3))+
  #guides(color="none")+
  scale_y_continuous(expand = c(0,0),breaks=pretty_breaks())+
  scale_x_continuous(expand = c(0,0),breaks=pretty_breaks())+
  expand_limits(y=620)+
  paper_theme()+
  theme(panel.spacing = unit(2,"lines"),
        #axis.text.y = element_blank()
        NULL)+
  labs(x="Percentile of offered power (%)",y=expression('Price'*phantom(1)*'($/MWh)'),
       NULL
  )+
  NULL  
ggsave("images/sampling.png",dpi=300,width=14,height=7)
  
ggplot() +
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  expand_limits(x=c(0,20))+
  expand_limits(y=c(0,250))+
  geom_function(fun = supply_1,args=list(tax=0),aes(color="firm 1",lty="no tax"))+
  geom_function(fun = supply_2,args=list(tax=0),aes(color="firm 2",lty="no tax"))+
  geom_function(fun = supply_mkt,args=list(tax=0),aes(color="market",lty="no tax"))+
  geom_function(fun = supply_1,args=list(tax=30),aes(color="firm 1",lty="tax"))+
  geom_function(fun = supply_2,args=list(tax=30),aes(color="firm 2",lty="tax"))+
  geom_function(fun = supply_mkt,args=list(tax=30),aes(color="market",lty="tax"))
  





geom_function(fun=supply_1(x))+
  geom_point(size=1.2)+
  facet_wrap(~facility_name_ab,nrow = 2)+
  scale_x_continuous(breaks = pretty_breaks(n=5))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 8, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions Compliance Cost'*phantom(10)*'($/MWh)'),
       title="GHG Compliance Obligation, Alberta Coal Power Plants (2007-2019)",
       subtitle=paste("Average annual net emissions credit value or compliance cost per unit of electricity production for plants with capacity greater than ",gas_cut," MW" ,sep=""),
       caption=str_wrap("Source: Alberta Environment SGER and CCIR compliance data (2007-2019). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("coal_plant_compliance_cost.png",width = 16,dpi=150)


library(numDeriv)
# NOT RUN {
sc2.f <- function(x){
  n <- length(x)
  sum((1:n) * (exp(x) - x)) / n
}

sc2.g <- function(x){
  n <- length(x)
  (1:n) * (exp(x) - 1) / n
}

sc2.test <- function(x,a=2){
  n <- length(x)
  sum((1:n) * (a*exp(x) - x)) / n
}


x0 <- rnorm(5)
hess <- hessian(func=sc2.f, x=x0)
hessc <- hessian(func=sc2.f, x=x0, "complex")
all.equal(hess, hessc, tolerance = .Machine$double.eps)


x0 <- rnorm(5)
hess <- hessian(func=sc2.test, x=x0,a=3)
hessc <- hessian(func=sc2.test, x=x0, "complex")
all.equal(hess, hessc, tolerance = .Machine$double.eps)




#  Hessian = Jacobian of the gradient
jac  <- jacobian(func=sc2.g, x=x0)
jacc <- jacobian(func=sc2.g, x=x0, "complex")
all.equal(hess, jac, tolerance = .Machine$double.eps)
all.equal(hessc, jacc, tolerance = .Machine$double.eps)
# }

