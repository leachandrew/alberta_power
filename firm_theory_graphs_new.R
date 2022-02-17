library(fOptions)
#using plot3d and plot3Drgl packages, color palette previously created using viridis
library(plot3D)
library(plot3Drgl)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(patchwork)
library(scales)
library(directlabels)

class_theme<-function(){
  theme_classic()+
    theme(
      plot.subtitle = element_text(color="grey10",size=rel(.7)),
      plot.title = element_text(face="bold",size=rel(.8)),
      plot.caption = element_text(color="grey50",size=rel(.5),hjust=0),
      legend.title = element_text(color="grey10",size=rel(.5)),
      legend.text = element_text(color="grey10",size=rel(.5)),
      axis.text = element_text(size=rel(1.5)),
      axis.title = element_text(size=rel(1.5)),
      panel.grid.major = element_line(size = .2,colour = "grey60"), 
      #plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"),
      plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
      #axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      NULL)
}



class_clean<-function(){
  theme_classic()+
    theme(
      plot.subtitle = element_text(color="grey10",size=rel(.7)),
      plot.title = element_text(face="bold",size=rel(.8)),
      plot.caption = element_text(color="grey50",size=rel(.5),hjust=0),
      legend.title = element_text(color="grey10",size=rel(.5)),
      legend.text = element_text(color="grey10",size=rel(.5)),
      axis.text = element_text(size=rel(1.5)),
      axis.title = element_text(size=rel(1.5)),
      #panel.grid.major = element_line(size=0,colour = "black"), 
      #plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"),
      plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
      #axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      NULL)
}



#u of a palette

colors_ua10 <- function()
{
  #return(c("#007C41", "#FFDB05", "#7D9AAA", "#CA7700", "#165788", "#A8B400",
  #         "#E0D760", "#404545", "#8D3C1E", "#004250"))
  c("#007C41", "#FFDB05", "#7D9AAA","#165788","#404545","#8D3C1E","#3CB6CE")
}


#produciton inputs map


#basic cost stuff


library(ggrepel)
iq_points<-tribble(
  ~"letter", ~x, ~y,
  "A", 1, 4,
  "B", 2, 2,
  "C", 4, 1,
  "D", 2, 4,
  "E", 1, 2
)

iq_8=function(x){8/x}
iq_4=function(x){4/x}
iq_2=function(x){2/x}

ggplot(data.frame(x=c(0,5)), aes(x=x))+
  stat_function(fun=iq_4, geom="line", size=2, color = "blue")+
  geom_label(x=4.5, y=iq_4(4.5), label=expression(q==2.00), color = "blue")+
  geom_point(data = subset(iq_points, letter %in% c("A","B","C")),
             aes(x=x,y=y), size =3)+
  geom_text_repel(data = subset(iq_points, letter %in% c("A","B","C")),
                  aes(x=x,y=y, label=letter), seed = 2,size =4)+
  scale_x_continuous(breaks=seq(0,5,1),
                     limits=c(0,5),
                     expand=expand_scale(mult=c(0,0.1)))+
  scale_y_continuous(breaks=seq(0,5,1),
                     limits=c(0,5),
                     expand=expand_scale(mult=c(0,0.1)))+
  #scale_colour_manual("Curves", values = line_colors, guide=F)+
  labs(x = "Labor",
       y = "Capital")+
  class_theme()



cd_iso<- function(A=1,k,l,alpha=0.5,beta=0.5,q_fix=10) q_fix^(1/alpha)*A^(-1/alpha)*l^(-beta/alpha) 
prod<- function(A=1,k,l,alpha=0.5,beta=0.5,q_fix=10) A*k^alpha*l^(beta) 


df=tibble(l=seq(.01,10,.01))

ggplot(df)+
  geom_line(aes(l,cd_iso(alpha=0.6,beta=0.6,l=l,q_fix=prod(k=4,l=4))), size=2, color = "red")+
  geom_line(aes(l,cd_iso(alpha=0.6,beta=0.6,l=l,q_fix=prod(k=2,l=2))), size=2, color = "blue")+
  geom_line(aes(l,cd_iso(alpha=0.6,beta=0.6,l=l,q_fix=prod(k=1,l=1))), size=2, color = "green")+
  
  geom_label(x=4.5,y=cd_iso(alpha=0.6,beta=0.6,l=4.5,q_fix=prod(k=4,l=4)), color = "red", label=paste("q=",prod(k=4,l=4)), size = 6)+
  geom_label(x=4.5,y=cd_iso(alpha=0.6,beta=0.6,l=4.5,q_fix=prod(k=2,l=2)), color = "blue", label=paste("q=",prod(k=2,l=2)), size = 6)+
  geom_label(x=4.5,y=cd_iso(alpha=0.6,beta=0.6,l=4.5,q_fix=prod(k=1,l=1)), color = "green", label=paste("q=",prod(k=1,l=1)), size = 6)+
  
  
  
  
  scale_x_continuous(breaks=seq(0,5,1),
                     limits=c(0,5),
                     expand=c(0,0))+
  scale_y_continuous(breaks=seq(0,5,1),
                     limits=c(0,5),
                     expand=c(0,0))+
  #scale_colour_manual("Curves", values = line_colors, guide=F)+
  labs(x = "Labour",
       y = "Capital")+
  class_theme()



#simple production function

f <- function(A=1,k,l,alpha=0.5,beta=0.5) A*k^alpha*l^beta

#build a grid
k<- seq(0,10,0.5)
l<- seq(0,10,0.5)

prod<-tibble()
for(k_val in k){
  for(l_val in l){
  prod<-rbind(prod,tibble(k_val,l_val))
  }
}




prod<-prod %>% mutate(prod_val=f(k=k_val,l=l_val))

#solve utility max problem and then graph it above and below

p_x<-1
p_y<-1
income<-10

budget <- function(x,y) (income-p_x*x-p_y*y)


z<-z %>% mutate(x_cons=income/p_x-y_val*p_y,
                util_cons=U(x_cons,y_val),
                )



isocost<-function(cost,l,w=5,r=5){cost/r-w/r*l}


min_iso_cost<-function(a_sent=1,alpha_sent=.5,beta_sent=.5,
											 prod_level,
											 w=5,r=5){
	#testing
	#prod_level=6
	#beta_sent<-.5
	#alpha_sent<-.5
	#k_val<-2
	#a_sent<-1
	#w<-5
	#r<-5
	#FOC MPl/w=mpk/r
	temp_prod<-function(a=a_sent,alpha=alpha_sent,beta=beta_sent,k,l){
		a*l^alpha*k^beta
	}
	
	foc<-function(k_val){
		l_derived<-l_der(a=a_sent,k=k_val,prod_sent=prod_level,alpha=alpha_sent,beta=beta_sent)
		(beta_sent*temp_prod(k=k_val,l=l_derived)/l_derived)/(alpha_sent*temp_prod(k=k_val,l=l_derived)/k_val)-w/r
	}
	
	k_min<-uniroot(foc,c(0.1,50),tol = 10^-6)[[1]]
	#print("capital")
	#print(k_min)
	l_derived<-l_der(a=a_sent,k=k_min,prod_sent=prod_level,alpha=alpha_sent,beta=beta_sent)
	#print("labour")
	#l_derived
	#print("production")
	#temp_prod(k=k_min,l=l_derived)
	cost_level<-w*l_der(a=a_sent,k=k_min,prod_sent=prod_level,alpha=alpha_sent,beta=beta_sent)+r*k_min
	#print("cost")
	#print(cost_level)
	tibble_row(min_cost=cost_level,k_min,l_min=l_derived)
}



#Cobb-Douglas isoqant map
production_map<-function(a_sent=1,alpha_sent=.5,beta_sent=.5,
												 x_label="L, Labour per unit time",
												 y_label="K, Capital per unit time",
												 prod_targ=c(2,4,6,8,10),
												 k_max=10,l_max=10,
												 w=5,r=5){
	
	#testing
	##l_max<-10
	#k_max<-10
	#a_sent<-1
	#x_label<-"L, Labour per unit time"
	#y_label<-"K, Capital per unit time"
	#alpha_sent<-.5
	#beta_sent<-.5
	#prod_targ=c(2,4)
	
	#get the cost_min values
	min_vals<-tibble(prod=prod_targ) %>% group_by(prod)%>%
		mutate(cost=min_iso_cost(a_sent,alpha_sent,beta_sent,prod_level=prod,w,r))
	
	
	min_vals<-min_vals%>% bind_cols(min_vals$cost)%>%select(-cost)
	
	print("min_vals")
	
	print(min_vals)
	
	#build a grid
	k_val<- seq(k_max/1000,k_max,k_max/1000)
	
	z <- tibble(prod=prod_targ) %>% group_by(prod)%>%
		expand(prod,k_val)%>%
		 mutate(
		l_val=(prod/a_sent/k_val^alpha_sent)^(1/beta_sent)
	)%>% filter(l_val<=l_max)%>%
		group_by(prod)%>%mutate(label = ifelse(l_val == max(l_val),paste("Y=",prod,sep=""),NA))%>%
		ungroup()%>%
		left_join(min_vals)
		
	
	
	
	base_plot<- ggplot(z)+
		geom_line(aes(y=k_val,x=l_val,group=prod),size=1.5,color="dodgerblue")+
		geom_label_repel(aes(y=k_val,x=l_val,group=prod,label=label),nudge_x = l_max*.01)+
		
		class_clean() + 
		#coord_equal()+
		
		theme(#legend.position = "bottom",
			#axis.text.x = element_text(angle=90,hjust = .5,vjust=.5 ),
			panel.grid.major =element_line(colour = "grey60",linetype="dotted",size=.5),
			#panel.grid.minor =element_line(colour = "black",linetype="dashed",size=.1),
			strip.text = element_text(size=rel(.7) )
		)+
		#annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
		scale_x_continuous(breaks = pretty_breaks(),limits = c(0,k_max*1.1),expand = c(0,0))+
		scale_y_continuous(breaks = pretty_breaks(),limits = c(0,l_max),expand = c(0,0))+
		expand_limits(x=l_max+1)+
		expand_limits(y = 0)+ 
		scale_colour_manual("",values = c("grey30"))+
		scale_fill_manual("",values = c("grey70"))+
		scale_shape_manual("",values = c(20))+
		labs(y=y_label,x=x_label,
				 #title="Indifference map",
				 #subtitle= expression(Utility~"function"~is~U~"="~sqrt(XY)),
				 #caption="Source: Statistics Canada Table 32-10-0056-01, Balance sheet of the agricultural sector as at December 31st, graph by Andrew Leach",
				 NULL)
	
			base_plot+
				geom_point(aes(l_min,k_min),size=3)
	  	
}

l_der<-function(a,k,prod_sent,alpha,beta) {(prod_sent/a/k^alpha)^(1/beta)}





production_map(prod_targ = c(2,4))+
	stat_function(fun=isocost,args = list(cost=20), geom="line", color="red", size=1)+
	geom_label(aes(x=4,y=0.25),label="Cost=20",color="red")+
	stat_function(fun=isocost,args = list(cost=30), geom="line", color="red", size=1)+
	geom_label(aes(x=6,y=0.25),label="Cost=30",color="red")+
	stat_function(fun=isocost,args = list(cost=40), geom="line", color="red", size=1)+
	geom_label(aes(x=8,y=0.25),label="Cost=40",color="red")+
	geom_point(aes(x=3,y=3),size=3,colour="black")+
	geom_text(aes(x=3,y=3),label="b",color="black",nudge_x = .25)+
	geom_text(aes(x=2,y=2),label="a",color="black",nudge_x = .25)+
	geom_text(aes(x=4,y=4),label="c",color="black",nudge_x = .25)+
	geom_point(aes(x=5,y=4),size=3,colour="black")+
	geom_text(aes(x=5,y=4),label="d",color="black",nudge_x = .25)+
	geom_point(aes(x=.55,y=7.45),size=3,colour="black")+
	geom_text(aes(x=.55,y=7.45),label="e",color="black",nudge_x = .25)
	
ggsave("isocost_quiz.png",dpi = 300,width = 10,height = 10)



	min_iso_cost(prod_level=2,color_sent = "dodgerblue")+
	min_iso_cost(prod_level=2,color_sent = "dodgerblue")+
	min_iso_cost(prod_level=4,color_sent = "red")	


	
U <- function(x,y) (x*y)^.5
utility_map(5,5,U,u_vals = c(2,4,6,8,10),x_shift = -.35,y_shift = .25)

#solve for all possible combinations of x and y that give 5, 10, 15, 20?



#problem set
U <- function(x,y) (x*y+y)
utility_map(5,300,U,u_vals = c(300,400,500),x_shift = -.35,y_shift = .25,
						x_label="Days of skiing",
						y_label="Consumption of All Other Goods")+
	scale_colour_manual("",values = colors_ua10())


U <- function(x,y) (x*y+y)
utility_map(5,300,U,u_vals = c(300,400,500),x_shift = -.35,y_shift = .25,
						x_label="Days of skiing",
						y_label="Consumption of All Other Goods")+
	geom_segment(aes(x = 0, y = 300, xend = 3, yend = 0, colour = "Without card"),size=2)+
	scale_colour_manual("",values = colors_ua10())








## 3D plot generation
#3d <- scatter3D(x = rescaled_SN$Views, y = rescaled_SN$Publications, z = rescaled_SN$Citations, colvar = as.numeric(rescaled_SN$cluster), col = scheme, col.key = list(col.clab = "Cluster"), xlab = "Views", ylab = "Publications", zlab = "Citations", plot = TRUE)


z<-runif.halton(200000,2)%>%as_tibble()%>% select(k_val=V1,l_val=V2)%>% mutate(k_val=k_val*10,l_val=l_val*100,prod=f(k=k_val,l=l_val))


plt_3d<- scatter3D(x=z$k_val,y=z$l_val,z=z$prod,xlab="Capital (k)",ylab="Labour (l)",zlab="Production (Y)")

plotrgl()


z<-z %>% mutate(mod=prod%%5)%>%filter(mod<0.05,prod>5)%>% add_row(k_val = 0, l_val = 0, prod=0,mod=0)
plt_3d<- scatter3D(x=z$k_val,y=z$l_val,z=z$prod,xlab="Capital (k)",ylab="Labour (l)",zlab="Production (Y)")

plotrgl()



#isocost and isoquants





#cost curves


#f <- function(A=1,k,l,alpha=0.5,beta=0.5) A*k^alpha*l^beta


w<-15 #wage rate

r<-7 #capital rental rate




	MC<-function(x){20*x+200}
	AC<-function(x){10*x+200+(1000/x)}
	AVC<-function(x){10*x+200}
	Demand<-function(x){500-5*x}
	MR<-function(x){500-10*x}
	
	
	ggplot(data.frame(x=c(0,12)), aes(x=x))+
		stat_function(fun=MC, geom="line", color="red", size=1)+
		geom_label(x=5,y=MC(5), color = "red", label="MC(q)")+
		stat_function(fun=AC, geom="line", color="orange", size=1)+
		geom_label(x=2.5,y=AC(2.5), color = "orange", label="AC(q)")+
		stat_function(fun=AVC, geom="line", color="brown", size=1)+
		geom_label(x=5,y=AVC(5), color = "brown", label="AVC(q)")+
		stat_function(fun=Demand, geom="line", color="blue", size=1)+
		geom_label(x=18,y=Demand(18), color = "blue", label="Demand")+
		stat_function(fun=MR, geom="line", color="purple", size=1)+
		geom_label(x=18,y=MR(18), color = "purple", label="MR(q)")+
		geom_segment(aes(x=10,y=0),xend=10,yend=400,linetype=3, size=2)+
		
		scale_x_continuous(breaks=seq(0,20,2),
											 limits=c(0,20),
											 expand=c(0,0))+
		scale_y_continuous(breaks=seq(0,700,100),
											 limits=c(0,700),
											 expand=c(0,0),
											 labels=scales::dollar)+
		labs(x = "Quantity (q)",
				 y = "Price (p)")+
		class_clean()


	
	max_q<-10
	map<-c(0,max_q)
	
	
	
	#total cost is TC=AX^2+BX+FC
	TC_a<-5
	TC_b<-40
	FC_SR<-10
	FC<-FC_SR+40
	
	D_int<-100
	D_slope<-10
	
	TC<-function(x){FC+TC_a*x^2+TC_b*x}
	MC<-function(x){2*TC_a*x+TC_b}
	AC<-function(x){TC(x)/x}
	AVC<-function(x){(TC(x)-FC+FC_SR)/x}
	D <-function(x){D_int-D_slope*x}
	MR<-function(x){D_int-D_slope*2*x}
	
	mono_q<-uniroot(function(z) MC(z) - MR(z),c(0,50))[[1]]
	#mono_q
	mono_p<-D(mono_q)
	paste("Monopoly quanntity is",mono_q,"at price",mono_p)
	comp_q<-uniroot(function(z) D(z) - MC(z),c(0,50))[[1]]
	comp_p<-D(comp_q)
	#comp_q
	paste("Competitive quanntity is",comp_q,"at price",comp_p)
	
	g_1<-Hmisc::bezier(x = map,y=D(map)) %>% as_tibble() %>% rename(q=x,p_dem=y)%>%
		mutate(rev=D(q)*q,
					 cost=TC(q),
					 mc=MC(q),
					 ac=AC(q),
					 avc=AVC(q),
					 MR=MR(q)
					 )
	
	ggplot(g_1, aes(x=q))+
		stat_function(fun=MC, geom="line", color="red", size=1.5)+
		geom_label(x=5,y=AVC(8), color = "red", label="MC(q)")+
		
		stat_function(fun=AC, geom="line", color="orange", size=1.5)+
		geom_label(x=7,y=AVC(8), color = "orange", label="AC(q)")+
		
		stat_function(fun=AVC, geom="line", color="brown", size=1.5)+
		geom_label(x=9,y=AVC(8), color = "brown", label="AVC(q)")+
		
		stat_function(fun=D, geom="line", color="blue", size=1.5)+
		geom_label(x=max_q-1,y=D(max_q-1), color = "blue", label="Demand")+
		
		stat_function(fun=MR, geom="line", color="purple", size=1.5)+
		geom_label(x=max_q/2,y=MR(max_q/2-1), color = "purple", label="MR(q)")+
		
		#monopoly prices, quantity and segments
		geom_point(aes(x=mono_q,y=mono_p),color="black",size=2.5)+
		#geom_label(aes(x=mono_q,y=mono_p), color = "black", label="a",nudge_y = 6)+
		
		geom_label_repel(data = data.frame(x=mono_q, y=mono_p),aes(x,y),label = 'a',nudge_y = 6)+
		
		
		
		geom_point(aes(x=mono_q,y=MR(mono_q)),color="black",size=2.5)+
		#geom_label(aes(x=mono_q,y=MR(mono_q)), color = "black", label="b",nudge_y = 6,nudge_x = .25)+
		geom_label_repel(data = data.frame(x=mono_q, y=MR(mono_q)),aes(x,y),label = 'b',nudge_x = .35,nudge_y = 0)+
		
		geom_point(aes(x=comp_q,y=comp_p),color="black",size=2.5)+
		geom_label_repel(data = data.frame(x=comp_q, y=comp_p),aes(x,y),label = 'c',nudge_x = .35,nudge_y = 0)+
		
		
		#vert
		geom_segment(aes(x = mono_q, y = mono_p, xend = mono_q, yend = 0), lty = "dashed",size=.5) +
		geom_segment(aes(x = comp_q, y = comp_p, xend = comp_q, yend = 0), lty = "dashed",size=.5) +
		
		#horiz
		geom_segment(aes(x = 0, y = mono_p, xend = mono_q, yend = mono_p), lty = "dashed",size=.5) +
		geom_segment(aes(x = 0, y = comp_p, xend = comp_q, yend = comp_p), lty = "dashed",size=.5) +
		
		
		#geom_segment(aes(x=10,y=0),xend=10,yend=400,linetype=3, size=2)+
		scale_x_continuous(breaks=pretty_breaks(),
											 limits=c(0,max_q),
											 expand=c(0,0))+
		scale_y_continuous(breaks=pretty_breaks(),
  										 limits=c(0,D(0)*1.1),
											 expand=c(0,0),
											 labels=scales::dollar)+
		labs(x = "Quantity (q)",
				 y = "Price (p)")+
		class_theme()
		#coord_equal()
	ggsave("cost_min.png",dpi=450,width = 10,height = 10)



	
	
base<-ggplot(g_1, aes(x=q))+
		stat_function(fun=MC, geom="line", color="red", size=1.5)+
		geom_label(x=5,y=MC(5), color = "red", label="MC(q)")+
		
		stat_function(fun=AC, geom="line", color="orange", size=1.5)+
		geom_label(x=7,y=AVC(8), color = "orange", label="AC(q)")+
		
		stat_function(fun=AVC, geom="line", color="brown", size=1.5)+
		geom_label(x=9,y=AVC(8), color = "brown", label="AVC(q)")+
		
		stat_function(fun=D, geom="line", color="blue", size=1.5)+
		geom_label(x=max_q-1,y=D(max_q-1), color = "blue", label="Demand")+
		
		
	
	#comp_equil
		geom_point(aes(x=comp_q,y=comp_p),color="black",size=2.5)+
		geom_label_repel(data = data.frame(x=comp_q, y=comp_p),aes(x,y),label = 'a',nudge_x = .35,nudge_y = 0)+
	
	#vert
	geom_segment(aes(x = comp_q, y = comp_p, xend = comp_q, yend = 0), lty = "dashed",size=.5) +
	
	#horiz
	geom_segment(aes(x = 0, y = comp_p, xend = comp_q, yend = comp_p), lty = "dashed",size=.5) +
	
	
	
	 	
		#geom_segment(aes(x=10,y=0),xend=10,yend=400,linetype=3, size=2)+
		scale_x_continuous(breaks=pretty_breaks(),
											 limits=c(0,max_q),
											 expand=c(0,0))+
		scale_y_continuous(breaks=pretty_breaks(),
											 limits=c(0,D(0)*1.1),
											 expand=c(0,0),
											 labels=scales::dollar)+
		labs(x = "Quantity (q)",
				 y = "Price (p)")+
		class_theme()
	#coord_equal()



monop<-base+
	#monopoly elements
	stat_function(fun=MR, geom="line", color="purple", size=1.5)+
	geom_label(x=max_q/2,y=MR(max_q/2-1), color = "purple", label="MR(q)")+
	
	#monopoly prices, quantity and segments
	geom_point(aes(x=mono_q,y=mono_p),color="black",size=2.5)+
	#geom_label(aes(x=mono_q,y=mono_p), color = "black", label="a",nudge_y = 6)+
	
	geom_label_repel(data = data.frame(x=mono_q, y=mono_p),aes(x,y),label = 'c',nudge_y = 6)+
	
	geom_point(aes(x=mono_q,y=MR(mono_q)),color="black",size=2.5)+
	#geom_label(aes(x=mono_q,y=MR(mono_q)), color = "black", label="b",nudge_y = 6,nudge_x = .25)+
	geom_label_repel(data = data.frame(x=mono_q, y=MR(mono_q)),aes(x,y),label = 'b',nudge_x = .35,nudge_y = 0)+
	
	#vert
	geom_segment(aes(x = mono_q, y = mono_p, xend = mono_q, yend = 0), lty = "dashed",size=.5) +
	
	#horiz
	geom_segment(aes(x = 0, y = mono_p, xend = mono_q, yend = mono_p), lty = "dashed",size=.5)
	
	


base
ggsave("mkt_base.png",dpi=450,width = 10,height = 10)
	
monop
ggsave("mkt_monop.png",dpi=450,width = 10,height = 10)

	
	