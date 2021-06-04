#merit_day<-filter(merit_data,date==ymd("2018-04-18"))
#save(merit_day,file="merit_day.RData")
#load(file="merit_day.RData")

#testing data set creation code
start_time<-Sys.time()
iter<-100
for (i in c(1:iter)) {
  merit_aug<-merit_day%>% select(date,he,asset_id,from,to,size,price,offer_sum)%>% group_by(date,he,asset_id)%>% arrange(date,he,asset_id,price) %>%
    mutate(bid_capacity=max(to))%>%
    summarize( #summarizing alone takes .13 seconds
      #offer_control=as.character(offer_sum)[1],
      #price=price[1],
      #adding this line takes about .2 seconds
      from=list(from/bid_capacity*100),price=list(price),
      #step_func=list(bid_func(from/bid_capacity*100,price)),
      #total_mw=mean(bid_capacity
    )%>% group_by(date,he,asset_id) %>%
    
    
    
    mutate(step_func=list(bid_func(from[[1]],price[[1]])))%>% select(-from,-price) %>%
    #bids=map(step_func,~.x(c(10,25,40,55,70,85,100))))  %>%
    ungroup()
  merit_aug$bids<-merit_aug$step_func[[2]](c(10,25,40,55,70,85,100))
  
}
paste("Average elapsed time is",time_length(interval(start_time, Sys.time())/iter, "seconds"),"seconds")

merit_aug$step_func[[1]](2)

start_time<-Sys.time()
for (i in c(1:2500)) {
  test_func=list(bid_func(seq(0,100,15),seq(0,100,150)))
  #print(i)
}
#start_time<-Sys.time()
paste("Elapsed time is",time_length(interval(start_time, Sys.time()), "seconds"),"seconds")

