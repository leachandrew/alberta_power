
#aeso intertie availability reports

#http://itc.aeso.ca/itc/public/queryHistoricalIntertieReport.do

#<form name="historicalIntertieReportForm" method="POST" action="/itc/public/queryHistoricalIntertieReport.do">
#  <input type="hidden" name="availableEffectiveDate" value="943279200000 1999-11-22 07:00:00 MST (1999-11-22 14:00:00 GMT)">
#  <input type="hidden" name="availableExpiryDate" value="1582354800000 2020-02-22 00:00:00 MST (2020-02-22 07:00:00 GMT)">
#<input type="text" name="startDate" value="2000-01-01" id="startDate">


library(httr)
library(tidyverse)
library(lubridate)
start_date<-ymd("2000-01-01")
end_date<-ymd("2000-01-31")
query = list(
  startDate = format(start_date, "%Y-%m-%d"),
  endDate = format(end_date, "%Y-%m-%d"),
  fileFormat = "csv",
  availableEffectiveDate="943279200000+1999-11-22+07%3A00%3A00+MST+%281999-11-22+14%3A00%3A00+GMT%29",
  availableExpiryDate="1582354800000+2020-02-22+00%3A00%3A00+MST+%282020-02-22+07%3A00%3A00+GMT%29"
  )

res <- POST("http://itc.aeso.ca/itc/public/historicalIntertieReport.do", body = query,
           write_disk("doc2.txt",overwrite = T))


#http://itc.aeso.ca/itc/public/historicalIntertieReport.do?fileFormat=CSV&startDate=2000-01-01&endDate=2000-01-31

library(stringi)
res <- POST("http://itc.aeso.ca/itc/public/queryHistoricalIntertieReport.do", body = query)
stop_for_status(res)
test<- content(res, as="text") %>% 
  stri_split_lines() %>% 
  flatten_chr()







test<-paste(test[3:length(test)], collapse="\n")
merit_data<-read.csv(text=test,header = TRUE, stringsAsFactors=FALSE)
clean_data<-janitor::clean_names(merit_data) %>% 
  tbl_df()
#key_firms<-firms()
#if(!is.null(key_firms))
#{
#  clean_data$key_firm<-grepl(paste(key_firms, collapse="|"), clean_data$offer_control)
#  clean_data$offer_sum<-ifelse(clean_data$key_firm,clean_data$offer_control,"Other")
#  for(firm in key_firms)
#    clean_data$offer_sum[grep(firm,clean_data$offer_sum)]<-firm
#  clean_data$offer_sum<-ordered(clean_data$offer_sum,levels=c(key_firms,"Other"))
#}
#if(is.null(key_firms))
#  clean_data$offer_sum<-clean_data$offer_control
clean_data$date<-mdy(clean_data$date)
return(clean_data)
}


login <- list(
  email = "login",
  password = "password"
  submit = "Login!"
)
res <- POST("http://kenpom.com/handlers/login_handler.php", body = login, encode = "form", verbose())
team <- GET("http://kenpom.com/team.php?team=Rice", verbose())





A slightly more modern approach is to use the httr package:
  
  library(httr)


sc2 <- function (..., .cookies = character(0)) {
  
  cookies <- c(..., .cookies)
  stopifnot(is.character(cookies))
  cookies_str <- vapply(cookies, RCurl::curlEscape, FUN.VALUE = character(1))
  cookie <- paste(names(cookies), cookies_str, sep = "=", 
                  collapse = ";")
  config(cookie = cookie)
  
}




start_date<-ymd("2000-01-01")
end_date<-ymd("2000-01-31")


#http://itc.aeso.ca/itc/public/queryHistoricalIntertieReport.do?availableEffectiveDate=943279200000+1999-11-22+07%3A00%3A00+MST+%281999-11-22+14%3A00%3A00+GMT%29&availableExpiryDate=1582354800000+2020-02-22+00%3A00%3A00+MST+%282020-02-22+07%3A00%3A00+GMT%29&fileFormat=CSV&startDate=2000-01-01&endDate=2000-01-31

res <- GET("http://itc.aeso.ca/itc/public/queryHistoricalIntertieReport.do",
            #body=list(`form[startDate]` = "2001-01-01", 
            #          `form[endDate]` = "2001-01-31", 
            #          `form[fileFormat]` = "csv"),
            body=list(
              startDate = format(start_date, "%Y-%m-%d"),
              endDate = format(end_date, "%Y-%m-%d"),
              fileFormat = "csv" ),
            write_disk("doc2.txt",overwrite = T))


cookie_set=c("JSESSIONID"="voByJDSDYHyVxLXcuZ0eIFlCUmfk-hlE72Ag6HKeUlP2qoox1_jt!1966326938")

cookie_set<-c(`JSESSIONID` = "voByJDSDYHyVxLXcuZ0eIFlCUmfk-hlE72Ag6HKeUlP2qoox1_jt!1966326938")

res <- POST("http://itc.aeso.ca/itc/public/queryHistoricalIntertieReport.do",
            #body=list(`form[startDate]` = "2001-01-01", 
            #          `form[endDate]` = "2001-01-31", 
            #          `form[fileFormat]` = "csv"),
            body=list(
              startDate = format(start_date, "%Y-%m-%d"),
              endDate = format(end_date, "%Y-%m-%d"),
              fileFormat = "csv" ),set_cookies(cookie_set),
            write_disk("doc2.txt",overwrite = T))

handle <- handle("http://itc.aeso.ca/itc/public/historicalIntertieReport.do")

cookies(res)
set_cookies("MeWant" = "cookies")
getwd()


list(
  startDate = format(start_date, "%Y-%m-%d"),
  endDate = format(end_date, "%Y-%m-%d"),
  fileFormat = "html",
  availableEffectiveDate="943279200000+1999-11-22+07%3A00%3A00+MST+%281999-11-22+14%3A00%3A00+GMT%29",
  availableExpiryDate="1582354800000+2020-02-22+00%3A00%3A00+MST+%282020-02-22+07%3A00%3A00+GMT%29"
)

