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

#FORECASTS
if(!exists("get_forecast_report", mode="function")) source("aeso_scrapes.R")

#seasons
if(!exists("getSeason", mode="function")) source("get_season.R")

library(keras)     # Neural Networks
library(tidyverse) # Data cleaning / Visualization
library(knitr)     # Table printing
library(rmarkdown) # Misc. output utilities 
library(ggridges)  # Visualization




allObservations %>% 
  mutate(recording_length = map_int(data,nrow)) %>% 
  ggplot(aes(x = recording_length, y = activityName)) +
  geom_density_ridges(alpha = 0.8)