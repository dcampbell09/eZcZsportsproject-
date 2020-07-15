``
#import data
getwd()
setwd("/Users/dcampbell/Desktop/eZcZ Data Project/")
rookie_data <- read.csv("Fangraphs Leaderboard.csv")
rookie_data
``

``
#find different interests
rookie_wRC_abv <- subset(rookie_data,(rookie_data$wRC.>100))
rookie_wRC_abv

``
#create histogram
hist(rookie_wRC_abv$wRC., freq = TRUE,
     main= "Rookies wRC+",
     xlab="wRC+",
     ylab="Frequency",
     col="blue")
```
setwd("/Users/dcampbell/Desktop/eZcZ Data Project/")
library(tidyverse)
library(ggplot2)
library(PerformanceAnalytics)
#import secondary data 

``
fg_data <- read.csv("FranGraphsSecondaryData.csv")
fg_data

``
#select column regarding player ID 
player_data <- fg_data%>%arrange(Name,Season)
player_data$seas_ind <- 1

``
#filter to show 1 & 2 year player data only
player_data <- player_data%>%group_by(Name)%>%mutate(cs = cumsum(seas_ind))
one_two_yr_players <- player_data%>%filter(cs %in% c(1,2))

``
#create percentages as numeric 
one_two_yr_players$BB. <- as.numeric(sub("%","", one_two_yr_players$BB.))
one_two_yr_players$K. <- as.numeric(sub("%","", one_two_yr_players$K.))

``
#find strictly rookie data
rookie_data <- one_two_yr_players%>%filter(cs %in% c(1))
rookie_data
mean(rookie_data$ISO)

``
#find strictly sophomore data 
sophomore_data <- one_two_yr_players%>%filter(cs %in% c(2))
sophomore_data 
mean(sophomore_data$ISO)

``
#ggplot scatter plot rookie data
rookie_data%>%ggplot(aes(x=wRC.,y=BABIP))+geom_point()+geom_smooth()+theme_bw()

#ggplot scatter plot sophomore data
sophomore_data%>%ggplot(aes(x=wRC.,y=BABIP))+geom_point()+geom_smooth()+theme_bw()

#ggplot histrogram rookie data
ggplot(data=rookie_data)+
  geom_histogram(mapping = 
                   aes(x= wRC.))

#ggplot histogram sophomore data
ggplot(data=sophomore_data)+
  geom_hist(mapping = 
              aes(y = wRC.))



