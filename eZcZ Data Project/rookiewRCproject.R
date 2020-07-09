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




