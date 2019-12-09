## Started 7 December 2019 ##
## By Lizzie ##

## Looking at correlations in PEP data ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/Github/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")

library(plyr)
library(dplyr)

# data
temp <- read.csv("output/dailyclim/temp_observed_48.16447_11.50293_1979_2014.csv")

chill <- read.csv("output/dailyclim/chill_observed_48.16447_11.50293_1979_2014.csv")

# code
onet <- subset(temp, Lat=="48.16447" & Long=="11.50293" & Month>2 & Month <6)
tempsumm <-
      ddply(onet, c("Year"), summarise,
      tmean = mean(Tmean))

df <- merge(tempsumm, chill, by.x="Year", by.y="End_year")

plot(tmean~Utah_Model, df, xlab="Utah Chilling", ylab="March-May Temperature", pch=16, col="skyblue")
abline(lm(tmean~Utah_Model, df))
summary(lm(tmean~Utah_Model, df))
