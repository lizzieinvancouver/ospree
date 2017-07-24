###compare real data to fake data for opsree
###Started by Dan on 24 July 2017
###use with bb_testdata_generate.R
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)
library(dplyr)
setwd("~/Documents/git/ospree/analyses/output")

# Setting working directory.

realospree<-read.csv("ospree_clean_withchill_BB.csv")
unique(realospree$respvar.simple)
realospree<-filter(realospree,respvar.simple=="daystobudburst")

###now run bb_testdata_generate.R ---->use testdat
par(mfrow=c(2,2))
hist(as.numeric(realospree$forcetemp))
hist(testdat$force)

hist(as.numeric(realospree$photoperiod_day))
hist(testdat$photo)

par(mfrow=c(1,3))
hist(as.numeric(realospree$chilltemp))
hist(as.numeric(realospree$Total_Chill_portions))
hist(testdat$chill)

