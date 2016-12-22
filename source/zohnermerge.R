rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()
library(dplyr)
library(tidyr)
setwd("~/Documents/git/ospree/analyses/input")
m<-read.csv("zohner_merge.csv", header = TRUE)
m<-subset(m,Source=="This study")
m<-separate(m,species,into=c("genus","species"))
m<-rename(m,DatasetID=Source)
m<-gather(m,photoperiod.day,response.time,degree.days.8h.day.length:degree.days.16h.day.length)
m$photoperiod.day[m$photoperiod.day=="degree.days.8h.day.length"] <- 8
m$photoperiod.day[m$photoperiod.day=="degree.days.16h.day.length"] <- 16
m$photoperiod.night<-NA
m$photoperiod.day<-as.numeric(m$photoperiod.day)
sapply(m, mode)
m$photoperiod.night<-(24-m$photoperiod.day)
m$DatasetID[m$DatasetID=="This study"] <- "zohner16"
View(m)
