###compare real data to fake data for opsree
###Started by Dan on 24 July 2017

rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)
library(dplyr)
setwd("~/Documents/git/ospree/analyses/output")

# Setting working directory.

d<-read.csv("ospree_clean_withchill_BB.csv")
View(table(d$genus))
bet<-filter(d, genus=="Betula")
fag<-filter(d,genus=="Fagus")
table(fag$species)
table(bet$species)
rib<-filter(d,genus=="Ribes")
rib<-filter(rib,species=="nigrum")
table(rib$datasetID)
pic<-filter(d, genus=="Picea")
pic<-filter(pic, species=="abies")
table(pic$datasetID)
###picea abies, ribes nigrum, betula pendula

### Picea
condition1<-c("daystobudburst","percentbudburst")
pic <- filter(pic, respvar.simple %in% condition1)

pic$forcetemp <- as.numeric(pic$forcetemp)
pic$photoperiod_day <- as.numeric(pic$photoperiod_day)
pic$Total_Utah_Model<-as.numeric(pic$Total_Utah_Model)
pic$response.time <- as.numeric(pic$response.time)
pic$provenance.lat<-as.numeric(pic$provenance.lat)

pic.2way<-pic.3way<-lm(response.time~forcetemp+photoperiod_day+Total_Utah_Model+forcetemp:provenance.lat+photoperiod_day:provenance.lat+Total_Utah_Model:provenance.lat+forcetemp:photoperiod_day+forcetemp:Total_Utah_Model+photoperiod_day:Total_Utah_Model,data=pic)
summary(pic.2way)
pic.3way<-lm(response.time~forcetemp+photoperiod_day+Total_Utah_Model+forcetemp:provenance.lat+photoperiod_day:provenance.lat+Total_Utah_Model:provenance.lat+forcetemp:photoperiod_day+forcetemp:Total_Utah_Model+photoperiod_day:Total_Utah_Model+forcetemp:photoperiod_day:provenance.lat+forcetemp:Total_Utah_Model:provenance.lat+photoperiod_day:Total_Utah_Model:provenance.lat, data=pic)

summary(pic.3way)

pic.mod1<-lm(response.time~forcetemp+photoperiod_day+Total_Chilling_Hours, data=pic)
summary(pic.mod1)

