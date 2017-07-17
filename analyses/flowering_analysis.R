###Dan is investigating if there is any flowering data to work with in OSPREEE on June 19, 2017
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)
library(dplyr)

# Setting working directory.
setwd("~/Documents/git/ospree/analyses/output")

###data
ds1<- read.csv("ospree_clean_withchill.csv", header=TRUE)
ds<- read.csv("ospree_clean.csv", header=TRUE)

###which responses deal with flowers?
unique(ds1$respvar)
unique(ds1$respvar.simple )
table(ds1$respvar.simple)

####check out "daystoflower"
flo<-filter(ds1, respvar.simple=="daystoflower")
dim(flo) ##nrow 180
table(flo$genus) ##only 3
table(flo$datasetID) ##only 4

###checl out "percectflower"
flo2<-filter(ds1, respvar.simple=="percentflower")
unique(flo2$datasetID) #only 4
unique(flo2$genus) #only 2
dim(flo2) ## 610and ~500 are from one study

####last hope "flowernumber"
flo3<-filter(ds1, respvar.simple=="flowernumber")
unique(flo3$genus) #1
dim(flo3) #163

phen<- filter(ds1, respvar.simple=="phenstage")
table(phen$respvar)
unique(phen$respvar)

###doubtful flowering data is useful for any kind of analysis############################## 
###Try it with just Ribes

currant<-filter(ds1, genus=="Ribes")
table(currant$respvar.simple)
table(currant$datasetID)
table(currant$multiresp)
currant<-filter(currant, multiresp==FALSE)
resp<-as.vector(currant$respvar.simple)

#extract latitude for photoperiod
#sonsteby14 60 40 N
#jones12

###filter for phenology
condition1<-c("daystobudburst","daystoflower")
bb <- filter(currant, resp %in% condition1)
table(bb$respvar.simple)
table(bb$datasetID)
table(bb$respvar)
table(bb$photoperiod_day)
table(bb$forcetemp)
table(bb$chilldays)
#### until I figure out how to deal with clinate
bb<-filter(bb, photoperiod_day!="ambient")

bb$response.time <- as.numeric(bb$response.time)
bb$forcetemp <- as.numeric(bb$forcetemp)
bb$photoperiod_day <- as.integer(bb$photoperiod_day)
bb$chilldays <- as.numeric(bb$chilldays)


flo_bb<-subset(bb,respvar=="daystoanthesis")
leaf_bb<-subset(bb,respvar=="daystobudburst")


mod1<-lm(response.time~forcetemp+chilldays, data=leaf_bb)
summary(mod1) ### it seems there aren't enoguh levels of photoperiod

mod2<-lm(response.time~forcetemp+chilldays, data=flo_bb)
summary(mod2)

######## % bb and time
table(currant$respvar.simple)

#Filter for % budburst
resp<-as.vector(currant$respvar.simple)
condition<-c("percentflower","percentbudburst")
perc <- filter(currant, resp %in% condition)
table(perc$respvar.simple)
table(perc$photoperiod_day)
table(perc$forcetemp)
table(perc$chilldays)
table(perc$datasetID)

#### until i figure out how to work with climate data
perc<-filter(perc, photoperiod_day!="ambient")

perc$response.time <- as.numeric(perc$response.time)
perc$forcetemp <- as.numeric(perc$forcetemp)
perc$photoperiod_day <- as.numeric(perc$photoperiod_day)
perc$chilldays <- as.numeric(perc$chilldays)
perc$response <- as.numeric(perc$response)

flo_perc<-subset(perc,respvar.simple=="percentflower") ###is resp.var simple really accurant?
leaf_perc<-subset(perc,respvar=="percentbudburst")

mod<-lm(response~response.time+response.time:forcetemp+response.time:chilldays+response.time:photoperiod_day, data=flo_perc)
summary(mod) ### not enough levels of photoperiod or response time.
