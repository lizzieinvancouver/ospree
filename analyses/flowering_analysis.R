###Dan is investigating if there is any flowering data to work with in OSPREEE on June 19, 2017
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)

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
flo3<-filter(ds, respvar.simple=="flowernumber")
unique(flo3$genus) #1
dim(flo3) #163

###doubtful flowering data is useful for any kind of analysis##############################
currant<-filter(ds1, genus=="Ribes")
table(currant$respvar.simple)
table(currant$datasetID)
table(currant$multiresp)
currant<-filter(currant, multiresp==FALSE)
resp<-as.vector(currant$respvar.simple)
condition1<-c("daystobudburst","daystoflower")
bb <- filter(currant, resp %in% condition1)
table(bb$respvar.simple)
table(bb$datasetID)
table(bb$respvar)


#bb$response.time <- as.numeric(bb$response.time)
bb$forcetemp <- as.numeric(bb$forcetemp)
bb$photoperiod_day <- as.numeric(bb$photoperiod_day) ####need to fill in the ambient with climate
bb$chilldays <- as.numeric(bb$pchilldays)


flo_bb<-subset(bb,respvar=="daystoanthesis")
leaf_bb<-subset(bb,respvar=="daystobudburst")

######## % bb and time
table(currant$respvar.simple)

resp<-as.vector(currant$respvar.simple)
condition<-c("percentflower","percentbudburst")
perc <- filter(currant, resp %in% condition)
table(perc$respvar.simple)
table(perc$photoperiod_day)
table(perc$forcetemp)
table(perc$chilldays)

flo_perc<-subset(perc,respvar.simple=="percentflower") ###is resp.var simple really accurant?
leaf_perc<-subset(perc,respvar=="percentbudburst")
