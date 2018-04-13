###what does the photoperiod response curve look like?
## look at viheraaarnio06
#partenen98
#schnabel87
#howe95
# read in data
#plot responsetime ~ photoperiod
#see if it is linear
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse)
library(ggplot2)
setwd("~/Documents/git/ospree/analyses")


bb.all<- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)


bb.some <- subset(bb.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(bb.some, response.time!="")


colnames(bbdat)
#To Use
# heide93
#ashby62
#cafarra11b


###plots
hei<-subset(bbdat,datasetID=="heide93a"& study=="exp3")
hei<-subset(hei,species=="sylvatica")
hei$days_cent<-hei$response.time/mean(hei$response.time)
ggplot(hei,aes(as.numeric(photoperiod_day),days_cent))+geom_line(aes(color=population))

hei$thingy[hei$population=="As.Norway"]<-9
hei$thingy[hei$population=="As.Norway (assuming)"]<-10
hei$thingy[hei$population=="Basel"]<-11
hei$thingy[hei$population=="Carpathia Mtns"]<-12
hei$thingy[hei$population=="Copenhagen"]<-13


ash1<-subset(bbdat,datasetID=="ashby62"&population=="Southwestern Michigan")
ash2<-subset(bbdat,datasetID=="ashby62"&population=="Central Wisconsin")
sh1<-subset(ash1,response.time<999)
ash2<-subset(ash2,response.time<999)
#ggplot(ash,(aes(as.numeric(photoperiod_day),response.time)))+geom_line(aes(group=population,color=fieldsample.date,))

ash1$thingy[ash1$fieldsample.date=="11-Dec-1956"]<-"Mich1"
ash1$thingy[ash1$fieldsample.date=="8-Jan-1957"]<-"Mich2" 
ash1$thingy[ash1$fieldsample.date=="5-Feb-1957"]<-"Mich3"   
ash1$thingy[ash1$fieldsample.date=="4-Mar-1957"]<-"Mich4"
ash2$thingy[ash2$fieldsample.date=="11-Dec-1956"]<-"Wisc1"
ash2$thingy[ash2$fieldsample.date=="8-Jan-1957"]<-"Wisc2" 
ash2$thingy[ash2$fieldsample.date=="5-Feb-1957"]<-"Wisc3"   
ash2$thingy[ash2$fieldsample.date=="4-Mar-1957"]<-"Wisc4"  
ash<-rbind(ash2,ash1)
ash$days_cent<-ash$response.time/mean(ash$response.time)
ggplot(ash,(aes(as.numeric(photoperiod_day),days_cent)))+geom_line(aes(color=datasetID,group=thingy))

caf<-subset(bbdat,datasetID=="caffarra11b"&study=="exp2"& response.time!=999.0)
table(caf$photoperiod_day)
table(caf$forcetemp)
caf$thingy[caf$chilldays==0]<-5
caf$thingy[caf$chilldays==30]<-6
caf$thingy[caf$chilldays==55]<-7
caf$thingy[caf$chilldays==95]<-8

caf$days_cent<-caf$response.time/mean(caf$response.time)
ggplot(ash,(aes(as.numeric(photoperiod_day),response.time)))+geom_line(aes(color=thingy))+ggtitle("Ashby62, Tilia americana")
ggplot(hei,aes(as.numeric(photoperiod_day),response.time))+geom_line(aes(color=population))+ggtitle("Heide93a,Fagus sylvatica")
ggplot(caf,(aes(as.numeric(photoperiod_day),response.time)))+geom_line(aes(color=chilldays))+ggtitle("caffarra11b, Betula pubescens")

ha<-rbind(caf,hei,ash)
  ggplot(ha,(aes(as.numeric(photoperiod_day),days_cent)))+geom_line(aes(color=datasetID,group=thingy))+theme_classic()+xlab("Photoperiod")+ylab("Days to budburst (scaled)")

  