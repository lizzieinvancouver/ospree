## Attempt to plot raw data looking at the effects of chilling units
# vs. forcing units (forcing temp by days to budburst) and assess by 
# number of species, experiment type, and labgroup (colorcode)

## Cat - 3 Feb 2017

# Clear Workspace
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)

## read data
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
setwd("~/Documents/git/ospree/analyses")
d<-read.csv("output/ospree_clean_withchill.csv",as.is=TRUE)

## First create a simple forcing unit
dbb<-subset(d,respvar.simple=="daystobudburst")

#dataset <- unique(dbb$datasetID)
dbb$resp = as.numeric(as.character(dbb$response.time))
dbb$force = as.numeric(as.character(dbb$forcetemp))
dbb$force_units <- dbb$resp * dbb$force
dbb<- dbb %>%
  filter(force_units >= 0)
#dbb<- dbb[which(dbb$force_units >= 0),]

# Now plot force_units~chilling units
ggplot((dbb), aes(x=force_units, y=Total_Chill_portions)) + xlab("Forcing Units") + ylab("Total Chill Portions") +
  geom_point(aes(col=species)) + theme(legend.position="none")

# Introduce Study Type
study<- read.csv("output/studytype.table.csv")
study$total <- rowSums(study > 1) # study where multiple predictors were manipulated
study$total<-study$total - 1
study<-study[which(study$total>=0),]
df<- full_join(dbb, study)
ggplot((df), aes(x=force_units, y=Total_Chill_portions)) + xlab("Forcing Units") + ylab("Total Chill Portions") +
  geom_point(aes(col=total)) 

######### Work on thermaltime ##############
dtt<-subset(d,respvar.simple=="thermaltime")
dtt$chill = as.numeric(as.character(dtt$Total_Chill_portions))
dtt$photo = as.numeric(as.character(dtt$photoperiod_day))
dtt$thermal = as.numeric(as.character(dtt$response.time))
dtt$lat = as.numeric(as.character(dtt$provenance.lat))

dtt<-dtt[which(dtt$chill!="NA"),]
#dtt<-dtt[which(dtt$photo!="NA"),]
dtt<-dtt[which(dtt$thermal!="NA"),]
#dtt<-dtt[which(dtt$lat!="NA"),]

ggplot((dtt), aes(x=chill, y=thermal)) + xlab("Chilling Hours") + ylab("Thermal Time") + geom_point()
ggplot((dtt), aes(x=photo, y=thermal)) + xlab("Photoperiod") + ylab("Thermal Time") + geom_point()
ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Photoperiod") + ylab("Thermal Time") + geom_point(col=)

ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Photoperiod") + ylab("Thermal Time") + geom_point(aes(col=photo)) 
  scale_y_continuous(limits=c(0,50))

ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Photoperiod") + ylab("Thermal Time") + geom_point(aes(col=photo)) 
  scale_y_continuous(limits=c(0,50))


