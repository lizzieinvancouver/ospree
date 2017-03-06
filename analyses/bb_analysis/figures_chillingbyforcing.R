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
setwd("~/Documents/git/ospree/analyses/bb_analysis")
source("cleaning/clean_thermaltimetodays.R")

# Create new forcing unit
dbb$resp = as.numeric(as.character(dbb$response.time))
dbb$force = as.numeric(as.character(dbb$forcetemp))
dbb$force_units <- dbb$resp * dbb$force
dbb<- dbb %>%
  filter(force_units >= 0)
#dbb<- dbb[which(dbb$force_units >= 0),]

# Now plot force_units~chilling units
ggplot((dbb), aes(x=force_units, y=Total_Chill_portions)) + xlab("Forcing Units") + ylab("Total Chill Portions") +
  geom_point(aes(col=genus)) + theme(legend.position="none")
ggplot((dbb), aes(x=force_units, y=Total_Chill_portions)) + xlab("Forcing Units") + ylab("Total Chill Portions") +
  geom_point(aes(col=datasetID))

# Introduce Study Type
study<- read.csv("output/studytype.table.csv")
study$total <- rowSums(study > 1) # study where multiple predictors were manipulated
study$total<-as.numeric(study$total - 1)
study<-study[which(study$total>=0),]
df<- full_join(dbb, study)
# study number indicates how many factors a study manipulated - 
# e.g. asby62 had different sampling dates, latitudes, and photoperiods so has a value of 3 for studytype
ggplot((df), aes(x=force_units, y=Total_Chill_portions)) + xlab("Forcing Units") + ylab("Total Chill Portions") +
  geom_point(aes(col=factor(total)))

### Work on thermaltime figures - really ugly, not enough info to show meaningful relationship
dtt<-subset(d,respvar.simple=="thermaltime") # basler12, karlsson03, laube14a, and skuterud94
dtt$chill = as.numeric(as.character(dtt$Total_Chill_portions)) # only skuterud94 has chilling units
dtt$photo = as.numeric(as.character(dtt$photoperiod_day))
dtt$thermal = as.numeric(as.character(dtt$response.time)) # excludes skuterud and laube14a
dtt$lat = as.numeric(as.character(dtt$provenance.lat))
# unable to convert basler12, karlsson03, laube14a, and skuterud94 to daystobudburst

#dtt<-dtt[which(dtt$chill!="NA"),]
#dtt<-dtt[which(dtt$photo!="NA"),]
#dtt<-dtt[which(dtt$thermal!="NA"),]
#dtt<-dtt[which(dtt$lat!="NA"),]

# Not enough data to show any sort of interesting relationship...
ggplot((dtt), aes(x=thermal, y=chill)) + xlab("Thermal Time") + ylab("Total Chill Portions") + 
  geom_point()
ggplot((dtt), aes(x=photo, y=thermal)) + xlab("Photoperiod") + ylab("Thermal Time") + geom_point(aes(col=datasetID))
ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Latitude") + ylab("Thermal Time") + geom_point(aes(col=datasetID))


ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Latitude") + ylab("Thermal Time") + geom_point(aes(col=photo)) 
  scale_y_continuous(limits=c(0,50))

ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Latitude") + ylab("Thermal Time") + geom_point(aes(col=factor(chill)))
  scale_y_continuous(limits=c(0,50))

