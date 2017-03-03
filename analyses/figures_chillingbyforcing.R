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
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 
setwd("~/Documents/git/ospree/analyses")
d<-read.csv("output/ospree_clean_withchill.csv",as.is=TRUE)

## First create a simple forcing unit
d<-within(d, respvar.simple[datasetID=="ghelardini10" & respvar.simple=="thermaltime"]<-"daystobudburst")
d<-within(d, respvar.simple[datasetID=="heide93"]<-"daystobudburst")
dbb<-subset(d,respvar.simple=="daystobudburst")
# attempting to convert response.time to daystobudburst using two methods, both have same error
dbb$response.time[which(dbb$datasetID=="ghelardini10")] <-
    as.numeric(dbb$response[which(dbb$datasetID=="ghelardini10")])/as.numeric(dbb$forcetemp[which(dbb$datasetID=="ghelardini10")])
dbb$response.time[which(dbb$datasetID=="heide93")] <-
    as.numeric(dbb$response[which(dbb$datasetID=="heide93")])/as.numeric(dbb$forcetemp[which(dbb$datasetID=="heide93")])
# if you subset the dataframe down and make a new dataframe, then it will work... example below
dg<-filter(dbb, datasetID=='ghelardini10')
dg$response<-as.numeric(as.character(dg$response))
dg$forcetemp<-as.numeric(as.character(dg$forcetemp))
dg$response.time<-dg$response/dg$forcetemp

#dataset <- unique(dbb$datasetID)
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
ggplot((df), aes(x=force_units, y=Total_Chill_portions)) + xlab("Forcing Units") + ylab("Total Chill Portions") +
  geom_point(aes(col=factor(total)))

######### Work on thermaltime ############## really ugly right now, still working out
# able to fix ghelardini (see above for daystobudburst section) unsure whether to change in ospree and which cells to fix
dtt<-subset(d,respvar.simple=="thermaltime") # basler12, ghelardini10, heide93, karlsson03, laube14a, and skuterud94
dtt$chill = as.numeric(as.character(dtt$Total_Chill_portions)) # only heide93 and skuterud94 have chilling units
dtt$photo = as.numeric(as.character(dtt$photoperiod_day))
dtt$thermal = as.numeric(as.character(dtt$response.time)) # excludes skuterud and laube14a
dtt$lat = as.numeric(as.character(dtt$provenance.lat))

#dtt<-dtt[which(dtt$chill!="NA"),]
#dtt<-dtt[which(dtt$photo!="NA"),]
#dtt<-dtt[which(dtt$thermal!="NA"),]
#dtt<-dtt[which(dtt$lat!="NA"),]

ggplot((dtt), aes(x=chill, y=thermal)) + xlab("Chilling Hours") + ylab("Thermal Time") + geom_point(aes(col=datasetID))
ggplot((dtt), aes(x=photo, y=thermal)) + xlab("Photoperiod") + ylab("Thermal Time") + geom_point(aes(col=datasetID))
ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Latitude") + ylab("Thermal Time") + geom_point(aes(col=datasetID))


ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Latitude") + ylab("Thermal Time") + geom_point(aes(col=photo)) 
  scale_y_continuous(limits=c(0,50))

ggplot((dtt), aes(x=lat, y=thermal)) + xlab("Latitude") + ylab("Thermal Time") + geom_point(aes(col=factor(chill)))
  scale_y_continuous(limits=c(0,50))

