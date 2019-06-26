## Started 7 July 2016 ##
## Adjusted by Cat - 24 July 2017
## By Cat and Lizzie##

## An R script to organize the studies by experiment type for the bud burst data. ##
## Studies were organized by number of field sampling dates, photoperiods, ##
## forcing temperatures, and experimental chilling hours. ##

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
  
  } else
  
  setwd("~/Documents/git/ospree/analyses")
ospree <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE, na.strings = c("", "NA"))
xx<-ospree

xx[is.na(xx)] <- 0
xx <- within(xx, { prov.lat <- ifelse(provenance.lat!=0, ave(provenance.lat, datasetID, species, 
                                                             study, FUN=function(x) length(unique(x))), 0)}) # multiple provenance.lats
xx <- within(xx, { field.sample <- ifelse(fieldsample.date!=0, ave(fieldsample.date, datasetID, species, 
                                                                   study, FUN=function(x) length(unique(x))), 0)}) # mult fieldsample.date
xx <- within(xx, { force <- ifelse(forcetemp!=0, ave(forcetemp, datasetID, species, 
                                                     study, FUN=function(x) length(unique(x))), 0)}) # mult forcetemp
xx <- within(xx, { photo <- ifelse(photoperiod_day!=0, ave(photoperiod_day, datasetID, species, 
                                                           study, FUN=function(x) length(unique(x))), 0)}) # mult photoperiod_day
xx <- within(xx, { chill <- ifelse(chilltemp!=0, ave(chilltemp, datasetID, species, 
                                                 study, FUN=function(x) length(unique(x))), 0)}) # mult studychill
xx <- within(xx, { chilltime <- ifelse(chilldays!=0, ave(chilldays, datasetID, species, 
                                                     study, FUN=function(x) length(unique(x))), 0)}) # mult studychill
xx <- within(xx, { spp <- ifelse(species!=0, ave(species, datasetID,
                                                       study,FUN=function(x) length(unique(x))), 0)}) # mult species
xx <- within(xx, { prov.long <- ifelse(provenance.long!=0, ave(provenance.long, datasetID, species, 
                                                               study, FUN=function(x) length(unique(x))), 0)}) # multiple provenance.longs

xx$fieldsample.date2<-as.Date(xx$fieldsample.date2, "%Y-%m-%d")
xx$field.doy<-yday(xx$fieldsample.date2)

d.sub<-xx%>%dplyr::select(datasetID, field.doy, species, study)
d.sub <- within(d.sub, { field.sample <- ave(field.doy, datasetID, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
d.sub<-d.sub[!duplicated(d.sub),]
d.sub<-subset(d.sub, field.sample>1)
d.sub<-d.sub[!is.na(d.sub$field.doy),]

d.sub$field.doy1<-ave(d.sub$field.doy, d.sub$datasetID, d.sub$species, FUN=min)
d.sub$field.doy2<-ave(d.sub$field.doy, d.sub$datasetID, d.sub$species, FUN=max)
d.sub$wein<-ifelse((d.sub$field.doy2-d.sub$field.doy1)>=14, 1, 0)
xx$wein<-NA
for(i in c(1:nrow(xx))) {
  for(j in c(1:nrow(d.sub)))
    if(xx$datasetID[i] == d.sub$datasetID[j] & xx$species[i] == d.sub$species[j] & xx$study[i]==d.sub$study[j])
      xx$wein[i]<-d.sub$wein[j]
}


xx<-dplyr::select(xx, datasetID, species, study, prov.lat, prov.long, field.sample, force, photo, chill, chilltime, spp, wein)
xx<-xx[!duplicated(xx),]
xx$wein<-ifelse(is.na(xx$wein), 0, xx$wein)

#write.csv(xx, file="~/Documents/git/ospree/analyses/output/studytype_withBB.csv", row.names = FALSE)
