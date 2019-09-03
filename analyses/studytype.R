## Started 7 July 2016 ##
## By Cat and Lizzie##
## Edited by Cat - 15 September 2017

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
} else
setwd("~/Documents/git/ospree/analyses")
ospree <- read.csv("output/ospree_clean.csv", header=TRUE)
ospree <- ospree[ospree$woody=="yes",]

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


xx<-dplyr::select(xx, datasetID, study, genus, species, prov.lat, prov.long, field.sample, force, photo, chill, chilltime, spp)
xx<-xx[!duplicated(xx),]

#write.csv(xx, file="~/Documents/git/ospree/analyses/output/studytype_table.csv", row.names = FALSE)
