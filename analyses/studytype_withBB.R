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
} else
  setwd("~/Documents/git/ospree/analyses")
ospree <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
xx<-ospree

xx <- within(xx, { prov.lat <- ave(provenance.lat, datasetID, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { field.sample <- ave(fieldsample.date, datasetID, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, datasetID, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, datasetID, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chill <- ave(Total_Chilling_Hours, datasetID, species, FUN=function(x) length(unique(x)))}) # mult Total_Chilling_Hours
xx <- within(xx, { spp <- ave(species, datasetID, FUN=function(x) length(unique(x)))}) # mult species
xx <- within(xx, { prov.long <- ave(provenance.long, datasetID, species, FUN=function(x) length(unique(x)))}) # multiple provenance.longs

xx<-dplyr::select(xx, datasetID, study, prov.lat, prov.long, field.sample, force, photo, chill, spp)

write.csv(xx, file="~/Documents/git/ospree/analyses/output/studytype_withBB.csv", row.names = FALSE)
