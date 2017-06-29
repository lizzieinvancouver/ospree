## Started 29 June 2017 - Cat

## An R script to organize data with multiple provenance latitudes studied and see what else is manipulated in the study

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set working directory

if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else
  setwd("~/Documents/git/ospree/analyses")
ospree<-read.csv("output/ospree_clean_withchill_bb.csv", header=TRUE)

## Find studies with multiple provenance latitudes
d<-ospree%>%
  dplyr::select(datasetID, genus, species, provenance.lat, provenance.long)%>%
  group_by(datasetID,provenance.lat)%>%
  arrange(datasetID)
d <- within(d, { count <- ave(provenance.lat, datasetID, species, FUN=function(x) length(unique(x)))})
d<- d[!duplicated(d), ]

d<-ungroup(d)
check<-d%>%dplyr::select(datasetID, count)
check<-check[!duplicated(check),]
check$total<-ifelse(check$count>=2, check$count, NA)
check<-na.omit(check)
check<-dplyr::select(check, -total)


## What else was manipulated?
datasets<-unique(check$datasetID)
xx<-ospree%>%
  filter(datasetID %in% datasets)%>%
  dplyr::select(datasetID, genus, species, provenance.lat, provenance.long, fieldsample.date, chilltemp, forcetemp, photoperiod_day, Total_Chilling_Hours, 
                growing.lat, growing.long)
xx <- within(xx, { prov <- ave(provenance.lat, datasetID, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { sample <- ave(fieldsample.date, datasetID, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, datasetID, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, datasetID, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chill <- ave(Total_Chilling_Hours, datasetID, species, FUN=function(x) length(unique(x)))}) # mult Total_Chilling_Hours
xx<-filter(xx, xx$prov>=2)
xx<-dplyr::select(xx, datasetID, genus, species, prov, sample, force, photo, chill)
xx<-xx[!duplicated(xx),]

write.csv(xx, file=("~/Documents/git/ospree/analyses/lat_analysis/lat_output/study_breakdown.csv"), row.names = FALSE)
