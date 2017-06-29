## Started 20 June 2017 - Cat

## An R script to organize data with multiple provenance latitudes studied

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

## Find studies with multiple Latitudes
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


#write.csv(check, file="~/Documents/git/ospree/analyses/lat_analysis/lat_output/lat_studies.csv", row.names = FALSE)
