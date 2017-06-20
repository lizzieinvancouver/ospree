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
studytype <- read.csv("output/studytype.csv", header=TRUE)

# Subset down to find all studies with multiple latitudes
lat<-studytype%>%
  filter(latitude.count>=2)%>%
  dplyr::select(datasetID, latitude.count, longitude.count)

##added by Dan
reduced<-filter(studytype, respvar.simple=="daystobudburst")
unique(reduced$datasetID)
reduced%>%
  filter(latitude.count>=2)%>%
  dplyr::select(datasetID, latitude.count, longitude.count)

write.csv(lat, file="~/Documents/git/ospree/analyses/lat_analysis/lat_output/lat_studies.csv")
