## 26 June 2017 - Cat
# What's going on with this phenstage madness?

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(geosphere)

# Set Working Directory
setwd("~/Documents/git/ospree/analyses/output")
d<-read.csv("ospree_clean_withchill.csv", header=TRUE)

phenstage <- d[which(d$respvar.simple=="phenstage"),]
unique(phenstage$datasetID)

datasets<-unique(phenstage$datasetID)
xx<-d[which(d$datasetID==datasets),] # cannell83, gansert02, gunderson12, pagter15, pettersen71, sonsteby13
unique(xx$respvar.simple)
daysto<-xx%>%filter(respvar.simple=="daystobudburst")

### Let's check...
# cannell83: not a useful daystobudburst conversion

# gansert02: we could use budstages 2-3... fixing gives 3 observations
gansert<-xx%>%filter(datasetID=="gansert02")
gansert$response<-ifelse(gansert$response>=2 & gansert$response<7, gansert$response, NA)
gansert<-gansert[which(!is.na(gansert$response)),]
gansert02<-gansert%>%group_by(fieldsample.date,species)%>%
  arrange(fieldsample.date)%>%filter(row_number()==1)
gansert02$respvar.simple<-"daystobudburst"
gansert02$response<-"timeonly"

# gunderson12: budburst is defined as stage 4 which is plotted in Figure 2 and is already recorded in ospree dataset

# pagter15: Between budstage 1 and 2 is considered budburst - gives 5 observations
pagter<-xx%>%filter(datasetID=="pagter15")
pagter$response<-ifelse(pagter$response>=1, pagter$response, NA)
pagter<-pagter[which(!is.na(pagter$response)),]
pagter15<-pagter%>%
  group_by(provenance.lat, fieldsample.date, chilldays)%>%
  arrange(provenance.lat) %>%
  filter(row_number()==1)
pagter15$respvar.simple<-"daystobudburst"
pagter15$response<-"timeonly"

# pettersen71: flowers - can't fix

# sonsteby13: flowers - can't fix



