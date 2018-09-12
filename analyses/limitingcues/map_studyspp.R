## Map showing OSPREE data for Limiting Cues paper
# Goal of the map is to introduce readers to the data
# Have datasetID color coded and number of species by size
# Cat - 31 Aug 2018

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)

## a bunch of this code is taken from cleaning/cleanup_checksmaps.R
# Get packages
d<-read.csv("output/ospree_clean_withchill_BB.csv")

d<-subset(d, select=c("datasetID", "genus", "species", "provenance.lat", "provenance.long"))
d<-d[!duplicated(d),]

d$complex<-paste(d$genus, d$species, sep="_")
d$numspp<-ave(d$complex, d$datasetID, FUN=length)
d$numspp<-as.numeric(d$numspp)

d$provenance.long<-ifelse(d$datasetID=="charrier11" & d$provenance.lat==45.77220, 3.147220, d$provenance.long)
d<-na.omit(d)

sp<-subset(d, select=c("datasetID", "provenance.long", "provenance.lat", "numspp"))
sp<-sp[!duplicated(sp),]

sp<-sp%>%dplyr::rename(long=provenance.long)%>%rename(lat=provenance.lat)
sp$legend<-NA
sp$legend<-ifelse(sp$numspp>=1 & sp$numspp<=5, "1-5", sp$legend)
sp$legend<-ifelse(sp$numspp>5 & sp$numspp<=15, "6-15", sp$legend)
sp$legend<-ifelse(sp$numspp>15 & sp$numspp<=30, "16-30", sp$legend)
sp$legend<-ifelse(sp$numspp>31 & sp$numspp<=50, "31-50", sp$legend)
sp$legend<-ifelse(sp$numspp>51 & sp$numspp<=100, "51-100", sp$legend)
sp$legend<-ifelse(sp$numspp>100, "101-150", sp$legend)

sp$numspp<-ifelse(sp$numspp>=1 & sp$numspp<=5, 1, sp$numspp)
sp$numspp<-ifelse(sp$numspp>5 & sp$numspp<=15, 2, sp$numspp)
sp$numspp<-ifelse(sp$numspp>15 & sp$numspp<=30, 3, sp$numspp)
sp$numspp<-ifelse(sp$numspp>31 & sp$numspp<=50, 4, sp$numspp)
sp$numspp<-ifelse(sp$numspp>51 & sp$numspp<=100, 5, sp$numspp)
sp$numspp<-ifelse(sp$numspp>100, 6, sp$numspp)


mapWorld <- borders("world", colour="gray88", fill="gray82") # create a layer of borders
mp <- ggplot(sp, aes(x=long, y=lat, color=datasetID, size=as.factor(numspp))) +   mapWorld +
  theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'grey98'),
        legend.position=c(0.1, 0.25),
        legend.key = element_rect(fill="white")) + geom_point(aes(color=datasetID, size=as.factor(numspp))) + geom_jitter() +
  guides(color=FALSE) + scale_size_manual(values=c(1,2,3,4,5,6), labels = c("1-5","6-15","16-30","31-50","51-100",">100"), name="Number of Species")

quartz()
mp




