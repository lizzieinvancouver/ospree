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
quartz()
space<-ggplot(d, aes(x=provenance.long, y=provenance.lat)) + geom_point(aes(col=datasetID, shape=datasetID, size=numspp)) 
  

sp<-subset(d, select=c("datasetID", "provenance.long", "provenance.lat", "numspp"))
sp<-sp[!duplicated(sp),]


library(raster)
library(rgdal)
spg<-sp
coordinates(spg)<- ~long+lat
proj4string(spg)<-CRS("+proj=longlat +datum=WGS84")
coords<-spTransform(spg, CRS("+proj=longlat"))
shapefile(coords, "~/Documents/git/regionalrisk/analyses/output/spaceparam.shp", overwrite=TRUE)

