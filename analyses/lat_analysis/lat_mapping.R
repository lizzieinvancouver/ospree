## Started 24 July 2017 - Cat

## An R script to organize data with multiple provenance latitudes studied and see 
# where the studies are and what species are assessed across the differnt locations

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(rgdal)
library(maptools)
library(rgeos)
library(ggmap)
library(ggplot2)
library(arm)

# Set working directory
setwd("~/Documents/git/regionalrisk/analyses")
land<-readShapeSpatial("input/natural_earth_vector/50m_physical/ne_50m_land.shp") ## 
boundars<-readShapeSpatial("input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
df<-read.csv("~/Documents/git/ospree/analyses/lat_analysis/lat_output/listofstudies_all.csv", header=TRUE)
photo<-read.csv("~/Documents/git/ospree/analyses/lat_analysis/lat_output/listofstudies_photo.csv", header=TRUE)
d<-read.csv("~/Documents/git/ospree/analyses/output/ospree_clean_withchill_bb.csv", header=TRUE)
df$sp.name<-paste(df$genus, df$species, sep="_")
photo$sp.name<-paste(photo$genus, photo$species, sep="_")
datasets<-unique(photo$datasetID)
d<-d%>%filter(datasetID %in% datasets)
d$sp.name<-paste(d$genus, d$species, sep="_")

## Mapping
all.spp<-ggplot() + 
  geom_polygon(data=land, aes(long,lat,group=group), fill="white")+
  geom_path(data=boundars, aes(long,lat, group=group), color="light grey",
            size=0.1) +
  geom_point(data=d, aes(provenance.long, provenance.lat, col=sp.name)) +
  #scale_size(range=c(1,7), guide = "legend",labs(size="Count")) +
  #coord_cartesian(xlim = c(-5, 35), ylim = c(30,70)) +
  theme(aspect.ratio=1)

eur.spp<-ggplot() + 
  geom_polygon(data=land, aes(long,lat,group=group), fill="white")+
  geom_path(data=boundars, aes(long,lat, group=group), color="light grey",
            size=0.1) +
  geom_point(data=d, aes(provenance.long, provenance.lat, col=sp.name)) +
  #scale_size(range=c(1,7), guide = "legend",labs(size="Count")) +
  coord_cartesian(xlim = c(-15, 35), ylim = c(30,70)) +
  theme(aspect.ratio=1)

eur.resp<-ggplot() + 
  geom_polygon(data=land, aes(long,lat,group=group), fill="white")+
  geom_path(data=boundars, aes(long,lat, group=group), color="light grey",
            size=0.1) +
  geom_point(data=d, aes(provenance.long, provenance.lat, col=response.time)) +
  #scale_size(range=c(1,7), guide = "legend",labs(size="Count")) +
  coord_cartesian(xlim = c(-15, 35), ylim = c(30,70)) +
  theme(aspect.ratio=1)



## Models..
mod<-lm(response.time~provenance.lat*provenance.long, data=d)
display(mod)
