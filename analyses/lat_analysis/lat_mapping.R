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

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

use.chillports = TRUE# change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = FALSE # change to false to use raw predictors

# Default is species complex and no crops
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE

# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE

#Default is all chilling data
use.expchillonly = FALSE # change to true for only experimental chilling 
#note: with only exp chilling, there is only exp photo and force too.
#also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
source("source/bbstanleadin.R")

bb.wlat <- bb.stan
bb.wlat <- within(bb.wlat, { prov.lat <- ave(provenance.lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
bb.wlat <- subset(bb.wlat, bb.wlat$prov.lat>1)  

lat.stan<-bb.wlat
lat.stan<-subset(lat.stan, lat.stan$resp<600)

lat.stan$lat <- lat.stan$provenance.lat

lat.stan$complex<-as.numeric(as.factor(lat.stan$complex.wname))

lat.stan<-na.omit(lat.stan)

longs<-subset(bb.resp, select=c("datasetID", "genus", "species", "provenance.lat", "resp",
                                "force", "photo", "chill", "provenance.long"))

d<-full_join(lat.stan, longs)

## Mapping
all.spp<-ggplot() + 
  geom_polygon(data=land, aes(long,lat,group=group), fill="white")+
  geom_path(data=boundars, aes(long,lat, group=group), color="light grey",
            size=0.1) +
  geom_point(data=d, aes(provenance.long, provenance.lat, col=complex.wname)) +
  #scale_size(range=c(1,7), guide = "legend",labs(size="Count")) +
  #coord_cartesian(xlim = c(-5, 35), ylim = c(30,70)) +
  theme(aspect.ratio=1)

eur.spp<-ggplot() + 
  geom_polygon(data=land, aes(long,lat,group=group), fill="white")+
  geom_path(data=boundars, aes(long,lat, group=group), color="light grey",
            size=0.1) +
  geom_point(data=d, aes(provenance.long, provenance.lat, col=complex.wname)) +
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
