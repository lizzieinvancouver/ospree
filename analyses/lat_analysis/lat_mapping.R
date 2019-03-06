## Started 24 July 2017 - Cat

## An R script to organize data with multiple provenance latitudes studied and see 
# where the studies are and what species are assessed across the differnt locations

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(grid)
library(gridExtra)
library(maptools)
library(ggplotify)

# Set working directory
setwd("~/Documents/git/regionalrisk/analyses")
#land<-readShapeSpatial("input/natural_earth_vector/50m_physical/ne_50m_land.shp") ## 
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

lats <- subset(lat.stan, select=c("datasetID", "genus", "species", "provenance.lat", 
                                  "resp", "force", "photo", "chill"))

longs<-subset(bb.noNA, select=c("datasetID", "genus", "species", "provenance.lat", "resp",
                                "force", "photo", "chill", "provenance.long"))

## Mapping
mapWorld<-fortify(boundars)
quartz()
my.pal<-rep(brewer.pal(n=12, name="Set3"),7)
my.pch<-rep(c(4,6,8), each=12)
mp <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=1.5,aes(x=d$provenance.long, y=d$provenance.lat, color=d$complex.wname, shape=d$complex.wname)) + theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.position=c(0.075, 0.163),
        #legend.position = "none",
        legend.key = element_rect(fill="white", color="white"),
        legend.box.background = element_rect(fill="white"),legend.text = element_text(size=7), legend.key.size = unit(0.3,"cm"),
        legend.title = element_text(size=8))+
  #guides(color=FALSE, shape=FALSE)  +
  scale_colour_manual(name="Species", values=my.pal,
                      labels=sort(unique(d$complex.wname))) + scale_shape_manual(name="Species", values=my.pch, labels=sort(unique(d$complex.wname))) +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-125, 50), ylim=c(25, 100))



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



