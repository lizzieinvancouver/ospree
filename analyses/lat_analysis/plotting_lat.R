### Started 6 April 2018 - by Cat ###
## Plotting Lat Analysis and making maps

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(rworldmap)
library(maps)
library(mapdata)
library(marmap)
library(RColorBrewer)
library(egg)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

setwd("~/Documents/git/ospree/analyses")
source('stan/savestan.R')


######Above is all old code. Below is what formats the data properly in accordance with other models
source("lat_analysis/source/bbdataplease.R")
## (2) Deal with species
dim(bb.noNA)
d <- bb.noNA
source("bb_analysis/source/speciescomplex.R")
bb.noNA.wtaxa <- d
dim(bb.noNA.wtaxa)
unique(bb.noNA.wtaxa$complex)
bb.wlab<-bb.noNA.wtaxa

###filter for species of interest
tt <- table(bb.wlab$complex)
bb.wlab <- subset(bb.wlab, complex %in% names(tt[tt > 100])) 

####below is handled in source i think
columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
                   "photoperiod_day", "response", "response.time", "Total_Utah_Model",
                   "complex", "provenance.lat", "provenance.long")
bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)
unique(bb.wlab.sm$complex)
table(bb.wlab.sm$complex)
myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica", "Picea_abies", "Picea_glauca",
         "Pseudotsuga_menziesii")
bb.wlab.sm<-dplyr::filter(bb.wlab.sm, complex%in%myspp)

## make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$chill <- as.numeric(bb.wlab.sm$Total_Utah_Model)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)
bb.wlab.sm$lat<-as.numeric(bb.wlab.sm$provenance.lat)
bb.wlab.sm$long<-as.numeric(bb.wlab.sm$provenance.long)

## subsetting data, preparing genus variable, removing NAs
ospr.prepdata <- subset(bb.wlab.sm, select=c("resp", "chill", "photo", "force", "complex", "lat", "long"))
dim(subset(bb.wlab.sm, is.na(chill)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE 
           & is.na(lat)==FALSE & is.na(long)==FALSE))
ospr.stan <- ospr.prepdata[complete.cases(ospr.prepdata),]
#ospr.stan$complex <- as.numeric(as.factor(ospr.stan$complex))

ospr.stan<-ospr.stan[(ospr.stan$resp!=999),]

ospr.stan$sm.chill<-ospr.stan$chill/240

gyms<-c("Picea_abies", "Picea_glauca", "Pseudotsuga_menziesii")
ospr.stan$type<-ifelse(ospr.stan$complex%in%gyms, "Gymnosperms", "Angiosperms")
### Let's make a map to start...
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35))
type<- ggplot(ospr.stan, aes(x=long, y=lat, col=as.factor(type))) +   mapWorld +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))
type<- type + theme(panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.key.size = unit(0.25, "cm"),
                  legend.text = element_text(size=8),
                  legend.title = element_text(size=9)) + geom_point(aes(col=as.factor(type))) + geom_jitter()+ 
  labs(color="Type") +
  xlab("Longitude") + ylab("Latitude")

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 235))
lat<- ggplot(ospr.stan, aes(x=long, y=lat, col=resp)) +   mapWorld +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))
lat<- lat + theme(panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.key.size = unit(0.25, "cm"),
                  legend.text = element_text(size=8),
                  legend.title = element_text(size=9)) + geom_point(aes(col=resp)) + geom_jitter()+ 
  labs(color="Days to Budburst") +
  xlab("Longitude") + ylab("Latitude") +sc

pc <- scale_colour_gradientn(colours = myPalette(100), limits=c(6, 24))
photo<- ggplot(ospr.stan, aes(x=long, y=lat, col=photo)) +   mapWorld +
  coord_cartesian(ylim=c(30,70),xlim=c(-10,35))
photo<- photo + theme(panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.key.size = unit(0.25, "cm"),
                  legend.text = element_text(size=8),
                  legend.title = element_text(size=9)) + geom_point(aes(col=photo)) + geom_jitter()+ 
  labs(color="Photoperiod") +
  xlab("Longitude") + ylab("Latitude") + pc

quartz()
ggarrange(type, lat, photo)


#### Now for some plotting...
ggplot(ospr.stan, aes(x=lat, y=resp, col=photo)) + geom_point()

ospr.stan$photo.class<-NA
ospr.stan$photo.class<-ifelse(ospr.stan$photo>=5 & ospr.stan$photo<10, "5-9 hrs", ospr.stan$photo.class)
ospr.stan$photo.class<-ifelse(ospr.stan$photo>=10 & ospr.stan$photo<15, "10-14 hrs", ospr.stan$photo.class)
ospr.stan$photo.class<-ifelse(ospr.stan$photo>=15 & ospr.stan$photo<20, "15-19 hrs", ospr.stan$photo.class)
ospr.stan$photo.class<-ifelse(ospr.stan$photo>=20 & ospr.stan$photo<25, "20-24 hrs", ospr.stan$photo.class)

ospr.stan$legend<-factor(ospr.stan$photo.class,
                      labels=c("5-9 hrs", "10-14 hrs", "15-19 hrs",
                               "20-24 hrs"))
ggplot(ospr.stan, aes(x=lat, y=resp)) + geom_smooth(aes(x=lat, y=resp, col=legend), 
                                                    method="glm", se=FALSE) +
  labs(col="Photoperiod") + xlab("Latitude") + ylab("Days to Budburst") + 
  scale_colour_manual(breaks=c("5-9 hrs", "10-14 hrs", "15-19 hrs",
                      "20-24 hrs"), 
                      values=c("green4", "firebrick3", "orange3","purple2"))

ggplot(ospr.stan, aes(x=lat, y=resp)) + geom_point(aes(x=lat, y=resp, col=legend), alpha=0.3) +
  labs(col="Photoperiod") + xlab("Latitude") + ylab("Days to Budburst") + 
  scale_colour_manual(breaks=c("5-9 hrs", "10-14 hrs", "15-19 hrs",
                               "20-24 hrs"), 
                      values=c("green4", "firebrick3", "orange3","purple2"))+ 
  geom_smooth(aes(x=lat, y=resp, col=legend), 
                                                                                          method="glm", se=FALSE)
  
