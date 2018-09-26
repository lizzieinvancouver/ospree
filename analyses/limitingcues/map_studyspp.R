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
library(grid)

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

sp$numspp<-ifelse(sp$numspp>=1 & sp$numspp<=5, 1, sp$numspp)
sp$numspp<-ifelse(sp$numspp>5 & sp$numspp<=15, 2, sp$numspp)
sp$numspp<-ifelse(sp$numspp>15 & sp$numspp<=30, 3, sp$numspp)
sp$numspp<-ifelse(sp$numspp>30 & sp$numspp<=50, 4, sp$numspp)
sp$numspp<-ifelse(sp$numspp>50 & sp$numspp<=100, 5, sp$numspp)
sp$numspp<-ifelse(sp$numspp>100, 6, sp$numspp)

my.pal<-rep(brewer.pal(n=12, name="Set3"), 7)
my.pch<-rep(c(4,6,8,15:18), each=12)


mapWorld <- borders("world", colour="gray88", fill="gray82") # create a layer of borders
mp <- ggplot(sp, aes(x=long, y=lat, color=datasetID, size=as.factor(numspp), shape=datasetID)) +   mapWorld +
  theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'white'),
        legend.position=c(0.93, 0.62),
        legend.key = element_rect(fill="white")) + geom_jitter(width=1,aes(color=datasetID, size=as.factor(numspp), shape=datasetID)) +
  guides(color=FALSE, shape=FALSE)  +
  scale_colour_manual(name="DatasetID", values=my.pal,
                      labels=sp$datasetID) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sp$datasetID) +
  scale_size_manual(values=c(1,2,3,4,5,6), labels = c("1-10","11-20","21-40","41-80","81-120",">120"), name="Number of Species") +
  xlab("") + ylab("")


euro<-ggplot(sp, aes(x=long, y=lat, color=datasetID, size=as.factor(numspp), shape=datasetID)) +   mapWorld +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'white'),
        legend.position="none",
        legend.key = element_rect(fill="white"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,-0.5), "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + geom_jitter(width=1, aes(color=datasetID, size=as.factor(numspp), shape=datasetID)) +
  guides(color=FALSE, shape=FALSE)  +
  scale_colour_manual(name="DatasetID", values=my.pal,
                      labels=sp$datasetID) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sp$datasetID) +
  scale_size_manual(values=c(1,2,3,4,5,6), labels = c("1-10","11-20","21-40","41-80","81-120",">120"), name="Number of Species") +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-15,35), ylim=c(35,70)) + labs(x=NULL, y=NULL)

vp <- viewport(width = 0.25, height = 0.4, x = 0.172, y = 0.262)

#Just draw the plot twice
print(mp)
print(euro, vp = vp)



