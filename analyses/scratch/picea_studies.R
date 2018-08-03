##DAn's picea investigation 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(lme4)
library(tidyverse)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) {    
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 
  setwd("~/Documents/git/ospree/analyses")

# get the data 
bb.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
# bb.all <- read.csv("output/ospree_clean_withchill_BB_2017Jun22.csv", header=TRUE)

bb.some <- subset(bb.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(bb.some, response.time!="")
pica<-filter(bbdat,genus=="Picea")
pica<-filter(bbdat,species=="abies")


library(ggmap)
library(ggplot2)

range(pica$provenance.lat)
range(pica$provenance.long)
### Set a range
lat <- c(55 ,60)                
lon <- c(-130, 80)  

map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 3,
               maptype = "satellite", source = "google")
ggmap(map)+geom_point(aes(x = provenance.long, y = provenance.lat,color=datasetID), data = pica , size = 1)

worrall<-filter(pica,datasetID=="worrall67")
nrow(worrall)
59/325 #18% are out of natural range.
