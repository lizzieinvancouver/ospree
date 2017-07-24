## Started 29 June 2017 - Cat

## An R script to organize data with multiple provenance latitudes studied and see what else is manipulated in the study

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(lme4)
library(arm)

# Set working directory

if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
}else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else
  setwd("~/Documents/git/ospree/analyses")
ospree<-read.csv("output/ospree_clean_withchill_bb.csv", header=TRUE)

## Find studies with multiple provenance latitudes
d<-ospree%>%
  dplyr::select(datasetID, genus, species, provenance.lat, provenance.long)%>%
  group_by(datasetID,provenance.lat)%>%
  arrange(datasetID)
d <- within(d, { count <- ave(provenance.lat, datasetID, species, FUN=function(x) length(unique(x)))})
d<- d[!duplicated(d), ]

d<-ungroup(d)
check<-d%>%dplyr::select(datasetID, count)
check<-check[!duplicated(check),]
check$total<-ifelse(check$count>=2, check$count, NA)
check<-na.omit(check)
check<-dplyr::select(check, -total)
check<-check[which(check$datasetID!="schnabel87"),]  ## Doesn't have multiple provenance latitudes

#write.csv(check, file=("~/Documents/git/ospree/analyses/lat_analysis/lat_output/lat_studies.csv"), row.names = FALSE)

## What else was manipulated?
datasets<-unique(check$datasetID)
xx<-ospree%>%
  filter(datasetID %in% datasets)%>%
  dplyr::select(datasetID, genus, species, provenance.lat, provenance.long, fieldsample.date, chilltemp, forcetemp, photoperiod_day, Total_Chilling_Hours, 
                growing.lat, growing.long)
xx <- within(xx, { prov <- ave(provenance.lat, datasetID, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { sample <- ave(fieldsample.date, datasetID, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, datasetID, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, datasetID, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chill <- ave(Total_Chilling_Hours, datasetID, species, FUN=function(x) length(unique(x)))}) # mult Total_Chilling_Hours
xx <- within(xx, { spp <- ave(species, datasetID, FUN=function(x) length(unique(x)))}) # mult species
xx<-filter(xx, xx$prov>=2)
xx<-dplyr::select(xx, datasetID, genus, species, provenance.lat, provenance.long, prov, sample, force, photo, chill, spp)
xx<-xx[!duplicated(xx),]
xx<- filter(xx, provenance.lat>=0) ## remove South Africa studies
xx<-filter(xx, provenance.long>=-10) ## remove N. America studies
#xx<-xx[which(xx$datasetID!="schnabel87"),] ## Doesn't have multiple provenance latitudes

write.csv(xx, file=("~/Documents/git/ospree/analyses/lat_analysis/lat_output/study_breakdown.csv"), row.names = FALSE)

########### How much data do we have then? ##############

df<-ospree%>% filter(datasetID %in% datasets) # 1577 obs
lats<-unique(df$provenance.lat) # 86 different latitudes
photo<-xx%>%filter(photo>=2)
datasets<-unique(photo$datasetID)

write.csv(photo, file=("~/Documents/git/ospree/analyses/lat_analysis/lat_output/listofstudies.csv"), row.names = FALSE)

## initial graphs...
df<-filter(df, provenance.lat>=0) ## remove South Africa study

ggplot((df), aes(x=response.time, y=as.numeric(provenance.lat))) + geom_point(aes(col=species))


### Check out a map?
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(grid)
library(rworldmap)
library(gridExtra)

worldMap <- getMap()

# European Countries
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Norway","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","Switzerland", "United Kingdom")
indEU <- which(worldMap$NAME%in%europeanUnion)
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

eur <- ggplot(europeCoords) + geom_polygon(data = europeCoords, aes(x = long, y = lat, group=region), 
                                           color="grey", fill="white") + coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

eur.map <- eur + 
  geom_point(aes(provenance.long, provenance.lat,  color=response.time),position="jitter", data=df) + scale_color_gradient(low = "blue", high="red")
plot(eur.map)

