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
library(rgdal)
library(maptools)
library(rgeos)
library(ggmap)

# Set working directory

#if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
#}else if
#(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
#}else
setwd("~/Documents/git/ospree/analyses")
ospree<-read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

## Find studies with multiple provenance latitudes
d<-ospree%>%
  dplyr::select(datasetID, genus, species, provenance.lat, provenance.long, respvar.simple)%>%
  group_by(datasetID,provenance.lat)%>%
  arrange(datasetID)
resps<-c("daystobudburst", "percentbudburst")
d<-filter(d, respvar.simple %in% resps)
lats <- within(d, { prov <- ave(provenance.lat, datasetID, species, FUN=function(x) length(unique(x)))})
lats$total<-ifelse(lats$prov>=2, lats$prov, NA)
lats<-na.omit(lats)


#d<-ungroup(d)
#check<-d%>%dplyr::select(datasetID, count)
#check<-check[!duplicated(check),]
#check$total<-ifelse(check$count>=2, check$count, NA)
#check<-na.omit(check)
#check<-dplyr::select(check, -total)
#check<-check[which(check$datasetID!="schnabel87"),]  ## Doesn't have multiple provenance latitudes

#write.csv(check, file=("~/Documents/git/ospree/analyses/lat_analysis/lat_output/lat_studies.csv"), row.names = FALSE)

## What else was manipulated?
datasets<-unique(lats$datasetID)
xx<-ospree%>%
  filter(datasetID %in% datasets)%>%
  dplyr::select(datasetID, genus, species, provenance.lat, provenance.long, fieldsample.date, chilltemp, forcetemp, photoperiod_day, Total_Chilling_Hours, 
                growing.lat, growing.long, respvar.simple)
#resps<-c("daystobudburst", "percentbudburst")
#xx<-filter(xx, respvar.simple %in% resps)
xx <- within(xx, { prov <- ave(provenance.lat, datasetID, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { sample <- ave(fieldsample.date, datasetID, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, datasetID, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, datasetID, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chill <- ave(Total_Chilling_Hours, datasetID, species, FUN=function(x) length(unique(x)))}) # mult Total_Chilling_Hours
xx <- within(xx, { spp <- ave(species, datasetID, FUN=function(x) length(unique(x)))}) # mult species
#xx<-filter(xx, xx$prov>=2)
xx<-dplyr::select(xx, datasetID, genus, species, provenance.lat, provenance.long, prov, sample, force, photo, chill, spp)
xx<-xx[!duplicated(xx),]
xx$provenance.long <- ifelse(xx$datasetID=="hawkins12", -xx$provenance.long, xx$provenance.long)
xx$provenance.long[which(xx$datasetID=="hawkins12" & xx$provenance.long == -427.78)]<--127.78
xx<- filter(xx, provenance.lat>=0) ## remove South Africa studies
xx<-filter(xx, provenance.long>=-15) ## remove N. America studies
xx<-filter(xx, provenance.long<=100) ## remove Asian studies
bads<-c("heide05","heide12", "partanen01", "skuterud94", "viheraaarnio06")
xx<-xx%>%filter(datasetID!= "heide05") ## Doesn't have multiple provenance latitudes
xx<-xx%>%filter(datasetID!= "heide12")
xx<-xx%>%filter(datasetID!= "partanen01")
xx<-xx%>%filter(datasetID!= "skuterud94")
xx<-xx%>%filter(datasetID!= "falusi96")
#list of studies to remove: heide05, heide12, partanen01, skuterud94 (no photo)


#write.csv(xx, file=("~/Documents/git/ospree/analyses/lat_analysis/lat_output/study_breakdown.csv"), row.names = FALSE)

########### How much data do we have then? ##############

#df<-ospree%>% filter(datasetID %in% datasets) # 1577 obs
#lats<-unique(df$provenance.lat) # 86 different latitudes
photo<-xx%>%filter(photo>=2)
chill<-xx%>%filter(chill>=2)
force<-xx%>%filter(force>=2)
datasets<-unique(photo$datasetID)

#write.csv(photo, file=("~/Documents/git/ospree/analyses/lat_analysis/lat_output/listofstudies_photo.csv"), row.names = FALSE)
#write.csv(xx, file=("~/Documents/git/ospree/analyses/lat_analysis/lat_output/listofstudies_all.csv"), row.names = FALSE)



ospree$sp.name<-paste(ospree$genus, ospree$species, sep="_")
fagus<-ospree%>%filter(sp.name=="Fagus_sylvatica")
resps<-c("daystobudburst", "percentbudburst")
fagus<-filter(fagus, respvar.simple %in% resps)

eur.fagus<-ggplot() + 
  geom_polygon(data=land, aes(long,lat,group=group), fill="white")+
  geom_path(data=boundars, aes(long,lat, group=group), color="light grey",
            size=0.1) +
  geom_point(data=fagus, aes(provenance.long, provenance.lat, col=response.time)) +
  #scale_size(range=c(1,7), guide = "legend",labs(size="Count")) +
  coord_cartesian(xlim = c(-15, 35), ylim = c(30,70)) +
  theme(aspect.ratio=1)

mod<-lm(response.time~provenance.long*provenance.lat, data=fagus)
display(mod)
