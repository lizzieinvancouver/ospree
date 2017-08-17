## Try and organize so useful for all models running
## Started by Dan B - 26 July 2017
# Edits by Cat C - 17 August 2017

###how to divide
# Housekeeping
#rm(list=ls()) 
#options(stringsAsFactors = FALSE)

## Load Libraries
#library(ggplot2)
#library(lme4)
#library(dplyr)

# Set Working Directory
#setwd("~/Documents/git/ospree/analyses/output")

#d<-read.csv("ospree_clean_withchill_BB.csv")
table(d$genus)

d$name<-paste(d$genus,d$species,sep="_")
#sp <- areone[which(areone$respvar.simple %in% respvar.time),]


xx<-d

xx <- within(xx, { prov.lat <- ave(provenance.lat, name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { field.sample <- ave(fieldsample.date, name, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, name, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, name, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chill <- ave(chilltemp, name, species, FUN=function(x) length(unique(x)))}) # mult expchill
xx <- within(xx, { spp <- ave(species, name, FUN=function(x) length(unique(x)))}) # mult species
xx <- within(xx, { prov.long <- ave(provenance.long,name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.longs
xx <- within(xx, { datasets <- ave(datasetID, name, species, FUN=function(x) length(unique(x)))}) 

xx<-dplyr::select(xx,name,genus, datasets, force, photo, chill,field.sample,prov.lat)
xx<-xx[!duplicated(xx),]

#write.csv(xx, file="~/Documents/git/ospree/analyses/output/species_manipulation_levels.csv", row.names = FALSE)

#############  Source this section once you subset down! ###############

###make object with all acceptable (<1 data set species)
accept<-filter(xx,datasets>1)
species4taxon<-c(accept$name)
spec.tax<-filter(d, name %in% species4taxon)
spec.tax$complex<-spec.tax$name
spec.tax$use<-"Y"

##### Once you subset down to species, must make complexes by hand, as seen below for entire Ospree dataset ############
###making complexes

comp<-filter(xx,datasets==1)
complex4taxon<-c(comp$name)
###building complexes
complexdata<- filter(d, name %in% complex4taxon)
goober<-dplyr::select(complexdata,name,genus, datasetID)
goober<-goober[!duplicated(goober),]

goober<- within(goober, {datasets<- ave(datasetID, genus, FUN=function(x) length(unique(x)))})
goober<-arrange(goober, genus)

goober<-filter(goober,datasets>1)
goober$complex<-paste(goober$genus, "complex", sep="_")
genus.tax<-filter(d, genus %in% genus4taxon)
genus.tax$complex<-paste(genus.tax$genus, "complex", sep="_")
genus.tax$use<-"Y"



