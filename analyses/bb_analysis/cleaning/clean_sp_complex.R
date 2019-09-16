### New strict species code for expanded ospree data
# Begun by Dan Sept 16 2019
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("~Documents/GitHub/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")


# Load libraries
library(dplyr)
library(tidyr)
library(geosphere)
library(lubridate)


d<-read.csv("output/ospree_clean_withchill_BB.csv")

d$name<-paste(d$genus,d$species,sep="_")

xx<-d
### make a list of which studies manipulate what.
colnames(xx)
xx <- within(xx, { prov.lat <- ave(provenance.lat, name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { field.sample <- ave(fieldsample.date, name, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, name, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, name, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chilltemp <- ave(chilltemp, name, species, FUN=function(x) length(unique(x)))}) # mult expchill
xx <- within(xx, { chilltime <- ifelse(chilldays!=0, ave(chilldays, datasetID, species, FUN=function(x) length(unique(x))), 0)}) # mult studychill
#xx <- within(xx, { spp <- ave(species, name, FUN=function(x) length(unique(x)))}) # mult species
#xx <- within(xx, { prov.long <- ave(provenance.long,name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.longs
xx <- within(xx, { datasets <- ave(datasetID, name, species, FUN=function(x) length(unique(x)))}) 

#datasetID)



###make object with all acceptable (>1 data set species and manipulated more than one cue) 
### This make a data sheet with all the complex that can be indivudal species
xx$force<-ifelse(xx$force<=1, 0, 1)
xx$photo<-ifelse(xx$photo<=1, 0, 1)
xx$chilltemp<-ifelse(xx$chilltemp<=1, 0, 1)
xx$chilltime<-ifelse(xx$chilltime<=1, 0, 1) ## Different methods of manipulating chilling
xx$field.sample<-ifelse(xx$field.sample<=1, 0, 1) ## Different methods of manipulating chilling
xx$chill<-ifelse(xx$chilltemp==1 | xx$chilltime==1 | xx$field.sample==1,1, 0)
xx$numcues<-xx$force + xx$photo + xx$chill


xx<-dplyr::select(xx,name,genus, datasets, force, photo, chill,numcues)#prov.lat, 
xx<-xx[!duplicated(xx),]
###HERE it appears some species are duplicated because of misspellings. Is there a place we fix this earlier in cleaning



accept<-xx[(xx$datasets>1& xx$numcues>1),] ## if species were in more than one dataset and manipulated more than one cue that kept
species4taxon<-c(accept$name) ## make a list of species with more than 1 study
accept$complex<-accept$name
accept$use<-"Y"