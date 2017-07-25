###how to divide


rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)
library(dplyr)
setwd("~/Documents/git/ospree/analyses/output")

# Setting working directory.

d<-read.csv("ospree_clean_withchill_BB.csv")
table(d$genus)

d$name<-paste(d$genus,d$species,sep="_")
sp <- areone[which(areone$respvar.simple %in% respvar.time),]


xx<-d

xx <- within(xx, { prov.lat <- ave(provenance.lat, name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { field.sample <- ave(fieldsample.date, name, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, name, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, name, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chill <- ave(chilltemp, name, species, FUN=function(x) length(unique(x)))}) # mult expchill
xx <- within(xx, { spp <- ave(species, name, FUN=function(x) length(unique(x)))}) # mult species
xx <- within(xx, { prov.long <- ave(provenance.long,name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.longs
xx <- within(xx, { datasets <- ave(datasetID, name, species, FUN=function(x) length(unique(x)))}) 

xx<-dplyr::select(xx,name, datasets, force, photo, chill)
xx<-xx[!duplicated(xx),]
View(xx)
multi.dat<-filter(xx,datasets>=3)
datforce<-filter(multi.dat, force>=3)
multiforcepho<-filter(datforce,photo>=3)
multiforcephochill<-filter(multiforcepho,chill>=3)

#write.csv(xx, file="~/Documents/git/ospree/analyses/output/species_manipulation_levels.csv", row.names = FALSE)
