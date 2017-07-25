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

xx<-dplyr::select(xx,name,genus, datasets, force, photo, chill,field.sample,prov.lat)
xx<-xx[!duplicated(xx),]

#write.csv(xx, file="~/Documents/git/ospree/analyses/output/species_manipulation_levels.csv", row.names = FALSE)

###make object with all acceptable (<1 data set species)
accept<-filter(xx,datasets>1)
species4taxon<-c(accept$name)
spec.tax<-filter(d, name %in% species4taxon)
spec.tax$complex<-spec.tax$name
spec.tax$use<-"Y"


###making complexes

comp<-filter(xx,datasets==1)
complex4taxon<-c(comp$name)
###building complexes
complexdata<- filter(d, name %in% complex4taxon)
goober<-dplyr::select(complexdata,name,genus, datasetID)
goober<-goober[!duplicated(goober),]
#write.csv(goober, file="~/Documents/git/ospree/analyses/bb_analysis/taxon/one_ataset_list.csv", row.names = FALSE)
complexdata$complex<-NA
complexdata$complex[complexdata$genus == "Acer"] <- "Acer_complex"
complexdata$complex[complexdata$genus == "Betula"] <- "Betula_complex"
complexdata$complex[complexdata$genus == "Fraxinus"] <- "Fraxinus_complex"
complexdata$complex[complexdata$genus == "Juglans"] <- "Juglans_complex"
complexdata$complex[complexdata$genus == "Picea"] <- "Picea_complex"
complexdata$complex[complexdata$genus == "Pinus"] <- "Pinus_complex"
complexdata$complex[complexdata$genus == "Populus"] <- "Populus_complex"
complexdata$complex[complexdata$genus == "Prunus"] <- "Prunus_complex"
complexdata$complex[complexdata$genus == "Pyrus"] <- "Pyrus_complex"
complexdata$complex[complexdata$genus == "Quercus"] <- "Quercus_complex"
complexdata$complex[complexdata$genus == "Rhododendron"] <- "Rhododendron_complex"
complexdata$complex[complexdata$genus == "Salix"] <- "Salix_complex"
complexdata$complex[complexdata$genus == "Sorbus"] <- "Sorbus_complex"
complexdata$complex[complexdata$genus == "Ulmus"] <- "Ulmus_complex"
complexdata$complex[complexdata$genus == "Vaccinium"] <- "Vaccinium_complex"
complexdata$complex[complexdata$genus == "Tilia"] <- "Tilia_complex"

complexdata$use<-NA
complexdata$use[!is.na(complexdata$complex)] <-"Y"
complexdata$use[is.na(complexdata$complex)] <-"N"

good<-select(spec.tax,genus,species,complex,use)
good<-good[!duplicated(good),]

bad<-select(complexdata,genus,species,complex,use)
bad<-bad[!duplicated(bad),]
complex.full<-rbind(good,bad)

write.csv(complex.full, file="~/Documents/git/ospree/analyses/bb_analysis/taxon/complex_levels.csv", row.names = FALSE)

