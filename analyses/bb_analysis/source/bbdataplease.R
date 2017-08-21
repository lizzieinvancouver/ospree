## Started 30 July 2017 ##
## Lizzie took this from bbmodels_stan.R ##

## Source file for reading in the data and
## doing some cleaning we should ALL do

## But! Keep an eye on this file! We probably need to edit it

# below file is cleaned, had chilling added and some BB cleaning done also
bb.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

if(FALSE){
# file to adjust species into species or species complexes ... 
taxon <- read.csv("output/bb_analysis/taxon/complex_levels.csv", header=TRUE)

## read taxon data to subset dataset
bb.walltaxa <- merge(bb.all, taxon, by=c("genus","species"), all.x=TRUE)
bb.wtaxa <-subset(bb.walltaxa, use=="Y")
}

# FIX! HACK for now ... 
bb.wtaxa <- bb.all

## just the bb data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.wtaxa.resp <- bb.wtaxa[which(bb.wtaxa$respvar.simple %in% respvars.thatwewant),]
bb.wtaxa.resp <- subset(bb.wtaxa.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)
bb.wtaxa.resp$force <- as.numeric(bb.wtaxa.resp$forcetemp)
bb.wtaxa.resp$photo <- as.numeric(bb.wtaxa.resp$photoperiod_day)
bb.wtaxa.resp$chill <- as.numeric(bb.wtaxa.resp$Total_Chilling_Hours)
bb.wtaxa.resp$resp <- as.numeric(bb.wtaxa.resp$response.time)

## remove the NAs (must do this before you can deal with taxon issues)
bb.noNA <- subset(bb.wtaxa.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
    is.na(chill)==FALSE & is.na(resp)==FALSE)

d <- bb.noNA

source("bb_analysis/cleaning/clean_speciescomplex.R")

if(FALSE){
## Now we have to check what complexes are down to 1 dataset
taxa.numprep <- aggregate(bb.noNA[c("resp")], bb.noNA[c("complex", "datasetID")], FUN=mean)
taxa.num <- aggregate(taxa.numprep[c("datasetID")], taxa.numprep[c("complex")], FUN=length)
subset(taxa.num, datasetID<2)

## Fix the ones that are now one dataset ...
bb.noNA$complex[which(bb.noNA$complex=="Acer_pseudoplatanus")] <- "Acer_complex"
bb.noNA$complex[which(bb.noNA$complex=="Betula_alleghaniensis")] <- "Betula_complex"
bb.noNA$complex[which(bb.noNA$complex=="Populus_tremula")] <- "Populus_complex"
# bb.noNA$complex[which(bb.noNA$complex=="Pyrus_pyrifolia")] <- "Pyrus_complex" # must be in same study
bb.noNA$complex[which(bb.noNA$complex=="Quercus_ilex")] <- "Quercus_complex"
bb.noNA$complex[which(bb.noNA$complex=="Quercus_petraea")] <- "Quercus_complex"
bb.noNA$complex[which(bb.noNA$complex=="Quercus_robur")] <- "Quercus_complex"
bb.noNA$complex[which(bb.noNA$complex=="Quercus_rubra")] <- "Quercus_complex"
bb.noNA$complex[which(bb.noNA$complex=="Sorbus_commixta")] <- "Sorbus_complex"
bb.noNA$complex[which(bb.noNA$complex=="Sorbus_aucuparia")] <- "Sorbus_complex"
bb.noNA$complex[which(bb.noNA$complex=="Tilia_cordata")] <- "Tilia_complex"

# delete the ones that have no one to go with
toremove <- c("Actinidia_deliciosa", "Aesculus_hippocastanum", "Cornus_alba", "Corylus_avellana", 
    "Larix_decidua", "Liquidambar_styraciflua", "Picea_complex", "Prunus_complex", 
    "Pyrus_complex","Pyrus_pyrifolia", "Pseudotsuga_menziesii", "Rhododendron_complex", 
    "Rubus_idaeus", "Ulmus_complex", "Vitis_vinifera")

bb.corrtaxa <- bb.noNA[which(!bb.noNA$complex %in% toremove),]

## Check our work
taxa.numprep.check <- aggregate(bb.corrtaxa[c("resp")], bb.corrtaxa[c("complex", "datasetID")], FUN=mean)
taxa.num.check <- aggregate(taxa.numprep.check[c("datasetID")], taxa.numprep.check[c("complex")], FUN=length)
subset(taxa.num.check, datasetID<2)
}

# slim down our columns
columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp", "forcetemp_night",
                   "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", 
                   "complex", "type","provenance.lat", "force", "photo", "chill", "resp", "type")
bb <- subset(bb.corrtaxa, select=columnstokeep)


## subsetting for experimental chilling
#bb<-subset(bb,!is.na(as.numeric(chilltemp)))

if(FALSE){ # To discuss
    
huh <- within(bb.wtaxa.resp, { datasets <- ave(datasetID, complex, complex, FUN=function(x) length(unique(x)))}) 
huh2<-select(huh,datasets, complex)
huh2<-huh2[!duplicated(huh2),]
#remerg<-filter(huh,datasets==1)
remerg<-select(huh,datasetID, complex)
remerg<-remerg[!duplicated(remerg),]

### making new 
huh <- within(huh,complex[complex=="Cornus_alba"]<-"Cornus_complex")
huh <- within(huh,complex[complex=="Cornus_mas"]<-"Cornus_complex")
huh <- within(huh,complex[complex=="Fraxinus_excelsior"]<-"Fraxinus_complex")
huh <- within(huh,complex[complex=="Prunus_avium"]<-"Prunus_complex")
huh <- within(huh, { datasets <- ave(datasetID, complex, complex, FUN=function(x) length(unique(x)))}) 

huh$use2<-NA
huh<- within(huh, use2[datasets<=1 ]<-"N")
huh<- within(huh, use2[datasets>1 ]<-"Y")


amended<-filter(huh,use2=="Y")

}

