## Started 30 July 2017 ##
## Lizzie took this from bbmodels_stan.R ##

## Source file for reading in the data and
## doing some cleaning we should ALL do

## But! Keep an eye on this file! We probably need to edit it

# below file is cleaned, had chilling added and some BB cleaning done also
bb.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
# file to adjust species into species or species complexes ... 
taxon <- read.csv("output/bb_analysis/taxon/complex_levels.csv", header=TRUE)

## read taxon data to subset dataset
bb.walltaxa <- merge(bb.all, taxon, by=c("genus","species"), all.x=TRUE)
bb.wtaxa <-subset(bb.walltaxa, use=="Y")

## just the bb data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.wtaxa.resp <- bb.wtaxa[which(bb.wtaxa$respvar.simple %in% respvars.thatwewant),]
bb.wtaxa.resp <- subset(bb.wtaxa.resp, respvar != "thermaltime") # doesn't remove anything

###we have complexes with 1 only dataset id. This happened because we
#filtered them out before reducing the response variables, code belowshould fix.
huh <- within(bb.wtaxa.resp, { datasets <- ave(datasetID, complex, complex, FUN=function(x) length(unique(x)))}) 
huh2<-select(huh,datasets, complex)
huh2<-huh2[!duplicated(huh2),]
#remerg<-filter(huh,datasets==1)
remerg<-select(huh,datasetID, complex)
remerg<-remerg[!duplicated(remerg),]


### making new 
huh<-within(huh,complex[complex=="Cornus_alba"]<-"Cornus_complex")
huh<-within(huh,complex[complex=="Cornus_mas"]<-"Cornus_complex")
huh<-within(huh,complex[complex=="Fraxinus_excelsior"]<-"Fraxinus_complex")
huh<-within(huh,complex[complex=="Prunus_avium"]<-"Prunus_complex")
huh <- within(huh, { datasets <- ave(datasetID, complex, complex, FUN=function(x) length(unique(x)))}) 

huh$use2<-NA
huh<- within(huh, use2[datasets<=1 ]<-"N")
huh<- within(huh, use2[datasets>1 ]<-"Y")


amended<-filter(huh,use2=="Y")

## subsetting for experimental chilling
#bb<-subset(bb,!is.na(as.numeric(chilltemp)))

table(amended$datasets)
# slim down our columns
columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp", "forcetemp_night",
                   "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", 
                   "complex", "type","provenance.lat")
bb <- subset(amended, select=columnstokeep)


