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

## subsetting for experimental chilling
#bb<-subset(bb,!is.na(as.numeric(chilltemp)))

# slim down our columns
columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp", "forcetemp_night",
                   "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", 
                   "complex", "type")
bb <- subset(bb.wtaxa.resp, select=columnstokeep)

