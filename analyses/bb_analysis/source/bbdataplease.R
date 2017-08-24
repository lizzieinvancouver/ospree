## Started 30 July 2017 ##
## Lizzie took this from bbmodels_stan.R ##

## Source file for reading in the data and
## doing some cleaning we should ALL do

## But! Keep an eye on this file! We probably need to edit it

# below file is cleaned, had chilling added and some BB cleaning done also
bb.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

# file to adjust species into species or species complexes ... 
taxon <- read.csv("output/bb_analysis/taxon/complex_levels.csv", header=TRUE)

## read taxon data to get the 'type' category, then delete what we don't need
bb.all.wtaxa <- merge(bb.all, taxon, by=c("genus","species"), all.x=TRUE)
bb.all.wtaxa$complex <- NULL
bb.all.wtaxa$use <- NULL

## just the bb data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- bb.all.wtaxa[which(bb.all.wtaxa$respvar.simple %in% respvars.thatwewant),]
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)
bb.resp$force <- as.numeric(bb.resp$forcetemp)
bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$chill <- as.numeric(bb.resp$Total_Chilling_Hours)
bb.resp$resp <- as.numeric(bb.resp$response.time)

## remove the NAs (must do this before you can deal with taxon issues)
bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
    is.na(chill)==FALSE & is.na(resp)==FALSE)

if(FALSE){
## what is lost due to NAs?
forceNA <- bb.resp[which(is.na(bb.resp$force)==TRUE),]
forceNA$forcetemp
subset(forceNA, forcetemp=="")  # what is up with the no entry ones? hawkins12 and gansert02
photoNA <- bb.resp[which(is.na(bb.resp$photo)==TRUE),]
photoNA$photoperiod_day
    }
