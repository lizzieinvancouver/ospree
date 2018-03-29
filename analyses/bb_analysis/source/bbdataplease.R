## Started 30 July 2017 ##
## Lizzie took this from bbmodels_stan.R ##

## Source file for reading in the data and
## doing some cleaning we should ALL do

## But! Keep an eye on this file! We probably need to edit it
checkdataforNAs <- FALSE # Set to TRUE for looking at missing data rows

# below file is cleaned, had chilling added and some BB cleaning done also
bb.all <- read.csv("..//output/ospree_clean_withchill_BB.csv", header=TRUE)

# file to adjust species into species or species complexes ... 
taxon <- read.csv("..//output/bb_analysis/taxon/complex_levels.csv", header=TRUE)

## read taxon data to get the 'type' category, then delete what we don't need
bb.all.wtaxa <- merge(bb.all, taxon, by=c("genus","species"), all.x=TRUE)
bb.all.wtaxa$complex <- NULL
bb.all.wtaxa$use <- NULL

## just the bb data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- bb.all.wtaxa[which(bb.all.wtaxa$respvar.simple %in% respvars.thatwewant),]
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)
bb.resp$forceday <- as.numeric(bb.resp$forcetemp)
bb.resp$forcenight <- as.numeric(bb.resp$forcetemp_night)
bb.resp$photonight <- as.numeric(bb.resp$photoperiod_night)

bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$force <- bb.resp$forceday
bb.resp$force[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] <-
    (bb.resp$forceday[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photo[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] +
    bb.resp$forcenight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photonight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE])/24

bb.resp$chill <- as.numeric(bb.resp$Total_Utah_Model) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions
bb.resp$resp <- as.numeric(bb.resp$response.time)

## center the predictors:
bb.resp$force.cen <- bb.resp$force/mean(bb.resp$force,na.rm=TRUE)
bb.resp$photo.cen <- bb.resp$photo/mean(bb.resp$photo,na.rm=TRUE)
bb.resp$chill.cen <- bb.resp$chill/mean(bb.resp$chill,na.rm=TRUE)

## remove the NAs (must do this before you can deal with taxon issues)
bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
    is.na(chill)==FALSE & is.na(resp)==FALSE)
# bb.noNA<-subset(bb.noNA, field.sample<=1)

# Vector needed to identify weinberger-design studies
if(FALSE){
weinberg<-c("falusi03", "falusi97", "heide93", "jones12", "partanen05", "ramos99",
            "ashby62","basler14","biasi12","boyer","calme94","charrier11","cook00b",
             "ganset02"  ,"gianfagna85","guerriero90","gunderson12")
}
       
if(checkdataforNAs){
forceissues <- subset(bb.resp, is.na(force)==TRUE)
table(forceissues$datasetID)
photoissues <- subset(bb.resp, is.na(photo)==TRUE)
table(photoissues$datasetID)
chillissues <- subset(bb.resp, is.na(chill)==TRUE)
table(chillissues$datasetID)
respissues <- subset(bb.resp, is.na(resp)==TRUE)
table(respissues$datasetID)
    }

# See imputechilling.R for notes (and issue # 147)

if(FALSE){
## what is lost due to NAs?
forceNA <- bb.resp[which(is.na(bb.resp$force)==TRUE),]
forceNA$forcetemp
subset(forceNA, forcetemp=="")  # what is up with the no entry ones? hawkins12 and gansert02
photoNA <- bb.resp[which(is.na(bb.resp$photo)==TRUE),]
photoNA$photoperiod_day # these datasetIDs are all mentioned as not possible to fix in clean_photoperiod.R
    }
