## Started 30 July 2017 ##
## Lizzie took this from bbmodels_stan.R ##

## Source file for reading in the data and
## doing some cleaning we should ALL do

## But! Keep an eye on this file! We probably need to edit it
checkdataforNAs <- FALSE # Set to TRUE for looking at missing data rows

#source("source/speciescomplex.R")
d<-read.csv("..//output/ospree_clean_withchill_BB.csv", header=TRUE)

## just the bb data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),]
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
bb.resp$chill.hrs <- as.numeric(bb.resp$Total_Chilling_Hours) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions
bb.resp$chill.ports <- as.numeric(bb.resp$Total_Chill_portions) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions

bb.resp$resp <- as.numeric(bb.resp$response.time)

## z-score the predictors:
bb.resp$force.z <- (bb.resp$force-mean(bb.resp$force,na.rm=TRUE))/sd(bb.resp$force,na.rm=TRUE)
bb.resp$photo.z <- (bb.resp$photo-mean(bb.resp$photo,na.rm=TRUE))/sd(bb.resp$photo,na.rm=TRUE)
bb.resp$chill.z <- (bb.resp$chill-mean(bb.resp$chill,na.rm=TRUE))/sd(bb.resp$chill,na.rm=TRUE)
bb.resp$chill.hrs.z <- (bb.resp$chill.hrs-mean(bb.resp$chill.hrs,na.rm=TRUE))/sd(bb.resp$chill.hrs,na.rm=TRUE)
bb.resp$chill.ports.z <- (bb.resp$chill.ports-mean(bb.resp$chill.ports,na.rm=TRUE))/sd(bb.resp$chill.ports,na.rm=TRUE)
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

