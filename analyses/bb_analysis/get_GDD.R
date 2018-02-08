
##############################################################################################################
# Script and functions to:
#' * Extract GDD from last day of chilling until day of BB - potential response variable  
#'
#'  Ospree
#'  started 21 July 2017
#'  
#'  This file is unfinished -- need to deal with Sstudies.need.attention -- Swartz 81 -
#'  in that study climate data is lacking for the second year
##############################################################################################################


## to start
rm(list=ls())
options(stringsAsFactors=FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")

## read in last day of chilling and format a little
chill.day <- read.csv("output/dailyclim/daily_expchill.csv")
chill.unique.exptreat<-unique(chill.day$uniqueID)
#chill.unique.exptreat<-unique(chill.day$ID_exptreat2)
#check the date of when this file was created
file.info("output/daily_expchill.csv")$ctime
#If the dates are deemed too old by you, then you should rerun
#'bb_daily_dataprep.R' script (this script is slow)."
chillneeded <- subset(chill.day, select=c("uniqueID", "lastchilldate"))
chilly <- chillneeded[!duplicated(chillneeded), ]
#head(chilly)

## read in daily climate data for each site
#load("output/dailyclim/fieldclimate_daily.RData")
#dailyclim.data <- tempval
#rm(tempval)
#studiesnames <- names(dailyclim.data)
#check the date of when these daily climate summary files were created in case they are older than you'd like:
file.info("output/dailyclim/percbb_dailyclimA.csv")$ctime
file.info("output/dailyclim/percbb_dailyclimB.csv")$ctime
file.info("output/dailyclim/percbb_dailyclimC.csv")$ctime
file.info("output/dailyclim/percbb_dailyclimD.csv")$ctime
#If those dates are deemed too old by you, then you should rerun
#'bb_daily_dataprep.R' script (this script is slow)."
clima <- read.csv("output/dailyclim/percbb_dailyclimA.csv", header=TRUE)
climb <- read.csv("output/dailyclim/percbb_dailyclimB.csv", header=TRUE)
climc <- read.csv("output/dailyclim/percbb_dailyclimC.csv", header=TRUE)
climd <- read.csv("output/dailyclim/percbb_dailyclimD.csv", header=TRUE)

climdatab <- rbind(clima,climb,climc,climd)
climdat <- climdatab[!duplicated(climdatab), ] 
rm(clima,climb,climc,climd)

## get all the BB data and format a little
dat.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
dat.some <- subset(dat.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(dat.some, response.time!="")

# add a column for when the experiment starts to bb data
# fill it in with either last date of experimental chilling or (if not present) field sample date
#bbdat$ID_exptreat2<-paste(bbdat$datasetID, bbdat$chilltemp, bbdat$chilldays, bbdat$chillphotoperiod, bbdat$forcetemp,
#    bbdat$forcetemp_night, sep=".")
bbdat$uniqueID <- paste(bbdat$datasetID, bbdat$fieldsample.date2, bbdat$forcetemp, bbdat$chilltemp, bbdat$chilldays,
    bbdat$chillphotoperiod, bbdat$photoperiod_day) # this is what becomes stn in the climate data 

setdiff(chilly$uniqueID, bbdat$uniqueID) # ALERT! need to work on this!

bb <- merge(chilly, bbdat, by="uniqueID", all.y=TRUE)
bb$expstartdate <- bb$lastchilldate # subset(bb, is.na(bb$expstartdate)==FALSE)
bb$expstartdate[which(is.na(bb$expstartdate)==TRUE)] <- bb$fieldsample.date2[which(is.na(bb$expstartdate)==TRUE)] 
#dim(subset(bb, is.na(bb$expstartdate)==TRUE))

# add a column for when the experiment starts to climate data
addstartdat <- subset(bb, select=c("uniqueID", "expstartdate"))
addstartdat <- addstartdat[!duplicated(addstartdat), ] # down to 1/6 of data ... 
climdat.wstart <- merge(addstartdat, climdat, by.x="uniqueID", by.y="stn", all.y=TRUE) # currently not losing any data ... 
climdat.wstart <- climdat.wstart[which(is.na(climdat.wstart$expstartdate)==FALSE),]
climdat.wstart$date <- as.Date(climdat.wstart$doy, origin=paste(climdat.wstart$year, "-01-01", sep=""), format="%Y-%m-%d")

climdat.sm <- subset(climdat.wstart, date>=expstartdate)
                         

## make GDD column in the climate data

climdat.sm <- climdat.sm[order(climdat.sm$uniqueID, climdat.sm$latitude, climdat.sm$longitude, climdat.sm$year, climdat.sm$doy),]
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
countcumna <- function(x){cumsum(is.na(x))}
climdat.sm$cumgdd <- NA
climdat.sm$cumgdd <- ave(climdat.sm$Tmean, list(climdat.sm$uniqueID, climdat.sm$latitude, climdat.sm$longitude), FUN=cumsumnona)
climdat.sm$numnas_gdd<- ave(climdat.sm$Tmean, list(climdat.sm$uniqueID, climdat.sm$latitude, climdat.sm$longitude), FUN=countcumna)

#head(climdat.sm,100)
# watch out ...
whereartthouna <- subset(climdat.sm, numnas_gdd>0)

## okay, need to get response.time and expstartdate into date formats...
bb$expstartdate <- as.Date(bb$expstartdate, format="%Y-%m-%d")
bb$response.time.integer <- as.integer(bb$response.time)
bb$bbdate <- as.Date(bb$response.time.integer, origin=bb$expstartdate, format="%Y-%m-%d")

bb.wstart <- subset(bb, is.na(bb$expstartdate)==FALSE)
bb.wstart$gdd <- NA
bb.wstart$numnas_gdd <- NA
#we should be able to delete the below line when climate data is correct
bb.wstart <- subset(bb.wstart, datasetID != "falusi97" & datasetID != "heide93" & datasetID !="partanen05" & datasetID !="ramos99") 

##
##

#head(bb.wstart)

#there are errors in the below but the errors may go away once cliamte data is correct
studies.need.no.attention<-list()
studies.need.attention<-list()

for (i in c(1:nrow(bb.wstart))){#i=c(1:nrow(bb.wstart));i=1
  print(i)
    subby <- climdat.sm[which(climdat.sm$uniqueID==bb.wstart$uniqueID[i]),]
    if(nrow(subby)>0 & length(which(subby$date==bb.wstart$bbdate[i]))>0){
    bb.wstart$gdd[i] <- subby$cumgdd[which(subby$date==bb.wstart$bbdate[i])]
    bb.wstart$numnas_gdd[i] <- subby$numnas_gdd[which(subby$date==bb.wstart$bbdate[i])]
    studies.need.no.attention[[i]]<-i
    } 
    if(nrow(subby)>0 & length(which(subby$date==bb.wstart$bbdate[i]))==0) {
      studies.need.attention[[i]]<-i
    }
}

studies.need.attention<-bb.wstart$uniqueID[unlist(studies.need.attention)] ## studies that need attention (all in swartz81)

#head(bb.wstart)
#sum(!is.na(bb.wstart$gdd))
#hist(bb.wstart$numnas_gdd)

## saving results to output -
write.csv(bb.wstart,"output/bbdata_wgdd.csv")



