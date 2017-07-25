
##############################################################################################################
# Script and functions to:
#' * Extract GDD from last day of chilling until day of BB - potential response variable  
#'
#'  Ospree
#'  started 21 July 2017
#'  
#'  The file is unfinished!! Still needs to extract correctly the BB date and communicate 
#'  chill data with pmp data
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
chill.day <- read.csv("output/daily_expchill.csv")
chill.unique.exptreat<-unique(chill.day$uniqueID)
#chill.unique.exptreat<-unique(chill.day$ID_exptreat2)

chillneeded <- subset(chill.day, select=c("uniqueID", "lastchilldate"))
chilly <- chillneeded[!duplicated(chillneeded), ]
#head(chill.day)

## read in data for pmp containing climate each day each site
#load("output/fieldclimate_pmp.RData")
#pmp.data <- tempval
#rm(tempval)
#studiesnames <- names(pmp.data)

clima <- read.table("output/pmp/percbb_clim_pmpA.txt", header=TRUE)
climb <- read.csv("output/pmp/percbb_clim_pmpB.csv", header=TRUE)
climc <- read.csv("output/pmp/percbb_clim_pmpC.csv", header=TRUE)
climd <- read.csv("output/pmp/percbb_clim_pmpD.csv", header=TRUE)

climdatab <- rbind(clima,climb,climc,climd)
climdat <- climdatab[!duplicated(climdatab), ] 

## get all the BB data and format a little
dat.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
dat.some <- subset(dat.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(dat.some, response.time!="")
#bbdat.pmp<-read.csv("output/pmp/percbb_bb_pmp.csv", header=TRUE)#

# add a column for when the experiment starts to bb data
# fill it in with either last date of experimental chilling or (if not present) field sample date
#bbdat$ID_exptreat2<-paste(bbdat$datasetID, bbdat$chilltemp, bbdat$chilldays, bbdat$chillphotoperiod, bbdat$forcetemp,
#    bbdat$forcetemp_night, sep=".")
bbdat$uniqueID <- paste(bbdat$datasetID, bbdat$fieldsample.date2, bbdat$forcetemp, bbdat$chilltemp, bbdat$chilldays,
    bbdat$chillphotoperiod, bbdat$photoperiod_day) # this is what becomes stn in the climate data 

setdiff(chilly$uniqueID, bbdat$uniqueID) # ALERT! need to work on this!

bb <- merge(chilly, bbdat, by="ID_exptreat2", all.y=TRUE)
bb$expstartdate <- bb$lastchilldate # subset(bb, is.na(bb$expstartdate)==FALSE)
bb$expstartdate[which(is.na(bb$expstartdate)==TRUE)] <- bb$fieldsample.date2[which(is.na(bb$expstartdate)==TRUE)] 
dim(subset(bb, is.na(bb$expstartdate)==TRUE))

# add a column for when the experiment starts to climate data
addstartdat <- subset(bb, select=c("uniqueID", "expstartdate"))
addstartdat <- addstartdat[!duplicated(addstartdat), ] # down to 1/6 of data ... 
climdat.wstart <- merge(addstartdat, climdat, by.x="uniqueID", by.y="stn", all.y=TRUE) # currently not losing any data ... 
climdat.wstart <- climdat.wstart[which(is.na(climdat.wstart$expstartdate)==FALSE),]
climdat.wstart$date <- as.Date(climdat.wstart$doy2, origin=paste(climdat.wstart$year, "-01-01", sep=""), format="%Y-%m-%d")

climdat.sm <- subset(climdat.wstart, date>expstartdate)
                         
## make GDD column in the climate data


climdat.sm <- climdat.sm[order(climdat.sm$uniqueID, climdat.sm$latitude, climdat.sm$longitude, climdat.sm$year, climdat.sm$doy2),]
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
countcumna <- function(x){cumsum(is.na(x))}
climdat.sm$cumgdd <- NA
climdat.sm$cumgdd <- ave(climdat.sm$Tmean, list(climdat.sm$uniqueID, climdat.sm$latitude, climdat.sm$longitude), FUN=cumsumnona)
climdat.sm$numnas_gdd<- ave(climdat.sm$Tmean, list(climdat.sm$uniqueID, climdat.sm$latitude, climdat.sm$longitude), FUN=countcumna)
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

#there are errors in the below but the errors may go away once cliamte data is correct
for (i in c(1:nrow(bb.wstart))){
    subby <- climdat.sm[which(climdat.sm$uniqueID==bb.wstart$uniqueID[i]),]
    if(nrow(subby)>0){
    bb.wstart$gdd[i] <- subby$cumgdd[which(subby$date==bb.wstart$bbdate[i])]
    bb.wstart$numnas_gdd[i] <- subby$numnas_gdd[which(subby$date==bb.wstart$bbdate[i])]
    }
}


## older code ..
## Nacho started this, then Lizzie took over and wrote the above
## Possible to delete?

GDD.each.study<-rep(NA,length(chill.unique.exptreat))
for(i in 1:length(chill.unique.exptreat)){#i=1
  
  ## define target study
  exp.treat.i<-chill.unique.exptreat[i]
  study.i<-unique(chill.day$datasetID[which(chill.day$ID_exptreat2==exp.treat.i)])
  last.chill.study.i<-unique(chill.day[chill.day$datasetID==study.i,"lastchilldate"])
  
  ## NOTE: the number of unique treatments and unique last chill days differ -- check if makes sense
  
  ## define which elements in pmp.data correspond to target study
  pmp.elements<-which(grepl(study.i,studiesnames))
  treatment.elements<-unique(subset(chill.day,datasetID==study.i)$ID_exptreat2)
  
  if(length(pmp.elemnts)>0)
  
  ## loop across elements and compute GDDs between end-chilling and BB
    gdds.each<-list()
    for(j in 1:length(pmp.elements)){#j=2
      element.j<-pmp.data[[pmp.elements[j]]]
      start.date<-as.Date(last.chill.study.i[j],format="%Y-%m-%d")
      
      ##NOTE: need to get bb date from dataset below, just an example to work the code
      end.date<-as.Date("1983-11-25",format="%Y-%m-%d")
      period<-seq(start.date,end.date,1)
      Temps.avg<-rowMeans(element.j[which(as.Date(element.j$Date,format="%Y-%m-%d")%in%(period-100)),4:5])
      gdds.each[[j]]<-sum(Temps.avg>10)
    }
  
  gdds<-unlist(gdds.each)
  GDD.each.study[[i]]<-gdds[which(!is.na(gdds))]
  
}

## this object is a list that should contain the gdd for each study. Do we append to d?
GDD.each.study



