
##############################################################################################################
# Script and functions to:
#' * Extract GDD from last day of chilling until day of BB - potential response variable  
#'
#'  Ospree
#'  started 21 July 2017
#'  
#'  This file is unfinished -- need to explicitly incorporate ambient and ambient +1, +4, etc.
##############################################################################################################

#if(FALSE){
## to start
#rm(list=ls())
#options(stringsAsFactors=FALSE)

# Set working directory: 
#if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
#} else if
#(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
#} else if
#(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
#} else 
#  setwd("~/Documents/git/ospree/analyses")
#}    

## read in last day of chilling and format a little
chill.day <- read.csv("output/daily_expchill.csv")
chill.unique.exptreat<-unique(chill.day$uniqueID)

chillneeded <- subset(chill.day, select=c("uniqueID", "lastchilldate"))
chilly <- chillneeded[!duplicated(chillneeded), ]
#dim(chilly)

## read in data containing climate each day each site (it's big so it is in pieces)
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
#dat.all <- read.csv("output/ospree_clean_withchill.csv", header=TRUE)
dat.all<-d
dat.some <- subset(dat.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(dat.some, response.time!="")

# format: make a column to match to climate data, and merge the BB and climate data
bbdat$uniqueID <- paste(bbdat$datasetID, bbdat$fieldsample.date2, bbdat$forcetemp, bbdat$chilltemp, 
    bbdat$chilldays,bbdat$chillphotoperiod, bbdat$photoperiod_day)  
bb <- merge(chilly, bbdat, by="uniqueID", all.y=TRUE)

## add a column for when the experiment starts to bb data
# fill it in with either last date of experimental chilling or (if not present) field sample date
bb$expstartdate <- bb$lastchilldate # subset(bb, is.na(bb$expstartdate)==FALSE)
bb$expstartdate[which(is.na(bb$expstartdate)==TRUE)] <- bb$fieldsample.date2[which(is.na(bb$expstartdate)==TRUE)] 
#dim(subset(bb, is.na(bb$expstartdate)==TRUE))

## okay, need to get response.time and expstartdate into date formats...
bb$expstartdate <- as.Date(bb$expstartdate, format="%Y-%m-%d")
bb$response.time.integer <- as.integer(bb$response.time)
bb$bbdate <- as.Date(bb$response.time.integer, origin=bb$expstartdate, format="%Y-%m-%d")

# generate an empty variable to store mean temps
bb$avg_bbtemp<-NA

## Loop to add mean temp to each line in bb
for(i in 1:nrow(bb)){#i=1832
  lon.i<-bb[i,"chill.long"]
  lat.i<-bb[i,"chill.lat"]
  start.i<-bb[i,"expstartdate"]
  end.i<-bb[i,"bbdate"]
  year.i<-as.numeric(format(start.i,"%Y"))
  year.end.i<-as.numeric(format(end.i,"%Y"))
  ID.i<-bb[i,'uniqueID']
  doy.i<-as.numeric(format(start.i,"%j"))
  doy.end.i<-as.numeric(format(end.i,"%j"))
  
  clim.i<-subset(climdat,stn==ID.i)
  
  #clim.i$Tmean
  if(!is.na(year.end.i)){
  if(nrow(clim.i)>0 & sum(!is.na(clim.i$Tmean))>0){
      print(i)
      bb$avg_bbtemp[i]<-mean(clim.i[which(clim.i$year==year.i & clim.i$doy==doy.i):
           which(clim.i$year==year.end.i & clim.i$doy==doy.end.i),"Tmean"],na.rm=T)
  #clim.i<-subset(clim.i,year==year.i | year==year.end.i)
  }
  }
}


## checking missing values 
#bb[which(!uniquevalsbb%in%uniquevalsd),c("datasetID","End_year","bbdate","avg_bbtemp")]
## this data is not appended to d, but it is not a problem given that it is all NAs belonging to biasi12

## saving results to output - d

# generate indexes
uniquevalsd<-apply(d,1,paste,collapse="")
uniquevalsbb<-apply(bb[which(names(bb)%in%names(d))],1,paste,collapse="")

indexbb<-which(uniquevalsbb%in%uniquevalsd)
indexd<-which(uniquevalsd%in%uniquevalsbb)


# append average ambient forcing temperature to ospree dataset d 
d$avg_bbtemp<-NA
d[indexd,"avg_bbtemp"]<-bb$avg_bbtemp[indexbb]



stop("Not an error, ambient forcing temperatures are extracted and appended to dataset d; 
     No need to worry about the warnings below, informing of if statement with 2 elements
     out of which only one is utilized")





