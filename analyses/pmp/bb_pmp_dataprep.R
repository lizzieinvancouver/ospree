## Started 13 June 2017 ##
## By Lizzie ##

## File extracts daily climate data for the OSPREE experiments to try PMP ##
## Right now mostly old code from when I was mainly focus on FAGSYL studies ##
## But we should try to do everything now ... ##

## Errors to deal with ... ##
## Check out what is up with the climate mean data not being between and min and max ##
## Check when tmin > tmax and reverse them! ##

## For PMP ##
# Need climate data in form of station, lat, yr, doy, Cmin, Cmax, Cmean (and need full year of data, so for most studies need two full years)
# Need phen data in form of station, population, yr, doy (of BB)
# Output should be TXT files 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
    (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
setwd("~/Documents/git/ospree/analyses")

# Load libraries
library(dplyr)
library(tidyr)
library(plyr)
library(geosphere)

# sourcing
source("source/commoncols.R")

# Get the data, only work with BB data!
dater.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
dater <- subset(dater.all, select=common.cols)
cdater <- read.csv("output/dailytemp.csv", header=TRUE) 
head(cdater)

# and format the dates ...
dater$date <- as.Date(dater$fieldsample.date2, format="%Y-%m-%d")
dater$sample.year <- as.numeric(format(dater$date , "%Y"))
dater$month <- as.numeric(format(dater$date , "%m"))

cdater$Date <- substr(cdater$Date, 1, 10)
cdater$date <- as.Date(cdater$Date, format="%Y-%m-%d")
cdater$year <- as.numeric(format(cdater$date , "%Y"))
cdater$doy <- as.numeric(format(cdater$date , "%j"))
# makes Tmean while we'here ... should check this
cdater$Tmean <- rowMeans(cbind(cdater$Tmin, cdater$Tmax))

# okay, we only want to work with phenology data for which we have climate data ...
dat <- dater[which(dater$datasetID %in% unique(cdater$datasetID)),]

cdat <- cdater[which(cdater$datasetID %in% unique(dat$datasetID)),]
# sigh
dim(dater)
dim(dat)
unique(cdat$datasetID)
unique(dater$datasetID)
unique(dat$datasetID) # well, now, that's not bad 

# note ..
unique(dat$respvar.simple)

# anyway, back to work...
# Getting ambient photoperiod 
cdat$daylength <- NA

for(i in c(1:nrow(cdat))){
    cdat$daylength[i] <- daylength(lat=cdat$lat[i], doy=cdat$date[i])
} # Err, can we do this with an apply command? My loop is really slow.... but it works if you wait a couple minutes

# Back to the phenology data ... 
# We need exp ID and field sample date.
# PMP expects one BB day per year per station by population ... 
# So I think we need each trmt to be a different 'location' eventually or something like that?

dat$uniqueID <- paste(dat$datasetID, dat$fieldsample.date2, dat$forcetemp, dat$chilltemp, dat$chilldays,
    dat$chillphotoperiod, dat$photoperiod_day)

# need to fix year ...
# rule for now: if field sample date < August, then use year + 1, otherwise use year ...
subset(dat, is.na(month)==TRUE) # WTF
dat <- subset(dat, is.na(month)==FALSE)
dat$year <- dat$sample.year
dat$year[dat$month>8] <- dat$sample.year[dat$month>8]+1
dat$year[dat$month<8] <- dat$sample.year[dat$month<8]
#Ailene started ading code here on July 12, 2017
#Now modify the climate data so that it switches from field conditions currently in cdat to 
#experimental conditions. 
head(dat[dat$datasetID=="ashby62",])
head(cdat[cdat$datasetID=="ashby62",])
#how many studies have chilltemp, chilldays, and chillphotoperiod:
chilldat<-dat[which(dat$chilltemp==""|dat$chilltemp=="ambient"),]#studies that do NOT need experimental chilling calculated
expchilldat<-dat[-which(dat$chilltemp==""|dat$chilltemp=="ambient"),]#studies that DO need experimental chilling calculated
cdat$Tmin.chill<-NA#create new column for daily Tmin under experimental conditions; this will remain NA for studies that do not manipulate chilling 
cdat$Tax.chill<-NA##create new column for daily Tmin under experimental conditions; this will remain NA for studies that do not manipulate chilling 
cdat
expchillstudies<-unique(expchilldat$datasetID)#list of studies that do manipulate chilling:13 studies
#dim(dat[-which(dat$chillphotoperiod==""|dat$chillphotoperiod=="ambient"),])#463 rows
#dim(dat[-which(dat$chilldays==""|dat$chillphotoperiod=="ambient"),])#463 rows
#For studies that do experimental chilling, fill in the experimental chilling data and dates
for (i in 1:length(expchillstudies)){
 tempcdat<-cdat[cdat$datasetID==expchillstudies[i],]
 tempdat<-dat[dat$datasetID==expchillstudies[i],] 
 #which(tempcdat$Date>tempcdat$fieldsample.date2)
#try creating new dataframe with same dimensions as cdat, using the data from tempdat
 chilltemps<-unique(tempdat$chilltemp)
 chilldays<-unique(tempdat$chilldays)
 
 for (j in 1:length(chilltemps)){}
 expchillcdat<-
#t
  #tempalldat<-left_join(tempcdat,tempdat, by=c("datasetID","fieldsample.date2"))
  
  chilltemp<-unique(expchilldat[expchilldat$datasetID==expchillstudies[i],]$chilltemp)
  chilldays<-unique(expchilldat[expchilldat$datasetID==expchillstudies[i],]$chilltemp)
  
}

###Try a different approach instead of for loop. merge cdat and dat, then create new colummns
alldat<-left_join(cdat,dat)
#lots of rows added by this join, so something is funny about the matching
head(alldat)

## OLD -- this was Lizzie's work to meet with Inaki in June 2017
## It is just doing Fagus sylvatica .... 
##

# Subset down (for now) to Fagus sylvatica studies
fagsyl <- subset(d, genus=="Fagus" & species=="sylvatica")
unique(fagsyl$datasetID)
unique(paste(fagsyl$datasetID, fagsyl$study))
fag.clim <- cdat[which(cdat$datasetID %in% unique(fagsyl$datasetID)),]
unique(fag.clim$datasetID)

# Slim down the columns in fagsyl
columns.tokeep <- c("datasetID", "ID_chilltreat", "study", "population", "provenance.lat",
     "fieldsample.date",  "forcetemp" , "forcetemp_night", "photoperiod_day" ,  "photoperiod_night",
     "response", "response.time", "respvar.simple", "datasetIDstudy", "chillbyhand", "fieldsample.date2",
      "date", "sample.year", "month")

fagsyl.sm <- subset(fagsyl, select=columns.tokeep)

# Get the phenology data ...
# We need exp ID and field sample date? And we need each trmt to be a different location eventually...
fag.phen <- fagsyl.sm
fag.phen$uniqueID <- paste(fag.phen$datasetID, fag.phen$fieldsample.date2, fag.phen$forcetemp)

# subset down to budburst ... should update this eventually!
fag.phen.bb <- subset(fag.phen, respvar.simple=="daystobudburst")

# need to fix year ...
# rule for now: if field sample date < August, then use year + 1, otherwise use year ...
fag.phen.bb.sm <- subset(fag.phen.bb, is.na(month)==FALSE)
fag.phen.bb.sm$year <- fag.phen.bb.sm$sample.year
fag.phen.bb.sm$year[fag.phen.bb.sm$month>8] <- fag.phen.bb.sm$sample.year[fag.phen.bb.sm$month>8]+1
fag.phen.bb.sm$year[fag.phen.bb.sm$month<8] <- fag.phen.bb.sm$sample.year[fag.phen.bb.sm$month<8]

# We need exp ID and field sample date? And we need each trmt to be a different location eventually...
fagus <- merge(fag.clim, fagsyl.sm, by=c("datasetID", "fieldsample.date2"), all.x=TRUE)
dim(fag.clim)
dim(fagus) ## hmm, definitely do not use this for the climate data (maybe try join instead of

# for now summarize to studies that vary only forcing temps
fagus.exp.types <-
      ddply(fag.phen.bb.sm, c("datasetID", "study"), summarise,
      n.photo.exps = length(unique(photoperiod_day)),
      n.temp.exps = length(unique(forcetemp)))

# ohdear, really only the fu13 study works for now... 
fu13 <- subset(fag.phen.bb.sm, datasetID=="fu13")

# summarizing data to try to answer this ... 
fagus.studies.summary <-
      ddply(fagus, c("datasetID", "fieldsample.date2"), summarise,
      n.exps = length(unique(study)))
fagus.samples.summary <-
      ddply(fagus, c("datasetID", "study"), summarise,
      n.exps = length(unique(fieldsample.date2)))
# I think we are okay with just fieldsample.date2

# Need climate data in form of station, lat, yr, doy, Cmin, Cmax, Cmean
fag.clim$uniqueID <- paste(fag.clim$datasetID, fag.clim$fieldsample.date2)
fag.clim.pmp <- subset(fag.clim, select=c("uniqueID", "lat", "year", "doy", "Tmin", "Tmax", "Tmean"))
names(fag.clim.pmp) <- c("stn", "latitude", "year", "doy", "Tmin", "Tmax", "Tmean")
fag.clim.pmp <- subset(fag.clim.pmp, stn=="fu13 2010-12-01" |stn=="fu13 2011-12-01")

# We have the data two sample dates, we need it now for 4 treatments which are all 2010 collections
# Very cheap approach below!
fu.amb1 <- subset(fag.clim.pmp, stn=="fu13 2011-12-01")
fu.amb2 <- subset(fag.clim.pmp, stn=="fu13 2011-12-01")
fu.amb3 <- subset(fag.clim.pmp, stn=="fu13 2011-12-01")
fu.amb4 <- subset(fag.clim.pmp, stn=="fu13 2011-12-01")
fu.amb5 <- subset(fag.clim.pmp, stn=="fu13 2011-12-01")
fu.amb6 <- subset(fag.clim.pmp, stn=="fu13 2011-12-01")
fu.amb1$Tmin[fu.amb1$year==2011 & fu.amb1$doy>333] <- fu.amb1$Tmin[fu.amb1$year=="2011" & fu.amb1$doy>333]+1
fu.amb1$Tmin[fu.amb1$year=="2012"] <- fu.amb1$Tmin[fu.amb1$year=="2012"]+2
fu.amb2$Tmin[fu.amb2$year=="2011" & fu.amb2$doy>333] <- fu.amb2$Tmin[fu.amb2$year=="2011" & fu.amb2$doy>333]+1
fu.amb2$Tmin[fu.amb2$year=="2012"] <- fu.amb2$Tmin[fu.amb2$year=="2012"]+2
fu.amb3$Tmin[fu.amb3$year=="2011" & fu.amb3$doy>333] <- fu.amb3$Tmin[fu.amb3$year=="2011" & fu.amb3$doy>333]+3
fu.amb3$Tmin[fu.amb3$year=="2012"] <- fu.amb3$Tmin[fu.amb3$year=="2012"]+3
fu.amb4$Tmin[fu.amb4$year=="2011" & fu.amb4$doy>333] <- fu.amb4$Tmin[fu.amb4$year=="2011" & fu.amb4$doy>333]+4
fu.amb4$Tmin[fu.amb4$year=="2012"] <- fu.amb4$Tmin[fu.amb4$year=="2012"]+4
fu.amb5$Tmin[fu.amb5$year=="2011" & fu.amb5$doy>333] <- fu.amb5$Tmin[fu.amb5$year=="2011" & fu.amb5$doy>333]+5
fu.amb5$Tmin[fu.amb5$year=="2012"] <- fu.amb5$Tmin[fu.amb5$year=="2012"]+5
fu.amb6$Tmin[fu.amb6$year=="2011" & fu.amb6$doy>333] <- fu.amb6$Tmin[fu.amb6$year=="2011" & fu.amb6$doy>333]+6
fu.amb6$Tmin[fu.amb6$year=="2012"] <- fu.amb6$Tmin[fu.amb6$year=="2012"]+6


# 1, 2, 5, 6 for 2010 
fu.amb1.2010 <- subset(fag.clim.pmp, stn=="fu13 2010-12-01")
fu.amb2.2010 <- subset(fag.clim.pmp, stn=="fu13 2010-12-01")
fu.amb5.2010 <- subset(fag.clim.pmp, stn=="fu13 2010-12-01")
fu.amb6.2010 <- subset(fag.clim.pmp, stn=="fu13 2010-12-01")
fu.amb1$Tmin[fu.amb1$year==2010 & fu.amb1$doy>333] <- fu.amb1$Tmin[fu.amb1$year=="2010" & fu.amb1$doy>333]+1
fu.amb1$Tmin[fu.amb1$year=="2011"] <- fu.amb1$Tmin[fu.amb1$year=="2011"]+2
fu.amb2$Tmin[fu.amb2$year=="2010" & fu.amb2$doy>333] <- fu.amb2$Tmin[fu.amb2$year=="2010" & fu.amb2$doy>333]+1
fu.amb2$Tmin[fu.amb2$year=="2011"] <- fu.amb2$Tmin[fu.amb2$year=="2011"]+2
fu.amb5$Tmin[fu.amb5$year=="2010" & fu.amb5$doy>333] <- fu.amb5$Tmin[fu.amb5$year=="2010" & fu.amb5$doy>333]+5
fu.amb5$Tmin[fu.amb5$year=="2011"] <- fu.amb5$Tmin[fu.amb5$year=="2011"]+5
fu.amb6$Tmin[fu.amb6$year=="2010" & fu.amb6$doy>333] <- fu.amb6$Tmin[fu.amb6$year=="2010" & fu.amb6$doy>333]+6
fu.amb6$Tmin[fu.amb6$year=="2011"] <- fu.amb6$Tmin[fu.amb6$year=="2011"]+6

# Now, label the stations ... 
fu.amb1$stn <- "fu13 2011-12-01 ambient+1"
fu.amb2$stn <- "fu13 2011-12-01 ambient+2" 
fu.amb3$stn <- "fu13 2011-12-01 ambient+3" 
fu.amb4$stn <- "fu13 2011-12-01 ambient+4"
fu.amb5$stn <- "fu13 2011-12-01 ambient+5"
fu.amb6$stn <-  "fu13 2011-12-01 ambient+6"
fu.amb1.2010$stn <- "fu13 2010-12-01 ambient+1"
fu.amb2.2010$stn <- "fu13 2010-12-01 ambient+2"
fu.amb5.2010$stn <- "fu13 2010-12-01 ambient+5"
fu.amb6.2010$stn <- "fu13 2010-12-01 ambient+6"

fag.clim.pmp$stn[fag.clim.pmp$stn=="fu13 2010-12-01"] <- "fu13 2010-12-01 ambient"
fag.clim.pmp$stn[fag.clim.pmp$stn=="fu13 2011-12-01"] <- "fu13 2011-12-01 ambient"

# And bind together ...
fu13.clim.pmp <- rbind(fag.clim.pmp, fu.amb1.2010, fu.amb2.2010, fu.amb5.2010, fu.amb6.2010,
    fu.amb1, fu.amb2, fu.amb3, fu.amb4, fu.amb5, fu.amb6)

# And check 
sort(unique(fu13$uniqueID))
sort(unique(fu13.clim.pmp$stn))

# End very cheap approach.

# ... cleanup 
subset(fu13.clim.pmp, is.na(Tmin)==TRUE)
fu13.clim.pmp[is.na(fu13.clim.pmp)] <- -9999

# prep BB data for PMP
fu13$doy <- as.integer(fu13$response.time)
fu13.pmp <- subset(fu13, select=c("uniqueID", "year", "doy"))
names(fu13.pmp) <- c("stn", "year", "doy")

write.csv(fu13.clim.pmp, "output/pmp/fu13.clim.pmp.csv", row.names=FALSE)
write.csv(fu13.pmp, "output/pmp/fu13.bb.pmp.csv", row.names=FALSE)
