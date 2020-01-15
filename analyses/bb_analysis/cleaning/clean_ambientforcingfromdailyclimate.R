
##############################################################################################################
# Script and functions to:
#' * Extract GDD from last day of chilling until day of BB - potential response variable  
#'
#'  Ospree
#'  started 21 July 2017
#'  
#'  This file explicitly incorporates ambient and ambient +1, +4, etc. (done by Cat on 29 Oct 2018)
#'  This file now incorporates ambient using the column 'avg_bbtemp' values for forcetemp when the force_type is ambient
#'  This includes ramped data (e.g., "ambient +1" forcing treatments, which are calculated in bb_daily_dataprep.R- the file that creates percbb_dailyclimA-D)
#'  This file does NOT include studies that change the number of hours per day that a forcing treatment was applied (e.g. linksalo06)
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
chill.day <- read.csv("output/dailyclim/daily_expchill.csv")
chill.unique.exptreat<-unique(chill.day$uniqueID)

chillneeded <- subset(chill.day, select=c("uniqueID", "lastchilldate"))
chilly <- chillneeded[!duplicated(chillneeded), ]
#dim(chilly)

## read in data containing climate each day each site (it's big so it is in pieces)
#check the date of when these daily climate summary files were created in case they are older than you'd like:
#file.info("output/dailyclim/percbb_dailyclimA.csv")$ctime
#file.info("output/dailyclim/percbb_dailyclimB.csv")$ctime
#file.info("output/dailyclim/percbb_dailyclimC.csv")$ctime
#file.info("output/dailyclim/percbb_dailyclimD.csv")$ctime
#If those dates are deemed too old by you, then you should rerun
#'pulldailyclim.R' and 'bb_daily_dataprep.R' script (these scripts are slow).
clima <- read.csv("output/dailyclim/percbb_dailyclimA.csv", header=TRUE)
climb <- read.csv("output/dailyclim/percbb_dailyclimB.csv", header=TRUE)
climc <- read.csv("output/dailyclim/percbb_dailyclimC.csv", header=TRUE)
climd <- read.csv("output/dailyclim/percbb_dailyclimD.csv", header=TRUE)
clime <- read.csv("output/dailyclim/percbb_dailyclimE.csv", header=TRUE)
climf <- read.csv("output/dailyclim/percbb_dailyclimF.csv", header=TRUE)
climg <- read.csv("output/dailyclim/percbb_dailyclimG.csv", header=TRUE)
climh <- read.csv("output/dailyclim/percbb_dailyclimH.csv", header=TRUE)
climi <- read.csv("output/dailyclim/percbb_dailyclimI.csv", header=TRUE)
climj <- read.csv("output/dailyclim/percbb_dailyclimJ.csv", header=TRUE)

climdatab <- rbind(clima,climb,climc,climd,clime,climf,climg,climh,climi,climj)
#dim(climdatab)#3331203
climdatab<-climdatab[-which(is.na(climdatab$Tmean)),]#Ailene added for now
#to remove climate data with NAs (need to fix dailyclim file)
#on 25 april 2018: there are 43 unique ids with NA climate data (from 7 studies)
climdat <- climdatab[!duplicated(climdatab), ]#559231 rows...why so much smaller?
#not sure why there are duplicates- duplicates were removed at the end of the bb_daily_dataprep.R code
rm(clima,climb,climc,climd,clime,climf,climg,climh,climi,climj)

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
bb<-bb[-which(is.na(bb$response.time.integer)),]#get rid if response.time==NA
# generate an empty variable to store mean temps
bb$avg_bbtemp<-NA
missingclim<-NA#keep track of how many uniqueIDs are missing climate
## Loop to add mean temp to each line in bb
for(i in 1:nrow(bb)){
  lon.i<-bb[i,"chill.long"]
  lat.i<-bb[i,"chill.lat"]
  start.i<-bb[i,"expstartdate"]
  end.i<-bb[i,"bbdate"]
  year.i<-as.numeric(format(start.i,"%Y"))
  year.end.i<-as.numeric(format(end.i,"%Y"))
  ID.i<-bb[i,'uniqueID']
  doy.i<-as.numeric(format(start.i,"%j"))
  doy.end.i<-as.numeric(format(end.i,"%j"))
  
  clim.i<-subset(climdat, uniqueID==ID.i)
  
  #clim.i$Tmean
  if(dim(clim.i)[1]==0){#if no climate data for any years, avg_bbtemp is NA
    bb$avg_bbtemp[i]<-NA
    missingclim<-c(missingclim,i)
  }
  else if (length(unique(clim.i$year==year.end.i & clim.i$doy==doy.end.i))==1 & unique(clim.i$year==year.end.i & clim.i$doy==doy.end.i)==FALSE){
    bb$avg_bbtemp[i]<-NA
  }#if no climate data for one of required years, avg_bbtemp is NA
  else if(!is.na(year.end.i) & dim(clim.i[which(clim.i$year==year.i & clim.i$doy==doy.i),])[1]!=0){
      print(i)
      bb$avg_bbtemp[i]<-mean(clim.i[which(clim.i$year==year.i & clim.i$doy==doy.i):
           which(clim.i$year==year.end.i & clim.i$doy==doy.end.i),"Tmean"],na.rm=TRUE)
  #clim.i<-subset(clim.i,year==year.i | year==year.end.i)
  }
  }


## checking missing values 
#length(missingclim)
#bb[which(!uniquevalsbb%in%uniquevalsd),c("datasetID","End_year","bbdate","avg_bbtemp")]
## this data is not appended to d, but it is not a problem given that it is all NAs belonging to biasi12

## saving results to output - d
bb.sub <- subset(bb, select=-c(expstartdate, response.time.integer, bbdate, lastchilldate))
d$uniqueID <- paste(d$datasetID, d$fieldsample.date2, d$forcetemp, d$chilltemp, 
                        d$chilldays,d$chillphotoperiod, d$photoperiod_day)
foo <- merge(d, bb.sub, by="uniqueID", all.y=TRUE) ## 7857 rows of data (so subsets down to only data with ambient forcing)
goo <- full_join(d, bb.sub) ## is still full dataset 11959 rows

if(FALSE){
# generate indexes
uniquevalsd <- apply(d,1,paste,collapse="")
uniquevalsbb <- apply(bb[which(names(bb)%in%names(d))],1,paste,collapse="")

indexbb <- which(uniquevalsbb %in% uniquevalsd)
indexd <- which(uniquevalsd %in% uniquevalsbb)

# append average ambient forcing temperature to ospree dataset d 
d$avg_bbtemp <- NA
d[indexd,"avg_bbtemp"] <- bb$avg_bbtemp[indexbb]
}

# note that only 2K rows have entires in avg_bbtemp and all have forcetemp
if(FALSE){
haveavgtemp <- subset(d, is.na(avg_bbtemp)==FALSE)
subset(haveavgtemp, is.na(forcetemp)==TRUE)
}
# but here's what we need
unique(d$forcetemp)
ambientforcetemp <- subset(d, forcetemp=="ambient"|forcetemp=="ambient + .7"|
   forcetemp=="ambient + 4.9")
# sonsteby13, cannell83: no field sample date (also sonsteby13 is flower bud)
# lamb37: field sample in 1946, our climate data start in 1950
# manson91: from New Zealand, so we do not have climate data
# should have ambient temp: boyer, sanzperez10

d$forcetemp<-ifelse(d$forcetemp=="ambient", d$avg_bbtemp, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient + .7", d$avg_bbtemp + .7, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient + 1.5", d$avg_bbtemp + 1.5, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient + 3", d$avg_bbtemp + 3, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient + 4", d$avg_bbtemp + 4, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient + 4.9", d$avg_bbtemp + 4.9, d$forcetemp)

### Updating new data now... 20 September 2019 by Cat
d$forcetemp<-ifelse(d$forcetemp=="ambient + 2.25", d$avg_bbtemp + 2.25, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient + 4.5", d$avg_bbtemp + 4.5, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient + 6.75", d$avg_bbtemp + 6.75, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient + 9", d$avg_bbtemp + 9, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient-1", d$avg_bbtemp-1, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient+0", d$avg_bbtemp + 0, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient+1", d$avg_bbtemp + 1, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient+2", d$avg_bbtemp + 2, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient+3", d$avg_bbtemp + 3, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient+4", d$avg_bbtemp + 4, d$forcetemp)
d$forcetemp<-ifelse(d$forcetemp=="ambient+5", d$avg_bbtemp + 5, d$forcetemp)


stop("Not an error, ambient forcing temperatures are extracted and appended to dataset d; 
     No need to worry about the warnings below, informing of if statement with 2 elements
     out of which only one is utilized")





