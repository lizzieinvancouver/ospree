## Started 13 June 2017 ##
## By Lizzie ##
##Added to by Ailene started on July 12, 2017
#General approach is:
#1. Create two daily climate datasets to pull from:
#a) ambient climate data 
#b) experimental chilling data (uses chilltemp, chilldays, chillphotoperiod to create daily experimental chilling data)
#2. Using the budburst data only, expand each row of data (which is a budburst event) to include 
#daily climate data up to that date,choosing ambient climate data or experimental chilling data, as appropriate,
#and then filling in daily experimental climate data for forcing, if necessary

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
#if chilling code was just run, you will need to do this:
detach("package:chillR", unload=TRUE)

# sourcing
source("source/commoncols.R")


# Get the data, only work with BB data!
dater.all <- read.csv("output/ospree_clean_withchill.csv", header=TRUE)
dater <- subset(dater.all, select=common.cols)
cdater <- read.csv("output/dailyclim/dailytemp.csv", header=TRUE) 

#1a. Format the ospree phenology datafile so that it can be connected to the climate data
source("bb_dailyclimate/source/bb_daily_dataprep_format_bbdat.R")

#1b. Format the ambient daily climate data (pulled in pulldailyclim.R) 
source("bb_dailyclimate/source/bb_daily_dataprep_format_climdat.R")

#1c. Create files with experimental chilling and photoperiod conditions.
source("bb_dailyclimate/source/bb_daily_dataprep_get_expclimdat.R")

#2. Make monster daily climate file with daily data for each budburst event date.
#The below loop takes a while....
#I'm not sure if it deals with the following:
#A.  multiple values for forcing treatments (e.g. "mean of 9, 12, 15","7-27.5")
#B. studies that manipulate ONLY photoperiod
#First, select out budburst data
dat.bb<-dat[dat$respvar.simple=="percentbudburst"|dat$respvar.simple=="daystobudburst",]
dat.bb<-dat.bb[-which(dat.bb$response.time=="no response"),]#i think this is ok to do...
dailyclim.bb<-data.frame()

for(i in 1:dim(dat.bb)[1]){#4637rows in dat#need to fix rows 3071:3244 (provenance.lat provenance.long=NA for all sanzperez10, should be 40.47, -3.37)
  print(i)
  x<-dat.bb[i,]#focal budburst event
  colnames(x)[9:10]<-c("lat","long")#match column names to climate data column names
  x$lat<-round(x$lat, digits=5)
  x$long<-round(x$long, digits=4)
  
  daystobb<-round(as.numeric(x$response.time), digits=0)
  #If no experimental chilling for focal budburst event, use ambient climate data
  if(x$chilltemp==""|x$chilltemp=="ambient"|x$chilldays==0){#select out ambient climate data from same datasetID, fieldsampledate, lat and long
    x.dailyclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & daily_ambtemp$lat==x$lat & daily_ambtemp$long==x$long,]
    #if no experimental forcing, no need to add anything:
    if(x$forcetemp=="ambient"|x$forcetemp==""|x$forcetemp=="meandaily"){
      x.all<-join(x,x.dailyclim)
      }
    #for studies with forcing that warms 4 degrees above ambient:
    if(x$forcetemp=="ambient + 4"|x$forcetemp=="ambient+4"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+4
      x.dailyclim$Tmax<-x.dailyclim$Tmax+4
      x.all<-join(x,x.dailyclim)
      }
    #for studies with forcing that warms 1.5 degrees above ambient:
    if(x$forcetemp=="ambient + 1.5"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+1.5
      x.dailyclim$Tmax<-x.dailyclim$Tmax+1.5
      x.all<-join(x,x.dailyclim)
      }
    #for studies with forcing that warms 3 degrees above ambient:
    if(x$forcetemp=="ambient + 3"|x$forcetemp=="ambient+3"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+3
      x.dailyclim$Tmax<-x.dailyclim$Tmax+3
      x.all<-join(x,x.dailyclim)
      }
    #for studies with forcing that warms 1 degree above ambient:
    if(x$forcetemp=="ambient+1"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+1
      x.dailyclim$Tmax<-x.dailyclim$Tmax+1
      x.all<-join(x,x.dailyclim)
      }
    #for studies with forcing that warms 2 degrees above ambient:
    if(x$forcetemp=="ambient+2"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+2
      x.dailyclim$Tmax<-x.dailyclim$Tmax+2
      x.all<-join(x,x.dailyclim)
      }
    #for studies with forcing that warms 5 degrees above ambient:
    if(x$forcetemp=="ambient+5"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+5
      x.dailyclim$Tmax<-x.dailyclim$Tmax+5
      x.all<-join(x,x.dailyclim)
      }
    #for studies with forcing that warms 6 degrees above ambient:
    if(x$forcetemp=="ambient+6"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+6
      x.dailyclim$Tmax<-x.dailyclim$Tmax+6
      x.all<-join(x,x.dailyclim)
      } #if there is other experimental forcing,  add it using the forcetemp, field sample date and response.time columns
    if(x$forcetemp_night =="ambient"){#one study (Sanz-Perez09) has ambient conditions at night but not during the day
      x.dailyclim$Tmin<-x.dailyclim$Tmin
      forcetmax<-x$forcetemp
      daystobb<-round(as.numeric(x$response.time), digits=0)
      forcedays<-dim(x.dailyclim[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2),])[1]##number of days of that should be replaced with the forcing treatment- this is the nubmer of rows after the field sample date because all of these should be replaced with forcing data number of days after field sample date-
      if (!is.na(x$photoperiod_day)){
        if(x$photoperiod_day !="ambient") {forcephoto<-x$photoperiod_day}
        }
      x.dailyclim$Tmax[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmax, times=forcedays)
    }
    
    if(substr(x$forcetemp,1,7)!="ambient" & x$forcetemp!="" & x$forcetemp!="meandaily" & x$forcetemp_night!="ambient"){
      forcetmax<-x$forcetemp
      if(x$forcetemp_night==""){
        forcetmin<-x$forcetemp
        } else if(x$forcetemp_night=="10 then decreased 1.1C every two days until -13°C"){
          forcetmin<- -13 
        } else if (x$forcetemp_night=="2 for 2 weeks"){
          forcetmin<-2
        } else forcetmin<-x$forcetemp_night
        daystobb<-round(as.numeric(x$response.time), digits=0)
        forcedays<-dim(x.dailyclim[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2),])[1]##number of days of that should be replaced with the forcing treatment- this is the nubmer of rows after the field sample date because all of these should be replaced with forcing data number of days after field sample date-
        if (!is.na(x$photoperiod_day)){
          if (x$photoperiod_day !="ambient"){forcephoto<-x$photoperiod_day}
          }
      #Replace tmin and tmax from ambient climate when Date >fieldsample.date2 with experimentalforcing climate
      x.dailyclim$Tmin[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmin, times=forcedays)
      if(x$forcetemp_night=="2 for 2 weeks"){#for trial 4 of schnabel87: "#after 2 weeks,temp was decreased to -1C for 5 d, then -5C for 5d
        x.dailyclim$Tmin[as.Date(x.dailyclim$Date)>(as.Date(x.dailyclim$fieldsample.date2)+14) & as.Date(x.dailyclim$Date)<(as.Date(x.dailyclim$fieldsample.date2)+20)]<-"-1"
        x.dailyclim$Tmin[as.Date(x.dailyclim$Date)>(as.Date(x.dailyclim$fieldsample.date2)+19) & as.Date(x.dailyclim$Date)<(as.Date(x.dailyclim$fieldsample.date2)+25)]<-"-5"
      }
      if(x$forcetemp_night=="10 then decreased 1.1C every two days until -13°C"){#for trial 1 of schnabel87: 
        tmins<-sort(c(seq(from=10, to=-13,by=-1.1),seq(from=10, to=-13,by=-1.1)), decreasing=TRUE)
        x.dailyclim$Tmin[as.Date(x.dailyclim$Date)>(as.Date(x.dailyclim$fieldsample.date2)) & as.Date(x.dailyclim$Date)<(as.Date(x.dailyclim$fieldsample.date2)+1+length(tmins))]<-tmins
      }
      x.dailyclim$Tmax[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmax, times=forcedays)
      
      if (!is.na(x$photoperiod_day)){
        if(x$photoperiod_day !="ambient"){x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcephoto, times=forcedays)}
        if(x$photoperiod_day=="13-9.5"){#for trial 4 of schnabel87
          forcephotos1<-c(seq(from=13, to=9.5,by=-.25),seq(from=13, to=9.5,by=-.25))
          forcephotos<-c(sort(forcephotos1, decreasing=TRUE),rep(9.5, times=forcedays-length(forcephotos1)))
          x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-forcephotos
        }
        if(x$photoperiod_day=="14-9.5"){#for trial 2 of schnabel87
          forcephotos1<-c(seq(from=14, to=9.5,by=-.25),seq(from=14, to=9.5,by=-.25))
          forcephotos<-c(sort(forcephotos1, decreasing=TRUE),rep(9.5, times=forcedays-length(forcephotos1)))
          x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-forcephotos
        }
        }
      x.all<-join(x,x.dailyclim)#end of work on studies with no experimental chilling
    }
#If there is experimental chilling
  } else if(!is.na(as.numeric(x$chilltemp))){#if the chilltemp is a single number, then use a combination of the ambient climate data and the experimental chilling data
    x.ambclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & daily_ambtemp$lat==x$lat & daily_ambtemp$long==x$long,]
    #select experimental chilling climate data
    x.expclim<-daily_chilltemp3[daily_chilltemp3$datasetID==x$datasetID & daily_chilltemp3$ID_exptreat2==x$ID_exptreat2 & daily_chilltemp3$fieldsample.date2==x$fieldsample.date2 & daily_chilltemp3$lat==x$lat & daily_chilltemp3$long==x$long,]
    firstchilldate<-min(as.Date(x.expclim$Date))
    lastchilldate<-unique(x.expclim$lastchilldate)
    if(max(as.Date(x.ambclim$Date))==as.Date(firstchilldate)-1){#if last date of ambient climate data is right before first date of chilling climate data, then just add chilling and forcing data below it
      #make columns match ambient and expclim by removing ID_exptreat2 column
      x.expclim<-x.expclim[,-which(colnames(x.expclim)=="ID_exptreat2")]
      x.expclim<-x.expclim[,-which(colnames(x.expclim)=="lastchilldate")]
      x.expclim$Date<-as.Date(x.expclim$Date)
      x.ambclim$Date<-as.Date(x.ambclim$Date)
      x.allclim<-rbind(x.ambclim,x.expclim)
      x.allclim<-x.allclim[order(x.allclim$Date),]
      #now add forcing
      if(substr(x$forcetemp,1,7)!="ambient" & x$forcetemp!=""& x$forcetemp!="meandaily"& x$forcetemp_night!="ambient"){
        forcetmax<-x$forcetemp
        if(x$forcetemp_night==""){
          forcetmin<-x$forcetemp
        } else forcetmin<-x$forcetemp_night
        daystobb<-round(as.numeric(x$response.time), digits=0)
        forcedays<-dim(x.dailyclim[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2),])[1]##number of days of that should be replaced with the forcing treatment- this is the nubmer of rows after the field sample date because all of these should be replaced with forcing data number of days after field sample date-
        if (!is.na(x$photoperiod_day)){
          if(x$photoperiod_day !="ambient") {forcephoto<-x$photoperiod_day}
        #Replace rows from ambient climate when Date >fieldsample.date2 with experimentalforcing climate
        x.dailyclim$Tmin[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmin, times=forcedays)
        x.dailyclim$Tmax[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmax, times=forcedays)
          if(x$photoperiod_day !="ambient"){
            x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcephoto, times=forcedays)}
          }
        x.all<-join(x,x.dailyclim)
      }
      } else if (!is.na(firstchilldate) & max(as.Date(x.ambclim$Date))>as.Date(firstchilldate)-1){#if ambient data goes beyond experimental chilling data (which it should once the climate pulling code is correct)
      x.dailyclim<-x.ambclim#ambient climate data,
      #Replace tmin and tmax columns with experimental climate when Date >fieldsample.date2 and when Date <lastchilldate with experimental chilling climate
      x.dailyclim$Tmin[as.Date(x.dailyclim$Date) > as.Date(x.dailyclim$fieldsample.date2) & as.Date(x.dailyclim$Date) < as.Date(lastchilldate)]<-x.expclim$Tmin
      x.dailyclim$Tmax[as.Date(x.dailyclim$Date) > as.Date(x.dailyclim$fieldsample.date2) & as.Date(x.dailyclim$Date) < as.Date(lastchilldate)]<-x.expclim$Tmax
      #warning message, but its ok
      if (!is.na(x$photoperiod_day)){
        if(x$photoperiod_day !="ambient"){x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-x.expclim$daylength}
      }
      #now add forcing
      if(substr(x$forcetemp,1,7)!="ambient" & x$forcetemp!="" & x$forcetemp!="meandaily"& x$forcetemp_night !="ambient"){
        forcetmax<-x$forcetemp
        if(x$forcetemp_night==""){
          forcetmin<-x$forcetemp
        } else forcetmin<-x$forcetemp_night
        daystobb<-round(as.numeric(x$response.time), digits=0)
        forcedays<-dim(x.dailyclim[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2),])[1]##number of days of that should be replaced with the forcing treatment- this is the nubmer of rows after the field sample date because all of these should be replaced with forcing data number of days after field sample date-
        if (!is.na(x$photoperiod_day)){
          if(x$photoperiod_day !="ambient") {forcephoto<-x$photoperiod_day}
        }
        #Replace rows from ambient climate when Date >lastchilldate with experimentalforcing climate
        x.dailyclim$Tmin[as.Date(x.dailyclim$Date)>as.Date(lastchilldate)]<-rep(forcetmin, times=forcedays)
        x.dailyclim$Tmax[as.Date(x.dailyclim$Date)>as.Date(lastchilldate)]<-rep(forcetmax, times=forcedays)
        if (!is.na(x$photoperiod_day)){
          if(x$photoperiod_day !="ambient"){x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(lastchilldate)]<-rep(forcephoto, times=forcedays)}
        }
        }
        x.all<-join(x,x.dailyclim)
        }
    }#end of work on studies with experimental chilling
  #make sure dates are formatted as dates
  x.all$Date<-as.Date(x.all$Date)
  dailyclim.bb<-rbind(dailyclim.bb,x.all)
}
#some checks of this file:
#sort(unique(dailyclim.bb$datasetID))#41 different studies
#sort(unique(dat$datasetID))#52 studies in full database
#dim(dailyclim.bb)#4041751     33#HUGE! but this makes sense given that the dat (percbb data file) was 4231 rows (4231*2*365= 3088630)
dim(dailyclim.bb)
dailyclim.bb2 <- dailyclim.bb[!duplicated(dailyclim.bb), ]
dim(dailyclim.bb2)#3331203 rows
#save daily climate data
dailyclim.bb2$year2<-as.numeric(format(dailyclim.bb2$Date , "%Y"))#year for climate data
dailyclim.bb2$doy2<-as.numeric(format(dailyclim.bb2$Date , "%j"))#doy for climate data
dailyclim.bb2$Tmin<-as.numeric(dailyclim.bb2$Tmin)#lose "skuterud94" (mean of ...) and Sanz-Perez09 here- Tmin=="ambient" why? Tmax is 12
dailyclim.bb2$Tmax<-as.numeric(dailyclim.bb2$Tmax)
#Ok, here is the issue: tail(sort(unique(dailyclim.bb$Tmax)))#some studies still have non-numeric values
dailyclim.bb2$Tmean<-(as.numeric(dailyclim.bb2$Tmin)+as.numeric(dailyclim.bb2$Tmax))/2
#dailyclim.bb2 <- dailyclim.bb2[!duplicated(dailyclim.bb2), ]
#dim(dailyclim.bb2)#3still 266908 rows
#Because the file is so big, I'll break it into 4 files
quart1<-as.integer(nrow(dailyclim.bb2)/4)
quart2<-as.integer(nrow(dailyclim.bb2)/2)
quart3<-(quart2+quart1)
dailyclim.bbA<-dailyclim.bb2[1:quart1,]
dailyclim.bbB<-dailyclim.bb2[(quart1+1):quart2,]
dailyclim.bbC<-dailyclim.bb2[(quart2+1):quart3,]
dailyclim.bbD<-dailyclim.bb2[(quart3+1):nrow(dailyclim.bb2),]
#check that everything is in these four datasets
nrow(dailyclim.bbA)+nrow(dailyclim.bbB)+nrow(dailyclim.bbC)+nrow(dailyclim.bbD)
nrow(dailyclim.bb2)
clim_dailyA<-dplyr::select(dailyclim.bbA,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyA)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyB<-dplyr::select(dailyclim.bbB,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyB)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyC<-dplyr::select(dailyclim.bbC,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)
colnames(clim_dailyC)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyD<-dplyr::select(dailyclim.bbD,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyD)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyALL<-dplyr::select(dailyclim.bb2,datasetID,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyALL)<-c("datasetID","uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")

write.csv(clim_dailyA, "output/dailyclim/percbb_dailyclimA.csv", row.names=FALSE)
write.csv(clim_dailyB, "output/dailyclim/percbb_dailyclimB.csv", row.names=FALSE)
write.csv(clim_dailyC, "output/dailyclim/percbb_dailyclimC.csv", row.names=FALSE)
write.csv(clim_dailyD, "output/dailyclim/percbb_dailyclimD.csv", row.names=FALSE)
write.csv(clim_dailyALL, "output/dailyclim/percbb_dailyclimALL.csv", row.names=FALSE)
#some checks on these files
clim_dailyALL$missingT<-0
clim_dailyALL$missingT[which(is.na(clim_dailyALL$Tmin))]<-1
temptab<-table(clim_dailyALL$datasetID,clim_dailyALL$missingT)
missingtemp<-temptab[temptab[,2]>0,]
dim(missingtemp)#7sites are missing some data
length(which(is.na(clim_dailyALL$Tmin)))/length(clim_dailyALL$Tmin)#0.009620849 of rows have NA...
head(clim_dailyALL)
tail(clim_dailyALL)
sort(unique(dailyclim.bb$datasetID))#41 in dailydata
tail(clim_dailyALL[clim_dailyALL$datasetID=="heide93",])
tail(clim_dailyALL[clim_dailyALL$datasetID=="sanzperez10",])#not sure why these are missing- longitude?

head(clim_dailyALL[clim_dailyALL$datasetID=="zohner16",])#looks good
tail(clim_dailyALL[clim_dailyALL$datasetID=="fu13",])#looks good
tail(clim_dailyALL[clim_dailyALL$datasetID=="gunderson12",])

#some questions: 
#why do some rows not get joined (for example:423,426,428,461, 468,470-471,477, 2225:2255,2272:; 2450:2459, 2460:2467, ). 
dat.bb[423:430,]#i think these are the NAs in climate data
tail(caffarra11b)<-clim_dailyALL[clim_dailyALL$datasetID=="caffarra11b",]
head(dat.bb[419:20,])#campbell75- has data now!
head(dat.bb[2290:2342,])#man10- why is there no man10 in climate data- is it because chilltemp="-3,2"? or because forcing="0 ramped up 3 degrees every 6 days" 
tail(clim_dailyALL[clim_dailyALL$datasetID=="man10",])
#Not possible to fix code to accomodate "mean of 9, 12, 15" (skuterud94)
