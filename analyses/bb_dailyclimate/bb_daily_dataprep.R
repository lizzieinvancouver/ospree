## Started 13 June 2017 ##
## By Lizzie ##
##Added to by Ailene started on July 12, 2017
###NOTE: This file is incomplete as we abandoned using it after deciding that we should not impute forcing for all studies
####The code is missing studies that do only experimental chilling and forcing (e.g. linksalo06)
###If you are confused about why we would need daily data for studies that do experimental chilling and forcing, 
### see linsalo06 as an (the only?) example!
##############################################################################################
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
#detach("package:chillR", unload=TRUE)

# sourcing
source("source/commoncols.R")


# Get the data, only work with BB data!
dater.all <- read.csv("output/ospree_clean_withchill.csv", header=TRUE)
dater <- subset(dater.all, select=common.cols.glatlong)
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
dat.bb<-dat.bb[-which(dat.bb$continent=="asia"),]#we only have climate data for north america and europe
### remove the messy fu and okie again
#fusandokies <- c("fu18", "okie11", "malyshev18")
#dat.bb<-dat.bb[-which(dat.bb$datasetID%in%fusandokies),]

## Issues with NAs versus blank entries. Fix here:
dat.bb$chilldays <- ifelse(is.na(dat.bb$chilldays), "", dat.bb$chilldays)
dat.bb$chillphotoperiod <- ifelse(is.na(dat.bb$chilldays), "", dat.bb$chilldays)

dat.bb[which(dat.bb$datasetID=="fu18"),]$provenance.lat <- dat.bb[which(dat.bb$datasetID=="fu18"),]$growing.lat
dat.bb[which(dat.bb$datasetID=="fu18"),]$provenance.long <- dat.bb[which(dat.bb$datasetID=="fu18"),]$growing.long

dailyclim.bb<-data.frame()
for(i in 1:dim(dat.bb)[1]){#4549 rows in dat.bb; i=1 i=1674* problem here!!!
  #also, a question: are all sites missing climate data on the day of budburst event (because of >, <)?
  print(i)
  x<-dat.bb[i,]#focal budburst event
  colnames(x)[9:10]<-c("lat","long")#match column names to climate data column names
  x$lat<-round(x$lat, digits=5)
  x$long<-round(x$long, digits=4)
  
  daystobb<-round(as.numeric(x$response.time), digits=0)
  #If no experimental chilling for focal budburst event, use ambient climate data
    if(x$chilltemp==""|x$chilltemp=="ambient"|x$chilltemp=="ambient + 1"|x$chilltemp=="ambient + 2"|x$chilltemp=="ambient + 3"
       |x$chilltemp=="ambient + 4"|x$chilltemp=="ambient + 5"|x$chilltemp=="ambient - 1"|x$chilldays==0){#select out ambient climate data from same datasetID, fieldsampledate, lat and long
    x.dailyclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & daily_ambtemp$lat==x$lat & daily_ambtemp$long==x$long,]
    if(dim(x.dailyclim)[1]==0 & x$usegrolatlong==1) {
      if(x$datasetID=="basler14"|x$datasetID=="gomory15"|x$datasetID=="partanen01"|x$datasetID=="Sanz-Perez09"|x$datasetID=="skre08"
         |x$datasetID=="flynn18"|x$datasetID=="malyshev18"){
        x$growing.lat<-round(x$growing.lat, digits=4)
        x$growing.long<-round(x$growing.long, digits=4)}
     x.dailyclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & round(daily_ambtemp$lat, digits=4)==x$growing.lat & daily_ambtemp$long==x$growing.long,]
      x.dailyclim$lat<-x$lat
      x.dailyclim$long<-x$long
      }
    #for partanen01; same growing/chill lat/long for all provenance lat/longs, so:
    if(x$datasetID=="partanen01"|x$datasetID=="calme94") {
      x.dailyclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2,]
      x.dailyclim$lat<-x$lat
      x.dailyclim$long<-x$long
    } 
    if(x$chilltemp=="ambient + 4" && x$datasetID!="fu18"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+4
      x.dailyclim$Tmax<-x.dailyclim$Tmax+4
    }
    #if no experimental forcing, no need to add anything:
    if(x$forcetemp=="ambient" & x$chilltemp!="ambient"){
      x.all<-join(x,x.dailyclim)
    }
    if(x$forcetemp==""|x$forcetemp=="meandaily"){
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
    #for studies with forcing that warms 1 degree below ambient:
    if(x$forcetemp=="ambient-1"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin-1
      x.dailyclim$Tmax<-x.dailyclim$Tmax-1
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
    if(x$forcetemp!="ambient" & x$forcetemp_night =="ambient"){#one study (Sanz-Perez09) has ambient conditions at night but not during the day
      x.dailyclim$Tmin<-x.dailyclim$Tmin
      forcetmax<-x$forcetemp
      daystobb<-round(as.numeric(x$response.time), digits=0)
      forcedays<-dim(x.dailyclim[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2),])[1]##number of days of that should be replaced with the forcing treatment- this is the nubmer of rows after the field sample date because all of these should be replaced with forcing data number of days after field sample date-
      if (!is.na(x$photoperiod_day)){
        if(x$photoperiod_day !="ambient") {forcephoto<-x$photoperiod_day}
        }
      x.dailyclim$Tmax[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmax, times=forcedays)
      x.all<-join(x,x.dailyclim)
      
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
  } else if(!is.na(as.numeric(x$chilltemp)|x$datasetID=="jones12"|x$datasetID=="man10")){#if the chilltemp is a single number, then use a combination of the ambient climate data and the experimental chilling data
    x.ambclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & daily_ambtemp$lat==x$lat & daily_ambtemp$long==x$long,]
      if(dim(x.ambclim)[1]==0 & x$usegrolatlong==1 & !x$datasetID=="flynn18") {
      x.ambclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & daily_ambtemp$lat==x$growing.lat & daily_ambtemp$long==x$growing.long,]
      x.ambclim$lat<-x$lat
      x.ambclim$long<-x$long
      }
      if(dim(x.ambclim)[1]==0 & x$usegrolatlong==1 & x$datasetID=="flynn18") {
      x.ambclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & round(daily_ambtemp$lat, digits=2)==round(x$growing.lat, digits=2) & round(daily_ambtemp$long, digits=2)==round(x$growing.long, digits=2),]
      x.ambclim$lat<-x$lat
      x.ambclim$long<-x$long
      }
    if(dim(x.ambclim)[1]==0 & x$datasetID=="skuterud94") {
      x.ambclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & daily_ambtemp$lat==x$lat & round(daily_ambtemp$long, digits=2)==round(x$long, digits=2),]
      x.ambclim$lat<-x$lat
      x.ambclim$long<-x$long
      }#rounding differences were making some not match, so correct
    #select experimental chilling climate data
    
    x.expclim<-daily_chilltemp3[daily_chilltemp3$datasetID==x$datasetID & daily_chilltemp3$ID_exptreat2==x$ID_exptreat2 & daily_chilltemp3$fieldsample.date2==x$fieldsample.date2 & daily_chilltemp3$lat==x$lat & daily_chilltemp3$long==x$long,]
    if(dim(x.expclim)[1]==0){x.expclim<-daily_chilltemp3[daily_chilltemp3$datasetID==x$datasetID & daily_chilltemp3$ID_exptreat2==x$ID_exptreat2 & daily_chilltemp3$fieldsample.date2==x$fieldsample.date2 & round(daily_chilltemp3$lat, digits=4)==round(x$lat,digits=4) & round(daily_chilltemp3$long,digits=4)==round(x$long,digits=4),]}
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
      x.dailyclim$Tmin[as.Date(x.dailyclim$Date) >= as.Date(x.dailyclim$fieldsample.date2) & as.Date(x.dailyclim$Date) <= as.Date(lastchilldate)]<-x.expclim$Tmin
      x.dailyclim$Tmax[as.Date(x.dailyclim$Date) >=as.Date(x.dailyclim$fieldsample.date2) & as.Date(x.dailyclim$Date) <= as.Date(lastchilldate)]<-x.expclim$Tmax
      #warning message, but its ok
      if (!is.na(x$photoperiod_day)){
        if(x$photoperiod_day !="ambient"){x.dailyclim$daylength[as.Date(x.dailyclim$Date)>=as.Date(x.dailyclim$fieldsample.date2) & as.Date(x.dailyclim$Date) <= as.Date(lastchilldate)]<-x.expclim$daylength}
      }
      #now add forcing
      if(substr(x$forcetemp,1,7)!="ambient" & x$forcetemp!="" & x$forcetemp!="meandaily"& x$forcetemp_night !="ambient"){
        forcetmax<-x$forcetemp
        if(x$forcetemp=="18-27 (20 average)"){forcetmax<-20}#campbell75
        if(x$forcetemp_night==""){
          forcetmin<-x$forcetemp
        } else forcetmin<-x$forcetemp_night
        if(x$forcetemp_night=="18-27 (20 average)"){forcetmin<-20}
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
#sort(unique(dailyclim.bb$datasetID))#44 different studies
#dim(dailyclim.bb)#4122830     36#HUGE! but this makes sense given that the dat (percbb data file) was 4231 rows (4231*2*365= 3088630)
dailyclim.bb2 <- dailyclim.bb[!duplicated(dailyclim.bb), ]
#dim(dailyclim.bb2)#3908412 rows
#save daily climate data
dailyclim.bb2$year2<-as.numeric(format(dailyclim.bb2$Date , "%Y"))#year for climate data
dailyclim.bb2$doy2<-as.numeric(format(dailyclim.bb2$Date , "%j"))#doy for climate data
dailyclim.bb2$Tmin<-as.numeric(dailyclim.bb2$Tmin)#lose "skuterud94" (mean of ...); this is not possible to fix
dailyclim.bb2$Tmax<-as.numeric(dailyclim.bb2$Tmax)

dailyclim.bb2$Tmean<-(as.numeric(dailyclim.bb2$Tmin)+as.numeric(dailyclim.bb2$Tmax))/2
#dailyclim.bb2 <- dailyclim.bb2[!duplicated(dailyclim.bb2), ]
#dim(dailyclim.bb2)#
#Because the file is so big, I'll break it into 10 files
#dailyclim.bb2<-read.csv("/Volumes/climdata/percbb_dailyclimALL.csv")
quart1<-as.integer(nrow(dailyclim.bb2)/10)
quart2<-as.integer(nrow(dailyclim.bb2)/5)
quart3<-(quart1+quart2)
quart4<-(quart1+quart3)
quart5<-as.integer(nrow(dailyclim.bb2)/2)
quart6<-(quart1+quart5)
quart7<-(quart1+quart6)
quart8<-(quart1+quart7)
quart9<-(quart1+quart8)
quart10<-(quart1+quart9)
dailyclim.bbA<-dailyclim.bb2[1:quart1,]
dailyclim.bbB<-dailyclim.bb2[(quart1+1):quart2,]
dailyclim.bbC<-dailyclim.bb2[(quart2+1):quart3,]
dailyclim.bbD<-dailyclim.bb2[(quart3+1):quart4,]
dailyclim.bbE<-dailyclim.bb2[(quart4+1):quart5,]
dailyclim.bbF<-dailyclim.bb2[(quart5+1):quart6,]
dailyclim.bbG<-dailyclim.bb2[(quart6+1):quart7,]
dailyclim.bbH<-dailyclim.bb2[(quart7+1):quart8,]
dailyclim.bbI<-dailyclim.bb2[(quart8+1):quart9,]
dailyclim.bbJ<-dailyclim.bb2[(quart9+1):nrow(dailyclim.bb2),]
#check that everything is in these four datasets
nrow(dailyclim.bbA)+nrow(dailyclim.bbB)+nrow(dailyclim.bbC)+nrow(dailyclim.bbD)+nrow(dailyclim.bbE)+nrow(dailyclim.bbF)+nrow(dailyclim.bbG)+nrow(dailyclim.bbH)+nrow(dailyclim.bbI)+nrow(dailyclim.bbJ)
nrow(dailyclim.bb2)
clim_dailyA<-dplyr::select(dailyclim.bbA,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyA)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyB<-dplyr::select(dailyclim.bbB,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyB)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyC<-dplyr::select(dailyclim.bbC,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)
colnames(clim_dailyC)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyD<-dplyr::select(dailyclim.bbD,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyD)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyE<-dplyr::select(dailyclim.bbE,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyE)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyF<-dplyr::select(dailyclim.bbF,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyF)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyG<-dplyr::select(dailyclim.bbG,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyG)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyH<-dplyr::select(dailyclim.bbH,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyH)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyI<-dplyr::select(dailyclim.bbI,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyI)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyJ<-dplyr::select(dailyclim.bbJ,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyJ)<-c("uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyALL<-dplyr::select(dailyclim.bb2,datasetID,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyALL)<-c("datasetID","uniqueID","latitude","longitude","year","doy","Tmin","Tmax","Tmean")

write.csv(clim_dailyA, "output/dailyclim/percbb_dailyclimA.csv", row.names=FALSE)
write.csv(clim_dailyB, "output/dailyclim/percbb_dailyclimB.csv", row.names=FALSE)
write.csv(clim_dailyC, "output/dailyclim/percbb_dailyclimC.csv", row.names=FALSE)
write.csv(clim_dailyD, "output/dailyclim/percbb_dailyclimD.csv", row.names=FALSE)
write.csv(clim_dailyE, "output/dailyclim/percbb_dailyclimE.csv", row.names=FALSE)
write.csv(clim_dailyF, "output/dailyclim/percbb_dailyclimF.csv", row.names=FALSE)
write.csv(clim_dailyG, "output/dailyclim/percbb_dailyclimG.csv", row.names=FALSE)
write.csv(clim_dailyH, "output/dailyclim/percbb_dailyclimH.csv", row.names=FALSE)
write.csv(clim_dailyI, "output/dailyclim/percbb_dailyclimI.csv", row.names=FALSE)
write.csv(clim_dailyJ, "output/dailyclim/percbb_dailyclimJ.csv", row.names=FALSE)
#write.csv(clim_dailyALL, "/Volumes/climdata/percbb_dailyclimALL.csv", row.names=FALSE)
#some checks on these files
clim_dailyALL$missingT<-0
clim_dailyALL$missingT[which(is.na(clim_dailyALL$Tmin))]<-1
temptab<-table(clim_dailyALL$datasetID,clim_dailyALL$missingT)
missingtemp<-temptab[temptab[,2]>0,]
dim(missingtemp)#4 sites are missing some data
length(which(is.na(clim_dailyALL$Tmin)))/length(clim_dailyALL$Tmin)# 0.003757792 of rows have NA...
head(clim_dailyALL)
tail(clim_dailyALL)
sort(unique(dailyclim.bb$datasetID))#44 in dailydata
tail(clim_dailyALL[clim_dailyALL$datasetID=="heide93",])
tail(clim_dailyALL[clim_dailyALL$datasetID=="sanzperez10",])

head(clim_dailyALL[clim_dailyALL$datasetID=="zohner16",])#looks good
tail(clim_dailyALL[clim_dailyALL$datasetID=="fu13",])#looks good
tail(clim_dailyALL[clim_dailyALL$datasetID=="gunderson12",])

tail(clim_dailyALL[clim_dailyALL$datasetID=="caffarra11b",])
#Not possible to fix code to accomodate "mean of 9, 12, 15" (skuterud94)
