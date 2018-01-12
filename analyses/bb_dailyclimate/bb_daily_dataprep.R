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
#Move this to the end
# makes Tmean while we'here ... should check this
cdater$Tmean <- rowMeans(cbind(cdater$Tmin, cdater$Tmax))

# okay, we only want to work with phenology data for which we have climate data ...
dat <- dater[which(dater$datasetID %in% unique(cdater$datasetID)),]

cdat <- cdater[which(cdater$datasetID %in% unique(dat$datasetID)),]
# sigh
#dim(dater)
#dim(dat)
#unique(cdat$datasetID)
#unique(dater$datasetID)
#unique(dat$datasetID) # well, now, that's not bad 

# note ..
#unique(dat$respvar.simple)

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
#We don't really need to worry about the below, since we are not using pmp but leave in for now
dat$uniqueID <- paste(dat$datasetID, dat$fieldsample.date2, dat$forcetemp, dat$chilltemp, dat$chilldays,
    dat$chillphotoperiod, dat$photoperiod_day)

# need to fix year ...
# rule for now: if field sample date < August, then use year + 1, otherwise use year ...
#subset(dat, is.na(month)==TRUE) # WTF
dat <- subset(dat, is.na(month)==FALSE)
dat$year <- dat$sample.year
dat$year[dat$month>8] <- dat$sample.year[dat$month>8]+1
dat$year[dat$month<8] <- dat$sample.year[dat$month<8]

#Ailene started adding code here on July 12, 2017
#General approach is:
#1. Create two daily climate datasets to pull from:
#a) ambient climate data (which Lizzie pulled together already)
#b) experimental chilling data (uses chilltemp, chilldays, chillphotoperiod to create daily experimental chilling data)
#2. Using the budburst data only, expand each row of data (which is a budburst event) to include 
#daily climate data up to that date,choosing ambient climate data or experimental chilling data, as appropriate,
#and then filling in daily experimental climate data for forcing, if necessary

#Lizzie has already done part 1a with the above code.
#For part 1b, we need to modify the climate data so that it switches from field (ambient) conditions currently in cdat to 
#experimental chilling conditions after the field sampling date. 
#First look to see how many studies have experimental climate (either/both chilling (chilltemp, chilldays, chillphotoperiod) and forcing (forctemp)):
dat$ID_exptreat2<-paste(dat$datasetID,dat$chilltemp,dat$chilldays,dat$chillphotoperiod,dat$forcetemp,dat$forcetemp_night,sep=".")
noexpchilldat<-dat[which(dat$chilltemp==""|dat$chilltemp=="ambient"),]#studies that do NOT need experimental chilling calculated
noexpclimdat<-noexpchilldat[which(noexpchilldat$forcetemp==""|noexpchilldat$forcetemp=="ambient"|noexpchilldat$forcetemp=="meandaily"),]#studies that do NOT need experimental chilling AND ALSO do not need experimental forcing calculated
#unique(noexpclimdat$photoperiod_day)#some studies manipulate ONLY photoperiod- ignore these for now
expclimdat<-dat[-which(dat$chilltemp=="" & dat$forcetemp==""),]#156 rows removed
#dim(expclimdat)#5315   rows
#which(expclimdat$chilltemp=="ambient" & expclimdat$forcetemp=="")#no rows
#which(expclimdat$chilltemp=="ambient" & expclimdat$forcetemp=="ambient")#no rows
expclimdat<-expclimdat[-which(expclimdat$chilltemp=="" & expclimdat$forcetemp=="ambient"),]#
#dim(expclimdat)#4211   rows
expclimstudies<-sort(unique(expclimdat$datasetID))#list of studies that do manipulate chilling and/or forcing:45 studies
expclimtreats<-sort(unique(expclimdat$ID_exptreat2))#list of all study-chilling&forcing treatment combinations: 320
noexpclimstudies<-unique(noexpclimdat$datasetID)[is.na(match(unique(noexpclimdat$datasetID),expclimstudies))]#studies that do no experimental climate or photoperiod manipulation: only 2 ("ashby62"   "hawkins12")

#For studies that do experimental chilling, fill in the experimental climate data and dates
#Things the below code does not yet deal with:
#1.multiple values for chilling treatments in a single row (e.g. "-4, 0, 4","-4, 8, 8","0, 4, 8", "-3,2")
#2.ambient + X chilling treatments (e.g. "ambient + 0.76","ambient + 4")
#3.ambient photoperiod
#4. studies that manipulate ONLY photoperiod
#Other random problems observed: 
##1. pop2000 has lots of NAs for temperature data in the ambient climate data
daily_chilltemp<-data.frame()
for (i in 1:length(expclimtreats)){
  tempdat<-dat[dat$ID_exptreat2==expclimtreats[i],] 
  startdate<-unique(tempdat$fieldsample.date2)
  for(j in 1:length(startdate)){
    tempdat2<-tempdat[tempdat$fieldsample.date2==startdate[j],]
    datasetID<-unique(tempdat2$datasetID)
    ID_exptreat2<-unique(tempdat2$ID_exptreat2)
    chilltemp<-unique(tempdat2$chilltemp)
    chilldays<-unique(tempdat2$chilldays)
    chillphoto<-unique(tempdat2$chillphotoperiod)
    if(chilltemp==""|chilltemp=="ambient"){next}
      #in this case, there is no experimental chilling, so we 
      #skip ahead to the next treatment. there may still be experimental forcing but we will calculate this in the monster loop below
      if(chilldays==""){next}
    enddate<-as.Date(startdate[j])+as.numeric(chilldays)-1
    if(as.Date(startdate[j])>as.Date(enddate)){enddate<-startdate[j]}
    if(as.Date(startdate[j])==as.Date(enddate)){next}
    aa<-data.frame(matrix(, nrow=as.numeric(chilldays),ncol=0))
    aa$datasetID<-rep(datasetID, times=chilldays)
    aa$ID_exptreat2<-rep(ID_exptreat2, times=chilldays)
    aa$fieldsample.date2<-rep(startdate[j],times=chilldays)
    aa$date<-seq(as.Date(startdate[j]),as.Date(enddate), by=1)
    aa$tmin<-rep(chilltemp, times=chilldays)
    aa$tmax<-rep(chilltemp, times=chilldays)
    aa$daylength<-rep(chillphoto, times=chilldays)
    aa$lastchilldate<-max(aa$date)#last date that chilling treatment occurred- this will be useful for calculating forcing later
    daily_chilltemp<-rbind(daily_chilltemp,aa)
    }
}

#Add lat/long to this file
chill.latlong <- dat %>% # start with the data frame
  distinct(ID_exptreat2, .keep_all = TRUE) %>% # select all unique chilltreatments
  select(datasetID,uniqueID,ID_exptreat2, provenance.lat, provenance.long,fieldsample.date2)
daily_chilltemp2<-join(daily_chilltemp,chill.latlong)#add lat/long to daily_chilltemp dataframe
daily_chilltemp3<-dplyr::select(daily_chilltemp2, datasetID, uniqueID,ID_exptreat2, provenance.lat,provenance.long,fieldsample.date2,date,tmin,tmax,daylength,lastchilldate)
colnames(daily_chilltemp3)<-c("datasetID","uniqueID","ID_exptreat2","lat","long","fieldsample.date2","Date","Tmin","Tmax","daylength","lastchilldate")

#save this daily chilling climate file, since it has a column for the last chilldate for each study combination
#Nacho needs this for calculating growing degree days
write.csv(daily_chilltemp3,"output/daily_expchill.csv", row.names=FALSE)

#not sure what the difference is between the "date" column and the "Date" column in cdat; using Date for now
daily_ambtemp<-dplyr::select(cdat, datasetID, lat,long,fieldsample.date2,Date,Tmin,Tmax,daylength)
#format the Date columns as dates (otherwise some of the  values get weird...) 
daily_ambtemp$Date<-as.Date(daily_ambtemp$Date)
daily_chilltemp3$Date<-as.Date(daily_chilltemp3$Date)
#Make monster daily climate file with daily data for each budburst event date.
#The below loop takes a while....
#Things to add:
##4.  multiple values for forcing treatments (e.g. "mean of 9, 12, 15","7-27.5")
#6. studies that manipulate ONLY photoperiod

#First, select out budburst data
dat.bb<-dat[dat$respvar.simple=="percentbudburst"|dat$respvar.simple=="daystobudburst",]#dat$respvar.simple=="percentbudburst",]#| dat$respvar.simple=="percentbudburst" and response.time!=""#| dat$respvar.simple=="percentbudburst" and response.time!=""#| dat$respvar.simple=="percentbudburst" and response.time!=""#| dat$respvar.simple=="percentbudburst" and response.time!=""
dailyclim.bb<-data.frame()
bbdates.bb<-data.frame()
for(i in 1:dim(dat.bb)[1]){#3252 rows in dat
  x<-dat.bb[i,]#focal budburst event
  colnames(x)[9:10]<-c("lat","long")#match column names to climate data column names
  daystobb<-round(x$response.time, digits=0)
  
  #If no experimental chilling for focal budburst event, use ambient climate data
  if(x$chilltemp==""|x$chilltemp=="ambient"){#select out only ambient climate data from same datasetID, fieldsampledate, lat and long
    x.dailyclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & daily_ambtemp$lat==x$lat & daily_ambtemp$long==x$long,] 
    #if no experimental forcing, no need to add anything:
    if(x$forcetemp=="ambient"){
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
      }
    #for studies with forcing that warms 4 degrees above ambient:
    if(x$forcetemp=="ambient + 4"|x$forcetemp=="ambient+4"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+4
      x.dailyclim$Tmax<-x.dailyclim$Tmax+4
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
    }
    #for studies with forcing that warms 1.5 degrees above ambient:
    if(x$forcetemp=="ambient + 1.5"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+1.5
      x.dailyclim$Tmax<-x.dailyclim$Tmax+1.5
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
    }
    #for studies with forcing that warms 3 degrees above ambient:
    if(x$forcetemp=="ambient + 3"|x$forcetemp=="ambient+3"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+3
      x.dailyclim$Tmax<-x.dailyclim$Tmax+3
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
    }
    #for studies with forcing that warms 1 degree above ambient:
    if(x$forcetemp=="ambient+1"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+1
      x.dailyclim$Tmax<-x.dailyclim$Tmax+1
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
    }
    #for studies with forcing that warms 2 degrees above ambient:
    if(x$forcetemp=="ambient+2"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+2
      x.dailyclim$Tmax<-x.dailyclim$Tmax+2
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
    }
    #for studies with forcing that warms 5 degrees above ambient:
    if(x$forcetemp=="ambient+5"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+5
      x.dailyclim$Tmax<-x.dailyclim$Tmax+5
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
    }
    #for studies with forcing that warms 5 degrees above ambient:
    if(x$forcetemp=="ambient+6"){
      x.dailyclim$Tmin<-x.dailyclim$Tmin+6
      x.dailyclim$Tmax<-x.dailyclim$Tmax+6
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
    }
    #if there is other experimental forcing,  add it using the forcetemp, field sample date and response.time columns
    if(x$forcetemp!="ambient"){
      forcetmax<-x$forcetemp
      if(x$forcetemp_night==""){
        forcetmin<-x$forcetemp
       } else forcetmin<-x$forcetemp_night
      daystobb<-round(x$response.time, digits=0)
      forcedays<-dim(x.dailyclim[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2),])[1]##number of days of that should be replaced with the forcing treatment- this is the nubmer of rows after the field sample date because all of these should be replaced with forcing data number of days after field sample date- 
      if (!is.na(x$photoperiod_day)){
        if(x$photoperiod_day !="ambient") {forcephoto<-x$photoperiod_day
        }
        }#Replace tmin and tmax from ambient climate when Date >fieldsample.date2 with experimentalforcing climate
      x.dailyclim$Tmin[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmin, times=forcedays)
      x.dailyclim$Tmax[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmax, times=forcedays)
      if (!is.na(x$photoperiod_day)){
      if(x$photoperiod_day !="ambient"){x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcephoto, times=forcedays)}
      }
      x.all<-join(x,x.dailyclim)
      bbdate<-as.Date(x$fieldsample.date2)+daystobb
      x.bb<-cbind(x,bbdate)
    }
    
  }else if(!is.na(as.numeric(x$chilltemp))){#if the chilltemp is a single number, then use a combination of the ambient climate data and the experimental chilling data
    
    x.ambclim<-daily_ambtemp[daily_ambtemp$datasetID==x$datasetID & daily_ambtemp$fieldsample.date2==x$fieldsample.date2 & round(daily_ambtemp$lat, digits=1)==round(x$lat,digits=1) & round(daily_ambtemp$long, digits=1)==round(x$long,digits=1),] #round the lat long because a few are slightly different?ask lizzie about this...
    #if(dim(x.ambclim)[1]==0){next}#if we have no ambient climate data skip to the next row- this should not be necessary...
    
    #select experimental chilling climate data
    x.expclim<-daily_chilltemp3[daily_chilltemp3$datasetID==x$datasetID &daily_chilltemp3$ID_exptreat2==x$ID_exptreat2 & daily_chilltemp3$fieldsample.date2==x$fieldsample.date2 & daily_chilltemp3$lat==x$lat& daily_chilltemp3$long==x$long,] 
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
      if(x$forcetemp!="ambient"){
        forcetmax<-x$forcetemp
        if(x$forcetemp_night==""){
          forcetmin<-x$forcetemp
        } else forcetmin<-x$forcetemp_night
        daystobb<-round(x$response.time, digits=0)
        forcedays<-dim(x.dailyclim[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2),])[1]##number of days of that should be replaced with the forcing treatment- this is the nubmer of rows after the field sample date because all of these should be replaced with forcing data number of days after field sample date- 
          if (!is.na(x$photoperiod_day)){  
          if(x$photoperiod_day !="ambient") {forcephoto<-x$photoperiod_day}
          }
          #Replace rows from ambient climate when Date >fieldsample.date2 with experimentalforcing climate
          x.dailyclim$Tmin[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmin, times=forcedays)
          x.dailyclim$Tmax[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcetmax, times=forcedays)
          if (!is.na(x$photoperiod_day)){
          if(x$photoperiod_day !="ambient"){x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-rep(forcephoto, times=forcedays)}
          }
          x.all<-join(x,x.dailyclim)
          bbdate<-as.Date(lastchilldate)+daystobb
          x.bb<-cbind(x,bbdate)
      }
    } else if(max(as.Date(x.ambclim$Date))>as.Date(firstchilldate)-1){#if ambient data goes beyond experimental chilling data (which it should once the climate pulling code is correct)
      x.dailyclim<-x.ambclim#ambient climate data,
      #select experimental chilling climate data, this was done above
      #x.expclim<-daily_chilltemp3[daily_chilltemp3$datasetID==x$datasetID &daily_chilltemp3$ID_exptreat2==x$ID_exptreat2 & daily_chilltemp3$fieldsample.date2==x$fieldsample.date2 & daily_chilltemp3$lat==x$lat& daily_chilltemp3$long==x$long,] 
      #firstchilldate<-min(as.Date(x.expclim$Date))
      #lastchilldate<-unique(x.expclim$lastchilldate)
      #Replace tmin and tmax columns with experimental climate when Date >fieldsample.date2 and when Date <lastchilldate with experimental chilling climate
      x.dailyclim$Tmin[as.Date(x.dailyclim$Date) > as.Date(x.dailyclim$fieldsample.date2) & as.Date(x.dailyclim$Date) < as.Date(lastchilldate)]<-x.expclim$Tmin
      x.dailyclim$Tmax[as.Date(x.dailyclim$Date) > as.Date(x.dailyclim$fieldsample.date2) & as.Date(x.dailyclim$Date) < as.Date(lastchilldate)]<-x.expclim$Tmax
      #warning message, but its ok
      if (!is.na(x$photoperiod_day)){
        if(x$photoperiod_day !="ambient"){x.dailyclim$daylength[as.Date(x.dailyclim$Date)>as.Date(x.dailyclim$fieldsample.date2)]<-x.expclim$daylength}
      }
        #now add forcing
      
      if(x$forcetemp!="ambient"){
        forcetmax<-x$forcetemp
        if(x$forcetemp_night==""){
          forcetmin<-x$forcetemp
        } else forcetmin<-x$forcetemp_night
        daystobb<-round(x$response.time, digits=0)
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
          x.all<-join(x,x.dailyclim)
        bbdate<-as.Date(lastchilldate)+daystobb
        x.bb<-cbind(x,bbdate)
      }
    }  
  }
  #else if(is.na(as.numeric(x$chilltemp))){next}#for now, ignore those studies for which we have to calculate chilling "by hand" (e.g. chilltemp= "4, 0, -4")
  #make sure dates are formatted as dates
  x.all$Date<-as.Date(x.all$Date)
  dailyclim.bb<-rbind(dailyclim.bb,x.all)
  x.bb$bbdate<-as.Date(x.bb$bbdate)
  bbdates.bb<-rbind(bbdates.bb,x.bb)
}

    
#some checks of this file:
sort(unique(dailyclim.bb$datasetID))#38 different studies
sort(unique(dat$datasetID))#48 studies in full OSPREEBB database. 
unique(climdatab$stn)[is.na(match(unique(climdatab$stn),unique(dat$datasetID)))]#

dim(dailyclim.bb)#2736285     36#HUGE! but this makes sense given that the origianl percbb data file was 1561 rows (1561*2*365=1139530)
head(sort(unique(dailyclim.bb$Date)))#11 different studies



#save file with everything, just to have- this file is too big for github!
#write.csv(dailyclim.bb,"output/pmp/percbb.csv", row.names=FALSE)
#save budburst data and climate data as separate txt files, with 
#stn="uniqueID" column- combination of many things to make it a unique identifier for each row. put species in population.  
dailyclim.bb$year2<-as.numeric(format(dailyclim.bb$Date , "%Y"))#year for climate data
dailyclim.bb$doy2<-as.numeric(format(dailyclim.bb$Date , "%j"))#doy for climate data
dailyclim.bb$Tmin<-as.numeric(dailyclim.bb$Tmin)
dailyclim.bb$Tmax<-as.numeric(dailyclim.bb$Tmax)
dailyclim.bb$Tmean<-(as.numeric(dailyclim.bb$Tmin)+as.numeric(dailyclim.bb$Tmax))/2
#forgot to do this before running the monster loop!
#dailyclim.bb$uniqueID <- paste(dailyclim.bb$datasetID, dailyclim.bb$fieldsample.date2, dailyclim.bb$forcetemp, dailyclim.bb$chilltemp, dailyclim.bb$chilldays,
#                              dailyclim.bb$chillphotoperiod, dailyclim.bb$photoperiod_day)
#Because the file is so big, I'll break it into 2 files
dailyclim.bbA<-dailyclim.bb[dailyclim.bb$datasetID=="ashby62"|dailyclim.bb$datasetID=="basler12"|dailyclim.bb$datasetID=="basler14"|dailyclim.bb$datasetID=="caffarra11b"|dailyclim.bb$datasetID=="calme94"|dailyclim.bb$datasetID=="campbell75"|dailyclim.bb$datasetID=="falusi03"|dailyclim.bb$datasetID=="falusi97"|dailyclim.bb$datasetID=="fu13"|dailyclim.bb$datasetID=="gianfagna85",]
dailyclim.bbB<-dailyclim.bb[dailyclim.bb$datasetID=="gomory15"|dailyclim.bb$datasetID=="guak98"|dailyclim.bb$datasetID=="guerriero90"|dailyclim.bb$datasetID=="gunderson12"|dailyclim.bb$datasetID=="hawkins12"|dailyclim.bb$datasetID=="heide12"|dailyclim.bb$datasetID=="heide93"|dailyclim.bb$datasetID=="heide93a"|dailyclim.bb$datasetID=="jones12"|dailyclim.bb$datasetID=="karlsson03",]
dailyclim.bbC<-dailyclim.bb[dailyclim.bb$datasetID=="morin10"|dailyclim.bb$datasetID=="myking98"|dailyclim.bb$datasetID=="pagter15"|dailyclim.bb$datasetID=="partanen01"|dailyclim.bb$datasetID=="partanen05"|dailyclim.bb$datasetID=="partanen98"|dailyclim.bb$datasetID=="pop2000"|dailyclim.bb$datasetID=="ramos99"|dailyclim.bb$datasetID=="Sanz-Perez09"|dailyclim.bb$datasetID=="sanzperez10",]
dailyclim.bbD<-dailyclim.bb[dailyclim.bb$datasetID=="schnabel87"|dailyclim.bb$datasetID=="skre08"|dailyclim.bb$datasetID=="skuterud94"|dailyclim.bb$datasetID=="spann04"|dailyclim.bb$datasetID=="spiers74"|dailyclim.bb$datasetID=="swartz81"|dailyclim.bb$datasetID=="webb78"|dailyclim.bb$datasetID=="zohner16",]

clim_pmpA<-dplyr::select(dailyclim.bbA,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#pmp needs one file with only climate data
colnames(clim_pmpA)<-c("stn","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_pmpB<-dplyr::select(dailyclim.bbB,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#pmp needs one file with only climate data
colnames(clim_pmpB)<-c("stn","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_pmpC<-dplyr::select(dailyclim.bbC,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#pmp needs one file with only climate data
colnames(clim_pmpC)<-c("stn","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_pmpD<-dplyr::select(dailyclim.bbD,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#pmp needs one file with only climate data
colnames(clim_pmpD)<-c("stn","latitude","longitude","year","doy","Tmin","Tmax","Tmean")

#budburst data
#forgot to do this before running the monster loop!
bbdates.bb$uniqueID <- paste(bbdates.bb$datasetID, bbdates.bb$fieldsample.date2, bbdates.bb$forcetemp, bbdates.bb$chilltemp, bbdates.bb$chilldays,
                             bbdates.bb$chillphotoperiod, bbdates.bb$photoperiod_day)
bbdates.bb$genus.species<-paste(bbdates.bb$genus, bbdates.bb$species, sep=".")
bbdates.bb$year2<-as.numeric(format(bbdates.bb$bbdate , "%Y"))#year for budburst event
bbdates.bb$doy2<-as.numeric(format(bbdates.bb$bbdate , "%j"))#doy forbudburst event
bb_pmp<-dplyr::select(bbdates.bb, uniqueID,genus.species,year2, doy2)#pmp needs one files with only bud burst dates
colnames(bb_pmp)<-c("stn","species","year","doy")
#PMP needs txt files
#write.table(clim_pmpA, "output/pmp/percbb_clim_pmpA.txt", row.names=FALSE,sep="\t")
#write.table(clim_pmpB, "output/pmp/percbb_clim_pmpB.txt", row.names=FALSE,sep="\t")
#write.table(clim_pmpC, "output/pmp/percbb_clim_pmpC.txt", row.names=FALSE,sep="\t")
#write.table(clim_pmpD, "output/pmp/percbb_clim_pmpD.txt", row.names=FALSE,sep="\t")
#write.table(bb_pmp, "output/pmp/percbb_bb_pmp.txt", row.names=FALSE,sep="\t") 
#i prefer csv files
write.csv(clim_pmpA, "output/pmp/percbb_clim_pmpA.csv", row.names=FALSE)
write.csv(clim_pmpB, "output/pmp/percbb_clim_pmpB.csv", row.names=FALSE)
write.csv(clim_pmpC, "output/pmp/percbb_clim_pmpC.csv", row.names=FALSE)
write.csv(clim_pmpD, "output/pmp/percbb_clim_pmpD.csv", row.names=FALSE)
#whole file:
#write.csv(bb_pmp, "output/pmp/percbb_bb_pmp.csv", row.names=FALSE) 

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
