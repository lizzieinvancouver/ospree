## Started 13 June 2017 ##
## By Lizzie ##
##Added to by Ailene started on July 12, 2017
#General approach is:
#1. Create two daily climate datasets to pull from:
#a) ambient climate data (which Lizzie pulled together already)
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

#Part 1a:
# sourcing
source("source/commoncols.R")

# Get the data, only work with BB data!
dater.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
dater <- subset(dater.all, select=common.cols)
cdater <- read.csv("output/dailyclim/dailytemp.csv", header=TRUE) 
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
write.csv(daily_chilltemp3,"output/dailyclim/daily_expchill.csv", row.names=FALSE)

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
sort(unique(dailyclim.bb$datasetID))#33 different studies
sort(unique(dat$datasetID))#42 studies in full OSPREEBB database. 

dim(dailyclim.bb)#2491075     33#HUGE! but this makes sense given that the dat (percbb data file) was 4231 rows (4231*2*365= 3088630)
sort(unique(dailyclim.bb$datasetID))


#save file with everything, just to have- this file is too big for github!
#write.csv(dailyclim.bb,"output/dailyclim/percbb.csv", row.names=FALSE)
#save daily climate data
dailyclim.bb$year2<-as.numeric(format(dailyclim.bb$Date , "%Y"))#year for climate data
dailyclim.bb$doy2<-as.numeric(format(dailyclim.bb$Date , "%j"))#doy for climate data
dailyclim.bb$Tmin<-as.numeric(dailyclim.bb$Tmin)
dailyclim.bb$Tmax<-as.numeric(dailyclim.bb$Tmax)
dailyclim.bb$Tmean<-(as.numeric(dailyclim.bb$Tmin)+as.numeric(dailyclim.bb$Tmax))/2
#Because the file is so big, I'll break it into 4 files
dailyclim.bbA<-dailyclim.bb[dailyclim.bb$datasetID=="ashby62"|dailyclim.bb$datasetID=="basler12"|dailyclim.bb$datasetID=="basler14"|dailyclim.bb$datasetID=="caffarra11b"|dailyclim.bb$datasetID=="calme94"|dailyclim.bb$datasetID=="campbell75"|dailyclim.bb$datasetID=="falusi03"|dailyclim.bb$datasetID=="falusi97"|dailyclim.bb$datasetID=="fu13"|dailyclim.bb$datasetID=="gianfagna85",]# dim: 326002     35
dailyclim.bbB<-dailyclim.bb[dailyclim.bb$datasetID=="gomory15"|dailyclim.bb$datasetID=="guak98"|dailyclim.bb$datasetID=="guerriero90"|dailyclim.bb$datasetID=="gunderson12"|dailyclim.bb$datasetID=="hawkins12"|dailyclim.bb$datasetID=="heide12"|dailyclim.bb$datasetID=="heide93"|dailyclim.bb$datasetID=="heide93a"|dailyclim.bb$datasetID=="jones12"|dailyclim.bb$datasetID=="karlsson03",]#
dailyclim.bbC<-dailyclim.bb[dailyclim.bb$datasetID=="laube14a"|dailyclim.bb$datasetID=="morin10"|dailyclim.bb$datasetID=="myking98"|dailyclim.bb$datasetID=="pagter15"|dailyclim.bb$datasetID=="partanen01"|dailyclim.bb$datasetID=="partanen05"|dailyclim.bb$datasetID=="partanen98"|dailyclim.bb$datasetID=="pop2000"|dailyclim.bb$datasetID=="ramos99"|dailyclim.bb$datasetID=="Sanz-Perez09"|dailyclim.bb$datasetID=="sanzperez10",]
dailyclim.bbD<-dailyclim.bb[dailyclim.bb$datasetID=="schnabel87"|dailyclim.bb$datasetID=="skre08"|dailyclim.bb$datasetID=="skuterud94"|dailyclim.bb$datasetID=="spann04"|dailyclim.bb$datasetID=="spiers74"|dailyclim.bb$datasetID=="swartz81"|dailyclim.bb$datasetID=="webb78"|dailyclim.bb$datasetID=="zohner16",]
#check that everything is in these four datasets
nrow(dailyclim.bbA)+nrow(dailyclim.bbB)+nrow(dailyclim.bbC)+nrow(dailyclim.bbD)
nrow(dailyclim.bb)

clim_dailyA<-dplyr::select(dailyclim.bbA,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyA)<-c("stn","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyB<-dplyr::select(dailyclim.bbB,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyB)<-c("stn","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyC<-dplyr::select(dailyclim.bbC,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)
colnames(clim_dailyC)<-c("stn","latitude","longitude","year","doy","Tmin","Tmax","Tmean")
clim_dailyD<-dplyr::select(dailyclim.bbD,uniqueID,lat,long,year2,doy2, Tmin, Tmax, Tmean)#
colnames(clim_dailyD)<-c("stn","latitude","longitude","year","doy","Tmin","Tmax","Tmean")

#budburst data
bbdates.bb$uniqueID <- paste(bbdates.bb$datasetID, bbdates.bb$fieldsample.date2, bbdates.bb$forcetemp, bbdates.bb$chilltemp, bbdates.bb$chilldays,
                             bbdates.bb$chillphotoperiod, bbdates.bb$photoperiod_day)
bbdates.bb$genus.species<-paste(bbdates.bb$genus, bbdates.bb$species, sep=".")
bbdates.bb$year2<-as.numeric(format(bbdates.bb$bbdate , "%Y"))#year for budburst event
bbdates.bb$doy2<-as.numeric(format(bbdates.bb$bbdate , "%j"))#doy forbudburst event
bb_dailyclim<-dplyr::select(bbdates.bb, uniqueID,genus.species,year2, doy2)#pmp needs one files with only bud burst dates
colnames(bb_dailyclim)<-c("stn","species","year","doy")
write.csv(clim_dailyA, "output/dailyclim/percbb_dailyclimA.csv", row.names=FALSE)
write.csv(clim_dailyB, "output/dailyclim/percbb_dailyclimB.csv", row.names=FALSE)
write.csv(clim_dailyC, "output/dailyclim/percbb_dailyclimC.csv", row.names=FALSE)
write.csv(clim_dailyD, "output/dailyclim/percbb_dailyclimD.csv", row.names=FALSE)
#whole file:
#write.csv(bb_dailyclim, "output/dailyclim/percbb_dailyclimALL.csv", row.names=FALSE) 
#dat<-read.csv("output/dailyclim/percbb_dailyclimALL.csv", header=TRUE)
