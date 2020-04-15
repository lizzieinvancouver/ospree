
#For part 1b, we need to modify the climate data so that it switches from field 
#(ambient) conditions currently in cdat to experimental chilling conditions after the field sampling date. 
#First look to see how many studies have experimental climate (either/both chilling (chilltemp, chilldays, chillphotoperiod) and forcing (forctemp)):

## okay need to fix provenance lat and long again for fu18
dat[which(dat$datasetID=="fu18"),]$provenance.lat <- dat[which(dat$datasetID=="fu18"),]$growing.lat
dat[which(dat$datasetID=="fu18"),]$provenance.long <- dat[which(dat$datasetID=="fu18"),]$growing.long

# Now fix okie11
dat[which(dat$datasetID=="okie11"),]$chillphotoperiod <- ""

dat$ID_exptreat2<-paste(dat$datasetID,dat$provenance.lat,dat$provenance.long,dat$chilltemp,dat$chilldays,dat$chillphotoperiod,dat$forcetemp,dat$forcetemp_night,sep=".")
#noexpchilldat<-dat[which(dat$chilltemp==""|dat$chilltemp=="ambient"),]#studies that do NOT need experimental chilling calculated
#noexpclimdat<-noexpchilldat[which(noexpchilldat$forcetemp==""|noexpchilldat$forcetemp=="ambient"|noexpchilldat$forcetemp=="meandaily"),]#studies that do NOT need experimental chilling AND ALSO do not need experimental forcing calculated
#unique(noexpclimdat$photoperiod_day)#some studies manipulate ONLY photoperiod- ignore these for now

#### Added by Cat 22 November 2019 - for some reason fu18 and one bad okie11 is weasling through to this point but should not be included here...
#fusandokies <- c("fu18", "okie11")
#expclimdat<-expclimdat[!(expclimdat$datasetID%in%fusandokies),]

expclimdat<-dat[-which(dat$chilltemp=="" & dat$forcetemp==""),]#156 rows removed

#dim(expclimdat)#7992   rows
#which(expclimdat$chilltemp=="ambient" & expclimdat$forcetemp=="")#no rows
#which(expclimdat$chilltemp=="ambient" & expclimdat$forcetemp=="ambient")#no rows

expclimdat<-expclimdat[-which(expclimdat$chilltemp=="" & expclimdat$forcetemp=="ambient"),]#
#dim(expclimdat)#7411   rows

expclimstudies<-sort(unique(expclimdat$datasetID))#list of studies that do manipulate chilling and/or forcing:50 studies
expclimtreats<-sort(unique(expclimdat$ID_exptreat2))#list of all study-chilling&forcing treatment combinations: 805

#For studies that do experimental chilling, fill in the experimental climate data and dates
#Things the below code does not yet deal with:
#1.studies that manipulate ONLY photoperiod
daily_chilltemp<-data.frame()
for (i in 1:length(expclimtreats)){#i=148
  tempdat<-dat[dat$ID_exptreat2==expclimtreats[i],] 
  startdate<-unique(tempdat$fieldsample.date2)
  for(j in 1:length(startdate)){ # j=1
    print(c(i, j))
    tempdat2<-tempdat[tempdat$fieldsample.date2==startdate[j],]
    datasetID<-unique(tempdat2$datasetID)
    ID_exptreat2<-unique(tempdat2$ID_exptreat2)
    chilltemp<-unique(tempdat2$chilltemp)
    chilldays<-unique(tempdat2$chilldays)
    chillphoto<-unique(tempdat2$chillphotoperiod)
    chilllat<-unique(tempdat2$provenance.lat)
    chilllong<-unique(tempdat2$provenance.long)
    chilluniqueID<-unique(tempdat2$uniqueID)
   if(chilltemp==""|chilltemp=="ambient"){next}
    #in this case, there is no experimental chilling, so we 
    #skip ahead to the next treatment. there may still be experimental forcing but we will calculate this in the monster loop below
    if(chilldays==""|is.na(chilldays)){next}
    enddate<-as.Date(startdate[j])+as.numeric(chilldays)-1
    if(as.Date(startdate[j])>as.Date(enddate)){enddate<-startdate[j]}
    if(as.Date(startdate[j])==as.Date(enddate)){next}
    for(k in 1:length(chilluniqueID)){
      tempdat3<-tempdat2[tempdat2$chilluniqueID==chilluniqueID[k],]
      aa<-data.frame(matrix(, nrow=as.numeric(chilldays),ncol=0))
      aa$datasetID<-rep(datasetID, times=chilldays)
      aa$ID_exptreat2<-rep(ID_exptreat2, times=chilldays)
      aa$fieldsample.date2<-rep(startdate[j],times=chilldays)
      aa$date<-seq(as.Date(startdate[j]),as.Date(enddate), by=1)
      aa$tmin<-rep(chilltemp, times=chilldays)
      aa$tmax<-rep(chilltemp, times=chilldays)
        #add in jones12, which has multiple difference chilltemps:
        if(chilltemp=="4, 0, -4"){#jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
        aa$tmin<-aa$tmax<-c(rep(4, times=as.numeric(chilldays)/3),rep(0, times=as.numeric(chilldays)/3),rep(-4,times=as.numeric(chilldays)/3))
      }
      if(chilltemp=="-4, 8, 8"){#jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
        aa$tmin<- aa$tmax<-c(rep(-4, times=as.numeric(chilldays)/3),rep(8, times=as.numeric(chilldays)/3),rep(-8,times=as.numeric(chilldays)/3))
      }
      if(chilltemp=="0, 4, 8"){#jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
        aa$tmin<- aa$tmax<-c(rep(0, times=as.numeric(chilldays)/3),rep(4, times=as.numeric(chilldays)/3),rep(8,times=as.numeric(chilldays)/3))
      }
      if(chilltemp=="4, 0, -4"){#jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
        aa$tmin<- aa$tmax<-c(rep(4, times=as.numeric(chilldays)/3),rep(0, times=as.numeric(chilldays)/3),rep(-4,times=as.numeric(chilldays)/3))
      }
      if(chilltemp=="8, 4, 0"){#jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
        aa$tmin<- aa$tmax<-c(rep(8, times=as.numeric(chilldays)/3),rep(4, times=as.numeric(chilldays)/3),rep(0,times=as.numeric(chilldays)/3))
      }
      if(chilltemp=="8, 8, -4"){#jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
        aa$tmin<- aa$tmax<-c(rep(8, times=as.numeric(chilldays)/3),rep(8, times=as.numeric(chilldays)/3),rep(-4,times=as.numeric(chilldays)/3))
      }
      if(chilltemp=="-3,2"){#man10- seedlings were chilled for one month at -3, and one month at 2
        aa$tmin<- aa$tmax<-c(rep(-3, times=as.numeric(chilldays)/2),rep(2, times=as.numeric(chilldays)/2))
      }
      if(chillphoto=="0,0"){chillphoto<-"0"}#man10
      aa$daylength<-rep(chillphoto, times=chilldays)
      aa$lastchilldate<-max(aa$date)#last date that chilling treatment occurred- this will be useful for calculating forcing later
      aa$lat<-rep(chilllat, times=chilldays)
      aa$long<-rep(chilllong, times=chilldays)
      aa$uniqueID<-rep(chilluniqueID[k], times=chilldays)
      daily_chilltemp<-rbind(daily_chilltemp,aa)
    }
  }
}

daily_chilltemp3<-dplyr::select(daily_chilltemp, datasetID, ID_exptreat2,uniqueID,lat,long,fieldsample.date2,date,tmin,tmax,daylength,lastchilldate)
colnames(daily_chilltemp3)<-c("datasetID","ID_exptreat2","uniqueID","lat","long","fieldsample.date2","Date","Tmin","Tmax","daylength","lastchilldate")
daily_chilltemp3$Date<-as.Date(daily_chilltemp3$Date)

#save this daily chilling climate file, since it has a column for the last chilldate for each study combination
#Nacho needs this for calculating growing degree days
write.csv(daily_chilltemp3,"output/dailyclim/daily_expchill.csv", row.names=FALSE)
