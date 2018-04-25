
#For part 1b, we need to modify the climate data so that it switches from field 
#(ambient) conditions currently in cdat to experimental chilling conditions after the field sampling date. 
#First look to see how many studies have experimental climate (either/both chilling (chilltemp, chilldays, chillphotoperiod) and forcing (forctemp)):
dat$ID_exptreat2<-paste(dat$datasetID,dat$provenance.lat,dat$provenance.long,dat$chilltemp,dat$chilldays,dat$chillphotoperiod,dat$forcetemp,dat$forcetemp_night,sep=".")
#noexpchilldat<-dat[which(dat$chilltemp==""|dat$chilltemp=="ambient"),]#studies that do NOT need experimental chilling calculated
#noexpclimdat<-noexpchilldat[which(noexpchilldat$forcetemp==""|noexpchilldat$forcetemp=="ambient"|noexpchilldat$forcetemp=="meandaily"),]#studies that do NOT need experimental chilling AND ALSO do not need experimental forcing calculated
#unique(noexpclimdat$photoperiod_day)#some studies manipulate ONLY photoperiod- ignore these for now
expclimdat<-dat[-which(dat$chilltemp=="" & dat$forcetemp==""),]#156 rows removed
#dim(expclimdat)#7992   rows
#which(expclimdat$chilltemp=="ambient" & expclimdat$forcetemp=="")#no rows
#which(expclimdat$chilltemp=="ambient" & expclimdat$forcetemp=="ambient")#no rows
expclimdat<-expclimdat[-which(expclimdat$chilltemp=="" & expclimdat$forcetemp=="ambient"),]#
#dim(expclimdat)#7583   rows
expclimstudies<-sort(unique(expclimdat$datasetID))#list of studies that do manipulate chilling and/or forcing:50 studies
expclimtreats<-sort(unique(expclimdat$ID_exptreat2))#list of all study-chilling&forcing treatment combinations: 805
#noexpclimstudies<-unique(noexpclimdat$datasetID)[is.na(match(unique(noexpclimdat$datasetID),expclimstudies))]#studies that do no experimental climate or photoperiod manipulation: only 3 ("ashby62"   "hawkins12" "sanzperez10")

#For studies that do experimental chilling, fill in the experimental climate data and dates
#Things the below code does not yet deal with:
#1.multiple values for chilling treatments in a single row (e.g. "-4, 0, 4","-4, 8, 8","0, 4, 8", "-3,2")
#2.studies that manipulate ONLY photoperiod
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
    chilllat<-unique(tempdat2$provenance.lat)
    chilllong<-unique(tempdat2$provenance.long)
    chilluniqueID<-unique(tempdat2$uniqueID)
    if(chilltemp==""|chilltemp=="ambient"){next}
    #in this case, there is no experimental chilling, so we 
    #skip ahead to the next treatment. there may still be experimental forcing but we will calculate this in the monster loop below
    if(chilldays==""){next}
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
