ospree$ID_daylengths<-paste(ospree$datasetID,ospree$photoperiod_day, sep="_")
ospree$ID_study<-paste(ospree$datasetID,ospree$study, sep="_")#I actually don't think we need to distiguish between experiments for this table?
#ospree$ID_daylengths<-paste(ospree$datasetID,ospree$study,ospree$photoperiod_day, sep="_")

photop <- ospree %>% # start with the data frame
  distinct(ID_daylengths, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(datasetID,ID_study, continent, provenance.lat, provenance.long, photoperiod_day)#growing or provenance lat/long?
photop<-photop[-which(photop$photoperiod_day=="ambient"),]
photop<-photop[-which(photop$photoperiod_day==""),]
photop$numtreats<-with(photop, ave(as.numeric(photoperiod_day), datasetID, FUN=function(x) length(unique(x))))

#with(photop, ave(as.numeric(photoperiod_day), datasetID, FUN=function(x) unique(x)))
#need to add delta photoperiod, space, and time equivalents of these
#try instead columns for daylength (min, max) and spatial temporal can be the difference between these
#add a column with all unique photoperiod treatments  by lat long and experiment within study
photop$idstudylatlong<-paste(photop$datasetID,photop$study,photop$provenance.lat,photop$provenance.long, sep="_")#I a
#aggregate to get min and max daylengths
photop_min<-aggregate(as.numeric(photop$photoperiod_day), by=list(photop$datasetID,photop$ID_study,photop$idstudylatlong,photop$continent,photop$provenance.lat,photop$provenance.long,photop$numtreats), FUN=min)
photop_max<-aggregate(as.numeric(photop$photoperiod_day), by=list(photop$datasetID,photop$ID_study,photop$idstudylatlong,photop$continent,photop$provenance.lat,photop$provenance.long,photop$numtreats), FUN=max)
#table(as.numeric(photop$photoperiod_day), by=list(photop$datasetID,photop$continent,photop$provenance.lat,photop$provenance.long))
colnames(photop_max)[8]<-"daylength_max"
colnames(photop_min)[8]<-"daylength_min"

idstudylatlong<-unique(photop$idstudylatlong)

#id<-unique(photop$datasetID)
alltreats<-c()
for (i in 1:length(idstudylatlong)){
  dat<-photop[photop$idstudylatlong==idstudylatlong[i],]
  treats<-paste(sort(as.numeric(unique(dat$photoperiod_day))), collapse=",")
  alltreats<-c(alltreats,treats)
}
phototreats2<-as.data.frame(cbind(idstudylatlong,alltreats))

photop2<-join(photop_min,photop_max, by=c("Group.1", "Group.2", "Group.3","Group.4","Group.5","Group.6","Group.7"), match="all")
colnames(photop2)[1:7]<-c("dataset","idstudy","idstudylatlong","continent","lat","long","numtreats")

photop_all<-join(photop2,phototreats2, by=c("idstudylatlong"), match="all")
#remove studies that only have one photoperiod treatment
photop_all<-photop_all[-which(photop_all$daylength_min==photop_all$daylength_max),]

#combine min and max into one column 
photop_all$daylength_range<-paste(photop_all$daylength_min,photop_all$daylength_max, sep="-")
photop_all$daylength_maxfordelta<-NA#for estimates of change in daylength, we will use true min and max for studies with only 2 treatments. for studies with more than 2, just use difference between min and next longest daylength treatment 
photop_all$daylength_maxfordelta<-NA
photop_all$daylength_maxfordelta[photop_all$numtreats==2]<-photop_all$daylength_max[photop_all$numtreats==2]#studies with only 2 treatments

for (i in 1:length(photop_all$alltreats[photop_all$numtreats>2])){
  photop_all$daylength_maxfordelta[photop_all$numtreats>2][i]<-strsplit(photop_all$alltreats[photop_all$numtreats>2], ",")[[i]][2]#select second photopoeriod treatment for each row
}
photop_all$daylength_maxfordelta<-as.numeric(photop_all$daylength_maxfordelta)
photop_all$delta<-photop_all$daylength_maxfordelta-photop_all$daylength_min


photop_all$space<-""
photop_all$time<-""
for(i in 1:length(photop_all$lat)){
  photos<-daylength(photop_all$lat[i], c(355:365,1:172))
  date<-strptime(c(355:365,1:172), format = "%j")
  date_expmin<-date[which(round(photos, digits=1)==round(photop_all$daylength_min[i], digits=1))]
  date_expmax<-date[which(round(photos, digits=1)==round(photop_all$daylength_maxfordelta[i], digits=1))]
  #when there is no date that matches the maximum date , choose the closest one, as long as it is within .5 hours
  if(length(date_expmax)==0)   {
    mindiff<-min(abs(photop_all$daylength_maxfordelta[i]-photos))
    if(mindiff<0.5){date_expmax<-date[which(abs(photop_all$daylength_maxfordelta[i]-photos)==mindiff)]
    }else {
      min_dlmax<-round(min(photos), digits=1)
      date_expmax<-NA
    }
  }
  #when there is no date that matches the minimum date, choose the closest one, as long as it is within .5 hours
  if(length(date_expmin)==0)   {
    mindiff<-min(abs(photop_all$daylength_min[i]-photos))
    if(mindiff<0.5){date_expmin<-date[which(abs(photop_all$daylength_min[i]-photos)==min(abs(photop_all$daylength_min[i]-photos)))]
    }else {
      min_dlmin<-round(min(photos), digits=1)
      date_expmin<-NA
    }
  }
  maxdelta_temp<-max(photos)-min(photos)#maximum difference in daylength at lat[i] (difference in daylength between summer solcstice and winter solcstice
  if(maxdelta_temp<photop_all$delta[i]){photop_all$time[i]<-"ER"}#exceeds range
  else if (is.na(date_expmax)){photop_all$time[i]<-paste("max NA (",min_dlmax,")", sep="")}
  else if (is.na(date_expmin)){photop_all$time[i]<-paste("min NA (",min_dlmin,")", sep="")}
  else
    photop_all$time[i]<-min(as.numeric(strftime(date_expmin, format = "%j"))-as.numeric(strftime(date_expmax, format = "%j")))#shift in days between date(s) of min daylength and max daylength in exp
  if(!is.na(as.numeric(photop_all$time[i])) & as.numeric(photop_all$time[i])>183){photop_all$time[i]<-0-(365-as.numeric(photop_all$time[i]))}
  #a different approach that i am abandoning for now:  
  #delta_mar21<-daylength(photop_all$lat[i],80)-daylength(photop_all$lat[i], c(355:365,1:172))
  #delta_jun21<-daylength(photop_all$lat[i],172)-daylength(photop_all$lat[i], c(355:365,1:172))
  #assume march 21, change in daylength for each day earlier than that back to winter solcistice
  #OR assume june 21, for maximum change in daylength
  
  
  
  #need to figure out the spatial shift thing- try different latitudes and find matching daylength to min and max on the same date. Eventually, date should be latitude-specific greenup date. For now, choose March 20 as a date.
  #In 100 years, with spatial shifts of ~6km ( or ~0.05 degrees) per decade (0.5 deg total) poleward as has been observed (Parmesan 2006)- this is a low end
  #phendate<-79#march 20 . eventually we should replace this with spring greenup date for that latitude
  phendate<-172#June 21
  latshift<-seq(0,40,by=.1)#look at daylengths of latitudes from study site to study site plus 40 degrees
  photos_spat<-daylength(photop_all$lat[i]+latshift, phendate)
  #photop$space[i]<-
  maxdelta_space<-max(photos_spat, na.rm=TRUE)-min(photos_spat, na.rm=TRUE)#maxim
  delta_space<-photos_spat-photos_spat[1]#change in daylength latitudes ranging from study site to study site plus 40 degrees
  
  if(maxdelta_space<abs(photop_all$delta[i])){photop_all$space[i]<-"ER"}#exceeds range
  else
    photop_all$space[i]<-latshift[min(which(round(delta_space, digits=2)==photop_all$delta[i]))]#select min lat shift required to get change in daylength in experiments
  if(is.na(photop_all$space[i])){
    mindiff<-min(abs(photop_all$delta[i]-delta_space), na.rm=TRUE)
    
    if(!is.na(mindiff) & mindiff<0.5){photop_all$space[i]<-latshift[which(abs(photop_all$delta[i]-delta_space)==mindiff)]
    }else {
      photop_all$space[i]<-NA
    }
  }
}

#sort by idstudy
photop_all<-photop_all[order(photop_all$idstudy),]
