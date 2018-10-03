## Started 28 September 2018 ##
## by Ailene ##

## We need to check that the levels of
## photoperiod, chilling, and forcing reported 
## The checking process should involve the following:
## 1) Check levels in the original OSPREE file (ospree.csv) 
## 2) Check levels in ospree_clean_withchill_BB.csv
## 3) Look at any differences between the two files and make sure they make sense
## 4) Check the levels reported and the tables/figures cited
## 5) If you believe there is an error, start an issue ... do not make any changes yet.
## 6) Check and check again! Be careful! 
## 7) Note whether or not effects of photoperiod or 
      ##interactions of forcing/chilling with photoperiod were significant



##This file creates the list of studies that we want to check
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#load libraries
library(dplyr)

# Setting working directory. Add in your own path in an if statement for your file structure
if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")}
## STEP 1: Generate list of studies for which we want to know whether or not photoperiod affected phenology
  # code starts with shifts_table.R code 
  ospree<-read.csv("output/ospree_clean.csv",header=T)
  ospree$ID_daylengths<-paste(ospree$datasetID,ospree$photoperiod_day, sep="_")
  ospree$ID_study<-paste(ospree$datasetID,ospree$study, sep="_")#I actually don't think we need to distiguish between experiments for this table?
  #round lat/longs so they are the same
  ospree$provenance.lat<-round(ospree$provenance.lat, digits=2)
  ospree$provenance.long<-round(ospree$provenance.long, digits=2)

  photop <- ospree %>% # start with the data frame
    distinct(ID_daylengths, .keep_all = TRUE) %>% # establishing grouping variables
    dplyr::select(datasetID,ID_study, continent, provenance.lat, provenance.long, photoperiod_day)#growing or provenance lat/long?
  photop<-photop[-which(photop$photoperiod_day=="ambient"),]
  photop<-photop[-which(photop$photoperiod_day==""),]
  photop$numtreats<-with(photop, ave(as.numeric(photoperiod_day), datasetID, FUN=function(x) length(unique(x))))
  #remove studies that only have one photoperiod treatment
  photop<-photop[-which(photop$numtreats==1),]

  #need to add delta photoperiod, space, and time equivalents of these
  #try instead columns for daylength (min, max) and spatial temporal can be the difference between these
  #add a column with all unique photoperiod treatments  by lat long and experiment within study
  photop$idstudylatlong<-paste(photop$datasetID,photop$study,photop$provenance.lat,photop$provenance.long, sep="_")#I a
  #remove non-numeric daylengths
  photop$photoperiod_day<-as.numeric(photop$photoperiod_day)
  photop<-photop[-which(is.na(photop$photoperiod_day)),]
  #aggregate to get min and max daylengths
  photop_min<-aggregate(as.numeric(photop$photoperiod_day), by=list(photop$datasetID,photop$ID_study,photop$idstudylatlong,photop$continent,photop$provenance.lat,photop$provenance.long,photop$numtreats), FUN=min)
  photop_max<-aggregate(as.numeric(photop$photoperiod_day), by=list(photop$datasetID,photop$ID_study,photop$idstudylatlong,photop$continent,photop$provenance.lat,photop$provenance.long,photop$numtreats), FUN=max)
  #table(as.numeric(photop$photoperiod_day), by=list(photop$datasetID,photop$continent,photop$provenance.lat,photop$provenance.long))
  colnames(photop_max)[8]<-"daylength_max"
  colnames(photop_min)[8]<-"daylength_min"

  idstudylatlong<-unique(photop$idstudylatlong)
  alltreats<-c()
  for (i in 1:length(idstudylatlong)){
    dat<-photop[photop$idstudylatlong==idstudylatlong[i],]
    treats<-paste(sort(as.numeric(unique(dat$photoperiod_day))), collapse=",")
    alltreats<-c(alltreats,treats)
  }
  phototreats2<-as.data.frame(cbind(idstudylatlong,alltreats))
  #remove studies with single values for photoperiod
  phototreats2<-phototreats2[grep(",",phototreats2$alltreats),]

  photop2<-full_join(photop_min,photop_max, by=c("Group.1", "Group.2", "Group.3","Group.4","Group.5","Group.6","Group.7"), match="all")
  colnames(photop2)[1:7]<-c("dataset","idstudy","idstudylatlong","continent","lat","long","numtreats")
  phototreats2$idstudylatlong<-as.character(phototreats2$idstudylatlong)
  photop_all<-left_join(phototreats2, photop2,by=c("idstudylatlong"), match="all")

  #combine min and max into one column 
  photop_all$daylength_range<-paste(photop_all$daylength_min,photop_all$daylength_max, sep="-")
  #remove row when min=max
  photop_all<-photop_all[-which(photop_all$daylength_min==photop_all$daylength_max),]
  photop_all$daylength_maxfordelta<-NA#for estimates of change in daylength, we will use true min and max for studies with only 2 treatments. for studies with more than 2, just use difference between min and next longest daylength treatment 
  photop_all$daylength_maxfordelta<-NA
  #although these studies had 9 treats to start, only 2 were from the same lat/long
  photop_all$numtreats[photop_all$idstudylatlong=="viheraaarnio06__67.73_24.93"]<-2
  photop_all$numtreats[photop_all$idstudylatlong=="viheraaarnio06__60.45_24.93"]<-2

  photop_all$daylength_maxfordelta[photop_all$numtreats==2]<-photop_all$daylength_max[photop_all$numtreats==2]#studies with only 2 treatments

  for (i in 1:length(photop_all$alltreats[photop_all$numtreats>2])){
    photop_all$daylength_maxfordelta[photop_all$numtreats>2][i]<-strsplit(photop_all$alltreats[photop_all$numtreats>2], ",")[[i]][2]#select second photopoeriod treatment for each row
    }
  photop_all$daylength_maxfordelta<-as.numeric(photop_all$daylength_maxfordelta)
  photop_all$delta<-photop_all$daylength_maxfordelta-photop_all$daylength_min
  #Select only the relevant columns
  phototocheck<-subset(photop_all, select=c(idstudy,lat,long,numtreats,daylength_range))
  phototocheck$daylength_range<-as.character(phototocheck$daylength_range)
## STEP 2: Add the studies that Lizzie wants to add
  studiestoadd<-c("basler12","schnabel87","guerriero90",
                  "morin10","hawerroth13","linkosalo06",
                  "hawkins12","cronje03","devries82",
                  "heide15","thielges75","ghelardini10",
                  "caffarra11b","ruesink98","spann04")
  add<-as.data.frame(ospree[match(studiestoadd,ospree$datasetID),])
  add2<-subset(add, select=c(ID_study,provenance.lat,provenance.long))
  colnames(add2)<-c("idstudy","lat","long")
  add2$numtreats<-""
  add2$daylength_range<-""
  #put lizzies list together with the photoperiod list
  allstudiestocheck<-rbind(phototocheck,add2)
  allstudiestocheck<-allstudiestocheck[order(allstudiestocheck$idstudy),]
  #remove duplicates studies
  allstudiestocheck<-allstudiestocheck[!duplicated(allstudiestocheck$idstudy),]
colnames(allstudiestocheck)[4]<-"numtreats_photo"
## STEP 3: And write the list of studies to check out
  write.csv(allstudiestocheck, "cleaning/checkphotoperiod/checkphoto.csv", row.names=FALSE)

## STEP 4: Add columns of additional info for checking photoperiod and other treatments
  
  #first, who will do work- assign haphazardly 
  who<-rep(c("AKE","CC","DB","EMW", "IM"), times=11)
  allstudiestocheck_who<-cbind(allstudiestocheck,who[1:42])
  #assign acouple experiments within same studies to same person
  allstudiestocheck_who$`who[1:42]`[19]<-"DB"
  allstudiestocheck_who$`who[1:42]`[20]<-"DB"
  allstudiestocheck_who$`who[1:42]`[39]<-"AKE"
  allstudiestocheck_who$`who[1:42]`[40]<-"AKE"
  allstudiestocheck_who$`who[1:42]`[41]<-"CC"
  
  colnames(allstudiestocheck_who)[6]<-"who"
  allstudiestocheck_who<-allstudiestocheck_who[order(allstudiestocheck_who$who),]
  
  allstudiestocheck_who$treats.okay<-""
  allstudiestocheck_who$photo.effect<-""
  allstudiestocheck_who$photo.effect[which(allstudiestocheck_who$numtreats=="")]<-"NA"
  allstudiestocheck_who$photo.effect.desc<-""
  allstudiestocheck_who$photo.effect.desc[which(allstudiestocheck_who$numtreats=="")]<-"NA"
  
  #check
  #cbind(allstudiestocheck$idstudy,allstudiestocheck$`who[1:42]`)
  
  write.csv(allstudiestocheck_who, "cleaning/checkphotoperiod/checkphoto_info.csv", row.names=FALSE)
  