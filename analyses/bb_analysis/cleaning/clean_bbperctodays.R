### Started by Ailene Ettinger ###
### January 2016 ###
## Script to convert files with 50% bb to "days to bb" and calculate max %bb per treatment per spp ##
## Sourced in bb_cleanmergeall.R ##

# Edited by Dan Flynn, then lots of edits in early 2017 by Nacho Morales-Castilla #

## read data
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
# setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/output")
#d<-read.csv("ospree_clean_withchill.csv",as.is=TRUE)

if(is.data.frame(d)){

subsetting.daysBB<-function(d,target.percent,type=c("add.columns","only.percentBB","BB_analysis")){
## generate new columns in dataset to store days to budburst results
d$response.time = as.numeric(as.character(d$response.time))
d$dbb=rep(NA,nrow(d))
d$maxperc_bb=rep(NA,nrow(d))
d$minperc_bb=rep(NA,nrow(d))
d$dist.50bb=rep(NA,nrow(d))

#dat_final <- data.frame(matrix(data=NA,nrow=0,ncol=7))
#colnames(dat_final) <- c("datasetID","study","treatment","dbb","maxperc_bb","minperc_bb","dist.50bb")

##Sites that use percent budburst
percbbsites<-subset(d,respvar=="percentbudburst")

#dim(percbbsites)

dataset <- unique(percbbsites$datasetID)

for(i in 1:length(dataset)){ # i = 2
#print(i)
  dat1 <- percbbsites[which(percbbsites$datasetID==dataset[i]),]
  
  study<-unique(dat1$study)
  ##make new column that combines all treatments (columns 1:33) into one columns
  dat1$treatment <- paste(dat1$genus,dat1$species, dat1$varetc,dat1$population,dat1$other.treatment,
                          dat1$dormancy_induction_temp,dat1$dormancy_induction_days,
                          dat1$dormancy_induction_photoperiod_day,dat1$dormancy_induction_photoperiod_night,
                          dat1$freeze.treatment.time,dat1$freeze.treatment.photoperiod_day,
                          dat1$freeze.treatment.photoperiod_night,dat1$freeze.treatment.temp_day,
                          dat1$freeze.treatment.temp_night,dat1$fieldchill,dat1$chilltemp,dat1$chillphotoperiod,
                          dat1$chilldays,dat1$number.longdays,dat1$photoperiod_day,dat1$photoperiod_night,dat1$forcetemp,
                          dat1$forcetemp_night, dat1$irradiance,dat1$humidity, dat1$fieldsample.date,sep = ".")

  for(j in 1:length(study)){ # j = 1
  dat2<-dat1[which(dat1$study==study[j]),]
  treat=unique(dat2$treatment)
	dat2$response <- as.numeric(as.character(dat2$response))
  
	for(k in 1:length(treat)) { # k = 6 # This for loop cleans each treatment within studies with more than one treatment
  print(paste(i,j,k))
    dat3 <- dat2[which(dat2$treatment==treat[k]),]
    
    maxperc_bb<-max(as.numeric(dat3$response)) # maxpercent budburst
    minperc_bb<-min(as.numeric(dat3$response)) # minpercent budburst - hopefully zero
    #dists.to.target<-dist(dat3$response,upper=F)
    #dat3$response-target.percent
                   
    # Check if this can be estimated at all.
    if(length(dat3$response[!is.na(dat3$response)])>1 & is.numeric(dat3$response.time)) {
    
      # Check how many values are within 25% of target percent, if at least one we proceed
      values.in.target<-which(dat3$response>(target.percent-target.percent*0.25)&dat3$response<(target.percent+target.percent*0.25))
      if(length(values.in.target)==1){
      index<-rownames(dat3[values.in.target,])  
      d[index,"dbb"]<-dat3[values.in.target,"response.time"]
      d[index,"maxperc_bb"]<-dat3[values.in.target,"response"]
      d[index,"minperc_bb"]<-dat3[values.in.target,"response"]
      d[index,"dist.50bb"]<-dat3[values.in.target,"response"]-target.percent
      
      } 
      # If there are more than 1 rows with values within 25% of target percent and values in the 
      # response variable are not equal to the targetted % we proceed
      if(length(values.in.target)>1 & length(which(dat3$response==target.percent))==0){  
        index<-rownames(dat3[values.in.target,])  
        d[index,"dbb"]<-dat3[values.in.target,"response.time"]
        d[index,"maxperc_bb"]<-rep(max(dat3[values.in.target,"response"]),length(values.in.target))
        d[index,"minperc_bb"]<-rep(min(dat3[values.in.target,"response"]),length(values.in.target))
        d[index,"dist.50bb"]<-abs(dat3[values.in.target,"response"]-target.percent)
        
      }
      # If there are more than 1 rows with values within 25% of target percent and at least one value in the 
      # response variable is equal to the targetted % in the function we proceed
      if(length(values.in.target)>1 & length(which(dat3$response==target.percent))>0){  
        index<-rownames(dat3[which(dat3$response==target.percent),])  
        d[index,"dbb"]<-dat3[which(dat3$response==target.percent),"response.time"]
        d[index,"maxperc_bb"]<-dat3[which(dat3$response==target.percent),"response"]
        d[index,"minperc_bb"]<-dat3[which(dat3$response==target.percent),"response"]
        d[index,"dist.50bb"]<-rep(0,length(index))
        
      }
      
      
  }
  

    } 
  
  }
}

if(type=="BB_analysis"){
  d.subset.1<-subset(d,respvar.simple=="daystobudburst"& !is.na(response.time))
  d.subset.2<-subset(d,!is.na(dbb))
  d.subsetted<-rbind(d.subset.1,d.subset.2)
  d.subsetted$response.time[which(!is.na(d.subsetted$dbb))]<-d.subsetted$dbb[which(!is.na(d.subsetted$dbb))]
  }

if(type=="add.columns"){
  d.subsetted<-d
}


if(type=="only.percentBB"){
  d.subsetted<-subset(d,!is.na(dbb))
}

return(d.subsetted)

}

d.subset<-subsetting.daysBB(d,90,"BB_analysis")
d<-d.subset

} else {
  print("Error: d not a data.frame")
}

stop("Not an error, now BB data is all cleaned, ready to analyze and save")
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
#write.csv(d,"ospree_clean_withchill_BB.csv",row.names=F)




