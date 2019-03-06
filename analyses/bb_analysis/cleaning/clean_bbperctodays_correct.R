### Started by Ailene Ettinger ###
### January 2016 ###
## Script to convert files with % bb and match "days to bb" data to one value of 'days to bb' and calculates max %bb per treatment per spp ##
## Sourced in bb_cleanmergeall.R ##

# Edited by Dan Flynn, then lots of edits in early 2017 by Nacho Morales-Castilla
# Then more edits by Nacho and Lizzie in Apr2018 to make sure the code does the following:
# (a) remove values which are not within target range 
# (b) extract data with just one value in target range
# (c) for data with more than one value in target range but one value that exactly equals target range (we should retain it, but I think this may be fairly uncommon), extract that value
# (d) for data with more than one value in target range (and no one value that exactly equals target range), extract the value closest to the target 
#

## This code runs from bb_mergeall.R, so...
# You should run that through to this files sourcing before running this code! ##

targetvalue <- 90 # we want value closest to this
acceptablerange <- 0.55 # meaning as low as 49.5% allowed (e.g., 90-90*0.55=49.5) ## decreasing the acceptable range increases the amount of rows deleted (more values cease to be close enough to targetted values)

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
    
    for(i in 1:length(dataset)){ # i = 1
      #print(i)
      dat1 <- percbbsites[which(percbbsites$datasetID==dataset[i]),]
      #dim(dat1)
      
      study<-unique(dat1$study)
      ##make new column that combines all treatments (columns 1:33) into one columns
      dat1$treatment <- paste(dat1$genus,dat1$species, dat1$varetc,dat1$population,dat1$other.treatment,
                              dat1$dormancy_induction_temp,dat1$dormancy_induction_days,
                              dat1$dormancy_induction_photoperiod_day,dat1$dormancy_induction_photoperiod_night,
                              dat1$freeze.treatment.time,dat1$freeze.treatment.photoperiod_day,
                              dat1$freeze.treatment.photoperiod_night,dat1$freeze.treatment.temp_day,
                              dat1$freeze.treatment.temp_night,dat1$fieldchill,dat1$chilltemp,dat1$chillphotoperiod,
                              dat1$chilldays,dat1$number.longdays,dat1$photoperiod_day,
                              dat1$photoperiod_night, dat1$forcetemp, dat1$forcetemp_night,
                              dat1$irradiance,dat1$humidity, dat1$fieldsample.date,sep = ".")
      
      for(j in 1:length(study)){ # j = 1
        dat2<-dat1[which(dat1$study==study[j]),]
        treat=unique(dat2$treatment)
        dat2$response <- as.numeric(as.character(dat2$response))
        #dim(d)
        for(k in 1:length(treat)) { # k = 1 # This for loop cleans each treatment within studies with more than one treatment
          #print(paste(i,j,k))
          dat3 <- dat2[which(dat2$treatment==treat[k]),]
          #print(paste(k,dim(dat3)[1]))
          
          maxperc_bb<-max(as.numeric(dat3$response)) # maxpercent budburst
          minperc_bb<-min(as.numeric(dat3$response)) # minpercent budburst - hopefully zero
          #dists.to.target<-dist(dat3$response,upper=F)
          dists.to.target<-dat3$response-target.percent
          mindist<-which.min(abs(dists.to.target))
          
          values.in.target<-which(dat3$response>(target.percent-target.percent*acceptablerange)&
                                    dat3$response<(target.percent+target.percent*acceptablerange)&
                                    dat3$response<100)
          
          # Check if this can be estimated at all.
          if(length(dat3$response[!is.na(dat3$response)])>1 & is.numeric(dat3$response.time)) {
            
            # Check how many values are within 40% of target percent, if only one we proceed
            if(length(values.in.target)==1){
              #print("case1!!!")
              index<-rownames(dat3[values.in.target,])  
              out.index<-rownames(dat3[which(!1:length(dat3$response)%in%values.in.target),])
              d[index,"dbb"]<-dat3[values.in.target,"response.time"]
              d[index,"maxperc_bb"]<-dat3[values.in.target,"response"]
              d[index,"minperc_bb"]<-dat3[values.in.target,"response"]
              d[index,"dist.50bb"]<-dat3[values.in.target,"response"]-target.percent
              # remove whatever not within range
              d<-d[!rownames(d)%in%out.index,]
              
            } 
            
            # If there are more than 1 rows with values within the acceptable range (e.g. 25 or 40%) of target percent and values in the 
            # response variable are not equal to the targetted % we proceed
            if(length(values.in.target)>1 & length(which(dat3$response==target.percent))==0){  
              #print("case2!!!")
              index<-rownames(dat3[values.in.target,])  
              out.index<-rownames(dat3)[which(!rownames(dat3)%in%index)]
              d[index,"dbb"]<-dat3[values.in.target,"response.time"]
              d[index,"maxperc_bb"]<-rep(max(dat3[values.in.target,"response"]),length(values.in.target))
              d[index,"minperc_bb"]<-rep(min(dat3[values.in.target,"response"]),length(values.in.target))
              d[index,"dist.50bb"]<-abs(dat3[values.in.target,"response"]-target.percent)
              # remove whatever not within range
              d<-d[!rownames(d)%in%out.index,]
              #dim(d)
            }
            
            # If there are more than 1 rows with values within within the acceptable range of target percent and at least one value in the 
            # response variable is equal to the targetted % in the function we proceed
            if(length(values.in.target)>1 & length(which(dat3$response==target.percent))>0){  
              #print("case3!!!")
              index<-rownames(dat3[which(dat3$response==target.percent),])  
              out.index<-rownames(dat3[which(!1:length(dat3$response)%in%mindist),])
              d[index,"dbb"]<-dat3[which(dat3$response==target.percent),"response.time"]
              d[index,"maxperc_bb"]<-dat3[which(dat3$response==target.percent),"response"]
              d[index,"minperc_bb"]<-dat3[which(dat3$response==target.percent),"response"]
              d[index,"dist.50bb"]<-rep(0,length(index))
              # remove whatever not within range
              d<-d[!rownames(d)%in%out.index,]
              
            }
            
            # If there are more than 1 rows but no values fall within within the acceptable range of target percent and at least one value in the 
            # response variable is equal to the targetted % in the function we proceed
            if(length(values.in.target)==0 & length(dat3$response)>0){  
              #print("case4!!!")
              #index<-rownames(dat3[mindist,])  
              #out.index<-rownames(dat3[which(!1:length(dat3$response)%in%mindist),])
              out.index<-rownames(dat3)
              #d[index,"dbb"]<-dat3[index,"response.time"]
              #d[index,"maxperc_bb"]<-dat3[index,"response"]
              #d[index,"minperc_bb"]<-dat3[index,"response"]
              #d[index,"dist.50bb"]<-rep(dists.to.target[mindist],length(index))
              # remove whatever not within range
              d<-d[!rownames(d)%in%out.index,]
              
            }
            
            
          }
          
          # If there is only 1 row with values we keep it only if it is within the range
          if(length(dat3$response[!is.na(dat3$response)])==1 & is.numeric(dat3$response.time)){  
            
            if(length(values.in.target)==0){
              out.index<-rownames(dat3)
              # remove whatever not within range
              d<-d[!rownames(d)%in%out.index,]
              
            }
            
            if(length(values.in.target)>0){
              
              index<-rownames(dat3[values.in.target,])  
              out.index<-rownames(dat3[which(!1:length(dat3$response)%in%values.in.target),])
              d[index,"dbb"]<-dat3[values.in.target,"response.time"]
              d[index,"maxperc_bb"]<-dat3[values.in.target,"response"]
              d[index,"minperc_bb"]<-dat3[values.in.target,"response"]
              d[index,"dist.50bb"]<-dat3[values.in.target,"response"]-target.percent
              # remove whatever not within range
              d<-d[!rownames(d)%in%out.index,]
            }
            
          }
        } 
        
      }
    }
    
    if(type=="BB_analysis"){
      d.subset.1<-subset(d,respvar.simple=="percentbudburst" & !is.na(response.time))
      #d.subset.2<-subset(d,!is.na(dbb))
      d.subset.2<-subset(d,respvar.simple!="percentbudburst")
      d.subsetted<-rbind(d.subset.1,d.subset.2)
      d.subsetted$response.time[which(!is.na(d.subsetted$dbb))]<-d.subsetted$dbb[which(!is.na(d.subsetted$dbb))]
    }
    
    if(type=="leave.nearest.2.target"){
      d.subset.1<-subset(d,respvar.simple=="percentbudburst" & !is.na(response.time))
      #d.subset.2<-subset(d,!is.na(dbb))
      d.subset.2<-subset(d,respvar.simple!="percentbudburst")
      d.subsetted<-rbind(d.subset.1,d.subset.2)
      d.subsetted$response.time[which(!is.na(d.subsetted$dbb))]<-d.subsetted$dbb[which(!is.na(d.subsetted$dbb))]
      
    }
    
    if(type=="add.columns"){
      d.subsetted <- d
    }
    
    if(type=="only.percentBB"){
      d.subsetted<-subset(d,!is.na(dbb))
    }
    
    return(d.subsetted)
    
  }
  
  d.subset <- subsetting.daysBB(d, targetvalue, "BB_analysis")
  d <- d.subset

} else {
  print("Error: d not a data.frame")
}
  ## Almost done! Now remove the values too low
  d.subset.1 <- subset(d, respvar.simple=="percentbudburst" & !is.na(response.time))
  d.subset.2 <- subset(d, respvar.simple!="percentbudburst")
  d.subset.1.rmlowvalues <- subset(d.subset.1, is.na(dbb)==FALSE)
  d.subsetted <- rbind(d.subset.1.rmlowvalues, d.subset.2)
  d<-d.subsetted
  
stop("Not an error, now BB data is all cleaned, ready to analyze and save;
     No need to worry about the warnings below, they inform that the dataset has empty
     elements that are assigned NA values by default"
)





