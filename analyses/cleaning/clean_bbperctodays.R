###Ailene Ettinger, January 2016, script to convert files with 50% bb to "days to bb" and calculate max %bb per treatment per spp
##To do:
##convert 50% bb to days to bb
##calculate max %bb per treatment per spp

# Edited DF
# Run this after running Jehane's script (Lizzie thinks this is clean_respvar.R) 

rm(list=ls()) 
options(stringsAsFactors=FALSE)

setwd("~/Documents/git/budreview")
# setwd("~/Documents/git/projects/treegarden/budreview/ospree")

dat<-read.csv("growthchambers_litreview_clean1.csv") # after response variables cleaned

dat$response.time = as.numeric(as.character(dat$response.time))

dat_final <- data.frame(matrix(data=NA,nrow=0,ncol=6))

colnames(dat_final) <- c("datasetID","study","treatment","dbb","maxperc_bb","minperc_bb")

##Sites that use percent budburst
percbbsites<-dat[which(dat$respvar=="percentbudburst"),] # Make sure this is after Jehane's script 

# dim(percbbsites)

dataset <- unique(percbbsites$datasetID)

for(i in 1:length(dataset)){ # i = 10

  dat1 <- percbbsites[which(percbbsites$datasetID==dataset[i]),]

  study<-unique(dat1$study)
  ##make new column that combines all treatments (columns 1:33) into one columns
  dat1$treatment <- paste(dat1$genus,dat1$species, dat1$varetc,dat1$population,dat1$other.treatment,dat1$dormancy_induction_temp,dat1$dormancy_induction_days,dat1$dormancy_induction_photoperiod_day,dat1$dormancy_induction_photoperiod_night,dat1$freeze.treatment.time,dat1$freeze.treatment.photoperiod_day,dat1$freeze.treatment.photoperiod_night,dat1$freeze.treatment.temp_day,dat1$freeze.treatment.temp_night,dat1$fieldchill,dat1$chilltemp,dat1$chillphotoperiod,dat1$chilldays,dat1$number.longdays,dat1$photoperiod_day,dat1$photoperiod_night,dat1$forcetemp,dat1$forcetemp_night, dat1$irradiance,dat1$humidity,sep = ".")

for(j in 1:length(study)){ # j = 1
  dat2<-dat1[which(dat1$study==study[j]),]
  treat=unique(dat2$treatment)
	dat2$response <- as.numeric(as.character(dat2$response))

  for(k in 1:length(treat)) { # k = 1 # Do we need to loop within treatment, not just study?
  
    dat3 <- dat2[which(dat2$treatment==treat[k]),]

    maxperc_bb<-max(as.numeric(dat3$response)) # maxpercent budburst
    minperc_bb<-min(as.numeric(dat3$response)) # minpercent budburst - hopefully zero
    
    # Check if this can be estimated at all.
    if(length(dat3$response[!is.na(dat3$response)])>1 & is.numeric(dat3$response.time) & length(unique(dat3$response.time))>1) {
    
  if(length(dat3[which(dat3$response==50),]$response.time)==1){dbb<-dat3[which(dat3$response==50),]$response.time}else#days to 50%bb if there is exactly one measurement at 50% bb
  
  if(length(dat3[which(dat3$response==50),]$response.time)==0){ # If there are no 50's
    
      dbb<-dat3[which(abs(dat3$response - 50) == min(abs(dat3$response-50))),]$response.time#choose closest value to 50 for percent 

    dbb_perc<-dat3[which(abs(dat3$response-50)==min(abs(dat3$response-50))),]$response
  # will be all zeros if both max and min are zero
      
    } #percent
  if(dbb_perc > 55 | dbb_perc < 45 & length(dat3$response)>1){ ###need to fix this so that it uses the latest 0 and the earliest max percent as start and end points for percent                      
    dat3$response=round(dat3$response, digits=0)
    dat4 <- dat3[c(which(dat3$response==min(dat3$response, na.rm=TRUE)), min(which(dat3$response==max(dat3$response,na.rm=TRUE)),na.rm=TRUE)),]
    
    mod<-lm(as.numeric(dat4$response.time)~as.numeric(dat4$response))  #only use days to bb estimate if the percent is >45 or <55. If not, then use regression to get estimate for days to 50 percent
    dbb<-coef(mod)[1]+coef(mod)[2]*50
     
  }
  
  if(dbb_perc>55|dbb_perc<45 & length(na.omit(dat3$response))<2){ ###need to fix this so that it uses the latest 0 and the earliest max percent as start and end points for percent                      
    dat3$response=round(dat3$response, digits=0)
    dat4<-dat3[c(which(dat3$response==min(dat3$response, na.rm=TRUE)), min(which(dat3$response==max(dat3$response,na.rm=TRUE))),na.rm=TRUE),]
    mod<-lm(as.numeric(dat4$response.time)~as.numeric(dat4$response))  #only use days to bb estimate if the percent is >45 or <55. If not, then use regression to get estimate for days to 50 percent
    dbb<-coef(mod)[1]+coef(mod)[2]*50
    
    } else dbb = NA
   
     sum_dbb<-cbind(dataset[i],study[j],treat[k],dbb,maxperc_bb,minperc_bb)
    
     colnames(sum_dbb) <- c("datasetID","study","treatment","dbb","maxperc_bb","minperc_bb")
   
     dat_final<- rbind(dat_final,sum_dbb)  
     
    } 
  
  }
}

}

dbb = dat_final

save(list=c('dbb'), file = "Days to BB.Rdata")
