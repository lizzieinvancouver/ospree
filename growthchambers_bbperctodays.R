###Ailene Ettinger, January 2016, script to convert files with 50% bb to "days to bb" and calculate max %bb per treatment per spp
##To do:
##convert 50% bb to days to bb
##calculate max %bb per treatment per spp

rm(list=ls()) 
options(stringsAsFactors=FALSE)

#setwd("~/Dropbox/Documents/Work/Wolkovich/retreat")
#get_pheno_est <- function(dat,percentbb){
#  dat_final <- data.frame(matrix(data=NA,nrow=0,ncol=0))
dat<-read.csv("growthchambers_litreview_2016-01-15.csv")
head(dat)
unique(dat$respvar)
dat$dbb<-NA
##Sites that use percent budburst
percbbsites<-dat[which(dat$respvar=="percentbudburst"),]
dim(percbbsites)
dataset<-unique(percbbsites$datasetID)
for(i in 1:length(dataset)){
dat1<-percbbsites[which(percbbsites$datasetID==dataset[i]),]
study<-unique(dat1$study)
##make new column that ombines all treatments (columns 1:33) into one columns
dat1$treatment <- paste(dat1$genus,dat1$species, dat1$varetc,dat1$population,dat1$other.treatment,dat1$dormancy_induction_temp,dat1$dormancy_induction_days,dat1$dormancy_induction_photoperiod_day,dat1$dormancy_induction_photoperiod_night,dat1$freeze.treatment.time,dat1$freeze.treatment.photoperiod_day,dat1$freeze.treatment.photoperiod_night,dat1$freeze.treatment.temp_day,dat1$freeze.treatment.temp_night,dat1$fieldchill,dat1$chilltemp,dat1$chillphotoperiod,dat1$chilldays,dat1$number.longdays,dat1$photoperiod_day,dat1$photoperiod_night,dat1$forcetemp,dat1$forcetemp_night, dat1$irradiance,dat1$humidity,sep = ".")
for(j in 1:length(study)){
  dat2<-dat1[which(dat1$study==study[j]),]
  treat=unique(dat2$treatment)
  for(k in 1:length(treat))
  dat3<-dat2[which(dat2$treatment==treat[k]),]
  maxperc_bb<-max(dat2$response)#maxpercent budburst
  dbb<-dat3[which(dat3$response==50),]$response.time#days to 50%bb
  if(length(dbb)==0){
    dbb<-dat3[which(abs(dat3$response-50)==min(abs(dat3$response-50))),]$response.time}#choose closest value to 50 for percent 
    dbb_perc<-dat3[which(abs(dat3$response-50)==min(abs(dat3$response-50))),]$response}#percemt
  if(dbb_perc>55|dbb_perc<45){ ###need to fix this so that it uses the latest 0 and the earliest max percent as start and end points for percent                      
    mod<-lm(as.numeric(dat3$response.time)~as.numeric(dat3$response))  #only use days to bb estimate if the percent is >45 or <55. If not, then use regression to get estimate for days to 50 percent
    dbb<-coef(mod)[1]+coef(mod)[2]*50
  }
}
}
##