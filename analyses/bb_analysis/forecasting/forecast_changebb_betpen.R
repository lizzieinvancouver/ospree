## Started 23 Jan 2019 ##
## By Ailene  ##

## Marginal effects from Stan models for particular species##
## Based off models_stan_plotting_APC.R ##
## Applying our ospree model to forecast effects of warming under different
## climatic conditions (conditions chosen using locations and bbdoy within range of 
## BETPEN in PEP data)
## Started 23 Jan 2019 ##
## By Ailene  ##

############################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#libraries
library(RColorBrewer)
library(plyr)
library(dplyr)
library(rstan)
library(ggplot2)
library(gridExtra)
library(geosphere)
#library(rgl)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

climatedrive = "/Volumes/climate" #Ailene's climate data drive

figpath <- "figures"

## set up the flags
use.chillports = FALSE
use.zscore = FALSE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

##
source("source/bbstanleadin.R")
##

# Set up colors (more than used currently) ...
# non-z-scored models
if(use.zscore==FALSE & use.chillports == TRUE){
  load("stan/output/m2lni_spcompexprampfpcp_nonz.Rda") # m2l.ni with chill portions
  fit <- m2l.ni
}
if(use.zscore==FALSE & use.chillports == FALSE){
  load("stan/output/m2lni_spcompexprampfputah_nonz.Rda") # m2l.ni with chill portions
  fit <- m2l.ni
}

fit.sum <- summary(fit)$summary


#new function (for just one daylength) and 3d plots
getest.bb2 <- function(fit, forcetemp, chill, daylength){
  listofdraws <- extract(fit)
  
  avgbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*forcetemp +
    listofdraws$b_photo[,sp.num[s]]*daylength + listofdraws$b_chill[,sp.num[s]]*chill
  yebbest <- list(avgbb)
  return(yebbest)
}

# Select the species and temperature change that you want
sp<-c("betpen")
sp.num<-c(9)
s=1
tempforecast<-c(1,2,3,4,5,6,7)#enter in the amount of warming (in degrees C) you want to forecast 

  spfilename<-paste("PEP_climate/input/pep_",sp[s],"_all.csv", sep="")
  spdat<-read.csv(spfilename, header=T)
  spdat<-spdat[spdat$YEAR<1961,] 
  spdat$lat.lon<-paste(spdat$LAT,spdat$LON,sep=".")
  spdir<-paste(climatedrive,"/",sp[s],sep="")
  
  #dim(spdat)
  latlon <- spdat %>% # start with the data frame
    distinct(lat.lon, .keep_all = TRUE) %>% # establishing grouping variables
    dplyr::select(LAT,LON)
  latlon <- latlon[apply(latlon, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
minlat<-min(latlon$LAT)
maxlat<-max(latlon$LAT)
#if you just want to look at min and max lat:
#ls<-c(which(latlon$LAT==minlat),which(latlon$LAT==maxlat))
ls<-dim(latlon)[1]
allforecasts.forheatmap<-c()
#if R quits before whole thing is finished, stitch together csv files with the following code:

#betpenfiles<-list.files(path="..//output/betpen_for3dplot/betpenpepsites")
#for (i in 1:(length(betpenfiles))){
#  fname<-paste("..//output/betpen_for3dplot/betpenpepsites/",betpenfiles[i], sep="")
#  f<-read.csv(fname, header=T)
#  row.names(f)<-NULL
#  allforecasts.forheatmap<-rbind(allforecasts.forheatmap,f)
#}
#allforecasts.forheatmap<-allforecasts.forheatmap[,-which(colnames(allforecasts.forheatmap)=="X")]
tempfiles<-list.files(path=paste(climatedrive,"/",sp[s],sep=""),pattern="temp_forforecast__")
chillfiles<-list.files(path=paste(climatedrive,"/",sp[s],sep=""),pattern="chill_observed_")

#997, 1119, 1244, 1405,1636,1637,1639, 2215,2313,2318,missing for chillforecasts...
for(l in 1:ls){#left off at 1985
    lat<- latlon$LAT[l] 
    long<- latlon$LON[l] 
    print(paste(lat,long,l));
    #numsites<-dim(latlon)[1]
    chillfors<- chillforfiles[which(substr(chillforfiles,19,nchar(chillforfiles))==paste(lat,"_",long,"_1951_1961.csv", sep=""))]
    
    if (length(chillfors)==0) next #if there are no chillforecaste files- eventuall this should not be necessary! missing some for some reason. 
    
    chillall<-read.csv(paste(climatedrive,"/",sp[s],"/chill_observed_",lat,"_",long,"_1951_1961.csv",sep=""), header=TRUE) 
    tempall<-read.csv(paste(climatedrive,"/",sp[s],"/temp_forforecast__",lat,"_",long,"_1951_2014.csv",sep=""), header=TRUE)
#if(length(chillfors)
   
   #because we want a "pre-warming estimate" only use years before 1980 for temeperature (to match bb)
tempall<-tempall[tempall$Year<1962,]
chillall<-chillall[chillall$End_year<1962,]

#tempall$Tmean[tempall$Month>3 & tempall$Month<7 ]<-"spring"
sprtemp <- mean(tempall$Tmean[tempall$Month>2 & tempall$Month<5])#March-April 31 because thats what cat did

  #to get reasonable bb doy, use PEP observations
  pepdat<-spdat[spdat$LAT==lat & spdat$LON==long,]#get lat/long for which we're getting climate
  budburstdoy<-as.integer(mean(pepdat$DAY))              
  daylengthbbdoy <- daylength(lat, budburstdoy)#$Daylength
  chillport <- mean(chillall$Chill_portions)
  utah<-mean(chillall$Utah_Model)/240
  temps<-c(0,tempforecast)
  #make blank dataframes to fill with estimates without adhoc adjustments for daylength, for all combinations of chilling and forcing at different warming levels
  z.matrix.dl <- matrix(NA,ncol=length(temps),nrow=length(temps))
  chill.forecast<-rep(NA, times=8)
  sprT.forecast<-rep(NA,times=8)
  winT.forecast<-rep(NA,times=8)
  #Fill matrix row by row

  #the below takes a while to run.if you want to avoid running the loop
  #z.matrix<-read.csv("output/bbmodests_for3dplot_8hr.csv")
  #c=1

  for (c in 1:length(temps)){#c=chilling
    #print(temps[c]);
    if(c==1){chillests<-chillall}
    if (c>1){chillforfilename<-paste(spdir,"/","chillforecast",tempforecast[c-1],"deg_",lat,"_",long,"_1951_1961.csv",sep="")
      chillfor<-read.csv(chillforfilename, header=TRUE) 
      chillests<-chillfor}
    dl <- daylengthbbdoy
    if(use.chillports==FALSE){chill.forecast[c]<-mean(chillests$Utah_Model)/240}
    if(use.chillports==TRUE){chill.forecast[c]<-mean(chillests$Chill_portions)}
    
    winT.forecast[c]<-mean(chillests$mntemp)
  
    for(f in 1:length(temps)){#forcing/spring temp
      if(f==1){sprtemp<-mean(tempall$Tmean[tempall$Month>2 & tempall$Month<5])
      sprT.forecast[f]<-mean(tempall$Tmean[tempall$Month>2 & tempall$Month<5])}
     if(f>1) {sprtemp <-mean(tempall$Tmean[tempall$Month>2 & tempall$Month<5])+tempforecast[f-1]
      sprT.forecast[f]<-mean(tempall$Tmean[tempall$Month>2 & tempall$Month<5])+tempforecast[f-1]}
      if(use.chillports==TRUE){
        bbposteriors <- getest.bb2(fit,sprtemp, mean(chillests$Chill_portions), dl)
        print(sprtemp);print(mean(chillests$Chill_portions))}
      if(use.chillports==FALSE){
        bbposteriors <- getest.bb2(fit,sprtemp, mean(chillests$Utah_Model)/240, dl)}
      #print(sprtemp);print(mean(chillests$Utah_Model)/240)}
    
      meanz <- unlist(lapply(bbposteriors, mean))#returns  avgbb
      z.matrix.dl[c,f]<-meanz#8 hour daylength only for now
  }#f
}#c

  colnames(z.matrix.dl)<-paste("bb.sprtemp",temps, sep=".")
  rownames(z.matrix.dl)<-paste("bb.wintemp",temps, sep=".")
  allforecast<-as.data.frame(cbind(temps,sprT.forecast,winT.forecast,chill.forecast,z.matrix.dl))
  colnames(allforecast)[1]<-"warming_C"
  allforecast$lat<-lat
  allforecast$lon<-long
  
  if(use.chillports==TRUE){
    write.csv(allforecast,paste("..//output/bbmodests",sp[s],lat,long,"_for3dplot_cp.csv", sep=""))}
  if(use.chillports==FALSE){
    write.csv(allforecast,paste("..//output/betpen_for3dplot/bbmodests",sp[s],lat,long,"_for3dplot_utah.csv"))} 
  
  allforecasts.forheatmap<-rbind(allforecasts.forheatmap,allforecast)
}#l
write.csv(allforecasts.forheatmap,"..//output/betpen_for3dplot/betpen.forecast.forheatmap.csv", row.names=FALSE)
