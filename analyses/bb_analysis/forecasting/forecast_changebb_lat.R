## Started 23 Jan 2019 ##
## By Ailene  ##

## Marginal effects from Stan models for particular species##
## Based off models_stan_plotting_APC.R ##
## Applying our ospree model to forecast effects of warming under different
## climatic conditions (conditions chosen using locations and bbdoy within range of 
## BETPEN and FAGSYL in PEP data)
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

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


figpath <- "figures"

# dostan = TRUE
use.chillports = FALSE# change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = FALSE # change to false to use raw predictors

# Default is species complex and no crops
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE

# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE

#Default is all chilling data
use.expchillonly = FALSE # change to true for only experimental chilling 
#note: with only exp chilling, there is only exp photo and force too.
#also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
source("source/bbstanleadin.R")

bb.wlat <- bb.stan
bb.wlat <- within(bb.wlat, { prov.lat <- ave(provenance.lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
bb.wlat <- subset(bb.wlat, bb.wlat$prov.lat>1)  

lat.stan<-bb.wlat

lat.stan<-subset(lat.stan, lat.stan$resp<600)

lat.stan$lat <- lat.stan$provenance.lat

lat.stan$complex<-as.numeric(as.factor(lat.stan$complex.wname))

lat.stan<-na.omit(lat.stan)
if(use.chillports == FALSE & use.zscore == FALSE){
  datalist.lat.nonz <- with(lat.stan, 
                            list(y = resp, 
                                 chill = chill, 
                                 force = force, 
                                 photo = photo,
                                 lat = lat,
                                 sp = complex,
                                 N = nrow(lat.stan),
                                 n_sp = length(unique(lat.stan$complex))
                            )
  )
}
# Set up colors (more than used currently ...

cols <- adjustcolor(c("goldenrod", "lightskyblue","purple4"), alpha.f = 0.8) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4
# non-z-scored models

if(use.zscore==FALSE & use.chillports == FALSE){
  load("../lat_analysis/stan/m2l.inter.lat.nonz.Rda") #
  fit <- m2l.inter
}

fit.sum <- summary(fit)$summary

#rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")

# Select the species and temperature change that you want

#rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")

# Select the species and temperature change that you want
allsp<-unique(lat.stan$complex.wname)
sp<-"fagsyl"
sp.num<- 15
tempforecast<-c(1,2,3,4,5,6,7)#enter in the amount of warming (in degrees C) you want to forecast 

#Define the function we will use to estimate budburst
getspest.bb <- function(fit, sprtemp, daylength, chill, lat, warmspring, warmwinter,
                        daylengthwarmspr, daylengthwarmwin, daylengthwarmsprwin){
  listofdraws <- extract(fit)
  avgbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*sprtemp + 
    listofdraws$b_photo[,sp.num[s]]*daylength + listofdraws$b_chill[,sp.num[s]]*chill + 
    listofdraws$b_lat[,sp.num[s]]*lat + listofdraws$b_pl[,sp.num[s]]*daylength*lat
  
  warmsprbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*(sprtemp+warmspring) + 
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmspr) + listofdraws$b_chill[,sp.num[s]]*chill +
    listofdraws$b_lat[,sp.num[s]]*lat + listofdraws$b_pl[,sp.num[s]]*(daylength + daylengthwarmspr)*lat
  
  warmwinbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*sprtemp + 
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmwin) + listofdraws$b_chill[,sp.num[s]]*(chill+warmwinter) +
    listofdraws$b_lat[,sp.num[s]]*lat + listofdraws$b_pl[,sp.num[s]]*(daylength + daylengthwarmwin)*lat
  
  warmsprwinbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*(sprtemp+warmspring) +
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmsprwin) + listofdraws$b_chill[,sp.num[s]]*(chill+warmwinter) +
    listofdraws$b_lat[,sp.num[s]]*lat + listofdraws$b_pl[,sp.num[s]]*(daylength + daylengthwarmsprwin)*lat
  
  yebbest <- list(avgbb, warmsprbb, warmwinbb, warmsprwinbb)
  return(yebbest)
}

#Choose whether or not you want to use our adhoc shift in daylength.
use.daylengthshift=TRUE

quartz(width=5,height=5)
par(mar=c(8,4,3,4), mfrow=c(1,2))
#s=2
for(s in 1:length(sp)){
  #I have now added lots of sites for 
  #s=1#for bet, s=1; for fag, s=2
  numsites<-length(list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__"))
  tempfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__")
  chillfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="chill_observed_")
  spdir<-paste("../output/dailyclim/",sp[s],sep="")
  #create a blank dataframe for each species, in which to add estimates
  spests<-c()
  spestsqlo<-c()
  spestsqhi<-c()
  latlongs.toplot<-c("46.85_15.7333","47.7333_16.33","48.7833_15.4_")
  sites.toplot<-c(which(substr(tempfiles,19,31)==latlongs.toplot[1]),which(substr(tempfiles,19,31)==latlongs.toplot[2]),which(substr(tempfiles,19,31)==latlongs.toplot[3]))
  
  #quartz(width=11,height=5)
  pdf("figures/forecasting/fagsyl_3lats.pdf", width=11,height=5)
  
  par(mar=c(4,4,3,4), mfrow=c(1,3))
  for (i in sites.toplot){

    # Read in observed chilling from 1950 to 2014
    chillall<-read.csv(paste(spdir,"/",chillfiles[i],sep=""), header=TRUE) 
    tempall<-read.csv(paste(spdir,"/",tempfiles[i],sep=""), header=TRUE)
    #because we want a "pre-warming estimate" only use years before 1961 to match other analyses
    tempall<-tempall[tempall$Year<1961,]
    chillall<-chillall[chillall$End_year<1961,]
    sprtemp <- mean(tempall$Tmean[tempall$Month>2 & tempall$Month<5])#March 1-April 30
    lat<-as.numeric(strsplit(substr(chillfiles[i],16,nchar(chillfiles[i])-14),"_")[[1]][1])
    long<-as.numeric(strsplit(substr(chillfiles[i],16,nchar(chillfiles[i])-14),"_")[[1]][2])
    
    #to get reasonable bb doy, use PEP observations
    pepdat<-read.csv(paste("../limitingcues/input/PEP_",sp[s],".csv",sep=""), header=TRUE)
    pepdat<-pepdat[pepdat$YEAR<1961,]#restrict to pre-1961 to get "prewarming" bbdoy estimates
    pepdat<-pepdat[pepdat$LAT==lat & pepdat$LON==long,]#get lat/long for which we're getting climate
    budburstdoy<-as.integer(mean(pepdat$DAY))              
    #March 1#change this to the bbdoy observed in pep!
    daylengthbbdoy <- daylength(lat, budburstdoy)#$Daylength
    chillport <- mean(chillall$Chill_portions)
    chill<-mean(chillall$Utah_Model)/240
    
    #make blank dataframes to fill with estimates with and without our adhoc adjustments for daylength
    predicts <- as.data.frame(matrix(NA,ncol=8,nrow=7))
    predicts.25per <- as.data.frame(matrix(NA,ncol=8,nrow=7))
    predicts.75per <- as.data.frame(matrix(NA,ncol=8,nrow=7))
    
    predicts.wdl <- as.data.frame(matrix(NA,ncol=8,nrow=7))
    predicts.25per.wdl <- as.data.frame(matrix(NA,ncol=8,nrow=7))
    predicts.75per.wdl <- as.data.frame(matrix(NA,ncol=8,nrow=7))
    
    colnames(predicts)<-colnames(predicts.25per) <-colnames(predicts.75per) <-
      colnames(predicts.wdl)<-colnames(predicts.25per.wdl) <-colnames(predicts.75per.wdl) <-
      c("warming","nowarm","sprwarm","winwarm","bothwarm","sprtmp","chill","chillchange")
    print(lat)
    for (j in 1:length(tempforecast)){
      chillforfilename<-paste(spdir,"/","chillforecast",tempforecast[j],"deg_",lat,"_",long,"_1951_2014.csv",sep="")
      chillfor<-read.csv(chillforfilename, header=TRUE) 
      chillfor<-chillfor[chillfor$End_year<1961,]
      photo.forplot <- daylengthbbdoy
      warmspring <-tempforecast[j]
      if(use.chillports==TRUE){warmwinter <- mean(chillfor$Chill_portions)-chillport}
      if(use.chillports==FALSE){warmwinter <- (mean(chillfor$Utah_Model)/240)-chill}
      
      print(tempforecast[j]);print(warmwinter)
      if(use.chillports==TRUE){
      bbposteriors <- getspest.bb(fit, sprtemp, daylengthbbdoy, chillport, lat,warmspring, warmwinter, 0, 0, 0)}
    if(use.chillports==FALSE){
    bbposteriors <- getspest.bb(fit, sprtemp, daylengthbbdoy, chill, lat,warmspring, warmwinter, 0, 0, 0)}
  
        
       meanz <- unlist(lapply(bbposteriors, mean))
      
      quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
      
      quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
      quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
      daychange.springwarm<-meanz[2]-meanz[1] 
      daychange.wintwarm<-meanz[3]-meanz[1]
      daychange.bothwarm<-meanz[4]-meanz[1] 
      daylengthchange.springwarm<-daylength(lat,budburstdoy+daychange.springwarm)-daylengthbbdoy
      daylengthchange.wintwarm<- daylength(lat,budburstdoy+daychange.wintwarm)-daylengthbbdoy
      daylengthchange.bothwarm<-daylength(lat,budburstdoy+daychange.bothwarm)-daylengthbbdoy
      
      if(use.chillports==TRUE){
        bbposteriors.wdaylength <- getspest.bb(fit, sprtemp, daylengthbbdoy, chillport, lat,warmspring, warmwinter, daylengthchange.springwarm, daylengthchange.wintwarm, daylengthchange.bothwarm)
      }
      if(use.chillports==FALSE){
        bbposteriors.wdaylength <- getspest.bb(fit, sprtemp, daylengthbbdoy, chill, lat,warmspring, warmwinter, daylengthchange.springwarm, daylengthchange.wintwarm, daylengthchange.bothwarm)
      }
      
      meanz.wdaylength <- unlist(lapply(bbposteriors.wdaylength, mean))
      quant25per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.25))))
      quant75per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.75))))
      if(use.chillports==TRUE){
      predicts[j,]<-c(warmspring,meanz,chillport+warmwinter,sprtemp+warmspring,warmwinter)}
      if(use.chillports==FALSE){
      predicts[j,]<-c(warmspring,meanz,chill+warmwinter,sprtemp+warmspring,warmwinter)
      
      predicts.25per[j,]<-c(warmspring,quant25per,chill,sprtemp+warmspring,warmwinter)
      predicts.75per[j,]<-c(warmspring,quant75per,chill,sprtemp+warmspring,warmwinter)
      
      predicts.wdl[j,]<-c(warmspring,meanz.wdaylength,chill,sprtemp+warmspring,warmwinter)
      predicts.25per.wdl[j,]<-c(warmspring,quant25per.wdaylength,chill,sprtemp+warmspring,warmwinter)
      predicts.75per.wdl[j,]<-c(warmspring,quant75per.wdaylength,chill,sprtemp+warmspring,warmwinter)
      }
    }
    
    if(use.chillports==TRUE){    
    predicts<-rbind(c(0,predicts$nowarm[1:4],chillport,lat,0),predicts)}
    
    if(use.chillports==FALSE){
    predicts<-rbind(c(0,predicts$nowarm[1:4],chill,lat,0),predicts)}
    
    predicts<-predicts[,-2]
    predicts.25per<-rbind(c(0,predicts.25per$nowarm[1:4]),predicts.25per)
    predicts.25per<-predicts.25per[,-2]
    predicts.75per<-rbind(c(0,predicts.75per$nowarm[1:4]),predicts.75per)
    predicts.75per<-predicts.75per[,-2]
    
    predicts.wdl<-rbind(c(0,predicts.wdl$nowarm[1:4]),predicts.wdl)
    predicts.wdl<-predicts.wdl[,-2]
    predicts.25per.wdl<-rbind(c(0,predicts.25per.wdl$nowarm[1:4]),predicts.25per.wdl)
    predicts.25per.wdl<-predicts.25per.wdl[,-2]
    predicts.75per.wdl<-rbind(c(0,predicts.75per.wdl$nowarm[1:4]),predicts.75per.wdl)
    predicts.75per.wdl<-predicts.75per.wdl[,-2]
    
    predicts$lat<-lat
    predicts$lon<-long
    predicts.25per$lat<-lat
    predicts.25per$lon<-long
    predicts.75per$lat<-lat
    predicts.75per$lon<-long
    spests<-rbind(spests,predicts)
    spestsqlo<-rbind(spestsqlo,predicts.25per)
    spestsqhi<-rbind(spestsqhi,predicts.75per)
    
    ymin = 10#min(predicts[,-1],predicts.25per[,-1],predicts.75per[,-1])
    ymax = 30#max(predicts[,-1],predicts.25per[,-1],predicts.75per[,-1])
    xlim = c(0, 7)
    ylim = c(ymin,ymax)   
    
    plot(x=NULL,y=NULL, xlim=xlim, xlab="Amount of warming (°C)", ylim=ylim,
         ylab="Days to budburst", bty="l")#main=paste(sp[s],", lat=",round(lat,digits=2),", doy=",budburstdoy,", ",round(daylengthbbdoy, digits=0)," hrs", sep=""), 
    pos.x <- 0
    pos.y <- predicts[1,2]
    #Add shading around line for credible intervals
    if(i==sites.toplot[1]) {
      legend("bottomleft",legend=c("Warming only","Warming with daylength shifts"),lty=c(1,2),lwd=2,col=c(cols[3],cols[1]),bty="n", cex=1.1)
      mtext("A)",side=3, adj=0, line=1)
    }
    if(i==sites.toplot[2]) {mtext("B)",side=3, adj=0, line=1)}
    if(i==sites.toplot[3]) {mtext("C)",side=3, adj=0, line=1)}
    
    for(t in 5){
       polygon(c(rev(predicts$warming), predicts$warming), c(rev(predicts.75per[,t-1]), predicts.25per[,t-1]), col = alpha(cols[t-2], 0.2), border = NA)
     }
    #wdl
    for(t in 5){
      polygon(c(rev(predicts.wdl$warming), predicts.wdl$warming), c(rev(predicts.75per.wdl[,t-1]), predicts.25per.wdl[,t-1]), col = alpha(cols[t-4], 0.2), border = NA)
    }
    for(t in 5){
      lines(predicts$warming, predicts[,t-1], 
           col=cols[t-2], lwd=2)}
    for(t in 5){#to compare lines with potential shifts in daylength that may occur with warming
      lines(predicts.wdl$warming, predicts.wdl[,t-1], 
            col=cols[t-4], lwd=2, lty=2)}
    
    # int<-summary(fit)$summary
    # sp.ints<-fit.sum[grep("a_sp",rownames(fit.sum)),]
    # sp.fos<-fit.sum[grep("b_force",rownames(fit.sum)),]
    # sp.chs<-fit.sum[grep("b_chill",rownames(fit.sum)),]
    # sp.phs<-fit.sum[grep("b_photo",rownames(fit.sum)),]
    # 
    # sp.int<-sp.ints[sp.num[s]+2,1]
    # sp.fo<-sp.fos[sp.num[s]+2,1]
    # sp.ch<-sp.chs[sp.num[s]+2,1]
    # sp.ph<-sp.phs[sp.num[s]+2,1]
    # #if(i==1){
    # mtext(paste("a_sp[",sp.num[s],"]=",round(sp.int, digits=2), sep=""), side=1, line=-5, adj=0, cex=.8)
    # mtext(paste("b_force[",sp.num[s],"]=",round(sp.fo, digits=2), sep=""), side=1, line=-4, adj=0, cex=.8)
    # mtext(paste("b_chill[",sp.num[s],"]=",round(sp.ch, digits=2), sep=""), side=1, line=-3, adj=0,cex=.8)
    # mtext(paste("b_photo[",sp.num[s],"]=",round(sp.ph, digits=2), sep=""), side=1, line=-2, adj=0, cex=.8)
    # mtext(paste("prewarm temp:",round(sprtemp, digits=0),", chill=",round(chillport, digits=0), sep=""), side=1, line=-1, adj=0, cex=.8)
    # 
    }
    
}
dev.off()
  #name<-paste(sp[s],lat,long,"forecast.csv",sep=".")
  #nameqlo<-paste(sp[s],lat,long,"forecast.qlo.csv",sep=".")
  #nameqhi<-paste(sp[s],lat,long,"forecast.qhi.csv",sep=".")
  #write.csv(spests,name, row.names = FALSE)
  #write.csv(spestsqlo,name, row.names = FALSE)
  #write.csv(spestsqhi,name, row.names = FALSE)
}
