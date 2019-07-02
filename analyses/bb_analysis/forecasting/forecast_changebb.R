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
library(rgl)
detach("package:chillR", unload=TRUE)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


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

# Set up colors (more than used currently ...

cols <- adjustcolor(c("maroon4", "lightskyblue","purple4"), alpha.f = 0.8) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4
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

#rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")

# Select the species and temperature change that you want

#rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")

# Select the species and temperature change that you want
sp<-c("betpen","fagsyl")
sp.num<-c(9,15)
tempforecast<-c(1,2,3,4,5,6,7)#enter in the amount of warming (in degrees C) you want to forecast 

#Define the function we will use to estimate budburst
getspest.bb <- function(fit, sprtemp, daylength, chill, warmspring, warmwinter,
                        daylengthwarmspr, daylengthwarmwin, daylengthwarmsprwin){
  listofdraws <- extract(fit)
  avgbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*sprtemp +
    listofdraws$b_photo[,sp.num[s]]*daylength + listofdraws$b_chill[,sp.num[s]]*chill

  warmsprbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*(sprtemp+warmspring) +
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmspr) + listofdraws$b_chill[,sp.num[s]]*chill

  warmwinbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*sprtemp +
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmwin) + listofdraws$b_chill[,sp.num[s]]*(chill+warmwinter)

  warmsprwinbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*(sprtemp+warmspring) +
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmsprwin) + listofdraws$b_chill[,sp.num[s]]*(chill+warmwinter)

  yebbest <- list(avgbb, warmsprbb, warmwinbb, warmsprwinbb)
  return(yebbest)
}

#Choose whether or not you want to use our adhoc shift in daylength.
use.daylengthshift=FALSE
title = TRUE

if(title == TRUE){
  figname<-paste("tempforecastbothspp",min(tempforecast),max(tempforecast),"degwarm_title.pdf", sep="_")}
if(title == FALSE){
  figname<-paste("tempforecastbothspp",min(tempforecast),max(tempforecast),"degwarm.pdf", sep="_")}

pdf(file.path(figpath,figname), width = 9, height = 9)

#quartz(width=6,height=6)

if(title==TRUE){par(mar=c(6,4,3,4), mfrow=c(2,2))}
if(title==FALSE){par(mar=c(2,2,2,2), oma=c(5,4,2,2), mfrow=c(2,2))}

#can do this as a loop, but i just want to pick 2 sites for now- max lat and min lat for each species
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
  #sites.toplot<-c(1,50)#just plot 2 sites for now- min lat and max lat
  if(s==2){sites.toplot<-c(1,21)}
  if(s==1){sites.toplot<-c(38,1)}


  for (i in sites.toplot){
    #for now do with i=1 and i=50 for each species (min and max)
    # Read in observed chilling from 1950 to 2014
    chillall<-read.csv(paste(spdir,"/",chillfiles[i],sep=""), header=TRUE)
    tempall<-read.csv(paste(spdir,"/",tempfiles[i],sep=""), header=TRUE)
    #because we want a "pre-warming estimate" only use years before 1980 for temeperature (to match bb)
    tempall<-tempall[tempall$Year<1980,]
    chillall<-chillall[chillall$End_year<1980,]
    sprtemp <- mean(tempall$Tmean[tempall$Month>2 & tempall$Month<5])#March 1-April 30 (4 degrees C)
    lat<-as.numeric(strsplit(substr(chillfiles[i],16,nchar(chillfiles[i])-14),"_")[[1]][1])
    long<-as.numeric(strsplit(substr(chillfiles[i],16,nchar(chillfiles[i])-14),"_")[[1]][2])

    #to get reasonable bb doy, use PEP observations
    pepdat<-read.csv(paste("../limitingcues/input/PEP_",sp[s],".csv",sep=""), header=TRUE)
    pepdat<-pepdat[pepdat$YEAR<1980,]#restrict to pre-1980 to get "prewarming" bbdoy estimates
    pepdat<-pepdat[pepdat$LAT==lat & pepdat$LON==long,]#get lat/long for which we're getting climate
    budburstdoy<-as.integer(mean(pepdat$DAY))
    #March 1#change this to the bbdoy observed in pep!
    daylengthbbdoy <- daylength(lat, budburstdoy)#$Daylength
    chillport <- mean(chillall$Chill_portions)
    chill<-mean(chillall$Utah_Model)/240

    #make blank dataframes to fill with estimates with and without our adhoc adjustments for daylength
    predicts <- as.data.frame(matrix(NA,ncol=5,nrow=7))
    predicts.25per <- as.data.frame(matrix(NA,ncol=5,nrow=7))
    predicts.75per <- as.data.frame(matrix(NA,ncol=5,nrow=7))

    predicts.wdl <- as.data.frame(matrix(NA,ncol=5,nrow=7))
    predicts.25per.wdl <- as.data.frame(matrix(NA,ncol=5,nrow=7))
    predicts.75per.wdl <- as.data.frame(matrix(NA,ncol=5,nrow=7))

    colnames(predicts)<-colnames(predicts.25per) <-colnames(predicts.75per) <-
      colnames(predicts.wdl)<-colnames(predicts.25per.wdl) <-colnames(predicts.75per.wdl) <-
      c("warming","nowarm","sprwarm","winwarm","bothwarm")
    print(lat)
    for (j in 1:length(tempforecast)){
      chillforfilename<-paste(spdir,"/","chillforecast",tempforecast[j],"deg_",lat,"_",long,"_1951_2014.csv",sep="")
      chillfor<-read.csv(chillforfilename, header=TRUE)
      photo.forplot <- daylengthbbdoy
      warmspring <-tempforecast[j]
      if(use.chillports==TRUE){warmwinter <- mean(chillfor$Chill_portions)-chillport}
      if(use.chillports==FALSE){warmwinter <- (mean(chillfor$Utah_Model)/240)-chill}

      print(tempforecast[j]);print(warmwinter)
      if(use.chillports==TRUE){
      bbposteriors <- getspest.bb(fit, sprtemp, daylengthbbdoy, chillport, warmspring, warmwinter, 0, 0, 0)}
    if(use.chillports==FALSE){
    bbposteriors <- getspest.bb(fit, sprtemp, daylengthbbdoy, chill, warmspring, warmwinter, 0, 0, 0)}


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
        bbposteriors.wdaylength <- getspest.bb(fit, sprtemp, daylengthbbdoy, chillport, warmspring, warmwinter, daylengthchange.springwarm, daylengthchange.wintwarm, daylengthchange.bothwarm)
      }
      if(use.chillports==FALSE){
        bbposteriors.wdaylength <- getspest.bb(fit, sprtemp, daylengthbbdoy, chill, warmspring, warmwinter, daylengthchange.springwarm, daylengthchange.wintwarm, daylengthchange.bothwarm)
      }

      meanz.wdaylength <- unlist(lapply(bbposteriors.wdaylength, mean))
      quant25per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.25))))
      quant75per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.75))))
      if(use.chillports==TRUE){
      predicts[j,]<-c(warmspring,meanz,chillport,warmwinter)}
      if(use.chillports==FALSE){
      predicts[j,]<-c(warmspring,meanz,chill,warmwinter)}

      predicts.25per[j,]<-c(warmspring,quant25per)
      predicts.75per[j,]<-c(warmspring,quant75per)
      predicts.wdl[j,]<-c(warmspring,meanz.wdaylength)
      predicts.25per.wdl[j,]<-c(warmspring,quant25per.wdaylength)
      predicts.75per.wdl[j,]<-c(warmspring,quant75per.wdaylength)
    }

    if(use.chillports==TRUE){
    predicts<-rbind(c(0,predicts$nowarm[1:4],chillport,0),predicts)}

    if(use.chillports==FALSE){
    predicts<-rbind(c(0,predicts$nowarm[1:4],chill,0),predicts)}

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
    # if(use.daylengthshift==TRUE){
    #   predicts<-predicts.wdl
    #   predicts.25per<-predicts.25per.wdl
    #   predicts.75per<-predicts.75per.wdl
    # }
    predicts$lat<-lat
    predicts$lon<-long
    predicts.25per$lat<-lat
    predicts.25per$lon<-long
    predicts.75per$lat<-lat
    predicts.75per$lon<-long
    spests<-rbind(spests,predicts)
    spestsqlo<-rbind(spestsqlo,predicts.25per)
    spestsqhi<-rbind(spestsqhi,predicts.75per)

    ymin = 13#min(predicts[,-1],predicts.25per[,-1],predicts.75per[,-1])
    ymax = 35#max(predicts[,-1],predicts.25per[,-1],predicts.75per[,-1])
    xlim = c(0, 7)
    ylim = c(ymin,ymax)
  
    if(title== TRUE){maintext= paste(sp[s],", lat=",round(lat,digits=2),", doy=",budburstdoy,", ",round(daylengthbbdoy, digits=0)," hrs", sep="")}
    if(title== FALSE){maintext= ""}
       
    plot(x=NULL,y=NULL, xlim=xlim, xlab="Amount of warming (°C)", ylim=ylim,
         ylab="Days to budburst", main=maintext, bty="l", cex.axis = 1.2, cex.lab = 1.2)
    pos.x <- 0
    pos.y <- predicts[1,2]
    points(pos.x, pos.y, cex=1.2, pch=19, bg="gray")
    #Add shading around line for credible intervals

     for(t in 3:5){
       polygon(c(rev(predicts$warming), predicts$warming), c(rev(predicts.75per[,t-1]), predicts.25per[,t-1]), col = alpha(cols[t-2], 0.2), border = NA)
     }

    for(t in 3:5){
      lines(predicts$warming, predicts[,t-1],
            col=cols[t-2], lwd=2)}
    #for(t in 3:5){#to compare lines with potential shifts in daylength that may occur with warming
    #  lines(predicts.wdl$warming, predicts.wdl[,t-1],
    #        col=cols[t-2], lwd=2, lty=2)}

    int<-summary(fit)$summary
    sp.ints<-fit.sum[grep("a_sp",rownames(fit.sum)),]
    sp.fos<-fit.sum[grep("b_force",rownames(fit.sum)),]
    sp.chs<-fit.sum[grep("b_chill",rownames(fit.sum)),]
    sp.phs<-fit.sum[grep("b_photo",rownames(fit.sum)),]
    sp.int<-sp.ints[sp.num[s]+2,1]
    sp.fo<-sp.fos[sp.num[s]+2,1]
    sp.ch<-sp.chs[sp.num[s]+2,1]
    sp.ph<-sp.phs[sp.num[s]+2,1]
    #if(i==1){
    #mtext(paste("a_sp[",sp.num[s],"]=",round(sp.int, digits=2), sep=""), side=1, line=-5, adj=0, cex=.8)
    #mtext(paste("b_force[",sp.num[s],"]=",round(sp.fo, digits=2), sep=""), side=1, line=-4, adj=0, cex=.8)
    #mtext(paste("b_chill[",sp.num[s],"]=",round(sp.ch, digits=2), sep=""), side=1, line=-3, adj=0,cex=.8)
    #mtext(paste("b_photo[",sp.num[s],"]=",round(sp.ph, digits=2), sep=""), side=1, line=-2, adj=0, cex=.8)
    #mtext(paste("prewarm temp:",round(sprtemp, digits=0),", chill=",round(chillport, digits=0), sep=""), side=1, line=-1, adj=0, cex=.8)

    #}
    if(i==38 & s==1)
      {
      legend("topleft",legend=c("Spring warming","Winter warming","Both"),lty=c(1,1,1),lwd=2,col=cols,bty="n", cex=1.2)
    }
    if(s==2 & i==1)
    {
      mtext("Days to budburst", side=2,line=2, adj=.5, cex= 1.2, outer=TRUE)
    }
    if(i==38 & s==1)
    {
      mtext("Amount of warming (°C)", side=1,line=2, adj=.5, cex= 1.2, outer=TRUE)
    }
    if(use.daylengthshift==TRUE)
      {
      legend("topright",legend=c("Spring warming","Winter warming","Both","with daylength shifts"),lty=c(1,1,1,2),lwd=2,col=cols,bty="n", cex=0.8)
      }
    
    # intervals
    # for(i in 3:5){
    #   lines(predicts.25per$warming, predicts.25per[,i-1],
    #         col=cols[i-2], lwd=1, lty=2)
    # }
    # for(i in 3:5){
    #   lines(predicts.75per$warming, predicts.75per[,i-1],
    #         col=cols[i-2], lwd=1, lty=2)
    # }
   
  }
  name<-paste(sp[s],lat,long,"forecast.csv",sep=".")
  nameqlo<-paste(sp[s],lat,long,"forecast.qlo.csv",sep=".")
  nameqhi<-paste(sp[s],lat,long,"forecast.qhi.csv",sep=".")
  write.csv(spests,name, row.names = FALSE)
  write.csv(spestsqlo,name, row.names = FALSE)
  write.csv(spestsqhi,name, row.names = FALSE)
}

dev.off()
#calculate difference in bb between no warming and warmed for each row
#spests$bbnowarm<-rep(spests$bothwarm[spests$warming==0], each=8)

#spests$bbchange<-spests$bothwarm-spests$bbnowarm

#3d plots
#new function (for just one daylength)
getest.bb2 <- function(fit, forcetemp, chill, daylength){
  listofdraws <- extract(fit)
  
  avgbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*forcetemp +
    listofdraws$b_photo[,sp.num[s]]*daylength + listofdraws$b_chill[,sp.num[s]]*chill
  yebbest <- list(avgbb)
  return(yebbest)
}

##converting forecast plots to 3d, not including change in daylength and not including error
#choose one site

#need to work on setting it up so that it looks good without tweaking by hand...
open3d() 
mfrow3d(2, 2, sharedMouse = TRUE)
#quartz()
#par(mfrow=c(2,2))
for(s in 1:length(sp)){
  
  if (s==1){is<-c(1,50) }
  if (s==2){is<-c(50,1) }
  
 numsites<-length(list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__"))
 tempfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__")
 chillfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="chill_observed_")
 spdir<-paste("../output/dailyclim/",sp[s],sep="")
   for(i in is){
   chillall<-read.csv(paste(spdir,"/",chillfiles[i],sep=""), header=TRUE) 
   tempall<-read.csv(paste(spdir,"/",tempfiles[i],sep=""), header=TRUE)
   #because we want a "pre-warming estimate" only use years before 1980 for temeperature (to match bb)
   tempall<-tempall[tempall$Year<1961,]
   chillall<-chillall[chillall$End_year<1961,]
 
   sprtemp <- mean(tempall$Tmean[tempall$Month>2 & tempall$Month<6])#March-May (4 degrees C) Should it be April-June instead (12 degrees C)?
   #extract the lat/long from the file name...argh!
   lat<-as.numeric(strsplit(substr(chillfiles[i],16,nchar(chillfiles[i])-14),"_")[[1]][1])
   long<-as.numeric(strsplit(substr(chillfiles[i],16,nchar(chillfiles[i])-14),"_")[[1]][2])
 
#   #to get reasonable bb doy, use PEP observations
   pepdat<-read.csv(paste("../limitingcues/input/PEP_",sp[s],".csv",sep=""), header=TRUE)
   pepdat<-pepdat[pepdat$YEAR<1961,]#restrict to pre-1961 to get "prewarming" bbdoy estimates
   pepdat<-pepdat[pepdat$LAT==lat & pepdat$LON==long,]#get lat/long for which we're getting climate
   budburstdoy<-as.integer(mean(pepdat$DAY))              
   daylengthbbdoy <- daylength(lat, budburstdoy)#$Daylength
   chillport <- mean(chillall$Chill_portions)
   chill <- mean(chillall$Utah_Model)/240
#   
   temps<-c(0,tempforecast)
   #make blank dataframes to fill with estimates without adhoc adjustments for daylength, for all combinations of chilling and forcing at different warming levels
# 
   z.matrix.dl <- matrix(NA,ncol=length(temps),nrow=length(temps))
   chill.forecast<-rep(NA, times=8)
   sprT.forecast<-rep(NA,times=8)
   winT.forecast<-rep(NA,times=8)
   #Fill matrix row by row


  #c=1

  for (c in 1:length(temps)){#c=chilling
    print(temps[c]);
    if(c==1){chillests<-chillall}
    if (c>1){chillforfilename<-paste(spdir,"/","chillforecast",tempforecast[c-1],"deg_",lat,"_",long,"_1951_2014.csv",sep="")
    chillfor<-read.csv(chillforfilename, header=TRUE)
    chillests<-chillfor}
    dl <- daylengthbbdoy
    if(use.chillports==TRUE){chill.forecast[c]<-mean(chillests$Chill_portions)}
    if(use.chillports==FALSE){chill.forecast[c]<-mean(chillests$Utah_Model)/240}
    winT.forecast[c]<-mean(chillests$mntemp)

    for(f in 1:length(temps)){#forcing/spring temp
      if(f==1){sprtemp<-mean(tempall$Tmean[tempall$Month>2 & tempall$Month<6])
      sprT.forecast[f]<-mean(tempall$Tmean[tempall$Month>2 & tempall$Month<6])}
     if(f>1) {sprtemp <-mean(tempall$Tmean[tempall$Month>2 & tempall$Month<6])+tempforecast[f-1]
      sprT.forecast[f]<-mean(tempall$Tmean[tempall$Month>2 & tempall$Month<6])+tempforecast[f-1]}
      if(use.chillports==TRUE){
        bbposteriors <- getest.bb2(fit,sprtemp, mean(chillests$Chill_portions), dl)
        print(sprtemp);print(mean(chillests$Chill_portions))}
      if(use.chillports==FALSE){
        bbposteriors <- getest.bb2(fit,sprtemp, mean(chillests$Utah_Model)/240, dl)
        print(sprtemp);print(mean(chillests$Utah_Model)/240)}

      meanz <- unlist(lapply(bbposteriors, mean))#returns  avgbb
      z.matrix.dl[c,f]<-meanz#8 hour daylength only for now
    }#f
    }#c

colnames(z.matrix.dl)<-paste("bb.sprtemp",temps, sep=".")
rownames(z.matrix.dl)<-paste("bb.wintemp",temps, sep=".")
allforecast<-cbind(temps,sprT.forecast,winT.forecast,chill.forecast,z.matrix.dl)
colnames(allforecast)[1]<-"warming_C"

if(use.chillports==TRUE){
  write.csv(allforecast,paste("..//output/bbmodests",sp[s],lat,long,"_for3dplot_cp.csv", sep=""))}
if(use.chillports==FALSE){
  write.csv(allforecast,paste("..//output/bbmodests",sp[s],lat,long,"_for3dplot_utah.csv"))}
allforecast<-as.data.frame(allforecast)
#to avoid running the above code
# if(use.chillports==TRUE){
#   allforecast<-read.csv(paste("..//output/bbmodests",sp[s],lat,long,"_for3dplot_cp.csv",sep=""), header=TRUE)}
# if(use.chillports==FALSE){
#   allforecast<-read.csv(paste("..//output/bbmodests",sp[s],lat,long,"_for3dplot_utah.csv", sep=""),header=TRUE)}

x=temps
y=temps
z=z.matrix.dl

zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1

colorlut <- heat.colors(zlen) # height color lookup table

col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point

plot3d(z,
       xlim = range(x), ylim = range(y), zlim = c(5,30),
       xlab = '',
       ylab = '', zlab = '', axes=FALSE)
aspect3d(1,1,1)
axes3d(edges=c("x--", "y+-", "z--"), box=TRUE, tick=TRUE, labels=TRUE)

#axis3d(edge="x", at = NULL, labels = TRUE, tick = TRUE, line = 0,
#       pos = NULL)
axis3d(edge="y+-", at = NULL, labels = TRUE, tick = TRUE, line = 0,
       pos = NULL,box=TRUE)
axis3d(edge="z--", at = NULL, labels = TRUE, tick = TRUE, line = 0,
       pos = NULL,box=TRUE)
axes3d(edges="bbox", labels=FALSE, tick = FALSE, box=TRUE)

#axes3d(edges=c("x--", "y+-", "z--"), box=TRUE)
surface3d(x,y, z,
          col=col, back = "lines")
#Make plots of changes in chilling estimates with warming

plot(allforecast$warming_C,allforecast$chill.forecast*240, type = "l", xlab="Warming (C)",ylab="Estimated chilling (Utah)", lwd=2, col="darkblue")
mtext(expression(~degree*C), side=1, line=-1, adj =.35, cex=0.8)
wint<-round(allforecast$winT.forecast[1], digits=1)
mtext(paste("Temp =",wint), side=1, line=-1, adj =.05, cex=.8)

  }#i
}#s

if(use.chillports==FALSE){
rgl.snapshot("figures/forecasting/tempforecastbothspp_1_7_degwarm_3D_utah.png")}
if(use.chillports==FALSE){
rgl.postscript("figures/forecasting/tempforecastbothspp_1_7_degwarm_3D_utah.pdf", "pdf")}

quartz()
par(mfrow=c(2,2))
for(s in 1:length(sp)){
  
  if (s==1){is<-c(1,50) }
  if (s==2){is<-c(50,1) }
  
  numsites<-length(list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__"))
  tempfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__")
  chillfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="chill_observed_")
  spdir<-paste("../output/dailyclim/",sp[s],sep="")
  for(i in is){
    chillall<-read.csv(paste(spdir,"/",chillfiles[i],sep=""), header=TRUE) 
    tempall<-read.csv(paste(spdir,"/",tempfiles[i],sep=""), header=TRUE)
    #because we want a "pre-warming estimate" only use years before 1980 for temeperature (to match bb)
    tempall<-tempall[tempall$Year<1961,]
    chillall<-chillall[chillall$End_year<1961,]
    
    sprtemp <- mean(tempall$Tmean[tempall$Month>2 & tempall$Month<6])#March-May (4 degrees C) Should it be April-June instead (12 degrees C)?
    #extract the lat/long from the file name...argh!
    lat<-as.numeric(strsplit(substr(chillfiles[i],16,nchar(chillfiles[i])-14),"_")[[1]][1])
    long<-as.numeric(strsplit(substr(chillfiles[i],16,nchar(chillfiles[i])-14),"_")[[1]][2])
    
    
    #   #to get reasonable bb doy, use PEP observations
    pepdat<-read.csv(paste("../limitingcues/input/PEP_",sp[s],".csv",sep=""), header=TRUE)
    pepdat<-pepdat[pepdat$YEAR<1961,]#restrict to pre-1961 to get "prewarming" bbdoy estimates
    pepdat<-pepdat[pepdat$LAT==lat & pepdat$LON==long,]#get lat/long for which we're getting climate
    budburstdoy<-as.integer(mean(pepdat$DAY))              
    daylengthbbdoy <- daylength(lat, budburstdoy)#$Daylength
    chillport <- mean(chillall$Chill_portions)
    chill <- mean(chillall$Utah_Model)/240
    #   
    temps<-c(0,tempforecast)
    #make blank dataframes to fill with estimates without adhoc adjustments for daylength, for all combinations of chilling and forcing at different warming levels
    chill.forecast<-rep(NA, times=8)
    winT.forecast<-rep(NA,times=8)
    
    for (c in 1:length(temps)){#c=chilling
      print(temps[c]);
      if(c==1){chillests<-chillall}
      if (c>1){chillforfilename<-paste(spdir,"/","chillforecast",tempforecast[c-1],"deg_",lat,"_",long,"_1951_2014.csv",sep="")
      chillfor<-read.csv(chillforfilename, header=TRUE)
      chillests<-chillfor}
      dl <- daylengthbbdoy
      if(use.chillports==TRUE){chill.forecast[c]<-mean(chillests$Chill_portions)}
      if(use.chillports==FALSE){chill.forecast[c]<-mean(chillests$Utah_Model)/240}
      winT.forecast[c]<-mean(chillests$mntemp)
      
    }#c
    
    colnames(z.matrix.dl)<-paste("bb.sprtemp",temps, sep=".")
    rownames(z.matrix.dl)<-paste("bb.wintemp",temps, sep=".")
    allforecast<-cbind(temps,sprT.forecast,winT.forecast,chill.forecast,z.matrix.dl)
    colnames(allforecast)[1]<-"warming_C"
    
