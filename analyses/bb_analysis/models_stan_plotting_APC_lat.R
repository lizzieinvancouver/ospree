## Started 3 Jan 2019 ##
## By Ailene ##

## Marginal effects from Stan models ##
## Based off models_stan_plotting_APC.R ##

############################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#libraries
library(RColorBrewer)
library(geosphere)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


figpath <- "figures"

## set up the flags
use.chillports = TRUE
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



bb.wlat <- bb.stan
bb.wlat <- within(bb.wlat, { prov.lat <- ave(provenance.lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
bb.wlat <- subset(bb.wlat, bb.wlat$prov.lat>1)  

#lat.stan<-bb.wlat.spp
lat.stan<-bb.wlat
#write.csv(lat.stan, "~/Documents/git/ospree/analyses/lat_analysis/lat_output/lat_arm.csv", row.names = FALSE)
#lat.stan<-subset(bb.wlat.spp, bb.wlat.spp$resp<600)
lat.stan<-subset(lat.stan, lat.stan$resp<600)

lat.stan$lat <- lat.stan$provenance.lat

lat.stan$complex<-as.numeric(as.factor(lat.stan$complex.wname))

lat.stan<-na.omit(lat.stan)

# Set up colors (more than used currently ...

cols <- adjustcolor(c("maroon4", "lightskyblue","purple4"), alpha.f = 0.8) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

# non-z-scored models
if(use.zscore==FALSE){
load("../lat_analysis/stan/m2l.inter.lat.chillport.nonz.Rda") # m2l.ni
modelhere <- m2l.inter
}
fit <-modelhere

fit.sum <- summary(fit)$summary

#rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")

#species to plot
sp<-c("betpen","fagsyl")
sp.num<-c(9,15)

# Select the species and temperature change that you want
tempforecast<-c(1,2,3,4,5,6,7)#enter in the amount of warming (in degrees C) you want to forecast 

#Define the function we will use to estimate budburst
getspest.bb <- function(fit, sprtemp, daylength, chillport, warmspring, warmwinter,
                        daylengthwarmspr, daylengthwarmwin, daylengthwarmsprwin){
  listofdraws <- extract(fit)
  avgbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*sprtemp + 
    listofdraws$b_photo[,sp.num[s]]*daylength + listofdraws$b_chill[,sp.num[s]]*chillport
  
  warmsprbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*(sprtemp+warmspring) + 
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmspr) + listofdraws$b_chill[,sp.num[s]]*chillport
  
  warmwinbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*sprtemp + 
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmwin) + listofdraws$b_chill[,sp.num[s]]*(chillport+warmwinter)
  
  warmsprwinbb <- listofdraws$a_sp[,sp.num[s]] + listofdraws$b_force[,sp.num[s]]*(sprtemp+warmspring) +
    listofdraws$b_photo[,sp.num[s]]*(daylength + daylengthwarmsprwin) + listofdraws$b_chill[,sp.num[s]]*(chillport+warmwinter)
  
  yebbest <- list(avgbb, warmsprbb, warmwinbb, warmsprwinbb)
  return(yebbest)
}

#Choose whether or not you want to use our adhoc shift in daylength.
use.daylengthshift=FALSE
#quartz(width=9,height=5)
#par(mar=c(8,4,3,4), mfrow=c(1,2))

for(s in 1:length(sp)){
  #50 sites were chosen within the range of each species
  numsites<-length(list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__"))
  tempfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__")
  chillfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="chill_observed_")
  spdir<-paste("../output/dailyclim/",sp[s],sep="")
  
  #sites.toplot<-c(1,50)#just plot 2 sites for now- min lat and max lat
  sites.toplot<-21#plot a single site, both species
  quartz(width=9,height=5)
  par(mar=c(8,4,3,4), mfrow=c(1,2))
  
  for (i in sites.toplot){
    #for now do with i=1 and i=50 for each species (min and max)
    # Read in observed chilling from 1950 to 2014
    chillall<-read.csv(paste(spdir,"/",chillfiles[i],sep=""), header=TRUE) 
    tempall<-read.csv(paste(spdir,"/",tempfiles[i],sep=""), header=TRUE)
    #because we want a "pre-warming estimate" only use years before 1980 for temeperature (to match bb)
    tempall<-tempall[tempall$Year<1980,]
    chillall<-chillall[chillall$End_year<1980,]
    
    tempall$Tmean[tempall$Month>3 & tempall$Month<7 ]<-"spring"
    sprtemp <- mean(tempall$Month[tempall$Month>2 & tempall$Month<6])#March-May (4 degrees C) Should it be April-June instead (12 degrees C)?
    #extract the lat/long from the file name...argh!
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
      warmwinter <- mean(chillfor$Chill_portions)-chillport
      print(tempforecast[j]);print(warmwinter)
      bbposteriors <- getspest.bb(fit, sprtemp, daylengthbbdoy, chillport, warmspring, warmwinter, 0, 0, 0)
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
      bbposteriors.wdaylength <- getspest.bb(fit, sprtemp, daylengthbbdoy, chillport, warmspring, warmwinter, daylengthchange.springwarm, daylengthchange.wintwarm, daylengthchange.bothwarm)
      meanz.wdaylength <- unlist(lapply(bbposteriors.wdaylength, mean))
      quant25per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.25))))
      quant75per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.75))))
      predicts[j,]<-c(warmspring,meanz)
      predicts.25per[j,]<-c(warmspring,quant25per)
      predicts.75per[j,]<-c(warmspring,quant75per)
      predicts.wdl[j,]<-c(warmspring,meanz.wdaylength)
      predicts.25per.wdl[j,]<-c(warmspring,quant25per.wdaylength)
      predicts.75per.wdl[j,]<-c(warmspring,quant75per.wdaylength)
    }
    predicts<-rbind(c(0,predicts$nowarm[1:4]),predicts)
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
    if(use.daylengthshift==TRUE){
      predicts<-predicts.wdl
      predicts.25per<-predicts.25per.wdl
      predicts.75per<-predicts.75per.wdl
    }
    
    ymin = min(predicts[,-1],predicts.25per[,-1],predicts.75per[,-1])
    ymax = max(predicts[,-1],predicts.25per[,-1],predicts.75per[,-1])
    xlim = c(0, 7)
    ylim = c(ymin,ymax)
    #figname<-paste("tempforecast",lat,long,min(tempforecast),max(tempforecast),"degwarm.pdf", sep="_")
    #pdf(file.path(figpath,figname), width = 9, height = 6)
    
    #pdf(paste(figpath,"/tempforecast",min(tempforecast),"-",max(tempforecast),"_deg_",lat,"_",long,".pdf",sep=""))
    #quartz()
    #par(mar=c(8,7,3,5), mfrow=c(1,2))
    plot(x=NULL,y=NULL, xlim=xlim, xlab="Amount of warming (C)", ylim=ylim,
         ylab="Days to BB", main=paste(sp[s],", lat=",round(lat,digits=2),", doy=",budburstdoy,", ",round(daylengthbbdoy, digits=0)," hrs", sep=""), bty="l")
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
    if(i==1){
      mtext(paste("a_sp[",sp.num[s],"]=",round(sp.int, digits=2), sep=""), side=1, line=-5, adj=0)
      mtext(paste("b_force[",sp.num[s],"]=",round(sp.fo, digits=2), sep=""), side=1, line=-4, adj=0)
      mtext(paste("b_chill[",sp.num[s],"]=",round(sp.ch, digits=2), sep=""), side=1, line=-3, adj=0)
      mtext(paste("b_photo[",sp.num[s],"]=",round(sp.ph, digits=2), sep=""), side=1, line=-2, adj=0)
    }
    if(i==50)
    {
      legend("bottomleft",legend=c("Spring warming","Winter warming","Both","with daylength shifts"),lty=c(1,1,1,2),lwd=2,col=cols,bty="n", cex=0.9)
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
    #dev.off()
  }
  
}
