## Started 3 Jan 2019 ##
## By Lizzie (to start), Ailene added to it ##

## Marginal effects from Stan models ##
## Based off models_stan_plotting.R ##

############################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#libraries
library(RColorBrewer)
library(geosphere)
library(rstan)
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
use.zscore =FALSE
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
if(use.zscore==FALSE){
load("stan/output/m2lni_spcompexprampfp_nonz.Rda") # m2l.ni
#load("stan/output/m2lnib_spcompexprampfp_nonz.Rda") # m2l.nib
modelhere <- m2l.ni
fit <- m2l.ni
}
if(use.zscore==TRUE){
  load("stan/output/m2lni_spcompexprampfp_z.Rda") # m2l.ni
  fit <- m2l.ni
}
fit.sum <- summary(fit)$summary
quartz()
hist(bb.stan$chill.ports)
rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")
#For main effects of model:
# Select the lat,long and temperature change that you want
lat<-48.16447
long<-11.50293

#The below code only works for not z-scored models:

tempforecast<-c(1,2,3,4,5,6,7)#enter in the amount of warming (in degrees C) you want to forecast 

# Read in cobserved chilling from 1979 to 2014
chillfilename<-paste("../output/dailyclim/chill_observed_",lat,"_",long,"_1979_2014.csv",sep="")
tempfilename<-paste("../output/dailyclim/temp_observed_",lat,"_",long,"_1979_2014.csv",sep="")
chillall<-read.csv(chillfilename, header=TRUE) 
tempall<-read.csv(tempfilename, header=TRUE)
tempall$Tmean[tempall$Month>3 & tempall$Month<7 ]<-"spring"
sprtemp <- mean(tempall$Month[tempall$Month>2 & tempall$Month<6])#March-May (4 degrees C) Should it be April-June instead (12 degrees C)?
budburstdoy<-60#March 1
daylengthbbdoy <- daylength(lat, budburstdoy)#$Daylength
chillport <- mean(chillall$Chill_portions)

## Plotting
# First, we estimate the posteriors for each thing we want to plot...
fit <-modelhere

list_of_draws <- extract(fit)
#print(names(list_of_draws))
#str(list_of_draws$mu_a_sp)


getest.bb <- function(fit, sprtemp, daylength, chillport, warmspring, warmwinter,
    daylengthwarmspr, daylengthwarmwin, daylengthwarmsprwin){
    listofdraws <- extract(fit)
    avgbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*sprtemp + 
        listofdraws$mu_b_photo_sp*daylength + listofdraws$mu_b_chill_sp*chillport
    warmsprbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*(sprtemp+warmspring) + 
        listofdraws$mu_b_photo_sp*(daylength + daylengthwarmspr) + listofdraws$mu_b_chill_sp*chillport
    warmwinbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*sprtemp + 
        listofdraws$mu_b_photo_sp*(daylength + daylengthwarmwin) + listofdraws$mu_b_chill_sp*(chillport+warmwinter)
    warmsprwinbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*(sprtemp+warmspring) +
        listofdraws$mu_b_photo_sp*(daylength + daylengthwarmsprwin) + listofdraws$mu_b_chill_sp*(chillport+warmwinter)
    yebbest <- list(avgbb, warmsprbb, warmwinbb, warmsprwinbb)
    return(yebbest)
}

# NOTE: I believe quantile is fine for extracting probabilities, see https://discourse.mc-stan.org/t/reporting-credible-confidence-intervals/2262

#make blank dataframe to fill with estimates
predicts <- as.data.frame(matrix(NA,ncol=5,nrow=7))

predicts.25per <- as.data.frame(matrix(NA,ncol=5,nrow=7))
predicts.75per <- as.data.frame(matrix(NA,ncol=5,nrow=7))
colnames(predicts)<-colnames(predicts.25per) <-colnames(predicts.75per) <-
  c("warming","nowarm","sprwarm","winwarm","bothwarm")

for (i in 1:length(tempforecast)){
  chillforfilename<-paste("../output/dailyclim/chill_forecast",tempforecast[i],"deg_",lat,"_",long,"_1979_2014.csv",sep="")  
  chillfor<-read.csv(chillforfilename, header=TRUE) 
  photo.forplot <- daylengthbbdoy
  warmspring <-tempforecast[i]
  warmwinter <- mean(chillfor$Chill_portions)-chillport
  bbposteriors <- getest.bb(fit, sprtemp, daylengthbbdoy, chillport, warmspring, warmwinter, 0, 0, 0)
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
  
  bbposteriors.wdaylength <- getest.bb(modelhere, sprtemp, daylengthbbdoy, chillport, warmspring, warmwinter, daylengthchange.springwarm, daylengthchange.wintwarm, daylengthchange.bothwarm)
  meanz.wdaylength <- unlist(lapply(bbposteriors.wdaylength, mean))
  quant25per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.25))))
  quant75per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.75))))
  predicts[i,]<-c(warmspring,meanz.wdaylength)
  predicts.25per[i,]<-c(warmspring,quant25per.wdaylength)
  predicts.75per[i,]<-c(warmspring,quant75per.wdaylength)
  print(tempforecast[i]);print(warmwinter);
}
predicts<-rbind(c(0,predicts$nowarm[1:4]),predicts)
predicts<-predicts[,-2]
predicts.25per<-rbind(c(0,predicts.25per$nowarm[1:4]),predicts.25per)
predicts.25per<-predicts.25per[,-2]
predicts.75per<-rbind(c(0,predicts.75per$nowarm[1:4]),predicts.75per)
predicts.75per<-predicts.75per[,-2]

xlim = c(0, 7)
ylim = c(15, 30)
figname<-paste("tempforecast",lat,long,min(tempforecast),max(tempforecast),"degwarm.pdf", sep="_")
pdf(file.path(figpath,figname), width = 9, height = 6)

#quartz()
par(mar=c(8,7,3,5))
plot(x=NULL,y=NULL, xlim=xlim, xlab="Amount of warming (C)", ylim=ylim,
     ylab="Days to BB", main=paste(round(daylengthbbdoy, digits=0)," hours", sep=""), bty="l")
  pos.x <- 0
  pos.y <- predicts[1,2]
  points(pos.x, pos.y, cex=1.2, pch=19, bg="gray")
  #Add shading around line for credible intervals
  
  for(i in 3:5){
  polygon(c(rev(predicts$warming), predicts$warming), c(rev(predicts.75per[,i-1]), predicts.25per[,i-1]), col = alpha(cols[i-2], 0.2), border = NA)
  }
  
  for(i in 3:5){
    lines(predicts$warming, predicts[,i-1], 
          col=cols[i-2], lwd=2)}
  
  # intervals
  # for(i in 3:5){
  #   lines(predicts.25per$warming, predicts.25per[,i-1], 
  #         col=cols[i-2], lwd=1, lty=2)
  # }
  # for(i in 3:5){
  #   lines(predicts.75per$warming, predicts.75per[,i-1], 
  #         col=cols[i-2], lwd=1, lty=2)
  # }
legend(0,18,legend=c("Spring warming","Winter warming","Both"),lty=1,lwd=2,col=cols,bty="n", cex=0.9)
dev.off()

##Make the above as a 3D plot
 #need to create a matrix that fills x columns=sprwarming, y=columns = winter warming and budburst day is values!
 #x= amount of winter warming=rows of z. matrix
 #y= amount of spring warming= columns of z. matrix
 #z= budburst days for each combination
#reset lat/long if you like:
lat=48.8667#46.8167#
long=15.1333#12.8
# Read in cobserved chilling from 1979 to 2014
chillfilename<-paste("../output/dailyclim/betpen/chill_observed_",lat,"_",long,"_1951_2014.csv",sep="")
tempfilename<-paste("../output/dailyclim/betpen/temp_forforecast__",lat,"_",long,"_1951_2014.csv",sep="")
chillall<-read.csv(chillfilename, header=TRUE) 
tempall<-read.csv(tempfilename, header=TRUE)
tempall$Tmean[tempall$Month>3 & tempall$Month<7 ]<-"spring"
sprtemp <- mean(tempall$Month[tempall$Month>2 & tempall$Month<6])#March-May (4 degrees C) Should it be April-June instead (12 degrees C)?
budburstdoy<-60#March 1
daylengthbbdoy <- daylength(lat, budburstdoy)#$Daylength
chillport <- mean(chillall$Chill_portions)




 z.matrix <- matrix(NA,ncol=length(tempforecast)+1,nrow=length(tempforecast)+1)
 temps=c(0,tempforecast)
 #Fill matrix row by row
 for (i in 1:length(temps)){#i=winter warming
   if(temps[i]>0){chillforfilename<-paste("../output/dailyclim/betpen/chillforecast",temps[i],"deg_",lat,"_",long,"_1951_2014.csv",sep="")
   chillfor<-read.csv(chillforfilename, header=TRUE) 
   warmwinter <- mean(chillfor$Chill_portions)-chillport}
   print(temps[i]);print(warmwinter)
   if(temps[i]==0){warmwinter <- 0}
   photo.forplot <- daylengthbbdoy
   for(j in 1:length(temps)){
    warmspring <-temps[j]
    bbposteriors <- getest.bb(fit, sprtemp, daylengthbbdoy, chillport, warmspring, warmwinter, 0, 0, 0)
    meanz <- unlist(lapply(bbposteriors, mean))#returns  avgbb, warmsprbb, warmwinbb, warmsprwinbb)
    z.matrix[i,j]<-meanz[4]
  }
 }
 
 z=z.matrix
 x=temps
 y=temps
 zlim <- range(y)
 zlen <- zlim[2] - zlim[1] + 1
 
 colorlut <- terrain.colors(zlen) # height color lookup table
 
 col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point

 plot3d(z.matrix, type = 'n', 
        xlim = range(x), ylim = range(y), zlim = range(z), 
        xlab = 'Winter warming', 
        ylab = 'Spring warming', zlab = 'Days to BB') 
 
surface3d(x,y, z,
          col=col, back = "lines")



##NOT FORECASTING, BUT COMPARING Z AND NONZ MODELS
###Now, instead of forecasting, use temperatures within the range of observations, using z-scored values:
#choose your desired credible intervals:
qlo=0.05
qhi=0.95

#use range of temperatures present in the z-scored data to look at effect of increasing chilling

if(use.zscore==TRUE){
temprange<-seq(from=min(bb.stan$force.z), to =max(bb.stan$force.z), by=abs(max(bb.stan$force.z)-min(bb.stan$force.z))/10)
chillrange<-seq(from=min(bb.stan$chill.ports.z), to =max(bb.stan$chill.ports.z), by=abs(max(bb.stan$chill.ports.z)-min(bb.stan$chill.ports.z))/10)
photorange<-seq(from=min(bb.stan$photo.z), to =max(bb.stan$photo.z), by=abs(max(bb.stan$photo.z)-min(bb.stan$photo.z))/10)
mntemp <- mean(bb.stan$force.z)
mndaylength <- mean(bb.stan$photo.z)
mnchillport <- mean(bb.stan$chill.ports.z)
}
if(use.zscore==FALSE){
  temprange<-seq(from=min(bb.stan$force), to =max(bb.stan$force), by=abs(max(bb.stan$force)-min(bb.stan$force))/10)
  chillrange<-seq(from=min(bb.stan$chill.ports), to =max(bb.stan$chill.ports), by=abs(max(bb.stan$chill.ports)-min(bb.stan$chill.ports))/10)
  photorange<-seq(from=min(bb.stan$photo), to =max(bb.stan$photo), by=abs(max(bb.stan$photo)-min(bb.stan$photo))/10)
  mntemp <- mean(bb.stan$force)
  mndaylength <- mean(bb.stan$photo)
  mnchillport <- mean(bb.stan$chill.ports)
}

## Plotting. Goal  is mostly to compare z-scored and nonz models
# First, we estimate the posteriors for each thing we want to plot...
list_of_draws <- extract(fit)
print(names(list_of_draws))

est.bb <- function(fit, temp, daylength, chillport){
    listofdraws <- extract(fit)
    avgbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*temp + 
    listofdraws$mu_b_photo_sp*daylength + listofdraws$mu_b_chill_sp*chillport
    yebbest <- list(avgbb)
    return(yebbest)
}


#make blank dataframe to fill with estimates
predict.temp <- as.data.frame(matrix(NA,ncol=4,nrow=11))
colnames(predict.temp)<-c("temp","daystobb","daystobb.05","daystobb.95")
predict.chill <- as.data.frame(matrix(NA,ncol=4,nrow=11))
colnames(predict.chill)<-c("chill","daystobb","daystobb.05","daystobb.95")
predict.photo <- as.data.frame(matrix(NA,ncol=4,nrow=11))
colnames(predict.photo)<-c("photo","daystobb","daystobb.05","daystobb.95")
#forcing
for (i in 1:length(temprange)){
  bbposteriors <- est.bb(fit, temprange[i], mndaylength, mnchillport)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(qlo, 0.5, qhi)))
  quantlo <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(qlo))))
  quanthi <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(qhi))))
  predict.temp[i,]<-c(temprange[i],meanz,quantlo,quanthi)
}
#chilling
for (i in 1:length(chillrange)){
  bbposteriors <- est.bb(fit, mntemp, mndaylength, chillrange[i])
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(qlo, 0.5, qhi)))
  quantlo <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(qlo))))
  quanthi <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(qhi))))
  predict.chill[i,]<-c(chillrange[i],meanz,quantlo,quanthi)
}
#daylength
for (i in 1:length(photorange)){
  bbposteriors <- est.bb(fit, mntemp, photorange[i], mnchillport)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(qlo, 0.5, qhi)))
  quantlo <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(qlo))))
  quanthi <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(qhi))))
  predict.photo[i,]<-c(photorange[i],meanz,quantlo,quanthi)
}

if(use.zscore==TRUE){
ylim = c(-5, 60)
figname<-"plot_apc_z.pdf"
pdf(file.path(figpath,figname), width = 9, height = 6)

#quartz()
par(mar=c(4,4,1,3), mfrow=c(3,1))
plot(predict.temp$temp,predict.temp$daystobb, xlab="Forcing temperature (C)",
     ylab="Days to BB", xaxt="n",bty="l", pch=21, bg="gray", ylim=ylim)
for (p in 1:length(predict.temp$daystobb)){
  arrows( predict.temp$temp[p],predict.temp$daystobb.05[p],predict.temp$temp[p],predict.temp$daystobb.95[p], 
          length=0.05, angle=90, code=3)
}
points(predict.temp$temp,predict.temp$daystobb, pch=21, bg="gray")
at=c(-2, -1, 0, 1, 2, 3)
labels=at*sd(bb.stan$force)+mean(bb.stan$force)
axis(side=1, at=at, labels=round(labels, 1))

plot(predict.chill$chill,predict.chill$daystobb, xlab="Chilling (chill portions)",
     ylab="Days to BB", xaxt="n",bty="l", pch=21, bg="gray", ylim=ylim)
for (p in 1:length(predict.chill$daystobb)){
  arrows( predict.chill$chill[p],predict.chill$daystobb.05[p],predict.chill$chill[p],predict.chill$daystobb.95[p], 
          length=0.05, angle=90, code=3)
}
points(predict.chill$chill,predict.chill$daystobb, pch=21, bg="gray")
at=c(-2, -1, 0, 1, 2,3)
labels=at*sd(bb.stan$chill)+mean(bb.stan$chill)
axis(side=1, at=at, labels=round(labels, 1))

plot(predict.photo$photo,predict.photo$daystobb, xlab="Daylength (hours)",
     ylab="Days to BB", xaxt="n",bty="l", pch=21, bg="gray", ylim=ylim)
for (p in 1:length(predict.chill$daystobb)){
  arrows( predict.photo$photo[p],predict.photo$daystobb.05[p],predict.photo$photo[p],predict.photo$daystobb.95[p], 
          length=0.05, angle=90, code=3)
}
points(predict.photo$photo,predict.photo$daystobb, pch=21, bg="gray")
at=c(-2, -1, 0, 1, 2,3)
labels=at*sd(bb.stan$photo)+mean(bb.stan$photo)
axis(side=1, at=at, labels=round(labels, 1))

dev.off()
}


if(use.zscore==FALSE){
  ylim = c(-5, 60)
  figname<-"plot_apc_nonz.pdf"
  pdf(file.path(figpath,figname), width = 9, height = 6)
  
  #quartz()
  par(mar=c(4,4,1,3), mfrow=c(3,1))
  plot(predict.temp$temp,predict.temp$daystobb, xlab="Forcing temperature (C)",
       ylab="Days to BB",bty="l", pch=21, bg="gray", ylim=ylim)
  for (p in 1:length(predict.temp$daystobb)){
    arrows( predict.temp$temp[p],predict.temp$daystobb.05[p],predict.temp$temp[p],predict.temp$daystobb.95[p], 
            length=0.05, angle=90, code=3)
  }
  points(predict.temp$temp,predict.temp$daystobb, pch=21, bg="gray")
  
  plot(predict.chill$chill,predict.chill$daystobb, xlab="Chilling (chill portions)",
       ylab="Days to BB", bty="l", pch=21, bg="gray", ylim=ylim)
  for (p in 1:length(predict.chill$daystobb)){
    arrows( predict.chill$chill[p],predict.chill$daystobb.05[p],predict.chill$chill[p],predict.chill$daystobb.95[p], 
            length=0.05, angle=90, code=3)
  }
  points(predict.chill$chill,predict.chill$daystobb, pch=21, bg="gray")
  
  plot(predict.photo$photo,predict.photo$daystobb, xlab="Daylength (hours)",
       ylab="Days to BB", bty="l", pch=21, bg="gray", ylim=ylim)
  for (p in 1:length(predict.chill$daystobb)){
    arrows( predict.photo$photo[p],predict.photo$daystobb.05[p],predict.photo$photo[p],predict.photo$daystobb.95[p], 
            length=0.05, angle=90, code=3)
  }
  points(predict.photo$photo,predict.photo$daystobb, pch=21, bg="gray")
  
  dev.off()
}
#Now instead of plotting it as separate panels, plot things together, as we did in the forecasting
#only for nonz for now

