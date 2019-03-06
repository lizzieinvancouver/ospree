## Started 27 Feb 2019 ##
## By Ailene  ##

## Marginal effects from Stan models ##

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
#Make a figure showing effects of forcing and chilling on budburst
#at different photoperiods



## Plotting
# First, we estimate the posteriors for each thing we want to plot...
fit <-modelhere

list_of_draws <- extract(fit)
#print(names(list_of_draws))
#str(list_of_draws$mu_a_sp)


getest.bb <- function(fit, forcetemp, chillport, daylength1,daylength2){
  listofdraws <- extract(fit)
  avgbbdl1 <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*forcetemp + 
    listofdraws$mu_b_photo_sp*daylength1 + listofdraws$mu_b_chill_sp*chillport
  avgbbdl2 <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*forcetemp + 
    listofdraws$mu_b_photo_sp*daylength2 + listofdraws$mu_b_chill_sp*chillport
  
   yebbest <- list(avgbbdl1, avgbbdl2)
  return(yebbest)
}

forcetemps<-seq(min(bb.stan$force), max(bb.stan$force), by=1)
chilltemps<-seq(min(as.numeric(bb.stan$chilltemp), na.rm=TRUE),max(as.numeric(bb.stan$chilltemp), na.rm=TRUE), by=1)
chilldays<-as.integer(mean(as.numeric(bb.stan$chilldays), na.rm=TRUE))
temps<-seq(min(c(chilltemps,forcetemps)),max(c(chilltemps,forcetemps)), by=1)
dl1<-8
dl2<-16
chillport<-mean(bb.stan$chill.ports)

#make blank dataframe to fill with estimates
predicts <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
predicts.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
predicts.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))

colnames(predicts)<-colnames(predicts.25per) <-colnames(predicts.75per) <-
  c("forcetemp","dl1","dl2")

for (i in 1:length(temps)){
  bbposteriors <- getest.bb(fit,temps[i], chillport, dl1,dl2)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
  
  predicts[i,]<-c(temps[i],meanz)
  predicts.25per[i,]<-c(temps[i],quant25per)
  predicts.75per[i,]<-c(temps[i],quant75per)
}

#now calculate with chilling effects
#create a vactor of chillportions, using different temperatures
chillests<-as.data.frame(matrix(NA,ncol=10,nrow=length(temps)))
JDay<-seq(1:chilldays)
Year<-2014
for(i in 1:length(temps)){
  Tmean<- temps[i]
  meandaily<-data.frame(JDay,Year,Tmean)
  #convert mean daily temperature data to hourly data
  hrly.temp =
    data.frame(
     Temp = c(rep(meandaily$Tmean, times = 24)),
     Year = c(rep(meandaily$Year, times = 24)),
      JDay = sort(c(rep(seq(1:length(JDay)), times = 24)))
    )
  chillests[i,]<-c(temps[i],chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp)]))
}
colnames(chillests)<- c("temp","Season","End_year","Season_days","Data_days","Perc_complete","Chilling_Hours","Utah_Model","Chill_portions","GDH")  

#make blank dataframe to fill with estimates
chillpredicts <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
chillpredicts.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
chillpredicts.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))

colnames(chillpredicts)<-colnames(chillpredicts.25per) <-colnames(chillpredicts.75per) <-
  c("chilltemp","dl1","dl2")
mnforce<-mean(as.numeric(bb.stan$forcetemp), na.rm=TRUE)
for (i in 1:length(temps)){
  bbposteriors <- getest.bb(fit,mnforce, chillests$Chill_portions[i], dl1,dl2)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
  
  chillpredicts[i,]<-c(temps[i],meanz)
  chillpredicts.25per[i,]<-c(temps[i],quant25per)
  chillpredicts.75per[i,]<-c(temps[i],quant75per)
}

#chilling and forcing simultanesouly altered
bothpredicts <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
bothpredicts.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
bothpredicts.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))

colnames(bothpredicts)<-colnames(bothpredicts.25per) <-colnames(bothpredicts.75per) <-
  c("chilltemp","dl1","dl2")
for (i in 1:length(temps)){
  bbposteriors <- getest.bb(fit,temps[i], chillests$Chill_portions[i], dl1,dl2)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
  
  bothpredicts[i,]<-c(temps[i],meanz)
  bothpredicts.25per[i,]<-c(temps[i],quant25per)
 bothpredicts.75per[i,]<-c(temps[i],quant75per)
}


xlim = c(range(temps))
ylim = c(range(c(predicts[,2:3],chillpredicts[,2:3],bothpredicts[,2:3])))
figname<-paste("mupredicts",min(temps),max(temps),".pdf", sep="_")
pdf(file.path(figpath,figname), width = 9, height = 6)

#quartz()
par(mar=c(8,7,3,5))
plot(predicts$forcetemp,predicts$dl1, xlim=xlim, xlab="Temperature (C)", ylim=ylim,
     ylab="Days to BB", type="l",bty="l", lty=1, lwd=2, col="darkred")
lines(predicts$forcetemp,predicts$dl2,lty=2, lwd=2,col="darkred")
lines(chillpredicts$chilltemp,chillpredicts$dl1,lty=1, lwd=2, col="blue")
lines(chillpredicts$chilltemp,chillpredicts$dl2,lty=2, lwd=2, col="blue")
lines(bothpredicts$chilltemp,bothpredicts$dl1,lty=1, lwd=2, col="purple")
lines(bothpredicts$chilltemp,bothpredicts$dl2,lty=2, lwd=2, col="purple")


legend("topright",legend=c("8 hr-forcing","16 hr-forcing","chilling","both"), lty=c(1,2,1,1), col=c("darkred","darkred","blue","purple"),lwd=2)

#Add shading around line for credible intervals

for(i in 3:5){
  polygon(c(rev(predicts$warming), predicts$warming), c(rev(predicts.75per[,i-1]), predicts.25per[,i-1]), col = alpha(cols[i-2], 0.2), border = NA)
}

for(i in 3:5){
  lines(predicts$warming, predicts[,i-1], 
        col=cols[i-2], lwd=2)}
