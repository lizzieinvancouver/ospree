## Started 3 Jan 2019 ##
## By Lizzie (to start) ##

## Marginal effects from Stan models ##
## Based off models_stan_plotting.R ##

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
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
else if(length(grep("Ignacio", getwd()))>0) { 
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
}

hist(bb.stan$chill.ports)
rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")

# Select the lat,long and temperature change that you want
lat<-48.16447
long<-11.50293
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
print(names(list_of_draws))
str(list_of_draws$mu_a_sp)


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
  
  bbposteriors.wdaylength <- getest.bb(m2l.ni, sprtemp, daylengthbbdoy, chillport, warmspring, warmwinter, daylengthchange.springwarm, daylengthchange.wintwarm, daylengthchange.bothwarm)
  meanz.wdaylength <- unlist(lapply(bbposteriors.wdaylength, mean))
  quant25per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.25))))
  quant75per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.75))))
  predicts[i,]<-c(warmspring,meanz.wdaylength)
  predicts.25per[i,]<-c(warmspring,quant25per.wdaylength)
  predicts.75per[i,]<-c(warmspring,quant75per.wdaylength)
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

#pdf(paste(figpath,"/tempforecast",min(tempforecast),"-",max(tempforecast),"_deg_",lat,"_",long,".pdf",sep=""))
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



