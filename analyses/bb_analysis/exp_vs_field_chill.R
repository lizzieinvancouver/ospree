#Plot the accumulated chilling for 
#experimental treatments (with constant temps) versus 
#measured temperature data with daily variation in temperature
#Goal is to understand how variation affects chilling accumulation and 
#if exp vs field chilling differs systematically
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#load libraries
library(chillR)
#library(autoimage)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

#read in file with different lat/longs from PEP and chiling estimates
spests<-read.csv("..//output/betpen_for3dplot/betpen.forecast.forheatmap.csv", header=TRUE)

#
## set up the flags
use.chillports = FALSE
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
##Range of ospree chilling is -5.239583 19.683333

##range of ospree temperatures is:
range(as.numeric(bb.stan.expramptypes$chilltemp), na.rm=TRUE) #-10-16
#why don't we use lower exp chilling temps in the 3d figure? we should!
#range of pep 
range(spests$chill.forecast[spests$warming_C==0])
#1.153542 12.953542
##range(spests$winT.forecast)
range(spests$winT.forecast[spests$warming_C==0])
#-6.311229  6.180634
dim(spests)#
range(spests$lat)#46.7167-54.8000
#
#quartz()
pdf("figures/forecasting/fagsyl_3lats.pdf", width=11,height=5)

plot(as.numeric(bb.stan.expramptypes$chilltemp), as.numeric(bb.stan.expramptypes$chill), pch=21, bg="gray", xlab="Chilling temperature (°C)", ylab= "Total Chilling (Utah units)", ylim=c(-6,25), bty = "l")
points(spests$winT.forecast,spests$chill.forecast, pch=21,bg="darkgreen")

#create a chilling dataset with constant daily temperatures from mean temperatures in allforecasts
chilltemps<-unique(as.numeric(spests$winT.forecast[spests$warming_C==0]))
days<-30+31+30+31+31+28+31#days in Sept-Mar
year<-2018#i don't think this matters?
doys<-seq(1:days)
chillcalc<-c()

for (i in 1:length(chilltemps)){
  hrly.temp =
    data.frame(
      Temp = c(rep(chilltemps[i], times = 24 * as.numeric(days))),
      Year = rep(year, times = 24 * as.numeric(days)),
      JDay = seq(1:days) )
 
   chillcalc<- rbind(chillcalc,chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp)])) 
}
#put all the things together
chillests<-as.data.frame(cbind(chilltemps,chillcalc$Chill_portions,chillcalc$Utah_Model/240))
colnames(chillests)<-c("temp","chillport","utah.240")
points(chillests$temp,chillests$utah.240, pch=21,bg="lightblue")

legend(x=-10,y=25, legend=c("OSPREE","Constant duration","Field","Field +1°C warming","Field +7°C warming"), pch=21,pt.bg=c("gray","lightblue","dark green","yellowgreen","darkred"), bty = "n")
points(spests$winT.forecast[spests$warming_C==1],spests$chill.forecast[spests$warming_C==1], pch=21,bg="yellowgreen")
points(spests$winT.forecast[spests$warming_C==2],spests$chill.forecast[spests$warming_C==2], pch=21,bg="yellow")
points(spests$winT.forecast[spests$warming_C==3],spests$chill.forecast[spests$warming_C==3], pch=21,bg="goldenrod")
points(spests$winT.forecast[spests$warming_C==4],spests$chill.forecast[spests$warming_C==4], pch=21,bg="orange")
points(spests$winT.forecast[spests$warming_C==5],spests$chill.forecast[spests$warming_C==5], pch=21,bg="darkorange")
points(spests$winT.forecast[spests$warming_C==6],spests$chill.forecast[spests$warming_C==6], pch=21,bg="red")
points(spests$winT.forecast[spests$warming_C==7],spests$chill.forecast[spests$warming_C==7], pch=21,bg="darkred")
dev.off()

#are chilling and forcing correlated?
pdf("figures/cor_force_chill_nature.pdf", width=11,height=5)
plot(spests$sprT[spests$warming_C==0], spests$chill.forecast[spests$warming_C==0],type="p", pch=21, bg="gray",xlab= "Spring Temperature (°C)", ylab="Chilling (Utah units")
abline(lm(spests$sprT[spests$warming_C==0]~spests$chill.forecast[spests$warming_C==0]))
dev.off()
cor.test(spests$chill.forecast[spests$warming_C==0],spests$sprT[spests$warming_C==0])
#0.7471090 0.7767555
#sample estimates:
# cor 
#0.7623319 
#range of forcetemps
range(as.numeric(bb.stan.expramptypes$forcetemp), na.rm=TRUE) #5-32


