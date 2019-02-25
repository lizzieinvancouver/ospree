options(stringsAsFactors = FALSE)
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/photoperiod") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses/photoperiod") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/photoperiod")
} else 
  setwd("~/Documents/git/ospree/analyses/photoperiod")
library(geosphere)
library(xtable)
library(plyr)
library(dplyr)
#library(tidyr)

require(dplyr)
require(xtable)

ospree<-read.csv("../../analyses/output/ospree_clean.csv",header=T)#which version should I use?
source("shifts_table.R")
#Now try to make a figure summarizing this table
cols=c("darkblue","salmon4")
shapes=c(21,24)
quartz(height=5, width=10)
par(mai=c(.3,1,.1,.5), oma=c(4,.5,.5,.5), mfrow=c(1,2))
plot(as.numeric(photop_all$delta),as.numeric(photop_all$space), bg="darkblue",pch=shapes[as.numeric(as.factor(photop_all$continent))],bty="l", xlab="", ylab="Equivalent shift in space (degrees)", xlim=c(0,12))
abline(h=1.5, lwd=2, col="gray", lty=2)#Chen shift rate

abline(lm(as.numeric(photop_all$space)~as.numeric(photop_all$delta)), lwd=2, col="darkblue")
mtext("B)", side=3, adj=-.3)
mtext("Experimental change in daylength (hrs)", side=1, line=2)

#par(new=TRUE)
plot(as.numeric(photop_all$delta), as.numeric(photop_all$time),bg="salmon4",pch=shapes[as.numeric(as.factor(photop_all$continent))],bty="l", xlab="Experimental change in daylength (hrs)",ylab="Equivalent shift in time (days)",xlim=c(0,12))
abline(h=-23, lwd=2, col="gray", lty=2)#parmesan shift rate
abline(lm(as.numeric(photop_all$time)~as.numeric(photop_all$delta)), lwd=2, col="salmon4")
mtext("Experimental change in daylength (hrs)", side=1, line=2)
mtext("C)", side=3, adj=-.3)

#axis(4)
#mtext("Shift in time (days)",side=4,line=3)
legend(8,-60,pch=c(24,21),pt.bg=c("salmon4","salmon4")
       ,legend=c("North America","Europe"), cex=0.9, bty="n")
legend(7,-75,lty=2,lwd=2,col="gray",
       legend=c("Observed shifts"), cex=0.9, bty="n")

#Flowering phenology has shifted earlier: 
#2.5–5 days per degC (
#  4.5 days per degC (Wolkovich et al 2012)
  
 # Leaf phenology has shifted earlier:
  #  6.4 days per degC (Wolkovich et al 2012)
  #All phenology together:
 #   2.3 days per decade mean advancement of spring events (Parmesan & Yohe 2003)
  #this is equivalent to 23 days over 100 years
#  Species ranges have shifted shifted:
 #   In elevation at a median rate of 11.0 meters per decade (Chen et al 2011) or 6.1 metres per decade upward (Parmesan & Yohe 2003) 
  
#  To higher latitudes at a median rate of 16.9 kilometers per decade (Chen et al 2011) or 6.1 km per decade (Parmesan & Yohe 2003)
#This is equivalent to 169 kilometers or 1.5 degrees (111 km/degree)  
