options(stringsAsFactors = FALSE)
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/photoperiod") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses/photoperiod") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/photoperiod")
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
cols=c("darkred","yellow3")
shapes=c(21,24)
quartz()
par(mai=c(1.5,1,1,1))
plot(as.numeric(photop_all$delta),as.numeric(photop_all$space), bg="darkred",pch=shapes[as.numeric(as.factor(photop_all$continent))],bty="u", xlab="Experimental change in daylength (hrs)", ylab="Shift in space (degrees)", xlim=c(0,12))
abline(lm(as.numeric(photop_all$space)~as.numeric(photop_all$delta)), lwd=2, col="darkred")

par(new=TRUE)
plot(as.numeric(photop_all$delta), as.numeric(photop_all$time),bg="goldenrod",pch=shapes[as.numeric(as.factor(photop_all$continent))],bty="u",xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,12))
abline(lm(as.numeric(photop_all$time)~as.numeric(photop_all$delta)), lwd=2, col="goldenrod")

axis(4)
mtext("Shift in time (days)",side=4,line=3)
legend(8,-20,pch=c(24, 24,21, 21),pt.bg=c("darkred","goldenrod","darkred"
                ,"goldenrod"),legend=c("Space, N. America","Time, N. America",
                                     "Space, Europe","Time, Europe"))
     