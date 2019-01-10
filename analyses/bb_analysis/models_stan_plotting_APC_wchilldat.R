## Started 3 Jan 2019 ##
## By Lizzie (to start) ##

## Marginal effects from Stan models ##
## Based off models_stan_plotting.R ##

## 9 Jan 2019 Ailene modified to include chilling calculations with different amounts of warming, for one particular latitude
## Effects are likely to vary by location becuase of the way that chilling is calculated. 
## Perhaps do something similar across the range of a species?
############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
elif(length(grep("Ignacio", getwd()))>0) { 
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

# Set up colors
if(FALSE){
library(RColorBrewer)
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4
}

# non-z-scored models
if(use.zscore==FALSE){
load("stan/output/m2lni_spcompexprampfp_nonz.Rda") # m2l.ni
load("stan/output/m2lnib_spcompexprampfp_nonz.Rda") # m2l.nib
modelhere <- m2l.ni
}

hist(bb.stan$chill.ports)
rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")

# Let's pretend that average spring temp is 5 and average chill portions is 140
# And, let's pretend 2 deg warming decreases chilling by 30 portions (I made this up)
sprtemp.forplot <- 5
chillport.forplot <- 140
photo.forplot <- 14
warmspring <- 2
warmwinter <- -30

#Instead of the above, let's try with real chilling data
chillall<-read.csv("output/dailyclim/chill_forforecast_48.16447_11.50293_1979_2014.csv", header=TRUE) 
chillplus2<-read.csv("output/dailyclim/chill_forecast2deg_48.16447_11.50293_1979_2014.csv", header=TRUE) 
chillplus4<-read.csv("output/dailyclim/chill_forecast4deg_48.16447_11.50293_1979_2014.csv", header=TRUE) 

# For this site, 1985= a cool winter year and 1995= a warm year
maxchill<-max(chillall$Chill_portions)# chillall[chillall$Chill_portions==max(chillall$Chill_portions),]
minchill<-min(chillall$Chill_portions)#chillall[chillall$Chill_portions==min(chillall$Chill_portions),]# interesting- lower mean temperature in year with lower chilling

chillport.forplot <- mean(chillall$Chill_portions)
photo.forplot <- 14
warmspring <- c(2,4)
warmwinter <- c(mean(chillfut2$Chill_portions)-chillport.forplot,mean(chillfut4$Chill_portions)-chillport.forplot)#actually, this is very close to Lizzie's -30, except that in this case it is associated with a 1 degree INCREASE in winter temp!


# CHEAP (and wrong) way to do this
# RIGHT way would be to create a new posterior!
getBBestimates <- function(modelvalues){
avgBB <- modelvalues[1] + modelvalues[2]*sprtemp.forplot +
    modelvalues[3]*photo.forplot + modelvalues[4]*chillport.forplot
warmsprBB <- modelvalues[1] + modelvalues[2]*(sprtemp.forplot + warmspring) +
    modelvalues[3]*photo.forplot + modelvalues[4]*chillport.forplot
warmwinBB <- modelvalues[1] + modelvalues[2]*sprtemp.forplot +
    modelvalues[3]*photo.forplot + modelvalues[4]*(chillport.forplot+warmwinter)
warmsprwinBB <- modelvalues[1] + modelvalues[2]*(sprtemp.forplot + warmspring) +
    modelvalues[3]*photo.forplot + modelvalues[4]*(chillport.forplot+warmwinter)
yeBBest <- c(avgBB, warmsprBB, warmwinBB, warmsprwinBB)
return(yeBBest)
}

goo <- summary(modelhere)$summary[rownameshere,"mean"]
lowci <- summary(modelhere)$summary[rownameshere,"25%"]
uppci <- summary(modelhere)$summary[rownameshere,"75%"]

lowci.values <- getBBestimates(lowci)
uppci.values <- getBBestimates(uppci)
mean.values <- getBBestimates(goo)

# More cheap stuff to add in photo effects
# Assume 14 hr day, which is Apr 17 in Berlin
mean.values[2]-mean.values[1] # 1.5 days earlier ... no real change
mean.values[3]-mean.values[1] # 3 days earlier ... 0.5 shorter day
mean.values[4]-mean.values[1] # no real change...
mean.values[5]-mean.values[1] # 4 days later...
mean.values[6]-mean.values[1] # 2 days earlier...
mean.values[7]-mean.values[1] # 1 day later...

modelvalues <- goo
photo.avgBB <- modelvalues[1] + modelvalues[2]*sprtemp.forplot +
    modelvalues[3]*photo.forplot + modelvalues[4]*chillport.forplot
photo.warmsprBB <- modelvalues[1] + modelvalues[2]*(sprtemp.forplot + warmspring) +
    modelvalues[3]*photo.forplot + modelvalues[4]*chillport.forplot
photo.warmwinBB <- modelvalues[1] + modelvalues[2]*sprtemp.forplot +
    modelvalues[3]*(photo.forplot+0.5) + modelvalues[4]*(chillport.forplot+warmwinter)
photo.warmsprwinBB <- modelvalues[1] + modelvalues[2]*(sprtemp.forplot + warmspring) +
    modelvalues[3]*(photo.forplot+0.5) + modelvalues[4]*(chillport.forplot+warmwinter)
quartz()
xlim = c(0, 8)
ylim = c(5, 40)
par(mar=c(8,7,3,5))
plot(x=NULL,y=NULL, xlim=xlim, xaxt='n', ylim=ylim,
     ylab="Days to BB", xlab="", main="14 hour photo, lat=48, long=11.5, chillports")
axis(1, at=1:7, labels=c("avg", "spr warming 2C","spr warming 4C", "winter warming 2C","winter warming 4C",
    "both warming 2C", "both warming 4C"), las=2)
  pos.x <- (1:7)
  pos.y <- mean.values
  points(pos.x,pos.y, cex=1.5, pch=19, col="darkblue")
  pos.y.photo <- c(photo.avgBB, photo.warmsprBB, photo.warmwinBB, photo.warmsprwinBB)
  points(pos.x, pos.y.photo, cex=1.5, pch=19, col="purple")
#for(i in 1:4){
 # lines(c(pos.x[i], pos.x[i]), c(lowci.values[i], uppci.values[i]), 
#      col="darkblue")
#}
#add points for 4 degrees of warming

    