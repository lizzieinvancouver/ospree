# Comparing effect sizes for species
# Started by Ailene 
# 2 Jan 2019

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
} else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

library(devtools)
#install_github("dchudz/predcomps")
library(predcomps)
#Load data and model (or run model with models_stan.R)

# dostan = TRUE
# Flags to choose for bbstanleadin.R

use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = TRUE # change to false to use raw predictors

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

m2l.ni = stan('stan/nointer_2level.stan', data = datalist.bb,
              iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))

#bbdf<-data.frame(
#                bb.stan$resp, bb.stan$chill.ports, bb.stan$force, 
#                 bb.stan$photo,bb.stan$complex)
#load("stan/output/m2lni_spcompexprampfp_nonz.Rda")
#nonz
m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
all<-round(cbind(m2lni.sum[grep("a_sp", rownames(m2lni.sum)),1],m2lni.sum[grep("b_force", rownames(m2lni.sum)),1],m2lni.sum[grep("b_photo", rownames(m2lni.sum)),1],m2lni.sum[grep("b_chill", rownames(m2lni.sum)),1]), digits=2)
all<-round(all, digits=2)
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & 
    use.expchillonly ==FALSE & use.chillports == TRUE & use.zscore == FALSE)
    {write.csv(all,"modelnotes/m2lni_spcompexprampfp_nonz_cp.csv")}
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & 
    use.expchillonly ==FALSE & use.chillports == FALSE & use.zscore == FALSE)
{write.csv(all,"modelnotes/m2lni_spcompexprampfp_nonz_ut.csv")}

#z
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & 
    use.expchillonly ==FALSE & use.chillports == TRUE & use.zscore == TRUE)
{write.csv(all,"modelnotes/m2lni_spcompexprampfp_z_cp.csv")}
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & 
    use.expchillonly ==FALSE & use.chillports == FALSE & use.zscore == TRUE)
{write.csv(all,"modelnotes/m2lni_spcompexprampfp_z_ut.csv")}


#GetPredCompsDF(m2l.ni,bbdf)#doesn't work for stan models
#cp<-read.csv("modelnotes/m2lni_spcompexprampfp_z_cp.csv", header=TRUE)
#ut<-read.csv("modelnotes/m2lni_spcompexprampfp_z_ut.csv", header=TRUE)
#cp<-cp[,-1]
#ut<-ut[,-1]
#colnames(cp)<-c("est","mu.cp",	"force.cp",	"photo.cp",	"chill.cp")
#colnames(ut)<-c("est","mu.ut",	"force.ut",	"photo.ut",	"chill.ut")
#all<-cbind(ut,cp[,2:5])
#write.csv(all,"modelnotes/m2lni_spcompexprampfp_z_compare.csv")
#or
#compare species effects across utah and chill portions
chill.compare<-read.csv("modelnotes/m2lni_spcompexprampfp_nonz_compare.csv", header=TRUE)

head(chill.compare)
quartz(height=7,width=7)
#par(mfrow=c(1,4))
plot(chill.compare$mu.ut[3:40],chill.compare$mu.cp[3:40],pch=21,xlab="Intercept, Utah",ylab="Intercept, Chill Port", xlim=c(40,110), ylim=c(40,110))
text(chill.compare$mu.ut[3:40],chill.compare$mu.cp[3:40], labels=as.character(chill.compare$est[3:40]), col="red", pos=c(3,4,4,1), offset=0.6)
points(chill.compare$mu.ut[1],chill.compare$mu.cp[1], pch=21, bg="blue",cex=1.1)
abline(0, 1)

quartz(height=7,width=7)
plot(chill.compare$force.ut[3:40],chill.compare$force.cp[3:40],pch=21,xlab="Force Est, Utah",ylab="Force Est, Chill Port", xlim=c(-3,1), ylim=c(-3,1))
text(chill.compare$force.ut[3:40],chill.compare$force.cp[3:40], labels=as.character(chill.compare$est[3:40]), col="red", pos=c(3,4,4,1), offset=0.6)
points(chill.compare$force.ut[1],chill.compare$force.cp[1], pch=21, bg="blue",cex=1.1)
abline(0, 1)

quartz(height=7,width=7)
plot(chill.compare$photo.ut[3:40],chill.compare$photo.cp[3:40],pch=21,xlab="Photo Est, Utah",ylab="Photo Est, Chill Port", xlim=c(-2,1.5), ylim=c(-2,1.5))
text(chill.compare$photo.ut[3:40],chill.compare$photo.cp[3:40], labels=as.character(chill.compare$est[3:40]), col="red", pos=c(3,4,4,1), offset=0.6)
points(chill.compare$photo.ut[1],chill.compare$photo.cp[1], pch=21, bg="blue",cex=1.1)
abline(0, 1)

quartz(height=7,width=7)
plot(chill.compare$chill.ut[3:40],chill.compare$chill.cp[3:40],pch=21,xlab="Chill Est, Utah",ylab="Chill est, Chill Port", xlim=c(-8,2), ylim=c(-1,0.5))
text(chill.compare$chill.ut[3:40],chill.compare$chill.cp[3:40], labels=as.character(chill.compare$est[3:40]), col="red", pos=c(3,4,4,1), offset=0.6)
points(chill.compare$chill.ut[1],chill.compare$chill.cp[1], pch=21, bg="blue",cex=1.1)

#1:1 line doesn't make sense here...
abline(lm(chill.compare$chill.cp[3:40]~chill.compare$chill.ut[3:40]), lty=2)

#check the relationships between all
cor(chill.compare$photo.cp[3:39],chill.compare$photo.ut[3:39])
cor(chill.compare$chill.cp[3:39],chill.compare$chill.ut[3:39])
cor(chill.compare$force.cp[3:39],chill.compare$force.ut[3:39])
cor(chill.compare$mu.cp[3:39],chill.compare$mu.ut[3:39])
#all r>0.84


#for centered data:
chill.compare<-read.csv("modelnotes/m2lni_spcompexprampfp_z_compare.csv", header=TRUE)

quartz(height=7,width=7)
#par(mfrow=c(1,4))
plot(chill.compare$mu.ut[3:39],chill.compare$mu.cp[3:39],pch=21,xlab="Intercept, Utah",ylab="Intercept, Chill Port", xlim=c(16,50), ylim=c(16,50))
text(chill.compare$mu.ut[3:39],chill.compare$mu.cp[3:39], labels=as.character(chill.compare$est[3:39]), col="red", pos=c(3,4,4,1), offset=0.6)
points(chill.compare$mu.ut[1],chill.compare$mu.cp[1], pch=21, bg="blue",cex=1.1)
abline(0, 1)

quartz(height=7,width=7)
plot(chill.compare$force.ut[3:39],chill.compare$force.cp[3:39],pch=21,xlab="Force Est, Utah",ylab="Force Est, Chill Port", xlim=c(-20,6), ylim=c(-20,6))
text(chill.compare$force.ut[3:39],chill.compare$force.cp[3:39], labels=as.character(chill.compare$est[3:39]), col="red", pos=c(3,4,4,1), offset=0.6)
points(chill.compare$force.ut[1],chill.compare$force.cp[1], pch=21, bg="blue",cex=1.1)
abline(0, 1)

quartz(height=7,width=7)
plot(chill.compare$photo.ut[3:39],chill.compare$photo.cp[3:39],pch=21,xlab="Photo Est, Utah",ylab="Photo Est, Chill Port", xlim=c(-12,6), ylim=c(-12,6))
text(chill.compare$photo.ut[3:39],chill.compare$photo.cp[3:39], labels=as.character(chill.compare$est[3:39]), col="red", pos=c(3,4,4,1), offset=0.6)
points(chill.compare$photo.ut[1],chill.compare$photo.cp[1], pch=21, bg="blue",cex=1.1)
abline(0, 1)

quartz(height=7,width=7)
plot(chill.compare$chill.ut[3:39],chill.compare$chill.cp[3:39],pch=21,xlab="Chill Est, Utah",ylab="Chill est, Chill Port", xlim=c(-29,5), ylim=c(-29,5))
text(chill.compare$chill.ut[3:39],chill.compare$chill.cp[3:39], labels=as.character(chill.compare$est[3:39]), col="red", pos=c(3,4,4,1), offset=0.6)
points(chill.compare$chill.ut[1],chill.compare$chill.cp[1], pch=21, bg="blue",cex=1.1)

#1:1 line doesn't make sense here...
abline(lm(chill.compare$chill.cp[3:39]~chill.compare$chill.ut[3:39]), lty=2)
abline(0, 1)
cor(chill.compare$photo.cp[3:39],chill.compare$photo.ut[3:39])
cor(chill.compare$chill.cp[3:39],chill.compare$chill.ut[3:39])
cor(chill.compare$force.cp[3:39],chill.compare$force.ut[3:39])
cor(chill.compare$mu.cp[3:39],chill.compare$mu.ut[3:39])
#all r>0.82

