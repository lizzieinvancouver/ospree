#Ploting slopes versus intercepts on centered model.
#Started by Ailene
#April 2019
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

## set up the flags
use.chillports = TRUE
use.zscore = TRUE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE
#read in model and data, as prepped for model
source("source/bbstanleadin.R")
# Load fitted stan model: no interactions
if(use.zscore==TRUE & use.cropspp==FALSE & use.chillports == TRUE){
  load("stan/output/m2lni_spcompexprampfpcp_z.Rda") #
  modelhere <- m2l.ni
  fit <- m2l.ni
} 
fit.sumz <- summary(fit)$summary
quartz()
hist(bb.stan$chill.ports)
hist(bb.stan$chill)#=utah

#First, plot only mean of posterior estimates for each species int and slope
#species intercepts
sp.ints<-summary(fit)$summary[10:46,1]
sp.f<-summary(fit)$summary[47:83 ,1]
sp.p<-summary(fit)$summary[84:120 ,1]
sp.c<-summary(fit)$summary[121:157 ,1]

int<-summary(fit)$summary[1,1]
f<-summary(fit)$summary[2,1]
c<-summary(fit)$summary[4,1]
p<-summary(fit)$summary[3,1]
spp <- sort(unique(bb.stan$complex))
n <- length(spp)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colv = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
cols<-colv[1:length(spp)]
quartz()
par(mfrow=(c(1,3)))
plot(sp.ints,sp.f,xlab="intercept",ylab="force.est",typ="p", pch=21, bg=cols)
points(int,f, pch=8,cex=2)
plot(sp.ints,sp.c,xlab="intercept",ylab="chille.est",typ="p", pch=21, bg=cols)
points(int,c, pch=8,cex=2)

plot(sp.ints,sp.p,xlab="intercept",ylab="photo.est",typ="p", pch=21, bg=cols)
points(int,p, pch=8,cex=2)

quartz()
par(mfrow=(c(1,3)))
#forcing
plot(int,f,xlab="Forcing",ylab="BB DOY",type="p", col="white", xlim=c(min(as.numeric(bb.stan$force.z)),max(as.numeric(bb.stan$force.z))),ylim=c(min(bb.stan$resp),100))
for(s in 1:length(spp)){
abline(sp.ints[s],sp.f[s], col=cols[s])
}
abline(int,f,lwd=2,col="black")
#chilling
plot(int,c,xlab="Chilling",ylab="BB DOY",type="p", col="white", xlim=c(min(as.numeric(bb.stan$chill.ports.z)),max(as.numeric(bb.stan$chill.ports.z))),ylim=c(min(bb.stan$resp),100))

for(s in 1:length(spp)){
  abline(sp.ints[s],sp.f[s], col=cols[s])
}
abline(int,c,lwd=2,col="black")

plot(sp.ints,sp.p,xlab="intercept",ylab="photo.est",typ="p", pch=21, bg=cols)
points(int,p, pch=8,cex=2)
