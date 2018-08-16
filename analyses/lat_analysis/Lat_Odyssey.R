### Looking at Lat Model in Odyssey

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(jtools)
library(brms)
library(egg)

ospr.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/lat_wRibesandUlmus.csv", header=TRUE)

ospr.stan$forcez <- (ospr.stan$force-mean(ospr.stan$force,na.rm=TRUE))/sd(ospr.stan$force,na.rm=TRUE)
ospr.stan$photoz <- (ospr.stan$photo-mean(ospr.stan$photo,na.rm=TRUE))/sd(ospr.stan$photo,na.rm=TRUE)
ospr.stan$sm.chillz <- (ospr.stan$sm.chill-mean(ospr.stan$sm.chill,na.rm=TRUE))/sd(ospr.stan$sm.chill,na.rm=TRUE)
ospr.stan$latz <- (ospr.stan$lat-mean(ospr.stan$lat,na.rm=TRUE))/sd(ospr.stan$lat,na.rm=TRUE)

lat.inter_brm<-brm(resp~ forcez + photoz + sm.chillz + latz + photoz:latz + forcez:latz + forcez:photoz + forcez:sm.chillz +
                     sm.chillz:photoz + 
                     (forcez + photoz + sm.chillz + latz|sp), 
                   data=ospr.stan, warmup=2500,iter=4000, chains = 2, cores = 1,
                   control = list(max_treedepth = 12,adapt_delta = 0.99))

save(lat.inter_brm, file="/n/wolkovich_lab/Lab/Cat/lat_justlat.Rdata")

warm<-interact_plot(model = lat.inter_brm, pred = forcez, modx = latz)
day<-interact_plot(model = lat.inter_brm, pred = photoz, modx = latz)


quartz()
ggarrange(warm, day)

bb.stan<-read.csv("~/Documents/git/ospree/analyses/lat_analysis/lat_output/lat_wRibesandUlmus.csv", header=TRUE)
bb.stan$forcez <- (bb.stan$force-mean(bb.stan$force,na.rm=TRUE))/sd(bb.stan$force,na.rm=TRUE)
bb.stan$photoz <- (bb.stan$photo-mean(bb.stan$photo,na.rm=TRUE))/sd(bb.stan$photo,na.rm=TRUE)
bb.stan$sm.chillz <- (bb.stan$sm.chill-mean(bb.stan$sm.chill,na.rm=TRUE))/sd(bb.stan$sm.chill,na.rm=TRUE)
bb.stan$latz <- (bb.stan$lat-mean(bb.stan$lat,na.rm=TRUE))/sd(bb.stan$lat,na.rm=TRUE)


plot(forcez~latz, data=bb.stan)
plot(forcez~photoz, data=bb.stan)
plot(forcez~sm.chillz, data=bb.stan)
plot(forcez~sm.chillz, data=bb.stan)
plot(photoz~latz, data=bb.stan)
plot(photoz~forcez, data=bb.stan)
plot(photoz~sm.chillz, data=bb.stan)

hist(bb.stan$latz)
mean(bb.stan$latz)
lowforce <- subset(bb.stan, latz>0)
hiforce <- subset(bb.stan, latz<=0)


intxnplot <- function(lowdf, hidf){
  par(mfrow=c(3,2))
  plot(resp~forcez, lowdf, main="low")
  abline(lm(resp~forcez, lowdf))
  plot(resp~forcez, hidf, main="high")
  abline(lm(resp~forcez, hidf))
  plot(resp~photoz, lowdf)
  abline(lm(resp~photoz, lowdf))
  plot(resp~photoz, hidf)
  abline(lm(resp~photoz, hidf))
}

intxnplot(lowforce, hiforce)
