#### geographic and climate corelation for European species 
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses/ranges/") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/ranges/")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges/") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges/") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges/") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

clim<-read.csv("output/Synthesis_climate_EUsps.csv")
extent<-read.csv("output/minmax_rangeextent.csv")

###1 Does species with more Chilling correlate with higher higher latitude
clim.chill<-filter(clim,variable=="Mean.Chill.Utah")
extent.EU<- extent %>% filter(continent=="europe") %>% dplyr::select(minlat,species,maxlat)%>%dplyr::group_by(species) %>% dplyr::summarise(meanmin=mean(minlat),meanmax=max(maxlat))
extent.EU$geo_extent<-extent.EU$meanmax-extent.EU$meanmin

cor(clim.chill$Geo.SD,extent.EU$geo_extent)
cor(clim.chill$Temp.SD,extent.EU$geo_extent)

par(mfrow=c(1,2))

jpeg("figures/extentbychillingvar.jpeg",width =8,height=6,units = "in",res=200)
par(mfrow=c(1,2))
plot(extent.EU$geo_extent,clim.chill$Geo.SD,main=round(cor(clim.chill$Geo.SD,extent.EU$geo_extent),digit=3))
abline(lm(clim.chill$Geo.SD~extent.EU$geo_extent),col="blue")

plot(extent.EU$geo_extent,clim.chill$Temp.SD,main=round(cor(clim.chill$Temp.SD,extent.EU$geo_extent),digit=3))
abline(lm(clim.chill$Temp.SD~extent.EU$geo_extent),col="red")

dev.off()

jpeg("figures/extentbychillingmean.jpeg",width =8,height=6,units = "in",res=200)
par(mfrow=c(1,2))
plot(extent.EU$geo_extent,clim.chill$Geo.Mean,main=round(cor(clim.chill$Geo.Mean,extent.EU$geo_extent),digit=3))
abline(lm(clim.chill$Geo.Mean~extent.EU$geo_extent),col="blue")

plot(extent.EU$geo_extent,clim.chill$Temp.Mean,main=round(cor(clim.chill$Temp.Mean,extent.EU$geo_extent),digit=3))
abline(lm(clim.chill$Temp.Mean~extent.EU$geo_extent),col="red")
dev.off()

####now this for northern limit
jpeg("figures/N_S_bychillingmean.jpeg",width =8,height=6,units = "in",res=200)
par(mfrow=c(1,4))
plot(extent.EU$meanmax,clim.chill$Geo.Mean,main=round(cor(clim.chill$Geo.Mean,extent.EU$meanmax),digit=3))
abline(lm(clim.chill$Geo.Mean~extent.EU$meanmax),col="blue")

plot(extent.EU$meanmin,clim.chill$Geo.Mean,main=round(cor(clim.chill$Geo.Mean,extent.EU$meanmin),digit=3))
abline(lm(clim.chill$Geo.Mean~extent.EU$meanmin),col="blue",lty=2)

plot(extent.EU$meanmax,clim.chill$Temp.Mean,main=round(cor(clim.chill$Temp.Mean,extent.EU$meanmax),digit=3))
abline(lm(clim.chill$Temp.Mean~extent.EU$meanmax),col="red")

plot(extent.EU$meanmin,clim.chill$Temp.Mean,main=round(cor(clim.chill$Temp.Mean,extent.EU$meanmin),digit=3))
abline(lm(clim.chill$Temp.Mean~extent.EU$meanmin),col="red",lty=2)

dev.off()

###forcing
clim.warm<-filter(clim,variable=="GDD")
par(mfrow=c(1,2))
jpeg("figures/extentbyGDDvar.jpeg",width =8,height=6,units = "in",res=200)
par(mfrow=c(1,2))
plot(extent.EU$geo_extent,clim.warm$Geo.SD,main=round(cor(clim.warm$Geo.SD,extent.EU$geo_extent),digit=3))
abline(lm(clim.warm$Geo.SD~extent.EU$geo_extent),col="blue")

plot(extent.EU$geo_extent,clim.warm$Temp.SD,main=round(cor(clim.warm$Temp.SD,extent.EU$geo_extent),digit=3))
abline(lm(clim.warm$Temp.SD~extent.EU$geo_extent),col="red")
dev.off()


jpeg("figures/extentbyGDDmean.jpeg",width =8,height=6,units = "in",res=200)
par(mfrow=c(1,2))
plot(extent.EU$geo_extent,clim.warm$Geo.Mean,main=round(cor(clim.warm$Geo.Mean,extent.EU$geo_extent),digit=3))
abline(lm(clim.warm$Geo.Mean~extent.EU$geo_extent),col="blue")

plot(extent.EU$geo_extent,clim.warm$Temp.Mean,main=round(cor(clim.warm$Temp.Mean,extent.EU$geo_extent),digit=3))
abline(lm(clim.warm$Temp.Mean~extent.EU$geo_extent),col="red")
dev.off()


jpeg("figures/N_S_byGDDmean.jpeg",width =8,height=6,units = "in",res=200)
par(mfrow=c(1,4))
plot(extent.EU$meanmax,clim.warm$Geo.Mean,main=round(cor(clim.warm$Geo.Mean,extent.EU$meanmax),digit=3))
abline(lm(clim.warm$Geo.Mean~extent.EU$meanmax),col="blue")

plot(extent.EU$meanmin,clim.warm$Geo.Mean,main=round(cor(clim.warm$Geo.Mean,extent.EU$meanmin),digit=3))
abline(lm(clim.warm$Geo.Mean~extent.EU$meanmin),col="blue",lty=2)

plot(extent.EU$meanmax,clim.warm$Temp.Mean,main=round(cor(clim.warm$Temp.Mean,extent.EU$meanmax),digit=3))
abline(lm(clim.warm$Temp.Mean~extent.EU$meanmax),col="red")

plot(extent.EU$meanmin,clim.warm$Temp.Mean,main=round(cor(clim.warm$Temp.Mean,extent.EU$meanmin),digit=3))
abline(lm(clim.warm$Temp.Mean~extent.EU$meanmin),col="red",lty=2)

dev.off()

###centroid
extent.EU$centroid<-(extent.EU$meanmax+extent.EU$meanmin)/2

jpeg("figures/centroidbyclimmeans.jpeg",width =8,height=6,units = "in",res=200)
par(mfrow=c(1,4))
plot(extent.EU$centroid,clim.chill$Geo.Mean,main=round(cor(clim.chill$Geo.Mean,extent.EU$centroid),digit=3))
abline(lm(clim.chill$Geo.Mean~extent.EU$centroid),col="blue")

plot(extent.EU$centroid,clim.chill$Temp.Mean,main=round(cor(clim.chill$Temp.Mean,extent.EU$centroid),digit=3))
abline(lm(clim.chill$Temp.Mean~extent.EU$centroid),col="red")

plot(extent.EU$centroid,clim.warm$Geo.Mean,main=round(cor(clim.warm$Geo.Mean,extent.EU$centroid),digit=3))
abline(lm(clim.warm$Geo.Mean~extent.EU$centroid),col="blue",lty=2)

plot(extent.EU$centroid,clim.warm$Temp.Mean,main=round(cor(clim.warm$Temp.Mean,extent.EU$centroid),digit=3))
abline(lm(clim.warm$Temp.Mean~extent.EU$centroid),col="red",lty=2)
dev.off()
