#### Plotting range data ####
#Faith Jones, started April 7th, 2020, 
#based partly on code in clean_rangesclimatedata.R by Cat 



# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Set working directory
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else setwd("~/Documents/git/ospree/analyses/ranges") 
setwd("~/Documents/github/ospree/analyses/ranges")

library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(tidyr)

# Set up colors
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
my.pch <- rep(15:18, each=12)

df <- read.csv("ranges_climclean.csv")
head(df)

latnam <- read.csv("range_extent.nasps.csv")
lateur <- read.csv("range_extent.eusps.csv")

#plots by Nacho - exploring lat - climate relationships 
#------------------------------------------------------

## getting data ready
latnam$region <- "NAm"
lateur$region <- "Eur"

latitudes <- rbind(latnam,lateur)
latitudes$latmidpoint <- rowMeans(cbind(latitudes$min,latitudes$max))

lat_clim <- merge(df, latitudes, by.x = "complex_name", by.y = "complex")

# beware lacking species!!
sort(unique(latitudes$complex))[which(! sort(unique(latitudes$complex))%in%sort(unique(lat_clim$complex_name)))]

## explore
latnam <- subset(lat_clim,region=="NAm")
latnam<-aggregate(latnam,by=list(latnam$complex_name),FUN = mean)

lateur <- subset(lat_clim,region=="Eur")
lateur <- aggregate(lateur, by = list(lateur$complex_name),FUN = mean)


## exploring geographical patterns in experienced climate 
par(mfrow=c(2,4),mar=c(5,5,1,1))

plot(latnam$max,latnam$gdd,
     xlab="max latitude",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(63,750,"North America")
abline(lm(latnam$gdd~latnam$max),col="grey")

plot(latnam$latmidpoint,latnam$gdd,
     xlab="mid latitude",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$gdd~latnam$latmidpoint),col="grey")

plot(latnam$min,latnam$gdd,
     xlab="mid latitude",ylab="min GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$gdd~latnam$min),col="grey")

plot(latnam$distance,latnam$gdd,
     xlab="latitudinal range",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$gdd~latnam$distance),col="grey")


plot(lateur$max,lateur$gdd,
     xlab="max latitude",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(8e+06,750,"Europe")
abline(lm(lateur$gdd~lateur$latmidpoint),col="grey")

plot(lateur$latmidpoint,lateur$gdd,
     xlab="mid latitude",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$gdd~lateur$latmidpoint),col="grey")

plot(lateur$min,lateur$gdd,
     xlab="mid latitude",ylab="min GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$gdd~lateur$min),col="grey")

plot(lateur$distance,lateur$gdd,
     xlab="latitudinal range",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$gdd~lateur$distance),col="grey")



## repeat for chilling
par(mfrow=c(2,4),mar=c(5,5,1,1))

plot(latnam$max,latnam$utah,
     xlab="max latitude",ylab="mean utah across years",
     ylim=c(500,2000),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(52,1800,"North America")
abline(lm(latnam$utah~latnam$max),col="grey")

plot(latnam$latmidpoint,latnam$utah,
     xlab="mid latitude",ylab="mean utah across years",
     ylim=c(500,2000),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$utah~latnam$latmidpoint),col="grey")

plot(latnam$min,latnam$utah,
     xlab="mid latitude",ylab="min utah across years",
     ylim=c(500,2000),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$utah~latnam$min),col="grey")

plot(latnam$distance,latnam$utah,
     xlab="latitudinal range",ylab="mean utah across years",
     ylim=c(500,2000),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$utah~latnam$distance),col="grey")


plot(lateur$max,lateur$utah,
     xlab="max latitude",ylab="mean utah across years",
     ylim=c(500,2000),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(2e+06,1800,"Europe")
abline(lm(lateur$utah~lateur$latmidpoint),col="grey")

plot(lateur$latmidpoint,lateur$utah,
     xlab="mid latitude",ylab="mean utah across years",
     ylim=c(500,2000),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$utah~lateur$latmidpoint),col="grey")

plot(lateur$min,lateur$utah,
     xlab="mid latitude",ylab="min utah across years",
     ylim=c(500,2000),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$utah~lateur$min),col="grey")

plot(lateur$distance,lateur$utah,
     xlab="latitudinal range",ylab="mean utah across years",
     ylim=c(500,2000),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$utah~lateur$distance),col="grey")






