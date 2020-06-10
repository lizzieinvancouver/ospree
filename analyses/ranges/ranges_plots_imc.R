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
#setwd("~/Documents/github/ospree/analyses/ranges")

library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(tidyr)

# Set up colors
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
my.pch <- rep(15:18, each=12)

df <- read.csv("output/ranges_climclean.csv")
head(df)

latnam <- read.csv("output/range_extent.nasps_corr.csv")
lateur <- read.csv("output/range_extent.eusps.corr.csv")

#plots by Nacho - exploring lat - climate relationships 
#------------------------------------------------------

## getting data ready
latnam$region <- "NAm"
lateur$region <- "Eur"

latitudes <- rbind(latnam,lateur)
latitudes$latmidpoint <- rowMeans(cbind(latitudes$min.y,latitudes$max.y))
latitudes$latextent <- apply(cbind(latitudes$min.y,latitudes$max.y),1,diff)


lat_clim <- merge(df, latitudes, by.x = "complex_name", by.y = "species")

# beware lacking species!!
sort(unique(latitudes$species))[which(! sort(unique(latitudes$species))%in%sort(unique(lat_clim$complex_name)))]

## explore
latnam <- subset(lat_clim,region=="NAm")
latnam<-aggregate(latnam,by=list(latnam$complex_name),FUN = mean)

lateur <- subset(lat_clim,region=="Eur")
lateur <- aggregate(lateur, by = list(lateur$complex_name),FUN = mean)


## exploring geographical patterns in experienced climate 
par(mfrow=c(2,4),mar=c(5,5,1,1))

plot(latnam$max.y,latnam$gdd,
     xlab="max latitude",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(63,750,"North America")
abline(lm(latnam$gdd~latnam$max.y),col="grey")

plot(latnam$latmidpoint,latnam$gdd,
     xlab="mid latitude",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$gdd~latnam$latmidpoint),col="grey")

plot(latnam$min.y,latnam$gdd,
     xlab="min latitude",ylab="meann GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$gdd~latnam$min.y),col="grey")

plot(latnam$latextent,latnam$gdd,
     xlab="latitudinal range",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$gdd~latnam$latextent),col="grey")


plot(lateur$max.y,lateur$gdd,
     xlab="max latitude",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(8e+06,750,"Europe")
abline(lm(lateur$gdd~lateur$max.y),col="grey")

plot(lateur$latmidpoint,lateur$gdd,
     xlab="mid latitude",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$gdd~lateur$latmidpoint),col="grey")

plot(lateur$min.y,lateur$gdd,
     xlab="mid latitude",ylab="min GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$gdd~lateur$min.y),col="grey")

plot(lateur$latextent,lateur$gdd,
     xlab="latitudinal range",ylab="mean GDD across years",
     ylim=c(200,800),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$gdd~lateur$latextent),col="grey")



## repeat for chilling
par(mfrow=c(2,4),mar=c(5,5,1,1))

plot(latnam$max.y,latnam$utah,
     xlab="max latitude",ylab="mean Utah across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(52,1800,"North America")
abline(lm(latnam$utah~latnam$max.y),col="grey")

plot(latnam$latmidpoint,latnam$utah,
     xlab="mid latitude",ylab="mean Utah across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$utah~latnam$latmidpoint),col="grey")

plot(latnam$min.y,latnam$utah,
     xlab="min latitude",ylab="mean Utah across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$utah~latnam$min.y),col="grey")

plot(latnam$latextent,latnam$utah,
     xlab="latitudinal range",ylab="mean utah across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$utah~latnam$latextent),col="grey")


plot(lateur$max.y,lateur$utah,
     xlab="max latitude",ylab="mean utah across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(2e+06,1800,"Europe")
abline(lm(lateur$utah~lateur$max.y),col="grey")

plot(lateur$latmidpoint,lateur$utah,
     xlab="mid latitude",ylab="mean utah across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$utah~lateur$latmidpoint),col="grey")

plot(lateur$min.y,lateur$utah,
     xlab="min latitude",ylab="mean utah across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$utah~lateur$min.y),col="grey")

plot(lateur$latextent,lateur$utah,
     xlab="latitudinal range",ylab="mean utah across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$utah~lateur$latextent),col="grey")




## repeat for chilling portions
par(mfrow=c(2,4),mar=c(5,5,1,1))

plot(latnam$max.y,latnam$ports,
     xlab="max latitude",ylab="mean ports across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(52,1800,"North America")
abline(lm(latnam$ports~latnam$max.y),col="grey")

plot(latnam$latmidpoint,latnam$ports,
     xlab="mid latitude",ylab="mean ports across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$ports~latnam$latmidpoint),col="grey")

plot(latnam$min.y,latnam$ports,
     xlab="min latitude",ylab="mean ports across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$ports~latnam$min.y),col="grey")

plot(latnam$latextent,latnam$ports,
     xlab="latitudinal range",ylab="mean ports across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(latnam$ports~latnam$latextent),col="grey")


plot(lateur$max.y,lateur$ports,
     xlab="max latitude",ylab="mean ports across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
text(2e+06,1800,"Europe")
abline(lm(lateur$ports~lateur$max.y),col="grey")

plot(lateur$latmidpoint,lateur$ports,
     xlab="mid latitude",ylab="mean ports across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$ports~lateur$latmidpoint),col="grey")

plot(lateur$min.y,lateur$ports,
     xlab="min latitude",ylab="mean ports across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$ports~lateur$min.y),col="grey")

plot(lateur$latextent,lateur$ports,
     xlab="latitudinal range",ylab="mean ports across years",
     #ylim=c(500,1200),
     pch=19,col=adjustcolor(1,0.7),cex=1.5)
abline(lm(lateur$ports~lateur$latextent),col="grey")






