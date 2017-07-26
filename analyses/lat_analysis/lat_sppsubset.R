## Started 25 July 2017 - Cat

## New approaches to Latitude questions!! Take 27...

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(arm)
library(rgdal)
library(maptools)
library(rgeos)
library(ggmap)
library(gridExtra)

# Set working directory
setwd("~/Documents/git/ospree/analyses")
ospree<-read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

ospree$chillhrs <- as.numeric(ospree$Total_Chilling_Hours)
ospree$chill<-as.numeric(ospree$chillhrs/240)
ospree$photo <- as.numeric(ospree$photoperiod_day)
ospree$force<- as.numeric(ospree$forcetemp)
resps<-c("percentbudburst", "daystobudburst")
ospree<-ospree%>%filter(respvar.simple%in%resps)
ospree$chillpors <- as.numeric(ospree$Total_Chill_portions)
ospree$chillutah<-as.numeric(ospree$Total_Utah_Model)
ospree$provenance.lat<-as.numeric(ospree$provenance.lat)
ospree$sp.name<-paste(ospree$genus, ospree$species)
picabi<-ospree%>%filter(sp.name=="Picea abies")
picabi<-picabi[which(!is.na(picabi$chill)),]
picabi<-picabi[which(!is.na(picabi$force)),]
picabi<-picabi[which(!is.na(picabi$photo)),]

hist(picabi$photo)
picabi$photocat[picabi$photo>12] <- "morethan12"
picabi$photocat[picabi$photo<=12] <- "lessthaneq12"

hist(picabi$provenance.lat)
picabi$latcat[picabi$provenance.lat>55] <- ">55"
picabi$latcat[picabi$provenance.lat<=55] <- "<55"

hist(picabi$force)
picabi$forcecat[picabi$force<10] <- "5-10"
picabi$forcecat[picabi$force<15 & picabi$force>=10] <- "10-15"
picabi$forcecat[picabi$force<20 & picabi$force>=15] <- "15-20"
picabi$forcecat[picabi$force<25 & picabi$force>=20] <- "20-25"

picabi$lat<-as.numeric(picabi$provenance.lat)
picabi$lat<-picabi$lat-min(picabi$lat)
pic.fixed<-lm(response.time~force+photo+chill+lat, data=picabi)
summary(pic.fixed)

cp<-ggplot(picabi, aes(x=chill, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(pic.fixed)[1], slope=coef(pic.fixed)[4]))
fp<-ggplot(picabi, aes(x=force, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(pic.fixed)[1], slope=coef(pic.fixed)[2]))
pp<-ggplot(picabi, aes(x=photo, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(pic.fixed)[1], slope=coef(pic.fixed)[3]))
lat.pic<-ggplot(picabi, aes(x=lat, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(pic.fixed)[1], slope=coef(pic.fixed)[5]))

grid.arrange(cp, fp, pp, lat.pic, ncol=2, nrow =2)



############################ BETPUB ##################################
betpub<-ospree%>%filter(sp.name=="Betula pubescens")
betpub<-betpub[which(!is.na(betpub$chill)),]
betpub<-betpub[which(!is.na(betpub$force)),]
betpub<-betpub[which(!is.na(betpub$photo)),]

hist(betpub$photo)
betpub$photocat[betpub$photo>12] <- "morethan12"
betpub$photocat[betpub$photo<=12] <- "lessthaneq12"

hist(betpub$provenance.lat)
betpub$latcat[betpub$provenance.lat>62] <- ">62"
betpub$latcat[betpub$provenance.lat<=62] <- "<62"


hist(betpub$force)
betpub$forcecat[betpub$force<10] <- "5-10"
betpub$forcecat[betpub$force<15 & betpub$force>=10] <- "10-15"
betpub$forcecat[betpub$force<20 & betpub$force>=15] <- "15-20"
betpub$forcecat[betpub$force<25 & betpub$force>=20] <- "20-25"
betpub$forcecat[betpub$force<35 & betpub$force>=20] <- "25-35"

betpub$lat<-as.numeric(betpub$provenance.lat)
betpub$lat<-betpub$lat-min(betpub$lat)
bpub.fixed<-lm(response.time~force+photo+chill+lat, data=betpub)
summary(bpub.fixed)

cp.bet<-ggplot(betpub, aes(x=chill, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(bpub.fixed)[1], slope=coef(bpub.fixed)[4]))
fp.bet<-ggplot(betpub, aes(x=force, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(bpub.fixed)[1], slope=coef(bpub.fixed)[2]))
pp.bet<-ggplot(betpub, aes(x=photo, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(bpub.fixed)[1], slope=coef(bpub.fixed)[3]))
lat.bet<-ggplot(betpub, aes(x=lat, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(bpub.fixed)[1], slope=coef(bpub.fixed)[5]))

grid.arrange(cp.bet, fp.bet, pp.bet, lat.bet, ncol=2, nrow =2)

############################ BETPEN ##################################
betpen<-ospree%>%filter(sp.name=="Betula pendula")
betpen<-betpen[which(!is.na(betpen$chill)),]
betpen<-betpen[which(!is.na(betpen$force)),]
betpen<-betpen[which(!is.na(betpen$photo)),]

hist(betpen$photo)
betpen$photocat[betpen$photo>12] <- "morethan12"
betpen$photocat[betpen$photo<=12] <- "lessthaneq12"

hist(betpen$provenance.lat)
betpen$latcat[betpen$provenance.lat>62] <- ">62"
betpen$latcat[betpen$provenance.lat<=62] <- "<62"


hist(betpen$force)
betpen$forcecat[betpen$force<10] <- "5-10"
betpen$forcecat[betpen$force<15 & betpen$force>=10] <- "10-15"
betpen$forcecat[betpen$force<20 & betpen$force>=15] <- "15-20"
betpen$forcecat[betpen$force<25 & betpen$force>=20] <- "20-25"

betpen$lat<-as.numeric(betpen$provenance.lat)
betpen$lat<-betpen$lat-min(betpen$lat)
bpen.fixed<-lm(response.time~force+photo+chill+lat, data=betpen)
summary(bpen.fixed)

cp.pen<-ggplot(betpen, aes(x=chill, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(bpen.fixed)[1], slope=coef(bpen.fixed)[4]))
fp.pen<-ggplot(betpen, aes(x=force, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(bpen.fixed)[1], slope=coef(bpen.fixed)[2]))
pp.pen<-ggplot(betpen, aes(x=photo, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(bpen.fixed)[1], slope=coef(bpen.fixed)[3]))
lat.pen<-ggplot(betpen, aes(x=lat, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(bpen.fixed)[1], slope=coef(bpen.fixed)[5]))

grid.arrange(cp.bet, fp.bet, pp.bet, lat.bet, ncol=2, nrow =2)

############################ BETPEN ##################################
fagsyl<-ospree%>%filter(sp.name=="Fagus sylvatica")
fagsyl<-fagsyl[which(!is.na(fagsyl$chill)),]
fagsyl<-fagsyl[which(!is.na(fagsyl$force)),]
fagsyl<-fagsyl[which(!is.na(fagsyl$photo)),]

hist(fagsyl$photo)
fagsyl$photocat[fagsyl$photo>12] <- "morethan12"
fagsyl$photocat[fagsyl$photo<=12] <- "lessthaneq12"

hist(fagsyl$provenance.lat)
fagsyl$latcat[fagsyl$provenance.lat>52] <- ">52"
fagsyl$latcat[fagsyl$provenance.lat<=52] <- "<52"


hist(fagsyl$force)
fagsyl$forcecat[fagsyl$force<10] <- "5-10"
fagsyl$forcecat[fagsyl$force<15 & fagsyl$force>=10] <- "10-15"
fagsyl$forcecat[fagsyl$force<20 & fagsyl$force>=15] <- "15-20"
fagsyl$forcecat[fagsyl$force<25 & fagsyl$force>=20] <- "20-25"
fagsyl$forcecat[fagsyl$force<35 & fagsyl$force>=25] <- "25-35"

fagsyl$lat<-as.numeric(fagsyl$provenance.lat)
fagsyl$lat<-fagsyl$lat-min(fagsyl$lat)
fsyl.fixed<-lm(response.time~force+photo+chill+lat, data=fagsyl)
summary(fsyl.fixed)

cp.fs<-ggplot(fagsyl, aes(x=chill, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(fsyl.fixed)[1], slope=coef(fsyl.fixed)[4]))
fp.fs<-ggplot(fagsyl, aes(x=force, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(fsyl.fixed)[1], slope=coef(fsyl.fixed)[2]))
pp.fs<-ggplot(fagsyl, aes(x=photo, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(fsyl.fixed)[1], slope=coef(fsyl.fixed)[3]))
lat.fs<-ggplot(fagsyl, aes(x=lat, y=response.time)) + geom_point() + 
  geom_abline(aes(intercept=coef(fsyl.fixed)[1], slope=coef(fsyl.fixed)[5]))

grid.arrange(cp.fs, fp.fs, pp.fs, lat.fs, ncol=2, nrow =2)

############################ BETPUB, PICABI, BETPEN, FAGSYL ##################################
myspp<-c("Picea abies", "Betula pubescens", "Betula pendula", "Fagus sylvatica")
all<-ospree%>%filter(sp.name%in%myspp)
all<-all[which(!is.na(all$chill)),]
all<-all[which(!is.na(all$force)),]
all<-all[which(!is.na(all$photo)),]

hist(all$provenance.lat)
all$latcat[all$provenance.lat>57] <- ">57"
all$latcat[all$provenance.lat<=57] <- "<57"

hist(all$photo)
all$photocat[all$photo>12] <- "morethan12"
all$photocat[all$photo<=12] <- "lessthaneq12"

hist(all$force)
all$forcecat[all$force<10] <- "5-10"
all$forcecat[all$force<15 & all$force>=10] <- "10-15"
all$forcecat[all$force<20 & all$force>=15] <- "15-20"
all$forcecat[all$force<25 & all$force>=20] <- "20-25"
all$forcecat[all$force<35 & all$force>=20] <- "25-35"

#ggplot(all,
       #aes(x=chill, y=force, color=response.time)) +
  #facet_wrap(~latcat, nrow=4) + 
  #geom_point(aes(shape=photocat)) +
  #geom_smooth(aes(col=response.time),method="loess", se=FALSE)

#ggplot(all,
       #aes(x=force, y=response.time, color=chill)) +
  #facet_wrap(~latcat, nrow=4) + 
  #geom_point(aes(shape=photocat)) 
#+ geom_smooth(aes(col=response.time),method="loess", se=FALSE)

#ggplot(all,
       #aes(x=photo, y=response.time, color=chill)) +
  #geom_point(aes(shape=forcecat))

#write.csv(all,file="~/Desktop/picabi_betpub.csv", row.names = FALSE)


all$lat<-as.numeric(all$provenance.lat)
all$lat<-all$lat-min(all$lat)
all.fixed<-lm(response.time~force+photo+chill+lat+sp.name, data=all)
summary(all.fixed)

cp.all<-ggplot(all, aes(x=chill, y=response.time)) + geom_point(aes(col=sp.name)) + 
  geom_abline(aes(intercept=coef(all.fixed)[1], slope=coef(all.fixed)[4]))
fp.all<-ggplot(all, aes(x=force, y=response.time)) + geom_point(aes(col=sp.name)) + 
  geom_abline(aes(intercept=coef(all.fixed)[1], slope=coef(all.fixed)[2]))
pp.all<-ggplot(all, aes(x=photo, y=response.time)) + geom_point(aes(col=sp.name)) + 
  geom_abline(aes(intercept=coef(all.fixed)[1], slope=coef(all.fixed)[3]))
lat.all<-ggplot(all, aes(x=lat, y=response.time)) + geom_point(aes(col=sp.name)) + 
  geom_abline(aes(intercept=coef(all.fixed)[1], slope=coef(all.fixed)[5]))

grid.arrange(cp.all, fp.all, pp.all, lat.all, ncol=2, nrow =2)


################# MAPPING #############################
setwd("~/Documents/git/regionalrisk/analyses")
land<-readShapeSpatial("input/natural_earth_vector/50m_physical/ne_50m_land.shp") ## 
boundars<-readShapeSpatial("input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
all<-read.csv("~/Desktop/picabi_betpub.csv", header=TRUE)
map.all<-ggplot() + 
  geom_polygon(data=land, aes(long,lat,group=group), fill="white")+
  geom_path(data=boundars, aes(long,lat, group=group), color="light grey",
            size=0.1) +
  geom_point(data=all, aes(provenance.long, provenance.lat, col=response.time, shape=sp.name)) +
  #scale_size(range=c(1,7), guide = "legend",labs(size="Count")) +
  coord_cartesian(xlim = c(-15, 35), ylim = c(30,70)) +
  theme(aspect.ratio=1)

map.all

############# EXTRA STUFF... ##############
all.3way<-lm(response.time~ sp.name + force + photo + chill + force:provenance.lat+photo:provenance.lat+chill:provenance.lat+
               force:photo +force:chill +chill:photo+chill:force:provenance.lat+ 
               force:photo:provenance.lat +chill:photo:provenance.lat, data=all)
summary(all.3way)

all.cont<-lm(response.time~sp.name+force+photo+chill+provenance.lat+force:provenance.lat+photo:provenance.lat+
               chill:provenance.lat+sp.name:provenance.lat+ force:photo+force:chill+chill:photo+chill:force:provenance.lat+ 
               force:photo:provenance.lat +chill:photo:provenance.lat, data=all)
summary(all.cont)

all.binary<-lm(response.time~sp.name+force+photo+chill+latcat+force:latcat+photo:latcat+
                 chill:latcat+sp.name:latcat+ force:photo+force:chill+chill:photo+chill:force:latcat+ 
                 force:photo:latcat +chill:photo:latcat, data=all)
summary(all.binary)

bet.bi2<-lm(response.time~force+photo+chill+latcat+force:latcat+photo:latcat+
              chill:latcat+force:photo+force:chill+photo:chill+chill:force:latcat+ 
              force:photo:latcat +chill:photo:latcat, data=betpub)
summary(bet.bi2)

pic.binary<-lm(response.time~force+photo+chill+latcat+force:latcat+photo:latcat+
                 chill:latcat+force:photo+force:chill+photo:chill+chill:force:latcat+ 
                 force:photo:latcat +chill:photo:latcat, data=picabi)
summary(pic.binary)

bet.binary<-lm(response.time~force+photo+chill+provenance.lat+force:provenance.lat+photo:provenance.lat+
                 chill:provenance.lat, data=betpub)
summary(bet.binary)



