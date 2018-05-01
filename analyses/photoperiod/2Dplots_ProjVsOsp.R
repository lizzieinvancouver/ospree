##############################################################################################################
#'  Ospree project - photoperiod paper
#'   Script and functions to run:
#'  plotting 3d - shifts in space, time as photoperiod changes
#'
#'  by I. Morales-Castilla
#'  started Sept 2017
#'  
#'  Edited by Cat on 16 March 2018 - using phenofit data and ospree data
##############################################################################################################
# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")

## to start
rm(list=ls())
options(stringsAsFactors=FALSE)

## load packages

library(tidyr)
library(dplyr)
library(geosphere)
library(lubridate)
library(ggplot2)


############################################
############# Phenofit Stuff ###############
############################################
fs.present<-read.csv("output/phenofit_projections/Fagus_sylvatica_1981_2000_daylength.csv", header=TRUE)
fs.pres<-read.csv("output/phenofit_projections/Fagus_sylvatica_1981_2000.csv", header=TRUE)
fs.proj<-read.csv("output/phenofit_projections/Fagus_sylvatica_A1Fi_2081_2100_daylength.csv", header=TRUE)
fs.proj.pres<-read.csv("output/phenofit_projections/Fagus_sylvatica_A1Fi_2081_2100.csv", header=TRUE)

fs.present<-fs.present%>%dplyr::rename(mleaf=MeanDateLeaf)
fs.proj<-fs.proj%>%dplyr::rename(proj.lodoy=lodoy)%>%dplyr::rename(proj.photo=daylength)

fs<-full_join(fs.present, fs.proj)

fs.pres<-subset(fs.pres, pres==1)
fs.pres$avg<-ave(fs.pres$Lat)
fs.proj.pres<-subset(fs.proj.pres, pres==1)
fs.proj.pres$avg<-ave(fs.proj.pres$Lat)
shift<-unique(fs.proj.pres$avg) - unique(fs.pres$avg)
fs.pres$geo.shift<-fs.pres$Lat + shift


fs.geo<-full_join(fs, fs.pres)

fs.geo$date<-as.Date(fs.geo$lodoy, origin = "2000-01-01")
fs.geo$photo.shift<-fs.geo$daylength-fs.geo$proj.photo
#fs.geo$photo.shift<-NA
#for(i in c(1:nrow(fs.geo))){
#  fs.geo$photo.shift[i] <- daylength(fs.geo$geo.shift[i], fs.geo$date[i])
#}

fs.geo<-fs.geo[!is.na(fs.geo$photo.shift),]
fs.geo<-fs.geo[!is.na(fs.geo$geo.shift),]
fs.geo$phen.shift<-fs.geo$lodoy-fs.geo$proj.lodoy ## first pheno.shift
fs.geo$type<-"projected"


###### Ospree now! ########
osp<-read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
fsyl<-osp[which(osp$genus=="Fagus" & osp$species == "sylvatica" & osp$respvar.simple=="daystobudburst"),]
fsyl<-fsyl%>%dplyr::select(datasetID, genus, species, photoperiod_day, response.time, provenance.lat, fieldsample.date2)
fsyl<-fsyl[(fsyl$response.time!=999),]
fsyl$photoperiod_day<-as.numeric(fsyl$photoperiod_day)
fsyl$photo.min<-ave(fsyl$photoperiod_day, fsyl$datasetID, FUN=min)
fsyl$photo.max<-ave(fsyl$photoperiod_day, fsyl$datasetID, FUN=max)
fsyl$photo.shift<-fsyl$photo.max-fsyl$photo.min

fsyl$response.time<-as.numeric(fsyl$response.time)
bb.min<-fsyl[which(fsyl$photoperiod_day==fsyl$photo.min),]
bb.min$bb.min<-ave(bb.min$response.time, bb.min$datasetID)
bb.max<-fsyl[which(fsyl$photoperiod_day==fsyl$photo.max),]
bb.max$bb.max<-ave(bb.max$response.time, bb.max$datasetID)


fsyl$phen.shift<-NA
fsyl$lo.pre<-NA
fsyl$lo.post<-NA
for(i in c(1:nrow(fsyl))){
  for(j in c(1:nrow(bb.min)))
    for(k in c(1:nrow(bb.max)))
      fsyl$phen.shift[i]<-ifelse(fsyl$datasetID[i]==bb.min$datasetID[j] & fsyl$datasetID[i]==bb.max$datasetID[k],
                                 bb.min$bb.min[j]-bb.max$bb.max[k], fsyl$phen.shift[i])
    fsyl$lo.pre[i]<-ifelse(fsyl$datasetID[i]==bb.min$datasetID[j] & fsyl$datasetID[i]==bb.max$datasetID[k],
                           bb.min$bb.min[j], fsyl$lo.pre[i])
    fsyl$lo.post[i]<-ifelse(fsyl$datasetID[i]==bb.min$datasetID[j] & fsyl$datasetID[i]==bb.max$datasetID[k],
                           bb.max$bb.max[k], fsyl$lo.post[i])
}

fsyl$provenance.lat<-as.numeric(fsyl$provenance.lat)
#### Using code from Ailene's shifts_table.R ###
fsyl$phendate<-ave(fsyl$response.time, fsyl$datasetID)

fsyl$space<-NA
for(i in 1:length(fsyl$provenance.lat)){
  latshift<-seq(0,40,by=.1)#look at daylengths of latitudes from study site to study site plus 40 degrees
  photos_spat<-daylength(fsyl$provenance.lat[i]+latshift, fsyl$phendate[i])
  #photop$space[i]<-
  maxdelta_space<-max(photos_spat, na.rm=TRUE)-min(photos_spat, na.rm=TRUE)#maxim
  delta_space<-photos_spat[1]-photos_spat#change in daylength latitudes ranging from study site to study site plus 40 degrees
  
  if(maxdelta_space<abs(fsyl$photo.shift[i])){fsyl$space[i]<-100}#exceeds range
  else
    fsyl$space[i]<-latshift[min(which(round(delta_space, digits=2)==fsyl$photo.shift[i]))]#select min lat shift required to get change in daylength in experiments
  if(is.na(fsyl$space[i])){
    mindiff<-min(abs(fsyl$photo.shift[i]-delta_space), na.rm=TRUE)
    
    if(!is.na(mindiff) & mindiff<0.5){fsyl$space[i]<-latshift[which(abs(fsyl$photo.shift[i]-delta_space)==mindiff)]
    }else {
      fsyl$space[i]<-NA
    }
  }
}
fsyl$space<-as.numeric(fsyl$space)
fsyl$geo.shift<-fsyl$space+fsyl$provenance.lat

fsyl<-fsyl%>%dplyr::rename(daylength=photoperiod_day)%>%dplyr::rename(mleaf=response.time)%>%dplyr::rename(Lat=provenance.lat)
fsyl$type<-"ospree"
ff<-full_join(fs.geo, fsyl)
ff<-dplyr::select(ff, Lat, mleaf, lodoy, daylength, type, photo.shift, geo.shift, phen.shift, proj.lodoy, proj.photo,
                  lo.pre, lo.post, photo.min, photo.max)
ff<-ff%>%dplyr::rename(current.photo=daylength)%>%dplyr::rename(osp.photo.pre=photo.min)%>%dplyr::rename(osp.photo.post=photo.max)

fx<-ff%>%gather("phen.type", "doy", -Lat, -mleaf, -type, -photo.shift, -geo.shift, 
                -phen.shift, -proj.photo, -osp.photo.pre, -osp.photo.post, -current.photo)
fxx<-fx%>%gather("photo.type", "photoperiod", -Lat, -mleaf, -phen.type, -type, -photo.shift, -geo.shift, -phen.shift,
                -doy)

fxx<-na.omit(fxx)

find_hull.geo <- function(fxx) fxx[chull(fxx$Lat, fxx$photoperiod), ]
#library(plyr)
hulls.geo <- ddply(fxx, "type", find_hull.geo)
fxx$photo.type<-ifelse(fxx$photo.type=="osp.photo.pre", "ospree", fxx$photo.type)
fxx$photo.type<-ifelse(fxx$photo.type=="osp.photo.post", "ospree", fxx$photo.type)
fxx$photo.type<-ifelse(fxx$photo.type=="current.photo", "current", fxx$photo.type)
fxx$photo.type<-ifelse(fxx$photo.type=="proj.photo", "projected", fxx$photo.type)

fxx$phen.type<-ifelse(fxx$phen.type=="lodoy", "current", fxx$phen.type)
fxx$phen.type<-ifelse(fxx$phen.type=="proj.lodoy", "projected", fxx$phen.type)
fxx$phen.type<-ifelse(fxx$phen.type=="lo.pre", "ospree", fxx$phen.type)
fxx$phen.type<-ifelse(fxx$phen.type=="lo.post", "ospree", fxx$phen.type)


geo.photo<-ggplot(fxx, aes(x=Lat, y=photoperiod)) + geom_point(aes(col=photo.type)) +
  geom_polygon( data=hulls.geo, alpha=.5, aes(fill=photo.type)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.ticks.y = element_blank(), legend.position = "none") + xlab("Latitude") + ylab("Daylength")


phen<- ggplot(ff, aes(x=phen.shift, y=photo.shift)) + geom_point(aes(col=type)) +
  geom_polygon( data=hulls.phen, alpha=.5, aes(fill=type)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.ticks.y = element_blank(), legend.position = "none")

find_hull.phen <- function(fxx) fxx[chull(fxx$doy, fxx$photoperiod), ]
hulls.phen <- ddply(fxx, "type", find_hull.phen)
doy.photo<- ggplot(fxx, aes(x=doy, y=photoperiod)) + geom_point(aes(col=phen.type)) +
  geom_polygon( data=hulls.phen, alpha=.5, aes(fill=phen.type)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.ticks.y = element_blank()) + xlab("Day of Budburst") + ylab("Daylength")


library(egg)
quartz()
ggarrange(geo.photo, doy.photo, ncol=2)




#########################
#### Quercus stuff! #####
#########################
qr.present<-read.csv("output/phenofit_projections/Quercus_robur_1981_2000_daylength.csv", header=TRUE)
qr.pres<-read.csv("output/phenofit_projections/Quercus_robur_1981_2000.csv", header=TRUE)
qr.proj<-read.csv("output/phenofit_projections/Quercus_robur_A1Fi_2081_2100_daylength.csv", header=TRUE)
qr.proj.pres<-read.csv("output/phenofit_projections/Quercus_robur_A1Fi_2081_2100.csv", header=TRUE)

qr.present<-qr.present%>%dplyr::rename(mleaf=MeanDateLeaf)
qr.proj<-qr.proj%>%dplyr::rename(proj.lodoy=lodoy)%>%dplyr::rename(proj.photo=daylength)

qr<-full_join(qr.present, qr.proj)

qr.pres<-subset(qr.pres, pres==1)
qr.pres$avg<-ave(qr.pres$Lat)
qr.proj.pres<-subset(qr.proj.pres, pres==1)
qr.proj.pres$avg<-ave(qr.proj.pres$Lat)
shift<-unique(qr.proj.pres$avg) - unique(qr.pres$avg)
qr.pres$geo.shift<-qr.pres$Lat + shift


qr.geo<-full_join(qr, qr.pres)

qr.geo$date<-as.Date(qr.geo$lodoy, origin = "2000-01-01")
qr.geo$photo.shift<-qr.geo$daylength-qr.geo$proj.photo
#qr.geo$photo.shift<-NA
#for(i in c(1:nrow(qr.geo))){
#  qr.geo$photo.shift[i] <- daylength(qr.geo$geo.shift[i], qr.geo$date[i])
#}

qr.geo<-qr.geo[!is.na(qr.geo$photo.shift),]
qr.geo<-qr.geo[!is.na(qr.geo$geo.shift),]
qr.geo$phen.shift<-qr.geo$lodoy-qr.geo$proj.lodoy ## first pheno.shift
qr.geo$type<-"projected"

qr$date<-as.Date(qr$lodoy, origin = "2000-01-01")
#qr$photo.shift<-NA
#for(i in c(1:nrow(qr))){
#  qr$photo.shift[i] <- daylength(qr$geo.shift[i], qr$date[i])
#}

qr$phen.shift<-qr$lodoy-qr$lday.prj ## first pheno.shift


### Now for Ospree! ####
qrob<-osp[which(osp$genus=="Fagus" & osp$species == "sylvatica" & osp$respvar.simple=="daystobudburst"),]
qrob<-qrob%>%dplyr::select(datasetID, genus, species, photoperiod_day, response.time, provenance.lat, fieldsample.date2)
qrob<-qrob[(qrob$response.time!=999),]
qrob$photoperiod_day<-as.numeric(qrob$photoperiod_day)
qrob$photo.min<-ave(qrob$photoperiod_day, qrob$datasetID, FUN=min)
qrob$photo.max<-ave(qrob$photoperiod_day, qrob$datasetID, FUN=max)
qrob$photo.shift<-qrob$photo.max-qrob$photo.min

qrob$response.time<-as.numeric(qrob$response.time)
bb.min<-qrob[which(qrob$photoperiod_day==qrob$photo.min),]
bb.min$bb.min<-ave(bb.min$response.time, bb.min$datasetID)
bb.max<-qrob[which(qrob$photoperiod_day==qrob$photo.max),]
bb.max$bb.max<-ave(bb.max$response.time, bb.max$datasetID)


qrob$phen.shift<-NA
for(i in c(1:nrow(qrob))){
  for(j in c(1:nrow(bb.min)))
    for(k in c(1:nrow(bb.max)))
      qrob$phen.shift[i]<-ifelse(qrob$datasetID[i]==bb.min$datasetID[j] & qrob$datasetID[i]==bb.max$datasetID[k],
                                 bb.min$bb.min[j]-bb.max$bb.max[k], qrob$phen.shift[i])
}

qrob$provenance.lat<-as.numeric(qrob$provenance.lat)
#### Using code from Ailene's shifts_table.R ###
qrob$phendate<-ave(qrob$response.time, qrob$datasetID)

qrob$space<-NA
for(i in 1:length(qrob$provenance.lat)){
  latshift<-seq(0,40,by=.1)#look at daylengths of latitudes from study site to study site plus 40 degrees
  photos_spat<-daylength(qrob$provenance.lat[i]+latshift, qrob$phendate[i])
  #photop$space[i]<-
  maxdelta_space<-max(photos_spat, na.rm=TRUE)-min(photos_spat, na.rm=TRUE)#maxim
  delta_space<-photos_spat[1]-photos_spat#change in daylength latitudes ranging from study site to study site plus 40 degrees
  
  if(maxdelta_space<abs(qrob$photo.shift[i])){qrob$space[i]<-100}#exceeds range
  else
    qrob$space[i]<-latshift[min(which(round(delta_space, digits=2)==qrob$photo.shift[i]))]#select min lat shift required to get change in daylength in experiments
  if(is.na(qrob$space[i])){
    mindiff<-min(abs(qrob$photo.shift[i]-delta_space), na.rm=TRUE)
    
    if(!is.na(mindiff) & mindiff<0.5){qrob$space[i]<-latshift[which(abs(qrob$photo.shift[i]-delta_space)==mindiff)]
    }else {
      qrob$space[i]<-NA
    }
  }
}
qrob$space<-as.numeric(qrob$space)
qrob$geo.shift<-qrob$space+qrob$provenance.lat

qrob<-qrob%>%dplyr::rename(daylength=photoperiod_day)%>%dplyr::rename(mleaf=response.time)%>%dplyr::rename(Lat=provenance.lat)
qrob$type<-"ospree"
qq<-full_join(qr.geo, qrob)
qq<-dplyr::select(qq, Lat, mleaf, lodoy, daylength, type, photo.shift, geo.shift, phen.shift)


find_hull.phen <- function(qq) qq[chull(qq$phen.shift, qq$photo.shift), ]
library(plyr)
hulls.phen <- ddply(qq, "type", find_hull.phen)

phen.qq<- ggplot(qq, aes(x=phen.shift, y=photo.shift)) + geom_point(aes(col=type)) +
  geom_polygon( data=hulls.phen, alpha=.5, aes(fill=type)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.ticks.y = element_blank(), legend.position = "none")

find_hull.geo <- function(qq) qq[chull(qq$geo.shift, qq$photo.shift), ]
hulls.geo <- ddply(qq, "type", find_hull.geo)
geo.qq<- ggplot(qq, aes(x=geo.shift, y=photo.shift)) + geom_point(aes(col=type)) +
  geom_polygon( data=hulls.geo, alpha=.5, aes(fill=type)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.ticks.y = element_blank())

quartz()
ggarrange(phen.qq, geo.qq, ncol=2)


