##############################################################################################################
#'  Ospree project - photoperiod paper
#'   Script and functions to run:
#'  plotting 3d - shifts in space, time as photoperiod changes
#'
#'  by I. Morales-Castilla
#'  started Sept 2017
#'  
#'  Editted by Cat on 16 March 2018 - using phenofit data and ospree data
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
packs.to.extract<-list('rgl','raster','sp','rgdal','insol','googleway')
lapply(packs.to.extract,require, character.only=T)
library(tidyr)
library(dplyr)
library(geosphere)
library(lubridate)

# Create some dummy data
#dat <- replicate(2, 1:3)

# Initialize the scene, no data plotted
#plot3d(dat, type = 'n', xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(0, 24), xlab = 'Geographical shift', 
#       ylab = 'Temporal shift', zlab = 'photoperiod') 

# Add planes
#planes3d(10, 10, 1, -10, col = 'red', alpha = 0.6)
#planes3d(1, -1, 1, 0, col = 'orange', alpha = 0.6)
#planes3d(1, -1, -1, -0.8, col = 'blue', alpha = 0.6)


# Create some dummy data based on real shifts:

#Flowering phenology has shifted earlier: 
  #2.5-5 days per degC (
  #4.5 days per degC (Wolkovich et al 2012)
  #Leaf phenology has shifted earlier:
   #   6.4 days per degC (Wolkovich et al 2012)
    #All phenology together: 
     # 2.3 days per decade mean advancement of spring events (Parmesan & Yohe 2003)
#pheno.shift=5    ## choosing 5 days per degree celsius

#   Species ranges have shifted shifted:
    #In elevation at a median rate of 11.0 meters per decade (Chen et al 2011) or 6.1 metres per decade upward (Parmesan & Yohe 2003) 
    #To higher latitudes at a median rate of 16.9 kilometers per decade (Chen et al 2011) or 6.1 km per decade (Parmesan & Yohe 2003)
#geo.shift=16.9/10 ## choosing 16.9Km  per decade     
#geo.shift=33.8 ## choosing 16.9Km*2 per degree celsius    

#dummy.lattice <- data.frame(dist.equator=seq(100,1200,100),
#                      Temp.gradient=seq(21,10,-1),
#                      warming.gradient=seq(0.2,2.4,0.2),
#                      geo.shift=seq(0.2,2.4,0.2)*33.8,
#                      pheno.shift=seq(0.2,2.4,0.2)*5,
#                      Photo.gradient=seq(12,6.5,-0.5)
#                      )


# Initialize the scene, no data plotted
#plot3d(dummy.lattice[,4:6], type = 'n', xlim = c(0, 100), ylim = c(0, 12), zlim = c(0, 24), xlab = 'Geographical shift (Km/year)', 
#       ylab = 'Temporal shift (days/degreeC)', zlab = 'Photoperiod (h)',size=10) 

#plot3d(dummy.lattice[,4:6], type = 'p', xlim = c(0, 100), ylim = c(0, 12), zlim = c(0, 24), xlab = 'Geographical shift (Km/year)', 
#       ylab = 'Temporal shift (days/degreeC)', zlab = 'Photoperiod (h)',size=10) 


## get point coordinates 

#open3d()
#geo.shift <- dummy.lattice$geo.shift+rnorm(12,0,0.07)
#pheno.shift<-dummy.lattice$pheno.shift+rnorm(12,0,0.9)
#photoperiod<-dummy.lattice$Photo.gradient

#fit <- lm(photoperiod ~ geo.shift + pheno.shift)
#plot3d(geo.shift, pheno.shift, photoperiod, type = "s", col = "indianred", size = 1)

#coefs <- coef(fit)
#a <- coefs["geo.shift"]
#b <- coefs["pheno.shift"]
#c <- -1
#d <- coefs["(Intercept)"]
#planes3d(a, b, c, d, alpha = 0.5,col="indianred2")


#rgl.postscript("photo.shifts.pdf", fmt="pdf")
#rgl.snapshot(filename="photo.shifts.png",fmt="png")

#geo.shift <- dummy.lattice$geo.shift+rnorm(12,0,0.07)
#pheno.shift<-dummy.lattice$pheno.shift+rnorm(12,0,0.9)
#photoperiod<-dummy.lattice$Photo.gradient

#fit <- lm(photoperiod ~ geo.shift + pheno.shift)
#plot3d(geo.shift, pheno.shift, photoperiod, type = "s", col = "indianred", size = 1)

#coefs <- coef(fit)
#a <- coefs["geo.shift"]
#b <- coefs["pheno.shift"]
#c <- -1
#d <- coefs["(Intercept)"]
#planes3d(a, b, c, d, alpha = 0.5,col="indianred2")


#rgl.postscript("photo.shifts.pdf", fmt="pdf")
#rgl.snapshot(filename="photo.shifts.png",fmt="png")


############################################
############# Phenofit Stuff ###############
############################################
fs.present<-read.csv("output/phenofit_projections/Fagus_sylvatica_1981_2000_daylength.csv", header=TRUE)
fs.pres<-read.csv("output/phenofit_projections/Fagus_sylvatica_1981_2000.csv", header=TRUE)
fs.proj<-read.csv("output/phenofit_projections/Fagus_sylvatica_A1Fi_2081_2100_daylength.csv", header=TRUE)
fs.proj.pres<-read.csv("output/phenofit_projections/Fagus_sylvatica_A1Fi_2081_2100.csv", header=TRUE)

fs.present<-fs.present%>%dplyr::rename(mleaf=MeanDateLeaf)
fs.proj<-fs.proj%>%dplyr::rename(mleaf.proj=MeanDateLeaf)%>%dplyr::rename(lday.prj=lodoy)%>%
  dplyr::rename(photo.prj=daylength)

fs<-full_join(fs.present, fs.proj)

fs.pres<-subset(fs.pres, pres==1)
fs.pres$avg<-ave(fs.pres$Lat)
fs.proj.pres<-subset(fs.proj.pres, pres==1)
fs.proj.pres$avg<-ave(fs.proj.pres$Lat)
shift<-unique(fs.proj.pres$avg) - unique(fs.pres$avg)
fs.pres$geo.shift<-fs.pres$Lat + shift

fs<-full_join(fs, fs.pres)

fs$date<-as.Date(fs$lodoy, origin = "2000-01-01")
fs$photo.shift<-NA
for(i in c(1:nrow(fs))){
  fs$photo.shift[i] <- daylength(fs$geo.shift[i], fs$date[i])
}

fs$phen.shift<-fs$lodoy-fs$lday.prj ## first pheno.shift

fitphen<-lm(photo.shift ~ geo.shift + phen.shift, data=fs)

osp<-read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
fsyl<-subset(osp, genus=="Fagus")
fsyl<-subset(fsyl, species=="sylvatica")
fsyl<-fsyl%>%dplyr::select(genus, species, photoperiod_day, response.time, provenance.lat, fieldsample.date2)
fsyl$photoperiod_day<-as.numeric(fsyl$photoperiod_day)
fsyl$response.time<-as.numeric(fsyl$response.time)
fsyl$provenance.lat<-as.numeric(fsyl$provenance.lat)

fsyl$avg<-ave(fsyl$provenance.lat)
osp.shift<-unique(fsyl$avg)-unique(fs.pres$avg)
fsyl$geo.shift<-fsyl$provenance.lat+osp.shift

fsyl$date<-as.Date(fsyl$response.time, origin = fsyl$fieldsample.date2)
fsyl$photo.shift<-NA
for(i in c(1:nrow(fsyl))){
  fsyl$photo.shift[i] <- daylength(fsyl$geo.shift[i], fsyl$date[i])
}

fsyl$osp.doy<-yday(fsyl$date)
fsyl$pres.doy<-NA
fs$Lat<-round(fs$Lat, digits=1)
fsyl$provenance.lat<-round(fsyl$provenance.lat, digits=1)
for(i in c(1:nrow(fsyl))){
  for(j in c(1:nrow(fs)))
    if(fsyl$provenance.lat[i]==fs$Lat[j])
      fsyl$pres.doy[i]<-fs$lodoy[j]
}

fsyl$phen.shift<-fsyl$pres.doy-fsyl$osp.doy

fitosp.fs<-lm(photo.shift ~ geo.shift + phen.shift, data=fsyl)
#open3d()
plot3d(fs$geo.shift, fs$phen.shift, fs$photo.shift, type = "s", col="lightblue", size=1)

coefs <- coef(fitphen)
af <- coefs["geo.shift"]
bf <- coefs["phen.shift"]
cf <- -1
df <- coefs["(Intercept)"]
planes3d(af, bf, cf, df, alpha = 0.5, col="lightblue" )

#plot3d(fsyl$geo.shift, fsyl$phen.shift, fsyl$photo.shift, type = "s", col="red", size=1)

coefs <- coef(fitosp.fs)
ao <- coefs["geo.shift"]
bo <- coefs["phen.shift"]
co <- -1
do <- coefs["(Intercept)"]
planes3d(ao, bo, co, do, alpha = 0.5, col="red" )

### Range of current BB dates (Fagus): 94-225
### Range of projected BB dates (Fagus): 92-167

#rgl.postscript("FAGSYL_shifts.pdf", fmt="pdf")
rgl.snapshot(filename="photoperiod/figures/FAGSYL_shifts.png",fmt="png")


#########################
#### Quercus stuff! #####
#########################
qr.present<-read.csv("output/phenofit_projections/Quercus_robur_1981_2000_daylength.csv", header=TRUE)
qr.pres<-read.csv("output/phenofit_projections/Quercus_robur_1981_2000.csv", header=TRUE)
qr.proj<-read.csv("output/phenofit_projections/Quercus_robur_A1Fi_2081_2100_daylength.csv", header=TRUE)
qr.proj.pres<-read.csv("output/phenofit_projections/Quercus_robur_A1Fi_2081_2100.csv", header=TRUE)

qr.present<-qr.present%>%dplyr::rename(mleaf=MeanDateLeaf)
qr.proj<-qr.proj%>%dplyr::rename(mleaf.proj=MeanDateLeaf)%>%dplyr::rename(lday.prj=lodoy)%>%
  dplyr::rename(photo.prj=daylength)

qr<-full_join(qr.present, qr.proj)

qr.pres<-subset(qr.pres, pres==1)
qr.pres$avg<-ave(qr.pres$Lat)
qr.proj.pres<-subset(qr.proj.pres, pres==1)
qr.proj.pres$avg<-ave(qr.proj.pres$Lat)
shift.qr<-unique(qr.proj.pres$avg) - unique(qr.pres$avg)
qr.pres$geo.shift<-qr.pres$Lat + shift

qr<-full_join(qr, qr.pres)

qr$date<-as.Date(qr$lodoy, origin = "2000-01-01")
qr$photo.shift<-NA
for(i in c(1:nrow(qr))){
  qr$photo.shift[i] <- daylength(qr$geo.shift[i], qr$date[i])
}

qr$phen.shift<-qr$lodoy-qr$lday.prj ## first pheno.shift

fitphen.qr<-lm(photo.shift ~ geo.shift + phen.shift, data=qr)


qrob<-subset(osp, genus=="Quercus")
qrob<-subset(qrob, species=="robur")
qrob<-qrob%>%dplyr::select(genus, species, photoperiod_day, response.time, provenance.lat, fieldsample.date2)
qrob$photoperiod_day<-as.numeric(qrob$photoperiod_day)
qrob$response.time<-as.numeric(qrob$response.time)
qrob$provenance.lat<-as.numeric(qrob$provenance.lat)

qrob$avg<-ave(qrob$provenance.lat)
osp.shift<-unique(qrob$avg)-unique(qr.pres$avg)
qrob$geo.shift<-qrob$provenance.lat+osp.shift

qrob$date<-as.Date(qrob$response.time, origin = qrob$fieldsample.date2)
qrob$photo.shift<-NA
for(i in c(1:nrow(qrob))){
  qrob$photo.shift[i] <- daylength(qrob$geo.shift[i], qrob$date[i])
}

qrob$osp.doy<-yday(qrob$date)
qrob$pres.doy<-NA
qr$Lat<-round(qr$Lat, digits=1)
qrob$provenance.lat<-round(qrob$provenance.lat, digits=1)
for(i in c(1:nrow(qrob))){
  for(j in c(1:nrow(qr)))
    if(qrob$provenance.lat[i]==qr$Lat[j])
      qrob$pres.doy[i]<-qr$lodoy[j]
}

qrob$phen.shift<-qrob$pres.doy-qrob$osp.doy

fitosp.qr<-lm(photo.shift ~ geo.shift + phen.shift, data=qrob)
#open3d()
plot3d(qr$geo.shift, qr$phen.shift, qr$photo.shift, type = "s", col="green", size=1)

coefs <- coef(fitphen.qr)
af <- coefs["geo.shift"]
bf <- coefs["phen.shift"]
cf <- -1
df <- coefs["(Intercept)"]
planes3d(af, bf, cf, df, alpha = 0.5, col="green" )

#plot3d(qrob$geo.shift, qrob$phen.shift, qrob$photo.shift, type = "s", col="red", size=1)

coefs <- coef(fitosp.qr)
ao <- coefs["geo.shift"]
bo <- coefs["phen.shift"]
co <- -1
do <- coefs["(Intercept)"]
planes3d(ao, bo, co, do, alpha = 0.5, col="red" )

### Range of current BB dates (QUEROB): 88-364
### Range of projected BB dates (QUEROB): 92-210

#rgl.postscript("~/ospree/analyses/photoperiod/figures/QUEROB_shifts.pdf", fmt="pdf")
rgl.snapshot(filename="photoperiod/figures/QUEROB_shifts.png",fmt="png")

#################### Code from Nacho below... ############################
############################################
######## getting real data for th US
############################################

####load rasters (average spring index 1981-2016, climate change, etc.)
M<-raster("~/GitHub/ospree/analyses/green_up/si-x-average_leaf_prism.tif")
Clim<-brick('~/Data_Harvard/vitis/future_projections/GCM.07.Tmax2006-2080.nc')
#source("~/GitHub/vin/climatefuture/analyses/projections/input/Script_GCM_rename_corrected_bias.R")

Clims<-rename.GCM(Clim,c(2006:2080))
clim.clip<-crop(subset(Clim,7500:7530),extent(M))
clim.clip<-calc(clim.clip,mean,na.rm=T)
#plot(clim.clip)
climpts <- rasterToPoints(clim.clip, spatial = TRUE)
dat.clim <- as.data.frame(climpts)
plot(clim.clip)

## get doy and photo values
doys<-extract(M,cbind(dat.clim$x,dat.clim$y))
unique.doys<-sort(unique(doys[which(!is.na(doys))]))

doys.rast<-clim.clip
values(doys.rast)[!is.na(values(doys.rast))]<-doys
plot(doys.rast)

doypts <- as.data.frame(rasterToPoints(doys.rast, spatial = TRUE))
dl<-daylength(doypts$y, doypts$x, doypts$layer,tmz = -6)
photo.rast<-doys.rast
values(photo.rast)[!is.na(values(photo.rast))]<-dl[,3]
plot(photo.rast)

## get climate warming values













