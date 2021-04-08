# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
setwd("~/Desktop/green_up_original 2")
library(raster)
library(sp)
library(rgdal)
library(geosphere)
library(dplyr)

M<-"MCD12Q2.006_Greenup_0_doy2009001_aid0001.tif"


M<-raster(M) ###
###make the raster more coarse
resampleFactor <- 25        
inputRaster <- raster(M)      
inCols <- ncol(inputRaster)
inRows <- nrow(inputRaster)
resampledRaster <- raster(ncol=(inCols / resampleFactor), nrow=(inRows / resampleFactor))
extent(resampledRaster) <- extent(inputRaster)
resampledRaster <- resample(M,resampledRaster,method='bilinear',overwrite=TRUE)

spts <- rasterToPoints(resampledRaster, spatial = TRUE)
###make it a data frame
dat <- as.data.frame(spts)
colnames(dat)[1]<-"source"
dat$Day<-as.Date(dat$source,origin = "1970-01-01")

library(lubridate)
dat$doy<-yday(dat$Day) 
dat.stv<-filter(dat,doy>=60)
dat.stv<-filter(dat,doy<=121)
dat.stv2<-filter(dat,doy>=90)
dat.stv2<-filter(dat,doy<=151)


head(dat)
dat.stv<-select(dat.stv,x,y,doy)
dat<-select(dat,x,y,doy)

dat.eu<-filter(dat,x>=-15)
dat.eu<-filter(dat.check,y>=35)

dat.na<-filter(dat,x<=-15)
dat.na<-filter(dat.na,y>=15)

dat.stv.eu<-filter(dat.stv,x>=-15)
dat.stv.eu<-filter(dat.stv.eu,y>=35)

dat.stv.na<-filter(dat.stv,x<=-15)
dat.stv.na<-filter(dat.stv.na,y>=15)

dat.stv2.na<-filter(dat.stv2,x<=-15)
dat.stv2.na<-filter(dat.stv2.na,y>=15)

nA<-rasterFromXYZ(dat.na)
stv.nA<-rasterFromXYZ(dat.stv.na)
stv2.nA<-rasterFromXYZ(dat.stv2.na)

EU<-rasterFromXYZ(dat.eu)
stv.eu<-rasterFromXYZ(dat.stv.eu)





pdf("~/Documents/git/ospree/analyses/ranges/figures/whystvbad.pdf")
par(mar=c(1,1,1,1))
par(mfrow=c(2,3))
plot(EU,col="black",legend=FALSE)
text(0,75,"STV: March-May",cex=1,font=2)
plot(nA,col="black",legend=FALSE)
text(x=-120,y=100,"STV: March-May",cex=1,font=2)
plot(nA,col="black",legend=FALSE)
text(x=-120,y=100,"STV: April-June",cex=1,font=2)
plot(stv.eu,col="darkgreen",legend=FALSE)
text(x=0,y=75,labels="78%")
plot(stv.nA,col="darkgreen",legend=FALSE)
text(x=-120,y=100,labels="37%")
plot(stv2.nA,col="darkgreen",legend=FALSE)
text(x=-120,y=100,labels="77%")

dev.off()
nrow(dat.stv.eu)/nrow(dat.eu) #.78
nrow(dat.stv.na)/nrow(dat.na) #.37
nrow(dat.stv2.na)/nrow(dat.na) #.775

