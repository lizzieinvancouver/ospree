rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(raster)
library(sp)
library(rgdal)
library(insol)
library(googleway)

setwd("~/Documents/git/ospree/analyses")

####load average spring index 1981-2016
M<-raster("green_up/si-x-average_leaf_prism.tif")
plot(M)
projection(M)
###make all the lat longs points
spts <- rasterToPoints(M, spatial = TRUE)
###make it a data frame
dat <- as.data.frame(spts)

long<-c(dat$x)
lat<-c(dat$y)
DOY<-c(dat$si.x.average_leaf_prism)
###arbritary time zone (I chose central) this seems to effect only sunrise sun set, not day length which is what we are interested in
zone<-6

###for examp
daylength(49.41667,-95.12500, 121,tmz =-5)
daylength(49.41667,-95.12500, 121,tmz =-6)
daylength(49.41667,-95.12500, 121,tmz =-10)
### all produce same day length

#calculate day length
dl<-daylength(c(lat), c(long), c(DOY),tmz = -6)
dl<-as.data.frame(dl)

###select day length
dl<-dplyr::select(dl,daylen)

###merge back with data (assume they are indexed properly but I dont know)
gooddat<-cbind(dl,dat)

#to make it a raster drop green up variable (si.x.averages etc)
d<-dplyr::select(gooddat,x,y, daylen)

###convert back to raster
dfr <- rasterFromXYZ(d)
##view it
plot(dfr)+title("Day length on day of Spring Index")

#It made something, not sure how to do any quality control, but maybe someone else does.