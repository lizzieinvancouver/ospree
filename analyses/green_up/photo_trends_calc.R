## Started 15 Oct 2020 ##
## Getting photoperiod trends at green-up out of maps
## of photoperiod generated with the photo_DOY_maping.R code from Dan
##    
## by Nacho ##


rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(raster)
library(sp)
library(rgdal)
#library(insol)
#library(googleway)
library(geosphere)
#setwd("~/Documents/git/ospree/analyses/green_up/greenup_files/")
#setwd("~/Data_Harvard/green_up/greenness copy/")
setwd("~/GitHub/ospree/analyses/green_up/greenup_files/")





## run regression across rasters

photomaps<-dir()
mystack <- raster::stack(photomaps[1:10])

## function to compute linear regression across years and store coefficients
## the function is set so that only pixels with at least 8 measurements are 
## used to compute regressions
funreg <- function(x) { if (sum(is.na(x))>2){ NA } else {lm(c(x) ~ c(1:10))$coefficients[2] }} 

## you can apply similar functions to store standard deviations
## of the coefficients
funregsds <- function(x) { if (sum(is.na(x))>2){ NA } else {summary(lm(c(x)~c(1:10)))$coefficients[2,2]}} 



## apply function across stack
phototrend <- calc(mystack, funreg)
phototrendsds <- calc(mystack, funregsds)


## explore data
plot(phototrend)
plot(phototrendsds)
hist(values(phototrend),30)
hist(values(phototrendsds),30)


## since there are some extreme values (+/-24, which is weird but affects
## to few pixels and probably has to do with pixels that have many NAs and 
## extreme values). We can remove values beyond +/-2.4, which would correspond
## to shifts of 24 hours of photoperiod across the 10 year-period examined. 
## This is, a pixel with +2.4 would have gone from a photoperiod of 0 to a
## photoperiod of 24 during that period.
## 
vec<-values(phototrend)
vec[vec<(-2.4)]<-NA
vec[vec>2.4]<-NA
corr.phot<-phototrend
values(corr.phot)<-vec
plot(corr.phot)

writeRaster(phototrend,filename = "Photo_trend.tif",
            format="GTiff",overwrite=T)
writeRaster(corr.phot,filename = "Photo_trend24.tif",
            format="GTiff",overwrite=T)

## in case you want it
writeRaster(phototrendsds,filename = "Photo_trendsds.tif",
            format="GTiff",overwrite=T)
