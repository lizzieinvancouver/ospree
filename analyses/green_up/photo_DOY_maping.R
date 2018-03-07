rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(raster)
library(sp)
library(rgdal)
#library(insol)
library(googleway)
library(geosphere)
setwd("~/Documents/git/ospree/analyses/green_up")

####load data
M<-raster("MCD12Q2.005_Onset_Greenness_Increase_0_doy2009001_aid0001.tif") ### This is modis file for 2001
#M2<-raster("MCD12Q2.005_Onset_Greenness_Increase_1_doy2009001_aid0001.tif")
###measured in days since 1/1/2000
#plot(M)
#plot(M2)#I think this is areas in the south that green up before Dec 31
projection(M)
###make all the lat longs points
spts <- rasterToPoints(M, spatial = TRUE)
###make it a data frame
dat <- as.data.frame(spts)
dat$Day<-as.Date(dat$MCD12Q2.005_Onset_Greenness_Increase_0_doy2009001_aid0001,origin = "2000-01-01")
head(dat)
long<-c(dat$x)
lat<-c(dat$y)
names(dat)
DOY<-c(dat$Day)

#calculate day length in geosphere
dl2<-geosphere::daylength(c(lat),c(DOY))


head(dl2)
dl2<-as.data.frame(dl2)

###select day length
dl<-dplyr::select(dl,daylen)

###merge back with data (assume they are indexed properly but I dont know)
#gooddat<-cbind(dl,dat) #insol
gooddat2<-cbind(dl2,dat) #geosphere
#to make it a raster drop green up variable (si.x.averages etc)

d2<-dplyr::select(gooddat2,x,y, dl2) #geosphere
###convert back to raster

dfr2<-rasterFromXYZ(d2) #geosphere
##view it
par(mar=c(1,1,1,1))

plot(dfr2) #geosphere
#hmmm, geosphere and insol give different values for days length
#how to trouble shoot this

#trouble shooting: check comparison for first row in dat
geosphere::daylength(49.41667,121)
##They are indeed making different calculations-what is true?
#check what day length seems more realisitc for DOY
library(chron)
month.day.year(121)
###geosphere seems more likely because calander day of 121 is 2 May, not close to the equinox 
#so ~14 hrs seems approximately more realisitic than ~12-- any other ideas how to check?