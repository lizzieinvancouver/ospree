rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(raster)
library(sp)
library(rgdal)
library(insol)
library(googleway)
library(geosphere)
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
head(lat)
head(DOY)
###arbritary time zone (I chose central) this seems to effect only sunrise sun set, not day length which is what we are interested in
zone<-6

###for examp
daylength(49.41667,-95.12500, 121,tmz =-5)
daylength(49.41667,-95.12500, 121,tmz =-6)
daylength(49.41667,-95.12500, 121,tmz =-10)
### all produce same day length

#calculate day length in insol
dl<-daylength(c(lat), c(long), c(DOY),tmz = -6)
dl<-as.data.frame(dl)
c(lat)
#calculate day length in geosphere
dl2<-geosphere::daylength(c(lat),c(DOY))


head(dl2)
dl2<-as.data.frame(dl2)

###select day length
dl<-dplyr::select(dl,daylen)

###merge back with data (assume they are indexed properly but I dont know)
gooddat<-cbind(dl,dat) #insol
gooddat2<-cbind(dl2,dat) #geosphere
#to make it a raster drop green up variable (si.x.averages etc)
d<-dplyr::select(gooddat,x,y, daylen) #insol
d2<-dplyr::select(gooddat2,x,y, dl2) #geosphere
###convert back to raster
dfr <- rasterFromXYZ(d) #insol
dfr2<-rasterFromXYZ(d2) #geosphere
##view it
par(mar=c(1,1,1,1))
plot(dfr)+title("Day length on day of Spring Index") #insol
plot(dfr2)+title("Day length with geosphere") #geosphere
#hmmm, geosphere and insol give different values for days length
#how to trouble shoot this

#trouble shooting: check comparison for first row in dat
geosphere::daylength(49.41667,121)
insol::daylength(49.41667,-95.12500,121,-6)
##They are indeed making different calculations-what is true?
#check what day length seems more realisitc for DOY
library(chron)
month.day.year(121)
###geosphere seems more likely because calander day of 121 is 2 May, not close to the equinox 
#so ~14 hrs seems approximately more realisitic than ~12-- any other ideas how to check?