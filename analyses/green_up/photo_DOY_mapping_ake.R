rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(raster)
library(sp)
library(rgdal)
#library(insol)
#library(googleway)
library(geosphere)
setwd("~/Documents/git/ospree/analyses/green_up/greenup_files/")
setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/green_up/greenup_files/")
savemap= FALSE #set to TRUE if you want to create new raster maps to save
files<-list.files(path = "./modis_original_files", full.names = TRUE)
files<-files[grep(".tif",files)]
####load data
for (file in files){
M<-raster(file) ###
###make the raster more coarse
resampleFactor <- 25        
inputRaster <- raster(M)      
inCols <- ncol(inputRaster)
inRows <- nrow(inputRaster)
resampledRaster <- raster(ncol=(inCols / resampleFactor), nrow=(inRows / resampleFactor))
extent(resampledRaster) <- extent(inputRaster)
resampledRaster <- resample(M,resampledRaster,method='bilinear',overwrite=TRUE)
#plot(resampledRaster)

###measured in days since 1/1/2000

###make all the lat longs points
spts <- rasterToPoints(resampledRaster, spatial = TRUE)
###make it a data frame
dat <- as.data.frame(spts)
colnames(dat)[1]<-"source"

dat$Day<-as.Date(dat$source,origin = "2000-01-01")#this does not seem right
head(dat)
long<-c(dat$x)
lat<-c(dat$y)
names(dat)
DOY<-c(dat$Day)
#print(DOY)
#calculate day length in geosphere
dl2<-geosphere::daylength(c(lat),c(DOY))



dl2<-as.data.frame(dl2)

###select day length
#dl<-dplyr::select(dl,daylen)

###merge back with data (assume they are indexed properly but I dont know)
#gooddat<-cbind(dl,dat) #insol
gooddat2<-cbind(dl2,dat) #geosphere
#to make it a raster drop green up variable (si.x.averages etc)

d2<-dplyr::select(gooddat2,x,y, dl2) #geosphere
d3<-dplyr::select(gooddat2,y,x,Day,dl2) #geosphere
colnames(d3)<-c("lat","long","date","daylength")
d3$year<-substr(file,49,52)
date <- as.Date(as.character(paste(d3$year, substr(d3$date,6,10), sep="-"),
                       format="%Y-%b-%d"))
d3$doy <- as.numeric(format(date, "%j"))

doyfilename<-paste("../greenup_dates/greenup",substr(file,49,52),".csv", sep = "")
write.csv(d3,doyfilename, row.names = FALSE)
###convert back to raster

if(savemap == TRUE){dfr2<-rasterFromXYZ(d2) #geosphere
##view it
par(mar=c(1,1,1,1))
  
tiff(filename = paste("output_",basename(file)))
plot(dfr2)
dev.off()}
}


#writeRaster(dfr2,filename = paste("output_",basename(file),sep=''), format="GTiff",overwrite=TRUE)
# above was generating a blank map
 
 #geosphere
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