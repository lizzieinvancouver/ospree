### Another version of a climate check 
# Checking on rasters individually and plotting directly
# 20 November 2019 - Cat

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")


# if they are not already loaded
library(rgdal)
library(raster)
library(geosphere)
library(ncdf4)
library(Interpol.T)
library(lubridate)

# set working directory to ensure R can find the file we wish to import
# setwd("working-dir-path-here")

set.seed(1234)
coords <- as.data.frame(randomCoordinates(100000))
coords <- coords[(coords$lon>=-125 & coords$lon<=-60 &
                    coords$lat>=30 & coords$lat<=50),]


nam <- data.frame(lon=as.numeric(coords$lon),
                  lat=as.numeric(coords$lat), 
                  year=as.numeric(sample(1955:2013, 2400, replace=TRUE)))

noyrs <- c(1960, 1964:1970, 1975:1978, 1986:1990, 1999, 2002, 2005)

nam <- nam[!(nam$year%in%noyrs),]
nam <- nam[sample(nrow(nam), 1000), ]

nam$id <- paste(nam$lat, nam$lon, nam$year)

#period <- c(1951:1960, 2001:2010)
yr <- 2008
nam$x<-nam$lon
nam$y<-nam$lat
Coords<-subset(nam, select=c(x, y))
#nsites<-length(sites$lat.long)

climatedrive = "/Volumes/climdata" # Cat's climate drive
nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]

#yr <- nam$year

tmax <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",yr), full.names = TRUE)
tmin <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",yr), full.names = TRUE)

par(mfrow=c(1,2))
#prince <- plot(rn[[1]], ylim=c(20, 60), xlim=c(230, 290))
#liv <- plot(brick(file, varname="Tmin", sep="")[[1]])

rn <- brick(tmin, varname="tmin", sep="")
rx <- brick(tmax, varname="tmax", sep="")

rn <- rotate(rn)
rx <- rotate(rx)

points.min <- SpatialPoints(Coords, proj4string = rn@crs)
points.max <- SpatialPoints(Coords, proj4string = rx@crs)

#names(rn) <- seq(1, 366, by=1)
#rnjune<- raster(subset(rn, names(rn)[c(152:181)]))

#names(rx) <- seq(1, 366, by=1)
#rxjune<- subset(rx, names(rx)[c(152:181)])

valuesmin <- raster::extract(rn,points.min)
valuesmax <- raster::extract(rx,points.max)

dclimmin <- cbind.data.frame(coordinates(points.min),valuesmin)
dclimmax <- cbind.data.frame(coordinates(points.max),valuesmax)

library(reshape2)
dxmin<-melt(dclimmin, id.vars=c("x","y"))
dxmax<-melt(dclimmax, id.vars=c("x","y"))

library(dplyr)
dxmin<-dxmin%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(doy=variable)%>%
  rename(Tmin=value)

dxmax<-dxmax%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(doy=variable)%>%
  rename(Tmax=value)

prince <- data.frame(lat=dxmin$lat, long=dxmin$long, date=dxmin$doy, tmin=dxmin$Tmin, tmax=dxmax$Tmax)


#### Now for livneh:
nafiles <- dir(climatedrive)[grep("livnehclimdata", dir(climatedrive))]
file <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0(200806), full.names = TRUE)

liv <- plot(brick(file, varname="Tmin", sep="")[[1]])

rn <- brick(file, varname="Tmin", sep="")
rx <- brick(file, varname="Tmax", sep="")

points.min <- SpatialPoints(Coords, proj4string = rn@crs)
points.max <- SpatialPoints(Coords, proj4string = rx@crs)

valuesmin <- raster::extract(rn,points.min)
valuesmax <- raster::extract(rx,points.max)

dclimmin <- cbind.data.frame(coordinates(points.min),valuesmin)
dclimmax <- cbind.data.frame(coordinates(points.max),valuesmax)

library(reshape2)
dxmin<-melt(dclimmin, id.vars=c("x","y"))
dxmax<-melt(dclimmax, id.vars=c("x","y"))

library(dplyr)
dxmin<-dxmin%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(doy=variable)%>%
  rename(Tmin=value)

dxmax<-dxmax%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(doy=variable)%>%
  rename(Tmax=value)

liv <- data.frame(lat=dxmin$lat, long=dxmin$long, date=dxmin$doy, tmin=dxmin$Tmin, tmax=dxmax$Tmax)



princetx <- raster(tmax)
foo <- raster(princetx, layer=c(152:242))
plot(princetx$Maximum.air.temperature-273.15)

plot(foo$Maximum.air.temperature-273.15)

jx <- nc_open(tmax)
jn <- nc_open(tmin)

endday <- strptime(paste(yr, "06-30", sep="-"),"%Y-%m-%d", tz = "GMT")
doyend <- yday(endday)
stday <- strptime(paste(yr, "06-01", sep="-"),"%Y-%m-%d", tz="GMT")
doyst <- yday(stday)

leaps<-c(1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988,
         1992, 1996, 2000, 2004, 2008, 2012, 2016)

if(yr%in%leaps){
  yrend <- 366
} else{
  365
}

jx$dim$time$vals<-seq(1, yrend, by=1)
thisyr <- which(jx$dim$time$vals<=doyend)
thisyr <- which(thisyr>=doyst)

lo <- nam[,"lon"]
la <- nam[,"lat"]

long.cell <- which.min(abs(jx$dim$lon$vals-as.numeric(lo)))
lat.cell <- which.min(abs(jx$dim$lat$vals-as.numeric(la)))

maxs<-as.matrix(ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1))[thisyr]-273.15)
princemaxs<-raster(maxs)

plot(princemaxs)

j <- paste0(yr, "06")
tmaxliv <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0(j), full.names = TRUE)


# import raster
DSM_HARV <- raster("NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")