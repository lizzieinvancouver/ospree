# Pieces for code for reading in and pulling data from netCDF files for Dan
# EJ Forrestel
# 20 May 2016

rm(list=ls()) 
options(stringsAsFactors = FALSE)

require('maptools')
require('ncdf4')
require('abind')
require('raster')
require('maptools')
require('sp')
require('foreach')
require('data.table')
require('gstat')
require('lubridate')
require('ff')
require('rgr')
setwd("/Volumes/LIL4/NCAR_final/climatefuture")


##reading in netCDF files
gcm.tempmn <- nc_open("b.e11.BRCP85C5CNBDRD.f09_g16.001.cam.h1.TREFHTMN.20060101-20801231.nc")
gcm.tempmx <- nc_open("b.e11.BRCP85C5CNBDRD.f09_g16.001.cam.h1.TREFHTMN.20810101-21001231.nc")
landfrac.nc <- nc_open("landfrac/b.e11.BRCP85C5CNBDRD.f09_g16.017.clm2.h0.QRUNOFF.208101-210012.nc")

##pulling relevant variables from the netCDF
landfrac <- ncvar_get(landfrac.nc,"landfrac")
lon <- ncvar_get(landfrac.nc,"lon")
lat <- ncvar_get(landfrac.nc,"lat")

###reading in netCDF as raster files
##reading in raster files of relevant layers
landfrac.r <- raster("landfrac/b.e11.BRCP85C5CNBDRD.f09_g16.017.clm2.h0.QRUNOFF.208101-210012.nc",varname="landfrac")
##reading in land fraction as a raster and rotating to make it match BEST data orientation; from (0,360) to (-180,180)
landfrac.r <- rotate(landfrac.r)
#image(landfrac.r)

###getting specific days and years from netCDF files; indexing
yrsdata <- 1951:2000
days <- ncvar_get(gcm.tempmn,"time")
landtemp <- ncvar_get(gcm.tempmn,'TREFHTMN',start=c(1,1,1),count=c(length(lon),length(lat),1))
start_date <- ncvar_get(gcm.tempmn,"nbdate")
start_year <- as.numeric(strsplit(as.character(ymd(start_date)),split='-',fixed=T)[[1]][1])

##finding cell in climate data closest to given coordinate
diff.long.cell <- abs(tempmax1$Var1-coords[i,'long'])
diff.lat.cell <- abs(tempmax1$Var2-coords[i,'lat'])
long.cell <- which(diff.long.cell==min(diff.long.cell))
lat.cell <- which(diff.lat.cell==min(diff.lat.cell))
coord_row <- intersect(long.cell,lat.cell)
   		
