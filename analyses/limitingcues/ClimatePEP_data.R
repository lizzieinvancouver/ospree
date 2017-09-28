# Extract Climate Data for Limiting Cues Paper
# 28 September 2017 - Cat 

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(Interpol.T)
library(chillR)
library(raster)
library(reshape2)
library(data.table)
library(arm)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else
  setwd("~/Documents/git/ospree/analyses/limitingcues")


rn<-brick("tn_0.25deg_reg_v15.0.nc", sep="")  ## from climate drive or W drive
rx<-brick("tx_0.25deg_reg_v15.0.nc", sep="")
betula<-read.csv("input/PEP_betula.csv", header=TRUE)
fagus<-read.csv("input/PEP_fagus.csv", header=TRUE)

df<- full_join(betula, fagus)
df$lat.long<-paste(df$LAT, df$LONG, sep=",")
df<-df[!duplicated(df$lat.long),]

lats <- df$LAT
lons <- df$LON

coords <- data.frame(x=lons,y=lats)
coords<-na.omit(coords)

points <- SpatialPoints(coords, proj4string = rn@crs)

values <- extract(rn,points)

dx <- cbind.data.frame(coordinates(points),values)

dx<-melt(df, id.vars=c("x","y"))

dx<-dx%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tmin=value)

dx$date<-substr(dx$date, 2,11)
dx$year<-as.numeric(substr(dx$date, 0,4))
dx$month<-as.numeric(substr(dx$date, 6, 7))
dx$day<-as.numeric(substr(dx$date, 9,10))
dx$date<-as.Date(paste(dx$year, dx$month, dx$day, sep="-"))

