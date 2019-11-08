### Prepare all data for climate data...
### 7 June 2019 - Cat
## based off of betpen_chillandgdd_tg.R but using Tmin and Tmax now to find Tmean

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(chillR)
library(egg)
library(raster)
library(RColorBrewer)

setwd("~/Documents/git/ospree/analyses/bb_analysis/PEP_climate")
d<-read.csv("input/pep_betpen_all.csv", header=TRUE)

df<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950 & YEAR<=2010)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df<-dplyr::select(df, year, PEP_ID, lat, long, lo)

df<-df[!duplicated(df),]

x<-paste(df$year, df$lo)
df$date<-as.Date(strptime(x, format="%Y %j"))
df$Date<- as.character(df$date)
df$lat.long <- paste(df$lat, df$long)
allpeps <- df[(df$year>=1951 & df$year<=1960) | (df$year>=2001 & df$year<=2010),]

allpeps$cc<-ifelse(allpeps$year>=1950 & allpeps$year<=1960, "apre", "post")
allpeps$num.years<-ave(allpeps$year, allpeps$lat.long, FUN=length)
mostdata<-allpeps[(allpeps$num.years>=20),]
tt<-as.data.frame(table(mostdata$cc, mostdata$lat.long))
tt<-tt[!(tt$Freq==0),]
bestsites<-as.data.frame(table(tt$Var2))
bestsites<-bestsites[(bestsites$Freq>1),]
bestsites <- bestsites$Var1

allpeps.subset<-mostdata[(mostdata$lat.long %in% bestsites),]

rn<-brick("~/Desktop/Big Data Items/tn_0.25deg_reg_v16.0.nc", sep="")
rx<-brick("~/Desktop/Big Data Items/tx_0.25deg_reg_v16.0.nc", sep="")

##### Now to calculate chilling using Chill portions based on Ailene's code `chillcode_snippet.R' #####
## Adjust the period you are using below to match the function you want to use (i.e. extractchillpre or extractchillpost)
period<-1951:1960
#period<-2001:2010
sites<-subset(allpeps.subset, select=c(lat, long, lat.long))
sites<-sites[!duplicated(sites$lat.long),]
badsites<-c("54.5 11.1", "49.7667 11.55", "47.8 11.0167") 
sites<-sites[!(sites$lat.long%in%badsites),]
sites$x<-sites$long
sites$y<-sites$lat
nsites<-length(sites$lat.long)
sites$siteslist<-1:45
tmin<-rn
tmax<-rx

lositeyear <- subset(allpeps.subset, select=c("lo", "lat", "long", "lat.long", "year"))
lositeyear <- lositeyear[!duplicated(lositeyear),]
lositeyear <- left_join(lositeyear, sites)
lositeyear<-na.omit(lositeyear)

leaps<-c(1952, 1956, 1960, 2000, 2004, 2008)

bb<-sites
bb$lat.long<-paste(bb$lat, bb$long, sep=",")
bb<-bb[!duplicated(bb$lat.long),]
lats <- bb$lat
lons <- bb$long

coords <- data.frame(x=lons,y=lats)

coords<- na.omit(coords)

points <- SpatialPoints(coords, proj4string = rn@crs)

values <- extract(rn,points)

dclim <- cbind.data.frame(coordinates(points),values)

dx<-reshape2::melt(dclim, id.vars=c("x","y"))

dx<-dx%>%
  dplyr::rename(long=x)%>%
  dplyr::rename(lat=y)%>%
  dplyr::rename(date=variable)%>%
  dplyr::rename(Tmin=value)

dx$date<-substr(dx$date, 2,11)
dx$Date<- gsub("[.]", "-", dx$date)

sites<-dplyr::select(sites, lat, long, siteslist)
dx<-dplyr::select(dx, -date)

tmindata<-inner_join(dx, sites)

## now for tmax
pointsmax <- SpatialPoints(coords, proj4string = rx@crs)

valuesmax <- extract(rx,pointsmax)

dclimmax <- cbind.data.frame(coordinates(pointsmax),valuesmax)

dxmax<-reshape2::melt(dclimmax, id.vars=c("x","y"))

dxmax<-dxmax%>%
  dplyr::rename(long=x)%>%
  dplyr::rename(lat=y)%>%
  dplyr::rename(date=variable)%>%
  dplyr::rename(Tmax=value)

dxmax$date<-substr(dxmax$date, 2,11)
dxmax$Date<- gsub("[.]", "-", dxmax$date)

dxmax<-dplyr::select(dxmax, -date)

tmaxdata<-inner_join(dxmax, sites)

tempdata <- left_join(tmaxdata, tmindata)
tempdata$temp <- (tempdata$Tmax + tempdata$Tmin)/2
tempdata$year <- as.numeric(substr(tempdata$Date, 0, 4))
tempdata$month <- as.numeric(substr(tempdata$Date, 6, 7))
tempdata$day <- as.numeric(substr(tempdata$Date, 9, 10))
tempdata$yday <- yday(tempdata$Date)
alltemps <- subset(tempdata, select=c("Date", "year", "yday", "day", "month", "temp", "siteslist"))
names(alltemps)<-c("date", "year", "yday", "day", "month", "temp", "spatial")
alltemps$date <- as.character(alltemps$date)
climatedatapre <- alltemps[(alltemps$year>=1950 & alltemps$year<=1961),]
climatedatapost <- alltemps[(alltemps$year>=2000 & alltemps$year<=2011),]

setwd("~/Documents/git/ospree/analyses/bb_analysis/pep_sims/simmonds_slidingwin")
write.csv(climatedatapre, file="input/bp_climatedatapre.csv", row.names=FALSE)
write.csv(climatedatapost, file="input/bp_climatedatapost.csv", row.names=FALSE)


