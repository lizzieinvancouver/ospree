### Prepare all data for climate data...
### 20 November 2018

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
library(raster)
library(reshape2)
library(data.table)

setwd("~/Documents/git/ospree/analyses/bb_analysis/PEP_climate")
#d<-read.csv("/n/wolkovich_lab/Lab/Cat/PEP725_DE_Betpen.csv", header=TRUE)
gersites<-read.csv("input/PEP725_DE_stations.csv", header=TRUE)
d<-read.csv("input/bbch_region_betpen.csv", header=TRUE)

peps<-as.vector(gersites$PEP_ID)
d<-subset(d, d$PEP_ID %in% peps)

df<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950 & YEAR<1980)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df<-dplyr::select(df, year, PEP_ID, lat, long, lo)

df<-df[!duplicated(df),]

sites<-sample(unique(df$PEP_ID), 100)
df<-df[(df$PEP_ID %in% sites),]
x<-paste(df$year, df$lo)
df$date<-as.Date(strptime(x, format="%Y %j"))
df$Date<- as.character(df$date)

df.post<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1980& YEAR<2017)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df.post<-dplyr::select(df.post, year, PEP_ID, lat, long, lo)

df.post<-df.post[!duplicated(df.post),]

#sites<-read.csv("output/betpen_sites.csv", header=TRUE)
df.post<-df.post[(df.post$PEP_ID %in% sites),]
x<-paste(df.post$year, df.post$lo)
df.post$date<-as.Date(strptime(x, format="%Y %j"))
df.post$Date<- as.character(df.post$date)


#r<-brick("/n/wolkovich_lab/Lab/Cat/Big Data Items/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")
r<-brick("~/Desktop/Big Data Items/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")

bb<-df
bb$lat.long<-paste(bb$lat, bb$long, sep=",")
bb<-bb[!duplicated(bb$lat.long),]
lats <- bb$lat
lons <- bb$long

coords <- data.frame(x=lons,y=lats)

coords<- na.omit(coords)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

dclim <- cbind.data.frame(coordinates(points),values)

dx<-melt(dclim, id.vars=c("x","y"))

dx<-dx%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tavg=value)

dx$date<-substr(dx$date, 2,11)
dx$Date<- gsub("[.]", "-", dx$date)

df$date<-NULL
dx$date<-NULL

dx$month<-substr(dx$Date, 6, 7)
dx$month<-as.numeric(dx$month)


dx$spring<-ifelse(dx$month>=2 & dx$month<=4, "spring", 0)
ddx<-dx[(dx$spring=="spring"),]
ddx<-ddx[!is.na(ddx$Tavg),]

ddx$year<-as.numeric(substr(ddx$Date, 0, 4))
ddx$lat.long<-paste(ddx$lat, ddx$long)
ddx$mat<-ave(ddx$Tavg, ddx$year, ddx$lat.long)

mst<-ddx%>%dplyr::select(-Tavg, -Date, -spring, -month)
mst<-mst[!duplicated(mst),]

mst$num.years<-ave(mst$mat, mst$lat.long, FUN=length)

preCC <- mst[(mst$year>=1950 & mst$year<=1960), ]
postCC <- mst[(mst$year>=2000 & mst$year<=2010), ]

df$lat.long<-paste(df$lat, df$long)
bb<-df
bb$PEP_ID<-NULL
bb$lat<-NULL
bb$long<-NULL
bb$Date<-NULL

preCC<-full_join(preCC, bb)
preCC<-preCC[!is.na(preCC$lo),]
preCC<-preCC[(preCC$year>=1950 & preCC$year<=1960),]
preCC$num.years<-ave(preCC$mat, preCC$lat.long, FUN=length)





df.post$lat.long<-paste(df.post$lat, df.post$long)
bb.post<-df.post
bb.post$PEP_ID<-NULL
bb.post$lat<-NULL
bb.post$long<-NULL
bb.post$Date<-NULL
bb.post<-bb.post[(bb.post$year>=2000 & bb.post$year<=2010),]

postCC$lat<-NULL
postCC$long<-NULL
postCC$num.years<-NULL

postCC<-full_join(postCC, bb.post)
postCC<-postCC[!is.na(postCC$lo),]
postCC<-postCC[(postCC$year>=2000 & postCC$year<=2010),]
postCC$num.years<-ave(postCC$mat, postCC$lat.long, FUN=length)



#write.csv(mst, file="output/betpen_mat_pre.csv", row.names = FALSE)
write.csv(sites, file="output/betpen_sites.csv", row.names = FALSE)


