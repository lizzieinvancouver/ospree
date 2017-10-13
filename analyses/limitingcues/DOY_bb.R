# Find the DOY for each species across lat/long sites
## Using PEP data for Limiting Cues paper
# 13 October 2017 - Cat

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("~/Documents/git/ospree/analyses/limitingcues/input")
bet<-read.csv("PEP_betpen.csv", header=TRUE)
fsyl<-read.csv("PEP_fagsyl.csv", header=TRUE)

bet<-bet%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(bet$YEAR, bet$DAY)
bet$date<-as.Date(strptime(x, format="%Y %j"))
bet$year<-as.numeric(substr(bet$date, 0,4))
bet$month<-as.numeric(substr(bet$date, 6, 7))
bet$day<-as.numeric(substr(bet$date, 9,10))
bet$prov<-paste(bet$lat, bet$long)
bet <- bet[order(bet$prov, bet$date), ]

bet$grow<-ifelse(is.na(bet$BBCH), NA, TRUE)
bet$count <- ave(
  bet$grow, bet$PEP_ID, bet$year,
  FUN=function(x) cumsum(c(1, head(x, -1)))
)

bet$count<-as.numeric(as.character(bet$count))
bet<-filter(bet, count==1)
bet$year<-bet$YEAR
bet$bb<-bet$DAY

d<-dplyr::select(bet, year, BBCH, bb, lat, long, species, date)


