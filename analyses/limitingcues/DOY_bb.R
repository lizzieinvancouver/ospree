## Estimate the DOY for each species across lat/long sites ##
## Using PEP data for Limiting Cues paper ##
## Updates by Lizzie ##

## Lizzie worked off some code from projects/misc/pep725/pep725spp.R ##

## Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

## Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/limitingcues") 
} else
  setwd("~/Documents/git/ospree/analyses/limitingcues")

betall <- read.csv("input/PEP_betpen.csv", header=TRUE)
fagall <- read.csv("input/PEP_fagsyl.csv", header=TRUE)

## Let's figure out sites and stages data ...
betagg <- aggregate(betall[("YEAR")], betall[c("PEP_ID", "BBCH", "National_ID", "LAT", "LON", "ALT")],
    FUN=length)
fagagg <- aggregate(fagall[("YEAR")], fagall[c("PEP_ID", "BBCH", "National_ID", "LAT", "LON", "ALT")],
    FUN=length)
ggplot(betagg, aes(x=YEAR, fill=as.factor(BBCH))) +
    geom_histogram(alpha=0.2, position="identity")
ggplot(fagagg, aes(x=YEAR, fill=as.factor(BBCH))) +
    geom_histogram(alpha=0.2, position="identity")
# Well, I guess we will go with 11!
nrow(betagg)
nrow(fagagg)
nrow(subset(betagg, YEAR>19))/nrow(betagg)
nrow(subset(fagagg, YEAR>19))/nrow(fagagg)

nrow(subset(betagg, YEAR>9))/nrow(betagg)
nrow(subset(fagagg, YEAR>9))/nrow(fagagg)
# Hmm, I guess we will start with 20 years ...
bet20 <- subset(betagg, YEAR>19)
fag20 <- subset(fagagg, YEAR>19)

# Annoying detour to figure out which info is needed for unique ID
betagg$ID1 <- paste(betagg$PEP_ID, betagg$National_ID)
betagg$ID2 <- paste(betagg$PEP_ID, betagg$National_ID, betagg$LAT, betagg$LON, betagg$ALT)

length(unique(betagg$PEP_ID))
length(unique(betagg$National_ID))
length(unique(betagg$ID1))
length(unique(betagg$ID2))
# PEP_ID seems unique

# Subset the data based on the above for now ... 
bet11 <- subset(betall, BBCH==11)
betuse <- bet11[which(bet11$PEP_ID %in% bet20$PEP_ID),]
fag11 <- subset(fagall, BBCH==11)
faguse <- fag11[which(fag11$PEP_ID %in% fag20$PEP_ID),]

## NEXT! Start work on the hinge model:
# fit hinge to each site, then predict a common year
# plot mean value for each site against common year estimate ... 




## Code from Cat, need to go through ##
## Also need to change bet to betall ... and fag to fagall ##

bet<-betall%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
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

db<-dplyr::select(bet, year, BBCH, bb, lat, long, species, date)

fag<-fag%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(fag$YEAR, fag$DAY)
fag$date<-as.Date(strptime(x, format="%Y %j"))
fag$year<-as.numeric(substr(fag$date, 0,4))
fag$month<-as.numeric(substr(fag$date, 6, 7))
fag$day<-as.numeric(substr(fag$date, 9,10))
fag$prov<-paste(fag$lat, fag$long)
fag <- fag[order(fag$prov, fag$date), ]

fag$grow<-ifelse(is.na(fag$BBCH), NA, TRUE)
fag$count <- ave(
  fag$grow, fag$PEP_ID, fag$year,
  FUN=function(x) cumsum(c(1, head(x, -1)))
)

fag$count<-as.numeric(as.character(fag$count))
fag<-filter(fag, count==1)
fag$year<-fag$YEAR
fag$bb<-fag$DAY

df<-dplyr::select(fag, year, BBCH, bb, lat, long, species, date)

d<- full_join(df, db)

