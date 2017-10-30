## Pull climate data for use in PMP by Lizzie 
# By Ailene Ettinger
# June 14, 2017
## This code is based on the chillmerge_all.R code, and the first 4 steps are sourced to the chilling folder where cleaning occurs


## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
    (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
setwd("~/Documents/git/ospree/analyses")

# Load libraries
library(dplyr)
library(tidyr)
library(plyr)
library(ncdf4)


# 1. Get the data
d <- read.csv("output/ospree_clean.csv")

# 2. Clean the chilltemp column

source("chilling/cleaning_chilltemp.R")

#3. Clean the provenance.latitude and provenance.longitude columns, to get appropriate locations
#source("chilling/cleaning_provlatlong.R")
#This step was moved to the cleaning folder

# 4. Pull daily min and max temperature for all sites with fieldsampledates (using growing or provenance lat/long to pull climate data)- REQUIRES EXTERNAL HARD DRIVE FOR THIS

# 4a: summarize lat/longs needed to pull climate data from europe and north america
source("chilling/fieldchillcalc_latlong.R")

# 4b: Set the location of the external hard drive, then pull daily climate data for Europe and North America
#Skip head to 4e if you do not have the climate data drive
climatedrive = "/Volumes/Ospree Climate" # (Ospree Climate is name of the external drive, change with new device)
# climatedrive =  "/Volumes/BackYouEvilFiend/ospreeclimate" # Lizzie's backup drive (at WeldHill currently)
# climatedrive = "/Volumes/My Book for Mac/ospreeclimate" # Lizzie's backup drive (at HUH currently)
# climatedrive="/Volumes/WeldShare/Wolkovich\ Lab/Budburst\ Review\ -\ Ospree/Climate\ Data/" ##access from Dan's comp
#climatedrive = "/Volumes/climate" #Ailene's climate data drive

# 4c. pull climate data from europe
source("bb_dailyclimate/pulldailyclimate_eur.R")

# 4d: pull climate data from north america
#If  just looking at nam climate, do this:
#tempval <- list() 
source("bb_dailyclimate/pulldailyclimate_nam.R")

#4e. If you want to avoid connecting to the external hard drive, then just do this:
#load this .RData workspace)
#load("output/fieldclimate_pmp.RData")
#dailytemp <- do.call("rbind", tempval)
dailytemp<-as.data.frame(cbind(row.names(dailytemp),dailytemp))
colnames(dailytemp)[1]<-"ID_fieldsample.date2"
dailytemp2<-separate(data = dailytemp, col = ID_fieldsample.date2, into = c("datasetID", "lat","long","fieldsample.date2"), sep = "\\_")
row.names(dailytemp2)<-NULL
dailytemp4<-subset(dailytemp2,select=c(datasetID,lat,long,fieldsample.date2,Date,Tmin,Tmax))


# July 2017: You will get a warning message: In addition: Warning message:
#Too many values at 228293 locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ... 
#this is ok- it is just getting rid of some extra stuff that is not needed as a column in the resulting "dailytemp3.csv" file
#write a csv file of the daily Tmin and Tmax data
write.csv(dailytemp3, "output/dailytemp.csv", row.names=FALSE, eol="\r\n")

#check code for 1950s studies (ashby)
#dailytemp3<-read.csv("output/dailytemp.csv")
#head(dailytemp3)
#ashby<-dailytemp3[dailytemp3$datasetID=="ashby62",]
#looks fine, though no climate data exists for  1957= all NAs after March 1957
#check code for swartz81
#swartz<-dailytemp3[dailytemp3$datasetID=="swartz81",]
#seems to be pulling correctly, but no climate data present after june 28, 1980 for this site.
#check how many other sites are mising data
#length(which(is.na(dailytemp3$Tmin)))#26116, which is 11% of all data
#unique(dailytemp3$datasetID[which(is.na(dailytemp3$Tmin))])#19/50 sites are missing some temperature data
#Make list of the studies that are missing large amounts of Temp data
dailytemp4$missingT<-0
dailytemp4$missingT[which(is.na(dailytemp4$Tmin))]<-1
temptab<-table(dailytemp4$datasetID,dailytemp4$missingT)
missingtemp<-temptab[temptab[,2]>0,]
#hmm...all NAM sites are missing large amounts of data, plus one european site (heide93)
#calme94<-dailytemp3[dailytemp3$datasetID=="calme94",]
#head(calme94)
#tail(calme94)
