#Checking for anomolies in climate data
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
library(Interpol.T)
library(chillR)

# 1. Get the data
d <- read.csv("output/ospree_clean.csv")

# 2. Clean the chilltemp column

source("chilling/cleaning_chilltemp.R")

#3. Clean the provenance.latitude and provenance.longitude columns, to get appropriate locations
source("chilling/cleaning_provlatlong.R")

# 4. Estimate field chilling (using growing or provenance lat/long to pull climate data)- REQUIRES EXTERNAL HARD DRIVE FOR THIS

# 4a: summarize lat/longs needed to pull climate data from europe and north america
source("chilling/fieldchillcalc_latlong.R")

load("output/fieldclimate.RData")
tempval_all <- do.call("rbind", tempval)
tempval_all<-cbind(row.names(tempval_all),tempval_all)
row.names(tempval_all)<-NULL
colnames(tempval_all)[1]<-"ID_fieldsample.date2"

#plot Tmax vs Tmin to see if there are any mistakes
plot(tempval_all$Tmin,tempval_all$Tmax)
abline(0,1)
#everything looks like its' above the 1:1 line, but I'll check another way just to be sure:
tempdif<-tempval_all$Tmax-tempval_all$Tmin#should always be greater than 0
which(tempdif<0)#none are greater than 0
