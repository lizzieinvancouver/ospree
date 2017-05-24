## Estimate total chilling and add to the OSPREE data ##
## By Ailene Ettinger, Dan Flynn, Lizzie and more! ###

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

# 4b: Set the location of the external hard drive, then pull daily climate data for Europe and North America
#Skip this step if you don't have the climate data drive
climatedrive = "/Volumes/Ospree Climate" # (Ospree Climate is name of new external drive, change with new device)
# climatedrive =  "/Volumes/BackYouEvilFiend/ospreeclimate"
# climatedrive = "/Volumes/My Book for Mac/ospreeclimate"

# 4c. pull climate data from europe
source("chilling/pullclimate_eur.R")

# 4d: pull climate data from north america
source("chilling/pullclimate_nam.R")

# 4e: Interpolate hourly temperatures from the daily values 
# & chilling using three different metrics
#(If you want to avoid connecting to the external hard drive, then start here)
dat<-read.csv("output/tempval_all.csv")
source("chilling/interpolclimate.R")

# 5. Calculate experimental chilling and merge in field chilling estimates with experimental chilling 
#(If you want to avoid interpolating hourly climate and calculating field chilling, then start here)
dat<-read.csv("output/fieldchillcalcslatlong.csv")

source("chilling/totalchillcalc.R")

# 6. Write out the file with total chilling estimates- (I have not done this yet!)

write.csv(dat4, "output/ospree_clean_withchill.csv", row.names=FALSE) ##

dim(dat4)
