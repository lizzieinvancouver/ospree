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
source("chilling/cleaning_provlatlong.R")

# 4. Pull daily min and max temperature for all sites with fieldsampledates (using growing or provenance lat/long to pull climate data)- REQUIRES EXTERNAL HARD DRIVE FOR THIS

# 4a: summarize lat/longs needed to pull climate data from europe and north america
source("chilling/fieldchillcalc_latlong.R")

# 4b: Set the location of the external hard drive, then pull daily climate data for Europe and North America
#Skip head to 4e if you do not have the climate data drive
climatedrive = "/Volumes/Ospree Climate" # (Ospree Climate is name of the external drive, change with new device)
# climatedrive =  "/Volumes/BackYouEvilFiend/ospreeclimate" # Lizzie's backup drive (at WeldHill currently)
# climatedrive = "/Volumes/My Book for Mac/ospreeclimate" # Lizzie's backup drive (at HUH currently)

# 4c. pull climate data from europe
source("pmp/pullclimate_eur_pmp.R")

# 4d: pull climate data from north america
source("pmp/pullclimate_nam_pmp.R")
# July 2017: Ailene gets an error that I'm not sure about:
#In addition: Warning message:
#Too many values at 228293 locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ... 
#dim(dailytemp3)#228293      7
#depsite the warning, the files appear to be in good shape...hopefuly they pilled the right climate data!



#write a csv file of the daily Tmin and Tmax data
write.csv(dailytemp3, "output/dailytemp.csv", row.names=FALSE, eol="\r\n")

#check code for 1950s studies (ashby)