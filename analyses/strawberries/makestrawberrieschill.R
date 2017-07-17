## Clean the OSPREE data ## but to include strawberry


## Note! We found some errors in chilling entries; all those are cleaned in clean_chilltemp.R ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")

# Load libraries
library(dplyr)
library(tidyr)
library(plyr)

# 1. Get the data
d <- read.csv("input/ospree.csv")

# 2. Need to deal with some basic cleaning, delete a few extraneous columns
d$X <- NULL
d$X.1 <- NULL
d$X.2 <- NULL
d$X.3 <- NULL

# 3. Clean up response variable names

source("cleaning/clean_respvar.R")

# 4. Clean up photoperiod #

source("cleaning/clean_photo.R")

# 5. Clean up forcetemp

source("cleaning/clean_forcetemp.R")

# 6. Get rid of non-woodys and clean species names

#source("cleaning/clean_woody_sps.R") 

# Run the below every so often (commented out because it is slow) 
# This checks the species list against www.theplantlist.org ... ask for manchecksp to see non-matches
# source("cleaning/clean_spp_match.R") 

# 7. Clean response and response time columns.
source("cleaning/clean_responsetime.R")

# 8. Remove duplicate lines

source("cleaning/clean_duplicates.R") 

source("chilling/cleaning_chilltemp.R")

#3. Clean the provenance.latitude and provenance.longitude columns, to get appropriate locations
source("chilling/cleaning_provlatlong.R")

# 4. Estimate field chilling (using growing or provenance lat/long to pull climate data)- REQUIRES EXTERNAL HARD DRIVE FOR THIS

# 4a: summarize lat/longs needed to pull climate data from europe and north america
source("chilling/fieldchillcalc_latlong.R")

# 4b: Set the location of the external hard drive, then pull daily climate data for Europe and North America
#Skip ahead to 4e if you do not have the climate data drive
#climatedrive = "/Volumes/Ospree Climate" # (Ospree Climate is name of the external drive, change with new device)
# climatedrive =  "/Volumes/BackYouEvilFiend/ospreeclimate" # Lizzie's backup drive (at WeldHill currently)
# climatedrive = "/Volumes/My Book for Mac/ospreeclimate" # Lizzie's backup drive (at HUH currently)
#climatedrive = "smb://128.103.155.31/WeldShare/Wolkovich Lab/Budburst Review - Ospree/Climate Data" # Access to the data from Weld Share (From Nacho's computer)
climatedrive="/Volumes/WeldShare/Wolkovich\ Lab/Budburst\ Review\ -\ Ospree/Climate\ Data" ##access from Dan's comp


# 4c. pull climate data from europe
source("chilling/pullclimate_eur.R")

# 4d: pull climate data from north america
source("chilling/pullclimate_nam.R")

# 4e: Interpolate hourly temperatures from the daily values 
# & chilling using three different metrics
#(If you want to avoid connecting to the external hard drive, then start here)
#load this .RData workspace)
load("output/fieldclimate.RData")
source("chilling/interpolclimate.R")

# 5. Calculate experimental chilling and merge in field chilling estimates with experimental chilling 
#(If you want to avoid interpolating hourly climate and calculating field chilling, then start here)
dat<-read.csv("output/fieldchillcalcslatlong.csv")

source("chilling/totalchillcalc.R")

# 6. Write out the file with total chilling estimates
berries<-filter(dat4, c(genus=="Fragaria"))
table(berries$chilldays)

write.csv(berries, "output/strawberries.csv", row.names=FALSE)
###I am not sure if this worked or not, I'll have to ask Ailene.