## Estimate total chilling and add to the OSPREE data ##
## By Ailene Ettinger, Dan Flynn, Lizzie and more! ###
## See _chillingREADME.txt in same folder for more info ##

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
#source("chilling/cleaning_provlatlong.R")
#This step was moved to the cleaning folder

# 4. Estimate field chilling (using growing or provenance lat/long to pull climate data)- STEP 4B REQUIRES EXTERNAL HARD DRIVE

# 4a: summarize lat/longs needed to pull climate data from europe and north america
source("chilling/fieldchillcalc_latlong.R")

# 4b: Set the location of the external hard drive, then pull daily climate data for Europe and North America
#Skip ahead to 4e if you do not have the climate data drive
 climatedrive = "/Volumes/Ospree Climate" # (Ospree Climate is name of the external drive, change with new device)
# climatedrive =  "/Volumes/BackYouEvilFiend/ospreeclimate" # Lizzie's backup drive (at HUH currently)
# climatedrive = "/Volumes/My Book for Mac/ospreeclimate" # Lizzie's backup drive (at WeldHill currently)
#climatedrive = "//128.103.155.31/WeldShare/Wolkovich Lab/Budburst Review - Ospree/Climate Data" # Access to the data from Weld Share (From Nacho's computer)
 #climatedrive = "/Volumes/climate" #Ailene's climate data drive
 #climatedrive="/Users/aileneettinger/Downloads"

# 4c. pull climate data from europe
source("chilling/pullclimate_eur.R")
# 4d: pull climate data from north america
  #tempval <- list() #required to just pull nam climate
source("chilling/pullclimate_nam.R")

# 4e: Interpolate hourly temperatures from the daily values 
# & chilling using three different metrics
#(If you want to avoid connecting to the external hard drive, then start here)
#load this .RData workspace)
#load("output/fieldclimate.RData")
source("chilling/interpolclimate.R")

# 5. Calculate experimental chilling and merge in field chilling estimates with experimental chilling 
#(If you want to avoid interpolating hourly climate and calculating field chilling, then start here)
#dat<-read.csv("output/fieldchillcalcslatlong.csv")

source("chilling/totalchillcalc.R")

# 6. Write out the file with total chilling estimates

write.csv(dat4, "output/ospree_clean_withchill.csv", row.names=FALSE) ##
#Make list of the studies that are missing chilling data
#dat4$missingCH<-0
#dat4$missingCH[which(is.na(dat4$Total_Chilling_Hours))]<-1
#chilltab<-table(dat4$datasetID,dat4$missingCH)
#missing<-chilltab[chilltab[,2]>0,]
