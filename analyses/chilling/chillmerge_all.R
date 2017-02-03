## Estimate total chilling and add to the OSPREE data ##
## By Ailene Ettinger, Dan Flynn, Lizzie and more! ###

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
}  
if(length(grep("ailene", getwd()))>0){setwd("/Users/aileneettinger/git/ospree/analyses")
  }
else setwd("~/Documents/git/ospree/analyses")

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

# 2. Clean the  chilltemp column

source("chilling/cleaning_chilltemp.R")

# 2. Estimate field chilling (using provenance lat/long to pull climate data)- REQUIRES EXTERNAL HARD DRIVE FOR THIS
#Skip this setp if you don't have the climate data drive
#source("chilling/fieldchillcalc_latlong.R")

# 3. Merge in field chilling estimates with experimental chilling 
#(If you want to avoid connecting to the external hard drive, then start here)
dat<-read.csv("output/fieldchillcalcslatlong.csv")

source("chilling/totalchillcalc.R")

# 4. Write out the file with total chilling estimates- (I have not done this yet!)

write.csv(dat4, "output/ospree_clean_withchill.csv", row.names=FALSE) ##

