## Started 7 May 2018 ##
## By Lizzie ##

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# need to check if below includes clean lat/long
dat <- read.csv("output/ospree_clean.csv",header = TRUE)

# make a bunch of things numeric (eek!)
dat$forcetemp <- as.numeric(dat$forcetemp)
dat$forcetemp_night <- as.numeric(dat$forcetemp_night)
dat$photoperiod_night <- as.numeric($photoperiod_night)
dat$photoperiod_day <- as.numeric(dat$photoperiod_day)

columnstokeep <- c("datasetID", "study", "genus", "species", "varetc", "woody", "provenance.lat", "provenance.long",
                   "material", "fieldsample.date", "forcetemp", "forcetemp_night",
                   "photoperiod_day", "photoperiod_night", "chilltemp", 
                   "chillphotoperiod", "chilldays", "response", "response.time")

d <- subset(dat, select=c(columnstokeep))

## summarizing data 
dsumm <-
      ddply(d, c("datasetID", "study"), summarise,
      mean.temp = mean(yvar),
      sd = sd(yvar),
      sem = sd(yvar)/sqrt(length(yvar)))
