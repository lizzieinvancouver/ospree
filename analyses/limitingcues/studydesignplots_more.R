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

# the below should already have cleaned lat/long
dat <- read.csv("output/ospree_clean.csv",header = TRUE)

# make a bunch of things numeric (eek!)
dat$forcetemp <- as.numeric(dat$forcetemp)
dat$forcetemp_night <- as.numeric(dat$forcetemp_night)
dat$photoperiod_night <- as.numeric(dat$photoperiod_night)
dat$photoperiod_day <- as.numeric(dat$photoperiod_day)
dat$chilltemp <- as.numeric(dat$chilltemp)


columnstokeep <- c("datasetID", "study", "genus", "species", "varetc", "woody",
    "provenance.lat", "provenance.long", "material", "year", "fieldsample.date", 
    "forcetemp", "forcetemp_night", "photoperiod_day", "photoperiod_night",
    "chilltemp", "chillphotoperiod", "chilldays", "response", "response.time")        
                   

d <- subset(dat, select=c(columnstokeep))

# Summarize the forcetemp by photoperiod
d$force <- d$forcetemp
d$force[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE] <-
    (d$forcetemp[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE]*
    d$photoperiod_day[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE] +
    d$forcetemp_night[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE]*
    d$photoperiod_night[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE])/24


## summarizing data
## START HERE ...
# Add: (1) mean.Latitude, (2) mean.long, (3) field.sample date (unique), (4) year
dsumm <-
      ddply(d, c("datasetID", "study"), summarise,
      mean.year = mean(year),
      mean.temp = mean(force),
      min.temp = min(force),
      max.temp = max(force),
      sd.temp = sd(force),
      mean.photo = mean(photoperiod_day),
      min.photo = min(photoperiod_day),
      max.photo = max(photoperiod_day),
      sd.photo = sd(photoperiod_day),
      mean.chill = mean(chilltemp),
      min.chill = min(chilltemp),
      max.chill = max(chilltemp),
      sd.chill = sd(chilltemp),
      field.sample.n = length(fieldsample.date))
