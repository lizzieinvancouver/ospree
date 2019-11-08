# Started 29 October 2019 by Cat
## Using code from Emily Simmond's sliding window approach: https://github.com/emilygsimmonds/Cue_Identification

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(climwin)
library(lubridate)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd()))>0) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
} else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


# Get budburst data across 45 sites for BETPEN
# Betula puendula data from PEP (both have has GDD from 1 Jan to leafout)
# bp has mat from March 1st to June 1st and mat.lo is 30 days before leafout (uses tg -- aka mean -- data from E-OBS)
# bpalt is similar, but calculated uses txtm -- aka min and max (and we caculate the mean ourselves from those values) -- data from E-OBS) ... we don't use this currently 
bp <- read.csv("PEP_climate/output/betpen_allchillsandgdds_45sites_mat_forsims.csv", header=TRUE)
  
bbsw <- subset(bp, select=c("year", "lo", "siteslist"))
bbsw$bb_date <- as.Date(bbsw$lo, origin=paste0(bbsw$year, "-01-01"))
bbsw$bb_date <- as.character(bbsw$bb_date)
bbsw$doy95 <- bbsw$lo - 4

bbsw <- subset(bbsw, select=c("year", "bb_date", "lo", "doy95", "siteslist"))
colnames(bbsw) <- c("Year", "bb_date", "bb_mean", "doy95", "spatial")
bbsw$bb_date <- as.character(bbsw$bb_date)

bbswpre <- bbsw[(bbsw$Year<=1960),]
bbswpost <- bbsw[(bbsw$Year>1960),]

bbswtest <- bbswpre[(bbswpre$spatial==1),]

### Now get the climate data for 45 sites for BETPEN (from betpen_climate_slidingwin.R)
climatedatapre <- read.csv("pep_sims/simmonds_slidingwin/bp_climatedatapre.csv")
climatedatapost <- read.csv("pep_sims/simmonds_slidingwin/bp_climatedatapost.csv")
  
source("pep_sims/simmonds_slidingwin/Run_SW.R")
# refday = c(day, mon)
# climate is a datafile that must include col = temp
# datafile = biological data
# default = absolute but can also run relative
#run_SW <- function(absolute = TRUE, datafile, climate, refday)

### Now checking Simmond's sliding window approach:
refday <- c(01, 03)
datafile <- bbswpre
climate <- climatedatapre 
climate$X <- NA ### needed in order to run... 

Results_SWRpre <- run_SW(absolute=TRUE, datafile, climate, refday) ## takes a while to run
write.csv(Results_SWRpre[[2]], file="output/results_swapre_bp.csv")
write.csv(Results_SWRpre[[1]], file="output/sumstats_swapre_bp.csv")

if(FALSE){
## Get data and parameters for prediction
source('Params_SW.R')

# extract parameters for complete dataset
Parameters_SWR1 <- get_params_SW(Results_SWR1, bbdata1$bb_mean, "complete", type = "Params")
# SAVE
write.csv(Parameters_SWR1, "output/parameters_swr1.csv", row.names=T)

# extract climate data for complete dataset 'best' window
Results_data_SWR1 <- get_params_SW(Results_SWR1, bbdata1$bb_mean, "complete", type = "Data")
# SAVE
write.csv(Results_data_SWR1, "output/results_data_swr1.csv")
}


if(FALSE){
swapre <- Results_SWRpre[[2]]
swapost <- Results_SWRpost[[2]]

swapre_stat <- Results_SWRpre[[1]]
swapost_stat <- Results_SWRpost[[1]]
}


