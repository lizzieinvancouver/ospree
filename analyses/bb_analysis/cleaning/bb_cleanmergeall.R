## Clean the BB data within OSPREE ##
## started Nacho ###

## Making a master cleaning file for BudBreak analyses ... ##
## And away we go! ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} 
if(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else {
  setwd("~/Documents/git/ospree/analyses")}


# Load libraries
library(dplyr)
library(tidyr)

# 1. Get the data (that has already been cleaned for respvar and chilling)
d <- read.csv("output/ospree_clean_withchill.csv")


# 2. Need to deal with thermal time to days
source("bb_analysis/cleaning/clean_thermaltimetodays.R")

# 3. Clean ambient forcing
source("bb_analysis/cleaning/clean_ambientforcing.R")

# 4. Clean/convert percentBB to days
source("bb_analysis/cleaning/clean_bbperctodays.R")


# 5. Write out the final file! 
write.csv(d, "output/ospree_clean_withchill_BB", row.names=FALSE) ##



