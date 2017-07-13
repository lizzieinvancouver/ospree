## Clean the BB data within OSPREE ##
## started Nacho ###

## Making a master cleaning file for BudBreak analyses ... ##
## And away we go! ##

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
library(geosphere)

# 1. Get the data (that has already been cleaned for respvar and chilling)
d <- read.csv("output/ospree_clean_withchill.csv")
#Ailene's checking code:
#dim(d)# 12744    79
#unique(d$respvar.simple)#12
#length(d$respvar.simple[d$respvar.simple=="daystobudburst" ])#3372
# 2. Need to deal with thermal time to days
source("bb_analysis/cleaning/clean_thermaltimetodays.R")
#Ailene's checking code:
#dim(d)# 12744    79
#unique(d$respvar.simple)#12
#length(d$respvar.simple[d$respvar.simple=="daystobudburst" ])#3402
# 3. Clean phenstage to get a little more data (a little, but still!). 
source("bb_analysis/cleaning/clean_phenstage.R")
#Ailene's checking code:
#dim(d)# 12744    79
#unique(d$respvar.simple)#12
#length(d$respvar.simple[d$respvar.simple=="daystobudburst" ])#
# 4. Select out the highest percentage of budburst only, and remove studies that contain duplicate data in two forms
source("bb_analysis/cleaning/multiresp.R")
#Ailene's checking code:
#dim(d)# 10308    79 (2436 rows lost)
#unique(d$respvar.simple)#now 11 - "otherpercents" removed
#length(d$respvar.simple[d$respvar.simple=="daystobudburst" ])#3442
# 5. Clean ambient forcing
source("bb_analysis/cleaning/clean_ambientforcing.R")
#Ailene's checking code:
#dim(d)# 10308    79
#sort(unique(d$forcetemp))#53 different values before cleaning, 50 after- still 15 different non-numeric values for this column
#length(d$respvar.simple[d$respvar.simple=="daystobudburst" ])#still 3442

# 6. Clean/convert percentBB to days, using a specified target bud-burst level (i.e. 90%)
source("bb_analysis/cleaning/clean_bbperctodays.R")
#Ailene's checking code:
#dim(d)# 10224   83
#length(d$respvar.simple[d$respvar.simple=="daystobudburst" ])#now  3442
#unique(d$respvar.simple)#still 11
# 7. Clean duplicate responses across treatments/categories
source("bb_analysis/cleaning/clean_moreduplicates.R") # as of 7 July 2017 this deletes >3000 rows (up from 3 a week ago)
#Ailene's checking code:
#dim(d)# 10220  #lost4 rows even though only 3 selected to be removed- why?
#
#length(d$respvar.simple[d$respvar.simple=="daystobudburst" ])
#unique(d$respvar.simple)

# 8. Clean photoperiod entries to try to get as much data as possible
source("bb_analysis/cleaning/clean_photoperiod.R")
#Ailene's checking code:
#dim(d)# 10220     83 
#length(d$respvar.simple[d$respvar.simple=="daystobudburst" ])#still 3441
#dim(d[d$photoperiod_day=="ambient",])#1471   83
#dim(d[d$photoperiod_night=="ambient",])#1451   83 rows with ambient not surewhy these are different
# 9. Write out the final file! 
write.csv(d, "output/ospree_clean_withchill_BB.csv", row.names=FALSE) ##



