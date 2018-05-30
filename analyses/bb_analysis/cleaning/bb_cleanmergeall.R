## Clean the BB data within OSPREE ##
## started Nacho ###

## Making a master cleaning file for Budbreak analyses ... ##
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
library(lubridate)

# 1. Get the data (that has already been cleaned for respvar and chilling)
d <- read.csv("output/ospree_clean_withchill.csv") # 29 May 2018: 12693 

# 2. Need to deal with thermal time to days
source("bb_analysis/cleaning/clean_thermaltimetodays.R") # 29 May 2018: 12693

# 3. Clean phenstage to get a little more data (a little, but still!). 
source("bb_analysis/cleaning/clean_respvarmore.R") # 29 May 2018: 12152 

# 4. Select out the highest percentage of budburst only, and remove studies that contain duplicate data in two forms
source("bb_analysis/cleaning/multiresp.R") # as of 16 July 2017, deletes ~2400 rows (same in May 2018) 9717 rows

# 5. Clean ambient forcing
# 5a. Clean up entries where we can estimate the forcing from the paper (e.g., ramped temps or they give monthly temps)
source("bb_analysis/cleaning/clean_ambientforcing.R")

# 5b. Check date of daily climate files used in step 5c-
#if they are too old for your taste,run pulldailyclim.R and bb_daily_dataprep.R scripts (these take a while)
source("bb_analysis/cleaning/clean_checkdateofclimatedata.R") # As of 29 May 2018: 9717 rows

# 5c. Clean ambient forcing data using daily climate data
source("bb_analysis/cleaning/clean_ambientforcingfromdailyclimate.R") # still 9717 rows

# 6. Clean/convert percentBB to days, using a specified target bud-burst level (i.e. 90%)
# ... with an allowable buffer (i.e., 55%)
source("bb_analysis/cleaning/clean_bbperctodays.R") # As of 29 May 2018: 7432 rows 

# 7. Clean duplicate responses across treatments/categories)
source("bb_analysis/cleaning/clean_moreduplicates.R") # As of 29 May 2018, deletes 2 rows (7534).

# 8. Clean photoperiod entries to try to get as much data as possible
source("bb_analysis/cleaning/clean_photoperiod.R")

# 9. Write out the final file! 
write.csv(d, "output/ospree_clean_withchill_BB.csv", row.names=FALSE) ## As of 29 May 2018: 7430 rows  (as of 12 Apr 2018: 8251)


