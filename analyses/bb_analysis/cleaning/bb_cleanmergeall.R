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
d <- read.csv("output/ospree_clean_withchill.csv")

# 2. Need to deal with thermal time to days
source("bb_analysis/cleaning/clean_thermaltimetodays.R")

# 3. Clean phenstage to get a little more data (a little, but still!). 
source("bb_analysis/cleaning/clean_respvarmore.R")

# 4. Select out the highest percentage of budburst only, and remove studies that contain duplicate data in two forms
source("bb_analysis/cleaning/multiresp.R") # as of 16 July 2017, deletes ~2400 rows

# 5. Clean ambient forcing
# 5a. Clean up entries where we can estimate the forcing from the paper (e.g., ramped temps or they give monthly temps)
source("bb_analysis/cleaning/clean_ambientforcing.R")

# 5b. Clean ambient forcing daily - not active yet
#source("bb_analysis/cleaning/clean_ambientforcingfromdailyclimate.R")

# 6. Clean/convert percentBB to days, using a specified target bud-burst level (i.e. 80%)
# ... with an allowable buffer (i.e., 40%)
source("bb_analysis/cleaning/clean_bbperctodays.R") # as of 16 July 2017, deletes 84 rows

# 7. Clean duplicate responses across treatments/categories
source("bb_analysis/cleaning/clean_moreduplicates.R") # as of 16 July 2017, deletes 4 rows.

# 8. Clean photoperiod entries to try to get as much data as possible
source("bb_analysis/cleaning/clean_photoperiod.R")

# 9. Now remove any percentbudburst lower than 40% (clean_bbperctodays.R only cleans lines with more than entry per treatment)
d$response.num <- as.numeric(d$response) # only 'timeonly' should be removed here
d <- d %>% filter(!(response.num<39.99 & respvar.simple=="percentbudburst"))
d$response.num <- NULL

# 10. Write out the final file! 
write.csv(d, "output/ospree_clean_withchill_BB.csv", row.names=FALSE) ##



