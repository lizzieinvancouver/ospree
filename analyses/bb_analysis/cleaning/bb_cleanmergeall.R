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

# 1. Get the data (that has already been cleaned for respvar and chilling)
d <- read.csv("output/ospree_clean_withchill.csv")

# 2. Need to deal with thermal time to days
source("bb_analysis/cleaning/clean_thermaltimetodays.R")

# 3. Clean phenstage to get a little more data (a little, but still!). 
# source("bb_analysis/cleaning/clean_phenstage.R")

# 4. Select out the highest percentage of budburst only, and remove studies that contain duplicate data in two forms
source("bb_analysis/cleaning/multiresp.R")

# 5. Clean ambient forcing
source("bb_analysis/cleaning/clean_ambientforcing.R")

# 6. Clean/convert percentBB to days, using a specified target bud-burst level (i.e. 90%)
source("bb_analysis/cleaning/clean_bbperctodays.R")

# 7. Clean duplicate responses across treatments/categories (see bbcleaning_README.txt)
source("bb_analysis/cleaning/clean_moreduplicates.R") # as of 1 July 2017 this deletes 3 rows

# 8. Write out the final file! 
write.csv(d, "output/ospree_clean_withchill_BB.csv", row.names=FALSE) ##



