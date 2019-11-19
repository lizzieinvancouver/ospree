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
(length(grep("ailene", getwd()))>0) {setwd("~Documents/GitHub/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")


# Load libraries
library(dplyr)
library(tidyr)
library(geosphere)
library(lubridate)

# 1. Get the data (that has already been cleaned for respvar and chilling)
d <- read.csv("output/ospree_clean_withchill.csv") # 5 Sept 2019: 14603 (30 Oct 2018: 12693)

# 2. Need to deal with thermal time to days
source("bb_analysis/cleaning/clean_thermaltimetodays.R") # 15 Oct 2019: 14603 

# 3. Clean phenstage to get a little more data (a little, but still!). 
source("bb_analysis/cleaning/clean_respvarmore.R") # 15 Oct 2019: 14062 

# 4. Select out the highest percentage of budburst only, and remove studies that contain duplicate data in two forms
source("bb_analysis/cleaning/multiresp.R") #  15 Oct 2019: 11404 (5 Sept 2019: 11567; pre-update: 9717 rows)

# 5. Clean ambient forcing
# 5a. Clean up entries where we can estimate the forcing from the paper (e.g., ramped temps or they give monthly temps)
source("bb_analysis/cleaning/clean_rampedandexpforcing.R") # 15 Oct: 11404 (no changes needed)

# 5b. Check date of daily climate files used in step 5c-
#if they are too old for your taste,run pulldailyclim.R and bb_daily_dataprep.R scripts (these take a while)
source("bb_analysis/cleaning/clean_checkdateofclimatedata.R") #  15 Oct 2019: 11404 (30 Oct 2018: 9717 rows)

# 5c. Clean ambient forcing data using daily climate data ... slow
source("bb_analysis/cleaning/clean_ambientforcingfromdailyclimate.R") # 15 Oct 2019: still 11404 

# 6. Clean/convert percentBB to days, using a specified target bud-burst level (i.e. 90%)
# ... with an allowable buffer (i.e., 55%)
source("bb_analysis/cleaning/clean_bbperctodays.R") #  15 Oct 2019: 9006 (22 Mar 2019: 8681)

# 7. Clean duplicate responses across treatments/categories)
source("bb_analysis/cleaning/clean_moreduplicates.R") #  15 Oct 2019: 9002 (22 Mar 2019: 7643)

# 8. Clean photoperiod entries to try to get as much data as possible
source("bb_analysis/cleaning/clean_photoperiod.R")

# 9. Write out the final file! 
write.csv(d, "output/ospree_clean_withchill_BB.csv", row.names=FALSE) ## As of 15 Oct 2019: 9002 (27 March 2019: 7643)



if(FALSE){
## Temporary addition to look at species we might end up with ...
source("bb_analysis/source/speciescomplex.multcues.R")
## just the bb data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),]
bb.resp <- subset(bb.resp, respvar != "thermaltime") 

## make a bunch of things numeric (eek!)
bb.resp$forceday <- as.numeric(bb.resp$forcetemp)
bb.resp$forcenight <- as.numeric(bb.resp$forcetemp_night)
bb.resp$photonight <- as.numeric(bb.resp$photoperiod_night)

bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$force <- bb.resp$forceday
bb.resp$force[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] <-
    (bb.resp$forceday[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photo[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] +
    bb.resp$forcenight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photonight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE])/24

bb.resp$chill <- as.numeric(bb.resp$Total_Utah_Model) 
quickdirty <- sppcomplexfx.multcue(bb.resp)
sort(unique(quickdirty$complex.wname))
write.csv(d, "output/ospree_clean_withchill_BB_sppcomplex.csv", row.names=FALSE) 
}
