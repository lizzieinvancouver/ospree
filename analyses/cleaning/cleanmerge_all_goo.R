## Clean the OSPREE data ##
## By Dan Flynn, Lizzie, Ailene, Cat C., Dan B., Nacho and more! ###

## Trying to create a master cleaning file ... ##
## And away we go! ##

## Note! We found some errors in chilling entries; all those are cleaned in clean_chilltemp.R ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses")
} else 
setwd("~/Documents/git/ospree/analyses")

# Flags for which data to clean
update2019 <- FALSE # set to false to clean the main OSPREE data (2015-2016 collected data)

# Load libraries
library(dplyr)
library(tidyr)

# 1. Get the data
if(update2019){
d <- read.csv("input/ospree_2019update_goo.csv", fileEncoding="latin1")
}
if(!update2019){
d <- read.csv("input/ospree.csv")
}

# 2. Need to deal with some basic cleaning, delete a few extraneous columns
if(!update2019){
d$X <- NULL
d$X.1 <- NULL
d$X.2 <- NULL
d$X.3 <- NULL
}
if(update2019){
d$X <- NULL
}

# 3. Clean up some super miscellaneous stuff
source("cleaning/clean_misc.R")

# 4. Clean up response variable names
source("cleaning/clean_respvar.R")

# 5. Clean up photoperiod #
source("cleaning/clean_photo.R")

# 6. Clean up forcetemp
source("cleaning/clean_forcetemp.R")

# 7. Get rid of non-woodys and clean species names
source("cleaning/clean_woody_sps.R") 

# Run the below every so often (commented out because it is slow) 
# This checks the species list against www.theplantlist.org ... ask for manchecksp to see non-matches
# source("cleaning/clean_spp_match.R") 

# 8. Clean response and response time columns.
source("cleaning/clean_responsetime.R")

# 9. Clean the lat/long (growing and provenance)
source("cleaning/clean_latlong.R")

# 10. Remove duplicate lines

source("cleaning/clean_duplicates.R") 

# 11. Write out the final file! 

if(!update2019){
write.csv(d, "output/ospree_clean.csv", row.names=FALSE)
}



