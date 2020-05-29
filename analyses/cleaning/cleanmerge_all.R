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
} else if
(length(grep("deirdreloughnan", getwd()))>0) {setwd("~/Documents/github/ospree/analyses")
} else 
setwd("~/Documents/git/ospree/analyses")

# Load libraries
library(dplyr)
library(tidyr)

# 1. Get the data 
dorg <- read.csv("input/ospree.csv")
dup <- read.csv("output/ospree2019update.csv") # built in merge_update2019.R

# 2. Need to deal with some basic cleaning, delete a few extraneous columns and bind it
dorg$X <- NULL
dorg$X.1 <- NULL
dorg$X.2 <- NULL
dorg$X.3 <- NULL

d <- rbind(dorg, dup) # damn, you think research is rushing on but new data is only 11% of data!

# 3. Add in some data that was not properly entered the first go-round
source("cleaning/clean_addata.R")

# 4. Clean up some super miscellaneous stuff
source("cleaning/clean_misc.R") # Updated by Dan 25 August
# The other error comes from making a temporary numeric column for cleaning linkosalo06

# 4. Clean up response variable names
source("cleaning/clean_respvar.R") # Updated 22 August by Lizzie

# 6. Clean up photoperiod #
source("cleaning/clean_photo.R") # Updated 19 August 2019 by Cat

# 7. Clean up forcetemp
source("cleaning/clean_forcetemp.R") # Updated 19 August 2019 by Cat

# 8. Get rid of non-woodys and clean species names
source("cleaning/clean_woody_sps.R") # removes 3409 rows of non-woody species 

# Run the below every so often (commented out because it is slow) 
# This checks the species list against www.theplantlist.org ... ask for manchecksp to see non-matches
# source("cleaning/clean_spp_match.R") ##Re-run by Dan B Sept 16

# 9. Clean response and response time columns.
source("cleaning/clean_responsetime.R") # Updated by Dan B 25 Aug 2019

# 10. Clean the lat/long (growing and provenance)
source("cleaning/clean_latlong.R") # Updated by Ailene in August 2019

# 11. Remove duplicate lines

source("cleaning/clean_duplicates.R") # removes 239 rows as of 22 August 2019

# 12. Write out the final file! 

write.csv(d, "output/ospree_clean.csv", row.names=FALSE)




