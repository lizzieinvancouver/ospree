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
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
} else 
setwd("~/Documents/git/ospree/analyses")

# Load libraries
library(dplyr)
library(tidyr)

# 1. Get the data
d <- read.csv("input/ospree.csv")

# 2. Need to deal with some basic cleaning, delete a few extraneous columns
d$X <- NULL
d$X.1 <- NULL
d$X.2 <- NULL
d$X.3 <- NULL

# 3. Add year to the Zohner data, somehow this got left off 
d$year[which(d$datasetID=="zohner16" & d$fieldsample.date=="21-Dec-2013")] <- 2013
d$year[which(d$datasetID=="zohner16" & d$fieldsample.date=="10-Feb-2014")] <- 2014
d$year[which(d$datasetID=="zohner16" & d$fieldsample.date=="21-Mar-2014")] <- 2014

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

# 9. Remove duplicate lines

source("cleaning/clean_duplicates.R") 

# 10. Write out the final file! 

write.csv(d, "output/ospree_clean.csv", row.names=FALSE)




