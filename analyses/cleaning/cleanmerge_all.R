## Clean the OSPREE data ##
## By Dan Flynn, Lizzie and more! ###

## Trying to create a master cleaning file ... ##
## And away we go! ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 
setwd("~/Documents/git/ospree/analyses")

# Load libraries
library(dplyr)
library(tidyr)

# 1. Get the data
d <- read.csv("input/ospree.csv")

# 2. Need to deal with some basic cleaning ... what are these columns and what should we do with them? 
unique(d$X.1)
unique(d$X.2)
unique(d$X.3) # d$X.3 <- NULL  # should delete columns

# NEED TO CHECK, see issue # 71
d$X.1 <- NULL
d$X.2 <- NULL
d$X.3 <- NULL

# To do: Check if we still need zohner_checkifweneed.R now that cleaning happens here.

# 3. Clean up response variable names

source("cleaning/clean_respvar.R")

# 4. Clean up photoperiod #

source("cleaning/clean_photo.R")

# 5. Clean up forcetemp

source("cleaning/clean_forcetemp.R")

# 6. Clean response and response time?

# source("cleaning/clean_responsetime.R")

# 7. Convert percent budburst to days to budburst (will possibly move) 

# source("cleaning/clean_bbperctodays.R") # Ailene, can you update this file to run as a source code here using dataframe d created above?

# 8. Clean up woody species

source("cleaning/clean_woody_sps.R") # need to rewrite this a little (Nacho)

# 8b. Remove duplicate lines

#source("clean_duplicates.final.R") ## should be checked before activation


# 9. Write out the final file! (I have not run this yet .... ) 

write.csv(d, "output/ospree_clean.csv", row.names=FALSE) ##



# X. Make table of taxonomy for possible phylogenetic work

#source("cleaning/clean_spp_match.R") # Slow, checking sp names against The Plant List

