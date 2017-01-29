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
setwd("~/Documents/git/ospree")

# Load libraries
library(dplyr)
library(tidyr)

# 1. Get the data
d <- read.csv("input/ospree.csv")

# 2. Need to deal with some basic cleaning ... what are these columns and what should we do with them? 
unique(d$X.1)
unique(d$X.2)
unique(d$X.3)

# 3. Add in Zohner data ... (need to rewrite code in zohner_addingtomaindata.R so that it works here, and move any code that cleans columns and deals with multiresp to clean_respvar.R ... ##

# source("zohner_addingtomaindata.R")

# 4. Clean up response variable names

source("cleaning/clean_respvar.R")

# 5. Clean up photoperiod #

source("cleaning/clean_photo.R")

# 6. Clean response and response time?

# source("cleaning/clean_responsetime.R")

# 7. Convert percent budburst to days to budburst

# source("cleaning/clean_bbperctodays.R") # Ailene, can you update this file to run as a source code here ysing dataframe d created above?

# 8. Clean up woody species

# source ("clean_woody_sps.R") # need to rewrite this a little (Nacho)

# 9. Write out the final file! (I have not run this yet .... ) 

# write.csv("output/ospree_clean.csv", row.names=FALSE) ##



# X. Make table of taxonomy for possible phylogenetic work

#source("cleaning/clean_spp_match.R") # Slow, checking sp names against The Plant List

