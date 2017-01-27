# Review data prep

if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 
setwd("~/Documents/git/ospree")

# 1. Clean up response variable names

source("cleaning/clean_respvar.R")

# 2. Convert percent budburst to days to budburst

#source("cleaning/clean_bbperctodays.R")

# 3. Make table of taxonomy for phylogenetic work

#source("cleaning/clean_spp_match.R") # Slow, checking sp names against The Plant List

