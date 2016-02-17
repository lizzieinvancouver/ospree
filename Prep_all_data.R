# Review data prep

setwd("~/Documents/git/budreview")

# 1. Clean up response variable names

source("clean_respvar.R")

# 2. Convert percent budburst to days to budburst

source("clean_bbperctodays.R")

# 3. Make table of taxonomy for phylogenetic work

source("clean_spp_match.R")

