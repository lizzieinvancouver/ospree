## Merge the 2019 OSPREE data update ##
## By Lizzie so far ... ###

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

# Load libraries
library(dplyr)
library(tidyr)

# Get the data
cjc <- read.csv("input/update2019/ospree_2019update_cjc.csv")
dl <- read.csv("input/update2019/ospree_2019update_dl.csv")
dmb <- read.csv("input/update2019/ospree_2019update_dmb.csv")
dss <- read.csv("input/update2019/ospree_2019update_DSS.csv")
my <- read.csv("input/update2019/ospree_2019update_my.csv")

dim(cjc)
head(cjc)
tail(cjc)

dim(dl)
dim(dmb)
dim(dss) # dim is wrong
dim(my)
