## Started in August 2019 (ish) ##
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
ks <- read.csv("input/update2019/ospree_2019update_ks.csv")
my <- read.csv("input/update2019/ospree_2019update_my.csv")

# See how the data looks and do a little clean-up
dim(cjc)
head(cjc)
tail(cjc)

dim(dl)
dim(dmb)
dim(dss) # dim is wrong
dim(ks)
dim(my)
head(dss[,54:75])
dss[,54:75] <- NULL
dim(dss)

# fix weird name
dmb$datasetID[dmb$datasetID=="fu_2018"] <- "fu18"

# Put the data together (not pretty but not really worth it to apply now)
d <- rbind(cjc, dl)
d <- rbind(d, dmb)
d <- rbind(d, dss)
d <- rbind(d, ks)
d <- rbind(d, my)

sum(nrow(cjc), nrow(dl), nrow(dmb), nrow(dss), nrow(ks), nrow(my)) # check! 

write.csv(d, "output/ospree2019update.csv", row.names=FALSE)
