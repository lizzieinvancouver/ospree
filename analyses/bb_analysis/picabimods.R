# Testing single species models
### Started 28 November 2018 ##
## By Ailene ##
library(lme4)
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillunits = FALSE # change to true for testing chill units
use.allphoto = TRUE #change to false if using only exp or ramped photo
use.allspp = TRUE #change to FALSE if using one cue or no crops or other

source("source/bbstanleadin.R")
picabi<-bb.exprampphotoforce[bb.exprampphotoforce$genus =="Picea" & bb.exprampphotoforce$species =="abies",]
dim(picabi)
picabi.mod<-lm(resp~force.z+chill.z+photo.z+force.z:chill.z+photo.z:chill.z+force.z:photo.z, data=picabi)
picabi.mmod<-lmer(resp~force.z+chill.z+photo.z+force.z:chill.z+photo.z:chill.z+force.z:photo.z + (1|datasetID), data=picabi)
summary(picabi.mmod)
