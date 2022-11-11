## Started 28 October 2022 ##
## By Lizzie ##

## Based on range_fig_output.R ##
# ... start around ... 'modna<-gddlf_jnt.nam'

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

# libraries
library("rstan")

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("dbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

## Need to run gddlf_jnt.nam in rangeleadin_osp.R ##
# save(gddlf_jnt.nam, file="output/gddlf_jnt.nam.Rda")

load("output/gddlf_jnt.nam.Rda")

modna <- gddlf_jnt.nam

# Goal: Remake the plots of traitors (see range_fig_output.R) -- but combine onto one panel the full final plot and the alphasp stuff and connect the changes per species with arrows 

sumer <- summary(modna)$summary
sumer[grep("betaTraitx", rownames(sumer)), "mean"]
