## Started 27 July 2017 ##
## By Lizzie to start ##

## Trying to examine whether estimated effects are consisent across methods ##

library(lme4)
# library(rstan)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}

# get the data .... 
bb <- read.csv("..//output/ospree_clean_withchill_BB.csv", header=TRUE)

## read taxon data
taxinfo <- read.csv("..//output/bb_analysis/taxon/species_manipulation_levels.csv")
taxons <-read.csv("..//output/bb_analysis/taxon/complex_levels.csv")
taxon.info<-subset(taxon.info,use=="Y")

# get the species we want to use
subset(taxinfo, field.sample>3 & chill>1 & datasets>1)

# okay, now we have to look at the studies these species are in and include only studies that vary either field sample or chill and then both vary the same other variables (forcing and/or photo) .... 

# Nacho's code 
taxon.info$taxa<-paste(taxon.info$genus,taxon.info$species,sep="_")
bb$bb.taxa<-paste(bb$genus,bb$species,sep="_")
bb<-subset(bb,bb.taxa%in%taxon.info$taxa)

