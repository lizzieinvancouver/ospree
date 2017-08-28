## Started 28 August 2017 ##
## By Lizzie ##

## Based off bbmodels_stan.R ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(shinystan)
# library(rstanarm)
library(lme4)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")


## 3 steps to major cleaning: Get the data, merge in taxa info, subset down to what we want for:
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!
## (1) Get the data and slim down to correct response and no NAs ...
source("bb_analysis/source/bbdataplease.R")
## (2) Deal with species
dim(bb.noNA)
d <- bb.noNA
source("bb_analysis/source/speciescomplex.R")
bb.noNA.wtaxa <- d
dim(bb.noNA.wtaxa)
unique(bb.noNA.wtaxa$complex)
# (3) Get fewer columns for sanity
source("bb_analysis/source/commoncols.R")
bb <- subset(bb.noNA.wtaxa, select=columnstokeep)

## For centering data, not doing it for now
#bb$photo.cen <- scale(bb$photo, center=TRUE, scale=TRUE)
#bb$force.cen <- scale(bb$force, center=TRUE, scale=TRUE)
#bb$chill.cen <- scale(bb$chill, center=TRUE, scale=TRUE)

## subsetting data, preparing genus variable, removing NAs (err, again
# remove crops?
# bb <- subset(bb, type!="crop")
bb.sm <- subset(bb, select=c("datasetID", "resp", "chill", "photo", "force", "complex", "type"))

# remove the two values above 600
bb.sm <- subset(bb.sm, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb.sm$chill <- bb.sm$chill/240

## look at a few single sp.
fagsyl <- subset(bb.sm, complex=="Fagus_sylvatica") 
betpub <- subset(bb.sm, complex=="Betula_pubescens") # 311 datapoints across 8 studies
betpen <- subset(bb.sm, complex=="Betula_pendula") # 312 datapoints across 7 studies, no big intxns
piceabi <- subset(bb.sm, complex=="Picea_abies") # 251 datapoints across 10 studies, no big intxns

onesp <- fagsyl

dim(onesp)
unique(onesp$datasetID)

simplemodel <- lm(resp~photo+force+chill, data=onesp)
intmodel <- lm(resp~(photo+force+chill)^2, data=onesp)
intmodel.nofc <- lm(resp~photo+force+chill+photo:chill+force:photo, data=onesp)


coef(simplemodel) 
coef(intmodel) # intercept seems high....

anova(simplemodel)
anova(intmodel)

othermodel <- lmer(resp~(photo+force+chill)^2 + (1|datasetID), data=onesp) 

## Back to all data, just looking ... 
othermodel1 <- lmer(resp~(photo+force+chill)^2 + (1|datasetID), data=bb.sm) # Maybe force:chill cannot be in models
othermodel2 <- lmer(resp~photo+force+chill+photo:chill+force:photo + (1|datasetID), data=bb.sm)
othermodel3 <- lmer(resp~photo+force+chill + (1|datasetID), data=bb.sm)
simpmodel3 <- lm(resp~photo+force+chill, data=bb.sm) # yep, datasetID is probably swamping study design variation. Ugh!
