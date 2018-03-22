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
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


# dostan = TRUE
source("source/bbstanleadin.R")

## look at a few single sp.
fagsyl <- subset(bb, complex=="Fagus_sylvatica") # 520 datapoints across 10 studies (force is positive)
betpub <- subset(bb, complex=="Betula_pubescens") # 280 datapoints across 8 studies
betpen <- subset(bb, complex=="Betula_pendula") # 347 datapoints across 12 studies
piceabi <- subset(bb, complex=="Picea_abies") # 275 datapoints across 13 studies

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

maineff <- lmer(resp~photo+chill+force + (1|datasetID), data=onesp)
wintxns <- lmer(resp~photo+chill+force+chill*photo+chill*force+photo*force + (1|datasetID), data=onesp)
w3wayintxn <- lmer(resp~photo*chill*force + (1|datasetID) + (1|complex), data=bb.stan)
wintxns.nofp <- lmer(resp~photo+chill+force+chill*photo+chill*force + (1|datasetID), data=onesp)
wintxns.nofc <- lmer(resp~photo+chill+force+chill*photo+photo*force + (1|datasetID), data=onesp)
wintxns.nocp <- lmer(resp~photo+chill+force+force*photo+chill*force + (1|datasetID), data=onesp)
wintxns.nofpcp <- lmer(resp~photo+chill+force+chill*force + (1|datasetID), data=onesp)
wintxns.nop <- lmer(resp~chill+force+chill*force + (1|datasetID), data=onesp)

bb.intxn <- onesp
bb.intxn$cp <- bb.intxn$chill*bb.intxn$photo
bb.intxn$cf <- bb.intxn$chill*bb.intxn$force
bb.intxn$fp <- bb.intxn$photo*bb.intxn$force

par(mfrow=c(1,3))
hist(bb.stan$chill)
hist(bb.stan$photo)
hist(bb.stan$force)
par(mfrow=c(2,3))
plot(resp~cp, data=bb.intxn)
plot(resp~cf, data=bb.intxn)
plot(resp~fp, data=bb.intxn)
}
###



## Back to all data, just looking ... 
othermodel1 <- lmer(resp~(photo+force+chill)^2 + (1|datasetID), data=bb.sm) # Maybe force:chill cannot be in models
othermodel2 <- lmer(resp~photo+force+chill+photo:chill+force:photo + (1|datasetID), data=bb.sm)
othermodel3 <- lmer(resp~photo+force+chill + (1|datasetID), data=bb.sm)
simpmodel3 <- lm(resp~photo+force+chill, data=bb.sm) # yep, datasetID is probably swamping study design variation. Ugh!
