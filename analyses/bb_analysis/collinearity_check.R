### 19 April 2018 - Cat ###
## Looking at model issues - do we have collinearity issues due to experimental design?
## Are the cue interactions species-specific?

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(arm)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

## 3 steps to major cleaning: Get the data, merge in taxa info, subset down to what we want for:
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!
## (1) Get the data and slim down to correct response and no NAs ...
source("source/bbdataplease.R")
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
source("source/othertreats.R")
dim(bb.noNA)
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 28 March 2018 should delete about 359 rows
dim(bb.noNA)
## (3) Deal with species
d <- bb.noNA

source("source/speciescomplex.R")
bb.noNA.wtaxa <- d
dim(bb.noNA.wtaxa)
unique(bb.noNA.wtaxa$complex)

# (4) Get fewer columns for sanity
source("source/commoncols.R")
bb <- subset(bb.noNA.wtaxa, select=c(columnstokeep, "chill.cen", "photo.cen", "force.cen"))

## subsetting data, preparing genus variable, removing NAs (err, again
# remove crops?
# bb <- subset(bb, type!="crop")
bb.stan <- subset(bb, select=c("datasetID", "resp", "chill", "photo", "force", "complex", "type",
                               "chill.cen", "photo.cen", "force.cen"))
bb.stan <- subset(bb.stan, resp<600)
bb.stan$chill <- bb.stan$chill/240
bb.stan<-rename(bb.stan, species=complex)


### Let's start with some plots...
cp<-ggplot(bb.stan, aes(x=chill, y=photo)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex, nrow=6, ncol=8) + theme(legend.position = "none")
fp<-ggplot(bb.stan, aes(x=force, y=photo)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex, nrow=6, ncol=8) + theme(legend.position = "none")
cf<-ggplot(bb.stan, aes(x=chill, y=force)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex, nrow=6, ncol=8) + theme(legend.position = "none")



### Let's start with some simple linear models..
bb.stan$complex<-as.numeric(as.factor(bb.stan$species))
ph<-lm(photo~complex, data=bb.stan)
display(ph)
#lm(formula = photo ~ complex, data = bb.stan)
#coef.est coef.se
#(Intercept) 14.92     0.24  
#complex      0.01     0.01  
#---
#  n = 2947, k = 2
#residual sd = 6.17, R-Squared = 0.00

fo<-lm(force~complex, data=bb.stan)
display(fo)
#lm(formula = force ~ complex, data = bb.stan)
#coef.est coef.se
#(Intercept) 15.18     0.21  
#complex      0.07     0.01  
#---
#  n = 2947, k = 2
#residual sd = 5.35, R-Squared = 0.02

ch<-lm(chill~complex, data=bb.stan)
display(ch)
#lm(formula = chill ~ complex, data = bb.stan)
#coef.est coef.se
#(Intercept)  6.10     0.13  
#complex     -0.07     0.01  
#---
#  n = 2947, k = 2
#residual sd = 3.43, R-Squared = 0.04

cp<-lm(chill+photo~complex, data=bb.stan)
display(cp)
#lm(formula = chill + photo ~ complex, data = bb.stan)
#coef.est coef.se
#(Intercept) 21.02     0.28  
#complex     -0.06     0.01  
#---
#  n = 2947, k = 2
#residual sd = 7.01, R-Squared = 0.01

cf<-lm(chill+force~complex, data=bb.stan)
display(cf)
#lm(formula = chill + force ~ complex, data = bb.stan)
#coef.est coef.se
#(Intercept) 21.28     0.24  
#complex      0.01     0.01  
#---
#  n = 2947, k = 2
#residual sd = 6.17, R-Squared = 0.00

fp<-lm(force+photo~complex, data=bb.stan)
display(fp)
#lm(formula = force + photo ~ complex, data = bb.stan)
#coef.est coef.se
#(Intercept) 30.10     0.36  
#complex      0.08     0.02  
#---
#  n = 2947, k = 2
#residual sd = 9.05, R-Squared = 0.01


######## Hmm... what if we do as.factor(complex)
ph<-lm(photo~species, data=bb.stan)
display(ph)
### ACESAC, ALNGLU, ALNINC, BET complex, BETPEN, BETPUB, Malus(!!), POPTRE, Prunus padus
## Pseudotsuga, Ribes, Vaccinium complex

fo<-lm(force~species, data=bb.stan)
display(fo)
### ACESAC, Fraxinus complex, Larix, Malus, PICABI, Pop complex, Prunus complex, Quercus complex,
## All Quercus, Ribes, Robinia, Ulmus complex, Vaccinium complex

ch<-lm(chill~species, data=bb.stan)
display(ch)
## Fagus, Pop complex, Prunus complex, Quercus faginea, Ulmus complex, Vaccinium complex

cp<-lm(chill+photo~species, data=bb.stan)
display(cp)
## ACESAC, ALNGLU, ALNINC, BET complex, BETPEN, BETPUB, Malus, POPTRE, Prunus padus, Ribes,
## Vaccinium complex

cf<-lm(chill+force~species, data=bb.stan)
display(cf)
## Acer complex, ACESAC, Carpinus, Fraxinus complex, Larix, Malus, PICABI, Quercus complex,
## Quercus faginea, Quercus ilex, Quercus robur, Quercus rubra, Ribes, Robinia, Sorbus,
## Vaccinium

fp<-lm(force+photo~species, data=bb.stan)
display(fp)
### Acer complex, ACESAC, ALNGLU, Betula complex, BETPUB, Fraxinus complex, Larix, 
## Malus(!!), PICABI, Populus complex, POPTRE, Prunus padus, Pseudotsuga, Quercus faginea, 
## Quercus ilex, Quercus robur, Quercus rubra, Ribes, Robinia, Ulmus, Vaccinium

cfp<-lm(chill+force+photo~species, data=bb.stan)
display(cfp)
### Acer complex, ACESAC, ALNGLU, ALNINC, Betcomplex, BETPUB, Carpinus, Fraxinus complex,
## Malus, PICABI, Populus complex, POPTRE, Prunus padus, Pseudotsuga, all Quercus,
## Ribes, Robinia, Symphoricarpos, Ulmus


### Dare we try the model without the main culprits??
remove.us<-c("Acer_saccharum", "Alnus_glutinosa", "Alnus_incana", "Betula_complex", 
             "Betula_pubescens","Malus_domestica", "Populus_tremula", "Prunus_padus", 
             "Ribes_nigrum", "Vaccinium_complex", "Fraxinus_complex", "Larix_decidua", 
             "Picea_abies", "Populus_complex", "Quercus_faginea", "Quercus_ilex", "Quercus_robur", 
             "Quercus_rubra", "Robinia_pseudoacacia", "Ulmus_complex", "Betula_pendula",
             "Populus_complex", "Pseudotsuga_menziesii", "Quercus_complex",
             "Acer_complex", "Carpinus_betulus", "Fagus_sylvatica", "Picea_abies",
             "Prunus_complex", "Symphoricarpos_albus")
#remove.us<-c("Acer_saccharum","Malus_domestica", "Ribes_nigrum",
#             "Vaccinium_complex", "Quercus_faginea")
sm.bb.stan<-filter(bb.stan, !species %in% remove.us)
sm.bb.stan$complex<-as.numeric(as.factor(sm.bb.stan$species))

fit1<-stan_glmer(resp~chill+force+photo+chill:force+chill:photo+
                   force:photo + (1|complex), data=sm.bb.stan)
#stan_glmer
#family:       gaussian [identity]
#formula:      resp ~ chill + force + photo + chill:force + chill:photo + force:photo + 
#  (1 | complex)
#observations: 222
#------
#  Median MAD_SD
#(Intercept) 101.4   20.0 
#chill        -3.2    2.0 
#force        -2.5    1.0 
#photo        -4.5    1.2 
#chill:force  -0.1    0.1 
#chill:photo   0.2    0.1 
#force:photo   0.1    0.1 
#sigma        10.9    0.6 

#Error terms:
#  Groups   Name        Std.Dev.
#complex  (Intercept)  9.5    
#Residual             10.9    
#Num. levels: complex 12 

#Sample avg. posterior predictive distribution of y:
#  Median MAD_SD
#mean_PPD 29.0    1.0  

#------
#  For info on the priors used see help('prior_summary.stanreg').

