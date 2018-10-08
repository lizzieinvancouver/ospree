## Started 21 Feb 2017 ##
## By Nacho, Lizzie, Dan and others ##

## Plotting results from Stan models ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

## To do!
## Clean up plotting code and add a figure showing all data and sp lines together (well, 3 figures)
## Figure out why sigma_a plots each sp in muplot, but no dice for sigma_bforce etc.

library(rstan)
library(ggplot2)
library(shinystan)
library(gridExtra)
library(plyr)
library(dplyr)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

source('stan/savestan.R')

figpath <- "lat_analysis/figures"

##
##
## << START OF taken from bbmodels_stan.R >> ##

## 3 steps to major cleaning: Get the data, merge in taxa info, subset down to what we want for:
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!
## (1) Get the data and slim down to correct response and no NAs ...
source("bb_analysis/source/bbdataplease.R")
## (2) Deal with species
dim(bb.noNA)
d <- bb.noNA
source("bb_analysis/source/speciescomplex.R")
myspp<-c("Betula_pendula", "Betula_pubscens", "Fagus_sylvatica", "Picea_abies",
                          "Ribes_nigrum", "Corylus_avellana", "Quercus_robur", "Larix_decidua")
bb.wlab<-dplyr::filter(d, complex%in%myspp)
# (3) Get fewer columns for sanity
source("bb_analysis/source/commoncols.R")
bb <- subset(bb.wlab, select=columnstokeep)

## subsetting data, preparing genus variable, removing NAs (err, again
# remove crops?
# bb <- subset(bb, type!="crop")
bb.stan <- subset(bb, select=c("datasetID", "resp", "chill", "photo", "force", "provenance.lat", "complex", "type"))
bb.stan$lat<- bb$stan$provenance.lat
bb.stan$complex.wname <- bb.stan$complex
bb.stan$complex <- as.numeric(as.factor(bb.stan$complex))

# remove the two values above 600
bb.stan <- subset(bb.stan, resp<600)

# adjust chilling (if needed)
bb.stan$chill <- bb.stan$chill/240

## << END OF taken from bbmodels_stan.R >> ##
##
##

# Load fitted stan model: no interactions
load("stan/lat/output/LAT_daysBBnointer_2level.Rda")
m1.bb <- td4
# summary(m1.bb)

# explore results in shinystan
#launch_shinystan(m1.bb)

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
source("lat_analysis/source/lat_muplot.R")

sumer.ni <- summary(td4)
sumer.ni[grep("mu_", rownames(sumer.ni)),]


source("lat_analysis/source/lat_muplot.R")
sumer.wi <- summary(bb.m1.2lint)$summary
sumer.wi[grep("mu_", rownames(sumer.wi)),]

## plot data and one model for species 1
subby <- subset(bb.stan)
subby$lat<-subby$provenance.lat
subby<-dplyr::select(subby, -provenance.lat)
plot(resp~chill, data=subby) # should add color coding by datasetID
intercepthere <- sumer.ni[grep("a_sp", rownames(sumer.ni)),1][3]
slopehere <- sumer.ni[grep("b_chill", rownames(sumer.ni)),1][3]
abline(intercepthere, slopehere)

cresp <- ggplot(subby, aes(x=chill, y=resp)) + geom_point(aes(colour=datasetID)) + 
  geom_smooth(aes(x=chill, y=resp),method="lm") + theme(legend.position="none")
fresp <- ggplot(subby, aes(x=force, y=resp)) + geom_point(aes(colour=datasetID)) + 
  geom_smooth(aes(x=force, y=resp),method="lm") + theme(legend.position="none")
presp <- ggplot(subby, aes(x=photo, y=resp)) + geom_point(aes(colour=datasetID)) + 
  geom_smooth(aes(x=photo, y=resp),method="lm") + theme(legend.position="none")
lresp <- ggplot(subby, aes(x=lat, y=resp)) + geom_point(aes(colour=datasetID)) + 
  geom_smooth(aes(x=lat, y=resp),method="lm") + theme(legend.position="none")

grid.arrange(cresp, fresp, presp, lresp, ncol=4, nrow=1)
