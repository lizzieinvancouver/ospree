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

library(rstan)
library(ggplot2)
library(shinystan)
library(gridExtra)
library(plyr)
library(dplyr)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

source('stan/savestan.R')

figpath <- "bb_analysis/figures"

##
# get the data (taken from bbmodels_stan.R)
source("bb_analysis/source/bbdataplease.R")

## make a bunch of things numeric (eek!)
bb$force <- as.numeric(bb$forcetemp)
bb$photo <- as.numeric(bb$photoperiod_day)
bb$chill <- as.numeric(bb$Total_Chilling_Hours)
bb$resp <- as.numeric(bb$response.time)

## For centering data, not doing it for now
#bb$photo.cen <- scale(bb$photo, center=TRUE, scale=TRUE)
#bb$force.cen <- scale(bb$force, center=TRUE, scale=TRUE)
#bb$chill.cen <- scale(bb$chill, center=TRUE, scale=TRUE)

## subsetting data, preparing genus variable, removing NAs
bb.prepdata <- subset(bb, select=c("datasetID", "resp", "chill", "photo", "force", "complex"))
dim(subset(bb, is.na(chill)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE))
bb.stan <- bb.prepdata[complete.cases(bb.prepdata),]
bb.stan$complex.wname <- bb.stan$complex
bb.stan$complex <- as.numeric(as.factor(bb.stan$complex))

# remove the two values above 600
bb.stan <- subset(bb.stan, resp<600)

# adjust chilling (if needed)
bb.stan$chill <- bb.stan$chill/240
##
##

# Load fitted stan model: no interactions
load("stan/bb/output/M1_daysBBnointer_2level.Rda")
m1.bb <- bb.m1.2l
# summary(m1.bb)

# explore results in shinystan
#launch_shinystan(m1.bb)

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
source("bb_analysis/source/bb_muplot.R")

sumer.ni <- summary(bb.m1.2l)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]


# Load fitted stan model: no interactions
load("stan/bb/output/M1_daysBBwinter_2level.Rda")
m1.bb <- bb.m1.2lint
# summary(m1.bb)

source("bb_analysis/source/bb_muplot.R")
sumer.wi <- summary(bb.m1.2lint)$summary
sumer.wi[grep("mu_", rownames(sumer.wi)),]

## plot data and one model for species 1
subby <- subset(bb.stan, complex==1)
plot(resp~chill, data=subby) # should add color coding by datasetID
intercepthere <- sumer.ni[grep("a_sp", rownames(sumer.ni)),1][3]
slopehere <- sumer.ni[grep("b_chill", rownames(sumer.ni)),1][3]
abline(intercepthere, slopehere)

plot(resp~force, data=subby) # should add color coding by datasetID
intercepthere <- sumer.ni[grep("a_sp", rownames(sumer.ni)),1][3]
slopehere <- sumer.ni[grep("b_force", rownames(sumer.ni)),1][3]
abline(intercepthere, slopehere)

plot(resp~photo, data=subby) # should add color coding by datasetID
intercepthere <- sumer.ni[grep("a_sp", rownames(sumer.ni)),1][3]
slopehere <- sumer.ni[grep("b_photo", rownames(sumer.ni)),1][3]
abline(intercepthere, slopehere)

# ggplot version of above, but not sure how to add ablines... 
cresp <- ggplot(subby, aes(x=chill, y=resp, colour=datasetID)) + geom_point()
fresp <- ggplot(subby, aes(x=force, y=resp, colour=datasetID)) + geom_point()
presp <- ggplot(subby, aes(x=photo, y=resp, colour=datasetID)) + geom_point()

grid.arrange(cresp, fresp, presp, ncol=3, nrow=1)



## scale up: plot each species with slopes from no interaction and with interactions model
whichmodel <- sumer.ni
othermodel <- sumer.wi
pdf(file.path(figpath, "M1inter.pdf"), width = 7, height = 3.5)
spp <- unique(bb.stan$complex)
for (sp in c(1:length(spp))){
    par(mfrow=c(1,3))
    subby <- subset(bb.stan, complex==spp[sp])
    # chilling
    plot(resp~chill, data=subby, main=subby$complex.wname[1]) 
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_chill", rownames(othermodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col="blue")
    # forcing 
    plot(resp~force, data=subby) # should add color coding by datasetID
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_force", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_force", rownames(othermodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col="blue")
    # photo
    plot(resp~photo, data=subby) # should add color coding by datasetID
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_photo", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_photo", rownames(othermodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col="blue")
}
dev.off()

# Let's plot interactions in data by species ...
colz <- c("red","blue","green", "orange", "brown", "grey", "black")

pdf(file.path(figpath, "M1inter_collinear.pdf"), width = 7, height = 3.5)
spp <- unique(bb.stan$complex)
for (sp in c(1:length(spp))){# i = 1
    par(mfrow=c(1,3))
    subby <- subset(bb.stan,  complex==spp[sp])
    plot(subby[["chill"]], subby[["force"]], , col=colz[as.factor(subby$datasetID)],
         main=subby$complex.wname[1], xlab="chilling", ylab="forcing")
    # abline(lm(subby[["chill"]]~ subby[["force"]]))
    plot(subby[["chill"]], subby[["photo"]], , col=colz[as.factor(subby$datasetID)],
         xlab="chilling", ylab="photo")
    plot(subby[["force"]], subby[["photo"]], , col=colz[as.factor(subby$datasetID)],
         xlab="photo", ylab="forcing")
}
dev.off()
