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

library(gridExtra)
library(plyr)
library(dplyr)

use.zscore = FALSE

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

figpath <- "figures"

##
source("source/bbstanleadin.R")
##

# Quick look at interactions
plot(force~chill, data=bb.stan)
plot(force~photo, data=bb.stan)
plot(photo~chill, data=bb.stan)

hist(bb.stan$force)
mean(bb.stan$force)
lowforce <- subset(bb.stan, force>14)
hiforce <- subset(bb.stan, force<20)

hist(bb.stan$photo)
mean(bb.stan$photo)
lowphoto <- subset(bb.stan, photo>12)
hiphoto <- subset(bb.stan, photo<15)

hist(bb.stan$chill)
mean(bb.stan$chill)
lowchill <- subset(bb.stan, chill>3)
hichill <- subset(bb.stan, chill<6)

intxnplot <- function(lowdf, hidf){
    par(mfrow=c(3,2))
    plot(resp~force, lowdf, main="low")
    abline(lm(resp~force, lowdf))
    plot(resp~force, hidf, main="high")
    abline(lm(resp~force, hidf))
    plot(resp~photo, lowdf)
    abline(lm(resp~photo, lowdf))
    plot(resp~photo, hidf)
    abline(lm(resp~photo, hidf))
    plot(resp~chill, lowdf)
    abline(lm(resp~chill, lowdf))
    plot(resp~chill, hidf)
    abline(lm(resp~chill, hidf))
}

pdf(file.path(figpath, "intxn_force.pdf"), width = 5, height = 8)
intxnplot(lowforce, hiforce)
dev.off()

pdf(file.path(figpath, "intxn_photo.pdf"), width = 5, height = 8)
intxnplot(lowphoto, hiphoto)
dev.off()

pdf(file.path(figpath, "intxn_chill.pdf"), width = 5, height = 8)
intxnplot(lowchill, hichill)
dev.off()

# Load fitted stan model: no interactions
load("stan/output/M1_daysBBnointer_2level.Rda")
m1.bb <- m2l.ni
# summary(m1.bb)

# explore results in shinystan
#launch_shinystan(m1.bb)

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
source("source/bb_muplot.R") # this code needs to be adjusted! I don't think it's plotting what it says it is.

sumer.ni <- summary(m2l.ni)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]

if(!use.zscore){
# Load fitted stan model: with interactions
load("stan/output/M1_daysBBwinter_2level.Rda")
m1.bb <- m2l.winsp
# summary(m1.bb)

sumer.wi <- summary(m2l.winsp)$summary
sumer.wi[grep("mu_", rownames(sumer.wi)),]
sumer.wi[c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp", "b_cf","b_cp","b_fp"),] # mu_a_sp
source("source/bb_muplot_m2l.winsp.R") # file also contains code for colored by species
}

unique(bb.stan$complex) # numbers are alphabetical I believe (checked head and tail on the data ...)
unique(bb.stan$complex.wname)
# sumer.wi[grep("chill", rownames(sumer.wi)),] # positive outlier is Ribes
# sumer.wi[grep("photo", rownames(sumer.wi)),] # Fagus is #15, positive outlier is Picea abies
# sumer.wi[grep("force", rownames(sumer.wi)),]

if(use.zscore){
# Load fitted stan model: with interactions -- z-scored data
load("stan/output/M1_daysBBwinter_2levelz.Rda")
m1.bbz <- m2l.winsp
sumer.wi <- summary(m2l.winsp)$summary
sumer.wi[c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp", "b_cf","b_cp","b_fp"),]
source("source/bb_muplot_m2l.winspz.R") # file also contains code for colored by species

fu2015spp <- c("Aesculus_hippocastanum", "Alnus_glutinosa", "Betula_pendula", "Fagus_sylvatica",
    "Fraxinus_excelsior", "Quercus_robur", "Tilia_cordata")
fu2015sppnum <- which(unique(bb.stan$complex.wname) %in% fu2015spp)
fu2015spp.cues <- data.frame(spp=fu2015spp, force=rep(NA, 7), photo=rep(NA, 7), chill=rep(NA, 7))
for (sp in c(1:7)){
    fu2015spp.cues$force[sp] <- sumer.wi[paste("b_force[", fu2015sppnum[sp], "]", sep=""),1]
    fu2015spp.cues$photo[sp] <- sumer.wi[paste("b_photo[", fu2015sppnum[sp], "]", sep=""),1]
    fu2015spp.cues$chill[sp] <- sumer.wi[paste("b_chill[", fu2015sppnum[sp], "]", sep=""),1]
    }
fu2015spp.cues

allspp.cues <- data.frame(
    force=sumer.wi[grep("b_force", rownames(sumer.wi)),][3:40,1],
    photo=sumer.wi[grep("b_photo", rownames(sumer.wi)),][3:40,1],
    chill=sumer.wi[grep("b_chill", rownames(sumer.wi)),][3:40,1])


par(mfrow=c(1,3))
plot(force~photo, data=allspp.cues)
plot(force~chill, data=allspp.cues)
plot(photo~chill, data=allspp.cues)
    
}

# Need to work more on below
if(FALSE){
# exploring unscaled-data intxns
getintxns <- sumer.wi[c("b_cf","b_cp","b_fp"),]
intxnhere <- getintxns[1,1] # cf is 0.12
forcenums <- seq(8,25, by=0.01)
colhere <- "deepskyblue"
plot(forcenums*2*intxnhere~forcenums, ylab="estimated effect", xlim=c(5, 30), ylim=c(0, 50),
    xlab="forcing temp", col=alpha(colhere, 0.2), type="l")
lines(forcenums*4*intxnhere~forcenums, ylab="estimated effect", xlab="forcing temp", col=alpha(colhere, 0.8))
lines(abs(sumer.wi[c("mu_b_force_sp"),][1]*forcenums)~forcenums)
}

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



## scale up: plot each species with slopes from the two selected models
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
    # abline(lm(subby[["chill"]]~subby[["force"]]))
    plot(subby[["chill"]], subby[["photo"]], , col=colz[as.factor(subby$datasetID)],
         xlab="chilling", ylab="photo")
    # abline(lm(subby[["chill"]]~subby[["photo"]]))
    plot(subby[["force"]], subby[["photo"]], , col=colz[as.factor(subby$datasetID)],
         xlab="photo", ylab="forcing")
   # abline(lm(subby[["force"]]~subby[["photo"]]))
}
dev.off()
