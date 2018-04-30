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
source("source/bb_muplot.R")

sumer.ni <- summary(m2l.ni)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]


# Load fitted stan model: no interactions with studyid
load("stan/output/M1_daysBBnointer_2level_studyint.Rda")
m1.bb.study <- m2l.nistudy
# summary(m1.bb.study)

sumer.nistudy <- summary(m2l.nistudy)$summary
sumer.nistudy[grep("mu_", rownames(sumer.nistudy)),]

# Load fitted stan model: with interactions
load("stan/output/M1_daysBBwinter_2level.Rda")
m1.bb <- m2l.wi
# summary(m1.bb)

source("source/bb_muplot.R")
sumer.wi <- summary(m2l.wi)$summary
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



## scale up: plot each species with slopes from the two selected models
whichmodel <- sumer.nistudy
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
