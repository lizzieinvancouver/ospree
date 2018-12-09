## Started 21 Feb 2017 ##
## By Nacho, Lizzie, Dan and others ##

## Plotting results from Stan models ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)

## To do!
## Clean up plotting code and add a figure showing all data and sp lines together (well, 3 figures)
## Figure out why sigma_a plots each sp in muplot, but no dice for sigma_bforce etc.

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

figpath <- "figures"

## set up the flags
use.chillports = FALSE 
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

## name your figures paths (based on flags above) ... this needs work
if(use.allspp==FALSE & use.expramptypes.fp==TRUE){
    figpathmore <- "spcom.expramp.fp"
    }
if(use.allspp==TRUE & use.expramptypes.fp==TRUE){
    figpathmore <- "allspp.expramp.fp"
    }


##

##
source("source/bbstanleadin.R")
source("source/bb_muplot.R")

##

# Quick look at interactions
ggplot(bb.stan, aes(chill, force, color=complex.wname)) +
    geom_point(alpha = 0.1) + 
    theme(legend.position="none")
ggplot(bb.stan, aes(photo, force, color=complex.wname)) +
    geom_point(alpha = 0.1) + 
    theme(legend.position="none")
ggplot(bb.stan, aes(chill, photo, color=complex.wname)) +
    geom_point(alpha = 0.1) + 
    theme(legend.position="none")

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

pdf(file.path(figpath, paste("intxn_force", figpathmore, ".pdf", sep="")), width = 5, height = 8)
intxnplot(lowforce, hiforce)
dev.off()

pdf(file.path(figpath, paste("intxn_photo", figpathmore, ".pdf", sep="")), width = 5, height = 8)
intxnplot(lowphoto, hiphoto)
dev.off()

pdf(file.path(figpath, paste("intxn_chill", figpathmore, ".pdf", sep="")), width = 5, height = 8)
intxnplot(lowchill, hichill)
dev.off()

# Load fitted stan model: no interactions
load("stan/output/m2lni_alltypes.Rda")
load("stan/output/m2lnib_alltypes.Rda") # m2l.nib

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

sumer.ni <- summary(m2l.ni)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]

unique(bb.stan$complex) # numbers are alphabetical
sort(unique(bb.stan$complex.wname))
# sumer.ni[grep("chill", rownames(sumer.ni)),] 
# sumer.ni[grep("photo", rownames(sumer.ni)),] # Fagus is #15
# sumer.ni[grep("force", rownames(sumer.ni)),]

# m1.bb <- m2l.ni
modelhere <- m2l.ni
muplotfx(modelhere, "model", 7, 8, c(0,3), c(-20, 10) , 12, 3)

## scale up: plot each species with slopes from two selected models
sumer.nib <- summary(m2l.nib)$summary
whichmodel <- sumer.ni
othermodel <- sumer.nib

pdf(file.path(figpath, "modelscompare.ni.pdf"), width = 10, height = 3.5)
spp <- sort(unique(bb.stan$complex))
for (sp in c(1:length(spp))){
    par(mfrow=c(1,3))
    subby <- subset(bb.stan, complex==spp[sp])
    # chilling
    # could add color coding by datasetID
    plot(resp~chill.z, data=subby, main=subby$complex.wname[1])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_chill", rownames(othermodel)),1]
    abline(intercepthere, slopehere, col="blue")
    # forcing 
    plot(resp~force.z, data=subby) 
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_force", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_force", rownames(othermodel)),1]
    abline(intercepthere, slopehere, col="blue")
    # photo
    plot(resp~photo.z, data=subby) 
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_photo", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_sp", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_photo", rownames(othermodel)),1]
    abline(intercepthere, slopehere, col="blue")
}
dev.off()

# more colors!
n <- length(spp)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colv = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(colv, n))

# Same plot as above, but show all species on one plot ....
pdf(file.path(figpath, "modelscompare.ni.combined.pdf"), width = 7, height = 3.5)
par(mfrow=c(1,3))
plot(resp~chill.z, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~chill.z, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
    abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
           whichmodel[grep("mu_b_chill_sp", rownames(whichmodel)),1], col="black", lwd=3)
    abline(othermodel[grep("mu_a_sp", rownames(othermodel)),1],
           othermodel[grep("b_chill", rownames(othermodel)),1], col="blue", lwd=2)

plot(resp~force.z, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~force.z, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_force", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
    abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
           whichmodel[grep("mu_b_force_sp", rownames(whichmodel)),1], col="black", lwd=3)
    abline(othermodel[grep("mu_a_sp", rownames(othermodel)),1],
           othermodel[grep("b_force", rownames(othermodel)),1], col="blue", lwd=2)

plot(resp~photo.z, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~photo.z, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_photo", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
    abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
           whichmodel[grep("mu_b_photo_sp", rownames(whichmodel)),1], col="black", lwd=3)
    abline(othermodel[grep("mu_a_sp", rownames(othermodel)),1],
           othermodel[grep("b_photo", rownames(othermodel)),1], col="blue", lwd=2)

dev.off()


# Let's plot interactions in data by species ...
colz <- c("red","blue","green", "orange", "brown", "grey", "black")

pdf(file.path(figpath, "data_collinear.pdf"), width = 7, height = 3.5)
spp <- sort(unique(bb.stan$complex))
for (sp in c(1:length(spp))){# i = 1
    par(mfrow=c(1,3))
    subby <- subset(bb.stan,  complex==spp[sp])
    plot(subby[["chill"]], subby[["force"]], col=colz[as.factor(subby$datasetID)],
         main=subby$complex.wname[1], xlab="chilling", ylab="forcing")
    # abline(lm(subby[["chill"]]~subby[["force"]]))
    plot(subby[["chill"]], subby[["photo"]], col=colz[as.factor(subby$datasetID)],
         xlab="chilling", ylab="photo")
    # abline(lm(subby[["chill"]]~subby[["photo"]]))
    plot(subby[["force"]], subby[["photo"]], col=colz[as.factor(subby$datasetID)],
         xlab="photo", ylab="forcing")
   # abline(lm(subby[["force"]]~subby[["photo"]]))
}
dev.off()

