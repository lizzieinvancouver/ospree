## Started 21 Feb 2017 ##
## By Nacho, Lizzie, Dan and others ##

## Plotting results from Stan models ##
## As of July 2019 Lizzie is pretty sure this is code not used in ms, mainly just used in understanding results ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)

## To do!
## Clean up plotting code and add a figure showing all data and sp lines together (well, 3 figures)
## Figure out why sigma_a plots each sp in muplot, but no dice for sigma_bforce etc.

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

figpath <- "figures"

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- TRUE
use.flags.for.allsppmodel <- FALSE # alert! This breaks some of the early code, but makes the cuebycue plot
use.yourown.flagdesign <- FALSE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
    use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
    & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
use.chillports = FALSE
use.zscore = FALSE
use.allspp = FALSE # for the main model this is false
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE
}

if(use.flags.for.allsppmodel){
use.chillports = FALSE
use.zscore = FALSE
use.allspp = TRUE
use.multcuespp = FALSE
use.cropspp = TRUE
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = TRUE # change to false to use raw predictors

# Default is species complex and no crops
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE

# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE

#Default is all chilling data
use.expchillonly = FALSE # change to true for only experimental chilling 
#note: with only exp chilling, there is only exp photo and force too.
#also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
}

## name your figures paths (based on flags above) ... this needs work
if(use.flags.for.mainmodel==TRUE){
    figpathmore <- "mainmodel"
    }
if(use.flags.for.allsppmodel==TRUE){
    figpathmore <- "allsppmodel"
    }
if(use.flags.for.mainmodel==FALSE & use.allspp==FALSE & use.expramptypes.fp==TRUE &
    use.zscore==TRUE){
    figpathmore <- "spcom_expramp_fpz"
    }
if(use.flags.for.mainmodel==FALSE & use.allspp==FALSE & use.expramptypes.fp==TRUE &
    use.zscore==TRUE & use.cropspp==TRUE){
    figpathmore <- "spcomwcrops_expramp_fpz"
    }
if(use.flags.for.allsppmodel==FALSE & use.allspp==TRUE & use.expramptypes.fp==TRUE & use.zscore==TRUE){
    figpathmore <- "allspp_expramp_fpz"
    }
if(use.flags.for.mainmodel==FALSE & use.allspp==FALSE & use.expramptypes.fp==TRUE & use.zscore==FALSE){
    figpathmore <- "spcom_expramp_fp"
    }
if(use.flags.for.allsppmodel==FALSE & use.allspp==TRUE & use.expramptypes.fp==TRUE & use.zscore==FALSE){
    figpathmore <- "allspp_expramp_fp"
    }

##
source("source/bbstanleadin.R")
source("source/bb_muplot.R")
source("source/plotletfx.R")


##

# Quick look at interactions
ggplot(bb.stan, aes(chill, force, color=complex.wname)) +
    geom_point(alpha = 0.1) + 
    theme(legend.position="none")
ggplot(bb.stan, aes(chill.ports, force, color=complex.wname)) +
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

# Set up colors
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4


# Load fitted stan model: no interactions
if(use.flags.for.mainmodel==TRUE){
load("stan/output/m2lni_spcompexprampfputah_z.Rda") # I think this is the main model, need to check!
}
if(use.zscore==TRUE & use.cropspp==FALSE & use.chillports == TRUE){
load("stan/output/m2lni_spcompexprampfp_z.Rda") # m2l.ni
load("stan/output/m2lnib_spcompexprampfp_z.Rda") # m2l.nib

sumer.ni <- summary(m2l.ni)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]

unique(bb.stan$complex) # numbers are alphabetical
sort(unique(bb.stan$complex.wname))
# sumer.ni[grep("chill", rownames(sumer.ni)),] 
# sumer.ni[grep("photo", rownames(sumer.ni)),] # Fagus is #15
# sumer.ni[grep("force", rownames(sumer.ni)),]

# m1.bb <- m2l.ni
modelhere <- m2l.ni
muplotfx(modelhere, "", 7, 8, c(0,3), c(-20, 10) , 12, 3)
}

# non-z-scored models
if(use.zscore==FALSE){
load("stan/output/m2lni_spcompexprampfp_nonz.Rda") # m2l.ni
load("stan/output/m2lnib_spcompexprampfp_nonz.Rda") # m2l.nib
modelhere <- m2l.ni
muplotfx(modelhere, "model", 7, 8, c(0,3), c(-3, 1) , 1.3, 3)
sumer.ni <- summary(m2l.ni)$summary
sumer.nib <- summary(m2l.nib)$summary
whichmodel <- sumer.ni
othermodel <- sumer.nib
}

# z-scored models with crops
if(use.zscore==TRUE & use.cropspp==TRUE){
load("stan/output/m2lni_spcompwcropsexprampfp_z.Rda") # m2l.ni
modelhere <- m2l.ni
muplotfx(modelhere, "model", 7, 8, c(0,3), c(-20, 10) , 12, 3)
}

## scale up: plot each species with slopes from two selected models
if(use.zscore==TRUE & use.cropspp==FALSE){
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
}

# more colors!
spp <- sort(unique(bb.stan$complex))
n <- length(spp)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colv = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(colv, n))

# Same plot as above, but show all species on one plot ....
if(use.zscore==TRUE & use.cropspp==FALSE){
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
}


# As above without z-scored data
if(use.zscore==FALSE){
pdf(file.path(figpath, "modelscompare.ni.combined.nonz.pdf"), width = 7, height = 3.5)
par(mfrow=c(1,3))
plot(resp~chill.ports, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~chill.ports, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
    abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
           whichmodel[grep("mu_b_chill_sp", rownames(whichmodel)),1], col="black", lwd=3)
    abline(othermodel[grep("mu_a_sp", rownames(othermodel)),1],
           othermodel[grep("b_chill", rownames(othermodel)),1], col="blue", lwd=2)

plot(resp~force, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~force, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_force", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
    abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
           whichmodel[grep("mu_b_force_sp", rownames(whichmodel)),1], col="black", lwd=3)
    abline(othermodel[grep("mu_a_sp", rownames(othermodel)),1],
           othermodel[grep("b_force", rownames(othermodel)),1], col="blue", lwd=2)

plot(resp~photo, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~photo, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_photo", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
    abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
           whichmodel[grep("mu_b_photo_sp", rownames(whichmodel)),1], col="black", lwd=3)
    abline(othermodel[grep("mu_a_sp", rownames(othermodel)),1],
           othermodel[grep("b_photo", rownames(othermodel)),1], col="blue", lwd=2)

dev.off()
}


# Let's plot interactions in data by species ...
colz <- c("red","blue","green", "orange", "brown", "grey", "black")
pdf(file.path(figpath, "cues.pdf"), width = 7, height = 3.5)
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

# same, but with chillports
pdf(file.path(figpath, "cues.chillports.pdf"), width = 7, height = 3.5)
spp <- sort(unique(bb.stan$complex))
for (sp in c(1:length(spp))){# i = 1
    par(mfrow=c(1,3))
    subby <- subset(bb.stan,  complex==spp[sp])
    plot(subby[["chill.ports"]], subby[["force"]], col=colz[as.factor(subby$datasetID)],
         main=subby$complex.wname[1], xlab="chilling", ylab="forcing")
    # abline(lm(subby[["chill"]]~subby[["force"]]))
    plot(subby[["chill.ports"]], subby[["photo"]], col=colz[as.factor(subby$datasetID)],
         xlab="chilling", ylab="photo")
    # abline(lm(subby[["chill"]]~subby[["photo"]]))
    plot(subby[["force"]], subby[["photo"]], col=colz[as.factor(subby$datasetID)],
         xlab="photo", ylab="forcing")
   # abline(lm(subby[["force"]]~subby[["photo"]]))
}
dev.off()



# Cue by cue plots

if(FALSE){ # For running this on 18 March 2021, without running the above code .. two models I have are ...
load("stan/output/m2lni_spcompexprampfputah_z.Rda") # m2l.ni
# load("stan/output/m2lni_spcompexprampfpcp_nonz.Rda") # m2l.ni
sumer.ni <- summary(m2l.ni)$summary
}

colz = c("brown", "blue3")
spp <- sort(unique(bb.stan$complex.wname))

if(use.flags.for.allsppmodel==TRUE){
load("stan/output/m2lni_allsppwcrop_utah_nonz.Rda") # I think this is the allspp model, need to check!
spp <- sort(unique(bb.stan$latbi))
sumer.ni <- summary(m2l.ni)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]
}

ylimhere <- c(-20, 5)
xlimhere <- c(-20, 5)

pdf(file.path(paste(figpath, "/cuebycue/", figpathmore, "model_cuebycue.pdf", sep="")), width = 7, height = 7)
par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Budburst \n Change (days) due to warming", font = 2, srt = 90) # \n\n add two line breaks

plotlet("b_photo", "b_force", 1, 1, 
         ylim = ylimhere,
         xlim = xlimhere,
         #  xaxt="n", 
         # group = treeshrub,
         data = sumer.ni)

plotlet("b_chill", "b_force", 1, 1, 
        ylim = ylimhere,
        xlim = xlimhere,
        yaxt="n",
        # xaxt="n", 
        # group = treeshrub,
        data = sumer.ni)
axis(2, seq(0, ylimhere[1], by = -5), labels = FALSE)

plotblank()
text(5,5, "Budburst \n Change (days) due to warming", font = 2, srt = 90)

plotlet("b_photo", "b_force", 1, 1, 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = ylimhere,
        xlim = xlimhere,
        #group = treeshrub,
        data = sumer.ni)

plotlet("b_chill", "b_force", 1, 1, 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = ylimhere,
        xlim = xlimhere,
        yaxt="n",
        # group = treeshrub,
        data = sumer.ni)
axis(2, seq(0, ylimhere[1], by = -5), labels = FALSE)

plotblank()

plotblank()
text(5.5, 5, "Change (days) due to longer photoperiod", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to change in chilling", font = 2, pos = 3)

dev.off();#system(paste("open", file.path(figpath, "Fig2_4panel.pdf"), "-a /Applications/Preview.app"))

## Plot with chilling x photoperiod ... (very hacky!)
pdf(file.path(paste(figpath, "/cuebycue/", figpathmore, "model_cuebycuephoto.pdf", sep="")), width = 7, height = 7)
par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Budburst \n Change (days) due to photoperiod", font = 2, srt = 90) # \n\n add two line breaks

plotlet("b_photo", "b_photo", 1, 1,
         ylim = ylimhere,
         xlim = xlimhere,
         data = sumer.ni)

plotlet("b_chill", "b_photo", 1, 1, 
        ylim = ylimhere,
        xlim = xlimhere,
        yaxt="n",
        data = sumer.ni)
axis(2, seq(0, ylimhere[1], by = -5), labels = FALSE)

plotblank()
text(5,5, "Budburst \n Change (days) due to longer photoperiod", font = 2, srt = 90)

plotlet("b_photo", "b_photo", 1, 1,
        ylim = ylimhere,
        xlim = xlimhere,
        data = sumer.ni)

plotlet("b_chill", "b_photo", 1, 1,  
        ylim = ylimhere,
        xlim = xlimhere,
        yaxt="n",
        data = sumer.ni)
axis(2, seq(0, ylimhere[1], by = -5), labels = FALSE)

plotblank()

plotblank()
text(5.5, 5, "Change (days) due to longer photoperiod", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to change in chilling", font = 2, pos = 3)

dev.off();

