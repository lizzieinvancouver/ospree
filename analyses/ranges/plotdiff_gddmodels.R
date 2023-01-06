## Started 6 January 2023 ##
## By Lizzie ##

## Plotting and comparing the GDD models ##
## Code was temporarily in popUP/rangeleadin_osp.R
## See also: https://github.com/lizzieinvancouver/ospree/issues/442

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

# libraries
library(shinystan)
library(reshape2)
library(rstan)
library(ggplot2)
# Setting working directory.
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if(length(grep("dbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

library(ggplot2)

modelmeans <- read.csv("output/gddrangemodelmeans.csv")
modelmeansnam <- subset(modelmeans, continent=="North America")
modelmeanseu <- subset(modelmeans, continent=="europe")

## Sanity checks -- rebuild the math
# namforbeta = namforalpha + forbeta*range
testy <- modelmeans$alphaforce + modelmeans$betatraitforce*modelmeans$rangeclim
plot(modelmeans$betaforce~testy) # Good news, math works!

## Plots -- look at what we want to capture:
# We want to compare how much the full cue is explained by the intercept (alpha) in ...
# Europe versus North America ... so first we plot this so we can visualize it
# Originally we thought somehow overlaying the side-by-side plots could work, but it doesn't (see below)
pchhere <- 16
pdf("figures/diffgddmodels_chill.pdf", height=8, width=10)
par(mfrow=c(2,2))
plot(modelmeansnam$alphachill~modelmeansnam$rangeclim, pch=pchhere,
     main="Chilling: N. America", xlab="Range climate", ylab="unexplained by range")
plot(modelmeansnam$betachill~modelmeansnam$rangeclim, pch=pchhere,
     main="Chilling: N. America", xlab="Range climate", ylab="full cue by range")
text(modelmeansnam$rangeclim+6, modelmeansnam$betachill, labels=modelmeansnam$latbi)
plot(modelmeanseu$alphachill~modelmeanseu$rangeclim, pch=pchhere,
     main="Europe", xlab="Range climate", ylab="unexplained by range")
plot(modelmeanseu$betachill~modelmeanseu$rangeclim, pch=pchhere,
     main="Europe", xlab="Range climate", ylab="full cue by range")
text(modelmeanseu$rangeclim + 0.5, modelmeanseu$betachill, labels=modelmeanseu$latbi)
dev.off()

pdf("figures/diffgddmodels_force.pdf", height=8, width=10)
par(mfrow=c(2,2))
plot(modelmeansnam$alphaforce~modelmeansnam$rangeclim, pch=pchhere,
     main="Forcing: N. America", xlab="Range climate", ylab="unexplained by range")
plot(modelmeansnam$betaforce~modelmeansnam$rangeclim, pch=pchhere,
     main="Forcing: N. America", xlab="Range climate", ylab="full cue by range")
text(modelmeansnam$rangeclim+6, modelmeansnam$betaforce, labels=modelmeansnam$latbi)
plot(modelmeanseu$alphaforce~modelmeanseu$rangeclim, pch=pchhere,
     main="Europe", xlab="Range climate", ylab="unexplained by range")
plot(modelmeanseu$betaforce~modelmeanseu$rangeclim, pch=pchhere,
     main="Europe", xlab="Range climate", ylab="full cue by range")
text(modelmeanseu$rangeclim + 0.5, modelmeanseu$betaforce, labels=modelmeanseu$latbi)
dev.off()

pdf("figures/diffgddmodels_photo.pdf", height=8, width=10)
par(mfrow=c(2,2))
plot(modelmeansnam$alphaphoto~modelmeansnam$rangeclim, pch=pchhere,
     main="Photo: N. America", xlab="Range climate", ylab="unexplained by range")
plot(modelmeansnam$betaphoto~modelmeansnam$rangeclim, pch=pchhere,
     main="Photo: N. America", xlab="Range climate", ylab="full cue by range")
text(modelmeansnam$rangeclim+6, modelmeansnam$betaphoto, labels=modelmeansnam$latbi)
plot(modelmeanseu$alphaphoto~modelmeanseu$rangeclim, pch=pchhere,
     main="Europe", xlab="Range climate", ylab="unexplained by range")
plot(modelmeanseu$betaphoto~modelmeanseu$rangeclim, pch=pchhere,
     main="Europe", xlab="Range climate", ylab="full cue by range")
text(modelmeanseu$rangeclim + 0.5, modelmeanseu$betaphoto, labels=modelmeanseu$latbi)
dev.off()

## How to compare them? Plot comparisons don't work because the scale of each cue in each continent is not held constant
# So a smaller relative change in Europe can look as big as large relative changes in North America
# We tried a few other things but they all seem to run afoul of the differences in both cue magnitude and/or
# range climate across the two continents .... I think we'd have to standardize more (as in pre and post model) to
# get the below ideas to work ... 

# What about ratios?
par(mfrow=c(1,2))
hist(modelmeansnam$alphaforce/modelmeansnam$betaforce)
hist(modelmeanseu$alphaforce/modelmeanseu$betaforce)

# Okay, get out of negative zone ...
valueadded <- 60
namforalphaadd <- modelmeansnam$alphaforce + valueadded
namforbetaaadd <- modelmeansnam$betaforce + valueadded
euforalphaadd <- modelmeanseu$alphaforce + valueadded
euforbetaaadd <- modelmeanseu$betaforce + valueadded

# Get ratios ...
quartz()
par(mfrow=c(1,2))
hist((namforbetaaadd-namforalphaadd)/namforbetaaadd)
hist((euforbetaaadd-euforalphaadd)/euforbetaaadd)

# Take the mean of the absolute differences for the species-level
quartz()
par(mfrow=c(1,2))
hist(abs(modelmeansnam$betaforce-modelmeansnam$alphaforce))
hist(abs(modelmeanseu$betaforce-modelmeanseu$alphaforce))

## So, in the end:
# Variance comparison might be best we can do ... 
var(modelmeansnam$alphaforce)/var(modelmeansnam$betaforce)
var(modelmeanseu$alphaforce)/var(modelmeanseu$betaforce)

var(modelmeansnam$alphachill)/var(modelmeansnam$betachill)
if(FALSE){
modelmeansnamnobl <- subset(modelmeansnam, latbi!="Betula_lenta")
var(modelmeansnamnobl$alphachill)/var(modelmeansnamnobl$betachill)
}
var(modelmeanseu$alphachill)/var(modelmeanseu$betachill)

var(modelmeansnam$alphaphoto)/var(modelmeansnam$betaphoto)
var(modelmeanseu$alphaphoto)/var(modelmeanseu$betaphoto)
