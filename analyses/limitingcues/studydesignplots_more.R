## Started 7 May 2018 ##
## By Lizzie ##

## TO DO ##
# (1) Should we get the centroid of the lat/long points for each study?
# Right now I cheaply take average lat and average long #
# (2) Calculate mean, min, max field sample date and plot

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)


# the below should already have cleaned lat/long
dat <- read.csv("output/ospree_clean.csv",header = TRUE)

# format species and date
dat$latbi <- paste(dat$genus, dat$species)
dat$fieldsample.date <- as.Date(dat$fieldsample.date, format="%d-%b-%Y")
dat$doy <- format(dat$fieldsample.date, "%j")

# make a bunch of things numeric (eek!)
dat$forcetemp <- as.numeric(dat$forcetemp)
dat$forcetemp_night <- as.numeric(dat$forcetemp_night)
dat$photoperiod_night <- as.numeric(dat$photoperiod_night)
dat$photoperiod_day <- as.numeric(dat$photoperiod_day)
dat$chilltemp <- as.numeric(dat$chilltemp)
dat$doy <- as.numeric(dat$doy)


columnstokeep <- c("datasetID", "study", "genus", "species", "latbi", "varetc", 
    "woody","provenance.lat", "provenance.long", "material", "year", 
    "fieldsample.date", "doy", "forcetemp", "forcetemp_night",  
    "photoperiod_day","photoperiod_night","chilltemp", "chillphotoperiod",
    "chilldays", "response", "response.time")        
                   

d <- subset(dat, select=c(columnstokeep))

# Summarize the forcetemp by photoperiod
d$force <- d$forcetemp
d$force[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE] <-
    (d$forcetemp[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE]*
    d$photoperiod_day[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE] +
    d$forcetemp_night[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE]*
    d$photoperiod_night[is.na(d$forcetemp_night)==FALSE & is.na(d$photoperiod_day)==FALSE &
    is.na(d$photoperiod_night)==FALSE])/24


## summarizing data
dsumm <-
      ddply(d, c("datasetID", "study"), summarise,
      mean.lat = mean(provenance.lat),
      mean.long = mean(provenance.long),
      mean.year = mean(year),
      mean.temp = mean(force),
      min.temp = min(force),
      max.temp = max(force),
      sd.temp = sd(force),
      mean.photo = mean(photoperiod_day),
      min.photo = min(photoperiod_day),
      max.photo = max(photoperiod_day),
      sd.photo = sd(photoperiod_day),
      mean.chill = mean(chilltemp),
      min.chill = min(chilltemp),
      max.chill = max(chilltemp),
      sd.chill = sd(chilltemp),
      spp.n = length(unique(latbi)),
      field.sample.n = length(unique(fieldsample.date)),
      mean.fieldsamp = mean(doy),
      min.fieldsamp = min(doy),
      max.fieldsamp = max(doy))

dsumm$range.temp <- dsumm$max.temp - dsumm$min.temp
dsumm$range.photo <- dsumm$max.photo - dsumm$min.photo
dsumm$range.chill <- dsumm$max.chill - dsumm$min.chill

dsumm.yr <-
      ddply(dsumm, c("mean.year"), summarise,
      mean.lat = mean(mean.lat),
      mean.long = mean(mean.long),
      mean.temp = mean(mean.temp),
      mean.min.temp = mean(min.temp),
      mean.max.temp = mean(max.temp),
      mean.photo = mean(mean.photo),
      mean.min.photo = mean(min.photo),
      mean.max.photo = mean(max.photo),
      mean.chill = mean(mean.chill),
      mean.min.chill = mean(min.chill),
      mean.max.chill = mean(max.chill),
      spp.n = mean(spp.n),
      field.sample.n = mean(field.sample.n),
      mean.fieldsamp = mean(mean.fieldsamp))

pdf("limitingcues/figures/timeseries.pdf", width = 6, height = 7)
par(mfrow=c(3,2))
plot(spp.n~mean.year, data=dsumm.yr, type="l")
plot(field.sample.n~mean.year, data=dsumm.yr, type="l")
plot(mean.temp~mean.year, data=dsumm.yr, type="l")
plot(mean.max.temp~mean.year, data=dsumm.yr, type="l")
plot(mean.photo~mean.year, data=dsumm.yr, type="l")
plot(mean.chill~mean.year, data=dsumm.yr, type="l")
dev.off()


colz <- topo.colors(90, alpha = 0.5)
plotxydat <- function(yvar, xvar, bywhat,  dat, legendwhere){
    plot(dat[[yvar]]~dat[[xvar]], ylab=yvar, xlab=xvar, type="n")
    for (i in c(1:length(unique(dat[[bywhat]])))){
        subby <- dat[which(dat[[bywhat]]==unique(dat[[bywhat]])[i]),]
        points(subby[[yvar]]~subby[[xvar]], ylab="", xlab="",
            col=colz[i], pch=16)
        }
    # legend(legendwhere, unique(dat[[bywhat]]), col=colz, pch=16,
     #    cex=0.2, bty="n")
    abline(lm(dat[[yvar]]~dat[[xvar]]))
    summary(lm(dat[[yvar]]~dat[[xvar]]))
           }

plotxydatabsX <- function(yvar, xvar, bywhat,  dat, legendwhere){
    plot(dat[[yvar]]~abs(dat[[xvar]]), ylab=yvar, xlab=xvar, type="n")
    for (i in c(1:length(unique(dat[[bywhat]])))){
        subby <- dat[which(dat[[bywhat]]==unique(dat[[bywhat]])[i]),]
        points(subby[[yvar]]~abs(subby[[xvar]]), ylab="", xlab="",
            col=colz[i], pch=16)
        }
    # legend(legendwhere, unique(dat[[bywhat]]), col=colz, pch=16,
     #    cex=0.2, bty="n")
    abline(lm(dat[[yvar]]~dat[[xvar]]))
    summary(lm(dat[[yvar]]~dat[[xvar]]))
           }

## plotting!


pdf("limitingcues/figures/tempxlat.pdf", width = 8, height = 6)
par(mfrow=c(2,2))
plotxydat("mean.temp", "mean.lat", "datasetID", dsumm, "topleft") # -0.1
plotxydat("min.temp", "mean.lat", "datasetID", dsumm, "topleft") # -0.1
plotxydat("max.temp", "mean.lat", "datasetID", dsumm, "topleft") # -0.08
plotxydat("range.temp", "mean.lat", "datasetID", dsumm, "topleft") # NR
dev.off()

# hmm, correcting the latitude doesn't seem to change temp or photo answers much
pdf("limitingcues/figures/tempxlatcorr.pdf", width = 8, height = 6)
par(mfrow=c(2,2))
plotxydatabsX("mean.temp", "mean.lat", "datasetID", dsumm, "topleft") # -0.1
plotxydatabsX("min.temp", "mean.lat", "datasetID", dsumm, "topleft") # -0.1
plotxydatabsX("max.temp", "mean.lat", "datasetID", dsumm, "topleft") # -0.08
plotxydatabsX("range.temp", "mean.lat", "datasetID", dsumm, "topleft") # NR
dev.off()

pdf("limitingcues/figures/photoxlat.pdf", width = 8, height = 6)
par(mfrow=c(2,2))
plotxydat("mean.photo", "mean.lat", "datasetID", dsumm, "topleft") # NR (but 24 only >60 deg)
plotxydat("min.photo", "mean.lat", "datasetID", dsumm, "topleft") # NR
plotxydat("max.photo", "mean.lat", "datasetID", dsumm, "topleft") # 0.08
plotxydat("range.photo", "mean.lat", "datasetID", dsumm, "topleft") # 0.08
dev.off()

pdf("limitingcues/figures/photoxlatcorr.pdf", width = 8, height = 6)
par(mfrow=c(2,2))
plotxydatabsX("mean.photo", "mean.lat", "datasetID", dsumm, "topleft") # NR
plotxydatabsX("min.photo", "mean.lat", "datasetID", dsumm, "topleft") # NR
plotxydatabsX("max.photo", "mean.lat", "datasetID", dsumm, "topleft") # 0.08
plotxydatabsX("range.photo", "mean.lat", "datasetID", dsumm, "topleft") # 0.08
dev.off()

pdf("limitingcues/figures/chillxlat.pdf", width = 8, height = 6)
par(mfrow=c(2,2))
plotxydat("mean.chill", "mean.lat", "datasetID", dsumm, "topleft") # -0.09
plotxydat("min.chill", "mean.lat", "datasetID", dsumm, "topleft") # -0.1
plotxydat("max.chill", "mean.lat", "datasetID", dsumm, "topleft") # -0.07
plotxydat("range.chill", "mean.lat", "datasetID", dsumm, "topleft") # NR
dev.off()



## TO DO for heatmap:
# (1) Make sure I am counting correctly
# (2) What to do with NA?
# (3) And to include chilling ...

## Summarizing data
d$force.int <- as.integer(d$force)
d$photo.int <- as.integer(d$photoperiod_day)
d$chill.int <- as.integer(d$chilltemp)

dsumm.treat <-
      ddply(d, c("datasetID", "study", "force.int", "photo.int", "chill.int"), summarise,
      mean.lat = mean(provenance.lat),
      mean.long = mean(provenance.long),
      mean.year = mean(year),
      spp.n = length(unique(latbi)),
      field.sample.n = length(unique(fieldsample.date)),
      mean.fieldsamp = mean(doy),
      min.fieldsamp = min(doy),
      max.fieldsamp = max(doy))

dsumm.nums <-
      ddply(dsumm.treat, c("force.int", "photo.int"), summarise,
      count = length(force.int))
# dsumm.nums[is.na(dsumm.nums)] <- 0

pdf("limitingcues/figures/heatmapforcexphoto.pdf", width = 6, height = 6)
ggplot(dsumm.nums, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=count)) +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred")
dev.off()

dsumm.numsch <-
      ddply(dsumm.treat, c("chill.int", "photo.int"), summarise,
      count = length(chill.int))
# dsumm.numsch[is.na(dsumm.numsch)] <- 0

pdf("limitingcues/figures/heatmapchillxphoto.pdf", width = 6, height = 6)
ggplot(dsumm.numsch, aes(as.factor(chill.int), as.factor(photo.int))) +
    geom_tile(aes(fill=count), colour="white") +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred")
dev.off()

