## Started 7 May 2018 ##
## By Lizzie ##
## Updated by Dan in later 2018; updated by Lizzie in February 2019 ##

## TO DO ##
# (1) Should we get the centroid of the lat/long points for each study? (Dan)
# Right now I cheaply take average lat and average long #
# (2) Calculate mean, min, max field sample date and plot (semi-done)
# (3) Work on heat-maps, more notes on this below

## SEE ALSO! studydesignplotsbb.R #

## Note: 9 July 2018 -- I did some checks of my code against counts of the data and against Cat's studytable output ...
# and it looks good. ##

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
library(geosphere)
library(viridis)

source("misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("misc/getcuesbystudy_fxs.R") # f(x) counts up cues

# the below should already have cleaned lat/long
dat <- read.csv("output/ospree_clean.csv",header = TRUE)
dat <- dat[dat$woody=="yes",]

# format species and date
dat$latbi <- paste(dat$genus, dat$species)
dat$fieldsample.date <- as.Date(dat$fieldsample.date, format="%d-%b-%Y")
dat$doy <- format(dat$fieldsample.date, "%j")

# a look at what we lose in converting to numeric
if(FALSE){
sort(unique(dat$forcetemp))
sort(unique(dat$forcetemp_night))
sort(unique(dat$photoperiod_night))
sort(unique(dat$photoperiod_day))
sort(unique(dat$chilltemp))
}

# re-label some non-numerics to add back in later ...
dat$photoext <- dat$photoperiod_day
dat$photoext[dat$photoext=="13-9.5"|dat$photoext=="14-9.5"] <- "variable"
dat$photoext[dat$photoext=="constant"|dat$photoext=="ambient"] <- "ambient" # looked up the constant paper (karlsson03) and it's room ambient daylength I think
dat$forceext <- dat$forcetemp
dat$forceext[grep("ambient", dat$forceext)] <- "ambient"
dat$forceext[dat$forceext=="0 ramped up 3 degrees every 6 days"|dat$forceext=="18-27 (20 average)"|
    dat$forceext=="7-27.5"|dat$forceext=="22-27"|dat$forceext=="mean of 9, 12, 15"|
    dat$forceext=="meandaily"] <- "variable"
dat$chillext <- dat$chilltemp
dat$chillext[dat$chillext=="Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C"] <- "variable"
dat$chillext[grep("\\, ", dat$chillext)] <- "variable"
dat$chillext[dat$chillext=="negative 23 to 13 degrees Celsius"] <- "variable"
dat$chillext[dat$chillext=="<16"|dat$chillext==">16"|dat$chillext=="ambient plus days at 4C"|
    dat$chillext=="neg 3,2"|dat$chillext=="Low"| dat$chillext=="High"|dat$chillext=="elevated"|
    dat$chillext=="6,.5"] <- "other"

# make a bunch of things numeric (eek!)
dat$forcetemp <- as.numeric(dat$forcetemp)
dat$forcetemp_night <- as.numeric(dat$forcetemp_night)
dat$photoperiod_night <- as.numeric(dat$photoperiod_night)
dat$photoperiod_day <- as.numeric(dat$photoperiod_day)
dat$chilltemp <- as.numeric(dat$chilltemp)
dat$doy <- as.numeric(dat$doy)
dat$datasetIDstudy <- paste(dat$datasetID, dat$study)


columnstokeep <- c("datasetID", "study", "datasetIDstudy", "genus", "species", "latbi", "varetc", 
    "woody","provenance.lat", "provenance.long", "material", "year", 
    "fieldsample.date", "doy", "forcetemp", "forcetemp_night",  
    "photoperiod_day","photoperiod_night","chilltemp", "chillphotoperiod",
    "chilldays", "response", "response.time", "photoext", "forceext", "chillext")        
                   
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

## Field sample dates
# Get the number of field sampling dates that are 14 or more weeks apart, first for each datasetIDx study ...
ddatefx.all <- subset(d, select=c("datasetID", "study", "fieldsample.date"))
ddatefx <- ddatefx.all[!duplicated(ddatefx.all), ]
ddatefx$datasetIDstudy <- paste(ddatefx$datasetID, ddatefx$study)

dates2weeks <- countfieldsample(ddatefx, 14)

# ... and next for each treatment
ddatefxtreat.all <- subset(d, select=c("datasetID", "study", "fieldsample.date", "force",
    "photoperiod_day", "chilltemp"))
ddatefxtreat <- ddatefxtreat.all[!duplicated(ddatefxtreat.all), ]
ddatefxtreat$datasetIDstudy <- paste(ddatefxtreat$datasetID, ddatefxtreat$study,
     ddatefxtreat$force, ddatefxtreat$photoperiod_day, ddatefxtreat$chilltemp)

dates2weekstreat <- countfieldsample(ddatefxtreat, 14)
names(dates2weekstreat)[names(dates2weekstreat)=="count"] <- "fs.date.count"


if(FALSE){ # Below by Dan B for Weinbgerger stuff (I think).
weinberger <- filter(dates2weekstreat,fs.date.count>=2)
#clean it a little bit
goo <- separate(weinberger, datasetIDstudy,c("datasetID","study","force","photo","chill") ,
   sep = " " , remove = FALSE)
unique(goo$datasetID)
}

## summarizing data: means, mins and maxes by study
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


## For PEP725 comparison:
## Calculate the DIFFERENCES between treatments for each study
forcecues <- getcueinfo(d, "force", "datasetIDstudy")
photocues <- getcueinfo(d, "photoperiod_day", "datasetIDstudy")
# chilldayscues <- getcueinfo(d, "chilldays", "datasetIDstudy") # need to make numeric
chilltempcues <- getcueinfo(d, "chilltemp", "datasetIDstudy")

# get consecutive diffs of cues (we decided not to use so much I think)
forcecues.consdiffs <- getcueinfo.consdiffs(d, "force", "datasetIDstudy")
# get all diffs of cues
forcecues.alldiffs <- getcueinfo.alldiffs(d, "force", "datasetIDstudy")
photocues.alldiffs <- getcueinfo.alldiffs(d, "photoperiod_day", "datasetIDstudy")
chilltempcues.alldiffs <- getcueinfo.alldiffs(d, "chilltemp", "datasetIDstudy")

# Merging ...
# Below seems to work fine to use for histograms
cueinfo <- merge(merge(forcecues, photocues, by = "datasetIDstudy", all=TRUE,
    suffixes=c("force", "photo")), chilltempcues, by = "datasetIDstudy", all=TRUE,
    suffixes=c("", "chill"))
# Double check that the merge works as well as single files
par(mfrow=c(1,2))
hist(cueinfo$maxdiff.treatforce, breaks=10)
hist(forcecues$maxdiff.treat, breaks=10) 
# Careful! The below retain all values, so it duplicates some treatments
# That is, don't assume all the crossed treatments were made
# Need to use the single cue files for more accurate histograms
cueinfo.alldiffs <- merge(merge(forcecues.alldiffs, photocues.alldiffs,
    by = "datasetIDstudy", all=TRUE, suffixes=c("force", "photo")), 
    chilltempcues.alldiffs, by = "datasetIDstudy", all=TRUE,
    suffixes=c("", "chill"))

pdf("limitingcues/figures/cuediffs.pdf", width = 6, height = 8)
par(mfrow=c(3,2))
hist(forcecues$maxdiff.treat, breaks=20, main="max", xlab="forcing")
hist(forcecues.alldiffs$diff.treat, breaks=20, main="all diffs", xlab="forcing")
hist(photocues$maxdiff.treat, breaks=20, main="max", xlab="photo")
hist(photocues.alldiffs$diff.treat, breaks=20, main="all diffs", xlab="photo")
hist(chilltempcues$maxdiff.treat, breaks=20, main="max", xlab="chill temp")
hist(chilltempcues.alldiffs$diff.treat, breaks=20, main="all diffs", xlab="chill temp")
dev.off()

# Now we look for just particular species ...
# Assume if a study did the species then we can use that study

betpen <- subset(d, genus=="Betula" & species=="pendula")
betpen.forcecues <- forcecues.alldiffs[which(forcecues.alldiffs$datasetIDstudy %in% unique(betpen$datasetIDstudy)),]
betpen.photocues <- photocues.alldiffs[which(photocues.alldiffs$datasetIDstudy %in% unique(betpen$datasetIDstudy)),]
betpen.chilltempcues <- chilltempcues.alldiffs[which(chilltempcues.alldiffs$datasetIDstudy %in%
    unique(betpen$datasetIDstudy)),]

pdf("limitingcues/figures/cuediffs_betpen.pdf", width = 6, height = 3)
par(mfrow=c(1,3))
hist(betpen.forcecues$diff.treat, breaks=20, main="Betula: all diffs", xlab="forcing")
hist(betpen.photocues$diff.treat, breaks=20, main="Betula: all diffs", xlab="photo")
hist(betpen.chilltempcues$diff.treat, breaks=20, main="Betula: all diffs", xlab="chill temp")
dev.off()

fagsyl <- subset(d, genus=="Fagus" & species=="sylvatica")
fagsyl.forcecues <- forcecues.alldiffs[which(forcecues.alldiffs$datasetIDstudy %in% unique(fagsyl$datasetIDstudy)),]
fagsyl.photocues <- photocues.alldiffs[which(photocues.alldiffs$datasetIDstudy %in% unique(fagsyl$datasetIDstudy)),]
fagsyl.chilltempcues <- chilltempcues.alldiffs[which(chilltempcues.alldiffs$datasetIDstudy %in%
    unique(fagsyl$datasetIDstudy)),]

pdf("limitingcues/figures/cuediffs_fagsyl.pdf", width = 6, height = 3)
par(mfrow=c(1,3))
hist(fagsyl.forcecues$diff.treat, breaks=20, main="Fagus: all diffs", xlab="forcing")
hist(fagsyl.photocues$diff.treat, breaks=20, main="Fagus: all diffs", xlab="photo")
# hist(fagsyl.chilltempcues$diff.treat, breaks=20, main="Fagus: all diffs", xlab="chill temp")
dev.off()

# write out the cuediffs we want ... 
fagsylcues <- fagsyl.forcecues
fagsylcues$cue <- "force"
fagsylcues$sp <- "Fagus_sylvatica"

betpen.chilltempcues$cue <- "chill"
betpen.forcecues$cue <- "force"
betpencues <- rbind(betpen.chilltempcues, betpen.forcecues)
betpencues$sp <- "Betula_pendula"

betfagcues <- rbind(betpencues, fagsylcues)

write.csv(betfagcues, "limitingcues/output/cues_fagben.csv", row.names=FALSE)
write.csv(forcecues.alldiffs, "limitingcues/output/cuesforce.alldiffs.csv", row.names=FALSE)
write.csv(chilltempcues.alldiffs, "limitingcues/output/cueschilltemp.alldiffs.csv", row.names=FALSE)

##

dsumm.wsp <-
      ddply(d, c("datasetID", "study", "latbi"), summarise,
      mean.year = mean(year),
      mean.temp = mean(force),
      min.temp = min(force),
      max.temp = max(force),
      sd.temp = sd(force))

      
par(mfrow=c(2,2))
dsumm.toplot <- dsumm.wsp
dsumm.fs <- subset(dsumm.wsp, latbi=="Fagus sylvatica")
dsumm.bp <- subset(dsumm.wsp, latbi=="Betula pendula")
# dsumm.toplot <- dsumm.fs
# dsumm.toplot <- dsumm.bp
hist(dsumm.toplot$max.temp, main="Max temp", xlab="Max temp")
plot(density(dsumm.toplot$max.temp, na.rm=TRUE), col="red",  main="Max temp density", xlab="Max temp")
hist(dsumm.toplot$min.temp, main="Min temp", xlab="Min temp")
plot(density(dsumm.toplot$min.temp, na.rm=TRUE), col="blue", main="Min temp density", xlab="Min temp")



if(FALSE){
# for cfi
chill <- subset(dsumm, mean.chill==""| is.na(mean.chill)==TRUE)
dim(chill)
chill$chilldiff <- chill$max.chill-chill$min.chill
length(subset(chill, chilldiff==0))   # 101+26 = 127 did not manipulate chilling experimentally
    }

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

pdf("limitingcues/figures/tempxlatminmaxcorr.pdf", width = 8, height = 4)
par(mfrow=c(1,2))
plotxydatabsX("min.temp", "mean.lat", "datasetID", dsumm, "topleft") # -0.1
plotxydatabsX("max.temp", "mean.lat", "datasetID", dsumm, "topleft") # -0.08
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

pdf("limitingcues/figures/photoxlatcorr2plots.pdf", width = 8, height = 6)
par(mfrow=c(2,2))
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

###
###
###

## TO DO for heatmap:
# (1) Make sure I am counting correctly
# (2) What to do with NA -- some are likely ambient, but not sure if all are... see "work on NAs" below
# (3) Make much, much prettier!

## Prep the data, make a column concatenating a bunch of info, merge in field sample dates,
# and make the cues intergers (so the heat maps make sense)
d$datasetIDstudytreat <- paste(d$datasetID, d$study, d$force, d$photoperiod_day, d$chilltemp)
d <- merge(d, dates2weekstreat, by.x="datasetIDstudytreat", by.y="datasetIDstudy", all.x=TRUE)
d$force.int <- as.integer(d$force)
d$photo.int <- as.integer(d$photoperiod_day)
d$chill.int <- as.integer(d$chilltemp)

## Add back in "photoext", "forceext", "chillext" to data so can add to heat maps ... 
d$force.plot <- d$force.int
d$force.plot[which(d$forceext=="variable"|d$forceext=="ambient")] <-
    d$forceext[which(d$forceext=="variable"|d$forceext=="ambient")]
d$photo.plot <- d$photo.int
d$photo.plot[which(d$photoext=="variable"|d$photoext=="ambient")] <-
    d$photoext[which(d$photoext=="variable"|d$photoext=="ambient")]
d$chill.plot <- d$chill.int
d$chill.plot[which(d$chillext=="variable"|d$chillext=="other")] <-
    d$chillext[which(d$chillext=="variable"|d$chillext=="other")]

dsumm.treat <-
      ddply(d, c("datasetID", "study", "force.plot", "photo.plot", "chill.plot"), summarise,
      mean.lat = mean(provenance.lat),
      mean.long = mean(provenance.long),
      mean.year = mean(year),
      spp.n = length(unique(latbi)),
      field.sample.n = mean(fs.date.count, na.rm=TRUE),
      mean.fieldsamp = mean(doy),
      min.fieldsamp = min(doy),
      max.fieldsamp = max(doy))

# sort the factors so that they show up in a logical order (I checked the new maps produced versus the old and they look good!)
dsumm.treat$force.plot <- factor(dsumm.treat$force.plot, levels=c(sort(unique(d$force.int)), "ambient", "variable", "NA"))
dsumm.treat$photo.plot <- factor(dsumm.treat$photo.plot, levels=c(sort(unique(d$photo.int)), "ambient", "variable", "NA"))
dsumm.treat$chill.plot <- factor(dsumm.treat$chill.plot, levels=c(sort(unique(d$chill.int)), "variable", "other", "NA"))

# work on NAs (ask lab to check?)
if(FALSE){
checkforcena <- subset(dsumm.treat, is.na(force.plot)==TRUE)
checkphotona <- subset(dsumm.treat, is.na(photo.plot)==TRUE)
checkchillna <- subset(dsumm.treat, is.na(chill.plot)==TRUE)
checkchillna.helper <- subset(dsumm.treat, field.sample.n>0)

unique(checkforcena$datasetID)
unique(checkphotona$datasetID)
unique(checkchillna$datasetID) # but let's subset to those that don't have field sample dates
unique(checkchillna$datasetID)[which(!unique(checkchillna$datasetID) %in% checkchillna.helper$datasetID)]

dfbb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
dfbbchill <- dfbb[which(dfbb$datasetID %in% checkchillna$datasetID),]
dfbbchillsm <- subset(dfbbchill, select=c("datasetID", "study", "chill_type"))
checkchillbb <- dfbbchillsm[!duplicated(dfbbchillsm),]
checkchillbyhand <- checkchillna[!which(checkchillna$datasetID %in% dfbb$datasetID),]
# So, a few rows in ashby62 (exp 1), caffarra11a (exp 1), cannell83 (exp 1-2), charrier11 (exp 1-2), falusi90 exp1, cook05 (exp 1), gansert02 (exp 1-2), granhus09 (exp 1), heide05 (exp 1-2), heide11 (exp 1-3), howe95 (exp 1), nishimoto95 (exp 1), pettersen71 (exp 1-2), sonsteby13 (exp 1 - 2), and worrall67 exp 1, 2, 5 still show as NA -- the rest are a mix.
    }

## The below heat maps show the COUNT of studies across different treatments
# For example, x is 20 C, y is 12 days and the third dimension is how many did that.
dsumm.nums <-
      ddply(dsumm.treat, c("force.plot", "photo.plot"), summarise,
      count = length(force.plot))
# dsumm.nums[is.na(dsumm.nums)] <- 0

pdf("limitingcues/figures/heatmapforcexphoto.pdf", width = 6, height = 6)
ggplot(dsumm.nums, aes(as.factor(force.plot), as.factor(photo.plot))) +
    geom_tile(aes(fill=count)) +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred")
dev.off()

dsumm.numschph <-
      ddply(dsumm.treat, c("chill.plot", "photo.plot"), summarise,
      count = length(chill.plot))
# dsumm.numsch[is.na(dsumm.numsch)] <- 0

dsumm.numschfor <-
      ddply(dsumm.treat, c("chill.plot", "force.plot"), summarise,
      count = length(chill.plot))

pdf("limitingcues/figures/heatmapchillxphoto.pdf", width = 6, height = 6)
ggplot(dsumm.numschph, aes(as.factor(chill.plot), as.factor(photo.plot))) +
    geom_tile(aes(fill=count), colour="white") +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred")
dev.off()

pdf("limitingcues/figures/heatmapchillxforce.pdf", width = 6, height = 6)
ggplot(dsumm.numschfor, aes(as.factor(chill.plot), as.factor(force.plot))) +
    geom_tile(aes(fill=count), colour="white") +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred")
dev.off()


dsumm.numschfs <-
      ddply(dsumm.treat, c("chill.plot", "field.sample.n"), summarise,
      count = length(chill.plot))

pdf("limitingcues/figures/heatmapchillxfs.date.pdf", width = 6, height = 4)
ggplot(dsumm.numschfs, aes(as.factor(chill.plot), as.factor(field.sample.n))) +
    geom_tile(aes(fill=count), colour="white") +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred")
dev.off()

dsumm.numsforfs <-
      ddply(dsumm.treat, c("force.plot", "field.sample.n"), summarise,
      count = length(chill.plot))

pdf("limitingcues/figures/heatmapforcexfs.date.pdf", width = 6, height = 4)
ggplot(dsumm.numsforfs, aes(as.factor(force.plot), as.factor(field.sample.n))) +
    geom_tile(aes(fill=count), colour="white") +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred")
dev.off()

dsumm.numsphfs <-
      ddply(dsumm.treat, c("photo.plot", "field.sample.n"), summarise,
      count = length(chill.plot))

pdf("limitingcues/figures/heatmapphotoxfs.date.pdf", width = 6, height = 4)
ggplot(dsumm.numsphfs, aes(as.factor(photo.plot), as.factor(field.sample.n))) +
    geom_tile(aes(fill=count), colour="white") +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred")
dev.off()


# make figures prettier than average
# ARGHH! Colors not working..
basesize <- 12
colz <- viridis_pal(option="magma")(3)
pdf("limitingcues/figures/heatmapphotoxforcexfs.date.pdf", width = 6, height = 4)
ggplot(dsumm.treat, aes(as.factor(photo.plot), as.factor(force.plot))) +
    geom_tile(aes(fill=field.sample.n), colour="white") +
    scale_fill_gradient2(name="Field sample \ndates n", low = colz[1], mid=colz[3],
        high = colz[2], na.value="gray95") +
    # scale_x_discrete(breaks=seq(-5,35,5)) +
    # scale_y_discrete(breaks=seq(6,24,2)) +
    theme_classic() +
    labs(colour="Field sample dates", y="Forcing temp", x="Photoperiod") +
    theme(legend.background=element_blank(), # legend.position=c(0.1, 0.85) , 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_blank(), text=element_text(size=basesize))
dev.off()


library(scales)
show_col(viridis_pal(option="magma")(3))
show_col(viridis_pal(option="magma")(3)[1])
show_col(viridis_pal(option="magma")(3)[2])
show_col(viridis_pal(option="magma")(3)[3])
