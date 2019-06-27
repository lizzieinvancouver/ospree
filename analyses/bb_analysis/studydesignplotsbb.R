## By Lizzie ##
## Started 14 Feb 2019 ##
## Based off studydesignplots.R ##

## Looking at what treatments show up in the analysed OSPREE data ##

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
} else setwd("~/Documents/git/ospree/analyses/bb_analysis")

######################################
# Flags to choose for bbstanleadin.R #
######################################

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- TRUE
use.flags.for.allsppmodel <- FALSE
use.yourown.flagdesign <- FALSE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
    use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
    & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
use.chillports = FALSE
use.zscore = FALSE
use.allspp =FALSE # for the main model this is false
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


source("source/bbstanleadin.R")

source("..//misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("..//misc/getcuesbystudy_fxs.R") # f(x) counts up cues

# set up figure paths
if(use.flags.for.mainmodel){
figurepath <- "mainmodel"
}
if(use.flags.for.allsppmodel){
figurepath <- "allsppmodel"
}

if(use.yourown.flagdesign){
figurepath <- "misc"
}

# Need bb.stan to return fieldsample.date

# d <- bb.all
d <- bb.stan

# format species and date
d$latbi <- paste(d$genus, d$species)
d$fieldsample.date <- as.Date(d$fieldsample.date, format="%d-%b-%Y")
d$doy <- as.numeric(format(d$fieldsample.date, "%j"))

## Field sample dates
# Get the number of field sampling dates that are 14 or more weeks apart, first for each datasetIDx study ...
# for each treatment
ddatefxtreat.all <- subset(d, select=c("datasetID", "study", "fieldsample.date", "force",
    "photoperiod_day", "chill", "chilltemp"))
ddatefxtreat <- ddatefxtreat.all[!duplicated(ddatefxtreat.all), ]
ddatefxtreat$datasetIDstudy <- paste(ddatefxtreat$datasetID, ddatefxtreat$study,
     ddatefxtreat$force, ddatefxtreat$photoperiod_day, ddatefxtreat$chilltemp)

dates2weekstreat <- countfieldsample(ddatefxtreat, 14) # need to check uniquestudy <- 70 to look into this more
names(dates2weekstreat)[names(dates2weekstreat)=="count"] <- "fs.date.count"

## Prep the data, make a column concatenating a bunch of info, merge in field sample dates,
# and make the cues intergers (so the heat maps make sense)
d$datasetIDstudytreat <- paste(d$datasetID, d$study, d$force, d$photoperiod_day, d$chilltemp)
d <- merge(d, dates2weekstreat, by.x="datasetIDstudytreat", by.y="datasetIDstudy", all.x=TRUE)
d$force.int <- as.integer(d$force)
d$photo.int <- as.integer(d$photoperiod_day)
d$chill.int <- as.integer(d$chill)
d$chilltemp.int <- as.integer(d$chilltemp)

# summarize by design, count field sample dates
dsumm.treat.chilltemp <-
      ddply(d, c("datasetID", "study", "force.int", "photo.int", "chilltemp.int"), summarise,
      mean.lat = mean(provenance.lat),
      spp.n = length(unique(latbi)),
      field.sample.n = mean(fs.date.count, na.rm=TRUE),
      mean.fieldsamp = mean(doy),
      min.fieldsamp = min(doy),
      max.fieldsamp = max(doy))

# summarize by design, include all chilling (field and exp)
dsumm.treat <-
      ddply(d, c("datasetID", "study", "force.int", "photo.int", "chill.int"), summarise,
      mean.lat = mean(provenance.lat),
      spp.n = length(unique(latbi)),
      field.sample.n = mean(fs.date.count, na.rm=TRUE),
      mean.fieldsamp = mean(doy),
      min.fieldsamp = min(doy),
      max.fieldsamp = max(doy))

# get counts of forcing treats ... not sure why I included this ...
dsumm.nums <-
      ddply(dsumm.treat, c("force.int", "photo.int"), summarise,
      count = length(force.int))
# dsumm.nums[is.na(dsumm.nums)] <- 0

# summarize by design, count field sample dates
dsumm.treat.chilltemp <-
      ddply(d, c("datasetID", "study", "force.int", "photo.int", "chilltemp.int"), summarise,
      mean.lat = mean(provenance.lat),
      spp.n = length(unique(latbi)),
      field.sample.n = mean(fs.date.count, na.rm=TRUE),
      mean.fieldsamp = mean(doy),
      min.fieldsamp = min(doy),
      max.fieldsamp = max(doy))


# make figures prettier than average
basesize <- 12

heatforcphotofielddate <- ggplot(dsumm.treat, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=field.sample.n), colour="white") +
    # alt color: scale_fill_viridis(option="C", direction=-1, na.value="gray97") + # requires viridis
    scale_fill_gradient2(name="Field sample \ndates", low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatforcephotoallchill <- ggplot(dsumm.treat, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=chill.int), colour="white") +
    scale_fill_gradient2(name="All chill \ntemps",low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) + theme_classic() +
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatforcephotoexpchill <- ggplot(dsumm.treat.chilltemp, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=chilltemp.int), colour="white") +
    scale_fill_gradient2(name="Exp chill \ntemps",low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

# But still need to: Make into one figure and cannot manage to alter legend name on colorbar!
pdf(paste("figures/studydesign/studydesign_heatforcephotofielddate", figurepath, ".pdf", sep=""), width = 5, height = 5)
heatforcphotofielddate
dev.off()

pdf(paste("figures/studydesign/studydesign_heatforcephotoallchill", figurepath, ".pdf", sep=""), width = 5, height = 5)
heatforcephotoallchill
dev.off()

pdf(paste("figures/studydesign/studydesign_heatforcephotoexpchill", figurepath, ".pdf", sep=""), width = 5, height = 5)
heatforcephotoexpchill
dev.off()


require(cowplot)
pdf(paste("figures/studydesign/studydesign_heat3panel", figurepath, ".pdf", sep=""), width = 16, height = 6)
plot_grid(heatforcephotoallchill, heatforcphotofielddate, heatforcephotoexpchill,
    labels = c(' (a) All chill', '(b) Field chill', '(c) Exp chill'), ncol=3)
dev.off()


##################
## Map of sites ##
##################
# for figure of study locations, we also need to identify studies that manipulated chilling somehow
# so identify the set of studies that have varied chilltemp, chilldays or field sample dates

# get data in main model and all spp model
mainmod <- read.csv("..//output/bbstan_mainmodel_utah_allsppwcrops_allfp_allchill.csv")
allsppmod <- read.csv("..//output/bbstan_allsppmodel_utahzscore_wcrops_allfp_allchill.csv")

# summarize by study, count field sample dates, chilldays and chilltemps
dsumm.chilling.anywhichway <-
      ddply(d, c("datasetID", "study"), summarise,
      spp.n = length(unique(latbi)),
      field.sample.n = mean(fs.date.count, na.rm=TRUE),
      chilltemp.n = length(unique(chilltemp)),
      chilldays.n = length(unique(chilldays)))

which.varychill <- dsumm.chilling.anywhichway[which(dsumm.chilling.anywhichway$field.sample.n>1 |
    dsumm.chilling.anywhichway$chilltemp.n>1 | dsumm.chilling.anywhichway$chilldays.n>1),]


bbmapdata <- subset(allsppmod, select=c("datasetID", "study"))
bbmapdata <- bbmapdata[!duplicated(bbmapdata), ]

bbmapdata$mainmodel <- NA
bbmapdata$chill.manipulated <- NA

bbmapdata$mainmodel[which(paste(bbmapdata$datasetID, bbmapdata$study) %in% paste(mainmod$datasetID, mainmod$study))] <- "yes"
bbmapdata$mainmodel[which(!paste(bbmapdata$datasetID, bbmapdata$study) %in% paste(mainmod$datasetID, mainmod$study))] <- "no"
bbmapdata$chill.manipulated[which(paste(bbmapdata$datasetID, bbmapdata$study) %in%
    paste(which.varychill$datasetID, which.varychill$study))] <- "yes"
bbmapdata$chill.manipulated[which(!paste(bbmapdata$datasetID, bbmapdata$study) %in%
    paste(which.varychill$datasetID, which.varychill$study))] <- "no"

write.csv(bbmapdata, "..//output/bbmapdata.csv", row.names=FALSE)
          
