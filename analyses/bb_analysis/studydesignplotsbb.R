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


## set up the flags
use.chillports = TRUE
use.zscore = FALSE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

source("source/bbstanleadin.R")


source("..//misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("..//misc/getcuesbystudy_fxs.R") # f(x) counts up cues

# Need bb.stan to return fieldsample.date

d <- bb.all

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


# make figures prettier than average
basesize <- 7

heatforcphotofielddate <- ggplot(dsumm.treat, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=field.sample.n), colour="white") +
    # alt color: scale_fill_viridis(option="C", direction=-1, na.value="gray97") + # requires viridis
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + 
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatforcephotoallchill <- ggplot(dsumm.treat, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=chill.int), colour="white") +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + 
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatforcephotoexpchill <- ggplot(dsumm.treat.chilltemp, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=chilltemp.int), colour="white") +
    scale_fill_gradient2(low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + 
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

# But still need to: Make into one figure and cannot manage to alter legend name on colorbar!
pdf("figures/studydesign_heatforcephotofielddate.pdf", width = 5, height = 5)
heatforcphotofielddate
dev.off()

pdf("figures/studydesign_heatforcephotoallchill.pdf", width = 5, height = 5)
heatforcephotoallchill
dev.off()

pdf("figures/studydesign_heatforcephotoexpchill.pdf", width = 5, height = 5)
heatforcephotoexpchill
dev.off()


require(cowplot)
pdf("figures/studydesign_heat3panel.pdf", width = 7, height = 16)
plot_grid(heatforcephotoallchill, heatforcphotofielddate, heatforcephotoexpchill,
    labels = c('(a) All chill', '(b) Field chill', '(c) Exp chill'), ncol=1)
dev.off()
