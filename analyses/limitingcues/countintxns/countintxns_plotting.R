## Started 12 Nov 2019 ##
## By Lizzie ##

# Heatmaps for all OSPREE data! (adjusted from studydesignplotsbb.R)
# Datafile built in countintxns.R -- careful here, some of the force and photoperiods are guesstimates (see force.org)

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")


library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)

source("misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify

datsm14d <- read.csv("limitingcues/output/osp14d_forheatmaps.csv")
datsm14d$fieldsample.date <- as.Date(datsm14d$fieldsample.date, format="%Y-%m-%d")

# Tricky here since we can't use the converted data we used above (since we had to guesstimate some of the treatments)

# first format the data to get the counts
ddatefxtreat.all <- subset(datsm14d, select=c("datasetID", "study", "fieldsample.date", "force.org",
    "photo.org", "chilldays", "chilltemp"))
ddatefxtreat <- ddatefxtreat.all[!duplicated(ddatefxtreat.all), ]
ddatefxtreat$datasetIDstudy <- paste(ddatefxtreat$datasetID, ddatefxtreat$study,
     ddatefxtreat$force.org, ddatefxtreat$photo.org, ddatefxtreat$chilldays, ddatefxtreat$chilltemp)

dates2weekstreat <- countfieldsample(ddatefxtreat, 14) # need to check uniquestudy <- 70 to look into this more
names(dates2weekstreat)[names(dates2weekstreat)=="count"] <- "fs.date.count"

datsm14d$datasetIDstudytreat <- paste(datsm14d$datasetID, datsm14d$study, datsm14d$force.org,
    datsm14d$photo.org, datsm14d$chilldays, datsm14d$chilltemp)
dmaps <- merge(datsm14d, dates2weekstreat, by.x="datasetIDstudytreat", by.y="datasetIDstudy", all.x=TRUE)

dmaps$force.int <- as.integer(as.numeric(dmaps$force.org))
dmaps$photo.int <- as.integer(as.numeric(dmaps$photo.org))
dmaps$chilltemp.int <- as.integer(as.numeric(dmaps$chilltemp))
dmaps$chilldays.int <- as.integer(as.numeric(dmaps$chilldays))

dsumm.treat <-
      ddply(dmaps, c("datasetID", "study", "force.int", "photo.int"), summarise,
      field.sample.n = mean(fs.date.count, na.rm=TRUE))

dsumm.treat.wchilltemp <-
      ddply(dmaps, c("datasetID", "study", "force.int", "photo.int", "chilltemp.int"), summarise,
      field.sample.n = mean(fs.date.count, na.rm=TRUE))

dsumm.treat.wchilldays <-
      ddply(dmaps, c("datasetID", "study", "force.int", "photo.int", "chilltemp.int", "chilldays.int"), summarise,
      field.sample.n = mean(fs.date.count, na.rm=TRUE))

# Now get counts of treatments ... 
dmaps$datstudy <- paste(dmaps$datasetID, dmaps$study)
dsumm.fp <-
      ddply(dmaps, c("force.int", "photo.int"), summarise,
      n = length(unique(datstudy)))
dsumm.ffs <-
      ddply(dmaps, c("force.int", "fs.date.count"), summarise,
      n = length(unique(datstudy)))
dsumm.pfs <-
      ddply(dmaps, c("photo.int", "fs.date.count"), summarise,
      n = length(unique(datstudy)))
dsumm.fc <-
      ddply(dmaps, c("force.int", "chilltemp.int"), summarise,
      n = length(unique(datstudy)))
dsumm.pc <-
      ddply(dmaps, c("photo.int", "chilltemp.int"), summarise,
      n = length(unique(datstudy)))


basesize <- 12

# First, plots of 3-way treatments ...
heatforcphotofielddate <- ggplot(dsumm.treat, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=field.sample.n), colour="white") +
    # alt color: scale_fill_viridis(option="C", direction=-1, na.value="gray97") + # requires viridis
    scale_fill_gradient2(name="Field sample \ndates", low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatforcephotoexpchilltemp <- ggplot(dsumm.treat.wchilltemp, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=chilltemp.int), colour="white") +
    scale_fill_gradient2(name="Exp chill \ntemps",low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatforcephotochilldays <- ggplot(dsumm.treat.wchilldays, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=chilldays.int), colour="white") +
    scale_fill_gradient2(name="Chill days \n(mixed)",low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="Field sample dates", x="Forcing temp", y="Photoperiod") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

pdf(paste("limitingcues/figures/heatallosp_3treats.pdf", sep=""), width = 16, height = 6)
plot_grid(heatforcphotofielddate, heatforcephotoexpchilltemp, heatforcephotochilldays,
    labels = c(' (A) Field sampling', '(B) Exp chill temps', '(C) Chill days'), ncol=3)
dev.off()


# Next, plots of 2-way treatments ...
heatforcphoto<- ggplot(dsumm.fp, aes(as.factor(force.int), as.factor(photo.int))) +
    geom_tile(aes(fill=n), colour="white") +
    # alt color: scale_fill_viridis(option="C", direction=-1, na.value="gray97") + # requires viridis
    scale_fill_gradient2(name="n of studies", low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="n of studies", x="Forcing temp", y="Photoperiod") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatforcfs <- ggplot(dsumm.ffs, aes(as.factor(force.int), as.factor(fs.date.count))) +
    geom_tile(aes(fill=n), colour="white") +
    scale_fill_gradient2(name="n of studies", low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="n of studies", x="Forcing temp", y="Field sample dates") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatphotfs <- ggplot(dsumm.pfs, aes(as.factor(photo.int), as.factor(fs.date.count))) +
    geom_tile(aes(fill=n), colour="white") +
    scale_fill_gradient2(name="n of studies", low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="n of studies", x="Photoperiod", y="Field sample dates") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatforcc <- ggplot(dsumm.fc, aes(as.factor(force.int), as.factor(chilltemp.int))) +
    geom_tile(aes(fill=n), colour="white") +
    scale_fill_gradient2(name="n of studies", low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="n of studies", x="Forcing temp", y="Chill temp") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

heatphotc <- ggplot(dsumm.pc, aes(as.factor(photo.int), as.factor(chilltemp.int))) +
    geom_tile(aes(fill=n), colour="white") +
    scale_fill_gradient2(name="n of studies", low = "white", mid ="lightgoldenrodyellow", high = "darkred",
        na.value="gray95") + scale_x_discrete(breaks=seq(-5,35,5)) +
  scale_y_discrete(breaks=seq(6,24,2)) +
    labs(colour="n of studies", x="Photoperiod", y="Chill temp") + theme_classic() +
    theme(legend.position=c(0.9, 0.2) , legend.background=element_blank(),
        panel.background = element_blank(), text=element_text(size=basesize))

pdf(paste("limitingcues/figures/heatallosp_4panel.pdf", sep=""), width = 16, height = 12)
plot_grid(heatforcfs, heatphotfs, heatforcc, heatphotc, 
    labels = c('(A)', '(B)', '(C)', '(D)'), ncol=2)
dev.off()
