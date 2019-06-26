## 18 June 2019 ##
## Copied partly from countinxns.R ##
## Counting up OSPREE studies for Isabelle's work ##

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

source("misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("misc/gettreatdists.R") # f(x) counts up field sample dates separated by a number of days you specify

###################
# All OSPREE data #
###################
dat <- read.csv("output/ospree_clean.csv", header = TRUE)
dat <- dat[dat$woody=="yes",]
dat$fieldsample.date <- as.Date(dat$fieldsample.date, format="%d-%b-%Y")
dat$doy <- format(dat$fieldsample.date, "%j")

datsm <- subset(dat, select=c("datasetID", "study", "genus", "species", "forcetemp", "forcetemp_night",                 
    "photoperiod_day",  "fieldsample.date", "chilltemp", "chillphotoperiod", "chilldays"))
head(datsm)

datsm$force <- as.numeric(datsm$forcetemp)
datsm$forcenight <- as.numeric(datsm$forcetemp_night)
datsm.noNA <- subset(datsm, is.na(force)==FALSE & is.na(forcenight)==FALSE)
unique(datsm$forcetemp)
unique(datsm$chilltemp)

## Which studies have more than one chill temp?
get.treatdists.singletreatment(datsm.noNA, "chilltemp")

## Any sign of studies that cross day and night forcing temps?
subset(get.treatdists(dat, "forcetemp", "forcetemp_night"), intxn>0) # one maybe

## Okay, now look for just studies that had different day/night temps

# Step 1: format the data ... a little
# NEXT! I need to check what is lost in conversion to numeric ...
# Then check my get.treatdists.daynight code!
datsm.noNA.daynight <- subset(datsm.noNA, is.na(forcetemp_night)==FALSE)
dim(datsm.noNA.daynight)
datsm.noNA.daynight$amp <- datsm.noNA.daynight$force-datsm.noNA.daynight$forcenight
datsm.noNA.daynight <-  subset(datsm.noNA.daynight, amp!=0)
dim(datsm.noNA.daynight)

# this just tries to count day and night temps, it is not perfect as I wrote it quickly (use at your own risk!)
osp.daynight.vary.alt <- get.treatdists.daynight.alt(datsm.noNA.daynight, "force", "forcenight")

# this is a better f(x), it should be more accurate
osp.daynight.all <- get.treatdists.daynight(datsm.noNA, "forcetemp", "forcetemp_night")
osp.daynight.vary <- subset(osp.daynight.all, treatinfo=="some diff daynight")
subset(osp.daynight.vary, numdiffforce>0)

if(FALSE){
# below uses the less great counting day/night treatment code
dat$datasetIDstudy <- paste(dat$datasetID, dat$study)
ampstudies <- dat[which(dat$datasetIDstudy %in% paste(osp.daynight.vary.alt$datasetID, osp.daynight.vary.alt$study)),]
ampstudies$force <- as.numeric(ampstudies$forcetemp)
ampstudies$forcenight <- as.numeric(ampstudies$forcetemp_night)
ampstudies$amp <- ampstudies$force-ampstudies$forcenight
ampstudies$response <- as.numeric(ampstudies$response.time)

library(ggplot2)
ggplot(ampstudies, aes(x=amp, y=log10(response), colour=datasetIDstudy)) +
    geom_point() 
}
