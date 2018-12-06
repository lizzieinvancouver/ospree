###New weinberger based on bb analysis data started by Dan B Dec 5 2018

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillunits = FALSE # change to true for testing chill units
# Default is species complex
use.allspp = TRUE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE

source("source/bbstanleadin.R") ###use bb.all as data frame
source("..//misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("..//misc/getcuesbystudy_fxs.R")

#d is bb.noNA, you need to use this data sheet because bb.all has already dropped the "study"column
dat<-d
dat$latbi <- paste(dat$genus, dat$species)
dat$fieldsample.date <- as.Date(dat$fieldsample.date, format="%d-%b-%Y")

# Turn things back to numeric so they can be counted
dat$forcetemp <- as.numeric(dat$forcetemp)
dat$forcetemp_night <- as.numeric(dat$forcetemp_night)
dat$photoperiod_night <- as.numeric(dat$photoperiod_night)
dat$photoperiod_day <- as.numeric(dat$photoperiod_day)
dat$chilltemp <- as.numeric(dat$chilltemp)
dat$datasetIDstudy <- paste(dat$datasetID, dat$study)




### These are the fuctions Lizzie made in getfielddates.R and getcuesbystudy_fxs.R
ddatefx.all <- subset(dat, select=c("datasetID", "study", "fieldsample.date"))
ddatefx <- ddatefx.all[!duplicated(ddatefx.all), ]
ddatefx$datasetIDstudy <- paste(ddatefx$datasetID, ddatefx$study)

dates2weeks <- countfieldsample(ddatefx, 14)

# ... and next for each treatment
ddatefxtreat.all <- subset(dat, select=c("datasetID", "study", "fieldsample.date", "force", "photoperiod_day", "chilltemp"))
ddatefxtreat <- ddatefxtreat.all[!duplicated(ddatefxtreat.all), ]
ddatefxtreat$datasetIDstudy <- paste(ddatefxtreat$datasetID, ddatefxtreat$study, ddatefxtreat$force,
                                     ddatefxtreat$photoperiod_day, ddatefxtreat$chilltemp)

dates2weekstreat <- countfieldsample(ddatefxtreat, 14)
names(dates2weekstreat)[names(dates2weekstreat)=="count"] <- "fs.date.count"

weinberger<-filter(dates2weekstreat,fs.date.count>=2)
#clean it a little bit
weinberger$datasetID<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 1)
weinberger$study<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 2)
weinberger$force<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 3)
weinberger$photo<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 4)
weinberger$chill<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 5)



# then make a new column...
unique(weinberger$datasetID)
intersect(unique(bb.all$datasetID),unique(weinberger$datasetID))
setdiff(unique(bb.all$datasetID),unique(weinberger$datasetID))

