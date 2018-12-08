# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R

use.chillports = FALSE # change to true for using chillportions instead of utah units

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
source("source/bbstanleadin.R")
source("..//misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("..//misc/getcuesbystudy_fxs.R")

#d is bb.noNA, use this to find the filed weiberger studies
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

#### This is a list of weinberger stduies
weinstuds<-as.vector(paste(weinberger$datasetID, weinberger$study))
weinstuds<-unique(weinstuds)

####make a new column in bb.stan
bb.stan$dataIDstudyID<-paste(bb.stan$datasetID, bb.stan$study)

##add the weinberger infrmation
bb.stan$weinberger<-ifelse(bb.stan$dataIDstudyID %in% weinstuds,1,0)

###check waht species are weinberger
wein.sp<-filter(bb.stan,weinberger==1)
unique(wein.sp$complex.wname) 
not.wein<-filter(bb.stan,weinberger==0)
unique(not.wein$complex.wname)
sp.match<-intersect(unique(wein.sp$complex.wname), unique(not.wein$complex.wname))

sp.match ### species in both
######################
####make datalist
wein.data <- with(bb.stan, 
                    list(y=resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         weinberger= weinberger,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

###model
m2l.ni = stan('stan/weinbergerint.stan', data = wein.data,
              iter = 2500, warmup=1500)

wein.mod.2 = stan('stan/weinberger_fewint.stan', data = wein.data,
              iter = 2500, warmup=1500)

wein.mod.3 = stan('stan/wein_intpoolonly.stan', data = wein.data,
                  iter = 2500, warmup=1500)


wein.sum <- summary(wein.mod.2)$summary
wein.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
               "b_weinberger", "b_cw","b_pw","b_fw"),]

wein.sum2 <- summary(wein.mod.3)$summary
wein.sum2[c("mu_a_sp", "b_force", "b_photo", "b_chill",
           "b_weinberger", "b_cw","b_pw","b_fw"),]



