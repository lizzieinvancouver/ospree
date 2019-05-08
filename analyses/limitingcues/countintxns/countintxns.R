## Started 2 March 2019 ##
## By Lizzie ##

# Start your own R file to make a f(x) to count interactive experiments, ideally for all possible intxns across photoperiod, chilling, forcing, and field sample date (maybe provenance) treatments! #

## See also: https://github.com/lizzieinvancouver/ospree/issues/235

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
dat <- read.csv("output/ospree_clean.csv",header = TRUE)
dat <- dat[dat$woody=="yes",]
dat$fieldsample.date <- as.Date(dat$fieldsample.date, format="%d-%b-%Y")
dat$doy <- format(dat$fieldsample.date, "%j")

# Get the number of field sampling dates that are 14 or more weeks apart, first for each datasetIDx study ...
ddatefx.all <- subset(dat, select=c("datasetID", "study", "fieldsample.date"))
ddatefx <- ddatefx.all[!duplicated(ddatefx.all), ]
ddatefx$datasetIDstudy <- paste(ddatefx$datasetID, ddatefx$study)

## Change main DF so only relevant dates are included ... not pretty, but should work
# First get the unique dates in a df
dates2weeks.count <- countfieldsample(ddatefx, 14)
uniquedates.df <- fieldsample.getuniquedates(ddatefx, 14)
uniquedates.df$selectcolumn <- paste(uniquedates.df$datasetIDstudy, uniquedates.df$date)
# Now subset to sane # of columnns
datsm <- subset(dat, select=c("datasetID", "study", "genus", "species", "forcetemp", "photoperiod_day", 
    "fieldsample.date", "chilltemp", "chillphotoperiod", "chilldays"))
head(datsm)

## Okay, formatting to look at intxns
datsm$force <- as.numeric(datsm$forcetemp)
datsm$photo <- as.numeric(datsm$photoperiod_day)

datsm.noNA <- subset(datsm, is.na(force)==FALSE & is.na(photo)==FALSE)

osp.fp <- get.treatdists(datsm.noNA, "photo", "force")
osp.fpintxn <- subset(osp.fp, intxn>=2)
osp.fpintxn[order(osp.fpintxn$datasetID),]

osp.ctf <- get.treatdists(datsm.noNA, "chilltemp", "force")
osp.ctfintxn <- subset(osp.ctf, intxn>=2)

osp.cdf <- get.treatdists(datsm.noNA, "chilldays", "force")
osp.cdfintxn <- subset(osp.cdf, intxn>=2)

lookatunique <- get.uniquetreats(datsm.noNA, "photo", "force")

# Now (not pretty part) we'll take all NA dates ...
datsm$selectcolumn <- paste(datsm$datasetID, datsm$study, datsm$fieldsample.date)
datsm14d <- datsm[which(datsm$selectcolumn %in% uniquedates.df$selectcolumn),]

dim(datsm)
dim(datsm14d) # not such a huge loss of rows

datsm14d.noNA <- subset(datsm14d, is.na(force)==FALSE & is.na(photo)==FALSE)

# Repeat of the above but correcting for field sampling date repetition
osp14d.fp <- get.treatdists(datsm14d.noNA, "photo", "force")
osp14d.fpintxn <- subset(osp14d.fp, intxn>=2) # 14 studies
osp14d.fpintxn[order(osp14d.fpintxn$datasetID),]

osp14d.ctf <- get.treatdists(datsm14d.noNA, "chilltemp", "force")
osp14d.ctfintxn <- subset(osp14d.ctf, intxn>=2) # 2 studies

osp14d.cdf <- get.treatdists(datsm14d.noNA, "chilldays", "force")
osp14d.cdfintxn <- subset(osp14d.cdf, intxn>=2) # same 2 studies # skuterud94  exp1  &  heide12  exp2

osp14d.daysf <- get.treatdists(datsm14d.noNA, "fieldsample.date", "force")
osp14d.daysfintxn <- subset(osp14d.daysf, intxn>=2) # 9 studies

osp14d.daysp <- get.treatdists(datsm14d.noNA, "fieldsample.date", "photo")
osp14d.dayspintxn <- subset(osp14d.daysp, intxn>=2) # 11 studies

length(unique(paste(datsm14d$datasetID, datsm14d$study)))

##################
# BB OSPREE data #
##################
## Tried to ref bbstandleadin.R here but a pain ... ##

bb <- read.csv("output/bbstan_utahzscore_nocrops_exprampedfp_allchill.csv", header=TRUE)

# Prep df for dates ..
bb$fieldsample.date <- as.Date(bb$fieldsample.date, format="%d-%b-%Y")

bb.ddatefx.all <- subset(bb, select=c("datasetID", "study", "fieldsample.date"))
bb.ddatefx <- bb.ddatefx.all[!duplicated(bb.ddatefx.all), ]
bb.ddatefx$datasetIDstudy <- paste(bb.ddatefx$datasetID, bb.ddatefx$study)

## Change bb DF so only relevant dates are included ... again, not pretty, but should work
# First get the unique dates in a df
bb.dates2weeks.count <- countfieldsample(bb.ddatefx, 14)
bb.uniquedates.df <- fieldsample.getuniquedates(bb.ddatefx, 14)
bb.uniquedates.df$selectcolumn <- paste(bb.uniquedates.df$datasetIDstudy, bb.uniquedates.df$date)
# Now (not pretty part) we'll take all NA dates ...
bb$selectcolumn <- paste(bb$datasetID, bb$study, bb$fieldsample.date)
bb14d <- bb[which(bb$selectcolumn %in% uniquedates.df$selectcolumn),]

dim(bb)
dim(bb14d) # this makes a big difference

bb14d.noNA <- subset(bb14d, is.na(force)==FALSE & is.na(photo)==FALSE)

# Repeat of the what we did above for all OSPREE data correcting for field sampling date repetition
bb14d.fp <- get.treatdists(bb14d.noNA, "photo", "force")
bb14d.fpintxn <- subset(bb14d.fp, intxn>=2) 
bb14d.fpintxn[order(bb14d.fpintxn$datasetID),]
length(unique(paste(bb14d.fpintxn$datasetID, bb14d.fpintxn$study))) # 14 studies

bb14d.ctf <- get.treatdists(bb14d.noNA, "chilltemp", "force")
bb14d.ctfintxn <- subset(bb14d.ctf, intxn>=2) 
length(unique(paste(bb14d.ctfintxn$datasetID, bb14d.ctfintxn$study))) # 1 study: skuterud94 exp1

bb14d.cdf <- get.treatdists(bb14d.noNA, "chilldays", "force")
bb14d.cdfintxn <- subset(bb14d.cdf, intxn>=2)  
length(unique(paste(bb14d.cdfintxn$datasetID, bb14d.cdfintxn$study))) # 5 studies

bb14d.daysf <- get.treatdists(bb14d.noNA, "fieldsample.date", "force")
bb14d.daysfintxn <- subset(bb14d.daysf, intxn>=2) # 
length(unique(paste(bb14d.daysfintxn$datasetID, bb14d.daysfintxn$study))) # 7 studies

bb14d.daysp <- get.treatdists(bb14d.noNA, "fieldsample.date", "photo")
bb14d.dayspintxn <- subset(bb14d.daysp, intxn>=2) 
length(unique(paste(bb14d.dayspintxn$datasetID, bb14d.dayspintxn$study))) # 9 studies


length(unique(paste(bb14d$datasetID, bb14d$study))) # 38
