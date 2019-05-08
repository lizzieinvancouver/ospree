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

# Build from a simple example
nointxn <- data.frame(datasetID=rep("bob12", 8), study=c(rep("exp2", 4),
    rep("exp2", 4)), photo=c(8, 8, 8, 8, 8, 8, 12, 12),
    force=c(15, 15, 18, 18, 15, 15, 15, 15))
wintxn <- data.frame(datasetID=rep("bob14", 8), study=rep("exp1", 8),
     photo=c(8, 8, 8, 8, 12, 12, 12, 12), force=c(20, 20, 25, 25, 20,
     20, 25, 25))
testdat <- rbind(nointxn, wintxn)
testdat

testdatunique <- testdat[!duplicated(testdat),]

# Combinations idea ... I did not finish this
goo <- subset(testdat, datasetID=="bob14" & study=="exp1")
bubber <- goo[!duplicated(goo), ]
expand.grid(unique(bubber$photo), unique(bubber$force))

# Diffs ideas ... find one level of one multi-level factor, then see if there are multiple levels of the other factor 
# First I do just a loop ... 
goober <- data.frame(datasetID=character(), study=character(), photo=numeric(),
    forcedistinphoto=numeric())
for (did in unique(testdat$datasetID)){
    subbydid <- subset(testdat, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp.allcols <- subset(subbydid, study==studyid)
          subbydidexp <- subbydidexp.allcols[,c("force", "photo")]
             for (photohere in unique(subbydidexp$photo)){
                 # Finally, subset to one photoperiod in each datasetID and study combo ...
                 samephoto <- subset(subbydidexp, photo==photohere)
                 uniquehere <- samephoto[!duplicated(samephoto),]
                 forcedist <- dist(uniquehere$force)
                 # If there is not more than one forcing treatment, intxn=0
                 if (length(forcedist)==0) gooberadd <- data.frame(datasetID=did,
                     study=studyid, intxn=0) else {
                     subby.othertreat <- subbydidexp[which(subbydidexp[["force"]] %in% uniquehere[["force"]]),]
                     subby.diffphoto <- subby.othertreat[which(!subby.othertreat[["photo"]] %in% photohere),]
                       # if there are not any treatments with a different photoperiod and these forcing temps, intxn=0
                       if (nrow(subby.diffphoto) <= 1) gooberadd <- data.frame(datasetID=did,
                       study=studyid, intxn=0) else{
                           # otherwise, see if there are more than one forcing temps ...                      
                           uniquehereagain <- subby.diffphoto[!duplicated(subby.diffphoto),]
                           forcedistagain <- dist(uniquehereagain$force)
                             if (length(forcedistagain)==0) gooberadd <- data.frame(datasetID=did,
                                study=studyid, intxn=0) else{
                                gooberadd <- data.frame(datasetID=did, study=studyid, intxn=1)
                                }
                           }
                     }
                 goober <- rbind(goober, gooberadd)
                 goobersum <- aggregate(goober["intxn"], goober[c("datasetID", "study")], FUN=sum)
                 }
          }
    }

goobersum 

# Functionalize the above!
get.treatdists <- function(df, treatcol, othertreatcol){
    dfbuild <- data.frame(datasetID=character(), study=character(), intxn=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp.allcols <- subset(subbydid, study==studyid)
          subbydidexp <- subbydidexp.allcols[,c(treatcol, othertreatcol)]
             for (treathere in unique(subbydidexp[[treatcol]])){
                 # Finally, subset to one treatment (of treatcol) in each datasetID and study combo ...
                 sametreat <- subbydidexp[which(subbydidexp[[treatcol]]==treathere),]
                 uniquehere <- sametreat[!duplicated(sametreat),]
                 othertreatdist <- dist(uniquehere[[othertreatcol]])
                 # If there is not more than one other treatment, intxn=0
                 if (length(othertreatdist)==0) dfbuildadd <- data.frame(datasetID=did,
                     study=studyid, intxn=0) else {
                     subby.othertreat <- subbydidexp[which(subbydidexp[[othertreatcol]] %in% uniquehere[[othertreatcol]]),]
                     subby.diff <- subby.othertreat[which(!subby.othertreat[[treatcol]] %in% treathere),]
                       # If there are not any other treatments with a different treats and treathere, intxn=0
                       if (nrow(subby.diff) <= 1) dfbuildadd <- data.frame(datasetID=did,
                       study=studyid, intxn=0) else{
                           uniquehereagain <- subby.diff[!duplicated(subby.diff),]
                           othertreatdistagain <- dist(uniquehereagain[[othertreatcol]])
                             if (length(othertreatdistagain)==0) dfbuildadd <- data.frame(datasetID=did,
                                study=studyid, intxn=0) else{
                                dfbuildadd <- data.frame(datasetID=did, study=studyid, intxn=1)
                                }
                           }
                     }
                 dfbuild <- rbind(dfbuild, dfbuildadd)
                 dfbuildsum <- aggregate(dfbuild["intxn"], dfbuild[c("datasetID", "study")], FUN=sum)
                 }
          }
    }
  return(dfbuildsum)
}

get.treatdists(testdat, "photo", "force")

# One more function we need
# f(x) to look at unique treats
get.uniquetreats <- function(df, treatcol, othertreatcol){
    dfbuild <- data.frame(datasetID=character(), study=character(), treat=numeric(), othertreat=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp <- subset(subbydid, study==studyid)
             for (treathere in unique(subbydidexp[[treatcol]])){
                 # Finally, subset to one treatment (of treatcol) in each datasetID and study combo ...
                 sametreat <- subbydidexp[which(subbydidexp[[treatcol]]==treathere),]
                 uniquehere <- sametreat[!duplicated(sametreat),]
                 dfadd <- data.frame(datasetID=rep(did, nrow(uniquehere)), study=rep(studyid, nrow(uniquehere)),
                     treat=uniquehere[[treatcol]], othertreat=uniquehere[[othertreatcol]])
                 dfbuild <- rbind(dfbuild, dfadd)
                 }
          }
     }
  return(dfbuild)
}


# Ready to switch to read data, go!

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

## Here I compare what I and Cat got to see if the code is working 
# Compare to what Cat got ... (taken from countinxns_Cat.R)
# My code was wrong in taking unique rows for the OSPREE data, I fixed it by deleting all but the columns for the two treatments in question from df ...
fp.cat <- c("basler14", "heide05", "heide08", "heide11", "heide93", "heide93a", "okie11", "partanen98", "pettersen71", "Sanz-Perez09", "sogaard08")
setdiff(fp.cat, unique(osp.fpintxn$datasetID))
setdiff(unique(osp.fpintxn$datasetID), fp.cat)

osp.ctf <- get.treatdists(datsm.noNA, "chilltemp", "force")
osp.ctfintxn <- subset(osp.ctf, intxn>=2)

osp.cdf <- get.treatdists(datsm.noNA, "chilldays", "force")
osp.cdfintxn <- subset(osp.cdf, intxn>=2)

lookatunique <- get.uniquetreats(datsm.noNA, "photo", "force")
subset(lookatunique, datasetID=="sogaard08") # According to my cheap code, sogaard08 has three forcing temperatures ... at only one temperature do they vary photoperiod ... what is that? Can you estimate intxns and main effects from it? I am at a loos. 
subset(lookatunique, datasetID=="partanen98") # partanen98 has three photoperiods, varies temp for only one of them.

# Now (not pretty part) we'll take all NA dates ...
datsm$selectcolumn <- paste(datsm$datasetID, datsm$study, datsm$fieldsample.date)
datsm14d <- datsm[which(datsm$selectcolumn %in% uniquedates.df$selectcolumn),]

dim(datsm)
dim(datsm14d)

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
# START HERE!
