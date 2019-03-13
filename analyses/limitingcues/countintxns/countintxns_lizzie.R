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

# Combinations idea ...
goo <- subset(testdat, datasetID=="bob14" & study=="exp1")
bubber <- goo[!duplicated(goo), ]
expand.grid(unique(bubber$photo), unique(bubber$force))

# Diffs ideas ...
# First I do just a loop ... 
goober <- data.frame(datasetID=character(), study=character(), photo=numeric(),
    forcedistinphoto=numeric())
for (did in unique(testdat$datasetID)){
    subbydid <- subset(testdat, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp <- subset(subbydid, study==studyid)
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
                             if (length(forcedist)==0) gooberadd <- data.frame(datasetID=did,
                                study=studyid, intxn=0) else{
                                gooberadd <- data.frame(datasetID=did, study=studyid, intxn=1)
                                }
                           }
                     }
                 goober <- rbind(goober, gooberadd)
                 goobermean <- aggregate(goober["intxn"], goober[c("datasetID", "study")], FUN=mean)
                 }
          }
    }

goobermean # Hmm, this does not come out the same as when I functionalize ....

# Functionalize the above!
get.treatdists <- function(df, treatcol, othertreatcol){
    goober <- data.frame(datasetID=character(), study=character(), intxn=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp <- subset(subbydid, study==studyid)
             for (treathere in unique(subbydidexp[[treatcol]])){
                 sametreat <- subbydidexp[which(subbydidexp[[treatcol]]==treathere),]
                 uniquehere <- sametreat[!duplicated(sametreat),]
                 othertreatdist <- dist(uniquehere[[othertreatcol]])
                 # If there is not more than one other treatment, intxn=0
                 if (length(othertreatdist)==0) gooberadd <- data.frame(datasetID=did,
                     study=studyid, intxn=0) else {
                     subby.othertreat <- subbydidexp[which(subbydidexp[[othertreatcol]] %in% uniquehere[[othertreatcol]]),]
                     subby.diff <- subby.othertreat[which(!subby.othertreat[[treatcol]] %in% treathere),]
                       # If there are not any other treatments with a different treats and treathere, intxn=0
                       if (nrow(subby.diff) <= 1) gooberadd <- data.frame(datasetID=did,
                       study=studyid, intxn=0) else{
                           uniquehereagain <- subby.diff[!duplicated(subby.diff),]
                           othertreatdistagain <- dist(uniquehereagain[[othertreatcol]])
                             if (length(othertreatdistagain)==0) gooberadd <- data.frame(datasetID=did,
                                study=studyid, intxn=0) else{
                                gooberadd <- data.frame(datasetID=did, study=studyid, intxn=1)
                                }
                           }
                     }
                 goober <- rbind(goober, gooberadd)
                 goobermean <- aggregate(goober["intxn"], goober[c("datasetID", "study")], FUN=mean)
                 }
          }
    }
  return(goobermean)
}

get.treatdists(testdat, "photo", "force")


# Ready to switch to read data? Maybe ...
dat <- read.csv("output/ospree_clean.csv",header = TRUE)
dat <- dat[dat$woody=="yes",]

datsm <- subset(dat, select=c("datasetID", "study", "genus", "species", "forcetemp", "photoperiod_day", "fieldsample.date",
    "chilltemp", "chillphotoperiod", "chilldays"))

datsm$force <- as.numeric(datsm$forcetemp)
datsm$photo <- as.numeric(datsm$photoperiod_day)

datsm.noNA <- subset(datsm, is.na(force)==FALSE & is.na(photo)==FALSE)

osp.fp <- get.treatdists(datsm.noNA, "photo", "force")
osp.fpintxn <- subset(osp.fp, intxn==1)
