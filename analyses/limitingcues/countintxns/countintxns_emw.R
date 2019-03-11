## Started 2 March 2019 ##
## By Lizzie ##

# Start your own R file to make a f(x) to count interactive experiments, ideally for all possible intxns across photoperiod, chilling, forcing, and field sample date (maybe provenance) treatments! #

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")



# Build a simple example
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
goober <- data.frame(datasetID=character(), study=character(), photo=numeric(),
    forcedistinphoto=numeric())
for (did in unique(testdat$datasetID)){
    subbydid <- subset(testdat, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp <- subset(subbydid, study==studyid)
             for (photohere in unique(subbydidexp$photo)){
                 samephoto <- subset(subbydidexp, photo==photohere)
                 uniquehere <- samephoto[!duplicated(samephoto),]
                 forcedist <- dist(uniquehere$force)
                 if (length(forcedist)==0) forcedist=0 else {forcedist <- as.vector(forcedist)}
                 gooberadd <- data.frame(datasetID=rep(did, length(forcedist)),
                     study=rep(studyid, length(forcedist)), photo=photohere, forcedist=forcedist)
                 goober <- rbind(goober, gooberadd)
                 }
          }
    }

# Functionalize ...
get.treatdists <- function(df, treatcol, othertreatcol){
    goober <- data.frame(datasetID=character(), study=character(), treat=numeric(),
        othertreat.n=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp <- subset(subbydid, study==studyid)
             for (treathere in unique(subbydidexp[[treatcol]])){
                 sametreat <- subbydidexp[which(subbydidexp[[treatcol]]==treathere),]
                 uniquehere <- sametreat[!duplicated(sametreat),]
                 othertreatdist <- dist(uniquehere[[othertreatcol]])
                 if (length(othertreatdist)==0) gooberadd <- data.frame(datasetID=did,
                     study=studyid, treat=treathere, othertreat.n=0) else {
                 gooberadd <- data.frame(datasetID=did, study=studyid, treat=treathere,
                     othertreat.n=length(as.vector(othertreatdist)))
                  }
                 goober <- rbind(goober, gooberadd)
                 }
          }
    }
  return(goober)
}

get.treatdists(testdat, "photo", "force")

# Ready to switch to read data?
dat <- read.csv("output/ospree_clean.csv",header = TRUE)
dat <- dat[dat$woody=="yes",]
