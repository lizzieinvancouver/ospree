## Started 2 February 2017 ##
## By Lizzie (so far) ###

## More on author clustering in OSPREE ##

###########################################################
## Comverts hand-written (eek, I know) labgroups to back to datasetID ##
###########################################################

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/labgroups")

# library (for set operations)
library(dplyr)

## useful f(x)
stripwhite <- function(x) {
    if (!is.character(x)) {
        stop("x must be a character vector")
    }
    sub("^ *([^ ]*) *$", "\\1", x)
}

# get the hand-written categories
auts <- read.csv("output/aut.names.categorized.csv", header=TRUE)
autsid <- read.csv("output/aut.long.sm.csv", header=TRUE)

names(auts) <- c("gephiID", "author", "cat")
names(autsid) <- c("datasetID", "autnum", "fullname", "author")

# clean and looksee
auts$cat[auts$cat==""] <- "other"
table(auts$cat)

autsid$author  <- stripwhite(autsid$author)
autsid$author <- tolower(autsid$author)
autsid.sm <- subset(autsid, select=c(datasetID, author))

# check for overlap
setdiff(unique(autsid$author), unique(auts$author))
setdiff(unique(auts$author),unique(autsid$author)) # none

# now back-convert to datasetID
labgroups <- merge(auts, autsid.sm, by="author", all.y=TRUE)
labgroups.out <- subset(labgroups, is.na(cat)==FALSE)

labgroups.out <- subset(labgroups.out, select=c("cat", "datasetID"))
labgroups.out <-  labgroups.out[!(duplicated(labgroups.out)),]

# check 
length(unique(labgroups.out$datasetID))
length(unique(autsid$datasetID))

# write output for now ....
write.csv(labgroups.out, "..//output/labgroups.csv", row.names=FALSE)
