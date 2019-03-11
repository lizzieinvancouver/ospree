#Dan's version
# Start your own R file to make a f(x) to count interactive experiments, ideally for all possible intxns across photoperiod, chilling, forcing, and field sample date (maybe provenance) treatments! #

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)
library(data.table)
library(forcats)
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

onsie<- data.frame(datasetID=rep("bob10", 8), study=rep("exp1", 8),
                     photo=c(8, 8, 8, 8, 8,8, 8,8), force=c(20, 20, 25, 25, 20,
                                                                  20, 25, 25))
testdat <- rbind(nointxn, wintxn,onsie)
testdat



testdat$ID<-paste(testdat$datasetID,testdat$study)

a<-testdat%>% group_by(datasetID,study,ID) %>% distinct(photo,force)

### next filter any studies that don't manipulate more than 1 level (Bob 10)
##then make sure there are interactions f







?distinct()


# Ready to switch to read data?
dat <- read.csv("output/ospree_clean.csv",header = TRUE)
dat <- dat[dat$woody=="yes",]
