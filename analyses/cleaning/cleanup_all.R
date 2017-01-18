# Started 18 January 2017 - Cat
## Removing duplicate rows and errors

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(dplyr)
library(tidyr)

# Set working directory: 
setwd("~/Documents/git/ospree/analyses/output")
d<- read.csv("ospree_master_clean.csv")

# ghelardini10 issues
for(i in d){
    y <- d[!(d$datasetID == "ghelardini10" & d$material == "root cuttings") &
             !(d$datasetID == "ghelardini10" & d$Entered.By == "DF"),]
  }

write.csv(y, "~/Documents/git/ospree/analyses/output/ospree_master_clean.csv", row.names = FALSE)
