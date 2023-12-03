## Started 3 December 2023 ##
## Try to pull out interesting chill studies ... ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) {    
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 
setwd("~/Documents/git/ospree/analyses")

# get the data 
bb <- read.csv("output/ospree_clean_withchill.csv", header=TRUE)

## Start with the interesting columns ... 
## Look at studies with dormancy induction information
dormin <- subset(bb, dormancy_induction_temp_day!="")
unique(dormin$datasetID)  
# 14 studies, incl. junttila12, schnabel87, thielges75, campbell75

## Studies with freeze treatments ...  see also checking_freeze.treatment.R 
freeze1 <- subset(bb, freeze.treatment.time!="")
freeze2 <- subset(bb, freeze.treatment.temp_day!="") # there are more columns too
unique(freeze1$datasetID)
unique(freeze2$datasetID)
# Check "cook05"     "schnabel87"

## What about chill treatments below 0?
unique(bb$chilltemp)
interestingchilltemps <- c("Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C",
	"-1", "-4, 0, 4", "-4, 8, 8", "-5", "0, 4, 8", "4, 0, -4",  "8, 4, 0", "8, 8, -4", "-3, 3", "-3", "-3,2", "-10")
chillneg <- bb[which(bb$chilltemp %in% interestingchilltemps),]
unique(chillneg$datasetID) # 7 studies, incl. jones12, lamb37, granhus09

# One more, which studies have light in their chilling?
unique(bb$chillphotoperiod)
interestingchillphotoperiods <- c("10", "9", "8", "16", "12", "13", "AB")
chillphoto <- bb[which(bb$chillphotoperiod %in% interestingchillphotoperiods),]
unique(chillphoto$datasetID) # 11 incl. worrall67, man17, nienstaedt66