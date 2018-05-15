## Started 15 May 2018 ##
## Select only the species Isabelle has been working on in 2017-2018 experiments ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) {    
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 
setwd("~/Documents/git/ospree/analyses")

# get the data 
bb <- read.csv("output/ospree_clean_withchill.csv", header=TRUE)

## Side note look at studies with dormancy induction information
dormin <- subset(bb, dormancy_induction_temp_day!="")
unique(dormin$datasetID)

## Get data with Isabelle species
bb$latbi <- paste(bb$genus, bb$species, sep="_")

isaspp <- c("Acer_saccharum", "Betula_alleghaniensis", "Fagus_grandifolia",
           "Populus_grandidentata", "Quercus_rubra", "Tilia_americana")

bbsm <- bb[which(bb$latbi %in% isaspp),]

# check the one species missing...
fagus <- subset(bb, genus=="Fagus")
unique(fagus$latbi) # no data on faggra

write.csv(bbsm, "output/bb_isabellespp.csv", row.names=FALSE)

