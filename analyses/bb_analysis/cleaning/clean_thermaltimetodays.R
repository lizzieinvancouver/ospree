# Cleaning thermaltime studies and changing to daystobudburst
# Cat - 6 March 2017
# Working on forcing and chilling plots for daystobudburst studies


# Clear Workspace
#rm(list=ls()) 
#options(stringsAsFactors=FALSE)

# Load libraries
#library(dplyr)
#library(tidyr)

## read data
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
#if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
#} else 
#  setwd("~/Documents/git/ospree/analyses")
#d<-read.csv("output/ospree_clean_withchill.csv",as.is=TRUE)

if(is.data.frame(d)){

## Can change ghelardini10 & heide93 from thermaltime to daystobudburst
# Add note for page number for thermal time conversion equation
d<-within(d, respvar.simple[datasetID=="ghelardini10" & respvar.simple=="thermaltime"]<-"daystobudburst")
d<-within(d, respvar.simple[datasetID=="heide93"]<-"daystobudburst")

dbb<-subset(d,respvar.simple=="daystobudburst")

# converting response.time to daystobudburst 
dbb$response.time[which(dbb$datasetID=="ghelardini10")] <-
    as.numeric(dbb$response[which(dbb$datasetID=="ghelardini10")])/
    as.numeric(dbb$forcetemp[which(dbb$datasetID=="ghelardini10")])
dbb$response.time[which(dbb$datasetID=="heide93")] <-
    as.numeric(dbb$response[which(dbb$datasetID=="heide93")])/
    as.numeric(dbb$forcetemp[which(dbb$datasetID=="heide93")])

} else {
  print("Error: d is not a data.frame")
}

stop("Not an error, thermal time is passed to days now")
