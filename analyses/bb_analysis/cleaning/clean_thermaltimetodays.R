# Cleaning thermaltime studies, converting to daystobudburst
# Cat - 6 March 2017 
# Cat - reevaluated 22 March 2017
# Using ospree_clean.csv right now, should use new file from Nacho's clean merge all file for bb_analysis/cleaning


# Clear Workspace
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Load libraries
library(dplyr)
library(tidyr)

## read data
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 

setwd("~/Documents/git/ospree/analyses")
d<-read.csv("output/ospree_clean.csv",as.is=TRUE)
dtt<-subset(d,respvar.simple=="thermaltime") #basler12, ghelardini10, heide93, karlsson03,
                                              #laube14a, skuterud94
# Both thermal time and daystobudburst were entered for basler12, 
# Both thermal time and percentbudburst were entered for ghelardini10
# Cannot figure out laube14a or skuterud94


## Can change heide93 from thermaltime to daystobudburst
# On pg 533, 2nd paragraph, equation can be found. Use 0 degC as base temp so to convert
# from degree days to days, simply divide the degree days by the forcetemp
# daystobudburst = response/forcetemp
d$response.time[which(d$datasetID=="heide93" & d$respvar.simple=="thermaltime")] <-
  as.numeric(d$response[which(d$datasetID=="heide93" & d$respvar.simple=="thermaltime")])/
  as.numeric(d$forcetemp[which(d$datasetID=="heide93" & d$respvar.simple=="thermaltime")])
d$response[which(d$datasetID=="heide93" & d$respvar.simple=="thermaltime")] <- 1
d<-within(d, respvar.simple[datasetID=="heide93" & respvar.simple=="thermaltime"]<-"daystobudburst")

## Can change karlsson03 from thermaltime to daystobudburst
# On pg 620, Fig 1 caption, equation can be found. Use 2 degC as base temp
# from degree days to days, simply divide the degree days by the forcetemp - 2degC
# daystobudburst = response.time/(forcetemp-2)
d$response.time[which(d$datasetID=="karlsson03")] <-
  as.numeric(d$response.time[which(d$datasetID=="karlsson03")])/(
  as.numeric(d$forcetemp[which(d$datasetID=="karlsson03")]) - 2)
d<-within(d, respvar.simple[datasetID=="karlsson03"]<-"daystobudburst")


