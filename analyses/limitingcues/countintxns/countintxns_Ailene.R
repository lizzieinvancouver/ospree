#Lizzie's count interactions challenge
#STarted by Ailene on 8 Mar 2019
#We need to count up the number of interactions across:
  
  #photoperiod_day
  #force
  #chill
  #chill days
  #field.sample date

#On both the BB data for OSPREE and the full OSPREE database (minus nonwoody species). 
#Start by reading in the database!
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")


# Load libraries
library(dplyr)
#Read in data
d <- read.csv("output/ospree_clean_withchill.csv") 
#d <- read.csv("output/ospree_clean_withchill_BB.csv") #BB only

N <- 10000

d <- data.frame(
  ID=seq(1, N), 
  v1=sample(c("M","F", "M", "L"), N, replace = TRUE), 
  v2=sample(c("D","M","D","D"), N, replace = TRUE), 
  v3=sample(c("F","G","F","E"), N, replace = TRUE),
  v4=sample(c("A","B","A","B"), N, replace = TRUE)
)

With dplyr



dsub<-subset(d,select=c(datasetID,study,photoperiod_day,forcetemp,chilltemp,chilldays,fieldsample.date))
#to be an interaction, you need to have more than one combination of values across 2 columns
#we want to do this for each study, so
dsub$st<-paste(dsub$datasetID,dsub$study,sep=".")
studies<-unique(dsub$st)
stintxns<-c()
for(i in 1:length(studies)){
  stdat<-dsub[dsub$st==studies[i],]
  intxns<-plyr::count(stdat, c('photoperiod_day','forcetemp','chilltemp','chilldays','fieldsample.date')) 
  pfint<-0
  pctempint<-0
  pcdaysint<-0
  fctempint<-0
  fcdaysint<-0
  numintxns<-c(stdat$datasetID[1],stdat$study[1],pfint,pctempint,pcdaysint,fctempint,fcdaysint)
  
  stintxns<-rbind(stintxns,numintxns)
}
intxns<-plyr::count(dsub, c('photoperiod_day','forcetemp','chilltemp','chilldays','fieldsample.date')) 

dim(intxns)
dsub$pfint<-0
dsub$pfint[dsub$photoperiod_day!=""|dsub$photoperiod_day!="constant"|]
