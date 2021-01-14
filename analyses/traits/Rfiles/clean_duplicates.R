# Looking for duplicated data in the traits data

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(stringr)
library(plyr)
library(dplyr)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Desktop/ospree_trait_analysis/")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

## reading in data
#setwd("~/GitHub/ospree/analyses/output/")

d <- read.csv("input/try_bien.csv") 

refs <- aggregate(d["SpeciesName"], d[c("Reference", "Reference...source","database")], FUN=length) 
#There are abou 38 datasets that might be duplicated

head(d)
  
d$refabr<-strtrim(d$Reference...source,4);head(d); #since the references are often not written in the same format, I am creating a new variable of just the first four letters

## select target variables for which we will search for duplicates:
tar.var<-c("SpeciesName","TraitName","UnitName","refabr")
resp.var<-c("TraitValue")

## subset data to look for duplicates (resp.var are included or most of the subset is duplicated)
trt.sub<-d[,c(tar.var,resp.var)]
head(trt.sub)
## remove duplicated rows in a simple way
trt.sub.no.dup<-d[!duplicated(trt.sub),]

dim(trt.sub)
dim(trt.sub.no.dup) # as of January 7, 2021 we delete 1233622

sort(unique(trt.sub.no.dup$refabr))

refs <- aggregate(d["SpeciesName"], d[c("Reference", "Reference...source", "refabr","database")], FUN=length) 
refs.nd <- aggregate(trt.sub.no.dup["SpeciesName"], trt.sub.no.dup[c("Reference", "Reference...source", "refabr","database")], FUN=length) 

# Think it worked, but it would be useful to get someone else to do some double checks.
write.csv(trt.sub.no.dup, "try_bien_dodups.csv")


# Don't seem to have lost anything unique
length(unique(d$SpeciesName)); length(unique(trt.sub.no.dup$SpeciesName))
length(unique(d$TraitName)); length(unique(trt.sub.no.dup$TraitName))
length(unique(d$TraitValue)); length(unique(trt.sub.no.dup$TraitValue))

              