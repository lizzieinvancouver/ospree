rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {setwd("~/Documents/github/ospree/analyses")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

trt<-read.csv('~/Desktop/ospree_trait_analysis/bienwide.csv', header=TRUE)

source("traits/source/bbdataplease.R")

head(bb.noNA)

# To do: 
#create an average trait value for each species
# subset ospree for the subset of species we are using here and combine it with the trait data
