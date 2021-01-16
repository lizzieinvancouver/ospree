# Code written by DS ~April 2020 to summarize TRY database data for OSPREE trait analsyes 
#DL edits May 26, 2020 and January 15, 2021 in order to summarize both the try and bien data after we had removed the duplicated data

#Understanding Try data
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(dplyr)

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Desktop/ospree_trait_analysis/")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

#load in dataset
trt <- read.csv("input/try_bien_dodups.csv")
unique(trt$TraitName)

dataperspecies <- trt %>%
  group_by(new.SpeciesName) %>%
  summarise(no_rows = length(new.SpeciesName))

dataperspeciesperdataset<- trt %>%
  group_by(new.SpeciesName, project_pi) %>%
  summarise(no_rows = length(new.SpeciesName))

dataperspeciespertrait<- trt %>%
  group_by(new.SpeciesName, TraitName) %>%
  summarise(no_rows = length(new.SpeciesName),no_datasets=length(unique(project_pi))) 

dataperspeciesperdatasetpertrait<- trt %>%
  group_by(new.SpeciesName, project_pi, TraitName) %>%
  summarise(no_rows = length(new.SpeciesName))

traitperdataset<- trt %>%
  group_by(project_pi) %>%
  summarise(no_trait=length(unique(TraitName))) 

dat<-dataperspeciespertrait
#dat<-dataperspeciesperdatasetpertrait

