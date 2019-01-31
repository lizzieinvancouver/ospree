###checking, running same as Cat's Latitude_Studies.R but using ospree_clean_withchill_bb.csv"
#instead of ospree_clean_withchill.csv" as datasheet

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set working directory

if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else
  setwd("~/Documents/git/ospree/analyses")
studytype <- read.csv("output/studytype.csv", header=TRUE)
ospree<-read.csv("output/ospree_clean_withchill_bb.csv", header=TRUE)

lat<-studytype%>%
  filter(latitude.count>=2)%>%
  dplyr::select(datasetID, latitude.count, longitude.count)

reduced<-studytype%>%
  filter(latitude.count>=2)%>%
  filter(respvar.simple=="daystobudburst")%>%
  dplyr::select(datasetID, latitude.count, longitude.count) %>%
  group_by(datasetID)%>%
  filter(row_number()==1)

datasets<-unique(reduced$datasetID)
osp<- ospree %>% filter(datasetID %in% datasets) #1418 rows
unique(osp$datasetID)
unique(osp$respvar)###this has other respvars
unique(ospree$respvar)
