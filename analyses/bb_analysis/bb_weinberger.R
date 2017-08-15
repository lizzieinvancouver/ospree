## Started 27 July 2017 ##
## By Lizzie to start ##
## Ailene added to code 17 Aug 2017
## Trying to examine whether estimated effects are consisent across methods ##
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(lme4)
# library(rstan)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else 
  setwd("~/Documents/git/ospree/analyses/bb_analysis")

# get the data .... 
bb <- read.csv("..//output/ospree_clean_withchill_BB.csv", header=TRUE)

## read taxon data
taxinfo <- read.csv("..//output/bb_analysis/taxon/species_manipulation_levels.csv")
taxons <-read.csv("..//output/bb_analysis/taxon/complex_levels.csv")
# get the species we want to use
taxon.info<-subset(taxons,use=="Y")#

# okay, now we have to look at the studies these species are in and include only studies that vary either field sample or chill and then both vary the same other variables (forcing and/or photo) .... 
#subset(taxinfo, field.sample>3 & chill>1 & datasets>1)
#taxinfo2$name
#taxon.info<-subset(taxon.info,taxinfo2$name%in%taxon.info$taxa)#

# Nacho's code 
taxon.info$taxa<-paste(taxon.info$genus,taxon.info$species,sep="_")
bb$bb.taxa<-paste(bb$genus,bb$species,sep="_")
bb<-subset(bb,bb.taxa%in%taxon.info$taxa)
#Look at output/studytype_withBB.csv and see which studies only manipulate fieldsample date and which only manipulate expchill (as a start).
studytype<-read.csv("..//output/studytype_withBB.csv")
#Look at output/studytype_withBB.csv and see which studies only manipulate fieldsample date or which only manipulate expchill (as a start).
fs_exps<-subset(studytype, field.sample>3 |chill>1)
studytype_studies<-unique(fs_exps$datasetID)
#Make a list of these studies, with a column for 'study type' (exp or fieldsample) ... then run a model!
#I think that i may start by using the Exp_Chilling_Hours and Field_Chilling_Hourscolumns in bb instead to create this list
bb$studytype<-NA
bb$studytype[which(!is.na(bb$Exp_Chilling_Hours) & is.na(bb$Field_Chilling_Hours))]<-"exp"#762 rows, across 17 studies have only experimental chilling
bb$studytype[which(is.na(bb$Exp_Chilling_Hours) & !is.na(bb$Field_Chilling_Hours))]<-"fieldsample"#2999 rows across 38 studies have only field chilling
bb$studytype[which(!is.na(bb$Exp_Chilling_Hours) & !is.na(bb$Field_Chilling_Hours))]<-"both"#834 rows across 16 studies have both
fs.studies<-unique(bb$datasetID[which(is.na(bb$Exp_Chilling_Hours) & !is.na(bb$Field_Chilling_Hours))])
exp.studies<-unique(bb$datasetID[which(!is.na(bb$Exp_Chilling_Hours) & is.na(bb$Field_Chilling_Hours))])
bb.study<-bb[which(bb$studytype=="exp"|bb$studytype=="fieldsample"),]
#dim(bb.study)#3761 85
#just include studies with multiple chilling levels:
bb.study<-subset(bb.study,studytype_studies%in%bb.study$datasetID)
#dim(bb.study)#2845 85, across 54 studies (17 exp studies and 37 fieldsample)