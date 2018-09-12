#Ailene Ettinger
#11 Sept 2018
#Compare data in new files with that in older files.
# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")


# Load libraries
library(dplyr)
library(tidyr)

d.new <- read.csv("output/ospree_clean_withchill_BB.csv") 
dim(d.new)#7370, 85
d.july <- read.csv("output/ospree_clean_withchill_BB_2018Jul12.csv") # 29 May 2018: 12693 
dim(d.july)#7370, 85
d.mar <- read.csv("output/ospree_clean_withchill_BB_2018Mar26.csv") # 29 May 2018: 12693 
dim(d.mar)#8484   84

#Check chilling data in each file:
d.new$Field_Utah_Model<-as.numeric(d.new$Field_Utah_Model)
d.mar$Field_Utah_Model<-as.numeric(d.mar$Field_Utah_Model)
d.july$Field_Utah_Model<-as.numeric(d.july$Field_Utah_Model)
d.new$forcetemp<-as.numeric(d.new$forcetemp)
d.mar$forcetemp<-as.numeric(d.mar$forcetemp)
d.july$forcetemp<-as.numeric(d.july$forcetemp)
d.new$photoperiod_day<-as.numeric(d.new$photoperiod_day)
d.mar$photoperiod_day<-as.numeric(d.mar$photoperiod_day)
d.july$photoperiod_day<-as.numeric(d.july$photoperiod_day)

length(which(is.na(d.new$Field_Utah_Model)))#3349; also looked at Total_Utah- fewer nas and less difference between march and now
length(which(is.na(d.july$Field_Utah_Model)))#3349
length(which(is.na(d.mar$Field_Utah_Model)))#3965
length(which(is.na(d.new$forcetemp)))#1309
length(which(is.na(d.july$forcetemp)))#1309
length(which(is.na(d.mar$forcetemp)))#1180
length(which(!is.na(d.new$forcetemp)))#6061
length(which(!is.na(d.july$forcetemp)))#6061
length(which(!is.na(d.mar$forcetemp)))#7304
#why was there more forcing data in march?
#did we lose a particular study?
sort(unique(d.mar$datasetID[which(is.na(d.mar$forcetemp))]))#8 studies had NAs for oforcing in march
#"ashby62"     "boyer"       "cannell83"   "lamb37" "ruesink98"   "sanzperez10" "skuterud94"  "sonsteby13" 
sort(unique(d.new$datasetID[which(is.na(d.new$forcetemp))]))#10 studies have NAs in forcing now- this makes sense
#"ashby62"     "basler12"   "boyer"       "cannell83"   "lamb37" "ruesink98"   "sanzperez10" "skuterud94"  "sonsteby13" 
#new nas in forcing are in basler12 and schnabel87 
#in march- all basler studies had 7 for forcetemp; now there are differeces
#fewer NAs in july/now then in march- good
sort(unique(d.mar$genus[which(!is.na(d.mar$Field_Utah_Model))]))#85 unique genera in march did not have NAs
sort(unique(d.new$genus[which(!is.na(d.new$Field_Utah_Model))]))#85 now too
sort(unique(d.july$genus[which(!is.na(d.july$Field_Utah_Model))]))#85
#so we didn't gain any new genera due to new chilling data
#how about species?
sort(unique(d.mar$species[which(!is.na(d.mar$Field_Utah_Model))]))#184 unique species in march did not have NAs
sort(unique(d.new$species[which(!is.na(d.new$Field_Utah_Model))]))#189 now, so gained 5 new species
sort(unique(d.july$species[which(!is.na(d.july$Field_Utah_Model))]))#189

#how about due to forcing data
length(which(is.na(d.new$)))#3349
length(which(is.na(d.july$Field_Utah_Model)))#3349
length(which(is.na(d.mar$Field_Utah_Model)))#3965

#now look at photoperiod
length(which(is.na(d.new$photoperiod_day)))#1309
length(which(is.na(d.july$photoperiod_day)))#1309
length(which(is.na(d.mar$photoperiod_day)))#1180

#more NAS now than in march...hmm
sort(unique(d.mar$datasetID[which(is.na(d.mar$photoperiod_day))]))#"cannell83" "ruesink98"
sort(unique(d.new$datasetID[which(is.na(d.new$photoperiod_day))]))#"cannell83" "ruesink98" "schnabel87"
#Look into some weird things:
#kiwi is in 2 studies! biasili12 (in south america) and guerriero90 (in europe)
cbind(d.new$datasetID,d.new$continent, 
      d.new$Total_Chill_portions,d.new$forcetemp,
      d.new$avg_bbtemp,d.new$photoperiod_day)[d.new$genus=="Actinidia",]
#let's look at kiwi in march: biasili12 (in south america) and guerriero90 (in europe)
cbind(d.mar$datasetID,d.mar$continent, 
      d.mar$Total_Chill_portions,d.mar$forcetemp,
      d.mar$avg_bbtemp,d.mar$photoperiod_day)[d.mar$genus=="Actinidia",]

#3 studies use V. vinifera: biasi12, nishimoto95 (in asia), schnabel87
cbind(d.new$datasetID,d.new$continent, 
      d.new$Total_Chill_portions,d.new$forcetemp,
      d.new$avg_bbtemp,d.new$photoperiod_day)[d.new$species=="vinifera",]
cbind(d.new$datasetID,d.new$Total_Chill_portions,d.new$forcetemp,
      d.new$avg_bbtemp,d.new$photoperiod_day)[d.new$species=="vinifera",]


cbind(d.new$datasetID[d.new$genus=="Actinidia"], 
      d.new$Total_Chill_portions[d.new$genus=="Actinidia"],d.mar$Total_Chill_portions[d.mar$genus=="Actinidia"],d.new$forcetemp[d.new$genus=="Actinidia"],d.mar$forcetemp[d.mar$genus=="Actinidia"],
      d.new$avg_bbtemp[d.new$genus=="Actinidia"])
cbind(d.new$datasetID[d.new$genus=="Actinidia"], 
      d.new$Total_Chill_portions[d.new$genus=="Actinidia"],d.mar$Total_Chill_portions[d.mar$genus=="Actinidia"],d.new$photoperiod_day[d.new$genus=="Actinidia"],d.mar$photoperiod_day[d.mar$genus=="Actinidia"])
#chilling has changed from march to now for some rows of guerriero90

cbind(d.new$datasetID[d.new$genus=="Actinidia"], 
      d.new$Total_Chill_portions[d.new$genus=="Actinidia"],d.mar$Total_Chill_portions[d.mar$genus=="Actinidia"],d.new$photoperiod_day[d.new$genus=="Actinidia"],d.mar$photoperiod_day[d.mar$genus=="Actinidia"])


cbind(d.new$datasetID[d.new$species=="vinifera"], 
      d.new$Total_Chill_portions[d.new$species=="vinifera"],d.mar$Total_Chill_portions[d.mar$species=="vinifera"],d.new$photoperiod_day[d.new$species=="vinifera"],d.mar$photoperiod_day[d.mar$species=="vinifera"])
