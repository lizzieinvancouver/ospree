#Started by Ailene
#May 1, 2018
#Checking things in daily data file:

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

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
#1. Check for mistakes similar to schnaebel, in which gradually decreasing 
#photoperiods were entered as multiple rows, each with a different photoperiod, 
#for the same budburst event (i.e. with the same response time).
d<-read.csv("output/ospree_clean_withchill_BB.csv",header=T)#which version should I use?
#identify rows within studies that have identical response or response.time
treats <- subset(d, select=c("datasetID", "study","genus","species","population","provenance.lat","growing.lat","year","fieldsample.date","chilltemp","chillphotoperiod","chilldays","forcetemp","forcetemp_night","photoperiod_day","respvar","response","response.time"))
treats$datasetID.study.species.response<-paste(treats$datasetID,treats$study,treats$genus,treats$species,treats$poplation,treats$provenance.lat,treats$growing.lat,treats$year,treats$fieldsample.date,treats$respvar)
treats2<-subset(treats, select=c("datasetID","datasetID.study.species.response","chilltemp","chillphotoperiod","chilldays","forcetemp","forcetemp_night","photoperiod_day","respvar","response","response.time"))
treats2dups <- treats2[duplicated(treats2), ]
dim(treats2dups)
unique(treats2dups$datasetID)
tab<-table(treats2dups$datasetID,treats2dups$respvar)
tab<-as.data.frame(cbind(tab,rowSums(tab)))
colnames(tab)[23]<-"sum"
tab2<-tab[which(tab$sum>1),]
#don't really seem to be any....
#2.Still some questions to look into for daily climate data. 

#skuterud94 (i=3226)
#really long forcedays (401), so not  enough daily climate data= need atleast 50 more days of cliamte data()
?
  
#THE BELOW ARE DONE OR CAN'T BE FIXED
#campbel75:DONE I THINK! Average added; also fixed several rounding errors for lats/longs
#"campbell75 1973-09-01 18-27 (20 average) 10 11 9 16", "campbell75 1973-09-01 18-27 (20 average) 10 22 9 16"  "campbell75 1973-09-01 18-27 (20 average) 10 33 9 16" 
# "campbell75 1973-09-01 18-27 (20 average) 10 44 9 16"  "campbell75 1973-09-01 18-27 (20 average) 4.4 11 9 16"
# "campbell75 1973-09-01 18-27 (20 average) 4.4 22 9 16" "campbell75 1973-09-01 18-27 (20 average) 4.4 33 9 16"
#"campbell75 1973-09-01 18-27 (20 average) 4.4 44 9 16" "campbell75 1973-09-01 18-27 (20 average) 7.2 11 9 16"
#"campbell75 1973-09-01 18-27 (20 average) 7.2 22 9 16" "campbell75 1973-09-01 18-27 (20 average) 7.2 33 9 16"
#"campbell75 1973-09-01 18-27 (20 average) 7.2 44 9 16" 

#partanen01- FIXED! All the the same growing lat/long, even though different provenance lat/longs
#"partanen01 1997-12-05 20 ambient 34 ambient 16"      
#"partanen01 1997-12-05 20 ambient 34 ambient 6": lat/longs do not match up.  x$lat/long=60.78333 27.5
#pulled climate lat/longs=61.93333,26.6833

#calme94: only has climate data for lat=46.5, long=-78.833; because only one growing lat/long
#missing climate data for longs: -70.633 -73.250 and for longs 46.133 45.633
#"calme94 1992-10-05 24.4    16","calme94 1992-11-02 24.4    16", "calme94 1992-11-16 24.4    16"                        "calme94 1992-12-07 24.4    16"                       
#"calme94 1993-02-01 24.4    16"                        "calme94 1993-03-29 24.4    16"                       
#"calme94 1993-04-26 24.4    16"                        "calme94 1993-01-11 24.4    16"                       
#"calme94 1993-03-01 24.4    16"                       
#skre08
#"skre08 2001-11-22 20 ambient 111  18"                
#"skre08 2001-01-25 20 ambient 111  18"  
#fixed: chilltemp=ambient+4; this had not been accounted for in code


#skuterud94: not possible to fix (can't distinguish treatments in figure)
#"skuterud94 1993-11-25 mean of 9, 12, 15 0 111 0 8"   
#"skuterud94 1993-11-25 mean of 9, 12, 15 0 39 0 8"     "skuterud94 1993-11-25 mean of 9, 12, 15 0 55 0 8"    
#"skuterud94 1993-11-25 mean of 9, 12, 15 0 67 0 8"     "skuterud94 1993-11-25 mean of 9, 12, 15 0 98 0 8"    
# "skuterud94 1993-11-25 mean of 9, 12, 15 3 111 0 8"    "skuterud94 1993-11-25 mean of 9, 12, 15 3 39 0 8"    
# "skuterud94 1993-11-25 mean of 9, 12, 15 3 55 0 8"     "skuterud94 1993-11-25 mean of 9, 12, 15 3 67 0 8"    
# "skuterud94 1993-11-25 mean of 9, 12, 15 3 98 0 8"     "skuterud94 1993-11-25 mean of 9, 12, 15 6 111 0 8"   
#"skuterud94 1993-11-25 mean of 9, 12, 15 6 39 0 8"     "skuterud94 1993-11-25 mean of 9, 12, 15 6 55 0 8"    
#"skuterud94 1993-11-25 mean of 9, 12, 15 6 67 0 8"     "skuterud94 1993-11-25 mean of 9, 12, 15 6 98 0 8"