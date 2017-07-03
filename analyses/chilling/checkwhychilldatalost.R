#Ailene's Task: Look at how many studies without chilling data:
#-are seeds;
#-are more recent data (too recent to be in livenh or eos);
#-are location based (i.e. frmo outside Europe or N. America); 
#-have field sample date missing; or other reasons for losing data. 
#-Find out how many rows and how many studies are lost.
#Started July 1, 2017

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")

# Load libraries

# Get the data
d <- read.csv("output/ospree_clean_withchill.csv")

# Check dimensions: 12744    79
dim(d)
length(unique(d$datasetID))#85 studies/papers
# how many rows lack chilling data
length(which(is.na(d$Total_Chilling_Hours)))#5404 NAs
length(which(is.na(d$Total_Utah_Model)))#5404 NAs
length(which(is.na(d$Total_Chill_portions)))#5404 NAs
length(which(is.na(d$Field_Chilling_Hours)))#6509 NAs
length(which(is.na(d$Field_Utah_Model)))#6509 NAs
length(which(is.na(d$Field_Chill_portions)))#6509 NAs
length(which(is.na(d$Exp_Chilling_Hours)))#9632 NAs
length(which(is.na(d$Exp_Utah_Model)))#9632 NAs
length(which(is.na(d$Field_Chill_portions)))#9632 NAs
#Make a new column for yes/no chilldat
d$chilldat<-1
d[which(is.na(d$Field_Chilling_Hours)),]$chilldat<-0
d[which(!is.na(d$Field_Chilling_Hours)),]$chilldat<-1

# We are most interested in those studies that have field colletion dates but no chilling data
dfs<-d[-which(d$fieldsample.date==""),]
dim(dfs)#8059 rows have field sample date
length(unique(dfs$datasetID))#61 studies/papers have field sample date
#-how many studies have have field sample date missing?
d_nfs<-d[which(d$fieldsample.date==""),]#4685 rows have no field sample date
dim(d_nfs)#4685 rows do not have field sample data
length(unique(d_nfs$datasetID))#28 studies/papers contain rows with no field sample date
#Now, looking at just those studies that have a field sample date:
# how many rows lack field chilling data
length(which(is.na(dfs$Field_Chilling_Hours)))#1824 NAs
length(unique(dfs[which(is.na(dfs$Field_Chilling_Hours)),]$datasetID))#17 studies/papers contain rows with no chilling data
#Now, let's figure out why these studies with field sample date do not have chilling data
#Of the studies with field sample date:
#-are location based (i.e. frmo outside Europe or N. America); #How many are not in North America or Europe (what are the continents of these studies?):
tapply(dfs$fieldsample.date,list(dfs$continent,dfs$chilldat),length)#
#88 rows of data that are in "asia" and we have chilling data for? this is weird...
174+190+29+128#521 rows of data are not in north america or europe; 1173 rows in europe WITHOUT chilling data and 130 in north america without chilling data
#how many are seedlings?
unique(dfs$material)
#the material column needs cleaning (e.g. "seedling" and "seedlings; cutting and cuttings)
tapply(dfs$fieldsample.date,list(dfs$material,dfs$chilldat),length)#
#the biggest chunk of material with no chilling data is from cuttings (1343 rows)
#-are more recent data (too recent to be in livenh or eos);
head(dfs[which(dfs$year>2012),])#no rows of data are later than 2014, so i don't think we're missing any data because of it being too recent
head(dfs[which(dfs$datasetID=="heide15"),])#has field sample dates in 2014 (same as zohner)
head(dfs[which(dfs$datasetID=="zohner16"),])#
dim(dfs[which(dfs$datasetID=="zohner16"),])#864 rows

#not sure why there are no climate data available for zohner...


##Now, looking at studies with NO chilling data, how many are in europe?
tapply(d_nfs$fieldsample.date,list(d_nfs$continent,d_nfs$chilldat),length)#
#4166 rows are in europe and 231 are in north america
tapply(d_nfs$fieldsample.date,list(d_nfs$material,d_nfs$chilldat),length)#
#many rows are seedings: 
192+2063+56+24+37#2372 rows are seedlings
#38 rows are trees
17+757+96#870 are cuttings
#
