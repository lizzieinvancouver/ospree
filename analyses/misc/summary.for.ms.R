#Summaries of ospree database of main ospree paper
#started by Ailene on 6 MArch 2019

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

#Read in clean_ospree
d<-read.csv("output/ospree_clean.csv")
dim(d)
#number studies
unique(d$datasetID)
#number years
unique(d$datasetID)
d$genus.species<-paste(d$genus,d$species, sep=".")
unique(d$genus.species)

#Read in clean_ospree
d<-read.csv("output/ospree_clean.csv")
dim(d)
#number studies
unique(d$datasetID)
#number years
unique(d$year)
range(d$year, na.rm=TRUE)

d$genus.species<-paste(d$genus,d$species, sep=".")
unique(d$genus.species)

#Read in ospree_clean_withchill_BB
d2<-read.csv("output/ospree_clean_withchill_BB.csv")
dim(d2)
#number studies
unique(d2$datasetID)
#number years
unique(d2$year)
range(d2$year, na.rm=TRUE)

d2$genus.species<-paste(d2$genus,d2$species, sep=".")
unique(d2$genus.species)
