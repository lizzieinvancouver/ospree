#Code to convert Phenofit fitness values to presence absence (0,1)
# By Ailene Ettinger
# February 5, 2018

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#load libraries
library(geosphere)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")


projs<-c("1981_2000","A1Fi_2081_2100","B2_2081_2100")

get_dayl <- function(species) {#
  for (i in 1:length(projs)){
    path=paste("misc/dataPhenofit/",species,"/",projs[i],sep="")
    file <- file.path(path, "DateLeafMean.txt")
    lo<-read.table(file, header=TRUE)
     name<-paste("output/",species,"_",projs[i],"_daylength.csv",sep="")
    write.csv(fitness,name, row.names=FALSE)#or txt file?
  }
}  
#For now, interested in the following species: "Fagus_sylvatica","Quercus_robur"
getpresabs("Fagus_sylvatica")
getpresabs("Quercus_robur")
