#Code to convert Phenofit fitness values to presence absence (0,1)
# By Ailene Ettinger
# February 5, 2018

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")


projs<-c("1981_2000","A1Fi_2081_2100","B2_2081_2100")

getpresabs <- function(species) {#
  for (i in 1:length(projs)){
    path=paste("misc/dataPhenofit/",species,"/",projs[i],sep="")
    file <- file.path(path, "FitnessMean.txt")
    fitness<-read.table(file, header=TRUE)
    fitness$pres<-0
    fitness$pres[fitness$MeanFitness>0.1]<-1#Presence if fitness is >0.1, as recommended by Isabelle 
    name<-paste("output/projections/",species,"_",projs[i],".csv",sep="")
    write.csv(fitness,name, row.names=FALSE)#or txt file?
  }
}  
#For now, interested in the following species: "Fagus_sylvatica","Quercus_robur"
getpresabs("Fagus_sylvatica")
getpresabs("Quercus_robur")
