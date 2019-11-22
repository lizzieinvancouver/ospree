# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/OSPREE/TRY_data/")

library(data.table)
library(tidyverse)
library(dplyr)
library(readr)
library(reshape2)
library(plyr)


myfiles = list.files(path="Sep_data/", pattern="*.csv", full.names=TRUE)
myfiles
dat_clean = ldply(myfiles, read_csv)
dat_clean

Trydata <- dcast(dat_clean, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+OrigUnitStr+TraitName+UnitName+ErrorRisk+OrigValueStr~DataName, value.var = "StdValue", na.rm=TRUE)

HerbaceousplantsofRougeNationalUrbanPark <- read_csv("sep_data/HerbaceousplantsofRougeNationalUrbanPark.csv")
View(HerbaceousplantsofRougeNationalUrbanPark)


Winona_oak <- dcast(HerbaceousplantsofRougeNationalUrbanPark, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+TraitName+UnitName+ErrorRisk+StdValue~DataName, value.var = "OrigValueStr", na.rm=TRUE)



path <- names(Winona_oak[,-5])



#Gives all traits values for dataset
low<- aggregate(data=Trydata,ObservationID~LastName+FirstName+DatasetID+Dataset+SpeciesName+TraitName+UnitName+ErrorRisk+StdValue,FUN=paste)

dat2 <- HerbaceousplantsofRougeNationalUrbanPark %>% group_by(ObservationID) %>% summarise(val=paste(Values, collapse=","))

write_csv(Trydata,"TRYDATA.CSV")


copy.Trydata <- Trydata

for(i in 1:nrow(Trydata)){
  temp.which <- which(!is.na(as.vector(Trydata[i, ]))) 
  copy.Trydata[temp.which] <- Trydata[i, temp.which]
}
