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

data <-fread("7572.txt")
data<-data[,1:25]

#First identify how many unique datasets there are:
length(unique(data$Dataset))

for (name in unique(data$Dataset)){
  #Subset the data by dataset name
  tmp <- subset(data,Dataset==name)
  #Create a new filename for each data set - the folder 'Dataset_georef_longlat' should already exist
  name2=gsub('/','', name)
  fn=paste('sep_data/', gsub(' ','',name2),'.csv',sep='')
  #Save the CSV file containing only one data set
  print(fn)
  write.csv(tmp,fn, row.names=FALSE)
}


myfiles = list.files(path="Sep_data/", pattern="*.csv", full.names=TRUE)
dat_clean = ldply(myfiles, read_csv)

#Why is there a big difference between number of observation 
Trydata <- dcast(dat_clean, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+OrigUnitStr+UnitName+ErrorRisk+OrigValueStr~DataName, value.var = "StdValue", na.rm=TRUE)

Trydata2 <- dcast(dat_clean, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+OrigUnitStr+StdValue+UnitName+ErrorRisk~DataName, value.var = "OrigValueStr", na.rm=TRUE)

Trydata$label<-paste(Trydata$ObservationID,Trydata$Trait, sep=".")

folder<-"~/Documents/Ph.D/OSPREE/TRY_data/Sep_data/"
file_list <- list.files(path=folder, pattern="*.csv")

not_all_na <- function(x) any(!is.na(x))

for (i in 1:length(file_list)){
  out<-assign(file_list[i], 
              read.csv(paste(folder, 
                             file_list[i], 
                             sep='')))
  out1<-dcast(out, 
              LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+OrigUnitStr~DataName, 
              value.var = c("OrigValueStr"), 
              na.rm=TRUE);head(out1)
  out$DataName<-paste("std",out$DataName, sep="_")
  out2<-dcast(out, 
              LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+UnitName~DataName, 
              value.var = "StdValue", 
              na.rm=TRUE)
  out2%>% select_if(not_all_na)
  out3<-merge(out1,
              out2, 
              by =c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","ObsDataID"))
  assign(paste("dat",i,sep="_"), out3)
}


copy.Trydata <- Trydata

for(i in 1:nrow(Trydata)){
  temp.which <- which(!is.na(as.vector(Trydata[i, ]))) 
  copy.Trydata[temp.which] <- Trydata[i, temp.which]
}

write_csv(Trydata,"TRYDATA.CSV")
write_csv(copy.Trydata, "copyTRYDATA.CSV")