# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/OSPREE/TRY_data/")
options(fileEncoding="latin1")

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

Encoding(url) <- 'UTF-8'

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
                             sep=''),fileEncoding = "latin1"))
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

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[1]
#GrowthandHerbivoryofJuvenilTrees

names(dat_1)

dat_1<-dat_1[,c(1:9,14,17:24,28,31,38,47,52)]

ag<-melt(dat_1,
         measure.vars=c("std_Leaf area: in case of compound leaves undefined if leaf or leaflet; undefined if petiole and rhachis in- or excluded",
                        "std_Plant height observed","std_SLA: undefined if petiole in- or excluded"),
         variable.name = "Trait",
         value.name = "value")

ag$label<-paste(ag$ObservationID,ag$Trait, sep=".")

ids <- unique(ag$label)

#take
stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(ag, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}
head(stor)
unique(stor$value)

write.csv(stor,"GrowthandHerbivoryofJuvenilTrees_Cleaned.csv", row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[2]
#HerbaceousplantsofRougeNationalUrbanPark

names(dat_2)

dat_2<-dat_2[,c(1:10,21,24,27:30,34:35,38,39)]

ag<-melt(dat_2,
         measure.vars=c("std_Leaf area: in case of compound leaves leaf; petiole and rhachis included",
                        "std_Leaf carbon content per dry mass","std_Leaf dry matter content per leaf water-saturated mass (LDMC)",
                        "std_Leaf nitrogen content per dry mass (Nmass)", "std_Number of leaves per plant",
                        "std_Plant height reproductive / generative", "std_SLA: petiole  included",
                        "std_Stem diameter"),
         variable.name = "Trait",
         value.name = "value")

ag$label<-paste(ag$ObservationID,ag$Trait, sep=".")

ids <- unique(ag$label)


stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(ag, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}
head(stor)
unique(stor$value)

#drops all Nas
stor<-stor %>% drop_na()

#removes all text after 
stor$Latitude <- gsub("N.*","",stor$Latitude)

write.csv(stor,"HerbaceousplantsofRougeNationalUrbanPark_Cleaned.csv", row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[3]
#ItalianAlpsPlantTraitsDatabase

names(dat_3)

dat_3<-dat_3[,c(1:9,11,13)]

dat_3 <- dat_3[,c(-9)]

dat_3 <- dat_3[,c(-8)]

ag<-melt(dat_3,
         measure.vars=c("std_Plant height (unspecified if vegetative or reproductive)"),
         variable.name = "Trait",
         value.name = "value")

ag$label<-paste(ag$ObservationID,ag$Trait, sep=".")

ids <- unique(ag$label)


stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(ag, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}
head(stor)
unique(stor$value)

#adds m to all values in the UnitName Column
stor$UnitName <- paste0(stor$UnitName, "m")

write.csv(stor,"ItalianAlpsPlantTraitsDatabase.csv", row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[4]
#JasperRidgeCalifornianWoodyPlantsDatabase

names(dat_4)

dat_4<-dat_4[,c(1:10,14,19,22:24,27:29)]

ag<-melt(dat_4,
         measure.vars=c("std_Leaf area: in case of compound leaves leaflet; petiole excluded",
                        "std_Leaf nitrogen content per area (Narea)",
                        "std_Leaf nitrogen content per dry mass (Nmass)", "std_Plant height vegetative",
                        "std_SLA: petiole  excluded",
                        "std_Wood density; stem specific density; wood specific gravity"),
                        variable.name = "Trait",
                        value.name = "value")

ag$label<-paste(ag$ObservationID,ag$Trait, sep=".")

ids <- unique(ag$label)


stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(ag, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}

write_csv(Trydata,"TRYDATA.CSV")
write_csv(copy.Trydata, "copyTRYDATA.CSV")