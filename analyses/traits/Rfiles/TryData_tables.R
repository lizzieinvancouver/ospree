# Code written by DS ~April 2020 to summarize TRY database data for OSPREE trait analsyes 
#DL edits May 26, 2020

#Understanding Try data
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(dplyr)

#load in dataset
TryDataCleaned22012020 <- read.csv("input/TryDataCleaned.csv")

dataperspecies <- TryDataCleaned22012020 %>%
  group_by(SpeciesName) %>%
  summarise(no_rows = length(SpeciesName))


dataperspeciesperdataset<- TryDataCleaned22012020 %>%
  group_by(SpeciesName, Dataset) %>%
  summarise(no_rows = length(SpeciesName))

dataperspeciespertrait<- TryDataCleaned22012020 %>%
  group_by(SpeciesName, Traits) %>%
  summarise(no_rows = length(SpeciesName),no_datasets=length(Dataset)) 

dataperspeciesperdatasetpertrait<- TryDataCleaned22012020 %>%
  group_by(SpeciesName, Dataset, Traits) %>%
  summarise(no_rows = length(SpeciesName))

dat<-dataperspeciespertrait

unique(dat$SpeciesName)

require(stringr)
dat$SpeciesName[which(dat$SpeciesName == "Acer pensilvanicum")] <- "Acer pensylvanicum"
dat$SpeciesName[which(dat$SpeciesName == "Rhamnus catharticus")] <- "Rhamnus cathartica"
dat$SpeciesName[which(dat$SpeciesName == "Quercus velutinam.")] <- "Quercus velutina"
dat$SpeciesName[which(dat$SpeciesName == "Quercus rubra I")] <- "Quercus rubra"
dat$SpeciesName[which(dat$SpeciesName == "Quercus shumardii Buckley")] <- "Quercus shumardii"
dat$SpeciesName[which(dat$SpeciesName == "Quercus ilex ilex")] <- "Quercus ilex"
dat$SpeciesName[which(dat$SpeciesName == "Pseudotsuga menziesii (Mirb.) Franco")] <- "Pseudotsuga menziesii" 
dat$SpeciesName[which(dat$SpeciesName == "Pseudotsuga menziesii var. menziesii")] <- "Pseudotsuga menziesii" 
dat$SpeciesName[which(dat$SpeciesName == "Prunus pensylvanica f.")] <- "Prunus pensylvanica"
dat$SpeciesName[which(dat$SpeciesName == "Quercus bicolor Willd.")] <- "Quercus bicolor"
dat$SpeciesName[which(dat$SpeciesName == "Alnus glutinosa (L.) Gaertn.")] <- "Alnus glutinosa"
dat$SpeciesName[which(dat$SpeciesName == "Betula pendula Roth")] <- "Betula pendula"
dat$SpeciesName[which(dat$SpeciesName == "Alnus incana (L.) Moench")] <- "Alnus incana"
dat$SpeciesName[which(dat$SpeciesName == "Facus grandifolia")] <- "Fagus grandifolia"
dat$SpeciesName[which(dat$SpeciesName == "Fagus grandfolia")] <- "Fagus grandifolia"
dat$SpeciesName[which(dat$SpeciesName == "Photinia melanocarpa (Michx.) K.R. Robertson & Phipps")] <- "Photinia melanocarpa"
dat$SpeciesName[which(dat$SpeciesName == "Populus grandidentata Michx.")] <- "Populus grandidentata"
dat$SpeciesName[which(dat$SpeciesName == "Prunus pennsylvanica")] <- "Prunus pensylvanica"
dat$SpeciesName[which(dat$SpeciesName == "Picea abies (L.) Karst.")] <- "Picea abies"
dat$SpeciesName[which(dat$SpeciesName == "Picea abies/obovata")] <- "Picea abies"
dat$SpeciesName[which(dat$SpeciesName == "Picea abies.x.obovata")] <- "Picea abies"
dat$SpeciesName[which(dat$SpeciesName == "Picea glauca (Moench) Voss")] <- "Picea glauca"
dat$SpeciesName[which(dat$SpeciesName == "Pieris japonica (Thunb.) D. Don ex G. Don")] <- "Pieris japonica"
dat$SpeciesName[which(dat$SpeciesName == "Pinus nigra Arnold")] <- "Pinus nigra"
dat$SpeciesName<-str_replace(dat$SpeciesName," (L.)","") 
dat$SpeciesName<-str_replace(dat$SpeciesName," L.","") 
dat$SpeciesName<-str_replace(dat$SpeciesName," Jacq.","") 
dat$SpeciesName<-str_replace(dat$SpeciesName," Ehrh.","") 
dat$SpeciesName<-str_replace(dat$SpeciesName," Marsh.","") 
dat$SpeciesName<-str_replace(dat$SpeciesName," Hook.","") 
dat$SpeciesName<-str_replace(dat$SpeciesName," Maxim.","") 

 unique(dat$SpeciesName) #now have 79 species
 
 #write.csv(dat,file="trydatasummary.csv")
 
 #Exploring the data, is there really only one trait value for so many species?
 d<-TryDataCleaned22012020

 abies<-subset(d, SpeciesName=="Abies alba")
 names(abies)
unique(abies$Traits) 