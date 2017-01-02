## Using Dan's Zohner Data to integrate into Study Type Table ##
## Started January 2 2017 - Cat ##
## Using same script as Weinberger ##

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("~/Documents/git/ospree/analyses/input")
ospree <- read.csv("zohner_formated.csv", header=TRUE, fileEncoding="latin1")

## Variation in Field Sample Dates
woody<- ospree %>%
  dplyr::select(datasetID,study,genus,species,woody,fieldsample.date,
                photoperiod_day,photoperiod_night,
                response.time,forcetemp) %>%
  filter(woody=="yes")%>%
  unite(genus.species, genus, species, sep=".") 
woody <- woody %>%
  group_by(datasetID, study, genus.species) %>%
  mutate(fieldsample.date = replace(fieldsample.date,
                                    fieldsample.date<0, NA))%>%
  mutate(photoperiod_day = replace(photoperiod_day,
                                   photoperiod_day<0, NA))%>%
  mutate(forcetemp = replace(forcetemp,forcetemp<0, NA))

field<-woody %>% ## studies with field sampling dates
  dplyr::select(fieldsample.date,datasetID,study,genus.species)%>%
  group_by(fieldsample.date,datasetID)%>%
  filter(!is.na(fieldsample.date))%>%
  filter(row_number()==1)

none.field<-woody%>% ## studies without field sampling dates
  dplyr::select(datasetID,study, genus.species,fieldsample.date)%>%
  filter(is.na(fieldsample.date))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)%>%
  rename("samplingdates.count"=fieldsample.date)

samplingdates<-as.data.frame(table(field$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("samplingdates.count"=Freq)

fieldsample<-full_join(samplingdates,none.field,by="datasetID")%>% ## table of number of field sample dates per study
  dplyr::select(datasetID,samplingdates.count.x)%>%
  arrange(datasetID)%>%
  rename("samplingdates.count"=samplingdates.count.x)

weinberger<-full_join(woody,fieldsample,by="datasetID")%>%
  arrange(datasetID) %>%
  filter(row_number()==1)

## Number of Species
species<-woody %>% 
  dplyr::select(genus.species,datasetID,study)%>%
  group_by(genus.species,datasetID)%>%
  filter(!is.na(genus.species))%>%
  filter(row_number()==1)

none.species<-woody%>%
  dplyr::select(datasetID,study,genus.species)%>%
  filter(is.na(genus.species))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

gen.spp<-as.data.frame(table(species$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("species.count"=Freq)

genus.species<-full_join(gen.spp,none.species,by="datasetID")%>%
  dplyr::select(datasetID,species.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,genus.species,by="datasetID")%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

## Variation in Photoperiod
photo<-woody %>% 
  dplyr::select(photoperiod_day,datasetID,study,genus.species)%>%
  group_by(photoperiod_day,datasetID)%>%
  filter(!is.na(photoperiod_day))%>%
  filter(row_number()==1)

none.photo<-woody%>%
  dplyr::select(datasetID,study,genus.species,photoperiod_day)%>%
  filter(is.na(photoperiod_day))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

photoperiods<-as.data.frame(table(photo$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("photoperiods.count"=Freq)

photo.day<-full_join(photoperiods,none.photo,by="datasetID")%>%
  dplyr::select(datasetID,photoperiods.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,photo.day,by="datasetID")%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

## Variation in Forcing
forcing<-woody %>% 
  dplyr::select(forcetemp,datasetID,study,genus.species)%>%
  group_by(forcetemp,datasetID)%>%
  filter(!is.na(forcetemp))%>%
  filter(row_number()==1)

none.force<-woody%>%
  dplyr::select(datasetID,study,genus.species,forcetemp)%>%
  filter(is.na(forcetemp))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

forcingtemps<-as.data.frame(table(forcing$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("forcetemps.count"=Freq)

forcetemp.day<-full_join(forcingtemps,none.force,by="datasetID")%>%
  dplyr::select(datasetID,forcetemps.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,forcetemp.day,by="datasetID")%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

write.csv(weinberger, file="~/Documents/git/ospree/analyses/output/studytype.csv",
          row.names=FALSE)

# Create a more concise table
studytype<- weinberger %>%
  group_by(study, genus.species, datasetID)%>%
  ungroup()%>%
  dplyr::select(datasetID, samplingdates.count, species.count, photoperiods.count, forcetemps.count) %>%
  group_by(datasetID) %>%
  arrange(datasetID) %>%
  filter(row_number()==1) 
studytype[is.na(studytype)] <- 0

write.csv(studytype, file="~/Documents/git/ospree/analyses/output/studytype.table.csv")
