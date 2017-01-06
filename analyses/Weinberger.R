## Started 7 July 2016 ##
## By Cat and Lizzie##

## An R script to organize the studies by experiment type for the bud burst data. ##
## Studies were organized by number of field sampling dates, photoperiods, ##
## forcing temperatures, and experimental chilling hours. ##

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
ospree <- read.csv("ospree_clean_respvar.csv", header=TRUE)

## Variation in Field Sample Dates
woody<- ospree %>%
  dplyr::select(datasetID,study,genus,species,woody,ID_fieldsample.date,fieldsample.date,
         Exp_Chilling_Hours,photoperiod_day,photoperiod_night,
         Total_Chilling_Hours,respvar.simple,response.time,forcetemp, provenance.lat, provenance.long) %>%
  filter(woody=="yes")%>%
  unite(genus.species, genus, species, sep=".") 
woody <- woody %>%
  group_by(datasetID, study, genus.species, respvar.simple) %>%
  mutate(fieldsample.date = replace(fieldsample.date,
                                    fieldsample.date<0, NA))%>%
  mutate(photoperiod_day = replace(photoperiod_day,
                                    photoperiod_day<0, NA))%>%
  mutate(forcetemp = replace(forcetemp,forcetemp<0, NA))

respvar<-as.data.frame(table(woody$respvar.simple))

field<-woody %>% ## studies with field sampling dates
  dplyr::select(fieldsample.date,datasetID,study,genus.species, respvar.simple)%>%
  group_by(fieldsample.date,datasetID)%>%
  filter(!is.na(fieldsample.date))%>%
  filter(row_number()==1)

none.field<-woody%>% ## studies without field sampling dates
  dplyr::select(datasetID,study, genus.species,fieldsample.date, respvar.simple)%>%
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
  dplyr::select(genus.species,datasetID,study, respvar.simple)%>%
  group_by(genus.species,datasetID)%>%
  filter(!is.na(genus.species))%>%
  filter(row_number()==1)

none.species<-woody%>%
  dplyr::select(datasetID,study,genus.species,respvar.simple)%>%
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
  dplyr::select(photoperiod_day,datasetID,study,genus.species, respvar.simple)%>%
  group_by(photoperiod_day,datasetID)%>%
  filter(!is.na(photoperiod_day))%>%
  filter(row_number()==1)

none.photo<-woody%>%
  dplyr::select(datasetID,study,genus.species,photoperiod_day, respvar.simple)%>%
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
  dplyr::select(forcetemp,datasetID,study,genus.species, respvar.simple)%>%
  group_by(forcetemp,datasetID)%>%
  filter(!is.na(forcetemp))%>%
  filter(row_number()==1)

none.force<-woody%>%
  dplyr::select(datasetID,study,genus.species,forcetemp, respvar.simple)%>%
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

## Variation in Experimental Chilling
expchill<-woody %>% 
  dplyr::select(Exp_Chilling_Hours,datasetID,study,genus.species, respvar.simple)%>%
  group_by(Exp_Chilling_Hours,datasetID)%>%
  filter(!is.na(Exp_Chilling_Hours))%>%
  filter(row_number()==1)

none.expchill<-woody%>%
  dplyr::select(datasetID,study,genus.species,Exp_Chilling_Hours, respvar.simple)%>%
  filter(is.na(Exp_Chilling_Hours))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

expchilling<-as.data.frame(table(expchill$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("expchill.count"=Freq)

expchill.hours<-full_join(expchilling,none.expchill,by="datasetID")%>%
  dplyr::select(datasetID,expchill.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,expchill.hours,by="datasetID")%>%
  arrange(datasetID) %>%
  filter(row_number()==1)

## Variation in Provenance Latitude
provlat<-woody %>% 
  dplyr::select(provenance.lat,datasetID,study,genus.species, respvar.simple)%>%
  group_by(provenance.lat,datasetID)%>%
  filter(!is.na(provenance.lat))%>%
  filter(row_number()==1)

none.provlat<-woody%>%
  dplyr::select(datasetID,study,genus.species,provenance.lat, respvar.simple)%>%
  filter(is.na(provenance.lat))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

provenancelat<-as.data.frame(table(provlat$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("latitude.count"=Freq)

latitude<-full_join(provenancelat,none.provlat,by="datasetID")%>%
  dplyr::select(datasetID,latitude.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,latitude,by="datasetID")%>%
  arrange(datasetID) %>%
  filter(row_number()==1)

## Variation in Provenance Longitude
provlong<-woody %>% 
  dplyr::select(provenance.long,datasetID,study,genus.species, respvar.simple)%>%
  group_by(provenance.long,datasetID)%>%
  filter(!is.na(provenance.long))%>%
  filter(row_number()==1)

none.provlong<-woody%>%
  dplyr::select(datasetID,study,genus.species,provenance.long, respvar.simple)%>%
  filter(is.na(provenance.long))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

provenancelong<-as.data.frame(table(provlong$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("longitude.count"=Freq)

longitude<-full_join(provenancelong,none.provlong,by="datasetID")%>%
  dplyr::select(datasetID,longitude.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,longitude,by="datasetID")%>%
  arrange(datasetID) %>%
  filter(row_number()==1)

write.csv(weinberger, file="~/Documents/git/ospree/analyses/output/studytype.csv",
          row.names=FALSE)

# Create a more concise table
studytype<- weinberger %>%
  group_by(study, genus.species, respvar.simple, datasetID)%>%
  ungroup()%>%
  dplyr::select(datasetID, samplingdates.count, species.count, photoperiods.count, forcetemps.count, 
                expchill.count, latitude.count, longitude.count) %>%
  group_by(datasetID) %>%
  arrange(datasetID) %>%
  filter(row_number()==1) 
studytype[is.na(studytype)] <- 0

write.csv(studytype, file="~/Documents/git/ospree/analyses/output/studytype.table.csv",
          row.names = FALSE)
