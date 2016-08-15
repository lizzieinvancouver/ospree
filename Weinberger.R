## Started 7 July 2016 ##
## By Cat and Lizzie##

## An R script to organize the studies by experiment type for the bud burst data. ##
## Studies were organized by number of field sampling dates, photoperiods, ##
## forcing temperatures, and experimental chilling hours. ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

dostan = TRUE

# Install Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("ggplot2", "dplyr","tidyr","rstan","shinystan")
ipak(packages)

setwd("~/Documents/git/ospree/input")
source('stan/savestan.R')
# get latest .Rdata file

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

ospree <- read.csv("ospree_clean_respvar.csv", header=TRUE)

## Variation in Field Sample Dates
woody<- ospree %>%
  select(datasetID,study,genus,species,woody,ID_fieldsample.date,fieldsample.date,
         Exp_Chilling_Hours,photoperiod_day,photoperiod_night,
         Total_Chilling_Hours,respvar.simple,response.time,forcetemp) %>%
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
  select(fieldsample.date,datasetID,study,genus.species, respvar.simple)%>%
  group_by(fieldsample.date,datasetID)%>%
  filter(!is.na(fieldsample.date))%>%
  filter(row_number()==1)

none.field<-woody%>% ## studies without field sampling dates
  select(datasetID,study, genus.species,fieldsample.date, respvar.simple)%>%
  filter(is.na(fieldsample.date))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)%>%
  rename("samplingdates.count"=fieldsample.date)

samplingdates<-as.data.frame(table(field$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("samplingdates.count"=Freq)

fieldsample<-full_join(samplingdates,none.field,by="datasetID")%>% ## table of number of field sample dates per study
  select(datasetID,samplingdates.count.x)%>%
  arrange(datasetID)%>%
  rename("samplingdates.count"=samplingdates.count.x)

weinberger<-full_join(woody,fieldsample,by="datasetID")%>%
  arrange(datasetID) %>%
  filter(row_number()==1)

## Variation in Photoperiod
photo<-woody %>% 
  select(photoperiod_day,datasetID,study,genus.species, respvar.simple)%>%
  group_by(photoperiod_day,datasetID)%>%
  filter(!is.na(photoperiod_day))%>%
  filter(row_number()==1)

none.photo<-woody%>%
  select(datasetID,study,genus.species,photoperiod_day, respvar.simple)%>%
  filter(is.na(photoperiod_day))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

photoperiods<-as.data.frame(table(photo$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("photoperiods.count"=Freq)

photo.day<-full_join(photoperiods,none.photo,by="datasetID")%>%
  select(datasetID,photoperiods.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,photo.day,by="datasetID")%>%
  arrange(datasetID)%>%
  filter(row_number()==1)
  
## Variation in Forcing
forcing<-woody %>% 
  select(forcetemp,datasetID,study,genus.species, respvar.simple)%>%
  group_by(forcetemp,datasetID)%>%
  filter(!is.na(forcetemp))%>%
  filter(row_number()==1)

none.force<-woody%>%
  select(datasetID,study,genus.species,forcetemp, respvar.simple)%>%
  filter(is.na(forcetemp))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

forcingtemps<-as.data.frame(table(forcing$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("forcetemps.count"=Freq)

forcetemp.day<-full_join(forcingtemps,none.force,by="datasetID")%>%
  select(datasetID,forcetemps.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,forcetemp.day,by="datasetID")%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

## Variation in Experimental Chilling
expchill<-woody %>% 
  select(Exp_Chilling_Hours,datasetID,study,genus.species, respvar.simple)%>%
  group_by(Exp_Chilling_Hours,datasetID)%>%
  filter(!is.na(Exp_Chilling_Hours))%>%
  filter(row_number()==1)

none.expchill<-woody%>%
  select(datasetID,study,genus.species,Exp_Chilling_Hours, respvar.simple)%>%
  filter(is.na(Exp_Chilling_Hours))%>%
  group_by(datasetID)%>%
  arrange(datasetID)%>%
  filter(row_number()==1)

expchilling<-as.data.frame(table(expchill$datasetID)) %>%
  rename("datasetID"=Var1)%>%
  rename("expchill.count"=Freq)

expchill.hours<-full_join(expchilling,none.chill,by="datasetID")%>%
  select(datasetID,expchill.count)%>%
  arrange(datasetID)

weinberger<-full_join(weinberger,expchill.hours,by="datasetID")%>%
  arrange(datasetID) %>%
  filter(row_number()==1)

write.table(weinberger, file="studytype.csv", sep=",", col.names=NA, qmethod="double")