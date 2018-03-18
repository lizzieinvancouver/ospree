## Started 24 May 2017 ##
## By Lizzie (for now) ##

## Budburst analysis, take whatever ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(lme4)
library(tidyverse)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) {    
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 
setwd("~/Documents/git/ospree/analyses")

# get the data 
bb.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
# bb.all <- read.csv("output/ospree_clean_withchill_BB_2017Jun22.csv", header=TRUE)

bb.some <- subset(bb.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(bb.some, response.time!="")

columnstokeep <- c("datasetID", "study", "genus", "species", "varetc", "woody", "forcetemp", "material",
    "photoperiod_day", "respvar", "respvar.simple", "response", "response.time", "fieldsample.date",
    "Total_Chilling_Hours","Total_Utah_Model", "Total_Chill_portions",
    "Exp_Chilling_Hours",  "Exp_Utah_Model","Exp_Chill_portions","chilldays","field.chill.units","figure.table..if.applicable.")
    
bb <- subset(bbdat, select=columnstokeep)

# species
bb$latbi <- paste(bb$genus, bb$species)
sort(unique(bb$latbi))

# which studies don't have fieldsample.date (see notes in github issue #98)
nofs <- subset(bb, fieldsample.date==""| is.na(fieldsample.date)==TRUE)

# 97 total studies (as of July 2017: 133!)
unique(paste(bb$datasetID, bb$study))

# make a bunch of things numeric (eek!)
bb$force <- as.numeric(bb$forcetemp)
bb$photo <- as.numeric(bb$photoperiod_day)
bb$resp <- as.numeric(bb$response.time)
bb$chillhrs <- as.numeric(bb$Total_Chilling_Hours)
bb$chillpor <- as.numeric(bb$Total_Chill_portions)
bb$utah <- as.numeric(bb$Total_Utah_Model)
bb$expchillhrs <- as.numeric(bb$Exp_Chilling_Hours)
bb$expchillpor <- as.numeric(bb$Exp_Chill_portions)
bb$exputah <- as.numeric(bb$Exp_Utah_Model)

###Dan is adding code her on 11 Nov 17 to look more at lost chilling
#in the context of thinking about imputation
##IS there a difference depending on which chilling calculation we use?

notchillhrs<-filter(bb,is.na(chillhrs))
dim(notchillhrs)
notchillpor<-filter(bb,is.na(chillpor))
dim(notchillpor)
notchillutah<-filter(bb,is.na(utah))
dim(notchillutah) ##as of 2/26/18 we are missing 434...down from 840
###utah is most complete.
unique(notchillutah$datasetID) 
supernotchill<-dplyr::select(notchillutah, datasetID,study,chillpor,chillhrs,utah,field.chill.units,chilldays)
supernotchill<-supernotchill[!duplicated(supernotchill),]
#write.csv(supernotchill,"updated_chillmeout_check.csv",row.names = FALSE) ###this makes a file of our missing chill
###okay, Dan out, back to the regularly schedule code from Lizzie

# where do we lose data? It's still mainly because of chilling.
dim(subset(bb, is.na(force)==FALSE)) # was: 3077; now: 7893
dim(subset(bb, is.na(photo)==FALSE)) # was: 2921; now: 8545
dim(subset(bb, is.na(chillhrs)==FALSE)) # was: 2142; now: 6290
dim(subset(bb, is.na(chillhrs)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE)) # was: 1577, now: 4552

bb.sm <- subset(bb, is.na(chillhrs)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE)

length(unique(bb.sm$datasetID)) # was 33, now 48!

## Let's talk about species ...

# At this point we have 19 species but how many appear in more than one study?
bb.countspp.wstudy.step1 <- aggregate(bb.sm["forcetemp"], bb.sm[c("datasetID","study", "genus", "species", "latbi")],
    FUN=length)
bb.countspp.wstudy <- aggregate(bb.countspp.wstudy.step1["forcetemp"], bb.countspp.wstudy.step1[c("genus", "species", "latbi")],
    FUN=length)
names(bb.countspp.wstudy)[names(bb.countspp.wstudy)=="forcetemp"] <- "num.studies"
subset(bb.countspp.wstudy, num.studies>1) 

# but only 10 spp if you remove study above, check it out
bb.countspp.step1 <- aggregate(bb.sm["forcetemp"], bb.sm[c("datasetID", "genus", "species", "latbi")],
    FUN=length)
bb.countspp <- aggregate(bb.countspp.step1["forcetemp"], bb.countspp.step1[c("genus", "species", "latbi")],
    FUN=length)
names(bb.countspp)[names(bb.countspp)=="forcetemp"] <- "num.studies"
subset(bb.countspp, num.studies>1)

# okay, let's try to subset chilling
hist(bb.sm$chillhrs, breaks=100)
bb.sm$chillcat <- NA
bb.sm$chillcat[bb.sm$chillhrs<100] <- 1
bb.sm$chillcat[bb.sm$chillhrs>99 & bb.sm$chillhrs>1000] <- 2
bb.sm$chillcat[bb.sm$chillhrs>999 & bb.sm$chillhrs>2000] <- 3
bb.sm$chillcat[bb.sm$chillhrs>1999] <- 4

ggplot(bb.sm,
    aes(x=force, y=response.time, color=photo, shape=as.factor(chillcat))) +
    scale_x_discrete(name="Temperature") + scale_y_continuous(name="Days to BB") +
    facet_wrap(~genus, nrow=6) + 
    geom_point()

ggplot(bb.sm,
    aes(x=photo, y=response.time, color=datasetID)) +
    scale_x_discrete(name="Temperature") + scale_y_continuous(name="Days to BB") +
    facet_wrap(~latbi, nrow=6) + 
    geom_point()
