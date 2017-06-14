## Started 24 May 2017 ##
## By Lizzie (for now) ##

## Budburst analysis, take whatever ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(lme4)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) {    
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else 
setwd("~/Documents/git/ospree/analyses")

# get the data 
bb.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

columnstokeep <- c("datasetID", "study", "genus", "species", "varetc", "woody", "forcetemp", "material",
    "photoperiod_day", "respvar", "respvar.simple", "response", "response.time", "fieldsample.date",
    "Total_Chilling_Hours","Total_Utah_Model", "Total_Chill_portions",
    "Exp_Chilling_Hours",  "Exp_Utah_Model","Exp_Chill_portions")
    
bb <- subset(bb.all, select=columnstokeep)

# species
bb$latbi <- paste(bb$genus, bb$species)
sort(unique(bb$latbi))

# which studies don't have fieldsample.date (see notes in github issue #98)
nofs <- subset(bb, fieldsample.date==""| is.na(fieldsample.date)==TRUE)

# 97 total studies
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

# where do we lose data? It's still mainly because of chilling.
dim(subset(bb, is.na(force)==FALSE))
dim(subset(bb, is.na(photo)==FALSE))
dim(subset(bb, is.na(chillhrs)==FALSE))
dim(subset(bb, is.na(chillhrs)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE))

bb.sm <- subset(bb, is.na(chillhrs)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE)


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
