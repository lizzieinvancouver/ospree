### Started 20 September 2016 ###
### By Lizzie ###

## Some quick code to look at data ##
## And some model stuff ##

## Updated on 3 February 20017 ##

## To do! ##
# Next up, extract lab group from my gephi figures and see if that helps ... 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstanarm)
library(ggplot2)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
  } else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

# get the data

# make sure this is the correct file (we're still cleaning as I write this!) 
bb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
labgroups <- read.csv("output/labgroups.csv", header=TRUE)

# merge in labgroup (we could do this elsewhere someday
bb.wlab <- merge(bb, labgroups, by="datasetID", all.x=TRUE)

# how much data?
dim(bb.wlab)

columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
    "photoperiod_day", "respvar", "respvar.simple", "response", "response.time", 
    "Total_Chilling_Hours","Total_Utah_Model", "Total_Chill_portions",
    "Exp_Chilling_Hours",  "Exp_Utah_Model","Exp_Chill_portions", "cat")
    
bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)

# make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)
bb.wlab.sm$chillhrs <- as.numeric(bb.wlab.sm$Total_Chilling_Hours)
bb.wlab.sm$chillpor <- as.numeric(bb.wlab.sm$Total_Chill_portions)
bb.wlab.sm$utah <- as.numeric(bb.wlab.sm$Total_Utah_Model)
bb.wlab.sm$expchillhrs <- as.numeric(bb.wlab.sm$Exp_Chilling_Hours)
bb.wlab.sm$expchillpor <- as.numeric(bb.wlab.sm$Exp_Chill_portions)
bb.wlab.sm$exputah <- as.numeric(bb.wlab.sm$Exp_Utah_Model)

# where do we lose data
dim(subset(bb.wlab.sm, is.na(force)==FALSE))
dim(subset(bb.wlab.sm, is.na(photo)==FALSE))
dim(subset(bb.wlab.sm, is.na(chillhrs)==FALSE))
dim(subset(bb.wlab.sm, is.na(chillhrs)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE))

## grab the data and set it up as though about to run Stan model on BB days
# see also: Ospree_Analysis.R
ospree <- read.csv("output/ospree_clean_withchill.csv", header=TRUE) # almost 12000 rows

bbvars <- c("daystobudburst", "daysto50percentbudburst", "daysto20%budburst",
    "daysto10percentbudburst","daysto50%budburst", "daystoleafout")

ospree.bb <- ospree[which(ospree$respvar %in% bbvars),] # down to ~2400 rows

dim(ospree)
dim(ospree.bb) # not so much data, 2392 rows if just do days to budburst

kmeans <- read.csv("refs/kmeans6.csv", header=TRUE, skip=1,
    col.names=c("datID", "labgroup"))

ospree.bb.lab <- merge(ospree.bb, kmeans, by.x="datasetID",
   by.y="datID", all.x=TRUE)

ospree.bb.lab$spp <- paste(ospree.bb.lab$genus, ospree.bb.lab$species, sep="")

ospree.bb.lab <- subset(ospree.bb.lab, is.na(spp)==FALSE)

# deal with response vs. responsetime (quick fix for now)
resp1 <- subset(ospree.bb.lab, response==1) # most of are data are like this
resp1.timeNA <- subset(resp1, is.na(response.time)==TRUE) # about 20 rows have this

ospree.bb.lab$responsedays <- ospree.bb.lab$response.time
ospree.bb.lab$responsedays[which(ospree.bb.lab$response>1 & ospree.bb.lab$response.time=="")] <-
    ospree.bb.lab$response[which(ospree.bb.lab$response>1 & ospree.bb.lab$response.time=="")]

# just to think on ...
ospree.bb.lab[which(ospree.bb.lab$response>1),28:31]


##
## plotting
ospr.plot <- ospree.bb.lab

ospr.plot$forcetemp <- as.numeric(ospr.plot$forcetemp)
ospr.plot$responsedays <- as.numeric(ospr.plot$responsedays) 

ggplot(ospr.plot,
     aes(x=forcetemp, y=responsedays, color=photoperiod_day)) +
     scale_x_discrete(name="Temperature") + scale_y_continuous(name="Days to BB") +
     facet_wrap(~spp, nrow=6) + 
     geom_point()

ggplot(ospr.plot,
    aes(x=Total_Chilling_Hours, y=responsedays, color=forcetemp)) + 
    scale_x_discrete(name="Chilling hours") + scale_y_continuous(name="Days to BB") +
    facet_wrap(~spp, nrow=6) + 
    geom_point()

ggplot(ospr.plot,
     aes(x=forcetemp, y=responsedays, color=spp)) +
     scale_x_discrete(name="Temperature") + scale_y_continuous(name="Days to BB") +
     facet_wrap(~labgroup, nrow=6) + 
     geom_point()

# deal with NAs for now ...
ospr.stan <- ospree.bb.lab

# Note: Missing data from photoperiod_day and force_temp generally are the same studies, mostly -- but studies missing totalchill are a whold other subset!
ospr.stan$totalchill <- as.numeric(ospr.stan$Total_Chilling_Hours) # 483 NA
ospr.stan$forcetemp <- as.numeric(ospr.stan$forcetemp) # 494 NA
ospr.stan$photoperiod_day <- as.numeric(ospr.stan$photoperiod_day) # 498 NA
ospr.stan$provenance.lat <- as.numeric(ospr.stan$provenance.lat)
ospr.stan$spp <- as.numeric(as.factor(ospr.stan$spp))
ospr.stan$responsedays <- as.numeric(ospr.stan$responsedays)

ospr.stan <- subset(ospr.stan, select=c("responsedays", "totalchill", "forcetemp", 
    "photoperiod_day", "provenance.lat", "spp"))





## quick diversion to get labgroup ....
# groupme <- as.numeric(as.factor(ospr.stan.noNA$labgroup))

## Simple run with no interactions, need to extract y_hat to look at lagroup?
load('~/Documents/git/projects/treegarden/budreview/ospree/Stan Output 2016-09-06 real.RData')
launch_shinystan(osp.r)

goo <- extract(osp.r)
dim(goo$b_force)

## Run with interactions (went really poorly!)
load('~/Documents/git/projects/treegarden/budreview/ospree/Stan Output 2016-09-20 real.RData')
launch_shinystan(osp.r)

