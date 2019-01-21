## Started 20 January 2919 ##
## By Lizzie ##

# Trying to run data from 6+ months ago given our current code #
# Why? I wanted to understand why the answers seemed to change so much (to me) #
# way back in late-summer/early-fall 2018

# Some notes on this issue: https://github.com/lizzieinvancouver/ospree/issues/219
# But back when I worked on issue #219 I don't think I ever ran current models & methods on the old data.

# So, the below code is pulled from models_stan.R and includes the written out form of ... #
# bbdataleadin.R and bbdataplease.R #

# You can't just use the old data on the current code because ...
# We added photo, force, chill type and call those columns in our current code (they don't exist yet in the old data)
# I think that is the main issue, other than needing to call a different file
# I named the datafiles based on their dates on github.

## So, what did I find?
# Well, I compared our old numbers in models_bb_compare.xlsx in the tab '2018Jun-Aug_LizzieAilene'
# Note that back then we used UTAH units and a different version of speciescomplex, and had some small differences in photoperiod estimates etc.
# In the old xlsx: m2lni (uncentered), INT was 71.1
# force: -1.1 (-1.9 when we included intxns)
# photo: -0.6 (-1.3 when we included intxns)
# chill: -2.9 (-6.9 when we included intxns)

# And when running m2lni (uncentered) with utah chill now ...
# INT was 71.3
# force: -1.3 (-1.1 when we included intxns)
# photo: -0.3 (+1 when we included intxns)
# chill: -2.5 (-5.2 when we included intxns)
## Umm, so that is not so crazy different ... until you add the interactions, which means really we changed lots of little things, but nothing crazy really happened, except that the interaction model, which was always a little unstable went back to being unstable.
# And when running m2lni (uncentered) with chill portions now ...
# INT was 78.6
# force: -1.4
# photo: -0.25
# chill: -0.25 (really)
## And, chill ports vs. utah matters lot here. 

# Remember when you leave out interactions you average over them in your main effects (Gelman and Hill), so the fact that our answers for the no-interaction model did not change too dramatically makes me feel better. 


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R

use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = FALSE # change to false to use raw predictors

# Default is species complex and no crops
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE

# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE

#Default is all chilling data
use.expchillonly = FALSE # change to true for only experimental chilling 
#note: with only exp chilling, there is only exp photo and force too.
#also: subsetting to exp chill only reduces dataset to 3 species, <9 studies

## bbstanleadin START ##

# libraries needed for the leadin code
library(plyr)
library(dplyr)
library(rstan)

source('..//stan/savestan.R') # Dan Flynn code
# incl. f(x)s to deal with biases in the data related to species, study, and design....
source("source/speciescomplex.R") # this makes sure all species/complexes present in 2 or more studies
source("source/speciescomplex.multcues.R") # as above, but requires all species/complexes to have more than one cue manipulated
source("source/speciescomplex.nocrops.R") # similar to speciescomplex.R but removes 4 crop species
source("source/stan_utility.R") # From Mike Betancourt

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################################################
# Running the models with fake data? See bb_testdata_analysis.R #
################################################################# 

## First steps to cleaning: Get the data, subset down to exact data columns etc. that we want 
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!

## (1) Get the data and slim down to correct response and no NAs ...
## bbdataplease START ##

##
## Started 30 July 2017 ##
## Lizzie took this from bbmodels_stan.R ##

## Source file for reading in the data and
## doing some cleaning we should ALL do

## But! Keep an eye on this file! We probably need to edit it
checkdataforNAs <- FALSE # Set to TRUE for looking at missing data rows

#source("source/speciescomplex.R")
d<-read.csv("modelnotes/check2018data/ospree_clean_withchill_BB_2Jul2018.csv", header=TRUE)
# d<-read.delim("modelnotes/check2018data/ospree_clean_withchill_BB_8Jun2018.csv", header=TRUE)


## just the bb data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),]
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)
bb.resp$forceday <- as.numeric(bb.resp$forcetemp)
bb.resp$forcenight <- as.numeric(bb.resp$forcetemp_night)
bb.resp$photonight <- as.numeric(bb.resp$photoperiod_night)

bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$force <- bb.resp$forceday
bb.resp$force[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] <-
    (bb.resp$forceday[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photo[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] +
    bb.resp$forcenight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photonight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE])/24

bb.resp$chill <- as.numeric(bb.resp$Total_Utah_Model) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions
bb.resp$chill.hrs <- as.numeric(bb.resp$Total_Chilling_Hours) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions
bb.resp$chill.ports <- as.numeric(bb.resp$Total_Chill_portions) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions

bb.resp$resp <- as.numeric(bb.resp$response.time)

bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(chill)==FALSE & is.na(resp)==FALSE)
if(use.chillports) bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(chill.ports)==FALSE & is.na(resp)==FALSE)

# bb.noNA<-subset(bb.noNA, field.sample<=1)

# Vector needed to identify weinberger-design studies
if(FALSE){
weinberg<-c("falusi03", "falusi97", "heide93", "jones12", "partanen05", "ramos99",
            "ashby62","basler14","biasi12","boyer","calme94","charrier11","cook00b",
             "ganset02"  ,"gianfagna85","guerriero90","gunderson12")
}
       
if(checkdataforNAs){
forceissues <- subset(bb.resp, is.na(force)==TRUE)
table(forceissues$datasetID)
photoissues <- subset(bb.resp, is.na(photo)==TRUE)
table(photoissues$datasetID)
chillissues <- subset(bb.resp, is.na(chill)==TRUE)
table(chillissues$datasetID)
respissues <- subset(bb.resp, is.na(resp)==TRUE)
table(respissues$datasetID)
    }

# See imputechilling.R for notes (and issue # 147)

if(FALSE){
## what is lost due to NAs?
forceNA <- bb.resp[which(is.na(bb.resp$force)==TRUE),]
forceNA$forcetemp
subset(forceNA, forcetemp=="")  # what is up with the no entry ones? hawkins12 and gansert02
photoNA <- bb.resp[which(is.na(bb.resp$photo)==TRUE),]
photoNA$photoperiod_day # these datasetIDs are all mentioned as not possible to fix in clean_photoperiod.R
    }

## bbdataplease END ##

##
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
source("source/othertreats.R")
dim(bb.noNA)
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
dim(bb.noNA)
d <- bb.noNA
## (3) Get fewer columns for sanity
source("source/commoncols.R")
bb <- d
# bb <- subset(d, select=c(columnstokeep, columnschillunits))

# remove the values above 600 (which means remove the right-censored data, coded as 999)
bb <- subset(bb, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb$chill <- bb$chill/240
length(unique(bb$datasetID))

if(FALSE){
# deal with photo
bb.expphoto <- subset(bb, photo_type=="exp")
bb.rampphoto <- subset(bb, photo_type=="ramped")
bb.exprampphoto <- subset(bb, photo_type=="exp" | photo_type=="ramped")
bb.ambphoto <- subset(bb, photo_type=="amb" | photo_type=="none")

#sort(unique(bb.expphoto$datasetID))
#sort(unique(bb.rampphoto$datasetID))
#sort(unique(bb.ambphoto$datasetID))

# add in forcing
bb.exprampphotoforce <- subset(bb.exprampphoto, force_type=="exp"|force_type=="ramped")
bb.expphotoforce <- subset(bb.expphoto, force_type=="exp")

# add in chilling (exp)
bb.exprampphotoforceexpch <- subset(bb.exprampphotoforce, chill_type=="exp")
bb.allexpch <- subset(bb, chill_type=="exp")
}


##################################################
# Set the data you want to use deal with species #
##################################################

bb.all <- bb
bb.stan <- sppcomplexfx(bb.all) 
# bb.stan.alltypes.multcue <- sppcomplexfx.multcue(bb.all) 
# bb.stan.alltypes.nocrops <- sppcomplexfx.nocrops(bb.all)


##################################
## Prep the data for Stan model ##
##################################

# making some list out of the processed data. It will be input for the model ...
# PICK one of the below!!

datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )


datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )


source("source/bb_zscorepreds.R")
datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )


str(datalist.bb)

## from here you can run the models in models_stan.R! Or models_stan_supp.R
