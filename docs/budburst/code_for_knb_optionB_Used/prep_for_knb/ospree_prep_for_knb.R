###ospree prep for knb###
#this file creates an ospree file with reduced columns for a more manageable number to prepare to post it on knb

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)
library(plyr)
library(dplyr)

# set working directory
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/docs/budburst/code_for_knb_optionB_Used/prep_for_knb") 
 } else if(length(grep("ailene", getwd())>0)){  setwd("~/GitHub/ospree/docs/budburst/code_for_knb_optionB_Used/prep_for_knb")
} else setwd("~/Documents/git/ospree/analyses")

#set whether you want the bb version or not
getbbonly=FALSE
## (1) Get fewer columns for sanity
source("source/commoncols.R")

## (1) Get the data and slim down to correct response and no NAs ..
if(getbbonly==TRUE){d<-read.csv("../../../../analyses/output/ospree_clean_withchill_BB.csv", header=TRUE)
  use.chillports=FALSE
  source("source/bbdataplease_knb.R")
  
  ## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
  
  source("source/othertreats.R")
  dim(bb.noNA)
  bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
  dim(bb.noNA)
  d <- bb.noNA
  
  bb <- subset(d, select=c(columnstokeep, columnschillunits))
  
  # remove the values above 600 (which means remove the right-censored data, coded as 999)
  bb <- subset(bb, resp<600)
  
  
  bb.all <- bb
  
  if(getbbonly==TRUE){write.csv(bb.all,"../ospreebb_forknb.csv", row.names = FALSE)}
  
}

#Added for limiting cues ms:

if(getbbonly==FALSE){
  d <- read.csv("../../../../analyses/output/ospree_clean.csv", header=TRUE)
  colstodelete <- c("other.treatment",
                    "irradiance",
                    "irradiance.units",
                    "humidity",
                    "number.longdays",
                    "dormancy_induction_temp_day",
                    "dormancy_induction_temp_night",       
                    "dormancy_induction_days",
                    "dormancy_induction_photoperiod_day",
                    "dormancy_induction_photoperiod_night",
                    "dormancy_induction_days.1",
                    "freeze.treatment.time",
                    "freeze.treatment.photoperiod_day",
                    "freeze.treatment.photoperiod_night",
                    "freeze.treatment.temp_day",
                    "freeze.treatment.temp_night",
                    "response..pre.treatment.",
                    "response..post.treatment",
                    "response..pre.treatment",
                    "datasetIDstudy",
                    "multiresp",
                    "multibothresp",
                    "multi_respvar",
                    "multi_respvar.simple",
                    "vector.duplicates",
                    "to.remove")

#need to add "force_type" and "photo_type"
d$force_type<-NA
d$photo_type<-NA
d <- d[,-which(names(d) %in% colstodelete)]

}

if(getbbonly==FALSE){write.csv(d,"../../../limitingcues/ospree_forknb_limcue.csv", row.names = FALSE)}

