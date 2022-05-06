###ospree prep for knb###
#this file creates an ospree file with reduced columns for a more manageable number to prepare to post it on knb

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)
library(plyr)
library(dplyr)

#set working directory
setwd("~/GitHub/ospree/docs/budburst/code_for_knb_optionB_Used/prep_for_knb")

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
  d<-read.csv("../../../../analyses/output/ospree_clean_withchill.csv", header=TRUE)
  d$chill.hrs<-d$Total_Chilling_Hours
  d$chill.ports<-d$Total_Chill_portions
  
  ## make a bunch of things numeric (taken from bbdataplease_knb.R)
  d$forceday <- as.numeric(d$forcetemp)
  d$forcenight <- as.numeric(d$forcetemp_night)
  d$photonight <- as.numeric(d$photoperiod_night)
  
  d$photo <- as.numeric(d$photoperiod_day)
  d$force <- d$forceday
  d$force[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
                is.na(d$photonight)==FALSE] <-
  (d$forceday[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
                      is.na(d$photonight)==FALSE]*
    d$photo[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
                     is.na(d$photonight)==FALSE] +
     d$forcenight[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
                          is.na(d$photonight)==FALSE]*
     d$photonight[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
                          is.na(d$photonight)==FALSE])/24

    d$chill <- as.numeric(d$Total_Utah_Model) 
    d$chill.hrs <- as.numeric(d$Total_Chilling_Hours) 
    d$chill.ports <- as.numeric(d$Total_Chill_portions) 

    d$resp <- as.numeric(d$response.time)

d.noNA <- subset(d, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(chill)==FALSE & is.na(resp)==FALSE)

#need to add "force_type" and "photo_type"
d.noNA$force_type<-NA
d.noNA$photo_type<-NA
d <- subset(d.noNA, select=c(columnstokeep, columnschillunits))


}

if(getbbonly==FALSE){write.csv(d,"../../../limitingcues/ospree_forknb_limcue.csv", row.names = FALSE)}

