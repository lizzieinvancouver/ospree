###ospree prep for knb###
#this file creates an ospree file with reduced columns for a more manageable number to prepare to post it on knb

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)
library(plyr)
library(dplyr)

## (1) Get the data and slim down to correct response and no NAs ..
d<-read.csv("../../../../analyses/output/ospree_clean_withchill_BB.csv", header=TRUE)
use.chillports=FALSE
source("source/bbdataplease_knb.R")
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
source("source/othertreats.R")
dim(bb.noNA)
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
dim(bb.noNA)
d <- bb.noNA
## (3) Get fewer columns for sanity
source("source/commoncols.R")
bb <- subset(d, select=c(columnstokeep, columnschillunits))

# remove the values above 600 (which means remove the right-censored data, coded as 999)
bb <- subset(bb, resp<600)


bb.all <- bb

write.csv(bb.all,"../ospreebb_forknb.csv", row.names = FALSE)
