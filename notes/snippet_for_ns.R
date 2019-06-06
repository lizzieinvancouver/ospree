#Code snippet to identify studies and species used in full ospree budburst analyses
rm(list=ls()) 
options(stringsAsFactors = FALSE)
## set up the flags
use.chillports = FALSE
use.zscore = FALSE
use.allspp = TRUE#for the main model this is false
use.multcuespp = FALSE
use.cropspp = TRUE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

##
source("source/bbstanleadin.R")
##


nsp<-unique(paste(bb.stan$genus,bb.stan$species))#203
length(nsp)#203
nexp<-unique(paste(bb.stan$datasetID,bb.stan$study))#72 experiments
length(nexp)
nst<-unique(bb.stan$datasetID)#49 studies
length(nst)
