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
nst<-unique(bb.stan$datasetID)#49
length(nst)

#The flags for the main model are:
#
## set up the flags
use.chillports = FALSE
use.zscore = FALSE
use.allspp =FALSE#for the main model this is false
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

##
source("source/bbstanleadin.R")
##


nsp_mod<-unique(paste(bb.stan$genus,bb.stan$species))#203
length(nsp_mod)#71
nexp_mod<-unique(paste(bb.stan$datasetID,bb.stan$study))#72 experiments
length(nexp_mod)#57
nst_mod<-unique(bb.stan$datasetID)#49 studies
length(nst_mod)#38