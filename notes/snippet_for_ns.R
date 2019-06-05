#Code snippet to identify studies and species used in full ospree budburst analyses

## set up the flags
use.chillports = FALSE
use.zscore = FALSE
use.allspp = TRUE#for the main model this is false
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

##
source("source/bbstanleadin.R")
##

nsp<-unique(paste(bb.stan$genus,bb.stan$species))#70 species
nexp<-unique(paste(bb.stan$datasetID,bb.stan$study))#57 studies