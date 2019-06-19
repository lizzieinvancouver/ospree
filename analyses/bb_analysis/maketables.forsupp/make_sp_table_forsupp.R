## StartedMay 2019 
## By Ailene, modified from models_stan_ploting to match names in models_stan
## Plotting results from Stan models ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

## set up the flags
use.chillports = FALSE
use.zscore = TRUE
use.allspp = TRUE
use.multcuespp = FALSE
use.cropspp = TRUE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE


## name your figures paths (based on flags above) ... this needs work
##Ailene updated to match models_stan file
# chill ports centered, with only expramptypes, with crops, not all sp
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE  & 
    use.chillports==FALSE){
    tablepath <- "allsppwcrops_exprampfp"
}

## functions for plotting 
source("source/bbstanleadin.R")
#get sp names
spnames<-sort(unique(bb.stan$complex.wname))


#get studies that go with the names
bb.stan$ID_sp<-paste(bb.stan$datasetID,bb.stan$complex.wname)
sp.studies <- bb.stan %>% # start with the data frame
  distinct(ID_sp, .keep_all = TRUE) %>% # establishing grouping variables
  #filter(continent == 'europe' & year >= 1950) %>%#select out europe#this removes all rows with  field sample date=2013-10-22, even though some are in europe
  dplyr::select(datasetID, complex.wname)