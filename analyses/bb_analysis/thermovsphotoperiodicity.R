### Checking on different ramped studies and how we made decisions vs how the study made decisions
### Started by Cat 5 Dec 2018

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillunits = FALSE # change to true for testing chill units
# Default is species complex
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE

source("source/bbstanleadin.R")

pho.ramp<-subset(bb.all, bb.all$photo_type=="ramped")
# sort(unique(pho.ramp$datasetID))
# [1] "basler12"   "basler14"   "partanen01" "partanen98"

## basler12: starts at 11h or 9.5hr and increases daily. Does ANOVA for tx differences. 
## basler14: starts at 10.8 or 9.2 and increases daily. Does ANOVA for tx diffs
## partanen98: amb, 6h constant, 6h increasing, 12h decreasing
## partanen01: 6h constant, 6h increasing, 16h decreasing - 16h decreasing has the most accum light period

for.ramp<-subset(bb.all, bb.all$force_type=="ramped")
# sort(unique(for.ramp$datasetID))
## [1] "basler12" "laube14a" "man10" 

# man10 removed from species complex step
## basler12: 5degC increasing 0.5 every 5 days - use GDDs
## laube14a: 7degC increasing until 27.5 - use GDDs

####################################################
#### Now to determine thermo vs photoperiodicity ###
####################################################
thermophoto<-bb.all[!(bb.all$photo_type=="amb"),] 

thermophoto$daynight<-ifelse(thermophoto$forcetemp!=thermophoto$forcetemp_night & 
                               thermophoto$forcetemp_night!="", "Y", NA)
thermophoto<-thermophoto[!is.na(thermophoto$daynight),]

# sort(unique(thermophoto$datasetID))
# [1] "falusi90"   "falusi96"   "man10"      "myking97"   "partanen01" "partanen98" "schnabel87"
# [8] "spann04"    "thielges75" "webb78"     "zohner16"  

## partanen01: changes in temperature were separate from photoperiod changes
## partanen98: day/night match changes in temperature
## falusi90: day/night match changes in temperature
## falusi96: day/night match changes in temperature
## man10: day/night match changes in temperature
## myking97: unclear... 
## schnabel87: day/night match changes in temperature
## spann04: day/night match changes in temperature
## thielges75: day/night match changes in temperature
## webb78: day/night match changes in temperature
## zohner16: day/night match changes in temperature

