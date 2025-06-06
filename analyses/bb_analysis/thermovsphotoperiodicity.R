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

use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = TRUE # change to false to use raw predictors

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


###########################################################################
#### Now look at studies with constant forcing but changing photoperiod ###
###########################################################################
bb.all$photoperiod_night<-NA
for(i in c(1:nrow(bb.noNA))){
  for(j in c(1:nrow(bb.all)))
    bb.all$photoperiod_night<-ifelse(bb.noNA$datasetID[i]==bb.all$datasetID[j] & bb.noNA$species[i]==bb.all$species[j] &
                                       bb.noNA$genus[i]==bb.all$genus[j] & bb.noNA$forcetemp[i]==bb.all$forcetemp[j] &
                                       bb.noNA$photoperiod_day[i]==bb.all$photoperiod_day[j] & bb.noNA$response.time[i]==bb.all$response.time[j],
                                     bb.noNA$photoperiod_night[i], bb.all$photoperiod_night)
}
stableforce<-bb.all[(bb.all$forcetemp==bb.all$forcetemp_night & bb.all$photoperiod_day !=bb.all$photoperiod_night),] 

#sort(unique(stableforce$datasetID))
## [1] "basler14"    "calme94"     "campbell75"  "chavarria09" "gianfagna85" "guerriero90" "heide12"     "heide93"     "heide93a"    "jones12"    
## [11] "li05"        "myking97"    "myking98"    "pagter15"    "partanen98"  "ramos99"     "schnabel87"  "sonsteby14"  "spiers74"    "swartz81"   
## [21] "worrall67" 





