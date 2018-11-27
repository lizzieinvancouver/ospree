###models Stan exp+ramp. This is R code to model OSPREE bud burst data with both experimental and ramped data.
##Began 27-Nov-2018 by Dan B

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

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
use.allspp = FALSE

source("source/bbstanleadin.R")

use.zscore = TRUE 
####This code is meant to model with ramped and exp photo and ramped and exp forcing. So, if testing all species the data should be
#bb.stan
######Lizzie already ran 6 models with all species and species complex
###my models should be with spcomplex.onecue species; spcomplex.nocrops

unique(bb.stan.onecue$complex.wname) ##this ran with all species ##needs fixing, ask Cat later?
unique(bb.stan$complex.wname)
setdiff(bb.stan$complex.wname,bb.stan.onecue$complex.wname)

if(use.zscore){
  datalist.bb.onecue <- with(bb.stan.onecue, 
                      list(y = resp, 
                           chill = chill.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan.onecue),
                           n_sp = length(unique(bb.stan.onecue$complex))
                      )
  )
}
########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
m2l.ni.onecue = stan('stan/nointer_2level.stan', data = datalist.bb.onecue,
              iter = 2500, warmup=1500)

check_all_diagnostics(m2l.ni.onecue)
# launch_shinystan(m2l.ni)

m2lni.onecue.sum <- summary(m2l.ni.onecue)$summary
m2lni.onecue.sum[grep("mu_", rownames(m2lni.onecue.sum)),]
