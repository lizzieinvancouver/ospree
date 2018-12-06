## Started 6 July 2017 ##
## By Cat and Dan and others ##
# Updated a tiny bit by Dan 19 June 2018
# Updated 8 Oct 2018
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses/phylogeny") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/phylogeny")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/phylogeny") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny")

library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillports = FALSE # change to true for using chillportions instead of utah units

# Default is species complex
use.allspp = FALSE
use.nocropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE

source("source/bbstanleadin.phyla.R")

#### Nacho, put your brms code here using bb.stan. You can also use datalist.bb if you want to use a stan model.
m2l.ni = stan('stan/winter_2level_lat_nolat.stan', data = datalist.bb,
              iter = 2500, warmup=1500)

launch_shinystan(m2l.ni)

