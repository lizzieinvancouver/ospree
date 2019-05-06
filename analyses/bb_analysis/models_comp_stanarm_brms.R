# Started 15 March 2018 
# By Ailene 
# Updated a tiny bit by Dan 19 June 2018
#updated again MAy 2019 to include flags and final ospree model/data

# Fitting bb models with brms to compare coefficients with stan models

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)
library(brms)
library(rstanarm)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R

use.chillports = FALSE # change to false for using utah instead of chill portions (most models use chill portions z)
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

######################################
## Overview of the model run below ##
######################################
# Main model:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################

m2l.ni.brms <- brm(y ~ force + photo + chill +#main effects
                    ((force + photo+ chill)|sp), #random effects
                   data = datalist.bb,
                    chains = 2, cores = 2,iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))
summary(m2l.ni.brms)
#brms model says a:28.41, f= -4.55 , p=--3.83, c= -8.81, 
# utah ...
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==FALSE){
  save(m2l.ni.brms, file="stan/output/m2lnibrms_spcompexprampfputah_z.Rda")
}

#now check rstanarm
m2l.ni.rstanarm <-stan_lmer(resp ~ force.z + photo.z + chill.z +#main effects
                                     ((force.z + photo.z+ chill.z)|complex), #random effects
                                   data = bb.stan,
                                   chains = 2, cores = 2,iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))

summary(m2l.ni.rstanarm)

#compare to ospree model and brms
load("stan/output/m2lni_spcompexprampfputah_z.Rda")
m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),1]

fixef(m2l.ni.rstanarm)
modcomps<-as.data.frame(cbind(m2lni.sum[grep("mu_", rownames(m2lni.sum)),1]
,fixef(m2l.ni.rstanarm),fixef(m2l.ni.brms)[,1]))
colnames(modcomps)<-c("stan","rstanarm","brms")
#really similar!
