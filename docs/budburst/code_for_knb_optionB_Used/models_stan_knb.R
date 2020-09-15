###########################################################
#             Fit Bayesian hierarchical model             #
#       to OSPREE "Days to budburst" data in stan         #
###########################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)
library(plyr)
library(dplyr)
library(rstan)

##########################################
# Flags to choose for bbstanleadin_knb.R #
##########################################
# Choose if you want the main model (figure 2 in main text) versus all spp model
use.flags.for.mainmodel <- TRUE
use.flags.for.allspp <-FALSE
source("flags_for_models_knb.R")

#some needed functions
source("speciescomplex.R") # this function makes sure all species/complexes present in 2 or more studies
source("speciescomplex_multcues.R") # this function similar to above but  requires all species/complexes to have more than one cue manipulated
source("speciescomplex_nocrops.R") # similar to speciescomplex.R but removes 4 crop species

#Read in the data
bb.all<-bb<-read.csv("ospreebb_forknb.csv", header = TRUE)

source("bbstanleadin_knb.R")

#optional:
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

########################################################
# Main model:                                          #
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)                #
########################################################
m2l.ni = stan('nointer_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]
