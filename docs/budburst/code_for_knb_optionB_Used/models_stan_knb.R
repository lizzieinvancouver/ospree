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

######################################
# Flags to choose for bbstanleadin.R #
######################################

# Master flags! Here you pick if you want the flags for the main model (figure 2 in main text) versus other versions (all spp model, chill portions, uncentered predictors)
use.flags.for.mainmodel <- FALSE 
use.flags.for.spcomp.cp <- FALSE #chill portions for chilling, all predictors centered
use.flags.for.allspp.utah <- TRUE
use.flags.for.spcomp.utah.nonz <- FALSE
use.flags.for.spcomp.cp.nonz <- FALSE # predictors on natural scale, spcomplex with chill portions units for chilling
use.flags.for.allspp.utah.nonz <- FALSE # predictors on natural scale, spcomplex with utah units for chilling, as in. Fig 3-4 in main text of budburst ms

source("flags.for.models.in.bbms.R")

#some needed functions
source("speciescomplex.R") # this function makes sure all species/complexes present in 2 or more studies
source("speciescomplex.multcues.R") # this function similar to above but  requires all species/complexes to have more than one cue manipulated
source("speciescomplex.nocrops.R") # similar to speciescomplex.R but removes 4 crop species

#Read in the data
bb<-read.csv("ospreebb_forknb.csv", header = TRUE)

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
