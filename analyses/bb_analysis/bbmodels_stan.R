## Started 25 July 2017 ##
## By Lizzie, and Dan and others ##

## Try to run REAL Ospree data ##
## With Stan! ##

## Take 1: This code is based heavily off bbmodel1_stan.R 
## Take 2: February 2017! ##
## Take 3: July 2017! ## New code to run stan models on Ospree (by Nacho, Lizzie and more)

## To do
# (a) think on adjusting forcetemp to incorporate nightime temps? And look at what we lose in photo and force!
# (b) see notes throughout on what Lizzie still needs to clean
# (c) subset down to relevant block/transplant treatments for gomory15
# (d) Try alternative metrics of chilling (chilldays?)

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
# library(rstanarm)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

source('stan/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################################################
# Running the models with fake data? See bb_testdata_analysis.R #
################################################################# 

## 3 steps to major cleaning: Get the data, merge in taxa info, subset down to what we want for:
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!
## (1) Get the data and slim down to correct response and no NAs ...
source("bb_analysis/source/bbdataplease.R")
## (2) Deal with species
dim(bb.noNA)
d <- bb.noNA
source("bb_analysis/source/speciescomplex.R")
bb.noNA.wtaxa <- d
dim(bb.noNA.wtaxa)
unique(bb.noNA.wtaxa$complex)
# (3) Get fewer columns for sanity
source("bb_analysis/source/commoncols.R")
bb <- subset(bb.noNA.wtaxa, select=columnstokeep)

## For centering data, not doing it for now
#bb$photo.cen <- scale(bb$photo, center=TRUE, scale=TRUE)
#bb$force.cen <- scale(bb$force, center=TRUE, scale=TRUE)
#bb$chill.cen <- scale(bb$chill, center=TRUE, scale=TRUE)

## subsetting data, preparing genus variable, removing NAs (err, again
# remove crops?
# bb <- subset(bb, type!="crop")
bb.stan <- subset(bb, select=c("datasetID", "resp", "chill", "photo", "force", "complex", "type"))
bb.stan$complex <- as.numeric(as.factor(bb.stan$complex))

# remove the two values above 600
bb.stan <- subset(bb.stan, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb.stan$chill <- bb.stan$chill/240

length(unique(bb.stan$datasetID))

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
# photoperiod, and where we have a response in days and total chilling. 

## Prep the data for Stan model
# making a list out of the processed data. It will be input for the model
datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)
## real data with only experimental chilling (no field chilling)
#osp.td3 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td,
 #              iter = 2000,warmup=1500,control=list(adapt_delta=0.95))

######################################
## Overview of the models run below ##
######################################
# All have partial pooling (pp) and include force (f), photo (p), chill (c)
# m2l.nib (b for basic): a(sp) + f + p + c
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)
# m2l.nisig: m2l.ni but with sigmoid (not running currently)
# m2l.wispint: a(sp) + f + p + c + cf + cp + fp
# m2l.winsp: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp
# m2l.wi: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp) + fp(sp)
# m2l.wicf: a(sp) + f(sp) + p(sp) + c(sp) + fp(sp) + cp(sp)
# m2l.wicp: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + fp(sp)
# m2l.wifp: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp)

########################################################
# real data on 2 level model (sp on intercept only) with no interactions 
# Note the notation: M1_daysBBnointer_2level_interceptonly.stan: m2l.nib
########################################################
m2l.nib = stan('stan/bb/M1_daysBBnointer_2level_interceptonly.stan', data = datalist.bb,
               iter = 2500, warmup=1500) 
  
m2l.nibsum <- summary(m2l.nib)$summary
m2l.nibsum[grep("mu_", rownames(m2l.nibsum)),] 
m2l.nibsum[grep("b_", rownames(m2l.nibsum)),]
# a: 80; f: -1.8; p: -0.36; c: -1.6

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: M1_daysBBnointer_2level.stan: m2l.ni
########################################################
m2l.ni = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 12 divtrans and a couple n_eff issues

betas.m2l.ni <- as.matrix(m2l.ni, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","b_force",
    "b_photo", "b_chill"))
# mcmc_intervals(betas.m2l.ni[,1:3])
# launch_shinystan(m2l.ni)
m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),] 
# a: 76; f: -1.5; p: -0.5; c: -1.8

# getting predicted values if needed
# preds.m2lni.sum <- m2lni.sum[grep("yhat", rownames(m2lni.sum)),]

save(m2l.ni, file="stan/bb/output/M1_daysBBnointer_2level.Rda")

########################################################
# real data on 2 level model (sp) with no interactions, with sigmoid
# Note the notation: M1_daysBBnointer_2level_interceptonly_sigmoid.stan: m2l.nisig
########################################################
# Note: Lizzie needs to check this section of code #
# So for now, it is block-commented out
if(FALSE){

m2l.nisig = stan('stan/bb/M1_daysBBnointer_2level_interceptonly_sigmoid.stan', data = datalist.bb,
               iter = 2000, warmup=1500, control=list(adapt_delta=0.95)) 

betas.m2l.nisig  <- as.matrix(m2l.nisig, pars = c("b_force", "b_photo","a_chill", "b_chill"))
mcmc_intervals(betas.m2l.nisig[,1:5])

}

########################################################
# real data on 2 level model (sp) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: M1_daysBBwinternospwinternosp_2level.stan: m2l.winsp
########################################################
m2l.winsp = stan('stan/bb/M1_daysBBwinternosp_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 7 divergent transitions, some real n_eff issues
 
save(m2l.winsp, file="stan/bb/output/M1_daysBBwinternosp_2level.Rda")

m2l.winsp.sum <- summary(m2l.winsp)$summary 
head(m2l.winsp.sum) 
m2l.winsp.sum[grep("mu_", rownames(m2l.winsp.sum)),]
m2l.winsp.sum[grep("b_cf", rownames(m2l.winsp.sum)),]
m2l.winsp.sum[grep("b_cp", rownames(m2l.winsp.sum)),]
m2l.winsp.sum[grep("b_fp", rownames(m2l.winsp.sum)),]
# a: 79; f: -1.4; p: -0.4; c: -3.2, small intxns (<0.09) # n

# with crops removed...


########################################################
# real data on 2 level model with 2 two-way interactions; partial pooling of sp on intercept ONLY
# Note the notation: M1_daysBBwinter_spintonly_2level.stan: m2l.wispint
########################################################
m2l.wispint = stan('stan/bb/M1_daysBBwinter_spintonly_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 0 divergent transitions, some n_eff issues
 
save(m2l.wispint, file="stan/bb/output/M1_daysBBwinter_spintonly_2level.Rda")

m2l.wispint.sum <- summary(m2l.wispint)$summary 
head(m2l.wispint.sum) 
m2l.wispint.sum[grep("b_", rownames(m2l.wispint.sum)),]
# a: 83; f: -1.7; p: -0.3; c: -2.8, small intxns



########################################################
# real data on 2 level model (sp) with interactions 
# Note the notation: M1_daysBBwinter_2level.stan: m2l.wi
########################################################
m2l.wi = stan('stan/bb/M1_daysBBwinter_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 883 divergent transitions and model has not converged! (ugh!)

save(m2l.wi, file="stan/bb/output/M1_daysBBwinter_2level.Rda")

mint.sum <- summary(m2l.wi)$summary
mint.sum[grep("mu_", rownames(mint.sum)),]
# not converged (10 divergent transitions and other issues)

########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nocf_2level.stan: m2l.wicf
########################################################
m2l.wicf = stan('stan/bb/M1_daysBBwinter_nocf_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 17 divergent transitions, n_eff issues

save(m2l.wicf, file="stan/bb/output/M1_daysBBwinter_nocf_2level.Rda")

m2l.wicf.sum <- summary(m2l.wicf)$summary
head(m2l.wicf.sum)
m2l.wicf.sum[grep("mu_", rownames(m2l.wicf.sum)),]
# a: 91; f: -1.4; p: -0.8; c: -2.9, small intxns


########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nocp_2level.stan: m2l.wicp
########################################################
m2l.wicp = stan('stan/bb/M1_daysBBwinter_nocp_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 703 divergent transitions and model has not converged! (ugh!)

save(m2l.wicp, file="stan/bb/output/M1_daysBBwinter_nocp_2level.Rda")

m2l.wicp.sum <- summary(m2l.wicp)$summary
head(m2l.wicp.sum)
# not converged (yep, still not okay)

########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nofp_2level.stan: m2l.wifp
########################################################
m2l.wifp = stan('stan/bb/M1_daysBBwinter_nofp_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 91 divergent transitions and n_eff issues
 
save(m2l.wifp, file="stan/bb/output/M1_daysBBwinter_nofp_2level.Rda")

m2l.wifp.sum <- summary(m2l.wifp)$summary 
head(m2l.wifp.sum)
m2l.wifp.sum[grep("mu_", rownames(m2l.wifp.sum)),]
# a: 88; f: -1.8; p: -1.0; c: -3.91, small intxns

