## Started 25 July 2017 ##
## By Lizzie, and Dan and others ##

## Try to run REAL Ospree data ##
## With Stan! ##

## Take 1: This code is based heavily off bbmodel1_stan.R 
## Take 2: February 2017! ##
## Take 3: July 2017! ## New code to run stan models on Ospree (by Nacho, Lizzie and more)

## To do
# (a) think on adjusting forcetemp to incorporate nightime temps?
# (b) see notes throughout on what Lizzie still needs to clean


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
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

source('stan/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################################################
# Running the models with fake data? See bb_testdata_analysis.R #
################################################################# 

## get the data, merge in taxa info, subset down to what we want for:
# (1) 'species'
# (2) columns
# (3) response data 
## Be sure to keep an eye on this source file, you will need to update it
## One update will be after we have the Laube data
source("bb_analysis/source/bbdataplease.R")

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


########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: M1_daysBBnointer_2level.stan: m1.2l
########################################################
bb.m1.2l = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500)
     # Lizzie deleted: control=list(adapt_delta=0.95)
     # not needed, don't add this is you don't need it, it just slows things down

betas.bb.m1.2l <- as.matrix(bb.m1.2l, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","b_force",
    "b_photo", "b_chill"))
# mcmc_intervals(betas.bb.m1.2l[,1:3])
# launch_shinystan(bb.m1.2l)
m1.2l.sum <- summary(bb.m1.2l)$summary
m1.2l.sum[grep("mu_", rownames(m1.2l.sum)),] 
# OLD: force: -1; photo: -1.1; chill: -1.6
# Updated on 15 Aug 2017 with new species names: force: 0.25; photo: -2.3; chill: -1.5
# with crops removed:
# force: -1.3; photo: -0.4; chill: -1.5

# getting predicted values if needed
# preds.m1.2l <- m1.2l.sum[grep("yhat", rownames(m1.2l.sum)),]

save(bb.m1.2l, file="stan/bb/output/M1_daysBBnointer_2level.Rda")

########################################################
# real data on 2 level model (sp) with no interactions, with sigmoid
# Note the notation: M1_daysBBnointer_2level_interceptonly_sigmoid.stan: m1.2l.sig
########################################################
# Note: Lizzie needs to check this section of code #
# So for now, it is block-commented out
if(FALSE){

bb.m1.2lsig = stan('stan/bb/M1_daysBBnointer_2level_interceptonly_sigmoid.stan', data = datalist.bb,
               iter = 2000, warmup=1500, control=list(adapt_delta=0.95)) 

betas.bb.m1.2lsig  <- as.matrix(bb.m1.2lsig, pars = c("b_force", "b_photo","a_chill", "b_chill"))
mcmc_intervals(betas.bb.m1.2lsig[,1:5])

# m1.2lsig.sum <- summary(bb.m1.2lsig)$summary
# preds.m1.2lsig <- m1.2lsig.sum[grep("yhat", rownames(m1.2lsig.sum)),]

}

########################################################
# real data on 2 level model (sp) with interactions 
# Note the notation: M1_daysBBwinter_2level.stan: m1.2lint
########################################################
bb.m1.2lint = stan('stan/bb/M1_daysBBwinter_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 15 divergent transitions (need to update)

save(bb.m1.2lint, file="stan/bb/output/M1_daysBBwinter_2level.Rda")

mint.sum <- summary(bb.m1.2lint)$summary
mint.sum[grep("mu_", rownames(mint.sum)),]
# 49 intercept; +1.9 force, +1.7 photo, -2.0 chill, small intxns but fp -0.2

# with crops removed:
# 55 intercept; +0.03 force, +0.9 photo, -0.6 chill, small intxns: cf: -0.02; cp: -0.04; fp: -0.09

########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nocf_2level.stan: m1.2lintcf
########################################################
bb.m1.2lintcf = stan('stan/bb/M1_daysBBwinter_nocf_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 5 divergent transitions (need to update) 

save(bb.m1.2lintcf, file="stan/bb/output/M1_daysBBwinter_nocf_2level.Rda")

mint1.sum <- summary(bb.m1.2lintcf)$summary
head(mint1.sum) # 49 intercept; +1.9 force, +1.7 photo, -1.8 chill, small intxns but fp -0.2

########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nocp_2level.stan: m1.2lintcp
########################################################
bb.m1.2lintcp = stan('stan/bb/M1_daysBBwinter_nocp_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 35 divergent transitions (need to update)

save(bb.m1.2lintcp, file="stan/bb/output/M1_daysBBwinter_nocp_2level.Rda")

mint2.sum <- summary(bb.m1.2lintcp)$summary
head(mint2.sum) # 43 intercept; +2.2 force, +1.9 photo, -1.3 chill, small intxns but fp -0.2

########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nofp_2level.stan: m1.2lintfp
########################################################
bb.m1.2lintfp = stan('stan/bb/M1_daysBBwinter_nofp_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 387 divergent transitions (need to update)
 
save(bb.m1.2lintfp, file="stan/bb/output/M1_daysBBwinter_nofp_2level.Rda")

mint3.sum <- summary(bb.m1.2lintfp)$summary 
head(mint3.sum) # 82 intercept; -1.1 force, -0.9 photo, +0.5 chill, small intxns


########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinternospwinternosp_2level.stan: m1.2lintnsp
########################################################
bb.m1.2lintnsp = stan('stan/bb/M1_daysBBwinternospwinternosp_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 0 divergent transitions, but n_eff still struggling
 
save(bb.m1.2lintnsp, file="stan/bb/output/M1_daysBBwinternosp_2level.Rda")

mint4.sum <- summary(bb.m1.2lintnsp)$summary 
head(mint4.sum) 
