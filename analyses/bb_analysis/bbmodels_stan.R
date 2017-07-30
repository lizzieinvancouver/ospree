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
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
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
## Be sure to keep an eye on this source file, I suspect we may need to update it!
source("bb_analysis/source/bbdataplease.R")

## make a bunch of things numeric (eek!)
bb$force <- as.numeric(bb$forcetemp)
bb$photo <- as.numeric(bb$photoperiod_day)
bb$chill <- as.numeric(bb$Total_Chilling_Hours)
bb$resp <- as.numeric(bb$response.time)

## For centering data, not doing it for now
#bb$photo.cen <- scale(bb$photo, center=TRUE, scale=TRUE)
#bb$force.cen <- scale(bb$force, center=TRUE, scale=TRUE)
#bb$chill.cen <- scale(bb$chill, center=TRUE, scale=TRUE)

## subsetting data, preparing genus variable, removing NAs
bb.prepdata <- subset(bb, select=c("datasetID", "resp", "chill", "photo", "force", "complex"))
dim(subset(bb, is.na(chill)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE))
bb.stan <- bb.prepdata[complete.cases(bb.prepdata),]
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
                         n_sp = length(unique(complex))
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
               iter = 2500, warmup=1500) # 15 divergent transitions

save(bb.m1.2lint, file="stan/bb/output/M1_daysBBwinter_2level.Rda")
