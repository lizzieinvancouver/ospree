## Started 25 July 2017 ##
## By Lizzie, and Dan and others ##

## Try to run REAL Ospree data ##
## With Stan! ##

## See also: models_stan_previous.R

## Take 1: This code is based heavily off bbmodel1_stan.R 
## Take 2: February 2017! ##
## Take 3: July 2017! ## New code to run stan models on Ospree (by Nacho, Lizzie and more)
## Take 4: June 2018! Lizzie re-organizes code and adds rstanarm 

## To do
# (a) think on adjusting forcetemp to incorporate nightime temps? And look at what we lose in photo and force! (Did I do this?)
# (b) see notes throughout on what Lizzie still needs to clean
# (c) subset down to relevant block/transplant treatments for gomory15
# (d) Try alternative metrics of chilling (chilldays?)

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
source("source/bbstanleadin.R")
use.zscore = FALSE # change to TRUE to use centered and scaled data 
# Impt: still need to do deal with provenance and material (which mean some treatments show up more than once) 
######################################
## Overview of the models run below ##
######################################
# All have partial pooling (pp) and include force (f), photo (p), chill (c)

# Main models:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)
# m2l.winsp: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp


##################################
## Main models as of July 2018 ##
##################################

# alternative: use centered data
if(use.zscore){
datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)
}

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: M1_daysBBnointer_2level.stan: m2l.ni
########################################################
m2l.ni = stan('stan/nointer_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) 

betas.m2l.ni <- as.matrix(m2l.ni, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","b_force",
    "b_photo", "b_chill"))
# mcmc_intervals(betas.m2l.ni[,1:3])
# launch_shinystan(m2l.ni)
m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),] 
# a: 71; f: -1.1; p: -0.6; c: -2.9
# z-score: a: 29; f: -5; p: -4; c: -9.5

# getting predicted values if needed
# preds.m2lni.sum <- m2lni.sum[grep("yhat", rownames(m2lni.sum)),]
if(!use.zscore){
save(m2l.ni, file="stan/output/M1_daysBBnointer_2level.Rda")
}

if(use.zscore){
save(m2l.ni, file="stan/output/M1_daysBBnointer_2levelz.Rda")
}


########################################################
# real data on 2 level model (sp) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: M1_daysBBwinternospwinternosp_2level.stan: m2l.winsp
########################################################
m2l.winsp = stan('stan/winternosp_2level.stan', data = datalist.bb,
               iter = 4000, warmup=2500) # some n_eff issues
 
save(m2l.winsp, file="stan/output/M1_daysBBwinternosp_2level.Rda")

m2l.winsp.sum <- summary(m2l.winsp)$summary 
m2l.winsp.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
    "b_cf","b_cp","b_fp"),]

# a: 95; f: -1.8; p: -1.3; c: -6.8, small intxns (all <0.15) # (low n_eff for some params, a whole mix of them!)
# z-score: a: 29; f: -5; p: -4; c: -10; cf: 2; cp: 2; cf: 0.4 

# launch_shinystan(m2l.winsp)

if(!use.zscore){
save(m2l.winsp, file="stan/output/M1_daysBBwinter_2level.Rda")
}

if(use.zscore){
save(m2l.winsp, file="stan/output/M1_daysBBwinter_2levelz.Rda")
}

##
# Rstanarm on the above models ...and run the first two models on centered data
##
if(FALSE){
library(rstanarm)

# 59 divergent transitions
m2l.ni.arm <- stan_glmer(resp ~ (force + photo + chill ) +
    ((force + photo + chill)|complex.wname), data = bb.stan, chains=4)
save(m2l.ni.arm, file="stan/output/M1_daysBBnointer_2level.arm.Rda")


# 51 divergent transitions (everything else on a glance, looks fine)
m2l.winsp.arm <- stan_glmer(resp ~ (force + photo + chill +
    force*photo + force*chill + photo*chill) +
    ((force + photo + chill)|complex.wname), data = bb.stan)
save(m2l.winsp.arm, file="stan/output/M1_daysBBwinter_2level.arm.Rda")


# Very slow! 144 div transitions
m2l.winsp.arm.alt <- stan_glmer(resp ~ (force + photo + chill +
    force*photo + force*chill + photo*chill) +
    ((force + photo + chill+force*photo + force*chill + photo*chill)|complex.wname), data = bb.stan, cores=4)
save(m2l.winsp.arm.alt, file="stan/output/M1_daysBBwinter_2level.arm.alt.Rda")

summary(m2l.winsp.arm)
# a: 97; f: -1.9; p: -1.5; c: -7; fp: 0; fc: 0.1; pc: 0.1
# launch_shinystan(m2l.winsp.arm)

# Just FYI (runs fast)
m2l.nistudy.arm.alt <- stan_glmer(resp ~ (force + photo + chill ) +
    (1|complex.wname) + (1|datasetID), data = bb.stan)
# a: 72; f: -1.4; p: -0.3; c: -1.9
}




## Code below not updated! ##
### But I think we will neeed it ##
########## SIDE BAR ##########
## Compare R2 today ##
observed.here <- bb.stan$resp

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),] 

# getting predicted values if needed
preds.m2lni.sum <- m2lni.sum[grep("yhat", rownames(m2lni.sum)),]
preds.m2lnistudy.sum <- m2lnistudy.sum[grep("yhat", rownames(m2lnistudy.sum)),]

m2lni.R2 <- 1- sum((observed.here-preds.m2lni.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
m2lnistudy.R2 <- 1- sum((observed.here-preds.m2lnistudy.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)

summary(lm(preds.m2lni.sum[,1]~observed.here)) # Multiple R-squared:  0.6051 (Chill Portions)
summary(lm(preds.m2lnistudy.sum[,1]~observed.here)) # Multiple R-squared:  0.7158 (Chill Portions)

# try the w/ interaction
preds.m2l.winsp.sum <- m2l.winsp.sum[grep("yhat", rownames(m2l.winsp.sum)),]
summary(lm(preds.m2l.winsp.sum[,1]~observed.here)) #Multiple R-squared:  0.6122

########## END SIDE BAR ##########
