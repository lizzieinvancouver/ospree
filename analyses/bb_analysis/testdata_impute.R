## Started 10 March 2017 ##
## By Lizzie to start ##
## Modified by Ailene 17 August 2018 to try out imputation in brms

## This file builds tests fake data for testing Stan models for OSPREE budburst analysis ##
## Then it adds in some NAs
## This files has versions with simple linear model with no interactions:
# bb ~ force + photo + chill
# and only random intercepts for species!
# And also with interactions ... 

library(lme4)
library(rstan)
library(shinystan)
library(bayesplot)
library(brms)
#setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else 
  setwd("~/Documents/git/ospree/analyses/bb_analysis")

# source('stan/savestan.R')
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

source("testdata_generate.R")

#now add some NAs to the force column
#our forcing data has 4.9%; chilling has 5.7% missing data
#Replace 5% of each of these with NAs
propmiss<-0.05
numiss<-propmiss*dim(testdat)[1]
#dim(testdat)
missing.chill<-as.integer(runif(numiss,min=1, max=1000))
missing.force<-as.integer(runif(numiss,min=1, max=1000))
missing.photo<-as.integer(runif(numiss,min=1, max=1000))

testdat.wNA<-testdat
testdat.wNA$chill[missing.chill]<-NA
testdat.wNA$force[missing.force]<-NA
testdat.wNA$photo[missing.photo]<-NA

##########################################################
# Model with no interactions; intercept only for grouping 
##########################################################

# lme version
summary(lme1 <- lmer(bb ~ chill+force+photo + (1|sp), data = testdat)) 
ranef(lme1)
fixef(lme1)

##
# try the model (intercept only)
datalist.td <- with(testdat, 
    list(y = bb, 
         chill = as.numeric(chill), 
         force = as.numeric(force), 
         photo = as.numeric(photo),
         sp = as.numeric(sp),
         N = nrow(testdat),
         n_sp = length(unique(sp))
         )
)

#osp.td <- stan('..//stan/bb/M1_daysBBnointer_2level_interceptonly.stan', data = datalist.td, 
 #                iter = 100
  #                ) 
#with no missing data
osp.td.brms<-brm(bb ~ chill+force+photo+#fixed effects
                    (1|sp), #random effects
                    data = testdat,
                    chains = 2) 
                      
summary(osp.td.brms)
fixef(osp.td.brms)
ranef(osp.td.brms)

#with missing data
osp.tdNA.brms<-brm(bb ~ chill+force+photo+#fixed effects
                     (1|sp), #random effects
                 data = testdat.wNA ,
                 chains = 2) 

summary(osp.tdNA.brms)
fixef(osp.tdNA.brms)
#estimates look ok even with missing data

##with missing data AND imputation.
## first with just chilling missing
bform <- bf(bb ~ mi(chill)+mi(force)+mi(photo)+#fixed effects
              (1|sp))+ #random effect
            bf(chill | mi() ~ bb+force+photo)+#how to mode missing chilling data)
             bf(force | mi() ~ bb+chill+photo)+#how to mode missing forcing data)
            bf(photo | mi() ~ bb+chill+force)#how to mode missing photoperiod data)
    
osp.tdNAimp.brms<-brm(bform, 
                   data = testdat.wNA) 

summary(osp.tdNAimp.brms)
#estimates look good (but they looked good with no imputation)





print(osp.td, pars = c("b_force", "b_photo", "b_chill", "mu_a_sp", "sigma_a_sp", "sigma_y"))

betas <- as.matrix(osp.td, pars = c("b_force", "b_photo", "b_chill"))
mcmc_intervals(betas)

spinters <- as.matrix(osp.td, pars=c(colnames(as.matrix(osp.td))[grep("a_sp",
    colnames(as.matrix(osp.td)))]))
mcmc_intervals(spinters)

siga_draws <- as.matrix(osp.td, pars = "sigma_a_sp")

siga_and_prior <- cbind(
  prior = rnorm(nrow(siga_draws), 0, 10), # draw from prior distribution
  posterior = siga_draws[, 1]
)
mcmc_areas(siga_and_prior) 

