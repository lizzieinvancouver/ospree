## Started 10 March 2017 ##
## By Lizzie to start ##

## This file builds tests fake data for testing Stan models for OSPREE budburst analysis ##

## This version has only simple linear model with no interactions:
# bb ~ force + photo + chill
# and only random intercepts for species!

library(lme4)
library(rstan)
library(shinystan)
library(bayesplot)

#setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else 
  setwd("~/Documents/git/ospree/analyses/bb_analysis")

source("bb_testdata_generate.R")

# lme version
summary(lme2 <- lmer(bb ~ chill+force+photo+chill*force+chill*photo+ force*photo+ (1|sp), data = testdat)) 
ranef(lme2)
fixef(lme2)


##
# try the model
datalist.td <- with(testdat2, 
    list(y = bb, 
         chill = as.numeric(chill), 
         force = as.numeric(force), 
         photo = as.numeric(photo),
         sp = as.numeric(sp),
         N = nrow(testdat),
         n_sp = length(unique(sp))
         )
)

osp.td <- stan('..//stan/bb/M1_daysBBwinter_2level.stan', data = datalist.td, 
                 iter = 100
                  ) 

osp.tdsum<-summary(osp.td)$summary

osp.tdsum[grep("mu_",rownames(osp.tdsum)),]

print(osp.td, pars = c("b_force", "b_photo", "b_chill", "b_cf","b_cp","b_fp", "mu_a_sp", "sigma_a_sp", "sigma_y"))

betas <- as.matrix(osp.td, pars = c("b_force", "b_photo", "b_chill", "b_cf","b_cp","b_fp"))
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


## Alternative! Try map2stan
library(rethinking)

# I think map2stan recognizes parameters because they have priors, so you always have to give them priors ...
# Note that the default iterations are very low!
goober <- map2stan(
    alist(
        y ~ dnorm(yhat, sigma_y),
        yhat ~ a[sp] + b_force*force + b_photo*photo + b_chill*chill,
        a[sp] ~ dnorm(mu_a_sp, sigma_a_sp),
        sigma_y ~ dnorm(0, 30),
        mu_a_sp ~ dnorm(0, 50),
        sigma_a_sp ~ dnorm(0, 10),
        b_force~ dnorm(0, 10),
        b_photo~ dnorm(0, 10),
        b_chill~ dnorm(0, 10)
    ) ,
    data=datalist.td )

# estimates
precis(goober)
## magic! See the underlying Stan code
stancode(goober)
