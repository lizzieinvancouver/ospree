## Started 10 March 2017 ##
## By Lizzie to start ##

## This file builds tests fake data for testing Stan models for OSPREE budburst analysis ##

## This files has versions with simple linear model with no interactions:
# bb ~ force + photo + chill
# and only random intercepts for species!
# And also with interactions ... 

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

# source('stan/savestan.R')
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("testdata_generate.R")

##########################################################
# Model with no interactions; intercept only for grouping 
##########################################################

# lme version
summary(lme1 <- lmer(bb ~ chill+force+photo + (1|sp), data = testdat)) 
ranef(lme1)
fixef(lme1)

##
# try the model (intercept only)
# continous y data, centered preds
datalist.td <- with(testdat, 
    list(y = bb, # bbint 
         chill = as.numeric(chill), 
         force = as.numeric(force), 
         photo = as.numeric(photo),
         sp = as.numeric(sp),
         N = nrow(testdat),
         n_sp = length(unique(sp))
         )
)

# Start on our normal distribution models 
osp.td <- stan('stan/nointer_2level.stan', data = datalist.td, 
                 iter = 2000, chains=4
                  ) # seems good!

# summary(osp.td)
print(osp.td, pars = c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp", "mu_a_sp", "sigma_a_sp", "sigma_y"))

print(osp.td, pars = c("b_force", "b_photo", "b_chill", "mu_a_sp", "sigma_a_sp", "sigma_y"))

# Need to update code below to run on current versions of rstan and related packages
if(FALSE){ 
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
}

# poisson version, centered preds still
datalist.tdint <- with(testdatp, 
    list(y = bb, 
         chill = as.numeric(chill), 
         force = as.numeric(force), 
         photo = as.numeric(photo),
         sp = as.numeric(sp),
         N = nrow(testdat),
         n_sp = length(unique(sp))
         )
)

osp.td.pcp <- stan('stan/nointer_2level_poisson.stan', data = datalist.tdint, 
                 iter = 4000, chains=4 # 835ish divergent transitions
                  )

osp.td.pncp <- stan('stan/nointer_2level_poisson_ncp.stan', data = datalist.tdint, 
                 iter = 4000, chains=4  # 8000 ish divergent transitions
                  )

print(osp.td, pars = c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp", "mu_a_sp", "sigma_a_sp"))


osp.td.nb <- stan('stan/nointer_2level_negbin.stan', data = datalist.tdint,
                 iter = 4000, chains=4 # 666 divergent transitions
                  )

osp.td.nb <- stan('stan/nointer_2level_negbin.stan', data = datalist.tdint,
                 iter = 4000, chains=4
                  )


# Random efforts in progress ... 
if(FALSE){
# Have not tried: a Weibull but need to work out priors etc.:
   # https://stats.stackexchange.com/questions/329818/determining-normalizing-constant-for-weibull-distribution
   # https://discourse.mc-stan.org/t/numerical-problem-in-fitting-gamma-and-weibull-distributions/1813/11



# CP: negbin: 166 divergent, all chains FLAT
# NCP: negbin: 64 div trans, all chains FLAT
# CP: poisson won't even run
testdat$bbint <- round(testdat$bb)

library(brms)
try <- brm(bbint~force + photo + chill + (1|sp), data=testdat, family="negbinomial")

# MAP2STAN work ... 
# see also map2stan.R for some code help

library(rethinking)
d <- with(testdat, 
    list(y = round(bb), 
         chill = as.numeric(chill)
         )
)

m2 <- map2stan(
    alist(
        bbint ~ dpois(lamdba),
        log(lamdba) <- a + chill_sp*chill + force_sp*force + photo_sp*photo,
        a ~ dnorm(50,20),
        chill_sp~ dnorm(0,10),
        force_sp~ dnorm(0,10),
        photo_sp~ dnorm(0,10)),
    data=testdat, iter=5000, chains=4)

m2.nb <- map2stan(
    alist(
        bbint ~ dgampois(lamdba, sigma_y),
        log(lamdba) <- a + chill_sp*chill + force_sp*force + photo_sp*photo,
        sigma_y ~ dnorm(0,20),
        a ~ dnorm(50,20),
        chill_sp ~ dnorm(0,10),
        force_sp ~ dnorm(0,10),
        photo_sp ~ dnorm(0,10)),
    data=testdat, iter=5000, chains=4)


# coef values were -3, -2, -1
summary(m2.nb) # why is sigma_y massive?
precis(m2.nb, digits=3)
exp(4.06)-exp(4.06-0.051)
exp(4.06)-exp(4.06-0.036)
exp(4.06)-exp(4.06-0.017)

stancode(m2.nb)

m3 <- map2stan(
    alist(
        bbint ~ dpois(lamdba),
        log(lamdba) <- a[sp] + chill_sp[sp]*chill + force_sp[sp]*force + photo_sp[sp]*photo,
        a[sp] ~ dnorm(50,20),
        chill_sp[sp] ~ dnorm(0,10),
        force_sp[sp] ~ dnorm(0,10),
        photo_sp[sp] ~ dnorm(0,10)),
    data=testdat, iter=4000, chains=4)

}


m3 <- map2stan(
alist(y ~ dpois(theta),
        theta <- a + chill_sp*chill,
        a ~ dnorm(50,20),
        chill_sp~ dnorm(0,10)),
    data=d, chains=2, iter=500, cores=1)
}



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


##########################################################
# Model with interactions; stan model has grouping on intercepts and all slopes
##########################################################

# lme version (just grouping on intercepts)
summary(lme2 <- lmer(bb ~ chill+force+photo+chill*force+chill*photo+ force*photo + (1|sp), data = testdat2)) 
ranef(lme2)
fixef(lme2)

testdatX<-testdat2 # for centered data
#testdatX<-testdat3 # for uncentered data, gives treedepth issues but returns the correct information
testdatX<-testdat2.smintxn # for centered data with small intxns, seems to return the generally correct values but does not perfectly estimate the intxns
testdatX<-testdat2.smintxn2 # for centered data with small intxns and larger sigmas
  
datalist.td2 <- with(testdatX, 
    list(y = bb, 
         chill = as.numeric(chill), 
         force = as.numeric(force), 
         photo = as.numeric(photo),
         sp = as.numeric(sp),
         N = nrow(testdat2),
         n_sp = length(unique(sp))
         )
)

osp.td2 <- stan('..//stan/bb/M1_daysBBwinter_2level.stan', data = datalist.td2, 
                 iter = 2000
                  ) 

sumer.td2 <- summary(osp.td2)$summary
sumer.td2[grep("mu_", rownames(sumer.td2)),] # the means look good! need to check some more ...


print(osp.td2, pars = c("b_force", "b_photo", "b_chill", "b_cf","b_cp","b_fp", "mu_a_sp", "sigma_a_sp", "sigma_y"))

betas <- as.matrix(osp.td2, pars = c("b_force", "b_photo", "b_chill", "b_cf","b_cp","b_fp"))
mcmc_intervals(betas)

spinters <- as.matrix(osp.td2, pars=c(colnames(as.matrix(osp.td2))[grep("a_sp",
    colnames(as.matrix(osp.td2)))]))
mcmc_intervals(spinters)

siga_draws <- as.matrix(osp.td2, pars = "sigma_a_sp")

siga_and_prior <- cbind(
  prior = rnorm(nrow(siga_draws), 0, 10), # draw from prior distribution
  posterior = siga_draws[, 1]
)
mcmc_areas(siga_and_prior) 

