## Started 10 March 2017 ##
## By Lizzie to start ##

## This file builds fake data for testing Stan models for OSPREE budburst analysis ##
## Based in part off FakeBudburst_Generate.R and FakeOspree_Generate.R (by Dan Flynn) ##

## This version has only simple linear model with no interactions:
# bb ~ force + photo + chill
# and only random intercepts for species!

library(lme4)

# nlab = 10 # number of labgroups
nsp = 20 # number of species

ntot = 50 # numbers of obs per species. 

#  with species
baseinter <- 20 # baseline intercept (days to BB) across all species
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

# now start building ...
testdat <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) no interactions, linear effects of force + photo + chill only
# (d) each obs is a different set of treatments

# and some important points ...
# (z) the below draws treatments from distribution in such a way that there is a lot more variation than we have


for(i in 1:nsp){ # loop over species. i = 1

    # continuous predictors, generate level (if you will) for each observation
    force = rnorm(ntot, 0, 2)
    photo = rnorm(ntot, 0, 2)
    chill = rnorm(ntot, 0, 5)

    # set up effect sizes
    chillcoef = -3 # steep slope for earlier day with higher chilling
    forcecoef = -2 # less steep for forcing
    photocoef = -1

    # SD for each treatment
    chillcoef.sd = 1
    forcecoef.sd = 0.5 
    photocoef.sd = 0.1

    # build model matrix 
    mm <- model.matrix(~chill+force+photo, data.frame(chill, force, photo))

    # coefficients need to match the order of the colums in the model matrix (mm)
    # so here, that's intercept, chill, force, photo
    coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd)
             )
  
    bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
    testdatx <- data.frame(bb, sp = i, 
                      chill, force, photo)
  
    testdat <- rbind(testdat, testdatx)  
}

# sanity check
summary(lme1 <- lmer(bb ~ chill+force+photo + (1|sp), data = testdat)) 
ranef(lme1)
fixef(lme1)


##
# try the model
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

library(rstan)
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

osp.td <- stan('stan/bb/M1_daysBBnointer_2level_interceptonly.stan', data = datalist.td, 
                 iter = 100
                  ) 
