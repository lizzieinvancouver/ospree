## Started 10 March 2017 ##
## By Lizzie to start ##

## This file builds fake data for testing Stan models for OSPREE budburst analysis ##
## Based in part off FakeBudburst_Generate.R and FakeOspree_Generate.R (by Dan Flynn) ##

set.seed(73)

library(msm) # for truncated normal distribution

##################################################################
## This version has only simple linear model with no interactions:
# bb ~ force + photo + chill
# and only random intercepts for species!
##################################################################

# Note to self (Lizzie): could improve code, so easier to see distribution for a and sigma_y
# I did this below #

# nlab = 10 # number of labgroups
nsp = 20 # number of species

ntot = 50 # numbers of obs per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)
baseinter <- 60 # baseline intercept (days to BB) across all species
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


############################################################
# Test data as above, but with a poisson distribution...  ##
# Imagine there is a more thoughtful way to construct this #
############################################################

nsp = 20 # number of species

ntot = 50 # numbers of obs per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)
baseinter <- 60 # baseline intercept (days to BB) across all species
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

# now start building ...
testdatp <- vector()

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

    meanhere = mm %*% coeff
    bb <- rpois(ntot, meanhere) # rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
    testdatx <- data.frame(bb, sp = i, 
                      chill, force, photo)
  
    testdatp <- rbind(testdatp, testdatx)
    testdatp$bb[is.na(testdatp$bb)] <- 0

}

##################################################################
## This version has only simple linear model with 2-way interactions:
# bb ~ force + photo + chill + fp + fc + pc
# species varies by slopes and intercepts (aka random slopes and intercepts)
# centered version (all predictors are centered at 0)
##################################################################

# Code designed with normal distribution for intercepts for a and sigma_y

# nlab = 10 # number of labgroups
nsp = 30 # number of species

ntot = 50 # numbers of obs per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)

#  with species  (note to self: This is not the best, better to draw from a distribution)
intermean <- 30 # mean for selecting intercept (days to BB) across all species
intersd <- 3 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)  # different intercepts by species

# now start building ...
testdat2 <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) only 2-way interactions between  force + photo + chill
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

    # set interaction effects. 3 two-way interactions
    forcechillcoef = -1
    forcephotocoef = 0.5
    chillphotocoef = -1.5

    # SD for interaction effects. 3 two-way interactions
    forcephotocoef.sd = 0.2
    forcechillcoef.sd = 0.8
    chillphotocoef.sd = 0.5

    # build model matrix 
    mm <- model.matrix(~(chill+force+photo)^2, data.frame(chill, force, photo))

    # coefficients need to match the order of the colums in the model matrix (mm)
    # so here, that's intercept, chill, force, photo
    coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd),
             rnorm(1, forcechillcoef, forcechillcoef.sd),
             rnorm(1, chillphotocoef, chillphotocoef.sd),
             rnorm(1, forcephotocoef, forcephotocoef.sd)
             )
  
    bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
    testdatx2 <- data.frame(bb, sp = i, 
                      chill, force, photo)
  
    testdat2 <- rbind(testdat2, testdatx2)  
}

#summary(lm(bb ~ (chill+force+photo)^2, data = testdat2)) # sanity check

#library(lme4)
#summary(lmer(bb ~ (chill+force+photo)^2 + (1|sp), data = testdat2))

##################################################################
## This version has only simple linear model with 2-way interactions:
# bb ~ force + photo + chill + fp + fc + pc
# species varies by slopes and intercepts (aka random slopes and intercepts)
# non-centered data version
##################################################################

# Code designed with normal distribution for intercepts for a and sigma_y

# nlab = 10 # number of labgroups
nsp = 30 # number of species

ntot = 50 # numbers of obs per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)

#  with species  (note to self: This is not the best, better to draw from a distribution)
intermean <- 30 # mean for selecting intercept (days to BB) across all species
intersd <- 3 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)  # different intercepts by species

# now start building ...
testdat3 <- vector()

# assumptions:
# (a) predictors are NOT centered
# (b) predictors are not correlated
# (c) 2-way interactions only
# (d) each obs is a different set of treatments

# and some important points ...
# (z) the below draws treatments from distribution in such a way that there is a lot more variation than we have


for(i in 1:nsp){ # loop over species. i = 1
  
  force = rnorm(ntot, 15, 3) # for centered: force = rnorm(ntot, 0, 2)
  photo = rtnorm(ntot, 12, 3, lower=0, upper=24) 
  chill = rtnorm(ntot, 20, 10, lower=0, upper=Inf) 
  
  # set up effect sizes
  chillcoef = -3 # steep slope for earlier day with higher chilling
  forcecoef = -2 # less steep for forcing
  photocoef = -1
  
  # SD for each treatment
  chillcoef.sd = 1
  forcecoef.sd = 0.5 
  photocoef.sd = 0.1
  
  # set interaction effects. 3 two-way interactions
  forcechillcoef = -1
  forcephotocoef = 0.5
  chillphotocoef = -1.5
  
  # SD for interaction effects. 3 two-way interactions
  forcephotocoef.sd = 0.2
  forcechillcoef.sd = 0.8
  chillphotocoef.sd = 0.5
  
  # build model matrix 
  mm <- model.matrix(~(chill+force+photo)^2, data.frame(chill, force, photo))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo
  coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd),
             rnorm(1, forcechillcoef, forcechillcoef.sd),
             rnorm(1, chillphotocoef, chillphotocoef.sd),
             rnorm(1, forcephotocoef, forcephotocoef.sd)
  )
  
  bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx3 <- data.frame(bb, sp = i, 
                          chill, force, photo)
  
  testdat3 <- rbind(testdat3, testdatx3)  
}


#summary(lmer(bb ~ (chill+force+photo)^2 + (1|sp), data = testdat3))




##################################################################
## This version has only simple linear model with 2-way interactions:
# bb ~ force + photo + chill + fp + fc + pc
# species varies by slopes and intercepts (aka random slopes and intercepts)
# centered version (all predictors are centered at 0)
# with very low effects of interactions!
##################################################################

# Code designed with normal distribution for intercepts for a and sigma_y

# nlab = 10 # number of labgroups
nsp = 30 # number of species

ntot = 50 # numbers of obs per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)

#  with species  (note to self: This is not the best, better to draw from a distribution)
intermean <- 30 # mean for selecting intercept (days to BB) across all species
intersd <- 3 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)  # different intercepts by species

# now start building ...
testdat2.smintxn <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) only 2-way interactions between  force + photo + chill
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

    # set interaction effects. 3 two-way interactions
    forcechillcoef = -0.2
    forcephotocoef = -0.007
    chillphotocoef = -0.05

    # SD for interaction effects. 3 two-way interactions
    forcephotocoef.sd = 0.2
    forcechillcoef.sd = 0.8
    chillphotocoef.sd = 0.5

    # build model matrix 
    mm <- model.matrix(~(chill+force+photo)^2, data.frame(chill, force, photo))

    # coefficients need to match the order of the colums in the model matrix (mm)
    # so here, that's intercept, chill, force, photo
    coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd),
             rnorm(1, forcechillcoef, forcechillcoef.sd),
             rnorm(1, chillphotocoef, chillphotocoef.sd),
             rnorm(1, forcephotocoef, forcephotocoef.sd)
             )
  
    bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
    testdatx2.smintxn <- data.frame(bb, sp = i, 
                      chill, force, photo)
  
    testdat2.smintxn <- rbind(testdat2.smintxn, testdatx2.smintxn)  
}

#summary(lm(bb ~ (chill+force+photo)^2, data = testdat2.smintxn)) # sanity check

#library(lme4)
#summary(lmer(bb ~ (chill+force+photo)^2 + (1|sp), data = testdat2.smintxn))




##################################################################
## This version has only simple linear model with 2-way interactions:
# bb ~ force + photo + chill + fp + fc + pc
# species varies by slopes and intercepts (aka random slopes and intercepts)
# centered version (all predictors are centered at 0)
# with very low effects of interactions and with large sigma on the inxns
##################################################################

# Code designed with normal distribution for intercepts for a and sigma_y

# nlab = 10 # number of labgroups
nsp = 30 # number of species

ntot = 50 # numbers of obs per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)

#  with species  (note to self: This is not the best, better to draw from a distribution)
intermean <- 30 # mean for selecting intercept (days to BB) across all species
intersd <- 3 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)  # different intercepts by species

# now start building ...
testdat2.smintxn2 <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) only 2-way interactions between  force + photo + chill
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

    # set interaction effects. 3 two-way interactions
    forcechillcoef = -0.2
    forcephotocoef = -0.007
    chillphotocoef = -0.05

    # SD for interaction effects. 3 two-way interactions
    forcephotocoef.sd = 2
    forcechillcoef.sd = 4.5
    chillphotocoef.sd = 0.5

    # build model matrix 
    mm <- model.matrix(~(chill+force+photo)^2, data.frame(chill, force, photo))

    # coefficients need to match the order of the colums in the model matrix (mm)
    # so here, that's intercept, chill, force, photo
    coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd),
             rnorm(1, forcechillcoef, forcechillcoef.sd),
             rnorm(1, chillphotocoef, chillphotocoef.sd),
             rnorm(1, forcephotocoef, forcephotocoef.sd)
             )
  
    bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
    testdatx2.smintxn2 <- data.frame(bb, sp = i, 
                      chill, force, photo)
  
    testdat2.smintxn2 <- rbind(testdat2.smintxn2, testdatx2.smintxn2)  
}

#summary(lm(bb ~ (chill+force+photo)^2, data = testdat2.smintxn2)) # sanity check

#library(lme4)
#summary(lmer(bb ~ (chill+force+photo)^2 + (1|sp), data = testdat2.smintxn2))
