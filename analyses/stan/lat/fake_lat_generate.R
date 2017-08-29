####This is fake data for the latitude analysis started by Dan B on 29/8/ 2017 
###baed ob bb_testdata_generate.R in bb_analysis

rm(list=ls()) 
options(stringsAsFactors = FALSE)
set.seed(73)

library(msm) # for truncated normal distribution

setwd("~/Documents/git/ospree/analyses")
##################################################################
## This version has only simple linear model with 2-way interactions:
# bb ~ force + photo + chill+lat + fp + fc + pc+ lf+lp+lc
# species varies by slopes and intercepts (aka random slopes and intercepts)

##################################################################

nsp = 7 # number of species

ntot = 100 # numbers of obs per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)
intermean <- 30 # mean for selecting intercept (days to BB) across all species
intersd <- 3 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)  # different intercepts by species

# now start building ...
testdat2 <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) only 2-way interactions between  force + photo + chill+lat
# (d) each obs is a different set of treatments

for(i in 1:nsp){ # loop over species. i = 1
  
  # continuous predictors, generate level (if you will) for each observation
  force = rnorm(ntot, 0, 2)
  photo = rnorm(ntot, 0, 2)
  chill = rnorm(ntot, 0, 5)
  lat = rnorm(ntot, 0, 15)
  
  # set up effect sizes
  chillcoef = -3 # steep slope for earlier day with higher chilling
  forcecoef = -2 # less steep for forcing
  photocoef = -1
  latcoef= -2 ###sure why not
  
  # SD for each treatment
  chillcoef.sd = 1
  forcecoef.sd = 0.5 
  photocoef.sd = 0.1
  latcoef.sd= 0.5
  
  # set interaction effects. 3 two-way interactions ### Cat should check Dan's assumtion of directionality here
  forcechillcoef = -1
  forcephotocoef = 0.5
  chillphotocoef = -1.5
  forcelatcoef= -1
  photolatcoef=0.5
  chilllatcoef=-1.5
  
  # SD for interaction effects. 3 two-way interactions
  forcephotocoef.sd = 0.2
  forcechillcoef.sd = 0.8
  chillphotocoef.sd = 0.5
  forcelatcoef.sd= 0.2
  photolatcoef.sd= 0.5
  chilllatcoef.sd=0.5
  
  # build model matrix 
  mm <- model.matrix(~(chill+force+photo+lat)^2, data.frame(chill, force, photo,lat))  
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo, lat
  coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd),
             rnorm(1,latcoef,latcoef.sd),
             rnorm(1, forcechillcoef, forcechillcoef.sd),
             rnorm(1, chillphotocoef, chillphotocoef.sd),
             rnorm(1, forcephotocoef, forcephotocoef.sd),
             rnorm(1, forcelatcoef,forcelatcoef.sd),
             rnorm(1,photolatcoef,photolatcoef.sd),
             rnorm(1,chilllatcoef,chilllatcoef.sd)
  )
  
  bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)  

  
  testdatx2 <- data.frame(bb, sp = i, 
                          chill, force, photo, lat)
  
  testdat2 <- rbind(testdat2, testdatx2)  
}  
###use testdat2 for model:

############Now do it for uncentered data.#######################################

nsp = 7 # number of species

ntot = 100 # numbers of obs per species. 



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
  lat = rtnorm(ntot, 55, 5, lower=40, upper=70)
  
  # set up effect sizes
  chillcoef = -3 # steep slope for earlier day with higher chilling
  forcecoef = -2 # less steep for forcing
  photocoef = -1
  latcoef= -2 ###sure why not
  
  # SD for each treatment
  chillcoef.sd = 1
  forcecoef.sd = 0.5 
  photocoef.sd = 0.1
  latcoef.sd= 0.5
  
  # set interaction effects. 3 two-way interactions
  forcechillcoef = -1
  forcephotocoef = 0.5
  chillphotocoef = -1.5
  forcelatcoef= -1
  photolatcoef=0.5
  chilllatcoef=-1.5
  
  # SD for interaction effects. 3 two-way interactions
  forcephotocoef.sd = 0.2
  forcechillcoef.sd = 0.8
  chillphotocoef.sd = 0.5
  forcelatcoef.sd= 0.2
  photolatcoef.sd= 0.5
  chilllatcoef.sd=0.5
  
  # build model matrix 
  mm <- model.matrix(~(chill+force+photo+lat)^2, data.frame(chill, force, photo, lat))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo, lat
  coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd),
             rnorm(1,latcoef,latcoef.sd),
             rnorm(1, forcechillcoef, forcechillcoef.sd),
             rnorm(1, chillphotocoef, chillphotocoef.sd),
             rnorm(1, forcephotocoef, forcephotocoef.sd),
             rnorm(1, forcelatcoef,forcelatcoef.sd),
             rnorm(1,photolatcoef,photolatcoef.sd),
             rnorm(1,chilllatcoef,chilllatcoef.sd)
  )
  
  bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1) 
  
  testdatx3 <- data.frame(bb, sp = i, 
                          chill, force, photo, lat)
  
  testdat3 <- rbind(testdat3, testdatx3)  
}


 
