## Started 25 July 2017 ##
## By Lizzie, and Dan and others ##

## Try to run REAL Ospree data ##
## With Stan! ##

## Take 1: This code is based heavily off bbmodel1_stan.R 
## Take 2: February 2017! ##
## Take 3: July 2017! ## New code to run stan models on Ospree (by Nacho, Lizzie and more)
## Take 4: June 2018! Lizzie re-organizes code and adds rstanarm 

## To do
# (a) think on adjusting forcetemp to incorporate nightime temps? And look at what we lose in photo and force!
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
usecentered = FALSE # change to TRUE to use centered data 
# Impt: still need to do deal with provenance and material (which mean some treatments show up more than once) 

#####################################
## A cheap run-thru of the issue ##
## May not be the most accurate way to go,
# but I am desperate! ##

if(FALSE){
library(lme4)
maineff <- lmer(resp~photo+chill+force + (1|datasetID) + (1|complex), data=bb.stan)
wintxns <- lmer(resp~photo+chill+force+chill*photo+chill*force+photo*force + (1|datasetID) + (1|complex), data=bb.stan)
w3wayintxn <- lmer(resp~photo*chill*force + (1|datasetID) + (1|complex), data=bb.stan)
wintxns.nofp <- lmer(resp~photo+chill+force+chill*photo+chill*force + (1|datasetID) + (1|complex), data=bb.stan)
wintxns.nofc <- lmer(resp~photo+chill+force+chill*photo+photo*force + (1|datasetID) + (1|complex), data=bb.stan)
wintxns.nocp <- lmer(resp~photo+chill+force+force*photo+chill*force + (1|datasetID) + (1|complex), data=bb.stan)
wintxns.nofpcp <- lmer(resp~photo+chill+force+chill*force + (1|datasetID) + (1|complex), data=bb.stan)
wintxns.nop <- lmer(resp~chill+force+chill*force + (1|datasetID) + (1|complex), data=bb.stan)

bb.intxn <- bb.stan
bb.intxn$cp <- bb.intxn$chill*bb.intxn$photo
bb.intxn$cf <- bb.intxn$chill*bb.intxn$force
bb.intxn$fp <- bb.intxn$photo*bb.intxn$force

bbno24p <- subset(bb.intxn, photo<23)
bbnofalusi <- subset(bb.intxn, datasetID!="falusi96" & datasetID!="falusi97" & datasetID!="falusi90")
wintxnsw3wayintxn.no24 <- lmer(resp~photo*chill*force + (1|datasetID) + (1|complex), data=bbno24p)

maineff.nofalusi <- lmer(resp~photo+chill+force + (1|datasetID) + (1|complex), data=bbnofalusi)
wintxns.nofalusi <- lmer(resp~photo+chill+force+chill*photo+chill*force+photo*force + (1|datasetID) + (1|complex),
    data=bbnofalusi)

par(mfrow=c(1,3))
hist(bb.stan$chill)
hist(bb.stan$photo)
hist(bb.stan$force)
par(mfrow=c(2,3))
plot(resp~cp, data=bb.intxn)
plot(resp~cf, data=bb.intxn)
plot(resp~fp, data=bb.intxn)
plot(resp~cp, data=bbnofalusi)
plot(resp~cf, data=bbnofalusi)
plot(resp~fp, data=bbnofalusi)
}
## End of a cheap run-thru 
#####################################

######################################
## Overview of the models run below ##
######################################
# All have partial pooling (pp) and include force (f), photo (p), chill (c)

# Main models:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)
# m2l.winsp: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp
# m2l.nistudy: a(sp) + a(datasetID) + f(sp) + p(sp) + c(sp)

# Other models: 
# m2l.nib (b for basic): a(sp) + f + p + c
# m2l.nisig: m2l.ni but with sigmoid (not running currently)
# m2l.wispint: a(sp) + f + p + c + cf + cp + fp
# m2l.wi: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp) + fp(sp)
# m2l.wicf: a(sp) + f(sp) + p(sp) + c(sp) + fp(sp) + cp(sp)
# m2l.wicp: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + fp(sp)
# m2l.wifp: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp)


##################################
## Main models as of July 2018 ##
##################################

# alternative: use centered data
if(usecentered){
datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.cen, 
                         force = force.cen, 
                         photo = photo.cen,
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
# centered: a: 71; f: -19; p: -8.6; c: -15

# getting predicted values if needed
# preds.m2lni.sum <- m2lni.sum[grep("yhat", rownames(m2lni.sum)),]

save(m2l.ni, file="stan/output/M1_daysBBnointer_2level.Rda")


########################################################
# real data on 2 level model (sp) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: M1_daysBBwinternospwinternosp_2level.stan: m2l.winsp
########################################################
m2l.winsp = stan('stan/winternosp_2level.stan', data = datalist.bb,
               iter = 4000, warmup=2500) # some n_eff issues
 
save(m2l.winsp, file="stan/output/M1_daysBBwinternosp_2level.Rda")

m2l.winsp.sum <- summary(m2l.winsp)$summary 
head(m2l.winsp.sum) 
m2l.winsp.sum[grep("mu_", rownames(m2l.winsp.sum)),]
m2l.winsp.sum[grep("b_cf", rownames(m2l.winsp.sum)),]
m2l.winsp.sum[grep("b_cp", rownames(m2l.winsp.sum)),]
m2l.winsp.sum[grep("b_fp", rownames(m2l.winsp.sum)),]
# a: 95; f: -1.8; p: -1.3; c: -6.8, small intxns (all <0.15) # 
# centered: a: 92; f: -30; p: -18; c: -34; cf: 10; cp: 8; f: 0.8 (low n_eff for some params)

# launch_shinystan(m2l.winsp)
save(m2l.winsp, file="stan/output/M1_daysBBwinter_2level.Rda")


#################################################################################
# real data on 2 level model (sp) with no interactions and study ID on intercept
# Note the notation: M1_daysBBnointer_2level_studyint.stan: m2l.nistudy
#################################################################################
datalist.bb.study <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         study = as.numeric(as.factor(bb.stan$datasetID)),
                         n_study = length(unique(bb.stan$datasetID)), 
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

datalist.bb.study.cen <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.cen, 
                         force = force.cen, 
                         photo = photo.cen,
                         study = as.numeric(as.factor(bb.stan$datasetID)),
                         n_study = length(unique(bb.stan$datasetID)), 
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

m2l.nistudy = stan('stan/nointer_2level_studyint.stan', data = datalist.bb.study,
               iter = 10000, warmup=6000) # struggling on intercepts a lot, for example mu_a is wandering around -100 to 100!

m2l.nistudy.cen = stan('stan/nointer_2level_studyint.stan', data = datalist.bb.study.cen,
               iter = 5000, warmup=3000)

betas.m2l.nistudy <- as.matrix(m2l.nistudy, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","b_force",
    "b_photo", "b_chill"))
m2lnistudy.sum <- summary(m2l.nistudy)$summary
m2lnistudy.sum[grep("mu_", rownames(m2lnistudy.sum)),]
# run 1: a_sp: 32; a_study: 41; f: -1.3; p: -0.3; c: -3.1
# run 2: a_sp: 39; a_study: 35; f: -1.3; p: -0.3; c: -3.1 (main effects are stable)
# launch_shinystan(m2l.nistudy.cen) # still not really converging

# launch_shinystan(m2l.nistudy)
save(m2l.nistudy, file="stan/output/M1_daysBBnointer_2level_studyint.Rda")


##
# Rstanarm on the above models ...and run the first two models on centered data
##
if(FALSE){
library(rstanarm)

# 86 divergent transitions! 
m2l.ni.arm <- stan_glmer(resp ~ (force + photo + chill ) +
    ((force + photo + chill)|complex.wname), data = bb.stan, chains=4)
# a: 69; f: -1.0; p: -0.6; c: -2.7


# 80 divergent transitions (everything else on a glance, looks fine)
m2l.winsp.arm <- stan_glmer(resp ~ (force + photo + chill +
    force*photo + force*chill + photo*chill) +
    ((force + photo + chill)|complex.wname), data = bb.stan)

summary(m2l.winsp.arm)
# a: 97; f: -1.9; p: -1.5; c: -7; fp: 0; fc: 0.1; pc: 0.1
# launch_shinystan(m2l.winsp.arm)

# Better n_eff than I got, but 49 divergent transitions
m2l.nistudy.arm <- stan_glmer(resp ~ (force + photo + chill ) +
    ((force + photo + chill)|complex.wname) + (1|datasetID), data = bb.stan)

m2lnistudy.arm.sum <- summary(m2l.nistudy.arm)
# launch_shinystan(m2l.nistudy.arm)
# a: 70.5; f: -1.1; p: -0.4; c: -2.8

# Just FYI (runs fast)
m2l.nistudy.arm.alt <- stan_glmer(resp ~ (force + photo + chill ) +
    (1|complex.wname) + (1|datasetID), data = bb.stan)
# a: 72; f: -1.4; p: -0.3; c: -1.9
}





##################################
## Other models as of July 2018 ##
##################################

########################################################
# real data on 2 level model (sp on intercept only) with no interactions 
# Note the notation: M1_daysBBnointer_2level_interceptonly.stan: m2l.nib
########################################################
m2l.nib = stan('stan/nointer_2level_interceptonly.stan', data = datalist.bb,
               iter = 2500, warmup=1500) 
  
m2l.nibsum <- summary(m2l.nib)$summary
m2l.nibsum[grep("mu_", rownames(m2l.nibsum)),] 
m2l.nibsum[grep("b_", rownames(m2l.nibsum)),]
# a: 60; f: -0.16; p: -0.68; c: -2.4


########################################################
# real data on 2 level model (sp) with no interactions, with sigmoid
# Note the notation: M1_daysBBnointer_2level_interceptonly_sigmoid.stan: m2l.nisig
########################################################
# Note: Lizzie needs to check this section of code #
# So for now, it is block-commented out
if(FALSE){

m2l.nisig = stan('stan/nointer_2level_interceptonly_sigmoid.stan', data = datalist.bb,
               iter = 2000, warmup=1500, control=list(adapt_delta=0.95)) 

betas.m2l.nisig  <- as.matrix(m2l.nisig, pars = c("b_force", "b_photo","a_chill", "b_chill"))
mcmc_intervals(betas.m2l.nisig[,1:5])

}



########################################################
# real data on 2 level model with 2 two-way interactions; partial pooling of sp on intercept ONLY
# Note the notation: M1_daysBBwinter_spintonly_2level.stan: m2l.wispint
########################################################
m2l.wispint = stan('stan/winter_spintonly_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 0 divergent transitions, some n_eff issues
 
save(m2l.wispint, file="stan/output/M1_daysBBwinter_spintonly_2level.Rda")

m2l.wispint.sum <- summary(m2l.wispint)$summary 
head(m2l.wispint.sum) 
m2l.wispint.sum[grep("b_", rownames(m2l.wispint.sum)),]
# a: 15; f: +3; p: +2; c: -1.8, small intxns



########################################################
# real data on 2 level model (sp) with interactions 
# Note the notation: M1_daysBBwinter_2level.stan: m2l.wi
########################################################
m2l.wi = stan('stan/winter_2level.stan', data = datalist.bb,
               iter=6000, warmup=3000, control = list(adapt_delta = 0.95)) 

save(m2l.wi, file="stan/output/M1_daysBBwinter_2level.Rda")

mint.sum <- summary(m2l.wi)$summary
mint.sum[grep("mu_", rownames(mint.sum)),]
# not converged (286 divergent transitions and other issues)

########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nocf_2level.stan: m2l.wicf
########################################################
m2l.wicf = stan('stan/winter_nocf_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 17 divergent transitions, n_eff issues

save(m2l.wicf, file="stan/bb/output/M1_daysBBwinter_nocf_2level.Rda")

m2l.wicf.sum <- summary(m2l.wicf)$summary
head(m2l.wicf.sum)
m2l.wicf.sum[grep("mu_", rownames(m2l.wicf.sum)),]
# 74 divergent transitions


########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nocp_2level.stan: m2l.wicp
########################################################
m2l.wicp = stan('stan/winter_nocp_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 703 divergent transitions and model has not converged! (ugh!)

save(m2l.wicp, file="stan/bb/output/M1_daysBBwinter_nocp_2level.Rda")

m2l.wicp.sum <- summary(m2l.wicp)$summary
head(m2l.wicp.sum)
# not converged (yep, still not okay, 270 divergent transitions)

########################################################
# real data on 2 level model (sp) with 2 two-way interactions 
# Note the notation: M1_daysBBwinter_nofp_2level.stan: m2l.wifp
########################################################
m2l.wifp = stan('stan/winter_nofp_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) # 91 divergent transitions and n_eff issues
 
save(m2l.wifp, file="stan/bb/output/M1_daysBBwinter_nofp_2level.Rda")

m2l.wifp.sum <- summary(m2l.wifp)$summary 
head(m2l.wifp.sum)
m2l.wifp.sum[grep("mu_", rownames(m2l.wifp.sum)),]
# a: 65; f: 0; p: 1.2; c: -1.7, small intxns (<0.1)
# 6 divergent transitions

###No photoperiod
m2l.wi.no.photo = stan('stan/winter_2levelnophoto.stan', data = datalist.bb,
              iter = 2500, warmup=1500)

mint.sum.nop <- summary(m2l.wi.no.photo)$summary
mint.sum.nop[grep("mu_", rownames(mint.sum.nop)),]
mint.sum.nop[grep("sigma_", rownames(mint.sum)),]

############## m2l.3winsp: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp + cfp
m2l.3winsp = stan('stan/internosp_2level.stan', data = datalist.bb,
              iter = 2500, warmup=1500)

save(m2l.3winsp, file="stan/output/wALLinternosp_2level.Rda")

m2l.3winsp.sum <- summary(m2l.3winsp)$summary 
head(m2l.3winsp.sum) 
m2l.3winsp.sum[grep("mu_", rownames(m2l.3winsp.sum)),]
m2l.3winsp.sum[grep("b_cf", rownames(m2l.3winsp.sum)),]
m2l.3winsp.sum[grep("b_cp", rownames(m2l.3winsp.sum)),]
m2l.3winsp.sum[grep("b_fp", rownames(m2l.3winsp.sum)),]
m2l.3winsp.sum[grep("b_cfp", rownames(m2l.3winsp.sum)),]




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


#################################################################################
# real data on 2 level model (sp) with interactions and study ID on intercept
# Note the notation: winter_2level_studyint.stan: m2l.wistudy
#################################################################################
m2l.wistudy = stan('stan/winter_2level_studyint.stan', data = datalist.bb.study,
               iter = 3000, warmup=2000) # 170 div. transitions (and other issues)

betas.m2l.wistudy <- as.matrix(m2l.wistudy, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","b_force",
    "b_photo", "b_chill"))
m2lwistudy.sum <- summary(m2l.wistudy)$summary
m2lwistudy.sum[grep("mu_", rownames(m2lwistudy.sum)),] 
# a_sp: 32; a_study: ; f: -1.3; p: -0.5; c: -1.6

save(m2l.wistudy, file="stan/output/M1_daysBBwinter_2level_studyint.Rda")
