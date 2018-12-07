## Started 25 July 2017 (but also 13 July 2018) ##
## UPDATED in December 2018 after a new round of model work at the retreat ##
## By Lizzie, and Dan and others ##

## These are models that were in models_stan.R but that for various reasons #
# we stopped working with so much ##

## Commit efcd6568fbc2226c37b4310a6c4d78e810427b7a (5 Dec 2018) also has gamma and negative binomial code we deleted ##

## Impt note! Some of the model results here were updated in July 2018 #
# after more data cleaning, but *not* all the models ##


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
source("source/bbstanleadin.R") # alert! You probably need an older version of this file for the below to run.
use.zscore = FALSE # change to TRUE to use centered and scaled data 
# Impt: still need to do deal with provenance and material (which mean some treatments show up more than once) 



#############################################################
## Overview of the main models running as of November 2018 ##
############################################################# 
# All have partial pooling (pp) and include force (f), photo (p), chill (c)

# Main model:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

# Models we worked on for a while, but no longer:
# m2l.wstudy: a + a(sp) + a(study) + f(sp) + p(sp) + c(sp)
# m2l.winsp = stan('stan/winternosp_2level.stan', data = datalist.bb,
   #            iter = 4000, warmup=2500) 
# m2l.winsp: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp
# m2l.wstudy = stan('stan/nointer_2level_studyint_ncp.stan', data = datalist.bb,
    #       iter = 5000, warmup=3500) 



########################################################
# real data on 2 level model (sp) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: winternosp_2level.stan: m2l.winsp
########################################################
m2l.winsp = stan('stan/archive/winternosp_2level.stan', data = datalist.bb,
               iter = 4000, warmup=2500) 
 

m2l.winsp.sum <- summary(m2l.winsp)$summary 
m2l.winsp.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
    "b_cf","b_cp","b_fp"),]


########################################################
# real data on 2 level model (sp and study) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: nointer_2level_studyint: m2l.wstudy
########################################################

# bb.stan <- bb.expphotoforce.allspp

datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         study = as.numeric(as.factor(bb.stan$datasetID)),
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex)),
                         n_study = length(unique(bb.stan$datasetID))
                    )
                    )


m2l.wstudy = stan('stan/archive/nointer_2level_studyint_ncp.stan', data = datalist.bb,
               iter = 5000, warmup=3500) 

check_all_diagnostics(m2l.wstudy)
# launch_shinystan(m2l.wstudy)

m2l.wstudy.sum <- summary(m2l.wstudy)$summary
m2l.wstudy.sum[grep("mu_", rownames(m2l.wstudy.sum)),]
m2l.wstudy.sum[grep("alpha", rownames(m2l.wstudy.sum)),]

m2l.wstudy.sum[,1]
# write.csv(m2l.wstudy.sum, "~/Desktop/quick.csv")


###########################################################
###########################################################
###########################################################


#########################################################
## Overview of the main models running as of July 2018 ##
#########################################################
# All have partial pooling (pp) and include force (f), photo (p), chill (c)

# Model with convergence issues ...
# m2l.nistudy: a(sp) + a(datasetID) + f(sp) + p(sp) + c(sp)

# Other models we tried to better understand our main models:
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

datalist.bb.study.z <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         study = as.numeric(as.factor(bb.stan$datasetID)),
                         n_study = length(unique(bb.stan$datasetID)), 
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

m2l.nistudy = stan('stan/nointer_2level_studyint.stan', data = datalist.bb.study,
               iter = 10000, warmup=6000) # struggling on intercepts a lot, for example mu_a is wandering around -100 to 100!

m2l.nistudy.z = stan('stan/nointer_2level_studyint.stan', data = datalist.bb.study.z,
               iter = 5000, warmup=3000)

betas.m2l.nistudy <- as.matrix(m2l.nistudy, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","b_force",
    "b_photo", "b_chill"))
m2lnistudy.sum <- summary(m2l.nistudy)$summary
m2lnistudy.sum[grep("mu_", rownames(m2lnistudy.sum)),]
# run 1: a_sp: 32; a_study: 41; f: -1.3; p: -0.3; c: -3.1
# run 2: a_sp: 39; a_study: 35; f: -1.3; p: -0.3; c: -3.1 (main effects are stable)
# launch_shinystan(m2l.nistudy.z) # still not really converging

# launch_shinystan(m2l.nistudy)
save(m2l.nistudy, file="stan/output/M1_daysBBnointer_2level_studyint.Rda")


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
