## Started 27 November 2018 ##
## By Nacho ##

## following the models_stan_normal.R by Lizzie ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillunits = FALSE # change to true for testing chill units
use.allspp = FALSE
expphotoforce = TRUE # To use only with experimental photo and forcing
use.onecue = FALSE
use.nocrops = TRUE

## This is only temporary, changes to bbstanleadin in bbstanleadin_expphotoforce.R
## will be merged into bbstanleadin.R, I'm waiting to do so now to avoid 
## sync issues (29th nov 2018)
  
source("source/bbstanleadin_expphotoforce.R") 

# Flags to choose for this here file
use.zscore = TRUE # change to TRUE to use centered and scaled data

########################
## Z-scored data here ##
########################

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

######################################
## Overview of the models run below ##
######################################
# All have partial pooling (pp) and include force (f), photo (p), chill (c)

# Main models:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)
# m2l.winsp: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp

# Semi-main:
# m2l.wstudy: a + a(sp) + a(study) + f(sp) + p(sp) + c(sp)


########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
#options(mc.cores = 4)

m2l.ni = stan('stan/nointer_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500)


check_all_diagnostics(m2l.ni)
# launch_shinystan(m2l.ni)

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]

# PPC 
if(FALSE){
y_pred <- extract(m2l.ni, 'y_ppc')
par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}

# write.csv(m2lni.sum, "~/Desktop/quick.csv")
# write.csv(m2lni.sum, "~/MEGA/Work_Harvard_postdoc/Ospree/models/bbmodels_dec2018/m2lni.sum_nocrops.csv")

# Code if you want to save your models (do NOT push output to git)
if(FALSE){
if(!use.zscore){
save(m2l.ni, file="stan/output/M1_daysBBnointer_2level.Rda")
}

if(use.zscore){
save(m2l.ni, file="stan/output/M1_daysBBnointer_2levelz.Rda")
}

if(use.allspp){
save(m2l.ni, file="stan/output/M1_daysBBnointer_2level.allspp.Rda")
}
}

########################################################
# real data on 2 level model (sp) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: winternosp_2level.stan: m2l.winsp
########################################################
m2l.winsp = stan('stan/winternosp_2level.stan', data = datalist.bb,
               iter = 4000, warmup=2500) 
 

m2l.winsp.sum <- summary(m2l.winsp)$summary 
m2l.winsp.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
    "b_cf","b_cp","b_fp"),]

# PPC 
if(FALSE){
y_pred <- extract(m2l.winsp, 'y_ppc')
par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="With intxn model")
hist(bb.stan$response.time, breaks=40,col="black",add=T)

}

check_all_diagnostics(m2l.winsp)
# launch_shinystan(m2l.winsp)

# easier to transcribe results ....
#write.csv(m2l.winsp.sum, "~/MEGA/Work_Harvard_postdoc/Ospree/models/bbmodels_dec2018/mwinexpphotforce.csv")
#write.csv(m2l.winsp.sum, "~/MEGA/Work_Harvard_postdoc/Ospree/models/bbmodels_dec2018/mwinexpphotforce_allsps.csv")
#write.csv(m2l.winsp.sum, "~/MEGA/Work_Harvard_postdoc/Ospree/models/bbmodels_dec2018/mwinexpphotforce_onecue.csv")


# Code if you want to save your models (do NOT push to git)
if(FALSE){
if(!use.zscore){
save(m2l.winsp, file="~/MEGA/Work_Harvard_postdoc/Ospree/models/M1_daysBBwinter_2level.Rda")
}

if(use.zscore){
save(m2l.winsp, file="stan/output/M1_daysBBwinter_2levelz.Rda")
}

if(use.allspp){
save(m2l.winsp, file="stan/output/M1_daysBBwinter_2level.allspp.Rda")
}
}


########################################################
# real data on 2 level model (sp and study) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: nointer_2level_studyint: m2l.wstudy
########################################################

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

m2l.wstudy = stan('stan/nointer_2level_studyint_ncp.stan', data = datalist.bb,
               iter = 5000, warmup=3500) 

check_all_diagnostics(m2l.wstudy)
# launch_shinystan(m2l.wstudy)

m2l.wstudy.sum <- summary(m2l.wstudy)$summary
m2l.wstudy.sum[grep("mu_", rownames(m2l.wstudy.sum)),]
m2l.wstudy.sum[grep("alpha", rownames(m2l.wstudy.sum)),]

m2l.wstudy.sum[,1]
# write.csv(m2l.wstudy.sum, "~/Desktop/quick.csv")
#write.csv(m2l.wstudy.sum, "~/MEGA/Work_Harvard_postdoc/Ospree/models/bbmodels_dec2018/m2l.wstudy.sumexpphotforce_onecue.csv")
