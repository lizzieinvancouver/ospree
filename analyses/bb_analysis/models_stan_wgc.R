## Started 20 November 2018 ##
## By Lizzie ##


## Try to run REAL Ospree data ##
## With Stan! ##

## See also: models_stan_previous.R

## Take 1: This code is based heavily off bbmodel1_stan.R 
## Take 2: February 2017! ##
## Take 3: July 2017! ## New code to run stan models on Ospree (by Nacho, Lizzie and more)
## Take 4: June 2018! Lizzie re-organizes code and adds rstanarm
## Take 5: 4-5 December 2018! Big reorganization (see models_stan_previous.R)

## To do
# (a) subset down to relevant block/transplant treatments for gomory15??
# Impt: not dealing with provenance and material (which mean some treatments show up more than once but the partial pooling should handle this we think)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

######################################
# Flags to choose for bbstanleadin.R #
######################################

# Master flags! Here you pick if you want the flags for the main model (figure 2 in main text) versus other versions (all spp model, chill portions, uncentered predictors, as in supp table and figures 3-4)
use.flags.for.mainmodel <- TRUE
use.flags.for.spcomp.cp <- FALSE
use.flags.for.allspp.utah <- FALSE
use.flags.for.spcomp.utah.nonz <- FALSE
use.flags.for.spcomp.cp.nonz <- FALSE # predictors on natural scale, spcomplex with utah units. Fig 3-4 in main text of budburst ms
use.flags.for.allspp.utah.nonz <- FALSE
use.yourown.flagdesign <- FALSE

source("source/flags.for.models.in.bbms.R")

source("source/bbstanleadin.R")

if(use.flags.for.mainmodel){
write.csv(bb.stan, "..//output/bbstan_mainmodel.csv", row.names=FALSE) 
}

########################################
## within group center the predictors ##
## cheap code!
########################################
bb.stan.new <- bb.stan[1,]
bb.stan.new$force.wgc <- NA
bb.stan.new$photo.wgc <- NA
bb.stan.new$chill.wgc <- NA
bb.stan.new <- bb.stan.new[-1,]
for(complex in unique(bb.stan$complex.wname)){
    subby <- bb.stan[which(bb.stan$complex.wname %in% complex),]
    subby$force.wgc <- subby$force-mean(subby$force)
    subby$photo.wgc <- subby$photo-mean(subby$photo)
    subby$chill.wgc <- subby$chill-mean(subby$chill)
    bb.stan.new <- rbind(bb.stan.new, subby)
    }

bb.stan <- bb.stan.new

datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.wgc, 
                           force = force.wgc, 
                           photo = photo.wgc,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
)


#####################################
## Get studies with interactions  ##
## Lizzie took from countinxns. R ##
## Need to deal with species complex! ##
####################################

datesetIDincl <- read.csv("..//limitingcues/output/bbstan_mainmodel_countinxns_datasetIDs.csv")
datesetIDincl$datIDstudy <- paste(datesetIDincl$datasetID, datesetIDincl$study)
bb.stan$datIDstudy <- paste(bb.stan$datasetID, bb.stan$study)
bb.stan.sm <- bb.stan[which(bb.stan$datIDstudy %in% datesetIDincl$datIDstudy),]

datalist.bb.sm <- with(bb.stan.sm, 
                      list(y = resp, 
                           chill = chill.wgc, # should change
                           force = force.wgc, 
                           photo = photo.wgc,
                           sp = complex,
                           N = nrow(bb.stan.sm),
                           n_sp = length(unique(bb.stan.sm$complex))
                      )
)

######################################
## Overview of the model run below ##
######################################
# Main model:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
m2l.ni = stan('stan/nointer_2level.stan', data = datalist.bb,
               iter = 3500, warmup=2500)

check_all_diagnostics(m2l.ni)
# launch_shinystan(m2l.ni)

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]

ys<-datalist.bb$y
# posterior predictive checks....
if(FALSE){
y_pred <- extract(m2l.ni, 'y_ppc')

par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}


# Code if you want to save your models (do NOT push output to git)

if(use.flags.for.mainmodel){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfputah_z.Rda")
}

if (use.flags.for.spcomp.cp){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfpcp_z.Rda")
}

if (use.flags.for.allspp.utah){
  save(m2l.ni, file="stan/output/m2lni_allsppwcrop_utah_z.Rda")
}

if (use.flags.for.spcomp.utah.nonz){
  save(m2l.ni, file="stan/output/m2lni_spcompalltypesutah_nonz.Rda")
}

if (use.flags.for.spcomp.cp.nonz){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfpcp_nonz.Rda")
}

if (use.flags.for.allspp.utah.nonz){
  save(m2l.ni, file="stan/output/m2lni_allsppwcrop_utah_nonz.Rda")
}

#Other combinations of flags used at some point (but not in the main bb manuscript)
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==TRUE &
    use.chillports==FALSE){
  save(m2l.ni, file="stan/output/m2lni_spcompalltypesutah_z.Rda")
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==FALSE){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfputah_nonz.Rda")
}

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==FALSE){
  save(m2l.ni, file="stan/output/m2lni_allsppexprampfputah_z.Rda")
}

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==FALSE){
  save(m2l.ni, file="stan/output/m2lni_allsppexprampfputah_nonz.Rda")
}


if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
save(m2l.ni, file="stan/output/m2lni_spcompalltypescp_z.Rda")
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
save(m2l.ni, file="stan/output/m2lni_spcompwcropsexprampfpcp_z.Rda")
}


###### SIDE BAR #####
## Getting R2 etc. ##


if(FALSE){
modelhere <- m2l.ni # m2l.nistudy 
observed.here <- bb.stan$resp

mod.sum <- summary(modelhere)$summary
mod.sum[grep("mu_", rownames(mod.sum)),] 

# getting predicted values if needed
preds.mod.sum <- mod.sum[grep("yhat", rownames(mod.sum)),]

# Here's our method to calculate R sq
mod.R2 <- 1- sum((observed.here-preds.mod.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)

# Which seems correct! See  https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
rsq <- function (x, y) cor(x, y) ^ 2
rsq(observed.here, preds.mod.sum[,1])
summary(lm(preds.mod.sum[,1]~observed.here)) # Multiple R-squared

# spcomplex, no crops, group by sp>9: 0.6028132, 0.6086478 for sp>4 ... mult R2 around 0.58
#  0.5689911
}
####### END SIDE BAR #######

