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
library(brms)
library(rethinking)

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
use.flags.for.mainmodel <- TRUE#centered predictors, spcomplex with utah units. Fig 2 in main text of budburst ms
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
               iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))

check_all_diagnostics(m2l.ni)
# launch_shinystan(m2l.ni)

m2lni.sum <- summary(m2l.ni)$summary
coefs<-m2lni.sum[grep("mu_", rownames(m2lni.sum)),1]
coefs.lci<-m2lni.sum[grep("mu_", rownames(m2lni.sum)),4]
coefs.uci<-m2lni.sum[grep("mu_", rownames(m2lni.sum)),8]

m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]

ys<-datalist.bb$y
# posterior predictive checks....
if(FALSE){
y_pred <- extract(m2l.ni, 'y_ppc')

par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}


######################################
## BRMS model for correlated slopes/intercepts ##
######################################

m2l.ni.brms <- brm(y ~ force + photo + chill +#main effects
                     ((force + photo+ chill)|sp), #random effects
                   data = datalist.bb,
                   prior=prior(normal(0,50), class=Intercept) +
                     prior(normal(0,10), class=sd) +
                     prior(normal(0,50), class=b),
                   chains = 4, cores = 2,iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))
summary(m2l.ni.brms)
summary(m2l.ni.brms$cov_ranef)
        
coefscov<-fixef(m2l.ni.brms)[,1]
coefscov.lci<-fixef(m2l.ni.brms)[,3]
coefscov.uci<-fixef(m2l.ni.brms)[,4]

m2lnibrms.sum <- summary(m2l.ni.brms)

m2lni.cov.sum <- summary(m2l.ni2.cov)$summary
m2lni.cov.sum[grep("mu_", rownames(m2lni.cov.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]
#We want 2 tables for the reviewer response
#Table 1: Compares estimates for model with our main model and one with correlation between slopes/intercepts (the brms model)

#compare cov mod with original model
cov.comp<-cbind(coefs,coefs.lci,coefs.uci,coefscov,coefscov.lci,coefscov.uci)
row.names(cov.comp)<-c("$\\mu_{\\alpha}$","$\\mu_{forcing}$","$\\mu_{photoperiod}$","$\\mu_{chilling}$")

#colnames(cov.comp)<-c("mainmod","mainmodwcov")
write.csv(cov.comp,"..//output/covcomp.csv")

#Table 2: Shows the covarance matrix
varcor.brms<-VarCorr(m2l.ni.brms)$sp$cor[,1,]
write.csv(varcor.brms,"..//output/varcor.csv")
make_stancode(y ~ force + photo + chill +#main effects
                ((force + photo+ chill)|sp), #random effects
              data = datalist.bb,
              prior=prior(normal(0,50), class=Intercept) +
                prior(normal(0,10), class=sd) +
                prior(normal(0,50), class=b),
              chains = 2, cores = 2,iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))


#The below was my attempt at writing the code myself and it did not work...


#make datalist for model with cov matrix
nVars <-4
Imat <- diag(4,nVars)

datalist.bb <- with(bb.stan, 
                    list(y=resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex)),
                         nVars = nVars,
                         Imat = Imat
                    )
)

#run model with correlated slopes/ints
#nVars<-3#number of predictors
m2l.ni2.cov = stan('stan/nointer_2level_cov.stan', data = datalist.bb,
                  iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))

# Code if you want to save your models (do NOT push output to git)

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
save()
# spcomplex, no crops, group by sp>9: 0.6028132, 0.6086478 for sp>4 ... mult R2 around 0.58
#  0.5689911
}
####### END SIDE BAR #######

