# Started 30 March 2020
# By Cat 

# Fitting bb models with brms to compare coefficients with stan models

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


######################################
## Overview of the model run below ##
######################################
# Main model:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################

get_prior(y ~ force + photo + chill +#main effects
                ((force + photo+ chill)|sp), #random effects
              data = datalist.bb)

m2l.ni.brms <- brm(y ~ force + photo + chill +#main effects
                     ((force + photo+ chill)|sp), #random effects
                   data = datalist.bb,
                   prior=prior(normal(0,50), class=Intercept) +
                     prior(normal(0,10), class=sd) +
                     prior(normal(0,50), class=b),
                   chains = 2, cores = 2,iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))
summary(m2l.ni.brms)

make_stancode(y ~ force + photo + chill +#main effects
           ((force + photo+ chill)|sp), #random effects
         data = datalist.bb,
         prior=prior(normal(0,50), class=Intercept) +
           prior(normal(0,10), class=sd) +
           prior(normal(0,50), class=b),
         chains = 2, cores = 2,iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))



###### Now, let's check map2stan...
m2l.ni.map <- map2stan(
  alist(
    resp ~ dnorm(mu, sigma),
    mu <- a[sp] + bf[sp]*force + bp[sp]*photo + bc[sp]*chill,
    c(a, bf, bp, bc)[sp] ~ dmvnorm2(mu_species, sigma_species, Rho),
    mu_species ~ dnorm(0, 50),
    sigma ~ dnorm(0,10),
    sigma_species ~ dnorm(0,10),
    Rho ~ dlkjcorr(4)
  ),
  data=datalist.bb,
  chains=2, iter=4000, warmup=2000 
)

stancode(m2l.ni.map)



