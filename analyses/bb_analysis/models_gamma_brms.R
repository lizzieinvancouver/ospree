# Started 16 Nov 2018
# By Cat based off Ailene's models_brms.R code
# Fitting bb models with brms to compare coefficients with stan models using different distributions
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

library(brms)
library(rstan)
library(rstanarm)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

use.chillunits = FALSE # change to true for testing chill units
use.allspp = FALSE
use.allphoto = FALSE
source("source/bbstanleadin.R")

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
# photoperiod, and where we have a response in days and total chilling. 

#range(bb.stan$resp)
bb.stan.onecue$respg<-bb.stan.onecue$resp+0.001

gamma.test <- stan_glmer(bb ~ (force + photo + chill +#main effects
                                 force*photo + force*chill + photo*chill)+ #interactions
                        ((force + photo + chill + force*photo + force*chill + photo*chill)|sp),warmup=2500,iter=4000, data = test.gamma,
                      chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99), family=Gamma(link="identity"))

#save(gamma.arm, file="~/Documents/git/regionalrisk/gammaoutput.Rdata")


#uncentered gamma:
m2l.winsp.brms <- brm(respg ~ (force + photo + chill +#main effects
                                force*photo + force*chill + photo*chill)+ #interactions
                        ((force + photo + chill + force*photo + force*chill + photo*chill)|complex.wname),warmup=2500,iter=4000, data = bb.stan.onecue,
                      chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99), family=Gamma("identity"))
#summary(m2l.winsp.brms)
#stancode(m2l.winsp.brms)
#str(brms::make_standata(resp ~ (force + photo + chill +#main effects
 #                                 force*photo + force*chill + photo*chill)+ #interactions
 #                         ((force + photo + chill)|complex.wname),data=bb.stan))


#stancode(m2l.winsp.brms)
#marginal_effects(m2l.wi.brms, surface = TRUE)
#stanplot(m2l.winsp.brms, pars = "^b_")

#centered gamma:
#m2l.winsp.brms.cen<- brm(respg ~ (force.cen + photo.cen + chill.cen +#main effects
 #                                  force.cen*photo.cen + force.cen*chill.cen + photo.cen*chill.cen)+ #interactions
  #                        ((force.cen + photo.cen + chill.cen+force.cen*photo.cen + force.cen*chill.cen + photo.cen*chill.cen)|complex.wname), data = bb.stan.onecue,
   #                     chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99), family=Gamma("log"))
#summary(m2l.winsp.brms.cen)
#stanplot(m2l.winsp.brms.cen, pars = "^b_")

#zscored gamma:
m2l.winsp.brms.z <- brm(respg ~ (force.z + photo.z + chill.z +#main effects
                                  force.z*photo.z + force.z*chill.z + photo.z*chill.z)+ #interactions
                          ((force.z + photo.z + chill.z+force.z*photo.z + force.z*chill.z + photo.z*chill.z)|complex.wname), data = bb.stan.onecue,
                        chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99), family=Gamma("log"))
summary(m2l.winsp.brms.z)
#brms model says a: 3.31, f= -0.32, p=-0.13, c= -0.32, 
#f*c: -0.15, c*p: 0.10  f*p: -0.12; sigma sp: 2.93
## Converted:
# a: 27.385, f= 0.726, p=0.878, c= 0.726, 
#f*c: -0.15, c*p: 0.10  f*p: -0.12; sigma sp: 2.93

marginal_effects(m2l.winsp.brms.z, surface = TRUE)
stanplot(m2l.winsp.brms.z, pars = "^b_")




