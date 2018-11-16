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
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("source/bbstanleadin.R")

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
# photoperiod, and where we have a response in days and total chilling. 

#range(bb.stan$resp)
bb.stan$respg<-bb.stan$resp+0.001

#uncentered gamma:
#m2l.winsp.brms <- brm(respg ~ (force + photo + chill +#main effects
 #                               force*photo + force*chill + photo*chill)+ #interactions
 #                       ((force + photo + chill + force*photo + force*chill + photo*chill)|complex.wname),warmup=2500,iter=4000, data = bb.stan,
  #                    chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99), family=Gamma("log"))
#summary(m2l.winsp.brms)
#stancode(m2l.winsp.brms)
#str(brms::make_standata(resp ~ (force + photo + chill +#main effects
 #                                 force*photo + force*chill + photo*chill)+ #interactions
 #                         ((force + photo + chill)|complex.wname),data=bb.stan))


#brms model says a:94.77, f= -1.85, p=-1.41, c= -6.94, 
#f*c: 0.15, c*p: 0.10  f*p: 0.01; sigma sp: 15.37

#stancode(m2l.winsp.brms)
#marginal_effects(m2l.wi.brms, surface = TRUE)
#stanplot(m2l.winsp.brms, pars = "^b_")

#zscored gamma:
m2l.winsp.brms.z <- brm(respg ~ (force.z + photo.z + chill.z +#main effects
                                  force.z*photo.z + force.z*chill.z + photo.z*chill.z)+ #interactions
                          ((force.z + photo.z + chill.z+force.z*photo.z + force.z*chill.z + photo.z*chill.z)|complex.wname), data = bb.stan,
                        chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99), family=Gamma("log"))
summary(m2l.winsp.brms.z)
#brms model says a: 28.90, f= -4.63, p=-4.43, c= -9.64, 
#f*c: 2.43, c*p: 2.16  f*p: -0.26; sigma sp: 14.60 
#marginal_effects(m2l.wi.brms, surface = TRUE)
stanplot(m2l.winsp.brms.z, pars = "^b_")




