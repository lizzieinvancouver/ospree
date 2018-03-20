# Started 15 March 2018 
# By Ailene 
# Fitting bb models with brms to compare coefficients with stan models
# housekeeping
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

library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("source/bbstanleadin.R")

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
# photoperiod, and where we have a response in days and total chilling. 

#Fit model with no interactions and a species random slopes effect
########## m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

m2l.ni.brms <- brm(resp ~ chill.cen+force.cen+photo.cen+#fixed effects
                     (chill.cen+force.cen+photo.cen|complex), #random effects
                   data = bb.stan, prior = c(set_prior("normal(0,50)", class = "b",coef="force.cen"),#priors
                             set_prior("normal(0,10)", class = "b",coef="photo.cen"),
                             set_prior("normal(0,50)", class = "b",coef="chill.cen")),
                   chains = 2) 

stancode(m2l.ni.brms)

#Stan model says: a: 71.6, f=-22.4, p=-7.1, c=-9.5, sigma: 23.33
summary(m2l.ni.brms)
#brms model says a: 73.52, f=-21.50, p=-9.61, c=-9.99; species sigma: 23.35
#brms model w/priors: a: 72.78, f=-22.10, p=-8.63, c=-9.87; species sigma: 23.35
#priors don't seem to affect much
marginal_effects(m2l.ni.brms, surface = TRUE)

#Fit model with interactions and a species random slopes effect
########## m2l.wi: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp) + fp(sp)
m2l.wi.brms <- brm(resp ~ chill.cent+force.cent +photo.cent+chill.cent:force.cent+chill.cent:photo.cent+force.cent:photo.cent + ((chill.cent+force.cent+photo.cent+chill.cent:force.cent+chill.cent:photo.cent+force.cent:photo.cent)|complex), data = bb.stan,
                   chains = 2, cores = 2)
#Stan model says: a:47.11 , f=1.77, p=13.5, c=-4.31, 
                  #f*c: -5.08, c*p: 0.15, f*p:-20.56 sigma sp: 25.03
summary(m2l.wi.brms)
#brms model says a: 57.88, f= -7.68, p=8.98, c= -10.79, 
                  #f*c: 2.35, c*p: -1.02 f*p: -16.94; sigma sp: 22.64
                  #1 divergent transition
marginal_effects(m2l.wi.brms, surface = TRUE)
