# Started 15 March 2018 
# By Ailene 
# Fitting bb models with brms to compare coefficients with stan models
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

#Fit model with no interactions and a species random slopes effect
########## m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

m2l.ni.brms <- brm(y ~ chill+force+photo+#fixed effects
                     ((chill+force+photo)|sp), #random effects
                   data = datalist.bb, 
                      chains = 2) 

stancode(m2l.ni.brms)

summary(m2l.ni.brms)
#brms model says a: 71.19, f=-22.10, p=--4.87 , c=-12.93; species sigma: 15.81 
#priors don't seem to affect much
#marginal_effects(m2l.ni.brms, surface = TRUE)
stanplot(m2l.ni.brms)
stanplot(m2l.ni.brms, pars = "^b_")

#Fit model with interactions and a species random slopes effect
########## m2l.wi: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp) + fp(sp)
#The model lizzie fit in arm:
#m2l.winsp.arm <- stan_glmer(resp ~ (force + photo + chill +
#                                      force*photo + force*chill + photo*chill) +
#                              ((force + photo + chill)|complex.wname), data = bb.stan)
#uncentered:
m2l.winsp.brms <- brm(resp ~ (force + photo + chill +#main effects
                             force*photo + force*chill + photo*chill)+ #interactions
                        ((force + photo + chill)|complex.wname), data = bb.stan,
                   chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))
summary(m2l.winsp.brms)
#brms model says a:94.77, f= -1.85, p=-1.41, c= -6.94, 
                  #f*c: 0.15, c*p: 0.10  f*p: 0.01; sigma sp: 15.37
#marginal_effects(m2l.wi.brms, surface = TRUE)
stanplot(m2l.winsp.brms, pars = "^b_")
launch_shinystan(m2l.winsp.brms)

#centered:
m2l.winsp.brms.cen <- brm(resp ~ (force.cen + photo.cen + chill.cen +#main effects
                                force.cen*photo.cen + force.cen*chill.cen + photo.cen*chill.cen)+ #interactions
                        ((force.cen + photo.cen + chill.cen)|complex.wname), data = bb.stan,
                      chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))
summary(m2l.winsp.brms.cen)
#brms model says a: 80.18, f= -23.28, p=-2.08, c= -28.11, 
#f*c: 9.34, c*p: 5.20  f*p: -9.77; sigma sp: 15.37
#marginal_effects(m2l.wi.brms, surface = TRUE)
stanplot(m2l.wisp.brms, pars = "^b_")
launch_shinystan(m2l.wisp.brms)


#Fit model with no interactions, a species random slopes effect, and a datasetID random intercept effect
########## a(sp)+ a(id) + f(sp) + p(sp) + c(sp) + 
#################################################################################
# real data on 2 level model (sp) with interactions and study ID on intercept
# Note the notation: M1_daysBBnointer_2level_studyint.stan: m2l.nistudy
#################################################################################
#Add study to model
datalist.bb.study.cen <- with(bb, 
                              list(y = resp, 
                                   chill = chill.cen, 
                                   force = force.cen, 
                                   photo = photo.cen,
                                   study = as.numeric(as.factor(bb.stan$datasetID)),
                                   n_study = length(unique(bb.stan$datasetID)), 
                                   sp = complex.wname,
                                   N = nrow(bb.stan),
                                   n_sp = length(unique(bb.stan$complex))
                              )
)
datalist.bb.study <- with(bb, 
                          list(y = resp, 
                               chill = chill, 
                               force = force, 
                               photo = photo,
                               study = as.numeric(as.factor(bb.stan$datasetID)),
                               n_study = length(unique(bb.stan$datasetID)), 
                               sp = complex.wname,
                               N = nrow(bb.stan),
                               n_sp = length(unique(bb.stan$complex))
                          )
)


#Fit model with NO interactions, a species random slopes effect, and a datasetID random intercept effect
########## a(sp) + f(sp) + p(sp) + c(sp)  + (datasetid)
m2l.nistudy.brms <- brm(y ~ chill+force +photo+
                       + ((chill+force+photo)|sp)+(1|study), 
                        data = datalist.bb.study,
                        chains = 2, cores = 2,control = list(max_treedepth = 12))
summary(m2l.nistudy.brms)

#Fit model with interactions, a species random slopes effect, and a datasetID random intercept effect
########## a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp) + fp(sp) + (datasetid)
m2l.wistudy.brms <- brm(y ~ chill+force +photo+
                    chill:force+chill:photo+force:photo + 
                    ((chill+force+photo+chill:force+chill:photo+force:photo)|sp)+(1|study), 
                    data = datalist.bb.study.cen,
                   chains = 2, cores = 2,control = list(max_treedepth = 12))
summary(m2l.wistudy.brms)
#brms model says a: 220.94, f= 104.09, p=455.34, c= -24.04, 
#f*c: -155.17, c*p: 153.17,  f*p: 150.58; sigma sp: 15.37

stanplot(m2l.wistudy.brms)
stanplot(m2l.wistudy.brms, pars = "^b_")
ranef(m2l.wistudy.brms)
launch_shinystan(m2l.wistudy.brms)

