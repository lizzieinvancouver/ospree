# Started 15 March 2018 
# By Ailene 
# Updated a tiny bit by Dan 19 June 2018
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
library(mice)#for imputation
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("source/bbstanleadin.R")

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
# photoperiod, and where we have a response in days and total chilling. 

#Fit model with no interactions and a species random slopes effect
########## m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

#m2l.ni.brms <- brm(y ~ chill+force+photo+#fixed effects
#                     ((chill+force+photo)|sp), #random effects
#                   data = datalist.bb, 
#                      chains = 2) 

#stancode(m2l.ni.brms)

#summary(m2l.ni.brms)
#brms model says a: 71.19, f=-22.10, p=--4.87 , c=-12.93; species sigma: 15.81 
#priors don't seem to affect much
#marginal_effects(m2l.ni.brms, surface = TRUE)
#stanplot(m2l.ni.brms)
#stanplot(m2l.ni.brms, pars = "^b_")
#zscored:
#m2l.nin.brms.z <- brm(resp ~ force.z + photo.z + chill.z +#main effects
                    ((force.z + photo.z + chill.z)|complex.wname), data = bb.stan,
                    chains = 2, cores = 2)
#summary(m2l.nin.brms.z)
#brms model says a:28.41, f= -4.55 , p=--3.83, c= -8.81, 

#Fit model with interactions and a species random slopes effect
########## m2l.wi: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp) + fp(sp)
#The model lizzie fit in arm:
#m2l.winsp.arm <- stan_glmer(resp ~ (force + photo + chill +
#                                      force*photo + force*chill + photo*chill) +
#                              ((force + photo + chill)|complex.wname), data = bb.stan)
#uncentered:
m2l.winsp.brms <- brm(resp ~ (force + photo + chill +#main effects
                             force*photo + force*chill + photo*chill)+ #interactions
                        ((force + photo + chill + force*photo + force*chill + photo*chil)|complex.wname),warmup=2500,iter=4000, data = bb.stan,
                   chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))
summary(m2l.winsp.brms)
stancode(m2l.winsp.brms)
str(brms::make_standata(resp ~ (force + photo + chill +#main effects
                                  force*photo + force*chill + photo*chill)+ #interactions
                          ((force + photo + chill)|complex.wname),data=bb.stan))


    #brms model says a:94.77, f= -1.85, p=-1.41, c= -6.94, 
                  #f*c: 0.15, c*p: 0.10  f*p: 0.01; sigma sp: 15.37

# Dan changed warm up and iters to match stan models:
#a 94.7, f:-1.83,p:-1.4,c:-6.97. f*p0.01,c*p=.11, f*c=15 sigma 14.59
## Cat removed apples:
# a: 95.50; f: -1.92; p: -1.46; c: -7.02; fp: 0.01; fc: 0.15; pc: 0.11; sigma: 14.96
#stancode(m2l.winsp.brms)
#marginal_effects(m2l.wi.brms, surface = TRUE)
stanplot(m2l.winsp.brms, pars = "^b_")
launch_shinystan(m2l.winsp.brms)

#centered:
m2l.winsp.brms.cen <- brm(resp ~ (force.cen + photo.cen + chill.cen +#main effects
                                force.cen*photo.cen + force.cen*chill.cen + photo.cen*chill.cen)+ #interactions
                        ((force.cen + photo.cen + chill.cen + force.cen*photo.cen + force.cen*chill.cen + photo.cen*chill.cen)|complex.wname), data = bb.stan,
                      chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))
summary(m2l.winsp.brms.cen)
#zscored:
m2l.winsp.brms.z <- brm(resp ~ (force.z + photo.z + chill.z +#main effects
                                    force.z*photo.z + force.z*chill.z + photo.z*chill.z)+ #interactions
                            ((force.z + photo.z + chill.z+force.z*photo.z + force.z*chill.z + photo.z*chill.z)|complex.wname), data = bb.stan,
                          chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))
summary(m2l.winsp.brms.z)
#brms model says a: 28.90, f= -4.63, p=-4.43, c= -9.64, 
#f*c: 2.43, c*p: 2.16  f*p: -0.26; sigma sp: 14.60 
#marginal_effects(m2l.wi.brms, surface = TRUE)
stanplot(m2l.wisp.brms, pars = "^b_")
launch_shinystan(m2l.wisp.brms)


########################
######Imputation########
########################
#same model with imputation, using brms imputation during model fitting

bform <- bf(bmi | mi() ~ age * mi(chl)) +
  bf(chl | mi() ~ age) + set_rescor(FALSE)
summary(m2l.wistudy.brms.zimp)

#test for picea 
picea<-bb.stan[bb.stan$complex.wname=="Picea_abies",]
pcmod<-lm(resp ~ force.cen + photo.cen + chill.cen +force.cen:photo.cen + force.cen*chill.cen + photo.cen*chill.cen, data=picea)
 summary(pcmod)         
 
#####Old Code that includes study
 ####We decided not to include study in model because it was making estimates unstable. Thus the below code is not necessary.
 #Fit model with no interactions, a species random slopes effect, and a datasetID random intercept effect
 ########## a(sp)+ a(id) + f(sp) + p(sp) + c(sp) + 
 #################################################################################
 # real data on 2 level model (sp) with interactions and study ID on intercept
 # Note the notation: M1_daysBBnointer_2level_studyint.stan: m2l.nistudy
 #################################################################################
 #Add study to model
 #datalist.bb.study.cen <- with(bb, 
 #                              list(y = resp, 
 #                                   chill = chill.cen, 
 #                                   force = force.cen, 
 #                                   photo = photo.cen,
 #                                   study = as.numeric(as.factor(bb.stan$datasetID)),
 #                                   n_study = length(unique(bb.stan$datasetID)), 
 #                                  sp = complex,
 #                                   N = nrow(bb.stan),
 #                                   n_sp = length(unique(bb.stan$complex))
 #                              )
 #)
 #datalist.bb.study <- with(bb, 
 #                          list(y = resp, 
 #                               chill = chill, 
 #                               force = force, 
 #                               photo = photo,
 #                               study = as.numeric(as.factor(bb.stan$datasetID)),
 #                               n_study = length(unique(bb.stan$datasetID)), 
 #                               sp = complex,
 #                               N = nrow(bb.stan),
 #                               n_sp = length(unique(bb.stan$complex))
 #                          )
 #)
 
 
 #Fit model with NO interactions, a species random slopes effect, and a datasetID random intercept effect
 ########## a(sp) + f(sp) + p(sp) + c(sp)  + (datasetid)
 m2l.nistudy.brms <- brm(y ~ chill+force +photo+
                           + ((chill+force+photo)|sp)+(1|study), 
                         data = datalist.bb.study,
                         chains = 2, cores = 2,control = list(max_treedepth = 12))
 summary(m2l.nistudy.brms)
 
 m2l.nistudy.brms.z <- brm(resp ~ force.z + photo.z + chill.z +#main effects
                             ((force.z + photo.z + chill.z)|complex.wname)+(1|datasetID), data = bb.stan,
                           chains = 2, cores = 2)
 summary(m2l.nistudy.brms.z)
 
 #Fit model with interactions, a species random slopes effect, and a datasetID random intercept effect
 ########## a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp) + fp(sp) + (datasetid)
 m2l.wistudy.brms <- brm(y ~ chill+force +photo+
                           chill:force+chill:photo+force:photo + 
                           ((chill+force+photo)|sp)+(1|study), 
                         data = datalist.bb.study.cen,
                         chains = 2, cores = 2,control = list(max_treedepth = 12))
 summary(m2l.wistudy.brms)
 #brms model says a: 220.94, f= 104.09, p=455.34, c= -24.04, 
 #f*c: -155.17, c*p: 153.17,  f*p: 150.58; sigma sp: 15.37
 
 stanplot(m2l.wistudy.brms)
 stanplot(m2l.wistudy.brms, pars = "^b_")
 ranef(m2l.wistudy.brms)
 launch_shinystan(m2l.wistudy.brms)
 
 #zscored
 m2l.wistudy.brms.z <- brm(resp ~ chill.z+force.z +photo.z+
                             chill.z:force.z+chill.z:photo.z+force.z:photo.z + 
                             ((chill.z+force.z+photo.z)|complex)+(1|datasetID), 
                           data = bb.stan,
                           chains = 2, cores = 2)
 summary(m2l.wistudy.brms.z)
 ranef(m2l.wistudy.brms.z)
 stanplot(m2l.wistudy.brms.z, pars = "^b_")
 stanplot(m2l.wistudy.brms.z)
 stanplot(m2l.wistudy.brms.z, type = "hist")
 
 #brms model says a: 220.94, f= 104.09, p=455.34, c= -24.04, 
 #f*c: -155.17, c*p: 153.17,  f*p: 150.58; sigma sp: 15.37
 