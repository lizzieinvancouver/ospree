
## Load libraries
library(rstan)
require(shinystan)

## Set number of cores
options(mc.cores = 4)

## Set seed
set.seed(202109)

## Set parameters
param <- list(
    Nrep = 20, # rep per trait
    Nstudy = 15, # number of studies with traits
    Nspp = 15, # number of species with traits
    trait_mu_grand = 20,
    trait_sigma_sp = 4,
    trait_sigma_study = 2,
    trait_sigma_traity = 3)

## Generate species and study offsets
mu_sp <- rnorm(n = param[["Nspp"]], mean = 0, sd = param[["trait_sigma_sp"]])
mu_study <- rnorm(n = param[["Nstudy"]], mean = 0, sd = param[["trait_sigma_study"]])

## Make empty table
yTraittable <- data.frame(Species = c(),
                          Study = c(),
                          Replicate = c(),
                          mu_grand = c(),
                          mu_sp = c(),
                          mu_study = c())
## Fill table with parameters
for(i in 1:param[["Nspp"]]){
    for(j in 1:param[["Nstudy"]]){        
        temp <- data.frame(Species = i,
                           Study = j,
                           Replicate = 1:param[["Nrep"]],
                           mu_grand = param[["trait_mu_grand"]],
                           mu_sp = mu_sp[i],
                           mu_study = mu_study[j])
        yTraittable <- rbind(yTraittable, temp)
    }
}

## Generate trait observation using parameters
yTraittable$yTraiti <- rnorm(n = nrow(yTraittable),
                             mean = yTraittable$mu_grand + yTraittable$mu_sp + yTraittable$mu_study,
                             sd = param[["trait_sigma_traity"]])

## Trait only stan model ###########################################################
trait_data <- list(yTraiti = yTraittable$yTraiti,
                   N = nrow(yTraittable),
                   n_spec = param[["Nspp"]], 
                   species = yTraittable$Species,
                   n_study = param[["Nstudy"]],
                   study = yTraittable$Study,
                   prior_mu_grand_mu = param[["trait_mu_grand"]],
                   prior_mu_grand_sigma = 5,
                   prior_sigma_sp_mu = param[["trait_sigma_sp"]],
                   prior_sigma_sp_sigma = 5,
                   prior_sigma_study_mu = param[["trait_sigma_study"]],
                   prior_sigma_study_sigma = 5,
                   prior_sigma_traity_mu = param[["trait_sigma_traity"]],
                   prior_sigma_traity_sigma = 5) 

mdl.traitonly <- stan("trait_only2.stan",
                      data = trait_data,
                      iter = 2000,
                      warmup = 1000,
                      chains = 4,
                      include = FALSE, pars = c("y_hat"),
                      seed = 202109)

summary(mdl.traitonly, pars = c("mu_grand"))$summary
## True value
param[["trait_mu_grand"]]

summary(mdl.traitonly, pars = c("sigma_sp"))$summary
## True value
param[["trait_sigma_sp"]]

summary(mdl.traitonly, pars = c("sigma_study"))$summary
## True value
param[["trait_sigma_study"]]

summary(mdl.traitonly, pars = c("sigma_traity"))$summary
## True value
param[["trait_sigma_traity"]]

## summary(mdl.traitonly, pars = c("mu_grand", "sigma_sp", "sigma_study", "sigma_traity"))$summary
## t(param)
