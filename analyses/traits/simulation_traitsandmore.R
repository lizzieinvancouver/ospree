
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
    trait_sigma_traity = 3,
    phenology_muForce = 2,
    phenology_muChill = -2.1,
    phenology_muPhoto = 0.45,
    phenology_sigmaForce = .5,
    phenology_sigmaChill = .6,
    phenology_sigmaPhoto = .7,
    phenology_betaTraitForce = .34,
    phenology_betaTraitChill = .25,
    phenology_betaTraitPhoto = -.5,
    phenology_muAlpha = 10,
    phenology_sigmaAlpha = 1,
    phenology_sigmaPheno = 2)

## Generate species and study offsets (traits)
mu_sp <- rnorm(n = param[["Nspp"]], mean = 0, sd = param[["trait_sigma_sp"]])
mu_study <- rnorm(n = param[["Nstudy"]], mean = 0, sd = param[["trait_sigma_study"]])

## Generate species and study offsets (phenology)
alphaPheno <- rnorm(n = param[["Nspp"]], mean = param[["phenology_muAlpha"]], sd = param[["phenology_sigmaAlpha"]])
alphaForce <- rnorm(n = param[["Nspp"]], mean = param[["phenology_muForce"]], sd = param[["phenology_sigmaForce"]])
alphaChill <- rnorm(n = param[["Nspp"]], mean = param[["phenology_muChill"]], sd = param[["phenology_sigmaChill"]])
alphaPhoto <- rnorm(n = param[["Nspp"]], mean = param[["phenology_muPhoto"]], sd = param[["phenology_sigmaPhoto"]])

## Make empty table
yTable <- data.frame(Species = c(),
                     Study = c(),
                     Replicate = c(),
                     mu_grand = c(),
                     mu_sp = c(),
                     mu_study = c(),
                     alphaPheno = c(),
                     alphaForce = c(),
                     alphaChill = c(),
                     alphaPhoto = c())
## Fill table with parameters
for(i in 1:param[["Nspp"]]){
    for(j in 1:param[["Nstudy"]]){        
        temp <- data.frame(Species = i,
                           Study = j,
                           Replicate = 1:param[["Nrep"]],
                           mu_grand = param[["trait_mu_grand"]],
                           mu_sp = mu_sp[i],
                           mu_study = mu_study[j],
                           alphaPheno = alphaPheno[i],
                           alphaForce = alphaForce[i],
                           alphaChill = alphaChill[i],
                           alphaPhoto = alphaPhoto[i])
        yTable <- rbind(yTable, temp)
    }
}

## Calculate latent trait values
yTable$trait_latent <- yTable$mu_grand + yTable$mu_sp + yTable$mu_study
## Generate trait observation using parameters
yTable$yTraiti <- rnorm(n = nrow(yTable),
                        mean = yTable$trait_latent,
                        sd = param[["trait_sigma_traity"]])

## Calculate response parameters
yTable$betaForce <- yTable$alphaForce + param[["phenology_betaTraitForce"]] * yTable$trait_latent
yTable$betaChill <- yTable$alphaChill + param[["phenology_betaTraitChill"]] * yTable$trait_latent
yTable$betaPhoto <- yTable$alphaPhoto + param[["phenology_betaTraitPhoto"]] * yTable$trait_latent

## Generate forcing, chilling, photo
yTable$forcei <- rnorm(n = nrow(yTable), mean = 0, sd = .5)
yTable$chilli <- rnorm(n = nrow(yTable), mean = 0, sd = .5)
yTable$photoi <- rnorm(n = nrow(yTable), mean = 0, sd = .5)

## Generate phenology response
yTable$yPhenologyi <- rnorm(n = nrow(yTable),
                            mean = yTable$alphaPheno + yTable$betaForce * yTable$forcei + yTable$betaChill * yTable$chilli + yTable$betaPhoto * yTable$photoi,
                            sd = param[["phenology_sigmaPheno"]])


## Prepare all data for Stan
all_data <- list(yTraiti = yTable$yTraiti,
                   N = nrow(yTable),
                   n_spec = param[["Nspp"]], 
                   trait_species = yTable$Species,
                   n_study = param[["Nstudy"]],
                   study = yTable$Study,
                   prior_mu_grand_mu = param[["trait_mu_grand"]],
                   prior_mu_grand_sigma = 5,
                   prior_sigma_sp_mu = param[["trait_sigma_sp"]],
                   prior_sigma_sp_sigma = 5,
                   prior_sigma_study_mu = param[["trait_sigma_study"]],
                   prior_sigma_study_sigma = 5,
                   prior_sigma_traity_mu = param[["trait_sigma_traity"]],
                   prior_sigma_traity_sigma = 5,
                   ## Phenology
                   phenology_species = yTable$Species,
                   Nph = nrow(yTable),
                   yPhenoi = yTable$yPhenologyi,
                   forcei = yTable$forcei,
                   chilli = yTable$chilli,
                   photoi = yTable$photoi,
                   prior_muForceSp_mu = param[["phenology_muForce"]],
                   prior_muForceSp_sigma = .5,
                   prior_muChillSp_mu = param[["phenology_muChill"]],
                   prior_muChillSp_sigma = 5,
                   prior_muPhotoSp_mu = param[["phenology_muPhoto"]],
                   prior_muPhotoSp_sigma = .1,
                   prior_muPhenoSp_mu = param[["phenology_muAlpha"]],
                   prior_muPhenoSp_sigma = 2,
                   prior_sigmaForceSp_mu = param[["phenology_sigmaForce"]],
                   prior_sigmaForceSp_sigma = 0.1,
                   prior_sigmaChillSp_mu = param[["phenology_sigmaChill"]],
                   prior_sigmaChillSp_sigma = 0.05,
                   prior_sigmaPhotoSp_mu = param[["phenology_sigmaPhoto"]],
                   prior_sigmaPhotoSp_sigma = 0.1,
                   prior_sigmaPhenoSp_mu = param[["phenology_sigmaAlpha"]],
                   prior_sigmaPhenoSp_sigma = .1,
                   prior_betaTraitxForce_mu = param[["phenology_betaTraitForce"]],
                   prior_betaTraitxForce_sigma = 0.1,
                   prior_betaTraitxChill_mu = param[["phenology_betaTraitChill"]],
                   prior_betaTraitxChill_sigma = 0.1,
                   prior_betaTraitxPhoto_mu = param[["phenology_betaTraitPhoto"]],
                   prior_betaTraitxPhoto_sigma = .1,
                   prior_sigmaphenoy_mu = param[["phenology_sigmaPheno"]],
                   prior_sigmaphenoy_sigma = 0.5
                   ) 

mdl.traitphen <- stan("stan/phenology_combined.stan",
                      data = all_data,
                      iter = 2000,
                      warmup = 1000,
                      chains = 4,
                      include = FALSE, pars = c("y_hat"),
                      seed = 202109)

summary(mdl.traitphen, pars = c("mu_grand"))$summary
## True value
param[["trait_mu_grand"]]

summary(mdl.traitphen)$summary[, "n_eff"]

summary(mdl.traitphen, pars = c("betaTraitxForce"))$summary
## True value
param[["phenology_betaTraitForce"]]







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
