
## Load libraries
library(rstan)
library(shinystan)

## Set cores
options(mc.cores = 4) # Parallelize chains
## options(browser = "chromium")

## Set basic simulation parameters
n <- 10 # number of replicates per sp
nsp <- 20 # number of species
nstudy <- 5 # number of studies
nobs <- n * nsp * nstudy # number of observations

## Parameters to be inferred
param <- list(
    # Traits
    mu_species = 3,
    sigma_species = 1.1,
    sigma_study = .6,
    sigma_obs = .5,
    # Pheno
    sigma_phen = 5,
    mu_a_phen = 100,
    sigma_a_phen = 10,
    mu_a_forcing  = -.8,
    sigma_a_forcing  = 2,
    mu_b_forcing = -.6,
    sigma_b_forcing = .4)

## Generate forcing
forcing <- rnorm(n = nstudy, mean = 10, sd = 5)
## Generate study effects
study_offset <- rnorm(n = nstudy, mean = 0, sd = param[["sigma_study"]]) # mean must be 0
## Generate (true) trait values
traits <- rnorm(n = nsp, mean = param[["mu_species"]], sd = param[["sigma_species"]])
## Generate phenology intercept
phenology_a <- rnorm(n = nsp, mean = param[["mu_a_phen"]], sd = param[["sigma_a_phen"]])
## Generate forcing intercept
forcing_a <- rnorm(n = nsp, mean = param[["mu_a_forcing"]], sd = param[["sigma_a_forcing"]])
## Generate forcing coefficients
forcing_b <- rnorm(n = nsp, mean = param[["mu_b_forcing"]], sd = param[["sigma_b_forcing"]])

## Create data table
dat <- matrix(NA, ncol = 8, nrow = 0)
for(i in 1:nstudy){
    temp <- matrix(c(rep(i, n * nsp), # study id
                     rep(1:nsp, n), # species id
                     rep(forcing[i], n * nsp), # forcing
                     rep(study_offset[i], n * nsp), # offset from study
                     rep(traits, n), # true trait value
                     rep(phenology_a, n), # phenology interecept
                     rep(forcing_a, n), # forcing intercept
                     rep(forcing_b, n)), # forcing coefficient                  
                   ncol = 8,
                   byrow = FALSE)
    dat <- rbind(dat, temp)
}
## Add names
colnames(dat) <- c("studyID", "speciesID", "forcing", "study_offset", "trait1", "phenology_a", "forcing_a", "forcing_b")
    
## Obtain phenological response
phen.response <- rnorm(n = nrow(dat),
                       mean = dat[, "phenology_a"] + (dat[, "forcing_a"] + dat[, "forcing_b"] * dat[, "trait1"]) * dat[, "forcing"],
                       sd = param[["sigma_phen"]])
## Obtain trait observation
trait.observe <- rnorm(n = nrow(dat),
                       mean = dat[, "trait1"] + dat[, "study_offset"],
                       sd = param[["sigma_obs"]])

## Combine into data table
dat2 <- matrix(c(dat[, "studyID"],
                 dat[, "speciesID"],
                 dat[, "forcing"],
                 trait.observe,
                 phen.response),
               ncol = 5,
               byrow = FALSE)
