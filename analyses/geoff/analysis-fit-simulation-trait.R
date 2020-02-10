## This file builds fake data (with trait effects!) for testing Stan models ##

# Source relevant libraries
library(rstan) # General Bayesian models

# Set seed
set.seed(2205)

# What are the model parameters?
## Trait coefficients are marked with ".trait"
model.parameters <- list(Intercept = 60,
                         force.coef = 22,
                         photo.coef = -2,
                         chill.coef = -1,
                         force.trait.coef = 1.23)
## Sigmas
model.sigmas <- list(sigma_y = 2,
                     sigma_a_sp = 5)

## Other simulation parameters
n.species <- 20
n.obs <- 200
### Generate trait value for each species
trait <- rnorm(n = n.species, mean = 0, sd = 1)
### Generate random slopes for each species
slopes <- rnorm(n = n.species, mean = model.parameters[["Intercept"]], sd = model.sigmas[["sigma_a_sp"]])

## Simulations
### Create empty matrix
testdata <- matrix(NA, ncol = length(model.parameters) + 1, nrow = n.species * n.obs)
### Fill it
for(i in 1:n.species){
    for(j in 1:n.obs){
        # Sample the environment
        force.temp <- rnorm(n = 1, mean = 0, sd = 1)
        photo.temp <- rnorm(n = 1, mean = 0, sd = 1)
        chill.temp <- rnorm(n = 1, mean = 0, sd = 1)
        resp.det <- slopes[i] +
            (model.parameters[["force.coef"]] + (model.parameters[["force.trait.coef"]] * trait[i])) * force.temp +
            model.parameters[["photo.coef"]] * photo.temp +
            model.parameters[["chill.coef"]] * chill.temp
        testdata[(j + (n.obs * (i - 1))), ] <- c(rnorm(n = 1, mean = resp.det, sd = model.sigmas[["sigma_y"]]), # response
                                                          i, # species
                                                          trait[i], # trait
                                                          force.temp, # forcing
                                                          photo.temp, # photoperiod
                                                          chill.temp) # chilling
    }
}

## Fit model to data
### Create compatible list
testdatastan <- list(y = testdata[, 1],
                     sp = testdata[, 2],
                     trait = testdata[, 3],
                     force = testdata[, 4],
                     photo = testdata[, 5],
                     chill = testdata[, 6],
                     N = n.obs * n.species,
                     n_sp = n.species)
### Fit
stanmodel1 <- stan("stan/intpool_trait.stan", data = testdatastan, iter = 5000, warmup = 2500, chains = 4, cores = 4, verbose = TRUE)

## Summarize fit
summary(stanmodel1, pars = c("mu_a_sp", "b_force", "b_force_trait", "b_photo", "b_chill", "sigma_a_sp", "sigma_y"))$summary

## Compare to model parameters
t(model.parameters)
t(model.sigmas)
## Not bad
