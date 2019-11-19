## This file builds fake data for testing Stan models for OSPREE budburst analysis ##

# Source relevant libraries
library(lme4) # Mixed effects models (Frequentist)
library(rstanarm) # Mixed effects models (Bayesian)
library(rstan) # General Bayesian models

# Source simulate function
source("func/simulate-linear.R")$value

# Set seed
set.seed(73)

# Set parameters
## Generate environmental variables (10,000 of each should be enough)
env <- list(force.env = rnorm(10000, 0, 2),
            photo.env = rnorm(10000, 0, 2),
            chill.env = rnorm(10000, 0, 5))

# What are the model parameters?
## Interaction parameters must contain ".X." and same names as their additive versions
model.parameters <- list(Intercept = 60,
                         force.coef = -3,
                         photo.coef = -2,
                         chill.coef = -1,
                         force.X.chill.coef = -1,
                         force.X.photo.coef = 0.5,
                         chill.X.photo.coef = -1.5)

# Simulate data
## Random intercept only [Species]
## Default parameters; 5 species, 200 observations per species
testdata1 <- simulate.linear(model.parameters, # parameters, already generated
                           model.random = c("Intercept"), # Which effects are random? Entries must correspond to parameters
                           env, # environment, already generated
                           response.distribution = c("rnorm"), # What is the name of the function that generates the distribution of the response variable? (rnorm, rpois supported)
                           n.species = 5, # number of species to simulate
                           n.observations = 200) # number of observations per species
head(testdata1)

# Fit models to data

## lme4 mixed effects model
### Fit
model1 <- lmer(Response ~ (force.env+photo.env+chill.env) ^ 2 + (1 | Species), data = testdata1)
### Summarize fit
summary(model1)
### Compare to true parameters (not necessarily same order)
t(model.parameters)

## rstanarm
### Fit
model2 <- stan_lmer(Response ~ (force.env+photo.env+chill.env) ^ 2 + (1|Species),
                    data = testdata1,
                    seed = 73,
                    iter = 5000,
                    chains = 4,
                    cores = 4,
                    prior_intercept = normal(60, scale = 10))
### Summarize fit
summary(model2)
### Compare to true parameters (not necessarily same order)
t(model.parameters)

## Pure stan
### Create compatible list
testdatastan <- list(y = testdata1$Response,
                     chill = testdata1$chill.env,
                     force = testdata1$force.env,
                     photo = testdata1$photo.env,
                     sp = testdata1$Species,
                     N = nrow(testdata1),
                     n_sp = length(unique(testdata1$Species)))
### Fit
stanmodel1 <- stan("stan/interac_intpool.stan", data = testdatastan, iter = 5000, warmup = 2500, chains = 4, verbose = TRUE)
### Summarize fit
summary(stanmodel1, pars = c("mu_a_sp", "b_force", "b_photo", "b_chill", "b_cf", "b_cp", "b_fp", "sigma_a_sp", "sigma_y"))$summary
### Compare to true parameters (not necessarily same order)
t(model.parameters)

# Simulate less data
## Default parameters; 3 species, 10 observations per species
testdata2 <- simulate.linear(model.parameters, # parameters, already generated
                           model.random = c("Intercept"), # Which effects are random? Entries must correspond to parameters
                           env, # environment, already generated
                           response.distribution = c("rnorm"), # What is the name of the function that generates the distribution of the response variable? (rnorm, rpois supported)
                           n.species = 3, # number of species to simulate
                           n.observations = 10) # number of observations per species
head(testdata2)
## Make data unbalanced and reduce sample size further
testdata2  <- testdata2[sample(1:nrow(testdata2), size = 8, replace = FALSE), ]
testdata2

# Fit models to data

## lme4 mixed effects model
### Fit
model2 <- lmer(Response ~ (force.env+photo.env+chill.env) ^ 2 + (1 | Species), data = testdata2)
### Summarize fit
summary(model2)
### Compare to true parameters (not necessarily same order)
t(model.parameters)

## Pure stan
### Create compatible list
testdatastan <- list(y = testdata2$Response,
                     chill = testdata2$chill.env,
                     force = testdata2$force.env,
                     photo = testdata2$photo.env,
                     sp = testdata2$Species,
                     N = nrow(testdata2),
                     n_sp = length(unique(testdata2$Species)))
### Fit
stanmodel2 <- stan("stan/interac_intpool.stan", data = testdatastan, iter = 5000, warmup = 2500, chains = 4, verbose = TRUE, control = list(adapt_delta = 0.99))
### Summarize fit
summary(stanmodel2, pars = c("mu_a_sp", "b_force", "b_photo", "b_chill", "b_cf", "b_cp", "b_fp", "sigma_a_sp", "sigma_y"))$summary
### Compare to true parameters (not necessarily same order)
t(model.parameters)
