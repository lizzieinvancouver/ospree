#Simulated code by Geoff to simulate trait data for the join model. Faith then tries to fit the joint model and see what we get back.  

setwd("/home/faith/Documents/github/ospree/ospree/analyses/jointmodel/")

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
    b_forcing = -.6)

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
## Generate forcing coefficient
forcing_b <- param[["b_forcing"]]

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
                     rep(forcing_b, n * nsp)), # forcing coefficient                  
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


## make into a df so I can use column names 
dat2df <- as.data.frame(dat2)
names(dat2df) <- c("studyID", "speciesID", "forcing", "trait.observe", "phen.response")

#Try to fit the joint model 
#-----------------------------------------


traitstanpheno <- list(
                        yTraiti = trait.observe, 
                        N = length(trait.observe), # sample size for trait data is the same as phenology data in this simulation 
                        n_spec = nsp, 
                        species = dat2df$speciesID, 
                        study = dat2df$studyID, 
                        n_study = nstudy, 
                        yPhenoi = dat2df$phen.response, 
                        Nph = length(dat2df$trait.observe), # sample size for trait data is the same as phenology data in this simulation  
                        forcingi = dat2df$forcing,
                        species2 = dat2df$speciesID) # number of species is the same for traits and phenology data.  


trialFit <- stan(file = "stan/stan_joint_newPriors.stan", data = traitstanpheno, warmup = 3000, iter = 4000,
    chains = 4, cores = 4,  control=list(max_treedepth = 15)) 


posterior <- extract(trialFit)

pdf("JointModel_GeoffSimFit.pdf")

#model 1
plot(density(posterior$sigmaTrait_y ), main = "sigmaTrait_y/sigma_obs") # sigma_obs 0.4
abline(v =  param$sigma_obs, col="red", lwd=3, lty=2)

plot(density(posterior$sigma_sp ), main = "sigma_sp/sigma_species") #sigma_species 1.1
abline(v =  param$sigma_species, col="red", lwd=3, lty=2)

plot(density(posterior$mu_g), main = "mu_g/mu_species") # mu_species 3
abline(v =  param$mu_species, col="red", lwd=3, lty=2)

plot(density(posterior$muSp ), xlim = c(-4,7), ylim = c(0,0.5), main = "muSp/traits") #trait1
par(new=TRUE)
plot(density(traits), col = "red", lwd=3, lty=2, xlim = c(-4,7), ylim = c(0,0.5),xlab = "", ylab = "", main = "")

plot(density(posterior$sigma_stdy), main = "sigma_stdy/sigma_study") #sigma_study 0.6.
abline(v =  param$sigma_study, col="red", lwd=3, lty=2)

plot(density(posterior$muStdy ), xlim = c(-3,4), ylim = c(0,0.6), main = "muStdy/study_offset") #study_offset
par(new=TRUE)
plot(density(study_offset), col = "red", lwd=3, lty=2, xlim = c(-3,4), ylim = c(0,0.6), xlab = "", ylab = "", main = "")



#model 2
plot(density(posterior$sigmapheno_y ), xlim = c(4.4,5.4), ylim = c(0,4), main = "sigmapheno_y/sigma_phen") #  sigma_phen 5
abline(v =  param$sigma_phen, col="red", lwd=3, lty=2)

plot(density(posterior$sigmaForceSp ), xlim = c(1,4), ylim = c(0,1.1), main = "sigmaForceSp/sigma_a_forcing") # sigma_a_forcing 2
abline(v =  param$sigma_a_forcing, col="red", lwd=3, lty=2)

plot(density(posterior$muForceSp ), xlim = c(-6,4), ylim = c(0,0.33), main = "muForceSp/mu_a_forcing") #mu_a_forcing -0.8 
abline(v =  param$mu_a_forcing, col="red", lwd=3, lty=2)

plot(density(posterior$alphaForcingSp), xlim = c(-10,10), ylim = c(0,0.25), main = "alphaForcingSp/forcing_a") # //forcing_a
par(new=TRUE)
plot(density(forcing_a), col="red", lwd=3, lty=2, xlim = c(-10,10), ylim = c(0,0.25), main = "", xlab = "", ylab = "")

plot(density(posterior$sigmaPhenoSp), xlim = c(4.5,20), ylim = c(0,0.3), main = "sigmaPhenoSp/sigma_a_phen") # sigma_a_phen 10
abline(v =  param$sigma_a_phen, col="red", lwd=3, lty=2)


plot(density(posterior$muPhenoSp), xlim = c(87,110), ylim = c(0,0.2), main = "muPhenoSp/mu_a_phen") # mu_a_phen 100
abline(v =  param$mu_a_phen, col="red", lwd=3, lty=2)


plot(density(posterior$sigmaPhenoSp), xlim = c(5,16), ylim = c(0,0.3), main = "sigmaPhenoSp/sigma_a_phen") # sigma_a_phen 10
abline(v =  param$sigma_a_phen, col="red", lwd=3, lty=2)


plot(density(posterior$alphaPhenoSp), xlim = c(70,125), ylim = c(0,0.05), main = "alphaPhenoSp/phenology_a") # //phenology_a
par(new=TRUE)
plot(density(phenology_a), col="red", lwd=3, lty=2, xlim = c(70,125), ylim = c(0,0.05), main = "", xlab = "", ylab = "")

plot(density(posterior$betaTraitxPheno), xlim = c(-3,1), ylim = c(0,1.3), main = "betaTraitxPheno/forcing_b") # // b_forcing. Geoff has one for each species though.  
par(new=TRUE)
plot(density(forcing_b), col="red", lwd=3, lty=2, xlim = c(-3,1), ylim = c(0,1.3), main = "", xlab = "", ylab = "")

dev.off()
