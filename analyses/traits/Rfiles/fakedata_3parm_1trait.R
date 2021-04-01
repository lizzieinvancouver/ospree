# Date started: March 31, 2021
# The purpose of this code is to generate test data for the traitors model with all three climate parameters and a single trait, here we start with height:

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/ospree/analyses/traits")
} else{
  setwd("/home/faith/Documents/github/ospree/ospree/analyses/jointmodel/") 
}

library(rstan)

rm(list=ls()) 
options(stringsAsFactors = FALSE)

Nstudy <- 10 # number of studies w/ traits
Nspp <- 5 # number of species with traits

# First making a data frame for the test trait data
trtrep <- 10 # rep per trait
Ntrt <- Nspp * trtrep * Nstudy

#make a dataframe for height
trt.dat <- data.frame(matrix(NA, Ntrt, 3))
names(trt.dat) <- c("species","study", "rep")
trt.dat$rep <- c(1:Ntrt)
trt.dat$species <- rep(c(1:Nspp), each = trtrep)
trt.dat$study <- rep(c(1:Nstudy), each = trtrep)

mu.trt <- 20
sigma.trt <- 10
ht <- rnorm(Nspp, mu.trt, sigma.trt)

#adding ht data for ea. sp
trt.dat$trt <- rep(ht, each = trtrep)

#########################################################
# Next, making a data frame for the pheno data

nphen <- 10 # rep per pheno event 
Nph <- Nspp * nphen

pheno.dat <- data.frame(matrix(NA, Nph, 2))
names(pheno.dat) <- c("species", "rep")
pheno.dat$rep <- c(1:Nph)
pheno.dat$species <- rep(c(1:Nspp), each = nphen)


# Generating data for the cues:
mu.force <- 20
sigma.force <- 5
forcingi <- rnorm(Nph, mu.force, sigma.force)
pheno.dat$forcingi <- forcingi

mu.chill <- 20
sigma.chill <- 5
chillingi <- rnorm(Nph, mu.chill, sigma.chill)
pheno.dat$chillingi <- chillingi

mu.photo <- 20
sigma.photo <- 5
photoi <- rnorm(Nph, mu.photo, sigma.photo)
pheno.dat$photoi <- photoi

# adding the species level differences
mu.pheno.sp <- 150
sigma.pheno.sp <- 2

alpha.pheno.sp <- rnorm(Nspp, mu.pheno.sp, sigma.pheno.sp)
pheno.dat$alpha.pheno.sp <- rep(alpha.pheno.sp, each = nphen)

# Adding species variation in cue use:
mu.force.sp <- -1 # negative bc warmer means earlier
sigma.force.sp <- 0.1
alpha.force.sp <- rnorm(Nspp, mu.force.sp, sigma.force.sp)

mu.chill.sp <- -2 
sigma.chill.sp <- 0.1
alpha.chill.sp <- rnorm(Nspp, mu.chill.sp, sigma.chill.sp)

mu.photo.sp <- -2 
sigma.photo.sp <- 0.1
alpha.photo.sp <- rnorm(Nspp, mu.photo.sp, sigma.photo.sp)

#interaction between trait and phenology?
betaTraitxchill <- -.8 
betaTraitxphoto<- -.8 
betaTraitxforce <- .8 

#combine teh effects of forcing and species trait differences into a slope
beta.forcing.sp1 <- alpha.force.sp + ht*betaTraitxforce
beta.forcing.sp <- rep(beta.forcing.sp1, )
pheno.dat$beta.forcing.sp <- rep(beta.forcing.sp, each = nphen)

beta.chilling.sp1 <- alpha.chill.sp + ht*betaTraitxchill
beta.chilling.sp <- rep(beta.chilling.sp1, )
pheno.dat$beta.chilling.sp <- rep(beta.chilling.sp, each = nphen)

beta.photo.sp1 <- alpha.photo.sp + ht*betaTraitxphoto
beta.photo.sp <- rep(beta.photo.sp1, )
pheno.dat$beta.photo.sp <- rep(beta.photo.sp, each = nphen)

#general variance
sigma.gen <- 2
gen.var <- rnorm(Nph, 0, sigma.gen) 
pheno.dat$gen.var <- gen.var

#"run" the full model to simulate data 
pheno.dat$doy.i <- pheno.dat$alpha.pheno.sp + pheno.dat$beta.forcing.sp * pheno.dat$forcingi +
  pheno.dat$beta.chilling.sp * pheno.dat$chillingi + pheno.dat$beta.photo.sp * pheno.dat$photoi + pheno.dat$gen.var

hist(pheno.dat$doy.i)

trt.pheno <- list(
  yTraiti = trt.dat$trt, 
  N = length(trt.dat), # sample size for trait data is the same as phenology data 
  n_spec = Nspp,  # number of species is the same for traits and phenology data
  species = trt.dat$species, 
  study = trt.dat$study, 
  n_study = Nstudy, 
  yPhenoi = pheno.dat$doy.i, 
  Nph = length(pheno.dat), # sample size for trait data is the same as phenology data  
  forcingi = pheno.dat$forcingi,
  photoi = pheno.dat$photoi,
  chillingi = pheno.dat$chillingi)  

test.dat <- stan('stan/stan_joint_traitors.stan', data = trt.pheno,
                 iter = 1000, warmup=1000)
