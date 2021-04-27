# Date started: March 31, 2021
# The purpose of this code is to generate test data for the traitors model with all three climate parameters and a single trait, here we start with height:
# 
# if(length(grep("deirdreloughnan", getwd()) > 0)) {
#   setwd("~/Documents/github/ospree/analyses/traits")
# } else{
#   setwd("~/R/traitors")
# }

library(rstan)
require(shinystan)
require(bayesplot)
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

Nrep <- 5 # rep per trait
Nstudy <- 20 # number of studies w/ traits
Nspp <- 20 # number of species with traits

# First making a data frame for the test trait data
Ntrt <- Nspp * Nstudy * Nrep # total number of traits observations
Ntrt
#make a dataframe for height
trt.dat <- data.frame(matrix(NA, Ntrt, 2))
names(trt.dat) <- c("rep"," study")
trt.dat$rep <- c(1:Ntrt)
trt.dat$study <- rep(c(1:Nstudy), each = Nspp)
trt.dat$species <- rep(c(1:Nspp), each = Nstudy)


# now generating the species trait data, here it is for height
mu.trt <- 20 # the grand mean trait value?
sigma.trtsp <- 10 #the species sigma for the traits model

alpha.trtsp <- rnorm(Nspp, 0, sigma.trtsp)
trt.dat$alpha.trtsp <- rep(alpha.trtsp, each = Nstudy) #adding ht data for ea. sp

#now generating the effects of study
sigma.study <- 5
alpha.study <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$alpha.study <- rep(alpha.study, each = Nspp) # generate data for ea study

# general variance
trt.var <- 2
trt.dat$trt.er <- rnorm(Ntrt, 0, trt.var)

# generate yhat for this first trt model
trt.dat$yTraiti <- mu.trt + trt.dat$alpha.trtsp + trt.dat$alpha.study + trt.dat$trt.er
#########################################################
# Next, making a data frame for the pheno data

nphen <- 20 # rep per pheno event 
Nph <- Nspp * nphen
Nph
pheno.dat <- data.frame(matrix(NA, Nph, 2))
names(pheno.dat) <- c("rep","species")
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
betaTraitxforce <- -.8 

#combine the effects of forcing and species trait differences into a slope
beta.forcing.sp1 <- alpha.force.sp + alpha.trtsp*betaTraitxforce
beta.forcing.sp <- rep(beta.forcing.sp1, )
pheno.dat$beta.forcing.sp <- rep(beta.forcing.sp, each = nphen)

beta.chilling.sp1 <- alpha.chill.sp + alpha.trtsp*betaTraitxchill
beta.chilling.sp <- rep(beta.chilling.sp1, )
pheno.dat$beta.chilling.sp <- rep(beta.chilling.sp, each = nphen)

beta.photo.sp1 <- alpha.photo.sp + alpha.trtsp*betaTraitxphoto
beta.photo.sp <- rep(beta.photo.sp1, )
pheno.dat$beta.photo.sp <- rep(beta.photo.sp, each = nphen)

#general variance
sigma.gen <- 2
gen.var <- rnorm(Nph, 0, sigma.gen) 
pheno.dat$gen.er <- gen.var

#"run" the full model to simulate data 
pheno.dat$doy.i <- pheno.dat$alpha.pheno.sp + pheno.dat$beta.forcing.sp * pheno.dat$forcingi +
  pheno.dat$beta.chilling.sp * pheno.dat$chillingi + pheno.dat$beta.photo.sp * pheno.dat$photoi + pheno.dat$gen.er


stan_data <- list(yTraiti = trt.dat$yTraiti, 
                  N = Ntrt, 
                  n_spec = Nspp, 
                  species = trt.dat$species, 
                  study = trt.dat$study, 
                  n_study = Nstudy, 
                  yPhenoi = pheno.dat$doy.i, 
                  Nph = Nph, 
                  forcingi = forcingi,
                  photoi = photoi, 
                  chillingi = chillingi,
                  species2 = pheno.dat$species) 

mdl.test <- stan('stan/stan_joint_traitors.stan',
                 data = stan_data, iter = 8000,
                 control = list(adapt_delta = 0.99, max_treedepth = 18))

save(mdl.test, file = "output.traitors.Rda")

#load("output/output.traitors.Rda")

# ssm <-  as.shinystan(mdl.test)
# launch_shinystan(ssm)
# 
# sumer <- summary(mdl.test)$summary
# post <- extract(mdl.test)
# # #
# y<-trt.dat$yTraiti
# yrep<-post$ymu # I want this to be a matrix, which it is, with one element for each data point in y
# 
# ppc_dens_overlay(y, yrep[1:50, ])
# #
# # #model 1
# plot(density(post$sigmaTrait_y )) #
# plot(density(post$muSp )) # a bit add
# plot(density(post$sigma_sp))
# plot(density(post$sigma_stdy))
# plot(density(post$muStdy )) # really odd looking
# 
# #model 2
# plot(density(post$alphaForcingSp))
# plot(density(post$alphaChillSp))
# plot(density(post$alphaPhotoSp))
# plot(density(post$sigmapheno_y )) #
# 
# plot(density(post$betaTraitxForcing))
# plot(density(post$betaTraitxPhoto))
# plot(density(post$betaTraitxChill))#
# 
# plot(density(post$muForceSp)) #
# plot(density(post$sigmaForceSp)) #
# 
# plot(density(post$muChillSp)) #
# plot(density(post$sigmaChillSp)) #
# 
# plot(density(post$muPhotoSp)) #
# plot(density(post$sigmaPhotoSp)) #
# 
