# Started by DL on June 14, 2021.

# There are some persistent issues with the joint model, so we are taking two steps back and going to get the trait only and pheno only model working. Lizzie suggested the following: 
#1. Make a nice, compact version of your R file that runs just your traits model. Make sure you can get nice results back that match your parameters and that the chains look well mixed. Ideally this should not take >4,000 iterations. Check!
#2. Assuming that works, make up a forcing-only phenology model and check via lmer, rstan or such that your code for phenology alone is correct (maybe make a separate R file for this also).Check! 
#3. Merge the two models only once 1 and 2 are done.
#4.If all goes well we could add in chilling and photoperiod. To do this, go back and do step 2 (adding chill and photo) then merge.

# modified Aug 5, 2021: We are concerned by the fact that the betatraitxcue did not do well when positive. I am looking into this more to better understand the issue and the sensitivities of the model

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/ospree/analyses/traits")
} else{
  setwd("/home/deirdre/ospree")
}

library(rstan)
require(shinystan)
require(bayesplot)
require(truncnorm)
library(ggplot2)


rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

Nrep <- 10 # rep per trait
Nstudy <- 25 # number of studies w/ traits (10 seems a little low for early simulation code; remember that you are estimating a distribution of this the same as for species)
Nspp <- 40 # number of species with traits (making this 20 just for speed for now)
# note I changed this to 30 to match the pheno mdl

# First making a data frame for the test trait data
Ntrt <- Nspp * Nstudy * Nrep # total number of traits observations
Ntrt

#make a dataframe for height
trt.dat <- data.frame(matrix(NA, Ntrt, 1))
names(trt.dat) <- c("rep")
trt.dat$rep <- c(1:Nrep)
trt.dat$study <- rep(c(1:Nstudy), each = Nspp)
trt.dat$species <- rep(1:Nspp, Nstudy)

# now generating the species trait data, here it is for height
mu.grand <- 10 # the grand mean of the height model
sigma.species <- 10 # we want to keep the variaiton across spp. high

#the alphaTraitSp in Faiths original code:
mu.trtsp <- rnorm(Nspp, 0, sigma.species)
trt.dat$mu.trtsp <- rep(mu.trtsp, Nstudy) #adding ht data for ea. sp

#now generating the effects of study
sigma.study <- 5
mu.study <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$mu.study <- rep(mu.study, each = Nspp) # generate data for ea study

# general variance
trt.var <- 15 #sigmaTrait_y in the stan code
trt.dat$trt.er <- rnorm(Ntrt, 0, trt.var)

# generate yhat - heights -  for this first trt model
trt.dat$yTraiti <- mu.grand + trt.dat$mu.trtsp + trt.dat$mu.study + trt.dat$trt.er

##########################################################################
##########################################################################
#increase Nspp and see if closer 150
# check the internal values for indiv spp. 
# look at the log posterior, bivariate plots: mu_phenosp on y and log_post on x
# always check the other parameter if a combination of 2 parameters

Nspp <- 40 # number of species with traits (making this 20 just for speed for now)
nphen <- 15 # rep per pheno event 
Nph <- Nspp * nphen
Nph

# # the trait effect
# sigma.species <- 5
# alpha.trait.sp<- rnorm(Nspp, 0, sigma.species)

pheno.dat <- data.frame(matrix(NA, Nph, 2))
names(pheno.dat) <- c("rep","species")
pheno.dat$rep <- c(1:Nph)
pheno.dat$species <- rep(c(1:Nspp), each = nphen)

# Generating data for the cues: this is the overall effect of each cue, not the species level effect

# Now generating the values for different species
# Phenological values across the different species
mu.pheno.sp <- 150
sigma.pheno.sp <- 10 #for a mu this large, I think this is pretty small
alpha.pheno.sp <- rnorm(Nspp, mu.pheno.sp, sigma.pheno.sp) 
pheno.dat$alpha.pheno.sp <- rep(alpha.pheno.sp, each = nphen)

mu.force.sp <- -1 # negative bc warmer means earlier
sigma.force.sp <- 5
alpha.force.sp <- rnorm(Nspp, mu.force.sp, sigma.force.sp)
pheno.dat$alpha.force.sp <- rep(alpha.force.sp, each = nphen)

mu.chill.sp <- -2 # negative bc warmer means earlier
sigma.chill.sp <- 5
alpha.chill.sp <- rnorm(Nspp, mu.chill.sp, sigma.chill.sp)
pheno.dat$alpha.chill.sp <- rep(alpha.chill.sp, each = nphen)

mu.photo.sp <- -2 # negative bc warmer means earlier
sigma.photo.sp <- 5
alpha.photo.sp <- rnorm(Nspp, mu.photo.sp, sigma.photo.sp)
pheno.dat$alpha.photo.sp <- rep(alpha.photo.sp, each = nphen)

betaTraitxforce <- 2 #interaction between trait and phenology
betaTraitxchill <- 2 #interaction between trait and phenology
betaTraitxphoto <- 2 #interaction between trait and phenology

beta.force.temp <- alpha.force.sp + mu.trtsp * betaTraitxforce
beta.force.sp <- rep(beta.force.temp,)
pheno.dat$beta.force.sp <- rep(beta.force.sp, each = nphen)

beta.chill.temp <- alpha.chill.sp + mu.trtsp * betaTraitxchill
beta.chill.sp <- rep(beta.chill.temp,)
pheno.dat$beta.chill.sp <- rep(beta.chill.sp, each = nphen)

beta.photo.temp <- alpha.photo.sp + mu.trtsp * betaTraitxphoto
beta.photo.sp <- rep(beta.photo.temp,)
pheno.dat$beta.photo.sp <- rep(beta.photo.sp, each = nphen)

#Generate the cue values (ie the F that gets multipled with betaForcing{sp})
mu.force <- 5 # This is a big forcing effect.  turned down to 5
sigma.force <- 1
force.i <- rnorm(Nph, mu.force, sigma.force)  # predictor frocing, forcei in stan
pheno.dat$force.i <- force.i

mu.chill <- 5 # This is a big forcing effect.  turned down to 5
sigma.chill <- 1
chill.i <- rnorm(Nph, mu.chill, sigma.chill)  # predictor frocing, chilli in stan
pheno.dat$chill.i <- chill.i

mu.photo <- 5 # This is a big forcing effect.  turned down to 5
sigma.photo <- 1
photo.i <- rnorm(Nph, mu.photo, sigma.photo)  # predictor frocing, photoi in stan
pheno.dat$photo.i <- photo.i

#general variance
sigma.gen <- 5
gen.var <- rnorm(Nph, 0, sigma.gen) 
pheno.dat$gen.er <- gen.var

#"run" the full model to simulate data 
pheno.dat$doy.i <- pheno.dat$alpha.pheno.sp + pheno.dat$beta.force.sp * pheno.dat$force.i + pheno.dat$beta.chill.sp * pheno.dat$chill.i + pheno.dat$beta.photo.sp * pheno.dat$photo.i + pheno.dat$gen.er

### Phenology only stan model ############################################
# For future when we think the linear model works! 
pheno_data <- list(yTraiti = trt.dat$yTraiti, 
                   N = Ntrt, 
                   n_spec = Nspp, 
                   species = trt.dat$species, 
                   study = trt.dat$study, 
                   n_study = Nstudy, 
                   yPhenoi = pheno.dat$doy.i, 
                   Nph = Nph, 
                   forcei = force.i,
                   photoi = photo.i, 
                   chilli = chill.i,
                   species2 = pheno.dat$species) 

mdl.jointfcp <- stan('stan/joint_forcingchillingphoto.stan',
                  data = pheno_data, iter = 4000)

mdl.jointfcp <- stan('stan/joint_3cue_newprior.stan',
                     data = pheno_data, iter = 4000)

# playing around with the betaTcue values, at first all (2,1), but trying them out as -ve values
# 2. with negative values look pretty good, but the sigma chill and photo are off by 0.4
# 3. trying to run the above, with -ve betaTraitcue, but with sigmachillsp and sigmaphotosp with smaller variances of 0.1 --> estimates were worse

# 4.  I think the best model run is with positive betaTraitcue values and variances of 1
#5. sigma_study was a little low, so I increased the number of studies to 25
#save(mdl.jointfcp, file = "output.joint.forcingchillingphoto.4.Rda") 


load(file = "output/output.joint.forcingchillingphoto.incrNspp.Rda")
####################################################################
ssm <-  as.shinystan(mdl.jointfcp)
launch_shinystan(ssm)

sum.jfcp <- summary(mdl.jointfcp)$summary
post.f <- rstan::extract(mdl.jointfcp)

h1 <- hist(rnorm(1000, 2, 3))
h2 <- hist(post.f$betaTraitxChill)

plot(h2, col=rgb(0,0,1,1/4), xlim = c(-2, 6))
plot(h1, col=rgb(1,0,1,1/4), add = T)


range(sum.jfcp[, "n_eff"])
range(sum.jfcp[, "Rhat"])

# ppc and trying to figure out what is going on! 
# y<- as.numeric(pheno.dat$doy.i)
# yrep<-post.f$ymu # I want this to be a matrix, which it is, with one element for each data point in y
# 
# ppc_dens_overlay(y, yrep[1:100, ]) # hmm the yrep does not appear
# plot(density(yrep))
# plot(density(y))

stan_hist(mdl.jointfcp) # defualt is the firest 10 parameters
stan_hist(mdl.jointfcp, pars = c("muForceSp","muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"))


post.f2 <- as.matrix(mdl.jointfcp, par = c("muPhenoSp", "muForceSp",  "sigmaForceSp","muChillSp", "sigmaChillSp","muPhotoSp", "sigmaPhotoSp", "sigmaPhenoSp","sigmapheno_y"))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(post.f2,
           pars = c("sigmaForceSp", "sigmaChillSp", "sigmaPhotoSp", "sigmaPhenoSp","sigmapheno_y"),
           prob = 0.8) + plot_title

mcmc_areas(post.f2,
           pars = c("muForceSp","muChillSp","muPhotoSp","muPhenoSp"),
           prob = 0.8) + plot_title    


# Is it giving me back the values?
# Is it giving me back the values?
mu_grand <- sum.jfcp[grep("mu_grand", rownames(sum.jfcp))]
sigma_sp <- sum.jfcp[grep("sigma_sp", rownames(sum.jfcp))]
sigma_studyesti <- sum.jfcp[grep("sigma_study", rownames(sum.jfcp))]
sigmaTrait_y <- sum.jfcp[grep("sigmaTrait_y", rownames(sum.jfcp))]

mu_chillsp <- sum.jfcp[grep("muChillSp", rownames(sum.jfcp))]
sigma_chillsp <- sum.jfcp[grep("sigmaChillSp", rownames(sum.jfcp))]
beta_tc <- sum.jfcp[grep("betaTraitxChill", rownames(sum.jfcp))]

mu_photosp <- sum.jfcp[grep("muPhotoSp", rownames(sum.jfcp))]
sigma_photosp <- sum.jfcp[grep("sigmaPhotoSp", rownames(sum.jfcp))]
beta_tp <- sum.jfcp[grep("betaTraitxPhoto", rownames(sum.jfcp))]

mu_forcesp <- sum.jfcp[grep("muForceSp", rownames(sum.jfcp))]
mu_phenosp <- sum.jfcp[grep("muPhenoSp", rownames(sum.jfcp))]
alpha.forcingsp <- sum.jfcp[grep("alphaForcingSp", rownames(sum.jfcp))]
sigma_forcesp <- sum.jfcp[grep("sigmaForceSp", rownames(sum.jfcp))]
sigma_phenosp <- sum.jfcp[grep("sigmaPhenoSp", rownames(sum.jfcp))]
sigma_phenoy <- sum.jfcp[grep("sigmapheno_y", rownames(sum.jfcp))]
beta_tf <- sum.jfcp[grep("betaTraitxForce", rownames(sum.jfcp))]


mdl.out <- data.frame( "Parameter" = c("mu_grand","sigma_sp","sigma_study", "sigmaTrait_y", 
                                       "mu_forcesp","mu_chillsp","mu_photosp","mu_phenosp","sigma_forcesp","sigma_chillsp","sigma_photosp", "sigma_phenosp", 
                                       "sigma_phenoy", "beta_tf", "beta_tc","beta_tp"),
                       "Test.data.values" = c(mu.grand, sigma.species, sigma.study, trt.var, 
                                              mu.force.sp, mu.chill.sp, mu.photo.sp, mu.pheno.sp, sigma.force.sp, sigma.chill.sp, sigma.photo.sp, sigma.pheno.sp, 
                                              sigma.gen, betaTraitxforce, betaTraitxchill, betaTraitxphoto),
                       "Estiamte" = c(mu_grand, sigma_sp, sigma_studyesti, sigmaTrait_y, 
                                      mu_forcesp, mu_chillsp, mu_photosp, mu_phenosp, sigma_forcesp, sigma_chillsp, sigma_photosp, sigma_phenosp, sigma_phenoy, beta_tf, beta_tc,  beta_tp))

mdl.out

# fit1 <- stan(...)
# fit1.extract <- extract(fit1)
# # Real data
# plot(doy.i ~ alpha.pheno.sp + beta.force.sp * force.i, data = pheno.dat, type = "l", col = "black")
# # Iteration 22 of the generated quantity y_pred
# points(post.f$ymu[22, ] ~ x, col = rgb(0, 0, 1, alpha = .4), type = "l")