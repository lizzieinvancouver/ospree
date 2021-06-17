# Started by DL on June 14, 2021.

# There are some persistent issues with the joint model, so we are taking two steps back and going to get the trait only and pheno only model working. Lizzie suggested the following: 
#1. Make a nice, compact version of your R file that runs just your traits model. Make sure you can get nice results back that match your parameters and that the chains look well mixed. Ideally this should not take >4,000 iterations. Check!
#2. Assuming that works, make up a forcing-only phenology model and check via lmer, rstan or such that your code for phenology alone is correct (maybe make a separate R file for this also).Check! 
#3. Merge the two models only once 1 and 2 are done.
#4.If all goes well we could add in chilling and photoperiod. To do this, go back and do step 2 (adding chill and photo) then merge.

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/ospree/analyses/traits")
} else{
  setwd("~/R/traitors")
}

library(rstan)
require(shinystan)
require(bayesplot)
require(truncnorm)
library(ggplot2)


rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

Nspp <- 30 # number of species with traits (making this 20 just for speed for now)
nphen <- 15 # rep per pheno event 
Nph <- Nspp * nphen
Nph

# the trait effect
sigma.species <- 5
alpha.trait.sp<- rnorm(Nspp, 0, sigma.species)

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
sigma.force.sp <- 2
alpha.force.sp <- rnorm(Nspp, mu.force.sp, sigma.force.sp)
pheno.dat$alpha.force.sp <- rep(alpha.force.sp, each = nphen)

mu.chill.sp <- -1 # negative bc warmer means earlier
sigma.chill.sp <- 2
alpha.chill.sp <- rnorm(Nspp, mu.chill.sp, sigma.chill.sp)
pheno.dat$alpha.chill.sp <- rep(alpha.chill.sp, each = nphen)


betaTraitxforce <- 2 #interaction between trait and phenology
betaTraitxchill <- 2

beta.force.temp <- alpha.force.sp + alpha.trait.sp * betaTraitxforce
beta.force.sp <- rep(beta.force.temp,)
pheno.dat$beta.force.sp <- rep(beta.force.sp, each = nphen)

#Generate the cue values (ie the F that gets multipled with betaForcing{sp})
mu.force <- 5 # This is a big forcing effect.  turned down to 5
sigma.force <- 1
force.i <- rnorm(Nph, mu.force, sigma.force)  # predictor frocing, forcei in stan
pheno.dat$force.i <- force.i

beta.chill.temp <- alpha.chill.sp + alpha.trait.sp * betaTraitxchill
beta.chill.sp <- rep(beta.chill.temp,)
pheno.dat$beta.chill.sp <- rep(beta.chill.sp, each = nphen)

#Generate the cue values (ie the F that gets multipled with betaForcing{sp})
mu.chill <- 5 # This is a big forcing effect.  turned down to 5
sigma.chill <- 1
chill.i <- rnorm(Nph, mu.chill, sigma.chill)  # predictor frocing, chilli in stan
pheno.dat$chill.i <- chill.i

#general variance
sigma.gen <- 5
gen.var <- rnorm(Nph, 0, sigma.gen) 
pheno.dat$gen.er <- gen.var

#"run" the full model to simulate data 
pheno.dat$doy.i <- pheno.dat$alpha.pheno.sp + pheno.dat$beta.force.sp * pheno.dat$force.i + pheno.dat$beta.chill.sp * pheno.dat$chill.i + pheno.dat$gen.er

### Phenology only stan model ############################################
# For future when we think the linear model works! 
pheno_data <- list(n_spec = Nspp, 
                   species = pheno.dat$species, 
                   yPhenoi = pheno.dat$doy.i, 
                   alphaTraitSp = alpha.trait.sp,
                   Nph = Nph, 
                   N = Nph,
                   forcei = force.i,
                   chilli = chill.i) 

mdl.pheno <- stan('stan/stan_joint_forcingchilling_pheno_only.stan',
                  data = pheno_data, iter = 4000)


save(mdl.pheno, file = "output.phenoonly.6.Rda")


save(mdl.force, file = "output.forcingonly.1.Rda")


load(file = "output/output.forcingonly.Rda")
####################################################################
ssm <-  as.shinystan(mdl.force)
launch_shinystan(ssm)

sum.f <- summary(mdl.pheno)$summary
post.f <- rstan::extract(mdl.pheno)


range(sum.f[, "n_eff"])
range(sum.f[, "Rhat"])

# ppc and trying to figure out what is going on! 
y<- as.numeric(pheno.dat$doy.i)
yrep<-post.f$ymu # I want this to be a matrix, which it is, with one element for each data point in y

ppc_dens_overlay(y, yrep[1:100, ]) # hmm the yrep does not appear
plot(density(yrep))
plot(density(y))

stan_hist(mdl.force) # defualt is the firest 10 parameters
stan_hist(mdl.force, pars = c("muForceSp","muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"))

stan_hist(mdl.force, pars = "alphaForceSp")
post.f2 <- as.matrix(mdl.force, par = c("muForceSp", "muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(post.f2,
           pars = c("muForceSp","muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"),
           prob = 0.8) + plot_title

mcmc_areas(post.f2,
           pars = c("muForceSp","muPhenoSp"),
           prob = 0.8) + plot_title    

mcmc_areas(post.f2,
           pars = c("sigmaForceSp","sigmaPhenoSp","sigmapheno_y"),
           prob = 0.8) + plot_title        

# Is it giving me back the values?
# Is it giving me back the values?

mu_chillsp <- sum.f[grep("muChillSp", rownames(sum.f))]
sigma_chillsp <- sum.f[grep("sigmaChillSp", rownames(sum.f))]
beta_tc <- sum.f[grep("betaTraitxChill", rownames(sum.f))]
mu_forcesp <- sum.p[grep("muForceSp", rownames(sum.p))]
mu_phenosp <- sum.p[grep("muPhenoSp", rownames(sum.p))]
alpha.forcingsp <- sum.p[grep("alphaForcingSp", rownames(sum.p))]
sigma_forcesp <- sum.p[grep("sigmaForceSp", rownames(sum.p))]
sigma_phenosp <- sum.p[grep("sigmaPhenoSp", rownames(sum.p))]
sigma_phenoy <- sum.p[grep("sigmapheno_y", rownames(sum.p))]
beta_tf <- sum.p[grep("betaTraitxPheno", rownames(sum.p))]

mdl.out <- data.frame( "Parameter" = c("mu_forcesp","mu_chillsp","mu_phenosp","sigma_forcesp","sigma_chillsp", "sigma_phenosp", 
                                       "sigma_phenoy", "beta_tf", "beta_tc"),
                       "Test.data.values" = c(mu.force.sp, mu.chill.sp, mu.pheno.sp, sigma.force.sp, sigma.chill.sp, sigma.pheno.sp, 
                                              sigma.gen, betaTraitxforce, betaTraitxchill),
                       "Estiamte" = c(mu_forcesp, mu_chillsp, mu_phenosp, sigma_forcesp, sigma_chillsp, sigma_phenosp, sigma_phenoy, beta_tf, beta_tc))

mdl.out



mdl.out <- data.frame( "Parameter" = c("sigma_sp", "mu_phenosp","mu_forcesp",
                                       "sigma_forcesp","mu_chillsp","sigma_chillsp",
                                       "sigma_phenosp", "sigma_phenoy", "beta_tf", "beta_tc"),
                       "Test.data.values" = c( sigma.species, mu.pheno.sp, mu.force.sp, 
                                               sigma.force.sp, mu.chill.sp, sigma.chill.sp, 
                                               sigma.pheno.sp, sigma.gen, betaTraitxforce, betaTraitxchill),
                       "Estiamte" = c( sigma_sp,  mu_phenosp, mu_forcesp,
                                       sigma_forcesp, mu_chillsp, sigma_chillsp, 
                                       sigma_phenosp, 
                                      sigma_phenoy, beta_tf, beta_tc))

mdl.out

# fit1 <- stan(...)
# fit1.extract <- extract(fit1)
# # Real data
# plot(doy.i ~ alpha.pheno.sp + beta.force.sp * force.i, data = pheno.dat, type = "l", col = "black")
# # Iteration 22 of the generated quantity y_pred
# points(post.f$ymu[22, ] ~ x, col = rgb(0, 0, 1, alpha = .4), type = "l")