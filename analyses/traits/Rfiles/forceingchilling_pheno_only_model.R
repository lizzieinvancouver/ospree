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

mu.force.sp <- 0 # negative bc warmer means earlier
sigma.force.sp <- 10
alpha.force.sp <- rnorm(Nspp, mu.force.sp, sigma.force.sp)
pheno.dat$alpha.force.sp <- rep(alpha.force.sp, each = nphen)

mu.photo.sp <- 0 # negative bc warmer means earlier
sigma.photo.sp <- 10
alpha.photo.sp <- rnorm(Nspp, mu.photo.sp, sigma.photo.sp)
pheno.dat$alpha.photo.sp <- rep(alpha.photo.sp, each = nphen)

mu.chill.sp <- 0 # negative bc warmer means earlier
sigma.chill.sp <- 10
alpha.chill.sp <- rnorm(Nspp, mu.chill.sp, sigma.chill.sp)
pheno.dat$alpha.chill.sp <- rep(alpha.chill.sp, each = nphen)


betaTraitxforce <- 0 #interaction between trait and phenology
betaTraitxphoto <- 0
betaTraitxchill <- 0

beta.force.temp <- alpha.force.sp + alpha.trait.sp * betaTraitxforce
beta.force.sp <- rep(beta.force.temp,)
pheno.dat$beta.force.sp <- rep(beta.force.sp, each = nphen)

#Generate the cue values (ie the F that gets multipled with betaForcing{sp})
mu.force <- 5 # This is a big forcing effect.  turned down to 5
sigma.force <- 1
force.i <- rnorm(Nph, mu.force, sigma.force)  # predictor frocing, forcei in stan
pheno.dat$force.i <- force.i

beta.photo.temp <- alpha.photo.sp + alpha.trait.sp * betaTraitxphoto
beta.photo.sp <- rep(beta.photo.temp,)
pheno.dat$beta.photo.sp <- rep(beta.photo.sp, each = nphen)

#Generate the cue values (ie the F that gets multipled with betaForcing{sp})
mu.photo <- 5 # This is a big forcing effect.  turned down to 5
sigma.photo <- 1
photo.i <- rnorm(Nph, mu.photo, sigma.photo)  # predictor frocing, photoi in stan
pheno.dat$photo.i <- photo.i

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
                   photoi = photo.i,
                   chilli = chill.i) 

mdl.pheno <- stan('stan/forcingchilling_pheno_only.stan',
                  data = pheno_data, iter = 4000)


#save(mdl.pheno, file = "output.fconly.1.Rda")

# Just running the code together with chilling: most are pretty good estimates, but mu_phenosp off by 3, sigma_forcsp, sigmachillsp, sigmaphenoy off by 0.25ish 

# 1. tried changing the sigma_chill_sp and sigma_force_sp to 5 --> mu_chillsp bad at -0.76, and sigma_force a little high at 5.21
# 2. leaving sigmas at 5 and trying muchillsp of -2: muchillsp not bad at 2.11, but now muForcesp terrible at -0.77
# 3. Increased muforcesp to -2 too, but resutls very similar to 1, I think muchill needs to be bigger than 
# 4. Increased muchillsp to -3, but this really didn't fix anything
# 5. Maybe changing the variance is what the key is in the priors? changed prior to sigmaForceSp ~ normal(5, 1), made muchill worse and sigma_force, so tried sigmaForceSp ~ normal(5, 0.1) it was slightly better but it had a similar effect on muchill 
# 6. Kept the variance for forcing low as above (0.1), and changed muchillsp back to -2: not absolutley terrible
# Parameter Test.data.values   Estiamte
# 1    mu_forcesp               -1  -1.070400
# 2    mu_chillsp               -2  -1.799864
# 3    mu_phenosp              150 147.661110
# 4 sigma_forcesp                5   4.984588
# 5 sigma_chillsp                5   5.126138
# 6 sigma_phenosp               10   9.883390
# 7  sigma_phenoy                5   4.919953
# 8       beta_tf                2   2.127240
# 9       beta_tc                2   2.093260

# what if i made the variance for chilling small too? or big --> model esitmates were worse for both

sum.fc <- summary(mdl.pheno)$summary
mu_chillsp <- sum.fc[grep("muChillSp", rownames(sum.fc))]
sigma_chillsp <- sum.fc[grep("sigmaChillSp", rownames(sum.fc))]
beta_tc <- sum.fc[grep("betaTraitxChill", rownames(sum.fc))]
mu_forcesp <- sum.fc[grep("muForceSp", rownames(sum.fc))]
mu_phenosp <- sum.fc[grep("muPhenoSp", rownames(sum.fc))]
alpha.forcingsp <- sum.fc[grep("alphaForcingSp", rownames(sum.fc))]
sigma_forcesp <- sum.fc[grep("sigmaForceSp", rownames(sum.fc))]
sigma_phenosp <- sum.fc[grep("sigmaPhenoSp", rownames(sum.fc))]
sigma_phenoy <- sum.fc[grep("sigmapheno_y", rownames(sum.fc))]
beta_tf <- sum.fc[grep("betaTraitxForce", rownames(sum.fc))]

mdl.out <- data.frame( "Parameter" = c("mu_forcesp","mu_chillsp","mu_phenosp","sigma_forcesp","sigma_chillsp", "sigma_phenosp", 
                                       "sigma_phenoy", "beta_tf", "beta_tc"),
                       "Test.data.values" = c(mu.force.sp, mu.chill.sp, mu.pheno.sp, sigma.force.sp, sigma.chill.sp, sigma.pheno.sp, 
                                              sigma.gen, betaTraitxforce, betaTraitxchill),
                       "Estiamte" = c(mu_forcesp, mu_chillsp, mu_phenosp, sigma_forcesp, sigma_chillsp, sigma_phenosp, sigma_phenoy, beta_tf, beta_tc))

mdl.out

load(file = "output/output.forcingonly.Rda")
####################################################################
ssm <-  as.shinystan(mdl.pheno)
launch_shinystan(ssm)

sum.fc <- summary(mdl.pheno)$summary
post.f <- rstan::extract(mdl.pheno)


range(sum.fc[, "n_eff"])
range(sum.fc[, "Rhat"])

# ppc and trying to figure out what is going on! 
y<- as.numeric(pheno.dat$doy.i)
yrep<-post.f$ymu # I want this to be a matrix, which it is, with one element for each data point in y

ppc_dens_overlay(y, yrep[1:100, ]) # hmm the yrep does not appear
plot(density(yrep))
plot(density(y))

stan_hist(mdl.pheno) # defualt is the firest 10 parameters
stan_hist(mdl.pheno, pars = c("muForceSp","muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"))

stan_hist(mdl.pheno, pars = "alphaForceSp")
post.f2 <- as.matrix(mdl.pheno, par = c("muForceSp", "muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"))

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
sum.fc <- summary(mdl.pheno)$summary
mu_chillsp <- sum.fc[grep("muChillSp", rownames(sum.fc))]
sigma_chillsp <- sum.fc[grep("sigmaChillSp", rownames(sum.fc))]
beta_tc <- sum.fc[grep("betaTraitxChill", rownames(sum.fc))]
mu_forcesp <- sum.fc[grep("muForceSp", rownames(sum.fc))]
mu_phenosp <- sum.fc[grep("muPhenoSp", rownames(sum.fc))]
alpha.forcingsp <- sum.fc[grep("alphaForcingSp", rownames(sum.fc))]
sigma_forcesp <- sum.fc[grep("sigmaForceSp", rownames(sum.fc))]
sigma_phenosp <- sum.fc[grep("sigmaPhenoSp", rownames(sum.fc))]
sigma_phenoy <- sum.fc[grep("sigmapheno_y", rownames(sum.fc))]
beta_tf <- sum.fc[grep("betaTraitxForce", rownames(sum.fc))]

mdl.out <- data.frame( "Parameter" = c("mu_forcesp","mu_chillsp","mu_phenosp","sigma_forcesp","sigma_chillsp", "sigma_phenosp", 
                                       "sigma_phenoy", "beta_tf", "beta_tc"),
                       "Test.data.values" = c(mu.force.sp, mu.chill.sp, mu.pheno.sp, sigma.force.sp, sigma.chill.sp, sigma.pheno.sp, 
                                              sigma.gen, betaTraitxforce, betaTraitxchill),
                       "Estiamte" = c(mu_forcesp, mu_chillsp, mu_phenosp, sigma_forcesp, sigma_chillsp, sigma_phenosp, sigma_phenoy, beta_tf, beta_tc))

mdl.out


# fit1 <- stan(...)
# fit1.extract <- extract(fit1)
# # Real data
# plot(doy.i ~ alpha.pheno.sp + beta.force.sp * force.i, data = pheno.dat, type = "l", col = "black")
# # Iteration 22 of the generated quantity y_pred
# points(post.f$ymu[22, ] ~ x, col = rgb(0, 0, 1, alpha = .4), type = "l")