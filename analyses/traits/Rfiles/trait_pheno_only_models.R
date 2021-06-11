# Started by DL on June 10, 2021.

# There are some persistent issues with the joint model, so we are taking two steps back and going to get the trait only and pheno only model working. Lizzie suggested the following: 
#1. Make a nice, compact version of your R file that runs just your traits model. Make sure you can get nice results back that match your parameters and that the chains look well mixed. Ideally this should not take >4,000 iterations.
#2. Assuming that works, make up a forcing-only phenology model and check via lmer, rstan or such that your code for phenology alone is correct (maybe make a separate R file for this also).
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

Nrep <- 10 # rep per trait
Nstudy <- 20 # number of studies w/ traits (10 seems a little low for early simulation code; remember that you are estimating a distribution of this the same as for species)
Nspp <- 20 # number of species with traits (making this 20 just for speed for now)

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

# Stop here and test your work a little ...okay, it's hard to interpret this output but we can check the general variance and intercept and check the relative variance of species and study (maybe using the SD?)

## Trait only stan model ###########################################################
trait_data <- list(yTraiti = trt.dat$yTraiti, 
                   N = Ntrt, 
                   n_spec = Nspp, 
                   species = trt.dat$species, 
                   study = trt.dat$study, 
                   n_study = Nstudy) 

mdl.traitonly <- stan('stan/stan_joint_traitonly.stan',
                      data = trait_data, iter = 4000)

# June 10: 26 transitions after warmup exceed max depth, large rhats
# 1. The sigmaTrait_y looked really weird, unrealistically narrow and low, so I changed the prior to it to: sigmaTrait_y ~ normal(10, 1) --> The Ess was still too low
# 2. Played around with the sigmaTrait_y prior, I tried (10, 5), nothing really changed
# 3. then I tried (15, 1) amd got no warning messages! Mu grand looks better, but the chains are not as good as everything else, the esti is 20.04 (vs 20), but the other esti look ok! all within 0.15 of the true value
#save(mdl.traitonly, file = "output.traitonly.3.Rda")
# 4. I am going to see what happens when I decrease mu grand to 10 -- > model output was slightly worse, 10.39 for mu_grand, but sigma_stidy was also off by 0.2
# 5. What is i made the the prior variance smaller (10, 0.5)? The chains look better, but the estimates are not as good for the other factors
#save(mdl.traitonly, file = "output.traitonly.5.Rda")
# 6. Out of curiosity, i increased the mu_grand prior to (20,2) --> a bit mixed, mugrand is 20.26, sigmasp and sigmay are closer but sigmastudy is off by .25, the chains for mu grand look aweful again though
# 7. Decreasing the variances for the larger mugrand (20, 0.5), not the best, but (10, 0.5) looked interesting:
save(mdl.traitonly, file = "output.traitonly.7.Rda")
# Parameter Test.data.values  Estiamte
# 1     mu_grand               10  9.993991
# 2     sigma_sp               10 10.075637
# 3  sigma_study                5  4.937649
# 4 sigmaTrait_y               15 15.140083

####################################################################
ssm <-  as.shinystan(mdl.traitonly)
launch_shinystan(ssm)

sumer <- summary(mdl.traitonly)$summary
post <- rstan::extract(mdl.traitonly)


range(sumer[, "n_eff"])
range(sumer[, "Rhat"])

# ppc and trying to figure out what is going on! 
y<-as.numeric(trt.dat$yTraiti)
yrep<-post$ymu # I want this to be a matrix, which it is, with one element for each data point in y

ppc_dens_overlay(y, yrep[1:100, ]) # hmm the yrep does not appear
plot(density(yrep))
plot(density(y))

stan_hist(mdl.traitonly) # defualt is the firest 10 parameters
stan_hist(mdl.traitonly, pars = c("sigmaTrait_y", "mu_grand", "sigma_sp","sigma_study"))


post2 <- as.matrix(mdl.traitonly, par = c("sigmaTrait_y", "mu_grand", "sigma_sp","sigma_study"))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(post2,
           pars = c("sigmaTrait_y", "mu_grand", "sigma_sp","sigma_study"),
           prob = 0.8) + plot_title

mcmc_areas(post2,
           pars = c("sigmaTrait_y"),
           prob = 0.8) + plot_title    

mcmc_areas(post2,
           pars = c("mu_grand"),
           prob = 0.8) + plot_title        

# Is it giving me back the values?
mu_grand <- sumer[grep("mu_grand", rownames(sumer))]
sigma_sp <- sumer[grep("sigma_sp", rownames(sumer))]
sigma_studyesti <- sumer[grep("sigma_study", rownames(sumer))]
sigmaTrait_y <- sumer[grep("sigmaTrait_y", rownames(sumer))]

mdl.out <- data.frame( "Parameter" = c("mu_grand","sigma_sp","sigma_study", "sigmaTrait_y"),
                       "Test.data.values" = c(mu.grand, sigma.species, sigma.study, trt.var),
                       "Estiamte" = c(mu_grand, sigma_sp, sigma_studyesti, sigmaTrait_y))

mdl.out

# mu_grand is still not great: but does that look that bad?
#plot(density(rnorm(1000, 10, 0.5)), col = "red", xlim = c(0,20),  ylim = c(0, 0.8)); lines(density(post$mu_grand ), lty = 1)

##########################################################################
##########################################################################
##########################################################################

nphen <- 10 # rep per pheno event 
Nph <- Nspp * nphen
Nph
pheno.dat <- data.frame(matrix(NA, Nph, 2))
names(pheno.dat) <- c("rep","species")
pheno.dat$rep <- c(1:Nph)
pheno.dat$species <- rep(c(1:Nspp), each = nphen)

mu.trtsp #the effect of species trait differences from the trait model
for(i in 1:length(mu.trtsp)){
  pheno.dat$mu.trtsp[pheno.dat$species == i] <- mu.trtsp[i]
}

# Generating data for the cues: this is the overall effect of each cue, not the species level effect
#Generate the cue values (ie the F that gets multipled with betaForcing{sp})
mu.force <- 20 # This is a big forcing effect. It seems a little high to me, but may be okay. You'll just want to be cautious on your priors and make sure they encompass this ..
sigma.force <- 5
force.i <- rnorm(Nph, mu.force, sigma.force)  # predictor frocing, forcei in stan
pheno.dat$force.i <- force.i

mu.chill <- 20
sigma.chill <- 5
chill.i <- rnorm(Nph, mu.chill, sigma.chill) # predictor chilling, chilli in stan
pheno.dat$chill.i <- chill.i

mu.photo <- 20
sigma.photo <- 5
photo.i <- rnorm(Nph, mu.photo, sigma.photo)
pheno.dat$photo.i <- photo.i

# Now generating the values for different species
# Phenological values across the different species
mu.pheno.sp <- 150
sigma.pheno.sp <- 10 #for a mu this large, I think this is pretty small

alpha.pheno.sp <- rnorm(Nspp, mu.pheno.sp, sigma.pheno.sp)
pheno.dat$alpha.pheno.sp <- rep(alpha.pheno.sp, each = nphen)

# Alert! I don't understand what the below does compared to the above ...? I feel like above you generated species variation in cues ... I think I am confused about what the above versus below does. 
# Adding species variation in cue use:
# May 7: I am making these values kinda large (I think?) and making the sigmas 2
mu.force.sp <- -1 # negative bc warmer means earlier
sigma.force.sp <- 2
alpha.force.sp <- rnorm(Nspp, mu.force.sp, sigma.force.sp)
pheno.dat$alpha.force.sp <- rep(alpha.force.sp, each = nphen)

mu.chill.sp <- -2 
sigma.chill.sp <- 2
alpha.chill.sp <- rnorm(Nspp, mu.chill.sp, sigma.chill.sp)
pheno.dat$alpha.chill.sp <- rep(alpha.chill.sp, each = nphen)

mu.photo.sp <- -3 
sigma.photo.sp <- 2
alpha.photo.sp <- rnorm(Nspp, mu.photo.sp, sigma.photo.sp)
pheno.dat$alpha.photo.sp <- rep(alpha.photo.sp, each = nphen)

# I suggest you set these below to zero and make sure your pheno model runs on its own before you add this  in...
#interaction between trait and cues
#increasing this value from -0.8
betaTraitxchill <- 0
betaTraitxphoto<- 0
betaTraitxforce <- 0

#combine the effects of forcing and species trait differences into a slope
beta.forcing.sp <- alpha.force.sp + mu.trtsp*betaTraitxforce
pheno.dat$beta.forcing.sp <- rep(beta.forcing.sp, each = nphen)

beta.chilling.sp <- alpha.chill.sp + mu.trtsp*betaTraitxchill
pheno.dat$beta.chilling.sp <- rep(beta.chilling.sp, each = nphen)

beta.photo.sp <- alpha.photo.sp + mu.trtsp*betaTraitxphoto
pheno.dat$beta.photo.sp <- rep(beta.photo.sp, each = nphen)

#general variance
# May 7: making this value, the general noise in the phenology model much smaller
sigma.gen <- 0.5
gen.var <- rnorm(Nph, 0, sigma.gen) 
pheno.dat$gen.er <- gen.var

#"run" the full model to simulate data 
pheno.dat$doy.i <- pheno.dat$alpha.pheno.sp + pheno.dat$beta.forcing.sp * pheno.dat$force.i +
  pheno.dat$beta.chilling.sp * pheno.dat$chill.i + pheno.dat$beta.photo.sp * pheno.dat$photo.i + pheno.dat$gen.er
#
# Taking a step back and trying to get the phenology model to run with just a linear model:

fm.pheno <- lmer(doy.i ~ force.i*beta.forcing.sp + photo.i*beta.photo.sp + chill.i*beta.chilling.sp + (1|species), data = pheno.dat)
summary(fm.pheno)

fm.pheno <- lmer(doy.i ~ force.i*beta.forcing.sp + (1|species), data = pheno.dat)
summary(fm.pheno)

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

mdl.pheno <- stan('stan/stan_joint_phenoonly.stan',
                  data = pheno_data, iter = 4000)

save(mdl.traitonly, file = "output.traitonly.7.Rda")
# Parameter Test.data.values  Estiamte
# 1     mu_grand               10  9.993991
# 2     sigma_sp               10 10.075637
# 3  sigma_study                5  4.937649
# 4 sigmaTrait_y               15 15.140083

####################################################################
ssm <-  as.shinystan(mdl.traitonly)
launch_shinystan(ssm)

sumer <- summary(mdl.traitonly)$summary
post <- rstan::extract(mdl.traitonly)


range(sumer[, "n_eff"])
range(sumer[, "Rhat"])

# ppc and trying to figure out what is going on! 
y<-as.numeric(trt.dat$yTraiti)
yrep<-post$ymu # I want this to be a matrix, which it is, with one element for each data point in y

ppc_dens_overlay(y, yrep[1:100, ]) # hmm the yrep does not appear
plot(density(yrep))
plot(density(y))

stan_hist(mdl.traitonly) # defualt is the firest 10 parameters
stan_hist(mdl.traitonly, pars = c("sigmaTrait_y", "mu_grand", "sigma_sp","sigma_study"))


post2 <- as.matrix(mdl.traitonly, par = c("sigmaTrait_y", "mu_grand", "sigma_sp","sigma_study"))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(post2,
           pars = c("sigmaTrait_y", "mu_grand", "sigma_sp","sigma_study"),
           prob = 0.8) + plot_title

mcmc_areas(post2,
           pars = c("sigmaTrait_y"),
           prob = 0.8) + plot_title    

mcmc_areas(post2,
           pars = c("mu_grand"),
           prob = 0.8) + plot_title        

# Is it giving me back the values?
mu_grand <- sumer[grep("mu_grand", rownames(sumer))]
sigma_sp <- sumer[grep("sigma_sp", rownames(sumer))]
sigma_studyesti <- sumer[grep("sigma_study", rownames(sumer))]
sigmaTrait_y <- sumer[grep("sigmaTrait_y", rownames(sumer))]

mdl.out <- data.frame( "Parameter" = c("mu_grand","sigma_sp","sigma_study", "sigmaTrait_y"),
                       "Test.data.values" = c(mu.grand, sigma.species, sigma.study, trt.var),
                       "Estiamte" = c(mu_grand, sigma_sp, sigma_studyesti, sigmaTrait_y))

mdl.out

# mu_grand is still not great: but does that look that bad?
plot(density(rnorm(1000, 10, 0.5)), col = "red", xlim = c(0,20),  ylim = c(0, 0.8)); lines(density(post$mu_grand ), lty = 1)
