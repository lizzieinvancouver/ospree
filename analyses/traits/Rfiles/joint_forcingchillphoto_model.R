# Started by DL on June 14, 2021.

# There are some persistent issues with the joint model, so we are taking two steps back and going to get the trait only and pheno only model working. Lizzie suggested the following: 
#1. Make a nice, compact version of your R file that runs just your traits model. Make sure you can get nice results back that match your parameters and that the chains look well mixed. Ideally this should not take >4,000 iterations. Check!
#2. Assuming that works, make up a forcing-only phenology model and check via lmer, rstan or such that your code for phenology alone is correct (maybe make a separate R file for this also).Check! 
#3. Merge the two models only once 1 and 2 are done.
#4.If all goes well we could add in chilling and photoperiod. To do this, go back and do step 2 (adding chill and photo) then merge.

# modified Aug 5, 2021: We are concerned by the fact that the betatraitxcue did not do well when positive. I am looking into this more to better understand the issue and the sensitivities of the model

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/ospree/analyses/traits")
}else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits")
}else{
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
Nstudy <- 30 # number of studies w/ traits (10 seems a little low for early simulation code; remember that you are estimating a distribution of this the same as for species)
Nspp <- 80 # number of species with traits (making this 20 just for speed for now)
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

betaTraitxforce <- 0 #interaction between trait and phenology
betaTraitxchill <- 0 #interaction between trait and phenology
betaTraitxphoto <- 0 #interaction between trait and phenology

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

# mdl.jointfcp <- stan('stan/joint_forcingchillingphoto.stan',
#                   data = pheno_data, iter = 4000)

mdl.jointfcp <- stan('stan/joint_3cue_newprior.stan',
                     data = pheno_data, warmup=3000, iter = 4000)

#load(file = "output/output.joint.forcingchillingphoto.Aug6.40.Rda")
####################################################################
ssm <-  as.shinystan(mdl.jointfcp)
launch_shinystan(ssm)

sum.jfcp <- summary(mdl.jointfcp)$summary
post.f <- rstan::extract(mdl.jointfcp)

range(sum.jfcp[, "n_eff"]) #646.4967 23451.4129
range(sum.jfcp[, "Rhat"])

# some plots (from Lizzie, please move/edit as needed! I did not go through code below, where it may fit better)
sumer <- summary(mdl.jointfcp)$summary
plot(mu.study , sumer[grep("muStudy\\[", rownames(sumer)), "mean"])
abline(a = 0, b = 1, lty = "dotted") # not amazing ... but not horrible

plot(alpha.pheno.sp , (sumer[grep("mu_grand", rownames(sumer)), "mean"] + sumer[grep("muSp\\[", rownames(sumer)), "mean"]))
abline(a = 0, b = 1, lty = "dotted") # hmm, I must not be plotting the right things ...

par(mfrow=c(1,3))
hist(alpha.force.sp, breaks=20)
hist(alpha.chill.sp, breaks=20)
hist(alpha.photo.sp, breaks=20)

mean(alpha.force.sp)
mean(alpha.chill.sp)
mean(alpha.photo.sp)


####################################################################
# Is it giving me back the values? Generate the summary table
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

# ------------------------------------------------------------# 
mu_grand2.5 <- sum.jfcp[grep("mu_grand", rownames(sum.jfcp)), "2.5%"]
sigma_sp2.5 <- sum.jfcp[grep("sigma_sp", rownames(sum.jfcp)), "2.5%"]
sigma_studyesti2.5 <- sum.jfcp[grep("sigma_study", rownames(sum.jfcp)), "2.5%"]
sigmaTrait_y2.5 <- sum.jfcp[grep("sigmaTrait_y", rownames(sum.jfcp)), "2.5%"]

mu_chillsp2.5 <- sum.jfcp[grep("muChillSp", rownames(sum.jfcp)), "2.5%"]
sigma_chillsp2.5 <- sum.jfcp[grep("sigmaChillSp", rownames(sum.jfcp)), "2.5%"]
beta_tc2.5 <- sum.jfcp[grep("betaTraitxChill", rownames(sum.jfcp)), "2.5%"]

mu_photosp2.5 <- sum.jfcp[grep("muPhotoSp", rownames(sum.jfcp)), "2.5%"]
sigma_photosp2.5 <- sum.jfcp[grep("sigmaPhotoSp", rownames(sum.jfcp)), "2.5%"]
beta_tp2.5 <- sum.jfcp[grep("betaTraitxPhoto", rownames(sum.jfcp)), "2.5%"]

mu_forcesp2.5 <- sum.jfcp[grep("muForceSp", rownames(sum.jfcp)), "2.5%"]
mu_phenosp2.5 <- sum.jfcp[grep("muPhenoSp", rownames(sum.jfcp)), "2.5%"]
alpha.forcingsp2.5 <- sum.jfcp[grep("alphaForcingSp", rownames(sum.jfcp)), "2.5%"]
sigma_forcesp2.5 <- sum.jfcp[grep("sigmaForceSp", rownames(sum.jfcp)), "2.5%"]
sigma_phenosp2.5 <- sum.jfcp[grep("sigmaPhenoSp", rownames(sum.jfcp)), "2.5%"]
sigma_phenoy2.5 <- sum.jfcp[grep("sigmapheno_y", rownames(sum.jfcp)), "2.5%"]
beta_tf2.5 <- sum.jfcp[grep("betaTraitxForce", rownames(sum.jfcp)), "2.5%"]

# ------------------------------------------------------------# 
mu_grand97.5 <- sum.jfcp[grep("mu_grand", rownames(sum.jfcp)), "97.5%"]
sigma_sp97.5 <- sum.jfcp[grep("sigma_sp", rownames(sum.jfcp)), "97.5%"]
sigma_studyesti97.5 <- sum.jfcp[grep("sigma_study", rownames(sum.jfcp)), "97.5%"]
sigmaTrait_y97.5 <- sum.jfcp[grep("sigmaTrait_y", rownames(sum.jfcp)), "97.5%"]

mu_chillsp97.5 <- sum.jfcp[grep("muChillSp", rownames(sum.jfcp)), "97.5%"]
sigma_chillsp97.5 <- sum.jfcp[grep("sigmaChillSp", rownames(sum.jfcp)), "97.5%"]
beta_tc97.5 <- sum.jfcp[grep("betaTraitxChill", rownames(sum.jfcp)), "97.5%"]

mu_photosp97.5 <- sum.jfcp[grep("muPhotoSp", rownames(sum.jfcp)), "97.5%"]
sigma_photosp97.5 <- sum.jfcp[grep("sigmaPhotoSp", rownames(sum.jfcp)), "97.5%"]
beta_tp97.5 <- sum.jfcp[grep("betaTraitxPhoto", rownames(sum.jfcp)), "97.5%"]

mu_forcesp97.5 <- sum.jfcp[grep("muForceSp", rownames(sum.jfcp)), "97.5%"]
mu_phenosp97.5 <- sum.jfcp[grep("muPhenoSp", rownames(sum.jfcp)), "97.5%"]
alpha.forcingsp97.5 <- sum.jfcp[grep("alphaForcingSp", rownames(sum.jfcp)), "97.5%"]
sigma_forcesp97.5 <- sum.jfcp[grep("sigmaForceSp", rownames(sum.jfcp)), "97.5%"]
sigma_phenosp97.5 <- sum.jfcp[grep("sigmaPhenoSp", rownames(sum.jfcp)), "97.5%"]
sigma_phenoy97.5 <- sum.jfcp[grep("sigmapheno_y", rownames(sum.jfcp)), "97.5%"]
beta_tf97.5 <- sum.jfcp[grep("betaTraitxForce", rownames(sum.jfcp)), "97.5%"]

mdl.out <- data.frame( "Parameter" = c("mu_grand","sigma_sp","sigma_study", "sigmaTrait_y", 
                                       "mu_forcesp","mu_chillsp","mu_photosp","mu_phenosp","sigma_forcesp","sigma_chillsp","sigma_photosp", "sigma_phenosp", 
                                       "sigma_phenoy", "beta_tf", "beta_tc","beta_tp"),
                       "Test.data.values" = c(mu.grand, sigma.species, sigma.study, trt.var, 
                                              mu.force.sp, mu.chill.sp, mu.photo.sp, mu.pheno.sp, sigma.force.sp, sigma.chill.sp, sigma.photo.sp, sigma.pheno.sp, 
                                              sigma.gen, betaTraitxforce, betaTraitxchill, betaTraitxphoto),
                       "Estiamte" = c(mu_grand, sigma_sp, sigma_studyesti, sigmaTrait_y, 
                                      mu_forcesp, mu_chillsp, mu_photosp, mu_phenosp, sigma_forcesp, sigma_chillsp, sigma_photosp, sigma_phenosp, sigma_phenoy, beta_tf, beta_tc,  beta_tp),
                       "2.5" = c(mu_grand2.5, sigma_sp2.5, sigma_studyesti2.5, sigmaTrait_y2.5, 
                                 mu_forcesp2.5, mu_chillsp2.5, mu_photosp2.5, mu_phenosp2.5, sigma_forcesp2.5, sigma_chillsp2.5, sigma_photosp2.5, sigma_phenosp2.5, sigma_phenoy2.5, beta_tf2.5, beta_tc2.5,  beta_tp2.5),
                       "97.5" = c(mu_grand97.5, sigma_sp97.5, sigma_studyesti97.5, sigmaTrait_y97.5, 
                                  mu_forcesp97.5, mu_chillsp97.5, mu_photosp97.5, mu_phenosp97.5, sigma_forcesp97.5, sigma_chillsp97.5, sigma_photosp97.5, sigma_phenosp97.5, sigma_phenoy97.5, beta_tf97.5, beta_tc97.5,  beta_tp97.5)
                       
)

mdl.out
####################################################################

# Histograms of the model output
# poorly estimated parameters include:

#muSp 
h1 <- hist(rnorm(1000, 0, 20))
h2 <- hist(post.f$muSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#sigma_sp
h1 <- hist(rnorm(1000, 20, 10))
h2 <- hist(post.f$sigma_sp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,40))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#muStudy
h1 <- hist(rnorm(1000, 0, 10))
h2 <- hist(post.f$muStudy)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#betaTraitxForce
h1 <- hist(rnorm(1000, 0,1))
h2 <- hist(post.f$betaTraitxForce)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(-4, 4))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#betaTraitxChill
h1 <- hist(rnorm(1000, 0,1))
h2 <- hist(post.f$betaTraitxChill)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(-4, 4))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#betaTraitxPhoto
h1 <- hist(rnorm(1000, 0,1))
h2 <- hist(post.f$betaTraitxPhoto)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(-4, 4))
plot(h1, col=rgb(1,0,1,1/4), add = T)

# mu_phenosp:
h1 <- hist(rnorm(1000, 150, 10))
h2 <- hist(post.f$muPhenoSp)

plot(h2, col=rgb(0,0,1,1/4))
plot(h1, col=rgb(1,0,1,1/4), add = T)

plot(h2, col=rgb(0,0,1,1/4))
plot(h1, col=rgb(1,0,1,1/4), add = T)

# sigmaPhenoSp
h1 <- hist(rnorm(1000, 10, 1))
h2 <- hist(post.f$sigmaPhenoSp)

plot(h2, col=rgb(0,0,1,1/4))
plot(h1, col=rgb(1,0,1,1/4), add = T)

plot(h2, col=rgb(0,0,1,1/4))
plot(h1, col=rgb(1,0,1,1/4), add = T)

# Trying to understand why the muSp for different sp get such low n_eff
h1 <- hist(rnorm(1000, 0, 10))
plot(h1, col=rgb(1,0,1,1/4), xlim = c(-50, 50))
abline(v = post.f$muSp[1], lwd =2, col = "blue")
abline(v = post.f$muSp[2], lwd =2, col = "blue")
abline(v = post.f$muSp[3], lwd =2, col = "blue")
abline(v = post.f$muSp[26], lwd =2, col = "blue")
# interesting, the estimates are all close to 25-28

# And for muStudy
h1 <- hist(rnorm(1000, 0, 5))
plot(h1, col=rgb(1,0,1,1/4), xlim = c(-50,50))
abline(v = post.f$muStudy[1], lwd =2, col = "blue")
abline(v = post.f$muStudy[2], lwd =2, col = "blue")
abline(v = post.f$muStudy[3], lwd =2, col = "blue")
abline(v = post.f$muStudy[40], lwd =2, col = "blue")

# Faith suggested to make more plots! These are a work in progress
names(trt.dat)
musp.esti <- sum.jfcp[grep("muSp", rownames(sum.jfcp))]
mustudy.esti <- sum.jfcp[grep("muStudy", rownames(sum.jfcp))]
yhat.esti <- 
plot(unique(trt.dat$mu.trtsp) ~ musp.esti, pch =19)

plot(unique(trt.dat$mu.study) ~ mustudy.esti, pch = 19)

#hist of posterior and include visual to the predicted value
# if some parameters are really bad, plot parameters agains each 
# if mixed effect mdl: plot sigma and the pred dist values for that variable (eg sites on doy, sigma site vs pred doy) --> could need to plot the sigma on the log

# could go back to the one trait one cue model and see if the n_eff 


# ppc and trying to figure out what is going on! 
# y<- as.numeric(pheno.dat$doy.i)
# yrep<-post.f$ymu # I want this to be a matrix, which it is, with one element for each data point in y
# 
# ppc_dens_overlay(y, yrep[1:100, ]) # hmm the yrep does not appear
# plot(density(yrep))
# plot(density(y))

# stan_hist(mdl.jointfcp) # defualt is the firest 10 parameters
# stan_hist(mdl.jointfcp, pars = c("muForceSp","muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"))
# 
# 
# post.f2 <- as.matrix(mdl.jointfcp, par = c("muPhenoSp", "muForceSp",  "sigmaForceSp","muChillSp", "sigmaChillSp","muPhotoSp", "sigmaPhotoSp", "sigmaPhenoSp","sigmapheno_y"))
# 
# plot_title <- ggtitle("Posterior distributions",
#                       "with medians and 80% intervals")
# 
# mcmc_areas(post.f2,
#            pars = c("sigmaForceSp", "sigmaChillSp", "sigmaPhotoSp", "sigmaPhenoSp","sigmapheno_y"),
#            prob = 0.8) + plot_title
# 
# mcmc_areas(post.f2,
#            pars = c("muForceSp","muChillSp","muPhotoSp","muPhenoSp"),
#            prob = 0.8) + plot_title    
# 
