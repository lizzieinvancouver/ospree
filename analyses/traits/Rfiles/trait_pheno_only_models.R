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

# 50 study, spp, reps, drop sigmatrait_y to 1
# boxplots of all the dots by species and study
#compare sp to study and make sure no correlations
# run test data through rstan arm - how fast? accurate? 
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

Nrep <- 50 # rep per trait
Nstudy <- 50 # number of studies w/ traits (10 seems a little low for early simulation code; remember that you are estimating a distribution of this the same as for species)
Nspp <- 50 # number of species with traits (making this 20 just for speed for now)

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
sigma.study <- 1
mu.study <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$mu.study <- rep(mu.study, each = Nspp) # generate data for ea study

# general variance
trt.var <- 1 #sigmaTrait_y in the stan code
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

mdl.traitonly <- stan('stan/joint_traitonly.stan',
                      data = trait_data, iter = 4000, warmup = 3000)

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
save(mdl.traitonly, file = "output.traitonly.Sept1.Rda")
# Parameter Test.data.values  Estiamte
# 1     mu_grand               10  9.993991
# 2     sigma_sp               10 10.075637
# 3  sigma_study                5  4.937649
# 4 sigmaTrait_y               15 15.140083

# Sept 1: the priors are too narrow, need to try to get it working for wider priors and see if I can fix the issues with n_eff for study and speices

####################################################################

load(file = "output/output.traitonly.Sept1.Rda")
ssm <-  as.shinystan(mdl.traitonly)
launch_shinystan(ssm)

sumer <- summary(mdl.traitonly)$summary
post <- rstan::extract(mdl.traitonly)

range(sumer[, "n_eff"])
range(sumer[, "Rhat"])

#pdf("standis_study.pdf", width=5, height=5)
plot(mu.study , sumer[grep("muStudy\\[", rownames(sumer)), "mean"])
abline(a = 0, b = 1, lty = "dotted") # not amazing ... but not horrible
#dev.off()

#pdf("standis_species.pdf", width=5, height=5)
plot(mu.trtsp , (sumer[grep("mu_grand", rownames(sumer)), "mean"] + sumer[grep("muSp\\[", rownames(sumer)), "mean"]))
abline(a = 0, b = 1, lty = "dotted")
#dev.off()

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

mu_grand2.5 <- sumer[grep("mu_grand", rownames(sumer)), "2.5%"]
sigma_sp2.5 <- sumer[grep("sigma_sp", rownames(sumer)), "2.5%"]
sigma_studyesti2.5 <- sumer[grep("sigma_study", rownames(sumer)), "2.5%"]
sigmaTrait_y2.5 <- sumer[grep("sigmaTrait_y", rownames(sumer)), "2.5%"]

# ------------------------------------------------------------# 
mu_grand97.5 <- sumer[grep("mu_grand", rownames(sumer)), "97.5%"]
sigma_sp97.5 <- sumer[grep("sigma_sp", rownames(sumer)), "97.5%"]
sigma_studyesti97.5 <- sumer[grep("sigma_study", rownames(sumer)), "97.5%"]
sigmaTrait_y97.5 <- sumer[grep("sigmaTrait_y", rownames(sumer)), "97.5%"]

mdl.out <- data.frame( "Parameter" = c("mu_grand","sigma_sp","sigma_study", "sigmaTrait_y"),
                       "Test.data.values" = c(mu.grand, sigma.species, sigma.study, trt.var),
                       "Estiamte" = c(mu_grand, sigma_sp, sigma_studyesti, sigmaTrait_y),
                       "2.5" = c(mu_grand2.5, sigma_sp2.5, sigma_studyesti2.5, sigmaTrait_y2.5),
                       "97.5" = c(mu_grand97.5, sigma_sp97.5, sigma_studyesti97.5, sigmaTrait_y97.5))

mdl.out

# Histograms of the model output
# poorly estimated parameters include:
# mu_grand
h1 <- hist(rnorm(1000, 10, 20))
h2 <- hist(post$muSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-100,100))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#sigmaTrait_y
h1 <- hist(rnorm(1000, 1, 1))
h2 <- hist(post$sigmaTrait_y)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-10,10))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#muSp 
h1 <- hist(rnorm(1000, 0, 10))
h2 <- hist(post$muSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-100,100))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#sigma_sp
h1 <- hist(rnorm(1000, 10, 10))
h2 <- hist(post$sigma_sp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,40))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#muStudy
h1 <- hist(rnorm(1000, 0, 10))
h2 <- hist(post$muStudy)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#sigma_study
h1 <- hist(rnorm(1000, 1,10))
h2 <- hist(post$sigma_study)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-5,5))
plot(h1, col=rgb(1,0,1,1/4), add = T)


# mu_grand is still not great: but does that look that bad?
#plot(density(rnorm(1000, 10, 0.5)), col = "red", xlim = c(0,20),  ylim = c(0, 0.8)); lines(density(post$mu_grand ), lty = 1)

# The odd effect from increasing the Nspp and Nstudy, is weird, but could try upping the warm up, could just be a scaling issue.
# could change the number and see if the value changes

##########################################################################
##########################################################################
##########################################################################
#increase Nspp and see if closer 150
# check the internal values for indiv spp. 
# look at the log posterior, bivariate plots: mu_phenosp on y and log_post on x
# always check the other parameter if a combination of 2 parameters

Nspp <- 40 # number of species with traits (making this 20 just for speed for now)
nphen <- 30 # rep per pheno event 
Nph <- Nspp * nphen
Nph

# the trait effect
sigma.species <- 1
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

mu.force.sp <- -2 
sigma.force.sp <- 1 # 1 --> 50 --> 10
alpha.force.sp <- rnorm(Nspp, mu.force.sp, sigma.force.sp)
pheno.dat$alpha.force.sp <- rep(alpha.force.sp, each = nphen)

betaTraitxforce <- 0 #interaction between trait and phenology

beta.force.temp <- alpha.force.sp + alpha.trait.sp * betaTraitxforce
beta.force.sp <- rep(beta.force.temp,)
pheno.dat$beta.force.sp <- rep(beta.force.sp, each = nphen)

#Generate the cue values (ie the F that gets multipled with betaForcing{sp})
mu.force <- 1 # This is a big forcing effect.  turned down to 5
sigma.force <- 1
force.i <- rnorm(Nph, mu.force, sigma.force)  # predictor frocing, forcei in stan
pheno.dat$force.i <- force.i

#general variance
sigma.gen <- 1
gen.var <- rnorm(Nph, 0, sigma.gen) 
pheno.dat$gen.er <- gen.var

#"run" the full model to simulate data 
pheno.dat$doy.i <- pheno.dat$alpha.pheno.sp + pheno.dat$beta.force.sp * pheno.dat$force.i + pheno.dat$gen.er

### Phenology only stan model ############################################
# For future when we think the linear model works! 
force.z <- (force.i-mean(force.i,na.rm=TRUE))/sd(force.i,na.rm=TRUE)

pheno_data <- list(n_spec = Nspp, 
                   species = pheno.dat$species, 
                   yPhenoi = pheno.dat$doy.i, 
                   alphaTraitSp = alpha.trait.sp,
                   Nph = Nph, 
                   forcei = force.z) 

mdl.pheno <- stan('stan/joint_phenoonly_aug15.stan',
                  data = pheno_data, iter = 4000)


#save(mdl.pheno, file = "output.phenoonly.6.Rda")

sum.p <- summary(mdl.pheno)$summary

# Is it giving me back the values?
mu_forcesp <- sum.p[grep("muForceSp", rownames(sum.p))]
mu_phenosp <- sum.p[grep("muPhenoSp", rownames(sum.p))]
alpha.forcingsp <- sum.p[grep("alphaForcingSp", rownames(sum.p))]
sigma_forcesp <- sum.p[grep("sigmaForceSp", rownames(sum.p))]
sigma_phenosp <- sum.p[grep("sigmaPhenoSp", rownames(sum.p))]
sigma_phenoy <- sum.p[grep("sigmapheno_y", rownames(sum.p))]
beta_tp <- sum.p[grep("betaTraitxForce", rownames(sum.p))]

mdl.out <- data.frame( "Parameter" = c("mu_forcesp","mu_phenosp","sigma_forcesp","sigma_phenosp", 
                                       "sigma_phenoy", "beta_tp"),
                       "Test.data.values" = c(mu.force.sp, mu.pheno.sp, sigma.force.sp, sigma.pheno.sp, 
                                              sigma.gen, betaTraitxforce),
                       "Estiamte" = c(mu_forcesp, mu_phenosp, sigma_forcesp, sigma_phenosp, sigma_phenoy, beta_tp))

mdl.out

# June 11: Initially the model runs with no issues, but it does a poor job of predicting mu_phenosp, sigmaFsp, sigma_phenosp, sigma_phenoy and the beta_tp
# Increased the Nspp to 30 and the mu_phenosp value did get closser to 150! The other values are still pretty close, changed prior to (150,20) 
#Increaing the variance on the 20 results in less accurate estimates of forcing
# 3.  Nspp = 20, muPheno 150, sigmPheno.sp =10, everything great, excetp mupheno so 147
#save(mdl.pheno, file = "output.phenoonly.3.Rda")

#4. Nspp = 30,  muPheno 150, sigmPheno.sp =10, some estimates are off by 0.2-0.3, but muphenosp is still 147

#5. Making muPheno more realistic (30, 10) Nspp 20, mufocesp way lower at -0.76, and other values off by 0.2-0.5, but muphenosp is good at 30.99

#6. making muPheno more realistic (30, 10), with Nspp of 30:
#       Parameter Test.data.values  Estiamte
# 1    mu_forcesp               -1 -1.083666
# 2    mu_phenosp               30 27.442441
# 3 sigma_forcesp                2  2.068836
# 4 sigma_phenosp               10  9.916716
# 5  sigma_phenoy                5  5.100674
# 6       beta_tp                2  2.005857
save(mdl.pheno, file = "output.phenoonly.6.Rda")
# Lizzie commented that this can't be scaled directly, so to see it is important we could have to increase the reps or Nspp, I increased the reps to 15: it kinda just made everything worse
# Parameter Test.data.values  Estiamte
# 1    mu_forcesp               -1 -1.292711
# 2    mu_phenosp               30 33.991997
# 3 sigma_forcesp                2  1.711115
# 4 sigma_phenosp               10  9.911498
# 5  sigma_phenoy                5  5.123357
# 6       beta_tp                2  2.067010

# 7. Can I improve the estimates for the mu.pheno.sp by decreasing the variance to 5? 
# Parameter Test.data.values  Estiamte
#sigmaPhenoSp ~ normal(5, 0.5);   muPhenoSp ~ normal(30, 5);  
# 1    mu_forcesp               -1 -0.718529
# 2    mu_phenosp               30 29.459860
# 3 sigma_forcesp                2  2.071941
# 4 sigma_phenosp                5  5.041610
# 5  sigma_phenoy                5  4.810782
# 6       beta_tp                2  2.148108

# sigmaPhenoSp ~ normal(5, 0.5);   muPhenoSp ~ normal(30, 10);  
# Parameter Test.data.values  Estiamte
# 1    mu_forcesp               -1 -1.143172
# 2    mu_phenosp               30 31.531197
# 3 sigma_forcesp                2  2.119935
# 4 sigma_phenosp                5  5.122597
# 5  sigma_phenoy                5  4.771651
# 6       beta_tp                2  2.095064

# sigmaPhenoSp ~ normal(10, 0.5);   muPhenoSp ~ normal(30, 5);  wow the mu.force.sp is terrible, only -0.368, other values look fine


####################################################################
ssm <-  as.shinystan(mdl.pheno)
launch_shinystan(ssm)

sum.p <- summary(mdl.pheno)$summary
post.p <- rstan::extract(mdl.pheno)


range(sum.p[, "n_eff"])
range(sum.p[, "Rhat"])

# ppc and trying to figure out what is going on! 
y<- as.numeric(pheno.dat$doy.i)
yrep<-post.p$ypred # I want this to be a matrix, which it is, with one element for each data point in y

ppc_dens_overlay(y, yrep[1:100, ]) # hmm the yrep does not appear
plot(density(yrep))
plot(density(y))

stan_hist(mdl.pheno) # defualt is the firest 10 parameters
stan_hist(mdl.pheno, pars = c("muForceSp","muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"))

stan_hist(mdl.pheno, pars = "alphaForceSp")
post.p2 <- as.matrix(mdl.pheno, par = c("muForceSp", "muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(post.p2,
           pars = c("muForceSp","muPhenoSp", "sigmaForceSp", "sigmaPhenoSp","sigmapheno_y"),
           prob = 0.8) + plot_title

mcmc_areas(post.p2,
           pars = c("muForceSp","muPhenoSp"),
           prob = 0.8) + plot_title    

mcmc_areas(post.p2,
           pars = c("sigmaForceSp","sigmaPhenoSp","sigmapheno_y"),
           prob = 0.8) + plot_title        

sum.p <- summary(mdl.pheno)$summary

# Is it giving me back the values?
mu_forcesp <- sum.p[grep("muForceSp", rownames(sum.p))]
mu_phenosp <- sum.p[grep("muPhenoSp", rownames(sum.p))]
alpha.forcingsp <- sum.p[grep("alphaForcingSp", rownames(sum.p))]
sigma_forcesp <- sum.p[grep("sigmaForceSp", rownames(sum.p))]
sigma_phenosp <- sum.p[grep("sigmaPhenoSp", rownames(sum.p))]
sigma_phenoy <- sum.p[grep("sigmapheno_y", rownames(sum.p))]
beta_tp <- sum.p[grep("betaTraitxForce", rownames(sum.p))]

mdl.out <- data.frame( "Parameter" = c("mu_forcesp","mu_phenosp","sigma_forcesp","sigma_phenosp", 
                                       "sigma_phenoy", "beta_tp"),
                       "Test.data.values" = c(mu.force.sp, mu.pheno.sp, sigma.force.sp, sigma.pheno.sp, 
                                              sigma.gen, betaTraitxforce),
                       "Estiamte" = c(mu_forcesp, mu_phenosp, sigma_forcesp, sigma_phenosp, sigma_phenoy, beta_tp))

mdl.out

h1 <- hist(rnorm(1000, -2,1))
h2 <- hist(post.p$muForceSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-5,5))
plot(h1, col=rgb(1,0,1,1/4), add = T)

h1 <- hist(rnorm(1000, 1,10))
h2 <- hist(post.p$sigmaForceSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-25,25))
plot(h1, col=rgb(1,0,1,1/4), add = T)



# mu_grand is still not great: but does that look that bad?
plot(density(rnorm(1000, 10, 0.5)), col = "red", xlim = c(0,20),  ylim = c(0, 0.8)); lines(density(post$mu_grand ), lty = 1)
