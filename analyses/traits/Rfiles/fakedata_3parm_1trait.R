# Date started: March 31, 2021
# The purpose of this code is to generate test data for the traitors model with all three climate parameters and a single trait, here we start with height:
# 
#  if(length(grep("deirdreloughnan", getwd()) > 0)) {
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

# May 7: making the number of species huge and the number of reps small
Nrep <- 5 # rep per trait
Nstudy <- 10 # number of studies w/ traits
Nspp <- 100 # number of species with traits

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
mu.grand <- 20 # the grand mean of the height model
# we want to keep the variaiton across spp. high, so I am keeping this at 10
sigma.species <- 1

mu.trtsp <- rnorm(Nspp, 0, sigma.species)
trt.dat$mu.trtsp <- rep(mu.trtsp, Nstudy) #adding ht data for ea. sp

#now generating the effects of study
sigma.study <- 5
mu.study <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$mu.study <- rep(mu.study, each = Nspp) # generate data for ea study

# general variance
trt.var <- 0.5 #sigmaTrait_y in the stan code
trt.dat$trt.er <- rnorm(Ntrt, 0, trt.var)

# generate yhat - heights -  for this first trt model
trt.dat$yTraiti <- mu.grand + trt.dat$mu.trtsp + trt.dat$mu.study + trt.dat$trt.er

# prior pred check
#build dataframe with length of iteraction, should have a prior for each parameter; have col. with iteration and save to each iteration, or could save as a list and bind at the end as dataframe, 
trait <- 1:10 #heights
mu.grand.prior <- rtruncnorm(1000, a = 0, b = 100 ,mean = 20, sd = 1) # since heights can't be negative, I am making this truncated
mu.trtsp.prior <- rnorm(1000, 0, 1)
mu.study.prior <- rnorm(1000, 0, 5)
sigma.sp.prior <- rnorm(1000,1, 0.5)
sigma.study.prior <- rnorm(1000, 1, 0.5)
sigmaTraity.prior <- rnorm(1000, 1, 0.5)

# start by setting up the dataframe
trt.ppc <- data.frame(cbind(rep(1:1000, times = length(trait)), rep(trait, times = 1000)))
names(trt.ppc) <- c("iteration","trait.value")
trt.ppc$trt.prior <- NA

for(i in 1:1000){
  traity.prior <- mu.grand.prior[i] + mu.trtsp.prior[i] + mu.study.prior[i] + sigmaTraity.prior[i] # yTraiti will be as long as x values
  trt.ppc$trt.prior[trt.ppc$iteration == i] <- traity.prior
  trt.ppc$mu.grand[trt.ppc$iteration == i] <- mu.grand.prior[i]
  trt.ppc$mu.trtsp[trt.ppc$iteration == i] <- mu.trtsp.prior[i]
  trt.ppc$mu.study[trt.ppc$iteration == i] <- mu.study.prior[i]
}

hist(trt.ppc$trt.prior)
hist(trt.ppc$mu.grand)
hist(trt.ppc$mu.trtsp)
hist(trt.ppc$mu.study)

temp1 <- subset(trt.ppc, iteration < 1000)
plot(temp1$trt.prior ~ temp1$mu.grand)
plot(temp1$trt.prior ~ temp1$mu.trtsp)
plot(temp1$trt.prior ~ temp1$mu.study)
#########################################################
# Next, making a data frame for the pheno data
 
nphen <- 10 # rep per pheno event 
Nph <- Nspp * nphen
Nph
pheno.dat <- data.frame(matrix(NA, Nph, 2))
names(pheno.dat) <- c("rep","species")
pheno.dat$rep <- c(1:Nph)
pheno.dat$species <- rep(c(1:Nspp), each = nphen)

# Generating data for the cues: this is the overall effect of each cue, not the species level effect
#in previous model runs making these values very small (0.1) resulted in divergent transitions
mu.force <- 20
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

# adding the species level differences
mu.pheno.sp <- 150
sigma.pheno.sp <- 2 #for a mu this large, I think this is pretty small

alpha.pheno.sp <- rnorm(Nspp, mu.pheno.sp, sigma.pheno.sp)
pheno.dat$alpha.pheno.sp <- rep(alpha.pheno.sp, each = nphen)

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

#interaction between trait and cues
#increasing this value from -0.8
betaTraitxchill <- -2
betaTraitxphoto<- -2
betaTraitxforce <- -2

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

# Prior predictive check for the phenology model:
#parameters:
force.i <- 1:10
chill.i <- 1:10 
photo.i <- 1:10

alpha.force.sp.prior <- rnorm(1000, -1, 2)
mu.force.sp.prior <- rnorm(1000, -1, 0.5)
sigma.force.sp.prior <- rnorm(1000, 2, 0.5)

alpha.chill.sp.prior <- rnorm(1000, -2, 2)
mu.chill.sp.prior <- rnorm(1000, -2, 0.5)
sigma.chill.sp.prior <- rnorm(1000, 2, 0.5)

alpha.photo.sp.prior <- rnorm(1000, -3, 2)
mu.photo.sp.prior <- rnorm(1000, -3, 0.5)
sigma.photo.sp.prior <- rnorm(1000, 2, 0.5)

alpha.pheno.sp.prior <- rnorm(1000, 150, 10) 
mu.pheno.sp.prior <- rnorm(1000, 150, 10)
sigma.pheno.sp.prior <- rnorm(1000, 2, 0.5)

betaTraitxForce.prior <- rnorm(1000, -2, 0.5)
betaTraitxChill.prior <- rnorm(1000, -2, 0.5)
betaTraitxPhoto.prior <- rnorm(1000, -2, 0.5)

sigma.pheno.y.prior <- rnorm(1000, 0.5, 0.5)

# start by setting up the dataframe
pheno.ppc <- data.frame(cbind(rep(1:1000, times = length(force.i)), rep(force.i, times = 1000), rep(chill.i, times = 1000), rep(photo.i, times = 1000)))
names(pheno.ppc) <- c("iteration","forcing", "chilling","photo")
pheno.ppc$doy.prior <- NA

for(i in 1:1000){
  beta.forcing.sp <- alpha.force.sp.prior[i] + betaTraitxForce.prior[i] + mu.trtsp.prior[i]
  beta.chilling.sp <- alpha.chill.sp.prior[i] + betaTraitxChill.prior[i] + mu.trtsp.prior[i]
  beta.photo.sp <- alpha.photo.sp.prior[i] + betaTraitxPhoto.prior[i] + mu.trtsp.prior[i]
  
  phenoy.prior <- alpha.pheno.sp.prior + beta.forcing.sp * force.i +
    beta.chilling.sp * chill.i + beta.photo.sp * photo.i + sigma.pheno.y.prior[i]
  pheno.ppc$doy.prior[pheno.ppc$iteration == i] <- phenoy.prior
  pheno.ppc$beta.forcing.sp[pheno.ppc$iteration == i] <- beta.forcing.sp
  pheno.ppc$beta.chilling.sp[pheno.ppc$iteration == i] <- beta.chilling.sp
  pheno.ppc$beta.photo.sp[pheno.ppc$iteration == i] <- beta.photo.sp
  pheno.ppc$alpha.pheno.sp[pheno.ppc$iteration == i] <- alpha.pheno.sp.prior
}
hist(pheno.ppc$doy.prior)

plot( 1, type = "n", xlim = c(-15,5), ylim = c(-400, 700), xlab = "Cue Value", ylab = "Day of bb")
points(pheno.ppc$beta.chilling.sp, pheno.ppc$doy.prior, col = "forestgreen", pch = 19)

plot( 1, type = "n", xlim = c(-15,10), ylim = c(-400, 700), xlab = "Cue Value", ylab = "Day of bb")
points(pheno.ppc$beta.forcing.sp, pheno.ppc$doy.prior, col = "purple", pch = 19)

plot( 1, type = "n", xlim = c(-15,5), ylim = c(-400, 700), xlab = "Cue Value", ylab = "Day of bb")
points(pheno.ppc$beta.photo.sp, pheno.ppc$doy.prior, col = "red", pch = 19)

plot( 1, type = "n", xlim = c(0,10), ylim = c(-400, 700), xlab = "Cue Value", ylab = "Day of bb")
points(pheno.ppc$chilling, pheno.ppc$doy.prior, col = "forestgreen", pch = 19)


plot( 1, type = "n", ylim = c(-25,20), xlim = c(-20,20), xlab = "Cue Value", ylab = "Day of bb")
points(pheno.ppc$beta.forcing.sp, pheno.ppc$beta.chilling.sp, col = "purple", pch = 19)
points(pheno.ppc$beta.forcing.sp, pheno.ppc$beta.photo.sp, col = "red", pch = 19)
points(pheno.ppc$beta.chilling.sp, pheno.ppc$beta.photo.sp, col = "goldenrod", pch = 19)

plot( 1, type = "n", xlim = c(0,65), ylim = c(-200, 200), xlab = "Cue Value", ylab = "Day of bb")
points(trt.ppc$trt.prior, pheno.ppc$doy.prior, col = "forestgreen", pch = 19)

plot( 1, type = "n", xlim = c(0,100), ylim = c(-500, 700), xlab = "Cue Value", ylab = "Day of bb")
fit.total <- lm( doy.prior ~ alpha.pheno.sp + beta.forcing.sp * forcing +
               beta.chilling.sp * chilling + beta.photo.sp * photo, data = pheno.ppc)
lines(fitted(fit.total))

plot( 1, type = "n", xlim = c(0,11), ylim = c(-100, 300), xlab = "Cue Value", ylab = "Day of bb")
for (i in 1:length(unique(pheno.ppc$iteration))){
  temp <- subset(pheno.ppc, iteration == i)
  fit <- lm( doy.prior ~ alpha.pheno.sp + beta.forcing.sp * forcing +
               beta.chilling.sp * chilling + beta.photo.sp * photo, data = temp)
  lines(fitted(fit))
  lines(fitted(fit.total), col= "red", lwd =2)
}

#################################################################################
head(trt.dat)
stan_data <- list(yTraiti = trt.dat$yTraiti, 
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

mdl.test <- stan('stan/stan_joint_traitors.stan',
                 data = stan_data, iter = 6000
                ,control = list(max_treedepth = 15))

save(mdl.test, file = "output.traitors.lgNspp_highdepth.Rda")

#load("output/output.traitors.lgNspp2.Rda")

ssm <-  as.shinystan(mdl.test)
launch_shinystan(ssm)

sumer <- summary(mdl.test)$summary
post <- rstan::extract(mdl.test)

# Making a table of the model output
mu_grand <- sumer[grep("mu_grand", rownames(sumer))]

muForceSp <- sumer[grep("muForceSp", rownames(sumer))]
muChillSp <- sumer[grep("muChillSp", rownames(sumer))]
muPhotoSp <- sumer[grep("muPhotoSp", rownames(sumer))]
muPhenoSp <- sumer[grep("muPhenoSp", rownames(sumer))]

betaTraitxForcing <- sumer[grep("betaTraitxForcing", rownames(sumer))]
betaTraitxChill <- sumer[grep("betaTraitxChill", rownames(sumer))]
betaTraitxPhoto <- sumer[grep("betaTraitxPhoto", rownames(sumer))]

sigma_sp <- sumer[grep("sigma_sp", rownames(sumer))]
sigma_study_esti <- sumer[grep("sigma_study", rownames(sumer))]
sigmaTrait_y <- sumer[grep("sigmaTrait_y", rownames(sumer))]
sigmapheno_y <- sumer[grep("sigmapheno_y", rownames(sumer))]

sigmaForceSp <- sumer[grep("sigmaForceSp", rownames(sumer))]
sigmaChillSp <- sumer[grep("sigmaChillSp", rownames(sumer))]
sigmaPhotoSp <- sumer[grep("sigmaPhotoSp", rownames(sumer))]
sigmaPhenoSp <- sumer[grep("sigmaPhenoSp", rownames(sumer))]

# mdl.out <- data.frame( "Parameter" = c("mu_grand","muForceSp","muChillSp","muPhotoSp","muPhenoSp",
#                                        "betaTraitxForcing","betaTraitxChill","betaTraitxPhoto","sigma_sp",
#                                        "sigma_study", "sigmaTrait_y", "sigmapheno_y",
#                                        "sigmaForceSp", "sigmaChillSp", "sigmaPhotoSp", "sigmaPhenoSp", "sigma_study"),
#                        "Test.data.values" = c(mu.grand, mu.force.sp, mu.chill.sp, mu.photo.sp, mu.pheno.sp,
#                                    betaTraitxforce, betaTraitxchill, betaTraitxphoto,sigma.species,
#                                    sigma.study, trt.var, sigma.gen,
#                                    sigma.force.sp, sigma.chill.sp, sigma.photo.sp, sigma.pheno.sp, sigma.study),
#                        "Estiamte" = c(mu_grand, muForceSp, muChillSp, muPhotoSp, muPhenoSp,
#                                       betaTraitxForcing, betaTraitxChill, betaTraitxPhoto, sigma_sp,
#                                       sigma_study, sigmaTrait_y, sigmapheno_y,
#                                       sigmaForceSp, sigmaChillSp, sigmaPhotoSp, sigmaPhenoSp, sigma_study_esti))
# 
# mdl.out

# # #
y<-trt.dat$yTraiti
yrep<-post$ymu # I want this to be a matrix, which it is, with one element for each data point in y

ppc_dens_overlay(y, yrep[1:50, ])
#
# #model 1
#
plot(density(post$sigmaTrait_y )) ; abline(v = trt.var, col = "red")
plot(density(post$sigma_sp)); abline(v = sigma.trtsp, col = "red")
plot(density(post$sigma_stdy), xlim = c(0, 9)); abline(v = mean(sigma.study), col = "red")
plot(density(post$muStdy ), xlim = c(0, 30)); abline(v = 0, col = "red")
plot(density(post$muSp )); abline(v = 0, col = "red")

#model 2
plot(density(post$alphaForcingSp)); abline(v = mu.force.sp, col = "red")
plot(density(post$alphaChillSp)); abline(v = mu.chill.sp, col = "red")
plot(density(post$alphaPhotoSp)); abline(v = mu.photo.sp, col = "red")
plot(density(post$sigmapheno_y )); abline(v = sigma.gen, col = "red")

plot(density(post$betaTraitxForcing), xlim = c(-5,1)); abline(v = betaTraitxforce, col = "red")
plot(density(post$betaTraitxPhoto), xlim = c(-5,1)); abline(v = betaTraitxphoto, col = "red")
plot(density(post$betaTraitxChill), xlim = c(-5,1)); abline(v = betaTraitxchill, col = "red")

plot(density(post$muForceSp));  abline(v = mu.force.sp, col = "red")
plot(density(post$sigmaForceSp)); abline(v = sigma.force, col = "red")

plot(density(post$muChillSp)) ; abline(v = mu.chill.sp, col = "red")
plot(density(post$sigmaChillSp)); abline(v = sigma.chill, col = "red")

plot(density(post$muPhotoSp));  abline(v = mu.photo.sp, col = "red")
plot(density(post$sigmaPhotoSp)) ; abline(v = sigma.photo, col = "red")

######################################################################
#######################################################################################
# Is the issue the test data? Lizzie suggested running the data with linear models

require(lme4)
# It is unclear to me how to incorporate a grand mean into a linear model, isn't it just adding one value?
fm.trait <- lmer(yTraiti ~ mu.grand + (1| mu.trtsp) + (1|mu.study), data = trt.dat)
#Error in model.frame.default(data = trt.dat, drop.unused.levels = TRUE,  : variable lengths differ (found for 'mu.grand')
fm.trait <- lmer(yTraiti ~ 1 + (1| mu.trtsp) + (1|mu.study), data = trt.dat)
summary(fm.trait)
coef(fm.trait)

#bc setting betaTraitxforce to zero, btf will be equal to the cue level variation
betaTraitxchill <- 0
betaTraitxphoto <- 0
betaTraitxforce <- 0

beta.force.sp <- pheno.dat$alpha.force.sp + trt.dat$mu.trtsp*betaTraitxforce
beta.chill.sp <- pheno.dat$alpha.chill.sp + trt.dat$mu.trtsp*betaTraitxchill
beta.photo.sp <- pheno.dat$alpha.photo.sp + trt.dat$mu.trtsp*betaTraitxphoto

fm.pheno <- lmer(doy.i ~ force.i*beta.force.sp + photo.i*beta.photo.sp + chill.i*beta.chill.sp + (1|species), data = pheno.dat)
#Error in model.frame.default(data = pheno.dat, drop.unused.levels = TRUE,: variable lengths differ (found for 'beta.chill.sp')
# doesn't work bc the trt and the pheno datasets are different lengths

# BUT becuase the beta.forcing.sp is just alpha.force.sp when betaTraitxForce is 0, I am subbing it in directly
fm.pheno <- lmer(doy.i ~ force.i*alpha.force.sp + photo.i*alpha.photo.sp + chill.i*alpha.chill.sp + (1|species), data = pheno.dat)
summary(fm.pheno)


