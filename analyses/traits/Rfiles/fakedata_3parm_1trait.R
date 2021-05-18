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
mu.grand <- 20 # the grand mean of the trait model
# May 7: we want to keep the variaiton across spp. high, so I am keeping this at 10
sigma.species <- 10 

alpha.trtsp <- rnorm(Nspp, 0, sigma.species)
trt.dat$alpha.trtsp <- rep(alpha.trtsp, Nstudy) #adding ht data for ea. sp

#now generating the effects of study
sigma.study <- 5
alpha.study <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$alpha.study <- rep(alpha.study, each = Nspp) # generate data for ea study

# general variance
trt.var <- 0.5 #sigmaTrait_y in the stan code?
trt.dat$trt.er <- rnorm(Ntrt, 0, trt.var)

# generate yhat for this first trt model
trt.dat$yTraiti <- mu.grand + trt.dat$alpha.trtsp + trt.dat$alpha.study + trt.dat$trt.er
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
# May 7: in previous model runs making these values very small (0.1) resulted in divergent transitions
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

#interaction between trait and phenology
# May 7: increasing this value from -0.8
betaTraitxchill <- -2
betaTraitxphoto<- -2
betaTraitxforce <- -2

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
# May 7: making this value, the general noise in the phenology model much smaller
sigma.gen <- 0.5
gen.var <- rnorm(Nph, 0, sigma.gen) 
pheno.dat$gen.er <- gen.var

#"run" the full model to simulate data 
pheno.dat$doy.i <- pheno.dat$alpha.pheno.sp + pheno.dat$beta.forcing.sp * pheno.dat$forcingi +
  pheno.dat$beta.chilling.sp * pheno.dat$chillingi + pheno.dat$beta.photo.sp * pheno.dat$photoi + pheno.dat$gen.er

#######################################################################################
# Is the issue the test data? Lizzie suggested running the data with linear models
betaTraitxchill <- 0
betaTraitxphoto<- 0
betaTraitxforce <- 0

require(lme4)
# I don't think this is the correct way to write the first model in a linear equation
#fm.trait <- lmer(yTraiti~ mu.grand + (1| species) + (1|study), data = trt.dat)
#fm.trait <- lmer(yTraiti~ mu.grand + alpha.species + alpha.study + (1| species) + (1|study), data = trt.dat)

fm.trait <- lmer(yTraiti~ 1 + (1| species) + (1|study), data = trt.dat)
summary(fm.trait)

#bc setting betaTraitxforce to zero, btf will be equal to the cue level variation
# btf <- alphaf.sp[i] + betaTraitxforce * (mu_grand[i] + mu.sp.splvl[i])
# btc <- alphac.sp[i] + betaTraitxchill * (mu_grand[i] + mu.sp.splvl[i])
# btp <- alphap.sp[i] + betaTraitxphoto * (mu_grand[i] + mu.sp.splvl[i])

fm.pheno <- lmer(doy.i ~ forcingi + photoi + chillingi + (1|species), data = pheno.dat)
summary(fm.pheno)
#######################################################################################
head(trt.dat)
stan_data <- list(yTraiti = trt.dat$yTraiti, 
                  N = Ntrt, 
                  n_spec = Nspp, 
                  species = trt.dat$species, 
                  study = trt.dat$study, 
                  n_study = Nstudy, 
                  yPhenoi = pheno.dat$doy.i, 
                  Nph = Nph, 
                  forcei = forcingi,
                  photoi = photoi, 
                  chilli = chillingi,
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
sigma_study <- sumer[grep("sigma_study", rownames(sumer))]
sigmaTrait_y <- sumer[grep("sigmaTrait_y", rownames(sumer))]
sigmapheno_y <- sumer[grep("sigmapheno_y", rownames(sumer))]

sigmaForceSp <- sumer[grep("sigmaForceSp", rownames(sumer))]
sigmaChillSp <- sumer[grep("sigmaChillSp", rownames(sumer))]
sigmaPhotoSp <- sumer[grep("sigmaPhotoSp", rownames(sumer))]
sigmaPhenoSp <- sumer[grep("sigmaPhenoSp", rownames(sumer))]

mdl.out <- data.frame( "Parameter" = c("mu_grand","muForceSp","muChillSp","muPhotoSp","muPhenoSp",
                                       "betaTraitxForcing","betaTraitxChill","betaTraitxPhoto","sigma_sp",
                                       "sigma_study", "sigmaTrait_y", "sigmapheno_y",
                                       "sigmaForceSp", "sigmaChillSp", "sigmaPhotoSp", "sigmaPhenoSp"),
                       "Test.data.values" = c(mu.grand, mu.force.sp, mu.chill.sp, mu.photo.sp, mu.pheno.sp,
                                   betaTraitxforce.test, betaTraitxchill.test, betaTraitxphoto.test,sigma.species,
                                   sigma.study, trt.var, sigma.gen,
                                   sigma.force.sp, sigma.chill.sp, sigma.photo.sp, sigma.pheno.sp),
                       "Estiamte" = c(mu_grand,muForceSp,muChillSp,muPhotoSp,muPhenoSp,
                                      betaTraitxForcing,betaTraitxChill,betaTraitxPhoto,sigma_sp,
                                      sigma_study, sigmaTrait_y, sigmapheno_y,
                                      sigmaForceSp, sigmaChillSp, sigmaPhotoSp, sigmaPhenoSp))

mdl.out

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
# checking to see if it is an identifiability issue:
# simulate the model using the true values and generate a set of outcomes
# simulate the model with the wrong values that the model is producing
# plot the densities of these two things, if they overlap a lot then we know different values are producing the same outcome, 

# starting with the trait model:
trait <- 1:5

mu.grand <- 20
mu.sp <- rnorm(1000, 0,10)
mu.study <- rnorm(1000, 0, 5)

sigmatrait.y <- rnorm(1000, 2, 0.5)
sigma.sp <- rnorm(1000, 10, 2)
sigma.study <- rnorm(1000, 5,1)

known.values <- data.frame(cbind(rep(1:1000, times = length(trait)), rep(trait, times = 1000)))
names(known.values) <- c("iteration","trait.value")
known.values$pred.y <- NA

for(i in 1:1000){
  ymu <- mu.grand + mu.sp[i] + mu.study[i]
  yhat <- rnorm(1000, ymu, sigmatrait.y[i]) # is it correct to have the 1000 there? It doesn't work otherwise
  known.values$pred.y[known.values$iteration == i] <- yhat
}
head(known.values)
plot(density(known.values$pred.y))

plot(density(post$ymu))
points(density(known.values$pred.y), pch=20, col ="red")

## But the above doesn't account for the species or study level effects, should it be something like
mu.grand <- 20
mu.sp <- 0
mu.study <- 0
mu.sp.splvl <- rnorm(1000, mu.sp, sigma.sp)
mu.study.stlvl <- rnorm(1000,mu.sp, sigma.study)
ymu <- mu.grand[1] + mu.sp.splvl[1] + mu.study.stlvl[1]
yhat <- rnorm(1000,ymu, sigmatrait.y[1]) # why does this generate 19 values unless 1000 are specified?

# now let's try generating values for the betaCueSp values

mu.force.sp <- -1
sigma.force.sp <- 4
alphaf.sp <- rnorm(1000, mu.force.sp, sigma.force.sp)

mu.chill.sp <- -2
sigma.chill.sp <- 3
alphac.sp <- rnorm(1000, mu.chill.sp, sigma.chill.sp)

mu.photo.sp <- -2
sigma.photo.sp <- 2.4
alphap.sp <- rnorm(1000, mu.photo.sp, sigma.photo.sp)

#interaction between trait and phenology?
betaTraitxchill <- -.8
betaTraitxphoto<- -.8
betaTraitxforce <- -.8

betaForcingSp <- alphaf.sp[1] + betaTraitxforce * (mu.grand[1] + mu.sp.splvl[1])
betaChillingSp <- alphac.sp[1] + betaTraitxchill * (mu.grand[1] + mu.sp.splvl[1])
betaPhotoSp <- alphap.sp[1] + betaTraitxphoto * (mu.grand[1] + mu.sp.splvl[1])

mu.force <- 20
sigma.force <- 5
forcingi <- rnorm(1000, mu.force, sigma.force)

mu.chill <- 20
sigma.chill <- 5
chillingi <- rnorm(1000, mu.chill, sigma.chill)

mu.photo <- 20
sigma.photo <- 5
photoi <- rnorm(1000, mu.photo, sigma.photo)

mu.pheno.sp <- 150
sigma.pheno.sp <- 2
alpha.pheno.sp <- rnorm(1000, mu.pheno.sp, sigma.pheno.sp)

sigmatrait.y <- 2
betaForcingSp
ypheno <- alpha.pheno.sp[1] + betaForcingSp[1] * forcingi[1] + betaPhotoSp[1] * photoi[1] + betaChillingSp[1] * chillingi[1]
yhat <- rnorm(1000, ypheno, sigma.gen) # sigma.gen = 2

# or are the betaForcingSp supposed to be alpha.force.sp + alpha.trtsp*betaTraitxforce

#combine the effects of forcing and species trait differences into a slope
beta.forcing.sp <- alphaf.sp + alpha.trtsp*betaTraitxforce
beta.chilling.sp <- alpha.chill.sp + alpha.trtsp*betaTraitxchill
beta.photo.sp <- alpha.photo.sp + alpha.trtsp*betaTraitxphoto

# finally generate data for the phenology model
# now lets loop it
known.trait <- data.frame(cbind(rep(1:1000, times = length(trait)), rep(trait, times = 1000)))
names(known.trait) <- c("iteration","trait.value")
known.trait$pred.trait <- NA

for(i in 1:1000){
  ymu <- mu.grand + mu.sp[i] + mu.study[i]
  yhat <- rnorm(1000, ymu, sigmatrait.y[i]) # is it correct to have the 1000 there? It doesn't work otherwise
  known.values$pred.y[known.values$iteration == i] <- yhat
}
head(known.values)

for(i in 1:1000){
  ymu <- mu.grand + mu.sp.splvl[i] + mu.study.stlvl[i]
  yhat <- rnorm(1000, ymu, sigmatrait.y[1]) # is it correct to have the 1000 there? It doesn't work otherwise
  known.trait$pred.trait[known.trait$iteration == i] <- yhat

  btf <- alphaf.sp[i] + betaTraitxforce * (mu_grand + mu.sp.splvl[i])
  btc <- alphac.sp[i] + betaTraitxchill * (mu_grand + mu.sp.splvl[i])
  btp <- alphap.sp[i] + betaTraitxphoto * (mu_grand + mu.sp.splvl[i])
  known.trait$pred.betaTforce[known.trait$iteration == i] <- btf
  known.trait$pred.betaTchill[known.trait$iteration == i] <- btc
  known.trait$pred.betaTphoto[known.trait$iteration == i] <- btp

  ypheno <- alpha.pheno.sp[1] + btf[1] * forcingi[1] + btp[1] * photoi[1] + btc[1] * chillingi[1]
  yhat.pheno <- rnorm(1000, ypheno, sigma.gen) # sigma.gen = 2
  known.trait$pred.pheno[known.trait$iteration == i] <- yhat.pheno

}
head(known.trait)
####################################################################################
####################################################################################
####################################################################################
 # now repeating for the values produced from the model
mu.grand <- 20
mu.sp <- 0
mu.study <- 0
mu.sp.splvl <- rnorm(1000, mu.sp, sigma.sp)
mu.study.stlvl <- rnorm(1000, mu.sp, sigma.study)

sigmatrait.y <- rnorm(1000, 2, 0.5)
sigma.sp <- 10.5
sigma.study <-  5.8

mu.force.sp <- -0.7
sigma.force.sp <- 4.7
alphaf.sp <- rnorm(1000, mu.force.sp, sigma.force.sp)

mu.chill.sp <- -2
sigma.chill.sp <- 3.9
alphac.sp <- rnorm(1000, mu.chill.sp, sigma.chill.sp)

mu.photo.sp <- -1.5
sigma.photo.sp <- 3.6
alphap.sp <- rnorm(1000, mu.photo.sp, sigma.photo.sp)

#interaction between trait and phenology?
betaTraitxchill <- -.29
betaTraitxphoto<- -.23
betaTraitxforce <- -.39

mu.force <- 20
sigma.force <- 5
forcingi <- rnorm(1000, mu.force, sigma.force)

mu.chill <- 20
sigma.chill <- 5
chillingi <- rnorm(1000, mu.chill, sigma.chill)

mu.photo <- 20
sigma.photo <- 5
photoi <- rnorm(1000, mu.photo, sigma.photo)

mu.pheno.sp <- 151
sigma.pheno.sp <- 1.9
alpha.pheno.sp <- rnorm(1000, mu.pheno.sp, sigma.pheno.sp)

sigma.gen <- 2.11

ypheno <- alpha.pheno.sp[1] + betaForcingSp[1] * forcingi[1] + betaPhotoSp[1] * photoi[1] + betaChillingSp[1] * chillingi[1]
yhat <- rnorm(1000, ypheno, sigma.gen) # sigma.gen = 2

# finally generate data for the phenology model
# now lets loop it
mdl.esti <- data.frame(cbind(rep(1:1000, times = length(trait)), rep(trait, times = 1000)))
names(mdl.esti) <- c("iteration","esti.value")
mdl.esti$pred.trait <- NA

for(i in 1:1000){
  ymu <- mu.grand + mu.sp.splvl[1] + mu.study.stlvl[1]
  yhat <- rnorm(1000, ymu, sigmatrait.y[i]) # is it correct to have the 1000 there? It doesn't work otherwise
  mdl.esti$esti.trait[mdl.esti$iteration == i] <- yhat

  btf <- alphaf.sp[i] + betaTraitxforce * (mu_grand + mu.sp.splvl[i])
  btc <- alphac.sp[i] + betaTraitxchill * (mu_grand + mu.sp.splvl[i])
  btp <- alphap.sp[i] + betaTraitxphoto * (mu_grand + mu.sp.splvl[i])
  mdl.esti$esti.betaTforce[mdl.esti$iteration == i] <- btf
  mdl.esti$esti.betaTchill[mdl.esti$iteration == i] <- btc
  mdl.esti$esti.betaTphoto[mdl.esti$iteration == i] <- btp

  ypheno <- alpha.pheno.sp[1] + betaForcingSp[1] * forcingi[1] + betaPhotoSp[1] * photoi[1] + betaChillingSp[1] * chillingi[1]
  yhat.pheno <- rnorm(1000, ypheno, sigma.gen) # sigma.gen = 2
  mdl.esti$esti.pheno[mdl.esti$iteration == i] <- yhat.pheno

}
head(mdl.esti)
head(known.trait)

plot(density(known.trait$pred.trait), col = "blue", ylim = c(0,0.3))
points(density(mdl.esti$esti.trait), col ="red", pch=20)

plot(density(known.trait$pred.betaTforce), col = "blue", ylim = c(0,0.3))
points(density(mdl.esti$esti.betaTforce), col ="red", pch=20)

plot(density(known.trait$pred.betaTchill), col = "blue", ylim = c(0,0.3))
points(density(mdl.esti$esti.betaTchill), col ="red", pch=20)

plot(density(known.trait$pred.betaTphoto), col = "blue", ylim = c(0,0.3))
points(density(mdl.esti$esti.betaTphoto), col ="red", pch=20)

plot(density(known.trait$pred.pheno), col = "blue", ylim = c(0,0.3))
points(density(mdl.esti$esti.pheno), col ="red", pch=20)


plot(density(known.trait$pred.pheno))
plot(density(mdl.esti$esti.pheno))
