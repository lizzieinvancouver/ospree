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
sigma.species <- 10 

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
#build dataframe with length of iteraction, should have a prior for each parameter
#have col. with iteration and save to each iteration, or could save as a list and bind at the end as dataframe, 
trait <- 1:10
mu.grand.prior <- rnorm(1000, 20, 10) # is from above, 10 just seems like a reasonable value to me
mu.trtsp.prior <- rnorm(1000, 0, 10)
mu.study.prior <- rnorm(1000, 0, 5)
sigma.sp.prior <- rnorm(1000,10, 1)
sigma.study.prior <- rnorm(1000, 5, 0.5)
sigmaTraity.prior <- rnorm(1000, 0, 0.5)

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
head(trt.ppc)

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

#interaction between trait and phenology
# May 7: increasing this value from -0.8
betaTraitxchill <- -2
betaTraitxphoto<- -2
betaTraitxforce <- -2

#combine the effects of forcing and species trait differences into a slope
beta.forcing.sp1 <- alpha.force.sp + mu.trtsp*betaTraitxforce
beta.forcing.sp <- rep(beta.forcing.sp1, )
pheno.dat$beta.forcing.sp <- rep(beta.forcing.sp, each = nphen)

beta.chilling.sp1 <- alpha.chill.sp + mu.trtsp*betaTraitxchill
beta.chilling.sp <- rep(beta.chilling.sp1, )
pheno.dat$beta.chilling.sp <- rep(beta.chilling.sp, each = nphen)

beta.photo.sp1 <- alpha.photo.sp + mu.trtsp*betaTraitxphoto
beta.photo.sp <- rep(beta.photo.sp1, )
pheno.dat$beta.photo.sp <- rep(beta.photo.sp, each = nphen)

#general variance
# May 7: making this value, the general noise in the phenology model much smaller
sigma.gen <- 0.5
gen.var <- rnorm(Nph, 0, sigma.gen) 
pheno.dat$gen.er <- gen.var

#"run" the full model to simulate data 
pheno.dat$doy.i <- pheno.dat$alpha.pheno.sp + pheno.dat$beta.forcing.sp * pheno.dat$forc.i +
  pheno.dat$beta.chilling.sp * pheno.dat$chill.i + pheno.dat$beta.photo.sp * pheno.dat$photo.i + pheno.dat$gen.er

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
                  forcei = forc.i,
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
# It is unclear to me how to incorporate a grand mean into a linear model
fm.trait <- lmer(yTraiti ~ (1| mu.trtsp) + (1|mu.study), data = trt.dat)
summary(fm.trait)
coef(fm.trait)

#bc setting betaTraitxforce to zero, btf will be equal to the cue level variation
betaTraitxchill <- 0
betaTraitxphoto<- 0
betaTraitxforce <- 0

beta.force.sp <- pheno.dat$alpha.force.sp + trt.dat$mu.trtsp*betaTraitxforce
beta.chill.sp <- pheno.dat$alpha.chill.sp + trt.dat$mu.trtsp*betaTraitxchill
beta.photo.sp <- pheno.dat$alpha.photo.sp + trt.dat$mu.trtsp*betaTraitxphoto

fm.pheno <- lmer(doy.i ~ forcingi*beta.force.sp + photoi*beta.photo.sp + chillingi*beta.chill.sp + (1|species), data = pheno.dat)
#Error in model.frame.default(data = pheno.dat, drop.unused.levels = TRUE,: variable lengths differ (found for 'beta.chill.sp')

# Becuase the beta.forcing.sp is just alpha.force.sp etc, I am subbing it in directly
fm.pheno <- lmer(doy.i ~ forcingi*alpha.force.sp + photoi*alpha.photo.sp + chillingi*alpha.chill.sp + (1|species), data = pheno.dat)
summary(fm.pheno)

#######################################################################################

# checking to see if it is an identifiability issue:
# simulate the model using the true values and generate a set of outcomes
# simulate the model with the wrong values that the model is producing
# plot the densities of these two things, if they overlap a lot then we know different values are producing the same outcome, 

# starting with the trait model:
trait <- 1:5

#when simulate data have a y value that you think it could really be, 1000 different y values bc 1000 different x values
#prior predictive check, you have x values (10), and we have a distribution of potential parameter values, some are more likley, stan searches through possible values and see which are right, simulate many iterations of your model using the potential parameter values.
mu.grand <- rnorm(1000, 20, 10)
mu.sp <- rnorm(20, 0,10)
mu.study <- rnorm(10, 0, 5)

sigmatrait.y <- rnorm(1000, 2, 0.5)
sigma.sp <- rnorm(1000, 10, 2)
sigma.study <- rnorm(1000, 5,1)

known.values <- data.frame(cbind(rep(1:1000, times = length(trait)), rep(trait, times = 1000)))
names(known.values) <- c("iteration","trait.value")
known.values$pred.y <- NA

for(i in 1:1000){
  ymu <- mu.grand + mu.sp[i] + mu.study[i] # should repeat these values 
  yhat <- rnorm(1, ymu, sigmatrait.y[i]) # should be one
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
yhat <- rnorm(1000,ymu, sigmatrait.y[1]) 

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

# or are the betaForcingSp supposed to be alpha.force.sp + mu.trtsp*betaTraitxforce

#combine the effects of forcing and species trait differences into a slope
beta.forcing.sp <- alpha.force.sp + mu.trtsp*betaTraitxforce
beta.chilling.sp <- alpha.chill.sp + mu.trtsp*betaTraitxchill
beta.photo.sp <- alpha.photo.sp + mu.trtsp*betaTraitxphoto

# finally generate data for the phenology model
# now lets loop it
known.trait <- data.frame(cbind(rep(1:1000, times = length(trait)), rep(trait, times = 1000)))
names(known.trait) <- c("iteration","trait.value")
known.trait$pred.trait <- NA

for(i in 1:1000){
  ymu <- mu.grand + mu.sp.splvl[i] + mu.study.stlvl[i]
  yhat <- rnorm(1000, ymu, sigmatrait.y[1]) # is it correct to have the 1000 there? It doesn't work otherwise
  known.trait$pred.trait<- yhat

  btf <- alphaf.sp[i] + betaTraitxforce * mu.sp.splvl[i]
  btc <- alphac.sp[i] + betaTraitxchill * mu.sp.splvl[i]
  btp <- alphap.sp[i] + betaTraitxphoto * mu.sp.splvl[i]
  known.trait$pred.betaTforce[known.trait$iteration == i] <- btf
  known.trait$pred.betaTchill[known.trait$iteration == i] <- btc
  known.trait$pred.betaTphoto[known.trait$iteration == i] <- btp

  ypheno <- alpha.pheno.sp[i] + btf * forcingi[i] + btp * photoi[i] + btc * chillingi[i]
  yhat.pheno <- rnorm(1000, ypheno, sigma.gen) # sigma.gen = 2
  known.trait$pred.pheno <- yhat.pheno

}
head(known.trait)
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
mdl.esti$esti.trait <- NA

for(i in 1:1000){
  ymu <- mu.grand + mu.sp.splvl[i] + mu.study.stlvl[i]
  yhat <- rnorm(1000, ymu, sigmatrait.y[i]) # is it correct to have the 1000 there? It doesn't work otherwise
  mdl.esti$esti.trait <- yhat

  betaForcingSp <- alphaf.sp[i] + betaTraitxforce * mu.sp.splvl[i]
  betaChillingSp <- alphac.sp[i] + betaTraitxchill * mu.sp.splvl[i]
  betaPhotoSp <- alphap.sp[i] + betaTraitxphoto * mu.sp.splvl[i]
  mdl.esti$esti.betaTforce[mdl.esti$iteration == i] <- betaForcingSp
  mdl.esti$esti.betaTchill[mdl.esti$iteration == i] <- betaChillingSp
  mdl.esti$esti.betaTphoto[mdl.esti$iteration == i] <- betaPhotoSp

  ypheno <- alpha.pheno.sp[i] + betaForcingSp * forcingi[i] + betaPhotoSp * photoi[i] + betaChillingSp * chillingi[i]
  yhat.pheno <- rnorm(1000,ypheno, sigma.gen) # sigma.gen = 2
  mdl.esti$esti.pheno <- yhat.pheno

}
head(mdl.esti)

head(known.trait)

plot(density(known.trait$pred.trait), col = "blue", ylim = c(0,0.3), xlim = c(-30,30))
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
