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

Nrep <- 10 # rep per trait
Nstudy <- 10 # number of studies w/ traits
Nspp <- 10 # number of species with traits

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
mu.grand <- 20 # the grand mean trait value?
sigma.trtsp <- 10 #the species sigma for the traits model

alpha.trtsp <- rnorm(Nspp, 0, sigma.trtsp)
trt.dat$alpha.trtsp <- rep(alpha.trtsp, Nstudy) #adding ht data for ea. sp

#now generating the effects of study
sigma.study <- 5
alpha.study <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$alpha.study <- rep(alpha.study, each = Nspp) # generate data for ea study

# general variance
trt.var <- 2
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
sigma.force.sp <- 4
alpha.force.sp <- rnorm(Nspp, mu.force.sp, sigma.force.sp)

mu.chill.sp <- -2 
sigma.chill.sp <- 3
alpha.chill.sp <- rnorm(Nspp, mu.chill.sp, sigma.chill.sp)

mu.photo.sp <- -2 
sigma.photo.sp <- 2.4
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
                 data = stan_data, iter = 4000 ,control = list(adapt_delta = 0.99, max_treedepth = 18))

save(mdl.test, file = "output.traitors.Rda")

#load("output/output.traitors.Rda")
# load("output/output.traitors.Rda")
# with 0.1, 0.5 for the cue sigma, produces 14 div transitions, all for the three cue sigmas and log-posterior
#load("output/output.traitors.5.Rda")
load("output/output.traitors.4,3,2.4.Rda")
# no divergent transitions if 5
# muForceSp -0.6/-1
# sigmaForceSp 5.43/5
#
# muChillSp -1.98
# sigmaChippSp 5.47/5
#
# muPhotoSp -1.69-2
# sigmaPhotoSp 5.46/5
#
# muPhenoSp 150
# Sigma.pheno.sp 2
#
# BetaTraitxF -0.17/ -0.8
# BetaTraitxC -016/ -0.8
# BetaTraitxP -0.18/ -0.8
# Sigmapheno_y 2.2/0
#load("output/output.traitors.02.Rda") #values look worse!

ssm <-  as.shinystan(mdl.test)
launch_shinystan(ssm)

sumer <- summary(mdl.test)$summary
post <- rstan::extract(mdl.test)
# #
y<-trt.dat$yTraiti
yrep<-post$ymu # I want this to be a matrix, which it is, with one element for each data point in y

ppc_dens_overlay(y, yrep[1:50, ])
#
# #model 1
# 
plot(density(post$sigmaTrait_y )) ; abline(v = trt.var, col = "red")
plot(density(post$sigma_sp)); abline(v = sigma.trtsp, col = "red")
plot(density(post$sigma_stdy)); abline(v = sigma.study, col = "red")
plot(density(post$muStdy )); abline(v = 0, col = "red")
plot(density(post$muSp )); abline(v = 0, col = "red")

#model 2
par(mfrow= c(1,1))
plot(density(post$alphaForcingSp)); abline(v = mu.force.sp, col = "red")
plot(density(post$alphaChillSp)); abline(v = mu.chill.sp, col = "red")
plot(density(post$alphaPhotoSp)); abline(v = mu.photo.sp, col = "red")
plot(density(post$sigmapheno_y )); abline(v = sigma.gen, col = "red")

plot(density(post$betaTraitxForcing), xlim = c(-0.9,1)); abline(v = betaTraitxforce, col = "red")
plot(density(post$betaTraitxPhoto), xlim = c(-0.9,1)); abline(v = betaTraitxphoto, col = "red")
plot(density(post$betaTraitxChill), xlim = c(-0.9,1)); abline(v = betaTraitxchill, col = "red")

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

mu.grand <- rnorm(1000, 20, 10)
mu.sp <- rnorm(1000, 0,10)
mu.study <- rnorm(1000, 0, 5)

sigmatrait.y <- rnorm(1000, 2, 0.5)
sigma.sp <- rnorm(1000, 10, 2)
sigma.study <- rnorm(1000, 5,1)

ymu <- mu.grand[1] + mu.sp[1] + mu.study[1] 
yhat <- rnorm(1000,ymu, sigmatrait.y[1]) # why does this generate 19 values?

# now lets loop it

known.values <- data.frame(cbind(rep(1:1000, times = length(trait)), rep(trait, times = 1000)))
names(known.values) <- c("iteration","trait.value")
known.values$pred.y <- NA  

for(i in 1:1000){
  ymu <- mu.grand[i] + mu.sp[i] + mu.study[i] 
  yhat <- rnorm(1000, ymu, sigmatrait.y[i]) # is it correct to have the 1000 there? It doesn't work otherwise
  known.values$pred.y[known.values$iteration == i] <- yhat
}
head(known.values)

## But the above doesn't account for the species or study level effects, should it be something like
mu.grand <- rnorm(1000, 20, 10)
mu.sp <- 0
mu.study <- 0
mu.sp.splvl <- rnorm(mu.sp, sigma.sp)
mu.study.stlvl <- rnorm(mu.sp, sigma.study)
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

betaForcingSp <- alphaf.sp[1] + betaTraitxforce * (mu_grand[1] + mu.sp.splvl[1])
betaChillingSp <- alphac.sp[1] + betaTraitxchill * (mu_grand[1] + mu.sp.splvl[1])
betaPhotoSp <- alphap.sp[1] + betaTraitxphoto * (mu_grand[1] + mu.sp.splvl[1])

# finally generate data for the phenology model
# now lets loop it
known.trait <- data.frame(cbind(rep(1:1000, times = length(trait)), rep(trait, times = 1000)))
names(known.trait) <- c("iteration","trait.value")
known.trait$pred.trait <- NA  

for(i in 1:1000){
  ymu <- mu.grand[i] + mu.sp.splvl[i] + mu.study.stlvl[i]
  yhat <- rnorm(1000, ymu, sigmatrait.y[i]) # is it correct to have the 1000 there? It doesn't work otherwise
  known.trait$pred.trait[known.trait$iteration == i] <- yhat
  
  btf <- alphaf.sp[i] + betaTraitxforce * (mu_grand[i] + mu.sp.splvl[i]) 
  btc <- alphac.sp[i] + betaTraitxchill * (mu_grand[i] + mu.sp.splvl[i]) 
  btp <- alphap.sp[i] + betaTraitxphoto * (mu_grand[i] + mu.sp.splvl[i]) 
  known.trait$pred.betaTforce[known.trait$iteration == i] <- btf
  known.trait$pred.betaTchill[known.trait$iteration == i] <- btc
  known.trait$pred.betaTphoto[known.trait$iteration == i] <- btp
}
head(known.trait)
####################################################################################

# generate the data
known.trait$pred.betaTforce <- NA  
known.trait$pred.betaTchill <- NA  
known.trait$pred.betaTphoto <- NA  

for(i in 1:1000){
  btf <- alphaf.sp[i] + betaTraitxforce * (mu_grand[i] + mu.sp.splvl[i]) 
  btc <- alphac.sp[i] + betaTraitxchill * (mu_grand[i] + mu.sp.splvl[i]) 
  btp <- alphap.sp[i] + betaTraitxphoto * (mu_grand[i] + mu.sp.splvl[i]) 
  known.trait$pred.betaTforce[known.trait$iteration == i] <- btf
  known.trait$pred.betaTchill[known.trait$iteration == i] <- btc
  known.trait$pred.betaTphoto[known.trait$iteration == i] <- btp
  
}
head(known.trait)

####################################################################################

yPhenoi[i] ~ normal( alphaPhenoSp[species2[i]] + betaForcingSp[species2[i]] * forcingi[i] + betaPhotoSp[species2[i]] * photoi[i] + betaChillSp[species2[i]] * chillingi[i], sigmapheno_y)

mu.trait <- 20 + 10*trait + 5*trait # I am not sure this is right, but taking 10 from muSp and 5 from muStudy..
ymu <- rnorm(mu.trait, 2) # 2 is the trt.var



mu.force.sp <- -1 
sigma.force.sp <- 4
alpha.force.sp <- rnorm(Nspp, mu.force.sp, sigma.force.sp)

mu.chill.sp <- -2 
sigma.chill.sp <- 3
alpha.chill.sp <- rnorm(Nspp, mu.chill.sp, sigma.chill.sp)

mu.photo.sp <- -2 
sigma.photo.sp <- 2.4
alpha.photo.sp <- rnorm(Nspp, mu.photo.sp, sigma.photo.sp)

#interaction between trait and phenology?
betaTraitxchill <- -.8 
betaTraitxphoto<- -.8 
betaTraitxforce <- -.8 
