# simulating data for a joint model 
#------------------------------------

#started by Faith 6th feb 2020
#simulating data from teh joint model Lizzie wrote after taklking to Michael Betancourt (december 18th 2019)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
dev.off()

setwd("/home/faith/Documents/github/ospree/ospree/analyses/jointmodel")

library(truncnorm)
library(rstan)

#simulate teh fisrt level of the model - trait varying intercepts
#no slopes yet, to keep things simple

#parameters in the model
n <- 10 # number of replicates
nSpecies <- 30 # number of species
nStudy <- 15 
N <- n * nSpecies * nStudy # overall number of observations 

#make a dataframe to put data 
#note: when i use rep, i use the assumption that of the dataframe column teh values are being 
#entered in is longer than the vector produced by rep, then the values just get repeated until they
#fill the whole column. So it is like there is another level of rep.  

#I think there should be only one trait estimated in this model

simulatedTrait <- data.frame(matrix(NA, N, 2))
names(simulatedTrait) <- c("obs", "Study")
simulatedTrait$obs <- c(1:N)
simulatedTrait$Study <- rep(1:nStudy, each = nSpecies)
simulatedTrait$species <- rep(1:nSpecies, times = nStudy)
tail(simulatedTrait)

#grand alpha trait value 
muTrait <- 8 # mean of the distribution of intercepts of different species for that trait 

#different alpha values for each species
sigmaAlphTrait <- 4 # variation around the mean because species differ a bit  
alphaTraitSp <- rnorm(nSpecies, 0, sigmaAlphTrait)
simulatedTrait$alphaTraitSp <- rep(alphaTraitSp, times = nStudy) # pop in the dataframe 

#study specific intercepts 
#muAlphaStudy <- 12 # mean of teh effect of study - Faith is trying a grand mean insted now Dec 2020
sigmaAlphaStudy <- 6 # variation for different studies in the intercept 
alphaStudy <- rnorm(nStudy, 0, sigmaAlphaStudy ) # getting an intercept values for each study 
simulatedTrait$alphaStudy <- rep(alphaStudy, each = nSpecies) # repeat values as many times as there are traits and species

#general varience
sigma2Trait <- 2 
simulatedTrait$eTrait <- rnorm(N, 0, sigma2Trait)

#simulate teh data by "running" the model 
simulatedTrait$yTraiti <- muTrait + simulatedTrait$alphaTraitSp + simulatedTrait$alphaStudy + simulatedTrait$eTrait


#build a model in stan that gets these values back
#-----------------------------------------------------
stan_data <- list(yTraiti = simulatedTrait$yTraiti, N = N, n_spec = nSpecies, species = simulatedTrait$species, 
	study = simulatedTrait$Study, n_study = nStudy)

#Comment Dec2020 by Faith - What is teh predictor value for thsi model????

fit1 <- stan(file = "stan/OneTraitOnlyModel.stan", data = stan_data, warmup = 1000, iter = 2000, chains = 4, cores = 4, thin = 1)

pairs(fit1, pars= c("sigmaTrait_y", "mu_grand", "sigma_sp", "sigma_stdy", "lp__" ))
fit1_post <- rstan::extract(fit1)
fit1sum <- summary(fit1)$summary

#str(fit1)

# values look close enouph, and rhat = 1. Some fitting problems because not many itterations.
plot(density(fit1_post$sigmaTrait_y ))
sigma2Trait

plot(density(fit1_post$mu_grand))
muTrait

plot(density(fit1_post$sigma_stdy))
sigmaAlphaStudy

plot(density(fit1_post$sigma_sp))
sigmaAlphTrait

#simulate the second half of teh joint model 
#--------------------------------------

nSpecies # here to renmind me that there is teh same number of species 
#in this part of the model as there was in the first part
alphaTraitSp # this is the effect of species trait differences from teh first half of the model 

#nPheno <- 12 # there shoudl be a phenology and forcing value for each species 
#nForcing <- 10 # number of different forcing 
nph <- 200 # number of observations per species/phenological combination 
#Nph <- nSpecies * nForcing * nph * nPheno #overall number of observations
Nph <- nSpecies * nph # 20 obervations per species for phenological event and forcing 

#make a dataframe to keep things organised
phenoData <- data.frame(matrix(NA, Nph, 2))
names(phenoData) <- c("obs", "species")
phenoData$obs <- c(1:Nph)
phenoData$species <- rep(c(1:nSpecies), each = nph)

#phenological values for different species
muPhenoSp <- 150# day 100 is mean phenological date
sigmaPhenoSp <- 2 # species generally vary around 2 days from mean 150
alphaPhenoSp <- rnorm(nSpecies, muPhenoSp, sigmaPhenoSp) 
phenoData$alphaPhenoSp <- rep(alphaPhenoSp, each = nph)

#different forcing values for each species 
muForcingSp <- -2 # mean effect of forcing is negative because more forcing means budburst earlier 
sigmaForcingSp <- 0.5
alphaForcingSp <- rnorm(nSpecies, muForcingSp, sigmaForcingSp)

#interaction between trait and phenology?
betaTraitxPheno <- 2 # realtive trait value has a positive effect i.e taller trees need more forcing 

#combine teh effects of forcing and species trait differences into a slope
betaForcingSP1 <- alphaForcingSp + alphaTraitSp*betaTraitxPheno
betaForcingSp <- rep(betaForcingSP1, )
phenoData$betaForcingSp <- rep(betaForcingSp, each = nph)

#big F in the  model - I think this should be the x value, although there is no i in the lizzie's annotation 
muForcing <- 5 #amount of GDD on average?
sigmaForcing <- 1 # how much teh amount of forcing varies for all values 
forcingi <- rnorm(Nph, muForcing, sigmaForcing)
phenoData$forcingi <- forcingi

#general variance
ePhenoSigma <- 2
ePheno <- rnorm(Nph, 0, ePhenoSigma) 
phenoData$ePheno <- ePheno

#"run" the full model to simulate data 
phenoData$yPhenoi <- phenoData$alphaPhenoSp + phenoData$betaForcingSp * phenoData$forcingi + phenoData$ePheno


hist(phenoData$yPhenoi )


#build a stan model for the second part of the model 
#--------------------------------------------------------------

#This model is predictng values well. 
stan_data2 <- list(yPhenoi = phenoData$yPhenoi, Nph = Nph, n_spec = nSpecies, species = phenoData$species, 
	alphaTraitSp = alphaTraitSp, forcingi = forcingi)

fit2 <- stan(file = "stan/forcingOnlyModel.stan", data = stan_data2, warmup = 1000, iter = 2000, chains = 4, cores = 4, thin = 1)


posterior2 <- extract(fit2)

plot(density(posterior2$sigmapheno_y )) # 
ePhenoSigma

plot(density(posterior2$betaTraitxPheno )) # 
betaTraitxPheno

plot(density(posterior2$muForceSp)) #
muForcingSp

plot(density(posterior2$sigmaForceSp)) # 
sigmaForcingSp

plot(density(posterior2$muPhenoSp)) # 
muPhenoSp

plot(density(posterior2$sigmaPhenoSp)) # 
sigmaPhenoSp



#----------------------------------------------------
#Try combining into a single joint model
#--------------------------------------------------




stan_data3 <- list(yTraiti = simulatedTrait$yTraiti, N = N, n_spec = nSpecies, species = simulatedTrait$species, 
	study = simulatedTrait$Study, n_study = nStudy, yPhenoi = phenoData$yPhenoi, Nph = Nph, forcingi = forcingi,
	species2 = phenoData$species)


fit3 <- stan(file = "stan/stan_joint.stan", data = stan_data3, warmup = 1500, iter = 6500, chains = 4, cores = 4, thin = 1)

posterior3 <- extract(fit3)


#model 1
plot(density(posterior3$sigmaTrait_y )) #2
plot(density(posterior3$muSp )) #8 - not bell plot 
plot(density(posterior3$sigma_stdy)) #6
plot(density(posterior3$sigma_sp)) #4 - a bit low
plot(density(posterior3$muStdy )) #12 - not bell plot 

#model 2
plot(density(posterior3$sigmapheno_y )) # 4
plot(density(posterior3$betaTraitxPheno )) # 1.5
plot(density(posterior3$muForceSp)) # -3
plot(density(posterior3$sigmaForceSp)) # 2
plot(density(posterior3$muPhenoSp)) # 3
plot(density(posterior3$sigmaPhenoSp)) # 5


#the model is struggling with the mean intercept values for the first part of the model
