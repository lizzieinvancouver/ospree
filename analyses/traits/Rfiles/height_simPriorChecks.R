#Code to simulate phenology and mean sla data, then run prior predictive checks for the model joint_3cue_phenoonly.stan

#Started by Faith Sep 29 2021 as part of the traitors  project

#Editted March 2nd 2022 by Faith to check the effect of a positive betatraitx on phenology

rm(list = ls()) 
options(stringsAsFactors = FALSE)

library(tidyr)
library(plyr)
library(dplyr)
library(reshape2)
library(rstan)
library(bayesplot)# nice posterior check plots 
library(shinystan)
library(truncnorm)
library(ggplot2)


set.seed(1984)

#Set flags
#_-----------------------

runStan <- FALSE
Midge <- FALSE
priorCheck <- TRUE
BayesSweave <- FALSE

#Source ospree trators data if this runs on Faith's section of Midge
if(Midge == TRUE){
  setwd("~/traits")

} 


if(Midge == FALSE){
# Anyone else working with this code should add their info/path here
	if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
		} else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/take2/ospree/analyses/traits")
		} else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
		} 
}
#Simulate data - values taken from results of the real model 
#-------------------------------------------------------------------------

Nrep <- 10 # rep per trait
Nstudy <- 20 # number of studies w/ traits (10 seems a little low for early simulation code; remember that you are estimating a distribution of this the same as for species)
Nspp <- 20 # number of species 
# note I changed this to 30 to match the pheno mdl

# First making a data frame for the test trait data
Ntrt <- Nspp * Nstudy * Nrep # total number of traits observations
Ntrt

mu.grand <- 15 # the grand mean of the Height model
sigma.species <- 5 # we want to keep the variaiton across spp. high
sigma.study <- 5 # changed to match sigma species
sigmaTrait_y <- 2

#make a dataframe for Height
trt.dat <- data.frame(matrix(NA, Ntrt, 1))
names(trt.dat) <- c("rep")
trt.dat$rep <- c(1:Nrep)
trt.dat$study <- rep(rep(c(1:Nstudy), each = Nspp), each = Nrep)
trt.dat$species <- rep(rep(c(1:Nspp), times = Nstudy), each = Nrep)
#trt.dat$study2 <- rep(c(1:Nstudy), each = Nspp) # Faith changed these lines to the above that are different in heigh?simPriorChecks?review.R
#trt.dat$species2 <- rep(1:Nspp, Nstudy)

# now generating the species trait data, here it is for Height
#the alphaTraitSp in Faiths original code:
alphaTraitSp <- rnorm(Nspp, 0, sigma.species)
trt.dat$alphaTraitSp <- rep(alphaTraitSp, times = Nstudy)
  #rep(rep(alphaTraitSp, times = Nstudy), each = Nrep) 

#now generating the effects of study
muStudy <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$muStudy <- rep(muStudy, each = Nspp) # generate data for ea study

# general variance
trt.var <- 5 #sigmaTrait_y in the stan code
trt.dat$trt.er <- rnorm(Ntrt, 0, trt.var)

# generate yhat - heights -  for this first trt model
#trt.dat$yTraiti <- mu.grand + trt.dat$alphaTraitSp + trt.dat$muStudy + trt.dat$trt.er

for (i in 1:Ntrt){
  trt.dat$mu_grand_sp[i] <-  trt.dat$alphaTraitSp[i] +  mu.grand
}

for (i in 1:Ntrt){
  trt.dat$yTraiti[i] <-  trt.dat$alphaTraitSp[i] + trt.dat$muStudy[i] +  mu.grand + trt.dat$trt.er[i]
}

# hist( trt.dat$yTraiti)
# hist(trt.dat$mu_grand_sp)
# head(trt.dat, 50)

#### Pheno data generation ##############################



#number fo species
n_spec <-Nspp
#Number of repeat observations per species
nRep <- 20
#Overall number of pbservations (rows)
Nph <- n_spec * nRep

#Make a data frame for input simulation data
pheno.dat <- data.frame(matrix(NA, Nph, 2))
names(pheno.dat) <- c("rep","species")
pheno.dat$rep <- c(1:Nph)
pheno.dat$species <- rep(c(1:n_spec), each = nRep)


#Simulate cues (z scored)
pheno.dat$forcei <- rnorm(Nph, 1, 1)
pheno.dat$photoi <- rnorm(Nph, 1, 0.5) # less photoperiod 
pheno.dat$chilli <- rnorm(Nph, 1, 1) #more chilling

# Parameter Values


#Species means
sigmaPhenoSp <- 20
muPhenoSp <- 80
alphaPhenoSp <- rnorm(n_spec, muPhenoSp, sigmaPhenoSp)
pheno.dat$alphaPhenoSp <- rep(alphaPhenoSp, each = nRep)


#Cue effects
betaTraitxForce <- 0.3 
betaTraitxPhoto <- -0.2
betaTraitxChill <- -0.4

#Species level slopes sans trait data
muForceSp <- -10
sigmaForceSp <- 4
alphaForceSp <- rnorm(n_spec, muForceSp, sigmaForceSp)
pheno.dat$alphaForceSp <- rep(alphaForceSp, each = nRep)

muPhotoSp <- -0.05
sigmaPhotoSp <- 4
alphaPhotoSp <- rnorm(n_spec, muPhotoSp, sigmaPhotoSp)
pheno.dat$alphaPhotoSp <- rep(alphaPhotoSp, each = nRep)

muChillSp <- -0.6
sigmaChillSp <- 4
alphaChillSp <- rnorm(n_spec, muChillSp, sigmaChillSp)
pheno.dat$alphaChillSp <- rep(alphaChillSp, each = nRep)


#general varience
sigmapheno_y <- 5
pheno.dat$ePhen <- rnorm(Nph, 0, sigmapheno_y)

# Add trait values for each species
pheno.datTrait <- merge(pheno.dat, unique(trt.dat[,c("species","mu_grand_sp")]), by = "species")
head(pheno.datTrait,50)

#slopes for each cue, combining trait and non-trait aspect of the slope.

for (i in 1:Nph){
    pheno.datTrait$betaForceSp[i] <-  pheno.datTrait$alphaForceSp[i] + (betaTraitxForce *  pheno.datTrait$mu_grand_sp[i])

    pheno.datTrait$betaPhotoSp[i]<- pheno.datTrait$alphaPhotoSp[i] + (betaTraitxPhoto*  pheno.datTrait$mu_grand_sp[i])

    pheno.datTrait$betaChillSp[i] <-pheno.datTrait$alphaChillSp[i] + (betaTraitxChill* pheno.datTrait$mu_grand_sp[i])
}

#Run full model to get mean simulated y values
for (i in 1:Nph){
    pheno.datTrait$yMu[i] <-  pheno.datTrait$alphaPhenoSp[i] +  pheno.datTrait$betaForceSp[i] * pheno.datTrait$forcei[i] +  pheno.datTrait$betaPhotoSp[i] * pheno.datTrait$photoi[i] + pheno.datTrait$betaChillSp[i] * pheno.datTrait$chilli[i]
}

#Final values
pheno.datTrait$yPhenoi <- pheno.datTrait$yMu + pheno.datTrait$ePhen

head(pheno.datTrait,50)
#What does the data look like?
plot(density(pheno.datTrait$yPhenoi))



#Run the Stan Model on simulated data and save the output
#-------------------------------------------------------------------

#set up date for model 


## Prepare all data for Stan
all.data <- list(yTraiti = trt.dat$yTraiti,
                 N = Ntrt,
                 n_spec = Nspp,
                 trait_species = as.numeric(as.factor(trt.dat$species)),
                 species = as.numeric(as.factor(trt.dat$species)),
                 n_study = Nstudy,
                 study = as.numeric(as.factor(trt.dat$study )),
                 prior_mu_grand_mu = 20,
                 prior_mu_grand_sigma = 10,
                 prior_sigma_sp_mu = 4,
                 prior_sigma_sp_sigma = 5,
                 prior_sigma_study_mu = 2,
                 prior_sigma_study_sigma = 5,
                 prior_sigma_traity_mu = 3,
                 prior_sigma_traity_sigma = 5,
                 Nph = nrow(pheno.datTrait),
                 phenology_species = as.numeric(as.factor(pheno.datTrait$species )),
                 species2 = as.numeric(as.factor(pheno.datTrait$species )),
                 yPhenoi = pheno.datTrait$yPhenoi,
                 forcei = pheno.datTrait$forcei,
                 chilli = pheno.datTrait$chilli,
                 photoi = pheno.datTrait$photoi,
                 prior_muForceSp_mu = -15,
                 prior_muForceSp_sigma = 10, #wider
                 prior_muChillSp_mu = -15,
                 prior_muChillSp_sigma = 10,#wider
                 prior_muPhotoSp_mu = -15,
                 prior_muPhotoSp_sigma = 10,#wider
                 prior_muPhenoSp_mu = 40,
                 prior_muPhenoSp_sigma = 10,#wider
                 prior_sigmaForceSp_mu = 5,
                 prior_sigmaForceSp_sigma = 5,
                 prior_sigmaChillSp_mu = 5,#wider
                 prior_sigmaChillSp_sigma = 5, #wider
                 prior_sigmaPhotoSp_mu = 5,
                 prior_sigmaPhotoSp_sigma = 5,
                 prior_sigmaPhenoSp_mu = 5, #wider
                 prior_sigmaPhenoSp_sigma = 5, #wider
                 prior_betaTraitxForce_mu = 0,
                 prior_betaTraitxForce_sigma = 1,
                 prior_betaTraitxChill_mu = 0,
                 prior_betaTraitxChill_sigma = 1,
                 prior_betaTraitxPhoto_mu = 0,
                 prior_betaTraitxPhoto_sigma = 1,
                 prior_sigmaphenoy_mu = 10,
                 prior_sigmaphenoy_sigma = 5 #wider
                 )


	warmupNumber <- 2000
	itterNumber <- 4000




if(runStan == TRUE){

#Run model
  mdl.trait <- stan("stan/joint_3cue_newprior.stan",
                       data = all.data,
                       iter = itterNumber,
                       warmup = warmupNumber,
                       chains = 4,
                       cores = 4,
                       include = FALSE, pars = c("y_hat"))
  
  postMeanSLA <- rstan::extract(mdl.trait)
  # 
  #plot main effects of cues
  postMeanSLAdf <- data.frame(postMeanSLA)
  #
  # 
  #       
  png("simPosteriorHist_trait_dl.png")
  par(mfrow=c(3,4))
  #       #Compare results to simulated values
  hist(postMeanSLAdf$muSp, main = paste("muSp is " , signif(alphaTraitSp,3), sep = ""))
  abline(v = muSp, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$muStudy, main = paste("muStudy is " , signif(muStudy,3), sep = ""))
  abline(v = muForceSp, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$muChillSp, main = paste("muChillSp is " , signif(muChillSp,3), sep = ""))
  abline(v = muChillSp, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$muPhotoSp, main = paste("muPhotoSp is " , signif(muPhotoSp,3), sep = ""))
  abline(v = muPhotoSp, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$sigmapheno_y, main = paste("sigmapheno_y is " , signif(sigmapheno_y,3), sep = ""))
  abline(v = sigmapheno_y, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$betaTraitxForce, main = paste("betaTraitxForce is " , signif(betaTraitxForce,3), sep = ""))
  abline(v = betaTraitxForce, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$betaTraitxChill, main = paste("betaTraitxChill is " , signif(betaTraitxChill,3), sep = ""))
  abline(v = betaTraitxChill, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$betaTraitxPhoto, main = paste("betaTraitxPhoto is " , signif(betaTraitxPhoto,3), sep = ""))
  abline(v = betaTraitxPhoto, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$sigmaChillSp, main = paste("sigmaChillSp is " , signif(sigmaChillSp,3), sep = ""))
  abline(v = sigmaChillSp, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$sigmaForceSp, main = paste("sigmaForceSp is " , signif(sigmaForceSp,3), sep = ""))
  abline(v = sigmaForceSp, col="red", lwd=3, lty=2)
  # 
  hist(postMeanSLAdf$sigmaPhotoSp, main = paste("sigmaPhotoSp is " , signif(sigmaPhotoSp,3), sep = ""))
  abline(v = sigmaPhotoSp, col="red", lwd=3, lty=2) 
  dev.off()
  par(mfrow=c(1,1))
  # 
  mdl.traitphenSim <- stan("stan/phenology_combined.stan",
                      data = all.data,
                      iter = itterNumber,
                      warmup = warmupNumber,
                      chains = 4,
                      cores = 4,
                      include = FALSE, pars = c("y_hat"))

	save(mdl.traitphenSim, file = "phenologyMeanTrait_sim_dlcode.RData")
	sum.jfcp <- summary(mdl.traitphenSim)$summary

}

 if(runStan == FALSE){
 
 load("Rfiles/phenologyMeanTrait_sim_posF.RData")
# 
 postMeanSLA <- rstan::extract(mdl.traitphenSim)
# 
#plot main effects of cues
 postMeanSLAdf <- data.frame(postMeanSLA)
# 
  cueEffects <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% c("muPhenoSp", "muForceSp", "muChillSp", "muPhotoSp")]
# 
   cueEffectPlot <- mcmc_intervals(cueEffects) + 
     theme_classic() + 
      labs(title = "main intercept, cue slopes and general error")
# 
#       
      png("simPosteriorHist_height_dl2.png")
       par(mfrow=c(4,4))
#       #Compare results to simulated values
       # hist(postMeanSLAdf$mu_grand, main = paste("sigmapheno_y is " , signif(sigmapheno_y,3), sep = ""))
       # abline(v = mu_grand, col="red", lwd=3, lty=2)
       # 
       hist(postMeanSLAdf$sigma_sp, main = paste("sigmaSp_ is " , signif(sigmapheno_y,3), sep = ""))
       abline(v = sigma.species, col="red", lwd=3, lty=2)
       
       hist(postMeanSLAdf$sigma_study, main = paste("sigmaStudy is " , signif(sigmapheno_y,3), sep = ""))
       abline(v = sigma.study, col="red", lwd=3, lty=2)
       
       hist(postMeanSLAdf$sigmapheno_y, main = paste("sigmapheno_y is " , signif(sigmapheno_y,3), sep = ""))
       abline(v = sigmapheno_y, col="red", lwd=3, lty=2)
       
       
       hist(postMeanSLAdf$muPhenoSp, main = paste("muPhenoSp is " , signif(muPhenoSp,3), sep = ""))
       abline(v = muPhenoSp, col="red", lwd=3, lty=2)
# 
       hist(postMeanSLAdf$muForceSp, main = paste("muForceSp is " , signif(muForceSp,3), sep = ""))
       abline(v = muForceSp, col="red", lwd=3, lty=2)
# 
       hist(postMeanSLAdf$muChillSp, main = paste("muChillSp is " , signif(muChillSp,3), sep = ""))
       abline(v = muChillSp, col="red", lwd=3, lty=2)
# 
       hist(postMeanSLAdf$muPhotoSp, main = paste("muPhotoSp is " , signif(muPhotoSp,3), sep = ""))
       abline(v = muPhotoSp, col="red", lwd=3, lty=2)
# 
       hist(postMeanSLAdf$sigmapheno_y, main = paste("sigmapheno_y is " , signif(sigmapheno_y,3), sep = ""))
       abline(v = sigmapheno_y, col="red", lwd=3, lty=2)
# 
       hist(postMeanSLAdf$betaTraitxForce, main = paste("betaTraitxForce is " , signif(betaTraitxForce,3), sep = ""))
      abline(v = betaTraitxForce, col="red", lwd=3, lty=2)
# 
      hist(postMeanSLAdf$betaTraitxChill, main = paste("betaTraitxChill is " , signif(betaTraitxChill,3), sep = ""))
       abline(v = betaTraitxChill, col="red", lwd=3, lty=2)
# 
       hist(postMeanSLAdf$betaTraitxPhoto, main = paste("betaTraitxPhoto is " , signif(betaTraitxPhoto,3), sep = ""))
       abline(v = betaTraitxPhoto, col="red", lwd=3, lty=2)
# 
       hist(postMeanSLAdf$sigmaChillSp, main = paste("sigmaChillSp is " , signif(sigmaChillSp,3), sep = ""))
       abline(v = sigmaChillSp, col="red", lwd=3, lty=2)
# 
       hist(postMeanSLAdf$sigmaForceSp, main = paste("sigmaForceSp is " , signif(sigmaForceSp,3), sep = ""))
       abline(v = sigmaForceSp, col="red", lwd=3, lty=2)
# 
      hist(postMeanSLAdf$sigmaPhotoSp, main = paste("sigmaPhotoSp is " , signif(sigmaPhotoSp,3), sep = ""))
       abline(v = sigmaPhotoSp, col="red", lwd=3, lty=2) 
       dev.off()
      par(mfrow=c(1,1))
# 
 png("simulatedPairs.png")
 pairs(mdl.traitphenSim, pars = c("mu_grand", "sigma_sp", "sigma_study","sigma_traity","muForceSp", "muChillSp", "muPhotoSp", "betaTraitxForce", "betaTraitxChill", "betaTraitxPhoto", "lp__")) 
 dev.off()
# pairs(mdl.traitphen, pars = c("muForceSp", "muChillSp", "muPhotoSp", "sigmapheno_y", "lp__")) 
# 
# 
}


if(priorCheck == TRUE){

	#Prior Predictive Check (Run 1000 times and plot results)
	#------------------------------------------------------------

	#Number fo prior check itterations 
	nRepPrior <- 300
	
	# ppc for traits portion
	priorCheckTrait <- data.frame(matrix(NA, Ntrt*nRepPrior, 3))
	names(priorCheckTrait) <- c("simRep", "rep", "species")
	priorCheckTrait$simRep <- rep(1:nRepPrior, each = Ntrt)
	priorCheckTrait$rep <- rep(1:Ntrt, times = nRepPrior)
	priorCheckTrait$species <- rep(1:Nspp, each = nRep)
	priorCheckTrait$study <- rep(1:Nstudy, each = nRep)
	
	#traitSLA <- rnorm(Ntrt, 20, 5)
	
	#Make this the name of the full vector of sla per species values - alphaTraitSp 
	#priorCheckTrait$alphaTraitSp <-  rep(rep(trt.dat$mu_grand_sp, times = nRepPrior))
	
	for (ir in 1:nRepPrior){
	  # Parameter Values
	  #ir <- 1
	  
	  muGrand <- rtruncnorm(1, a = 0, mean = all.data$prior_mu_grand_mu, sd = all.data$prior_mu_grand_sigma)
	  sigmaSp <- rtruncnorm(1, a = 0, mean = all.data$prior_sigma_sp_mu, sd = all.data$prior_sigma_sp_sigma)
	  sigmaStudy <- rtruncnorm(1, a = 0, mean = all.data$prior_sigma_study_mu, sd = all.data$prior_sigma_study_sigma)
	  
	  alphaTraitSp <- rnorm(Nspp, 0, sigma.species)
	  priorCheckTrait$alphaTraitSp[priorCheckTrait$simRep == ir] <- rep(alphaTraitSp, each = nRep)
	  
	  muSp <- rnorm(Nspp, 0, sigma.species)
	  priorCheckTrait$muSp[priorCheckTrait$simRep == ir] <- rep(muSp, each = nRep)
	  
	  muStudy <- rnorm(Nstudy, 0, sigmaStudy)
	  priorCheckTrait$muStudy[priorCheckTrait$simRep == ir] <- rep(muStudy, each = nRep)
	  
	  #general varience - pick one value for the whole rep, adn use to get general varience for each row of teh data 
	  priorCheckTrait$sigmaTrait_y[priorCheckTrait$simRep == ir] <- rtruncnorm(1,  a = 0, all.data$prior_sigma_traity_sigma)
	  priorCheckTrait$e[priorCheckTrait$simRep == ir] <- rnorm(Ntrt, 0, priorCheckTrait$sigmaTrait_y[1])
	  
      #add up all the simulated values to get expected budburst date
	  priorCheckTrait$yTraiti <- muGrand + priorCheckTrait$muSp + priorCheckTrait$muStudy + priorCheckTrait$e
	}# end simulating new priors, from here vectorize code

	#Final values
	priorCheckTrait$muGrandSp <- muGrand + priorCheckTrait$muSp

	
	png("figures/priorChecks/density_Trait_Prior_height.png")
	plot(density(priorCheckTrait$yTraiti), xlab = "Predicted day of budburst", main = "Height Prior Check")
	dev.off()
	
	png("figures/priorChecks/GrandSp_PlotPrior_height.png")
	plot(priorCheckTrait$yTraiti ~ priorCheckTrait$muGrandSp, xlab = "muGrandSp", ylab = "Trait", main = "Height Prior Check")
	dev.off()
	
	png("figures/priorChecks/MuSp_PlotPrior_height.png")
	plot(priorCheckTrait$yTraiti ~ priorCheckTrait$muSp, xlab = "MuSp", ylab = "Trait", main = "Height Prior Check")
	dev.off()
	
	png("figures/priorChecks/MuStudy_PlotPrior_height.png")
	plot(priorCheckTrait$yTraiti ~ priorCheckTrait$muStudy, xlab = "MuStudy", ylab = "Trait", main = "Height Prior Check")
	dev.off()
#####################################################################################
	
	#Make a data frame for input simulation data
	priorCheckPheno <- data.frame(matrix(NA, Nph*nRepPrior, 3))
	names(priorCheckPheno) <- c("simRep","rep","species")

	priorCheckPheno$simRep <- rep(1:nRepPrior, each = Nph)

	priorCheckPheno$rep <- rep(c(1:Nph), times = nRepPrior)
	priorCheckPheno$species <- rep(rep(c(1:n_spec), each = nRep), times = nRepPrior)
	


	#Simulate SLA data per species
	muGrandSp <- muGrand + muSp
	#Make this the name of the full vector of sla per species values - alphaTraitSp 
	priorCheckPheno$alphaTraitSp <-  rep(rep(muGrandSp, times = nRepPrior)) # use the mean mu_grand_sp, grand mean + study, not study


	#Simulate cues (z scored)
	priorCheckPheno$forcei <- rnorm(Nph, 1, 1)
	priorCheckPheno$photoi <- rnorm(Nph, 1, 1) # less photoperiod 
	priorCheckPheno$chilli <- rnorm(Nph, 1, 1) #more chilling


	for (ir in 1:nRepPrior){
		# Parameter Values
		#ir <- 1

		#Species means
		sigmaPhenoSp <- rtruncnorm(1, a = 0, mean = all.data$prior_sigmaPhenoSp_mu, sd = all.data$prior_sigmaPhenoSp_sigma)
		muPhenoSp <- rnorm(1, all.data$prior_muPhenoSp_mu, all.data$prior_muPhenoSp_sigma)
		alphaPhenoSp <- rnorm(n_spec, muPhenoSp, sigmaPhenoSp)
		priorCheckPheno$alphaPhenoSp[priorCheckPheno$simRep == ir] <- rep(alphaPhenoSp, each = nRep)

		#Cue effects
		priorCheckPheno$betaTraitxForce[priorCheckPheno$simRep == ir] <- rnorm(1,all.data$prior_betaTraitxForce_mu,all.data$prior_betaTraitxForce_sigma)
		priorCheckPheno$betaTraitxPhoto[priorCheckPheno$simRep == ir] <- rnorm(1,all.data$prior_betaTraitxPhoto_mu,all.data$prior_betaTraitxPhoto_sigma)
		priorCheckPheno$betaTraitxChill[priorCheckPheno$simRep == ir] <- rnorm(1,all.data$prior_betaTraitxChill_mu,all.data$prior_betaTraitxChill_sigma)

		#Species level slopes sans trait data
		muForceSp <- rnorm(1,all.data$prior_muForceSp_mu,  all.data$prior_muForceSp_sigma)
		sigmaForceSp <- rtruncnorm(1, a = 0,mean = all.data$prior_sigmaForceSp_mu,sd = all.data$prior_sigmaForceSp_sigma)
		alphaForceSp <- rnorm(n_spec, muForceSp, sigmaForceSp)
		priorCheckPheno$alphaForceSp[priorCheckPheno$simRep == ir] <- rep(alphaForceSp, each = nRep)

		muPhotoSp <- rnorm(1, all.data$prior_muPhotoSp_mu, all.data$prior_muPhotoSp_sigma)
		sigmaPhotoSp <- rtruncnorm(1, a = 0,mean = all.data$prior_sigmaPhotoSp_mu, sd = all.data$prior_sigmaPhotoSp_sigma )
		alphaPhotoSp <- rnorm(n_spec, muPhotoSp, sigmaPhotoSp)
		priorCheckPheno$alphaPhotoSp[priorCheckPheno$simRep == ir] <- rep(alphaPhotoSp, each = nRep)

		muChillSp <-  rnorm(1,all.data$prior_sigmaChillSp_mu,all.data$prior_sigmaChillSp_sigma)
		sigmaChillSp <- rtruncnorm(1, a = 0,mean = all.data$prior_sigmaChillSp_mu,sd = all.data$prior_sigmaChillSp_sigma)
		alphaChillSp <- rnorm(n_spec, muChillSp, sigmaChillSp)
		priorCheckPheno$alphaChillSp[priorCheckPheno$simRep == ir] <- rep(alphaChillSp, each = nRep)


		#general varience
		priorCheckPheno$sigmapheno_y[priorCheckPheno$simRep == ir] <- rtruncnorm(1,  a = 0, all.data$prior_sigmaphenoy_sigma)
		priorCheckPheno$e[priorCheckPheno$simRep == ir] <- rnorm(Nph, 0, priorCheckPheno$sigmapheno_y[1])

	}# end simulating new priors, from here vectorize code
		#slopes for each cue, combining trait and non-trait aspect of the slope.

	
	priorCheckPheno$betaForceSp <- priorCheckPheno$alphaForceSp + priorCheckPheno$betaTraitxForce *  priorCheckPheno$alphaTraitSp
	 
	priorCheckPheno$betaPhotoSp <-  priorCheckPheno$alphaPhotoSp + priorCheckPheno$betaTraitxPhoto * priorCheckPheno$alphaTraitSp

	priorCheckPheno$betaChillSp <-  priorCheckPheno$alphaChillSp + priorCheckPheno$betaTraitxChill * priorCheckPheno$alphaTraitSp

	#Run full model to get mean simulated y values
	priorCheckPheno$yMu <-  priorCheckPheno$alphaPhenoSp +  priorCheckPheno$betaForceSp * priorCheckPheno$forcei +  priorCheckPheno$betaPhotoSp* priorCheckPheno$photoi + priorCheckPheno$betaChillSp * priorCheckPheno$chilli

	#Final values
	priorCheckPheno$yPhenoi <- priorCheckPheno$yMu + priorCheckPheno$e

	head(priorCheckPheno)
	plot(priorCheckPheno$betaForceSp ~ priorCheckPheno$alphaTraitSp )
	priorCheckPheno_posF <- priorCheckPheno[priorCheckPheno$betaForceSp > 0,]
	plot(priorCheckPheno_posF$betaForceSp ~ priorCheckPheno_posF$alphaTraitSp )

	png("figures/priorChecks/densityYPrior_height.png")
	plot(density(priorCheckPheno$yPhenoi), xlab = "Predicted day of budburst", main = "Height Prior Check")
	dev.off()

	png("figures/priorChecks/photoPlotPrior_height.png")
	plot(priorCheckPheno$yPhenoi ~ priorCheckPheno$photoi, xlab = "Photoperiod", ylab = "Phenological Date", main = "Height Prior Check")
	dev.off()

	png("figures/priorChecks/forcingPlotPrior_height.png")
	plot(priorCheckPheno$yPhenoi ~ priorCheckPheno$forcei, xlab = "Forcing", ylab = "Phenological Date", main = "Height Prior Check")
	dev.off()

	png("figures/priorChecks/chillingPlotPrior_height.png")
	plot(priorCheckPheno$yPhenoi ~ priorCheckPheno$chilli, xlab = "Chillina", ylab = "Phenological Date", main = "Height Prior Check")
	dev.off()

}

if(BayesSweave == TRUE){
	#For the BayesClass sweave documents 
	setwd("/home/faith/Documents/github/bayes2020/Projects/Faith/traitorsModel")
}
	sum.jfcp <- summary(mdl.traitphenSim)$summary
	
	mu_grand <- sum.jfcp[grep("mu_grand", rownames(sum.jfcp))]
	sigma_sp <- sum.jfcp[grep("sigma_sp", rownames(sum.jfcp))]
	sigma_studyesti <- sum.jfcp[grep("sigma_study", rownames(sum.jfcp))]
	sigmaTrait_y <- sum.jfcp[grep("sigma_traity", rownames(sum.jfcp))]
	
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
	
	mdl.out <- data.frame( "Parameter" = c("mu_grand","sigma_sp","sigma_study","sigmaTrait_y","mu_forcesp","mu_chillsp","mu_photosp","mu_phenosp","sigma_forcesp","sigma_chillsp","sigma_photosp", "sigma_phenosp", "sigma_phenoy", "beta_tf", "beta_tc","beta_tp"),  "Test.data.values" = c(mu.grand, sigma.species, sigma.study, trt.var,                                                                    muForceSp, muChillSp, muPhotoSp, muPhenoSp, sigmaForceSp, sigmaChillSp, sigmaPhotoSp, sigmaPhenoSp,sigmapheno_y, betaTraitxForce, betaTraitxChill, betaTraitxPhoto) ,
	                       "Estiamte"= c(mu_grand[1], sigma_sp, sigma_studyesti, sigmaTrait_y,  mu_forcesp, mu_chillsp, mu_photosp, mu_phenosp, sigma_forcesp, sigma_chillsp, sigma_photosp, sigma_phenosp, sigma_phenoy, beta_tf, beta_tc,  beta_tp)
	                       
	)
	
	mdl.out
	
	muPhenoSp <- sum.jfcp[grep("alphaPhenoSp", rownames(sum.jfcp))]
	muStudyEsti <- sum.jfcp[grep("muStudy", rownames(sum.jfcp))]
	
	pdf("muPhenoSp_esti.pdf")
	plot(muPhenoSp ~ alphaPhenoSp, xlab = "simulated muPhenoSp", ylab = "mdl estimated muPhenoSp")
	abline(0,1)
	dev.off()
	
	pdf("study_esti.pdf")
	plot(muStudyEsti ~ muStudy, xlab = "simulated muStudy", ylab = "mdl estimated muStudy")
	abline(0,1)
	dev.off()
	
	
	
	
	