#Code to simulate phenology and mean sla data, then run prior predictive checks for the model joint_3cue_phenoonly.stan

#Started by Faith Sep 29 2021 as part of the traitors  project


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


set.seed(197365)

#Set flags
#_-----------------------

runStan <- FALSE
Midge <- FALSE
priorCheck <- FALSE
BayesSweave <- TRUE

#Source ospree trators data if this runs on Faith's section of Midge
if(Midge == TRUE){
  setwd("~/traits")

} 



if(Midge == FALSE){
# Anyone else working with this code should add their info/path here
	if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
		} else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
		} else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
		} 
}
#Simulate data - values taken from results of the real model 
#-------------------------------------------------------------------------



#number fo species
n_spec <-150
#Number of repeat observations per species
nRep <- 15
#Overall number of pbservations (rows)
Nph <- n_spec * nRep

#Make a data frame for input simulation data
pheno.dat <- data.frame(matrix(NA, Nph, 2))
names(pheno.dat) <- c("rep","species")
pheno.dat$rep <- c(1:Nph)
pheno.dat$species <- rep(c(1:n_spec), each = nRep)


#Simulate mean SLA offset data per species (not mean value)
meanSLA <- rnorm(n_spec, 0, 5)
#Make this the name of the full vector of sla per species values - alphaTraitSp 
pheno.dat$alphaTraitSp <- rep(meanSLA, each = nRep)


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
betaTraitxForce <- -0.3 
betaTraitxPhoto <- -0.2
betaTraitxChill <- -0.4

#Species level slopes sans trait data
muForceSp <- -0.4
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
pheno.dat$e <- rnorm(Nph, 0, sigmapheno_y)

#slopes for each cue, combining trait and non-trait aspect of the slope.

for (i in 1:Nph){
    pheno.dat$betaForceSp[i] = pheno.dat$alphaForceSp[i] + betaTraitxForce * ( pheno.dat$alphaTraitSp[i]);
    }
    
    for (i in 1:n_spec){
    pheno.dat$betaPhotoSp[i] = pheno.dat$alphaPhotoSp[i] + betaTraitxPhoto* ( pheno.dat$alphaTraitSp[i]);
    }

    for (i in 1:n_spec){
    pheno.dat$betaChillSp[i] = pheno.dat$alphaChillSp[i] + betaTraitxChill* (pheno.dat$alphaTraitSp[i]);
}

#Run full model to get mean simulated y values
for (i in 1:Nph){
    pheno.dat$yMu[i] <-  pheno.dat$alphaPhenoSp[i] +  pheno.dat$betaForceSp[i] * pheno.dat$forcei[i] +  pheno.dat$betaPhotoSp[i] * pheno.dat$photoi[i] + pheno.dat$betaChillSp[i] * pheno.dat$chilli[i]
}

#Final values
pheno.dat$yPhenoi <- pheno.dat$yMu + pheno.dat$e

#What does the data look like?
plot(density(pheno.dat$yPhenoi))








#Run the Stan Model on simulated data and save the output
#-------------------------------------------------------------------

#set up date for model 

## pehnology and mean trait stan model ###########################################################
pheno_data <- list(alphaTraitSp = pheno.dat$alphaTraitSp, #mean species trait value 
                   Nph = Nph, 
                   n_spec = n_spec, 
                   species = as.integer(as.factor(pheno.dat$species)), 
                   yPhenoi = pheno.dat$yPhenoi, 
                   forcei = pheno.dat$forcei,
                   photoi = pheno.dat$photoi, 
                   chilli = pheno.dat$chilli,
                #Priors
                #Priors
                   prior_sigmaphenoy_mu =20,  #mean of prior distribution of the general error (sigma_y) around the mean predicted value
                   prior_sigmaphenoy_sigma = 5, # variance of the prior distribution of the general error sigma)y around the mean predicted value
                   
                   prior_muForceSp_mu = 0, # mean of the prior distribution of the mean effect of forcing 
                   prior_muForceSp_sigma = 1, # vareince of the prior distributionof the mean effect of forcing 
                   prior_sigmaForceSp_mu = 4, # mean of the prior distribution of the varience around the mean effect of forcing 
                   prior_sigmaForceSp_sigma = 3,# variance of the prior distribution of the varience around the mean effect of forcing ,

                   prior_muChillSp_mu = 0,# mean of the prior distribution of the mean effect of chilling 
                   prior_muChillSp_sigma = 1,# varience of the prior distribution of the mean effect of chilling 
                   prior_sigmaChillSp_mu = 4,# mean of the prior distribution of the varience around the mean effect of chilling 
                   prior_sigmaChillSp_sigma= 3, #variance of the prior distribution of the varience around the mean effect of chilling

                   prior_muPhotoSp_mu = 0,# mean of the prior distribution of the varience around the mean effect of photoperiod 
                   prior_muPhotoSp_sigma = 1,# varience of the prior distribution of the varience around the mean effect of photoperiod 
                   prior_sigmaPhotoSp_mu=4,# mean of the prior distribution of the varience around the mean effect of photoperiod
                   prior_sigmaPhotoSp_sigma=3,#variance of the prior distribution of the varience around the mean effect of photoperiod

                   prior_muPhenoSp_mu = 100, # mean of prior distribution of the mean (grand alpha) value of the phenology model
                   prior_muPhenoSp_sigma = 20, # variance of prior distribution of the mean (grand alpha) value of the phenology model
                   prior_sigmaPhenoSp_mu = 0,#the mean of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                   prior_sigmaPhenoSp_sigma = 10,  #the varience of the prior of the spread of species phenology values around teh grand mean muPhenoSp 


                   #prior_sigma_sp_sigma = 10,  # Faith doesn't knwo what these might
                   #prior_mu_study = 0,
                   #prior_sigma_study_mu = 10,
                   #prior_sigma_study_sigma = 10,



                   prior_betaTraitxForce_mu=0, # the mean of the prior distribution of the effect of trait on the slope of forcing 
                   prior_betaTraitxForce_sigma=0.5, # the varience of the prior distribution of the effect of trait on the slope of forcing 
                   prior_betaTraitxChill_mu=0,# the mean of the prior distribution of the effect of trait on the slope of chilling 
                   prior_betaTraitxChill_sigma=0.5,# the varience of the prior distribution of the effect of trait on the slope of chilling 
                   prior_betaTraitxPhoto_mu=0,# the mean of the prior distribution of the effect of trait on the slope of photo period 
                   prior_betaTraitxPhoto_sigma=0.5# the varience of the prior distribution of the effect of trait on the slope of photo period 
                ) 

	warmupNumber <- 4000
	itterNumber <- 6000
if(runStan == TRUE){

#Run model

	mdl.phen <- stan('stan/joint_3cue_phenoonly.stan',
                     data = pheno_data, warmup=warmupNumber, iter = itterNumber, cores = 4 )

	save(mdl.phen, file = "Rfiles/phenologyMeanTrait_sim.RData")

}

if(Midge == FALSE){

load("Rfiles/phenologyMeanTrait_sim.RData")

postMeanSLA <- extract(mdl.phen)

#plot main effects of cues
postMeanSLAdf <- data.frame(postMeanSLA)

  cueEffects <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% c("muPhenoSp", "muForceSp", "muChillSp", "muPhotoSp", "sigmapheno_y")]

  cueEffectPlot <- mcmc_intervals(cueEffects) + 
     theme_classic() + 
      labs(title = "main intercept, cue slopes and general error")

      
      png("figures/simPosteriorHist.png")
      par(mfrow=c(3,4))
      #Compare results to simulated values
      hist(postMeanSLAdf$muPhenoSp, main = paste("muPhenoSp is " , signif(muPhenoSp,3), sep = ""))
      abline(v = muPhenoSp, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muForceSp, main = paste("muForceSp is " , signif(muForceSp,3), sep = ""))
      abline(v = muForceSp, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muChillSp, main = paste("muChillSp is " , signif(muChillSp,3), sep = ""))
      abline(v = muChillSp, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muPhotoSp, main = paste("muPhotoSp is " , signif(muPhotoSp,3), sep = ""))
      abline(v = muPhotoSp, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmapheno_y, main = paste("sigmapheno_y is " , signif(sigmapheno_y,3), sep = ""))
      abline(v = sigmapheno_y, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxForce, main = paste("betaTraitxForce is " , signif(betaTraitxForce,3), sep = ""))
      abline(v = betaTraitxForce, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxChill, main = paste("betaTraitxChill is " , signif(betaTraitxChill,3), sep = ""))
      abline(v = betaTraitxChill, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxPhoto, main = paste("betaTraitxPhoto is " , signif(betaTraitxPhoto,3), sep = ""))
      abline(v = betaTraitxPhoto, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaChillSp, main = paste("sigmaChillSp is " , signif(sigmaChillSp,3), sep = ""))
      abline(v = sigmaChillSp, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaForceSp, main = paste("sigmaForceSp is " , signif(sigmaForceSp,3), sep = ""))
      abline(v = sigmaForceSp, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaPhotoSp, main = paste("sigmaPhotoSp is " , signif(sigmaPhotoSp,3), sep = ""))
      abline(v = sigmaPhotoSp, col="red", lwd=3, lty=2) 
      dev.off()
      par(mfrow=c(1,1))

png("figures/simulatedPairs.png")
pairs(mdl.phen, pars = c("muForceSp", "muChillSp", "muPhotoSp", "betaTraitxForce", "betaTraitxChill", "betaTraitxPhoto", "lp__")) 
dev.off()
pairs(mdl.phen, pars = c("muForceSp", "muChillSp", "muPhotoSp", "sigmapheno_y", "lp__")) 


}






if(priorCheck == TRUE){

	#Prior Predictive Check (Run 1000 times and plot results)
	#------------------------------------------------------------

	#Number fo prior check itterations 
	nRepPrior <- 300

	#Make a data frame for input simulation data
	priorCheck <- data.frame(matrix(NA, Nph*nRepPrior, 3))
	names(priorCheck) <- c("simRep","rep","species")

	priorCheck$simRep <- rep(1:nRepPrior, each = Nph)

	priorCheck$rep <- rep(c(1:Nph), times = nRepPrior)
	priorCheck$species <- rep(rep(c(1:n_spec), each = nRep), times = nRepPrior)


	#Simulate mean SLA data per species
	meanSLA <- rnorm(n_spec, 20, 5)
	#Make this the name of the full vector of sla per species values - alphaTraitSp 
	priorCheck$alphaTraitSp <-  rep(rep(meanSLA, each = nRep), times = nRepPrior)


	#Simulate cues (z scored)
	priorCheck$forcei <- rnorm(Nph, 1, 1)
	priorCheck$photoi <- rnorm(Nph, 1, 1) # less photoperiod 
	priorCheck$chilli <- rnorm(Nph, 1, 1) #more chilling


	for (ir in 1:nRepPrior){
		# Parameter Values
		#ir <- 1

		#Species means
		sigmaPhenoSp <- rtruncnorm(1, a = 0, mean = pheno_data$prior_sigmaPhenoSp_mu, sd = pheno_data$prior_sigmaPhenoSp_sigma)
		muPhenoSp <- rnorm(1, pheno_data$prior_muPhenoSp_mu, pheno_data$prior_muPhenoSp_sigma)
		alphaPhenoSp <- rnorm(n_spec, muPhenoSp, sigmaPhenoSp)
		priorCheck$alphaPhenoSp[priorCheck$simRep == ir] <- rep(alphaPhenoSp, each = nRep)


		#Cue effects
		priorCheck$betaTraitxForce[priorCheck$simRep == ir] <- rnorm(1,pheno_data$prior_betaTraitxForce_mu,pheno_data$prior_betaTraitxForce_sigma)
		priorCheck$betaTraitxPhoto[priorCheck$simRep == ir] <- rnorm(1,pheno_data$prior_betaTraitxPhoto_mu,pheno_data$prior_betaTraitxPhoto_sigma)
		priorCheck$betaTraitxChill[priorCheck$simRep == ir] <- rnorm(1,pheno_data$prior_betaTraitxChill_mu,pheno_data$prior_betaTraitxChill_sigma)

		#Species level slopes sans trait data
		muForceSp <- rnorm(1,pheno_data$prior_muForceSp_mu,  pheno_data$prior_muForceSp_sigma)
		sigmaForceSp <- rtruncnorm(1, a = 0,mean = pheno_data$prior_sigmaForceSp_mu,sd = pheno_data$prior_sigmaForceSp_sigma)
		alphaForceSp <- rnorm(n_spec, muForceSp, sigmaForceSp)
		priorCheck$alphaForceSp[priorCheck$simRep == ir] <- rep(alphaForceSp, each = nRep)

		muPhotoSp <- rnorm(1, pheno_data$prior_muPhotoSp_mu, pheno_data$prior_muPhotoSp_sigma)
		sigmaPhotoSp <- rtruncnorm(1, a = 0,mean = pheno_data$prior_sigmaPhotoSp_mu, sd = pheno_data$prior_sigmaPhotoSp_sigma )
		alphaPhotoSp <- rnorm(n_spec, muPhotoSp, sigmaPhotoSp)
		priorCheck$alphaPhotoSp[priorCheck$simRep == ir] <- rep(alphaPhotoSp, each = nRep)

		muChillSp <-  rnorm(1,pheno_data$prior_sigmaChillSp_mu,pheno_data$prior_sigmaChillSp_sigma)
		sigmaChillSp <- rtruncnorm(1, a = 0,mean = pheno_data$prior_sigmaChillSp_mu,sd = pheno_data$prior_sigmaChillSp_sigma)
		alphaChillSp <- rnorm(n_spec, muChillSp, sigmaChillSp)
		priorCheck$alphaChillSp[priorCheck$simRep == ir] <- rep(alphaChillSp, each = nRep)


		#general varience
		priorCheck$sigmapheno_y[priorCheck$simRep == ir] <- rtruncnorm(pheno_data$prior_sigmaphenoy_mu,  a = 0, pheno_data$prior_sigmaphenoy_sigma)
		priorCheck$e[priorCheck$simRep == ir] <- rnorm(Nph, 0, sigmapheno_y)

	}# end simulating new priors, from here vectorize code
		#slopes for each cue, combining trait and non-trait aspect of the slope.

	
	priorCheck$betaForceSp <- priorCheck$alphaForceSp + priorCheck$betaTraitxForce *  priorCheck$alphaTraitSp
	 
	priorCheck$betaPhotoSp <-  priorCheck$alphaPhotoSp + priorCheck$betaTraitxPhoto * priorCheck$alphaTraitSp

	priorCheck$betaChillSp <-  priorCheck$alphaChillSp + priorCheck$betaTraitxChill * priorCheck$alphaTraitSp

	#Run full model to get mean simulated y values
	priorCheck$yMu <-  priorCheck$alphaPhenoSp +  priorCheck$betaForceSp * priorCheck$forcei +  priorCheck$betaPhotoSp* priorCheck$photoi + priorCheck$betaChillSp * priorCheck$chilli

	#Final values
	priorCheck$yPhenoi <- priorCheck$yMu + priorCheck$e

	png("figures/densityYPrior.png")
	plot(density(priorCheck$yPhenoi))
	dev.off()

	png("figures/photoPlotPrior.png")
	plot(priorCheck$yPhenoi ~ priorCheck$photoi, xlab = "Photoperiod", ylab = "Phenological Date")
	dev.off()

	png("figures/forcingPlotPrior.png")
	plot(priorCheck$yPhenoi ~ priorCheck$forcei, xlab = "Forcing", ylab = "Phenological Date")
	dev.off()

	png("figures/chillingPlotPrior.png")
	plot(priorCheck$yPhenoi ~ priorCheck$chilli, xlab = "Chillina", ylab = "Phenological Date")
	dev.off()



}













if(BayesSweave == TRUE){
	#For the BayesClass sweave documents 
	setwd("/home/faith/Documents/github/bayes2020/Projects/Faith/traitorsModel")
}