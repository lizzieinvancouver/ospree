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

# Anyone else working with this code should add their info/path here
	if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
		} else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
		} else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
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

mu.grand <- 15 # the grand mean of the LNC model
sigma.species <- 5 # we want to keep the variaiton across spp. high
sigma.study <- 5
sigmaTrait_y <- 2

#make a dataframe for LNC
trt.dat <- data.frame(matrix(NA, Ntrt, 1))
names(trt.dat) <- c("rep")
trt.dat$rep <- c(1:Nrep)
trt.dat$study <- rep(rep(c(1:Nstudy), each = Nspp), each = Nrep)
trt.dat$species <- rep(rep(c(1:Nspp), times = Nstudy), each = Nrep)

alphaTraitSp <- rnorm(Nspp, 0, sigma.species)
trt.dat$alphaTraitSp <- rep(rep(alphaTraitSp, times = Nstudy), each = Nrep) 

muStudy <- rnorm(Nstudy, 0, sigma.study) 
#trt.dat$muStudy <- rep(rep(muStudy, times = Nspp), each = Nstudy) 
trt.dat$muStudy <- rep(muStudy, each = Nrep*Nspp) 

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
#plot(density(pheno.datTrait$yPhenoi))

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
                 # prior_mu_grand_mu = 20,
                 # prior_mu_grand_sigma = 10,
                 # prior_sigma_sp_mu = 4,
                 # prior_sigma_sp_sigma = 5,
                 # prior_sigma_study_mu = 2,
                 # prior_sigma_study_sigma = 5,
                 # prior_sigma_traity_mu = 3,
                 # prior_sigma_traity_sigma = 5,
                 Nph = nrow(pheno.datTrait),
                 phenology_species = as.numeric(as.factor(pheno.datTrait$species )),
                 species2 = as.numeric(as.factor(pheno.datTrait$species )),
                 yPhenoi = pheno.datTrait$yPhenoi,
                 forcei = pheno.datTrait$forcei,
                 chilli = pheno.datTrait$chilli,
                 photoi = pheno.datTrait$photoi
                 # prior_muForceSp_mu = -15,
                 # prior_muForceSp_sigma = 10, #wider
                 # prior_muChillSp_mu = -15,
                 # prior_muChillSp_sigma = 10,#wider
                 # prior_muPhotoSp_mu = -15,
                 # prior_muPhotoSp_sigma = 10,#wider
                 # prior_muPhenoSp_mu = 40,
                 # prior_muPhenoSp_sigma = 10,#wider
                 # prior_sigmaForceSp_mu = 5,
                 # prior_sigmaForceSp_sigma = 5,
                 # prior_sigmaChillSp_mu = 5,#wider
                 # prior_sigmaChillSp_sigma = 5, #wider
                 # prior_sigmaPhotoSp_mu = 5,
                 # prior_sigmaPhotoSp_sigma = 5,
                 # prior_sigmaPhenoSp_mu = 5, #wider
                 # prior_sigmaPhenoSp_sigma = 5, #wider
                 # prior_betaTraitxForce_mu = 0,
                 # prior_betaTraitxForce_sigma = 1,
                 # prior_betaTraitxChill_mu = 0,
                 # prior_betaTraitxChill_sigma = 1,
                 # prior_betaTraitxPhoto_mu = 0,
                 # prior_betaTraitxPhoto_sigma = 1,
                 # prior_sigmaphenoy_mu = 10,
                 # prior_sigmaphenoy_sigma = 5 #wider
                 )


	warmupNumber <- 2000
	itterNumber <- 4000


# mdl.trait <- stan("stan/joint_3cue_newprior.stan",
#                        data = all.data,
#                        iter = itterNumber,
#                        warmup = warmupNumber,
#                        chains = 4,
#                        cores = 4,
#                        include = FALSE, pars = c("y_hat"))
#   save(mdl.trait, file="output/traitonlysecond.Rdata")
#   
#   postMeanSLA <- rstan::extract(mdl.trait)
#   # 
#   #plot main effects of cues
#   postMeanSLAdf <- data.frame(postMeanSLA)
#   
  
  mdl.traitphenSim <- stan("stan/phenology_combined.stan",
                      data = all.data,
                      iter = itterNumber,
                      warmup = warmupNumber,
                      chains = 4,
                      cores = 4,
                      include = FALSE, pars = c("y_hat"))

	save(mdl.traitphenSim, file = "phenologyMeanTrait_sim_5_5.RData")
	sum.jfcp <- summary(mdl.traitphenSim)$summary
 
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
      png("simPosteriorHist_height_5_5.png")
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

	sum.jfcp <- summary(mdl.traitphenSim)$summary
	
	mu_grand <- sum.jfcp[grep("mu_grand", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
	sigma_sp <- sum.jfcp[grep("sigma_sp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	sigma_studyesti <- sum.jfcp[grep("sigma_study", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	sigmaTrait_y <- sum.jfcp[grep("sigma_traity", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	
	mu_chillsp <- sum.jfcp[grep("muChillSp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	sigma_chillsp <- sum.jfcp[grep("sigmaChillSp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	beta_tc <- sum.jfcp[grep("betaTraitxChill", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	
	mu_photosp <- sum.jfcp[grep("muPhotoSp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
	sigma_photosp <- sum.jfcp[grep("sigmaPhotoSp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
	beta_tp <- sum.jfcp[grep("betaTraitxPhoto", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	
	mu_forcesp <- sum.jfcp[grep("muForceSp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	mu_phenosp <- sum.jfcp[grep("muPhenoSp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	alpha.forcingsp <- sum.jfcp[grep("alphaForcingSp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	sigma_forcesp <- sum.jfcp[grep("sigmaForceSp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	sigma_phenosp <- sum.jfcp[grep("sigmaPhenoSp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	sigma_phenoy <- sum.jfcp[grep("sigmapheno_y", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
	beta_tf <- sum.jfcp[grep("betaTraitxForce", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
	
	mdl.out <- data.frame( "Parameter" = c("mu_grand","sigma_sp","sigma_study","sigmaTrait_y","mu_forcesp","mu_chillsp","mu_photosp","mu_phenosp","sigma_forcesp","sigma_chillsp","sigma_photosp", "sigma_phenosp", "sigma_phenoy", "beta_tf", "beta_tc","beta_tp"),  "Test.data.values" = c(mu.grand, sigma.species, sigma.study, trt.var,                                                                    muForceSp, muChillSp, muPhotoSp, muPhenoSp, sigmaForceSp, sigmaChillSp, sigmaPhotoSp, sigmaPhenoSp,sigmapheno_y, betaTraitxForce, betaTraitxChill, betaTraitxPhoto) ,
	                       "Estiamte"= c(mu_grand[1,1], sigma_sp[1], sigma_studyesti[1], sigmaTrait_y[1],  mu_forcesp[1], mu_chillsp[1], mu_photosp[1], mu_phenosp[1], sigma_forcesp[1], sigma_chillsp[1], sigma_photosp[1], sigma_phenosp[1], sigma_phenoy[1], beta_tf[1], beta_tc[1],  beta_tp[1]),"2.5"= c(mu_grand[1,2], sigma_sp[2], sigma_studyesti[2], sigmaTrait_y[2],  mu_forcesp[2], mu_chillsp[2], mu_photosp[2], mu_phenosp[2], sigma_forcesp[2], sigma_chillsp[2], sigma_photosp[2], sigma_phenosp[2], sigma_phenoy[2], beta_tf[2], beta_tc[2],  beta_tp[2]),"97.5"= c(mu_grand[1,3], sigma_sp[3], sigma_studyesti[3], sigmaTrait_y[3],  mu_forcesp[3], mu_chillsp[3], mu_photosp[3], mu_phenosp[3], sigma_forcesp[3], sigma_chillsp[3], sigma_photosp[3], sigma_phenosp[3], sigma_phenoy[3], beta_tf[3], beta_tc[3],  beta_tp[3])
	                       
	)
	
	mdl.out
	
	muPhenoSp <- sum.jfcp[grep("alphaPhenoSp", rownames(sum.jfcp))]
	muTraitSp <- sum.jfcp[grep("muSp", rownames(sum.jfcp))]
	muStudyEsti <- sum.jfcp[grep("muStudy", rownames(sum.jfcp))]
	
	pdf("muRespSp_study_esti_discourse.pdf",width = 10, height = 5)
	par(mfrow = c(1,3))
	plot(muTraitSp ~ alphaTraitSp, xlab = "simulated muTraitSp", ylab = "mdl estimated muTraitSp")
	abline(0,1)
	plot(muStudyEsti ~ muStudy, xlab = "simulated muStudy", ylab = "mdl estimated muStudy")
	abline(0,1)
	plot(muPhenoSp ~ alphaPhenoSp, xlab = "simulated muPhenoSp", ylab = "mdl estimated muPhenoSp")
	abline(0,1)
	dev.off()
	
	muStudyEsti <- sum.jfcp[grep("muStudy", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
temp <- cbind(muStudy, muStudyEsti)
colnames(temp) <- c("muStudy_param", "muStudy_esti", "2.5", "97.5")
	
	