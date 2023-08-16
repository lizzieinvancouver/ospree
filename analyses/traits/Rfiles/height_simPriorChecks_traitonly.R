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
Nstudy <- 40 # number of studies w/ traits (10 seems a little low for early simulation code; remember that you are estimating a distribution of this the same as for species)
Nspp <- 40 # number of species 

# First making a data frame for the test trait data
Ntrt <- Nspp * Nstudy * Nrep # total number of traits observations
Ntrt

mu.grand <- 15 # the grand mean of the LNC model
sigma.species <- 5 # we want to keep the variaiton across spp. high
sigma.study <- 5
#sigmaTrait_y <- 2

#make a dataframe for LNC
trt.dat <- data.frame(matrix(NA, Ntrt, 1))
names(trt.dat) <- c("rep")
trt.dat$rep <- c(1:Nrep)
trt.dat$study <- rep(rep(c(1:Nstudy), each = Nspp), each = Nrep)

muStudy <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$muStudy <-  rep(muStudy, each = Nspp*Nrep)  #rep(rep(alphaTraitSp, times = Nstudy), each = Nrep)  # generate data for ea study
trt.dat$species <- rep(rep(c(1:Nspp), times = Nstudy), each = Nrep)
# trt.dat$study <- rep(c(1:Nstudy), each = Nspp)
# trt.dat$species <- rep(1:Nspp, Nstudy)

# now generating the species trait data, here it is for LNC
#the alphaTraitSp in Faiths original code:
alphaTraitSp <- rnorm(Nspp, 0, sigma.species)
trt.dat$alphaTraitSp <- rep(rep(alphaTraitSp, times = Nstudy), each = Nrep) 

#now generating the effects of study


# general variance
trt.var <- 2 #sigmaTrait_y in the stan code
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
                 study = as.numeric(as.factor(trt.dat$study ))
                
                 )


	warmupNumber <- 2000
	itterNumber <- 4000

mdl.traitonly <- stan("stan/phenology_combined_traitonly.stan",
                       data = all.data,
                       iter = itterNumber,
                       warmup = warmupNumber,
                       chains = 4,
                       cores = 4,
                       include = FALSE, pars = c("y_hat"))
  save(mdl.traitonlyGeoff, file="output/traitonly_5_5_largeNsppNstudy.Rdata")
  
  postMeanSLA <- rstan::extract(mdl.traitonly)
  
  sum.jfcp <- summary(mdl.traitonly)$summary
  
  mu_grand <- sum.jfcp[grep("mu_grand", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
  sigma_sp <- sum.jfcp[grep("sigma_sp", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
  sigma_studyesti <- sum.jfcp[grep("sigma_study", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
  sigmaTrait_y <- sum.jfcp[grep("sigma_traity", rownames(sum.jfcp)), c("mean","2.5%","97.5%")]
  
trt.out  <- data.frame( 
  "Parameter" = c("mu_grand","sigma_sp","sigma_study","sigmaTrait_y"),  
  "Test.data.values" = c(mu.grand, sigma.species, sigma.study, trt.var) ,
  "Estiamte"= c(mu_grand[1,1], sigma_sp[1], sigma_studyesti[1], sigmaTrait_y[1]),
  "2.5"= c(mu_grand[1,2], sigma_sp[2], sigma_studyesti[2], sigmaTrait_y[2]),
  "97.5"= c(mu_grand[1,3], sigma_sp[3], sigma_studyesti[3], sigmaTrait_y[3])
                       
)


  muStudyEsti <- sum.jfcp[grep("muStudy", rownames(sum.jfcp))]
  muSpEsti <- sum.jfcp[grep("muSp", rownames(sum.jfcp))]
  
  #pdf("mu_study_esti.pdf",width = 10, height = 5)
  par(mfrow = c(1,2))
  plot(muStudyEsti ~ muStudy, xlab = "simulated muStudy", ylab = "mdl estimated muStudy")
  abline(0,1)
  plot(muSpEsti ~ alphaTraitSp, xlab = "simulated muSpecies", ylab = "mdl estimated muSpecies")
  abline(0,1)
 # dev.off()
  	