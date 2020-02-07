# simulating data for a joint model 
#------------------------------------

#started by Faith 6th feb 2020
#simulating data from teh joint model Lizzie wrote after taklking to Michael Betancourt (december 18th 2019)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
dev.off()

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


#different alpha values for each species
muTrait <- 8 # mean of the distribution of intercepts of different species for that trait 
sigmaAlphTrait <- 4 # variation around the mean because species differ a bit  
alphaTraitSp <- rnorm(nSpecies, muTrait, sigmaAlphTrait)
simulatedTrait$alphaTraitSp <- rep(alphaTraitSp, times = nStudy) # pop in the dataframe 

#study specific intercepts 
muAlphaStudy <- 12 # mean of teh effect of study 
sigmaAlphaStudy <- 6 # variation for different studies in the intercept 
alphaStudy <- rnorm(nStudy, muAlphaStudy, sigmaAlphaStudy ) # getting an intercept values for each study 
simulatedTrait$alphaStudy <- rep(alphaStudy, each = nSpecies) # repeat values as many times as there are traits and species

#general varience
sigma2Trait <- 2 
simulatedTrait$eTrait <- rnorm(N, 0, sigma2Trait)

#simulate teh data by "running" the model 
simulatedTrait$yTraiti <- simulatedTrait$alphaTraitSp + simulatedTrait$alphaStudy + simulatedTrait$eTrait

#build a model in stan that gets these values back
#-----------------------------------------------------
stan_data <- list(yTraiti = simulatedTrait$yTraiti, N = N, n_spec = nSpecies, species = simulatedTrait$species, 
	study = simulatedTrait$Study, n_study = nStudy)

write("// running a simle model of teh fisrt part of teh joint model
	// it should get species specific trait values 


data {
	int < lower = 1 > N; // Sample size
 
 	//level 1
 	vector[N] yTraiti; // Outcome

 	//level 2
	int < lower = 1 > n_spec; // number of random effect levels (species) 
	int < lower = 1, upper = n_spec > species[N]; // id of random effect (species)

	int < lower = 1 > n_study; // number of random effect levels (study) 
	int < lower = 1, upper = n_study > study[N]; // id of random effect (study)

}


parameters{

	//level 1
	// general varience/error
	real <lower =0> sigmaTrait_y; // overall variation accross observations


	//level 2
	real <lower = 0> sigma_sp; // variation of intercept amoung species
	real muSp[n_spec]; // mean of the alpha value for species

	real <lower = 0> sigma_stdy; // variation of intercept amoung studies
	real muStdy[n_study]; // mean of the alpha value for studies 

}

transformed parameters{
	//Individual mean for species and study
	real ymu[N];

	//Individual mean calculation 
	for (i in 1:N){
		ymu[i] = muSp[species[i]] + muStdy[study[i]];  
	}
}
model{ 
	//assign priors
	sigmaTrait_y ~ normal(0,5);

	sigma_sp ~ normal(0,5);
	muSp ~ normal(10, sigma_sp);

	sigma_stdy ~ normal(0, 5);
	muStdy ~ normal(10, sigma_stdy);

	// run the actual model - likihood
	for (i in 1:N){
		yTraiti[i] ~ normal(ymu[i], sigmaTrait_y);
	}


}


generated quantities {
} // The posterior predictive distribution",

"stan_Part1.stan")


stan_Part1 <- "stan_Part1.stan"


fit1 <- stan(file = stan_Part1, data = stan_data, warmup = 1000, iter = 4000, chains = 4, cores = 4, thin = 1)

str(fit1)

posterior1 <- extract(fit1)
str(fit1)

# values look close enouph, and rhat = 1
plot(density(posterior1$sigmaTrait_y ))
plot(density(posterior1$muSp ))
plot(density(posterior1$sigma_stdy))
plot(density(posterior1$sigma_sp))

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
muPhenoSp <- 3
sigmaPhenoSp <- 5
alphaPhenoSp <- rnorm(nSpecies, muPhenoSp, sigmaPhenoSp) 
phenoData$alphaPhenoSp <- rep(alphaPhenoSp, each = nph)

#different forcing values for each species 
muForcingSp <- -2
sigmaForcingSp <- 2
alphaForcingSp <- rnorm(nSpecies, muForcingSp, sigmaForcingSp)

#interaction between trait and phenology?
betaTraitxPheno <- 1.5

#combine teh effects of forning and species trait differences 
betaForcingSP1 <- alphaForcingSp + alphaTraitSp*betaTraitxPheno
betaForcingSp <- rep(betaForcingSP1, )
phenoData$betaForcingSp <- rep(betaForcingSp, each = nph)

#big F in the  model - some sort of slope? 
Forcing <- 2

#general variance
ePhenoSigma <- 4
ePheno <- rnorm(Nph, 0, ePhenoSigma) 
phenoData$ePheno <- ePheno

#"run" the full model to simulate data 
phenoData$yPhenoi <- phenoData$alphaPhenoSp + phenoData$betaForcingSp * Forcing + phenoData$ePheno

























































