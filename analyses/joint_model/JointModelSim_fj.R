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
table(simulatedTraits$Trait)

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





























































#what if there were multiple traits considered?

simulatedTraits <- data.frame(matrix(NA, N, 2))
names(simulatedTraits) <- c("obs", "Trait")
simulatedTraits$obs <- c(1:N)
simulatedTraits$Trait <- rep(1:nTrait, each = nSpecies)
simulatedTraits$Species <- rep(1:nSpecies, times = nTrait)
simulatedTraits$Study <- rep(1:nStudy, each = nTrait*nSpecies)
tail(simulatedTraits)
table(simulatedTraits$Trait)

#trait and species specific interecpts
sigmaTraitAlpha <- 8 # variation around alpha because of the trait
sigmaTraitAlphaY <- rtruncnorm(nTrait, 0, sigmaTraitAlpha) # get an alpha value for each trait 

sigmaAlphaSpecies <- 4 # varation for different species 
#sigmaAlphaSpeciesS <- rtruncnorm(nSpecies, 0, sigmaAlphaSpecies) # get intercept values for each species

for (i in i:nTrait){ # modify species intercepts based on the effect of trait 
	iTrait <- rnorm(nSpecies, sigmaTraitAlphaY[i], sigmaAlphaSpecies)  
	simulatedTraits$AlphaTraitSpecies[simulatedTraits$Trait == i] <- iTrait
}


sigmaAlphaTrait <- 5 # variation  between different trait/species combinations  
alphaTraitSp <- rnorm(nSpecies*nTrait, muAlphaTrait, sigmaAlphaTrait) # get iterecpt values for each species/trait 
simulatedTraits$alphaTraitSp <- rep(alphaTraitSp , times = nStudy) # repeat values 15 times so each observation has a trait alpha attached 

#study specific intercepts 
muAlphaStudy <- 12 # mean of teh effect of study 
sigmaAlphaStudy <- 6 # variation for different studies in the intercept 
alphaStudy <- rnorm(nStudy, muAlphaStudy, sigmaAlphaStudy ) # getting an intercept values for each study 
simulatedTraits$alphaStudy <- rep(alphaStudy, each = nTrait*nSpecies) # repeat values as many times as there are traits and species

#general varience
sigma2Trait <- 2 
sigma2TraitY <- rtruncnorm(nTrait, 0 , sigma2Trait)
for ( i in 1:nTrait){
	eTraiti <- rtruncnorm(n*nSpecies, 0, sigma2TraitY[i]) # get a distribution of observation errors based on trait variation 
	simulatedTraits$eTrait[simulatedTraits$Trait == i] <- eTraiti 

}
simulatedTraits$eTrait <- rnorm(N, 0, sigma2Trait)

