### Dan changes, based on Faith fake data for traits joint model. This fits a 1 parementer version of the ospree model.
rm(list=ls()) 
options(stringsAsFactors = FALSE)
dev.off()

setwd("~/Documents/git/ospree/analyses/ranges/popUP/")

#library(truncnorm)
library(rstan)



nSpecies<-20 # number of species
#in this part of the model as there was in the first part


nph <- 200 # number of observations per species/phenological combination 
#Nph <- nSpecies * nForcing * nph * nPheno #overall number of observations
Nph <- nSpecies * nph # 20 obervations per species for phenological event and forcing 

#make a dataframe to keep things organised
phenoData <- data.frame(matrix(NA, Nph, 2))
names(phenoData) <- c("obs", "species")
phenoData$obs <- c(1:Nph)
phenoData$species <- rep(c(1:nSpecies), each = nph)

### treatment data
#climate variable value for each species
muRangeSp<-10
sigmaRangeSp<-4
climparam <- rnorm(nSpecies, muRangeSp, sigmaRangeSp)
phenoData$climparam <- rep(climparam , each = nph)

#big F in the  model - I think this should be the x value, although there is no i in the lizzie's annotation 
muForcing <- 20 #amount of GDD on average?
sigmaForcing <- 1 # how much teh amount of forcing varies for all values 
forcingi <- rnorm(Nph, muForcing, sigmaForcing)
phenoData$forcingi <- forcingi




#phenological values for different species
muPhenoSp <- 150# day 100 is mean phenological date
sigmaPhenoSp <- 2 # species generally vary around 2 days from mean 150

alphaPhenoSp <- rnorm(nSpecies, muPhenoSp, sigmaPhenoSp) 
phenoData$alphaPhenoSp <- rep(alphaPhenoSp, each = nph)

#different forcing values for each species 
muForcingSp <- -1# mean effect of forcing is negative because more forcing means budburst earlier 
sigmaForcingSp <- 0.1
alphaForcingSp <- rnorm(nSpecies, muForcingSp, sigmaForcingSp)

#interaction between trait and phenology?
betaTraitxPheno <- -.2 # more

#combine teh effects of forcing and species trait differences into a slope
betaForcingSP1 <- alphaForcingSp + climparam*betaTraitxPheno
betaForcingSp <- rep(betaForcingSP1, )
phenoData$betaForcingSp <- rep(betaForcingSp, each = nph)


#general variance
ePhenoSigma <- 2
ePheno <- rnorm(Nph, 0, ePhenoSigma) 
phenoData$ePheno <- ePheno

#"run" the full model to simulate data 
phenoData$yPhenoi <- phenoData$alphaPhenoSp + phenoData$betaForcingSp * phenoData$forcingi + phenoData$ePheno


hist(phenoData$yPhenoi )


#build a stan model 
#--------------------------------------------------------------
faker<- with(phenoData, 
             list(yPhenoi = yPhenoi, 
                  forcingi = forcingi,
                  species = species,
                  N = nrow(phenoData),
                  n_spec = length(unique(phenoData$species)),
                  climvar=climparam
             ))

mod1<-stan('stan/jointish_climvar_db.stan', data = faker,
           iter = 4000, warmup=3000, chains=4)

posterior2 <- extract(mod1)

plot(density(posterior2$sigmapheno_y )) # 
ePhenoSigma

