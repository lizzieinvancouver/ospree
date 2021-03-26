### Dan changes, based on Faith fake data for traits joint model. This fits a 1 parementer version of the ospree model.


### Dan changes, based on Faith fake data for traits joint model. This fits a 1 parementer version of the ospree model.
rm(list=ls()) 
options(stringsAsFactors = FALSE)
dev.off()

setwd("~/Documents/git/ospree/analyses/ranges/popUP/")

#library(truncnorm)
library(rstan)



nSpecies<-60 # number of species
#in this part of the model as there was in the first part


#nPheno <- 12 # there shoudl be a phenology and forcing value for each species 
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
sigmaForcing <- 5 # how much teh amount of forcing varies for all values 
forcingi <- rnorm(Nph, muForcing, sigmaForcing)
phenoData$forcingi <- forcingi


##chilling

muChilling<- 20 #amount of chillingon average?
sigmaChilling<- 5 # how much teh amount of forcing varies for all values 
chillingi <- rnorm(Nph, muChilling, sigmaChilling)
phenoData$chillingi <- chillingi

muPhoto<-20 #amount of photo average?
sigmaPhoto<- 5 # how much teh amount of forcing varies for all values 
photoi <- rnorm(Nph, muPhoto, sigmaPhoto)
phenoData$photoi <- photoi




#phenological values for different species
muPhenoSp <- 150# day 100 is mean phenological date
sigmaPhenoSp <- 2 # species generally vary around 2 days from mean 150

alphaPhenoSp <- rnorm(nSpecies, muPhenoSp, sigmaPhenoSp) 
phenoData$alphaPhenoSp <- rep(alphaPhenoSp, each = nph)

#different forcing values for each species 
muForcingSp <- -1# mean effect of forcing is negative because more forcing means budburst earlier 
sigmaForcingSp <- 0.1
alphaForcingSp <- rnorm(nSpecies, muForcingSp, sigmaForcingSp)


muChillingSp <- -2# mean effect of forcing is negative because more forcing means budburst earlier 
sigmaChillingSp <- 0.1
alphaChillingSp <- rnorm(nSpecies, muChillingSp, sigmaChillingSp)

muPhotoSp <- -2# mean effect of forcing is negative because more forcing means budburst earlier 
sigmaPhotoSp <- 0.1
alphaPhotoSp <- rnorm(nSpecies, muPhotoSp, sigmaPhotoSp)


#interaction between trait and phenology?
betaTraitxchill <- -.8 # more
betaTraitxphoto<- -.8 # more
betaTraitxforce <- .8 # more

#combine teh effects of forcing and species trait differences into a slope
betaForcingSP1 <- alphaForcingSp + climparam*betaTraitxforce
betaForcingSp <- rep(betaForcingSP1, )
phenoData$betaForcingSp <- rep(betaForcingSp, each = nph)

betaChillingSP1 <- alphaChillingSp + climparam*betaTraitxchill
betaChillingSp <- rep(betaChillingSP1, )
phenoData$betaChillingSp <- rep(betaChillingSp, each = nph)



betaPhotoSP1 <- alphaPhotoSp + climparam*betaTraitxphoto
betaPhotoSp <- rep(betaPhotoSP1, )
phenoData$betaPhotoSp <- rep(betaPhotoSp, each = nph)


#general variance
ePhenoSigma <- 2
ePheno <- rnorm(Nph, 0, ePhenoSigma) 
phenoData$ePheno <- ePheno

#"run" the full model to simulate data 
phenoData$yPhenoi <- phenoData$alphaPhenoSp + phenoData$betaForcingSp * phenoData$forcingi+
  phenoData$betaChillingSp * phenoData$chillingi +
  phenoData$betaPhotoSp * phenoData$photoi+
  phenoData$ePheno



hist(phenoData$yPhenoi )
faker<- list(yPhenoi = phenoData$yPhenoi, 
                  forcingi = phenoData$forcingi,
                  chillingi = phenoData$chillingi,
                  photoi = phenoData$photoi,
                  species = phenoData$species,
                  N = nrow(phenoData),
                  n_spec = length(unique(phenoData$species)),
                  climvar=climparam
             )



fake_jnt = stan('stan/joint_climvar_3param_db.stan', data = faker,
                      iter = 6000, warmup=4000)
