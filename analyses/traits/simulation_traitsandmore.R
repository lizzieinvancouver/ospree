
## Load libraries
library(rstan)
require(shinystan)

## Set number of cores
options(mc.cores = 4)

## Set seed
set.seed(202109)

 if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")}

## Set parameters
param <- list(
    Nrep = 20, # rep per trait
    Nstudy = 15, # number of studies with traits
    Nspp = 15, # number of species with traits
    trait_mu_grand = 20,
    trait_sigma_sp = 4,
    trait_sigma_study = 2,
    trait_sigma_traity = 3,
    phenology_muForce = 2,
    phenology_muChill = -2.1,
    phenology_muPhoto = 0.45,
    phenology_sigmaForce = .5,
    phenology_sigmaChill = .6,
    phenology_sigmaPhoto = .7,
    phenology_betaTraitForce = .34,
    phenology_betaTraitChill = .25,
    phenology_betaTraitPhoto = -.5,
    phenology_muAlpha = 10,
    phenology_sigmaAlpha = 1,
    phenology_sigmaPheno = 2)

## Generate species and study offsets (traits)
mu_sp <- rnorm(n = param[["Nspp"]], mean = 0, sd = param[["trait_sigma_sp"]])
mu_study <- rnorm(n = param[["Nstudy"]], mean = 0, sd = param[["trait_sigma_study"]])

## Generate species and study offsets (phenology)
alphaPheno <- rnorm(n = param[["Nspp"]], mean = param[["phenology_muAlpha"]], sd = param[["phenology_sigmaAlpha"]])
alphaForce <- rnorm(n = param[["Nspp"]], mean = param[["phenology_muForce"]], sd = param[["phenology_sigmaForce"]])
alphaChill <- rnorm(n = param[["Nspp"]], mean = param[["phenology_muChill"]], sd = param[["phenology_sigmaChill"]])
alphaPhoto <- rnorm(n = param[["Nspp"]], mean = param[["phenology_muPhoto"]], sd = param[["phenology_sigmaPhoto"]])

## Make empty table
yTable <- data.frame(Species = c(),
                     Study = c(),
                     Replicate = c(),
                     mu_grand = c(),
                     mu_sp = c(),
                     mu_study = c(),
                     alphaPheno = c(),
                     alphaForce = c(),
                     alphaChill = c(),
                     alphaPhoto = c())
## Fill table with parameters
for(i in 1:param[["Nspp"]]){
    for(j in 1:param[["Nstudy"]]){        
        temp <- data.frame(Species = i,
                           Study = j,
                           Replicate = 1:param[["Nrep"]],
                           mu_grand = param[["trait_mu_grand"]],
                           mu_sp = mu_sp[i],
                           mu_study = mu_study[j],
                           alphaPheno = alphaPheno[i],
                           alphaForce = alphaForce[i],
                           alphaChill = alphaChill[i],
                           alphaPhoto = alphaPhoto[i])
        yTable <- rbind(yTable, temp)
    }
}

## Calculate latent trait values
yTable$trait_latent <- yTable$mu_grand + yTable$mu_sp + yTable$mu_study
## Generate trait observation using parameters
yTable$yTraiti <- rnorm(n = nrow(yTable),
                        mean = yTable$trait_latent,
                        sd = param[["trait_sigma_traity"]])

## Calculate response parameters
yTable$betaForce <- yTable$alphaForce + param[["phenology_betaTraitForce"]] * yTable$trait_latent
yTable$betaChill <- yTable$alphaChill + param[["phenology_betaTraitChill"]] * yTable$trait_latent
yTable$betaPhoto <- yTable$alphaPhoto + param[["phenology_betaTraitPhoto"]] * yTable$trait_latent

## Generate forcing, chilling, photo
yTable$forcei <- rnorm(n = nrow(yTable), mean = 0, sd = .5)
yTable$chilli <- rnorm(n = nrow(yTable), mean = 0, sd = .5)
yTable$photoi <- rnorm(n = nrow(yTable), mean = 0, sd = .5)

## Generate phenology response
yTable$yPhenologyi <- rnorm(n = nrow(yTable),
                            mean = yTable$alphaPheno + yTable$betaForce * yTable$forcei + yTable$betaChill * yTable$chilli + yTable$betaPhoto * yTable$photoi,
                            sd = param[["phenology_sigmaPheno"]])


## Prepare all data for Stan
all_data <- list(yTraiti = yTable$yTraiti,
                   N = nrow(yTable),
                   n_spec = param[["Nspp"]], 
                   trait_species = yTable$Species,
                   n_study = param[["Nstudy"]],
                   study = yTable$Study,
                   prior_mu_grand_mu = param[["trait_mu_grand"]],
                   prior_mu_grand_sigma = 5,
                   prior_sigma_sp_mu = param[["trait_sigma_sp"]],
                   prior_sigma_sp_sigma = 5,
                   prior_sigma_study_mu = param[["trait_sigma_study"]],
                   prior_sigma_study_sigma = 5,
                   prior_sigma_traity_mu = param[["trait_sigma_traity"]],
                   prior_sigma_traity_sigma = 5,
                   ## Phenology
                   phenology_species = yTable$Species,
                   Nph = nrow(yTable),
                   yPhenoi = yTable$yPhenologyi,
                   forcei = yTable$forcei,
                   chilli = yTable$chilli,
                   photoi = yTable$photoi,
                   prior_muForceSp_mu = param[["phenology_muForce"]],
                   prior_muForceSp_sigma = .5,
                   prior_muChillSp_mu = param[["phenology_muChill"]],
                   prior_muChillSp_sigma = 5,
                   prior_muPhotoSp_mu = param[["phenology_muPhoto"]],
                   prior_muPhotoSp_sigma = .1,
                   prior_muPhenoSp_mu = param[["phenology_muAlpha"]],
                   prior_muPhenoSp_sigma = 2,
                   prior_sigmaForceSp_mu = param[["phenology_sigmaForce"]],
                   prior_sigmaForceSp_sigma = 0.1,
                   prior_sigmaChillSp_mu = param[["phenology_sigmaChill"]],
                   prior_sigmaChillSp_sigma = 0.05,
                   prior_sigmaPhotoSp_mu = param[["phenology_sigmaPhoto"]],
                   prior_sigmaPhotoSp_sigma = 0.1,
                   prior_sigmaPhenoSp_mu = param[["phenology_sigmaAlpha"]],
                   prior_sigmaPhenoSp_sigma = .1,
                   prior_betaTraitxForce_mu = param[["phenology_betaTraitForce"]],
                   prior_betaTraitxForce_sigma = 0.1,
                   prior_betaTraitxChill_mu = param[["phenology_betaTraitChill"]],
                   prior_betaTraitxChill_sigma = 0.1,
                   prior_betaTraitxPhoto_mu = param[["phenology_betaTraitPhoto"]],
                   prior_betaTraitxPhoto_sigma = .1,
                   prior_sigmaphenoy_mu = param[["phenology_sigmaPheno"]],
                   prior_sigmaphenoy_sigma = 0.5
                   ) 

mdl.traitphen <- stan("stan/phenology_combined.stan",
                      data = all_data,
                      iter = 2000,
                      warmup = 1000,
                      chains = 4,
                      include = FALSE, pars = c("y_hat"),
                      seed = 202109)

summary(mdl.traitphen, pars = c("mu_grand"))$summary
## True value
param[["trait_mu_grand"]]

summary(mdl.traitphen)$summary[, "n_eff"]

summary(mdl.traitphen, pars = c("betaTraitxForce"))$summary
## True value
param[["phenology_betaTraitForce"]]







summary(mdl.traitonly, pars = c("sigma_sp"))$summary
## True value
param[["trait_sigma_sp"]]

summary(mdl.traitonly, pars = c("sigma_study"))$summary
## True value
param[["trait_sigma_study"]]

summary(mdl.traitonly, pars = c("sigma_traity"))$summary
## True value
param[["trait_sigma_traity"]]

## summary(mdl.traitonly, pars = c("mu_grand", "sigma_sp", "sigma_study", "sigma_traity"))$summary
## t(param)


#Faith testing to see how well the model estimates simulated parameters
#---------------------------------------------------------------------------


postSLA <- extract(mdl.traitphen)

#plot main effects of cues
postMeanSLAdf <- data.frame(postSLA)
names(postMeanSLAdf)


      #Compare results to simulated values
      pdf("figures/simPosteriorHist_fullModel.pdf")

      par(mfrow=c(2,2))

      hist(postMeanSLAdf$mu_grand, main = paste("trait_mu_grand is " , signif(param$trait_mu_grand,3), sep = ""))
      abline(v = param$trait_mu_grand, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigma_traity, main = paste("sigma_traity is " , signif(param$trait_sigma_traity,3), sep = ""))
      abline(v = param$trait_sigma_traity, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigma_sp, main = paste("sigma_sp is " , signif(param$trait_sigma_sp,3), sep = ""))
      abline(v = param$trait_sigma_sp, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigma_study, main = paste("sigma_sp is " , signif(param$trait_sigma_study,3), sep = ""))
      abline(v = param$trait_sigma_study, col="red", lwd=3, lty=2)


      par(mfrow=c(3,4))

      hist(postMeanSLAdf$muPhenoSp, main = paste("muPhenoSp is " , signif(param$phenology_muAlpha,3), sep = ""))
      abline(v = param$phenology_muAlpha, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muForceSp, main = paste("muForceSp is " , signif(param$phenology_muForce,3), sep = ""))
      abline(v = param$phenology_muForce, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muChillSp, main = paste("muChillSp is " , signif(param$phenology_sigmaChill,3), sep = ""))
      abline(v = param$phenology_sigmaChill, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muPhotoSp, main = paste("muPhotoSp is " , signif(param$phenology_muPhoto,3), sep = ""))
      abline(v = param$phenology_muPhoto, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmapheno_y, main = paste("sigmapheno_y is " , signif(param$phenology_sigmaPheno,3), sep = ""))
      abline(v = param$phenology_sigmaPheno, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxForce, main = paste("betaTraitxForce is " , signif(param$phenology_betaTraitForce,3), sep = ""))
      abline(v = param$phenology_betaTraitForce, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxChill, main = paste("betaTraitxChill is " , signif(param$phenology_betaTraitChill,3), sep = ""))
      abline(v = param$phenology_betaTraitChill, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxPhoto, main = paste("betaTraitxPhoto is " , signif(param$phenology_betaTraitPhoto,3), sep = ""))
      abline(v = param$phenology_betaTraitPhoto, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaChillSp, main = paste("sigmaChillSp is " , signif(param$phenology_sigmaChill,3), sep = ""))
      abline(v = param$phenology_sigmaChill, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaForceSp, main = paste("sigmaForceSp is " , signif(param$phenology_sigmaForce,3), sep = ""))
      abline(v = param$phenology_sigmaForce, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaPhotoSp, main = paste("sigmaPhotoSp is " , signif(param$phenology_sigmaPhoto,3), sep = ""))
      abline(v = param$phenology_sigmaPhoto, col="red", lwd=3, lty=2) 
      dev.off()

    par(mfrow=c(1,1))






    #Try with much wider Priors 
    #--------------------------------




## Prepare all data for Stan
all_data2 <- list(yTraiti = yTable$yTraiti,
                   N = nrow(yTable),
                   n_spec = param[["Nspp"]], 
                   trait_species = yTable$Species,
                   n_study = param[["Nstudy"]],
                   study = yTable$Study,
                   prior_mu_grand_mu = param[["trait_mu_grand"]],
                   prior_mu_grand_sigma = 5,
                   prior_sigma_sp_mu = param[["trait_sigma_sp"]],
                   prior_sigma_sp_sigma = 5,
                   prior_sigma_study_mu = param[["trait_sigma_study"]],
                   prior_sigma_study_sigma = 5,
                   prior_sigma_traity_mu = param[["trait_sigma_traity"]],
                   prior_sigma_traity_sigma = 5,
                   ## Phenology
                   phenology_species = yTable$Species,
                   Nph = nrow(yTable),
                   yPhenoi = yTable$yPhenologyi,
                   forcei = yTable$forcei,
                   chilli = yTable$chilli,
                   photoi = yTable$photoi,
                   prior_muForceSp_mu = 0, # mean of the prior distribution of the mean effect of forcing 
                   prior_muForceSp_sigma = 1, # vareince of the prior distributionof the mean effect of forcing 
                   prior_muChillSp_mu = 0,# mean of the prior distribution of the mean effect of chilling 
                   prior_muChillSp_sigma = 1,# varience of the prior distribution of the mean effect of chilling
                   prior_muPhotoSp_mu = 0,
                   prior_muPhotoSp_sigma = 1,
                   prior_muPhenoSp_mu = 80,
                   prior_muPhenoSp_sigma = 20,
                   prior_sigmaForceSp_mu = 4,
                   prior_sigmaForceSp_sigma = 3,
                   prior_sigmaChillSp_mu = 4,
                   prior_sigmaChillSp_sigma = 3,
                   prior_sigmaPhotoSp_mu = 4,
                   prior_sigmaPhotoSp_sigma = 3,
                   prior_sigmaPhenoSp_mu = 0,
                   prior_sigmaPhenoSp_sigma = 10,
                   prior_betaTraitxForce_mu = 0,
                   prior_betaTraitxForce_sigma = 0.5,
                   prior_betaTraitxChill_mu = 0,
                   prior_betaTraitxChill_sigma = 0.5,
                   prior_betaTraitxPhoto_mu = 0,
                   prior_betaTraitxPhoto_sigma = 0.5,
                   prior_sigmaphenoy_mu =20,  #mean of prior distribution of the general error (sigma_y) around the mean predicted value
                   prior_sigmaphenoy_sigma = 5 # variance of the prior distribution of the general error sigma)y around the mean predicted value
                   ) 


                   
                   

mdl.traitphen2 <- stan("stan/phenology_combined.stan",
                      data = all_data2,
                      iter = 2000,
                      warmup = 1000,
                      chains = 4,
                      include = FALSE, pars = c("y_hat"),
                      seed = 202109)


#Faith testing to see how well the model estimates simulated parameters
#---------------------------------------------------------------------------


postSLA2 <- extract(mdl.traitphen2)

#plot main effects of cues
postMeanSLAdf <- data.frame(postSLA2)
names(postMeanSLAdf)


      #Compare results to simulated values
      pdf("figures/simPosteriorHist_fullModel_widePriors.pdf")

      par(mfrow=c(2,2))

      hist(postMeanSLAdf$mu_grand, main = paste("trait_mu_grand is " , signif(param$trait_mu_grand,3), sep = ""))
      abline(v = param$trait_mu_grand, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigma_traity, main = paste("sigma_traity is " , signif(param$trait_sigma_traity,3), sep = ""))
      abline(v = param$trait_sigma_traity, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigma_sp, main = paste("sigma_sp is " , signif(param$trait_sigma_sp,3), sep = ""))
      abline(v = param$trait_sigma_sp, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigma_study, main = paste("sigma_sp is " , signif(param$trait_sigma_study,3), sep = ""))
      abline(v = param$trait_sigma_study, col="red", lwd=3, lty=2)


      par(mfrow=c(3,4))

      hist(postMeanSLAdf$muPhenoSp, main = paste("muPhenoSp is " , signif(param$phenology_muAlpha,3), sep = ""))
      abline(v = param$phenology_muAlpha, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muForceSp, main = paste("muForceSp is " , signif(param$phenology_muForce,3), sep = ""))
      abline(v = param$phenology_muForce, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muChillSp, main = paste("muChillSp is " , signif(param$phenology_sigmaChill,3), sep = ""))
      abline(v = param$phenology_sigmaChill, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$muPhotoSp, main = paste("muPhotoSp is " , signif(param$phenology_muPhoto,3), sep = ""))
      abline(v = param$phenology_muPhoto, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmapheno_y, main = paste("sigmapheno_y is " , signif(param$phenology_sigmaPheno,3), sep = ""))
      abline(v = param$phenology_sigmaPheno, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxForce, main = paste("betaTraitxForce is " , signif(param$phenology_betaTraitForce,3), sep = ""))
      abline(v = param$phenology_betaTraitForce, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxChill, main = paste("betaTraitxChill is " , signif(param$phenology_betaTraitChill,3), sep = ""))
      abline(v = param$phenology_betaTraitChill, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$betaTraitxPhoto, main = paste("betaTraitxPhoto is " , signif(param$phenology_betaTraitPhoto,3), sep = ""))
      abline(v = param$phenology_betaTraitPhoto, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaChillSp, main = paste("sigmaChillSp is " , signif(param$phenology_sigmaChill,3), sep = ""))
      abline(v = param$phenology_sigmaChill, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaForceSp, main = paste("sigmaForceSp is " , signif(param$phenology_sigmaForce,3), sep = ""))
      abline(v = param$phenology_sigmaForce, col="red", lwd=3, lty=2)

      hist(postMeanSLAdf$sigmaPhotoSp, main = paste("sigmaPhotoSp is " , signif(param$phenology_sigmaPhoto,3), sep = ""))
      abline(v = param$phenology_sigmaPhoto, col="red", lwd=3, lty=2) 
      dev.off()

    par(mfrow=c(1,1))






