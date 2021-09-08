
library("rstan")
library("shinystan")

options(mc.cores = 4)

nsp <- 20
nreps <- 100

yTrait <- rnorm(n = nsp, mean = 0, sd = 2)

param <- list(
    muForce = 2,
    muChill = -2.1,
    muPhoto = 0.45,
    sigmaForce = .5,
    sigmaChill = .6,
    sigmaPhoto = .7,
    betaTraitForce = .34,
    betaTraitChill = .25,
    betaTraitPhoto = -.5,
    muAlpha = 10,
    sigmaAlpha = 1,
    sigmaPheno = 2
)

alphaPheno <- rnorm(n = nsp, mean = param[["muAlpha"]], sd = param[["sigmaAlpha"]])
alphaForce <- rnorm(n = nsp, mean = param[["muForce"]], sd = param[["sigmaForce"]])
alphaChill <- rnorm(n = nsp, mean = param[["muChill"]], sd = param[["sigmaChill"]])
alphaPhoto <- rnorm(n = nsp, mean = param[["muPhoto"]], sd = param[["sigmaPhoto"]])

yPhenotable <- data.frame(Species = c(),
                          Replicate = c(),
                          alphaPheno = c(),
                          trait = c(),
                          betaForce = c(),
                          betaChill = c(),
                          betaPhoto = c())
for(i in 1:nsp){
    temp <- data.frame(Species = i,
                       Replicate = 1:nreps,
                       alphaPheno = alphaPheno[i],
                       trait = yTrait[i],
                       betaForce = alphaForce[i] + param[["betaTraitForce"]] * yTrait[i],
                       betaChill = alphaChill[i] + param[["betaTraitChill"]] * yTrait[i],
                       betaPhoto = alphaPhoto[i] + param[["betaTraitPhoto"]] * yTrait[i])
    yPhenotable <- rbind(yPhenotable, temp)
}

head(yPhenotable)
tail(yPhenotable)

forcei <- rnorm(n = nsp * nreps, mean = 0, sd = .5)
chilli <- rnorm(n = nsp * nreps, mean = 0, sd = .5)
photoi <- rnorm(n = nsp * nreps, mean = 0, sd = .5)


yPhenotable$ypheno <- rnorm(n = nrow(yPhenotable),
                            mean = yPhenotable$alphaPheno + yPhenotable$betaForce * forcei + yPhenotable$betaChill * chilli + yPhenotable$betaPhoto * photoi,
                            sd = param[["sigmaPheno"]])

head(yPhenotable)
tail(yPhenotable)

data.stan <- list(n_spec = nsp,
                  species = yPhenotable$Species,
                  yTrait = yPhenotable$trait,
                  Nph = nrow(yPhenotable),
                  yPhenoi = yPhenotable$ypheno,
                  forcei = forcei,
                  chilli = chilli,
                  photoi = photoi,
                  prior_muForceSp_mu = 2,
                  prior_muForceSp_sigma = .5,
                  prior_muChillSp_mu = -2.1,
                  prior_muChillSp_sigma = .5,
                  prior_muPhotoSp_mu = .45,
                  prior_muPhotoSp_sigma = .1,
                  prior_muPhenoSp_mu = 10,
                  prior_muPhenoSp_sigma = 2,
                  prior_sigmaForceSp_mu = 0.5,
                  prior_sigmaForceSp_sigma = 0.1,
                  prior_sigmaChillSp_mu = 0.6,
                  prior_sigmaChillSp_sigma = 0.05,
                  prior_sigmaPhotoSp_mu = 0.7,
                  prior_sigmaPhotoSp_sigma = 0.1,
                  prior_sigmaPhenoSp_mu = 1,
                  prior_sigmaPhenoSp_sigma = .1,
                  prior_betaTraitxForce_mu = 0.34,
                  prior_betaTraitxForce_sigma = 0.1,
                  prior_betaTraitxChill_mu = 0.25,
                  prior_betaTraitxChill_sigma = 0.1,
                  prior_betaTraitxPhoto_mu = -0.5,
                  prior_betaTraitxPhoto_sigma = .1,
                  prior_sigmaphenoy_mu = 2,
                  prior_sigmaphenoy_sigma = 0.5
                  )
### Call Stan
fit.stan1 <- stan("stan/3cue_traitfixed.stan",
                 data = data.stan,
                 iter = 2000,
                 warmup = 1000,
                 chains = 4,
                 seed = 20200602)

options(browser = "chromium")
launch_shinystan(fit.stan1)

## Summarize posterior of continuous parameters
summary(fit.stan1, pars = c("muForceSp", "muChillSp", "muPhotoSp", "sigmaForceSp", "sigmaChillSp", "sigmaPhotoSp", "betaTraitxForce", "betaTraitxChill", "betaTraitxPhoto", "muPhenoSp", "sigmaPhenoSp", "sigmapheno_y"))$summary[, "mean"]
t(param)

summary(fit.stan1, pars = c("alphaPhenoSp"))$summary[, "mean"]

summary(fit.stan1, pars = c("muForceSp"))$summary
t(param[["muForce"]])
summary(fit.stan1, pars = c("muChillSp"))$summary
t(param[["muChill"]])
summary(fit.stan1, pars = c("muPhotoSp"))$summary
t(param[["muPhoto"]])

summary(fit.stan1, pars = c("sigmaForceSp"))$summary
t(param[["sigmaForce"]])
summary(fit.stan1, pars = c("sigmaChillSp"))$summary
t(param[["sigmaChill"]])
summary(fit.stan1, pars = c("sigmaPhotoSp"))$summary
t(param[["sigmaPhoto"]])

