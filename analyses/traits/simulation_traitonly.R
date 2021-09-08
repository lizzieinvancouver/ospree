
library(rstan)
## require(rstanarm)
require(shinystan)
## require(bayesplot)
## require(truncnorm)
## library(ggplot2)

# 50 study, spp, reps, drop sigmatrait_y to 1
# boxplots of all the dots by species and study
#compare sp to study and make sure no correlations
# run test data through rstan arm - how fast? accurate? 
options(mc.cores = 4)

Nrep <- 50 # rep per trait
Nstudy <- 12 # number of studies w/ traits (10 seems a little low for early simulation code; remember that you are estimating a distribution of this the same as for species)
Nspp <- 12 # number of species with traits (making this 20 just for speed for now)

# First making a data frame for the test trait data
Ntrt <- Nspp * Nstudy * Nrep # total number of traits observations
Ntrt

#make a dataframe for height
trt.dat <- data.frame(matrix(NA, Ntrt, 1))
names(trt.dat) <- c("rep")
trt.dat$rep <- c(1:Nrep)
trt.dat$study <- rep(c(1:Nstudy), each = Nspp)
trt.dat$species <- rep(1:Nspp, Nstudy)

# now generating the species trait data, here it is for height
mu.grand <- 10 # the grand mean of the height model
sigma.species <- 3 # we want to keep the variaiton across spp. high

#the alphaTraitSp in Faiths original code:
mu.trtsp <- rnorm(Nspp, 0, sigma.species)
trt.dat$mu.trtsp <- rep(mu.trtsp, Nstudy) #adding ht data for ea. sp

#now generating the effects of study
sigma.study <- 5
mu.study <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$mu.study <- rep(mu.study, each = Nspp) # generate data for ea study

# general variance
trt.var <- 1 #sigmaTrait_y in the stan code
trt.dat$trt.er <- rnorm(Ntrt, 0, trt.var)

# generate yhat - heights -  for this first trt model
trt.dat$yTraiti <- mu.grand + trt.dat$mu.trtsp + trt.dat$mu.study + trt.dat$trt.er

## Trait only stan model ###########################################################
trait_data <- list(y = trt.dat$yTraiti, 
                   N = Ntrt, 
                   n_variety = Nspp, 
                   variety = trt.dat$species, 
                   company = trt.dat$study, 
                   n_company = Nstudy,
                   prior_a_grand_mu = 10,
                   prior_a_grand_sigma = 3,
                   prior_sigma_a_variety_mu = 3,
                   prior_sigma_a_variety_sigma = 1,
                   prior_sigma_a_company_mu = 5,
                   prior_sigma_a_company_sigma = 2,
                   prior_sigma_y_mu = 1,
                   prior_sigma_y_sigma = .3) 

mdl.traitonly <- stan("stan/int_variety_company.stan",
                      data = trait_data,
                      iter = 3000,
                      warmup = 2000,
                      chains = 4,
                      include = FALSE,
                      pars = "mu_y")

summary(mdl.traitonly, pars = c("a_grand", "sigma_a_company", "sigma_a_variety", "sigma_y"))$summary
