
library(rstan)

options(mc.cores = 4)

set.seed(202199)

Nrep <- 100 # rep per trait (not all will be used)
Nstudy <- 13 # number of studies w/ traits (10 seems a little low for early simulation code; remember that you are estimating a distribution of this the same as for species)
Nspp <- 13 # number of species with traits (making this 20 just for speed for now)

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
mu.grand <- 1.3 # the grand mean of the height model
sigma.species <- 2 # we want to keep the variaiton across spp. high

#the alphaTraitSp in Faiths original code:
mu.trtsp <- rnorm(Nspp, 0, sigma.species)
trt.dat$mu.trtsp <- rep(mu.trtsp, Nstudy) #adding ht data for ea. sp

#now generating the effects of study
sigma.study <- 3
mu.study <- rnorm(Nstudy, 0, sigma.study) #intercept for each study
trt.dat$mu.study <- rep(mu.study, each = Nspp) # generate data for ea study

# general variance
trt.var <- 1 #sigmaTrait_y in the stan code
trt.dat$trt.er <- rnorm(Ntrt, 0, trt.var)

# generate yhat - heights -  for this first trt model
trt.dat$yTraiti <- mu.grand + trt.dat$mu.trtsp + trt.dat$mu.study + trt.dat$trt.er

## balanced subset
trt.dat1 <- subset(trt.dat, trt.dat$rep <= 50)
nrow(trt.dat1)

## Trait only stan model
trait_data <- list(y = trt.dat1$yTraiti, 
                   N = nrow(trt.dat1), 
                   n_variety = Nspp, 
                   variety = trt.dat1$species, 
                   company = trt.dat1$study, 
                   n_company = Nstudy,
                   prior_a_grand_mu = 1.3,
                   prior_a_grand_sigma = 0.3,
                   prior_sigma_a_variety_mu = 2,
                   prior_sigma_a_variety_sigma = .75,
                   prior_sigma_a_company_mu = 3,
                   prior_sigma_a_company_sigma = 1.5,
                   prior_sigma_y_mu = 1,
                   prior_sigma_y_sigma = .25) 

mdl.balanced <- stan("stan/int_variety_company.stan",
                     data = trait_data,
                     iter = 3000,
                     warmup = 2000,
                     chains = 4,
                     include = FALSE,
                     pars = "mu_y",
                     seed = 202199)## ,
                      ## control = list(max_treedepth = 15))

## ## break balancing
trt.dat2 <- NULL
for(i in 1:Nstudy){
    temp <- subset(trt.dat, trt.dat$study == i)
    if(i == 1){
        temp2 <- subset(temp, temp$rep <= 40)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
    if(i == 2){
        temp2 <- subset(temp, temp$rep <= 60)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
    if(i == 3){
        temp2 <- subset(temp, temp$rep <= 10)
        trt.dat2 <- rbind(trt.dat2, temp2)
     }
    if(i == 4){
        temp2 <- subset(temp, temp$rep <= 90)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }    
    if(i == 5){
        temp2 <- subset(temp, temp$rep <= 55)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
    if(i == 6){
        temp2 <- subset(temp, temp$rep <= 45)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
    if(i == 7){
        temp2 <- subset(temp, temp$rep <= 22)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
    if(i == 8){
        temp2 <- subset(temp, temp$rep <= 78)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
    if(i == 9){
        temp2 <- subset(temp, temp$rep <= 6)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
    if(i == 10){
        temp2 <- subset(temp, temp$rep <= 94)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
    if(i > 10){
        temp2 <- subset(temp, temp$rep <= 50)
        trt.dat2 <- rbind(trt.dat2, temp2)
    }
}
nrow(trt.dat2)

aggregate(rep ~ species, data = trt.dat2, FUN = length)
aggregate(rep ~ study, data = trt.dat2, FUN = length)
aggregate(rep ~ study * species, data = trt.dat2, FUN = length)

## Trait only stan model ###########################################################
trait_data <- list(y = trt.dat2$yTraiti, 
                   N = nrow(trt.dat2), 
                   n_variety = Nspp, 
                   variety = trt.dat2$species, 
                   company = trt.dat2$study, 
                   n_company = Nstudy,
                   prior_a_grand_mu = 1.3,
                   prior_a_grand_sigma = 0.3,
                   prior_sigma_a_variety_mu = 2,
                   prior_sigma_a_variety_sigma = .75,
                   prior_sigma_a_company_mu = 3,
                   prior_sigma_a_company_sigma = 1.5,
                   prior_sigma_y_mu = 1,
                   prior_sigma_y_sigma = .25) 

mdl.unbalanced <- stan("stan/int_variety_company.stan",
                      data = trait_data,
                      iter = 3000,
                      warmup = 2000,
                      chains = 4,
                      include = FALSE,
                      pars = "mu_y",
                     seed = 202199)## ,
                      ## control = list(max_treedepth = 15))

summary(mdl.balanced, pars = c("a_grand", "sigma_a_company", "sigma_a_variety", "sigma_y"))$summary
summary(mdl.unbalanced, pars = c("a_grand", "sigma_a_company", "sigma_a_variety", "sigma_y"))$summary

summary(mdl.balanced, pars = c("a_variety", "a_company"))$summary
summary(mdl.unbalanced, pars = c("a_variety", "a_company"))$summary

head(summary(mdl.balanced, pars = c("a_variety"))$summary)
head(summary(mdl.unbalanced, pars = c("a_variety"))$summary)

head(summary(mdl.balanced, pars = c("a_company"))$summary)
head(summary(mdl.unbalanced, pars = c("a_company"))$summary)


