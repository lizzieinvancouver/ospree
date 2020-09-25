## Started 18 September 2020 ##
## By Cat, help from many ##
# Let's make some test data for our rstan pop models


library(rstan)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

set.seed(12221)


# setwd
# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses/ranges") 
} else 
  setwd("~/Documents/git/ospree/analyses/ranges")

#--------------------------------------
# Set up the range model first - we can change to whatever cue or cues we choose to use in the future
# resp ~ force[sp] + force[pop] + sigma_y 
# force[sp] and force[study] are your standard hierarhical parameters to try and compare inter- vs intraspecific variation in the forcing cue


#### New attempt at building fake data: based on https://rpubs.com/kaz_yos/stan-multi-2
## Data generation
set.seed(12221)

nsp = 10
npop = 8
nobs = 10
ntot = nsp * npop * nobs

## Budburst intercept
intercept   <- 50
## Forcing slope
b_force    <- -10
sigma_y <- 10

## Level-1 errors
sigma_a   <- 10
e_int <- rnorm(n = ntot, mean = 0, sd = sigma_a)

## Level-2 errors (species)
sigma_sp <- 15
e_sp <- rnorm(n = npop * nsp, mean = 0, sd = sigma_sp)
sigma_bsp <- 10
e_bsp <- rnorm(n = npop * nsp, mean = 0, sd = sigma_bsp)

## Level-3 errors (population)
sigma_pop <- 5
e_pop <- rnorm(n = npop, mean = 0, sd = sigma_pop)
sigma_bpop <- 2
e_bpop <- rnorm(n = npop, mean = 0, sd = sigma_bpop)

## Varying intercepts
## Level 3 (repeat for level 2)
a_sp   <- rep(intercept + e_pop, nsp)
## Level 2 (repeat for level 1)
a_sppop  <- rep(a_sp + e_sp, nsp)

## Varying slopes
## Level 3 (repeat for level 2)
b_forcesp   <- rep(b_force + e_bpop, nsp)
## Level 2 (repeat for level 1)
b_forcesppop  <- rep(b_forcesp + e_bsp, nsp)

## Predictor
force   <- rnorm(n = ntot, 5, 2)

## Outcome
resp    <- a_sppop + b_forcesppop*force + rnorm(ntot, 0, sigma_y)

simpheno <- data.frame(species=rep(1:nsp, each=nobs), pop=rep(1:npop, each=nobs*nsp),
                          force=force, resp=resp)

library(lme4)
modtest <- lmer(resp ~ force + (force|species/pop), data=simpheno) ## Quick look looks good!

write.csv(simpheno, file="~/Desktop/testing123.csv", row.names=FALSE)


N <- length(simpheno$resp)
forcepop <- list(y = simpheno$resp,
                 N = N, 
                 n_sp = nsp, 
                 n_pop = npop,
                 sp = simpheno$species,
                 pop = simpheno$pop,
                 N = N, 
                 force = simpheno$force)



# Try to run the Stan model 
forcepopfit <- stan(file = "stan/nointer_3levelwpop.stan", data = forcepop, warmup = 2000, iter = 4000,
                    chains = 4,  control=list(max_treedepth = 12, adapt_delta=0.95)) 


modelhere <- forcepopfit 
mod.sum <- summary(modelhere)$summary
mod.sum[grep("b_force", rownames(mod.sum)),]
mod.sum[grep("sigma", rownames(mod.sum)),] 

#save(jointfit, file="output/stan/jointlatphoto.Rda")

### Let's just look at the data a bit to make sure it looks okay...
library(ggplot2)
library(egg)

spp <- ggplot(simpheno, aes(y=resp, x=force, col=as.factor(species), group=as.factor(species))) + 
  geom_point() + geom_smooth(method="lm") + theme_classic()

pop <- ggplot(simpheno, aes(y=resp, x=force, col=as.factor(pop), group=as.factor(pop))) + 
  geom_point() + geom_smooth(method="lm") + theme_classic()

quartz()
ggarrange(spp, pop)


anova(lm(resp~force + species + pop, data=simpheno))

real <- bb.stan.here

library(brms)
modpop3.force.real <- brm(formula = resp ~ force + ( force |latbi/pophere), 
                           data = real,iter=1000,warmup=500,chains=4, 
                     control=list(adapt_delta = 0.95, max_treedepth=12))

save(modpop3.force.real, file="~/Desktop/m3l_ni_brms_realdata.Rdata")

