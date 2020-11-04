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

nsp = 12
npop = 10
nobs = 14
ntot = nsp * npop * nobs

## Budburst intercept
intercept   <- 50
sigma_y <- 15
## Forcing slope
b_force    <- -5
b_photo    <- -2


## Level-1 errors
#sigma_a   <- 3

## Level-2 errors (population)
sigma_apop <- 10
sigma_bpop <- 5

## Level-3 errors (species)
sigma_asp <- 15
sigma_bsp <- 10

## Varying intercepts
## Level 3 (repeat for level 2)
a_sp  <- intercept + rnorm(nsp, 0, sigma_asp) ### maybe this should be n = nsp*nobs or something different
## Level 2 
a_sppop <- a_sp + rnorm(npop*nsp, 0, sigma_apop)
a_sppop <- rep(a_sppop, each=nobs)

## Varying slopes
## Level 3 (repeat for level 2)
b_force_sp  <- b_force + rnorm(nsp, 0, sigma_bsp) ### maybe this should be n = nsp*nobs or something different
## Level 2 
b_force_sppop <- b_force_sp + rnorm(npop*nsp, 0, sigma_bpop)
b_force_sppop <- rep(b_force_sppop, each=nobs)

b_photo_sp  <- b_photo + rnorm(nsp, 0, sigma_bsp) ### maybe this should be n = nsp*nobs or something different
## Level 2 
b_photo_sppop <- b_photo_sp + rnorm(npop*nsp, 0, sigma_bpop)
b_photo_sppop <- rep(b_photo_sppop, each=nobs)

## Predictor
force   <- rnorm(n = ntot, 5, 2)
photo   <- rnorm(n = ntot, 3, 2)

## Outcome
resp <- a_sppop + b_force_sppop*force + b_photo_sppop*photo + rnorm(ntot, 0, sigma_y)
simpheno <- data.frame(species=rep(1:nsp, each=nobs), pop=rep(1:npop, each=nobs*nsp),
                       force=force, a_sppop=a_sppop, b_force_sppop=b_force_sppop,
                       photo=photo, b_photo_sppop=b_photo_sppop,resp=resp)


N <- length(simpheno$resp)
forcepop <- list(y = simpheno$resp,
                 N = N, 
                 n_sp = nsp, 
                 n_pop = npop,
                 sp = simpheno$species,
                 pop = simpheno$pop,
                 N = N, 
                 force = simpheno$force,
                 photo = simpheno$photo)



#forcepopfit <- stan(file = "stan/nointer_3levelwpop_classroomexamp.stan", data = forcepop, warmup = 4000, iter = 5000,
 #                   chains = 4,  control=list(max_treedepth = 15, adapt_delta=0.99)) 

forcephotopopfit <- stan(file = "stan/nointer_3levelwpop_force&photo.stan", data = forcepop, warmup = 4000, iter = 5000,
                    chains = 4,  control=list(max_treedepth = 15, adapt_delta=0.99)) 


modelhere <- forcepopfit 
mod.sum <- summary(modelhere)$summary
mod.sum[grep("b_force", rownames(mod.sum)),]
mod.sum[grep("sigma", rownames(mod.sum)),] 

launch_shinystan(forcepopfit)

#save(forcepopfit, file="~/Desktop/forcepopfit_sims.Rda")


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

