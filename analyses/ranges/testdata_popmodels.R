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

if(FALSE){
#--------------------------------------
# Set up the range model first - we can change to whatever cue or cues we choose to use in the future
# resp ~ force[sp] + force[pop] + sigma_y 
# force[sp] and force[study] are your standard hierarhical parameters to try and compare inter- vs intraspecific variation in the forcing cue

nsp <- 10 # number of species
npop <- 8
nsppop <- nsp * npop# obervations per species for phenological event and photoperiod


# Parameters for pheno
beta_force <- -15

sigma_bforce <- 10
sigma_bforcepop <- 5
sigma_y <- 10

### This will be a fixed effects model but I think we need some mua_sp to create some variation around our species estimates
## And now let's add a greater sigma since our data is centered
sigma_asp <- 5
mua_sp <- 0

sigma_apop <- 5
mua_pop <- 0

a_sp <- rnorm(nsppop, mua_sp, sigma_asp)
a_pop <- rnorm(nsppop, a_sp, sigma_apop)

sppops <- data.frame(sp = rep(1:nsp, 8), pop = rep(1:npop, each=nsp),
                     a_pop = a_pop, a_sp = a_sp)

b_force <- rnorm(nsppop, beta_force, sigma_bforce)
b_force_pop <- rnorm(nsppop, b_force, sigma_bforcepop)

sppops$b_force <- b_force
sppops$b_force_pop <- b_force_pop

Fmean <- 5
Fsigma <- 2

simpheno <- data.frame(sp=numeric(), mua_pop=numeric(), pop=numeric(), b_force=numeric(), b_force_pop=numeric(),
                       F=numeric())

for (sp in 1:nsp){
  Fhere <- rnorm(nsp, Fmean, Fsigma)
  simphenoadd <- data.frame(sp=rep(sp, nsppop), mua_pop=a_pop, pop=rep(1:npop, nsp),
                            b_force=b_force, b_force_pop=b_force_pop, F=Fhere)
  simpheno <- rbind(simpheno, simphenoadd)
}

simpheno$resp <- simpheno$mua_pop + simpheno$F*b_force_pop + rnorm(nrow(simpheno), 0, sigma_y)

table(simpheno$sp)

library(lme4)
lmer(resp ~ F + (F|sp) + (F|pop), data=simpheno)
}

### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 10 # number of species
npop = 8 # number of populations
nsppop = nsp*npop # numbers of species per population 

sample_a <- list(force.env = rnorm(1000, 5, 2))

model.parameters <- list(intercept = 30,
                         force.coef = -10)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsppop, replace = TRUE)})
mm <- model.matrix(~env.samples)

#  4) We need to make a random intercept model for each species
parameters.sp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsppop, byrow = TRUE)
parameters.pop <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsppop, byrow = TRUE)

# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "force.coef"), collapse = "|"), x = names(model.parameters))

# Generate random parameters (by species)
parameters.sp[, 1] <- sapply(1:npop, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 20), nsp)})
parameters.sp[, 2] <- sapply(1:npop, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 10), nsp)})


### Now add the population level
spp.parameters.int <- unique(parameters.sp[,1])
spp.parameters.force <- unique(parameters.sp[,2])
parameters.sp[, 1] <- sapply(1:npop, FUN=function(x){
  rnorm(n = nsp, mean = spp.parameters.int[x], sd = 10)})
parameters.sp[, 2] <- sapply(1:npop, FUN = function(x){ 
  rnorm(n = nsp, mean = spp.parameters.force[x], sd = 5)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.sp[x, ], sd = 10)})

simpheno <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, npop))),
                                       pop = as.vector(rep(as.vector(1:npop), times=nsp)),
                                       resp = response, force = env.samples[,1]))

write.csv(simpheno, file="output/testdata_popmodels.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(resp ~ force + (force|species/pop), data=simpheno) ## Quick look looks good!


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
                    chains = 4,  control=list(max_treedepth = 12)) 


modelhere <- forcepopfit 
mod.sum <- summary(modelhere)$summary
mod.sum[grep("b_force", rownames(mod.sum)),]
mod.sum[grep("sigma", rownames(mod.sum)),] 

#save(jointfit, file="output/stan/jointlatphoto.Rda")

### Let's just look at the data a bit to make sure it looks okay...
library(ggplot2)
library(egg)

spp <- ggplot(simpheno, aes(y=resp, x=F, col=as.factor(sp), group=as.factor(sp))) + 
  geom_point() + geom_smooth(method="lm") + theme_classic()

pop <- ggplot(simpheno, aes(y=resp, x=F, col=as.factor(pop), group=as.factor(pop))) + 
  geom_point() + geom_smooth(method="lm") + theme_classic()

ggarrange(spp, pop)


anova(lm(resp~F + sp + pop, data=simpheno))

library(rstanarm)
modpop3.force <- stan_lmer(formula = resp ~ F + ( F |sp/pop), 
                           data = simpheno,iter=1000,warmup=500,chains=1, prior = normal(0,50),
                           prior_intercept = normal(0,50) )

rstan::get_stanmodel(modpop3.force$stanfit)

library(brms)
modpop3.force <- brm(formula = resp ~ force + ( force |species/pop), 
                           data = simpheno,iter=4000,warmup=2500,chains=1, 
                     control=list(adapt_delta = 0.8))

save(modpop3.force, file="~/Desktop/m3l_ni_brms_testdata.Rdata")

library(broom)
tidy(modpop3.force)
