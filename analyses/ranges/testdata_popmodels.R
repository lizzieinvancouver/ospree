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

nsp <- 10 # number of species
npop <- 8
nsppop <- nsp * npop# obervations per species for phenological event and photoperiod


# Parameters for pheno
beta_force <- -10

sigma_bforce <- 5
sigma_bforcepop <- 10
sigma_y <- 10

### This will be a fixed effects model but I think we need some mua_sp to create some variation around our species estimates
## And now let's add a greater sigma since our data is centered
sigma_asp <- 10
mua_sp <- 0

sigma_apop <- 5
mua_pop <- 0

a_sp <- rnorm(nsp, mua_sp, sigma_asp)
a_pop <- rnorm(nsppop, a_sp, sigma_apop)

b_force <- rnorm(nsp, beta_force, sigma_bforce)

Fmean <- 5
Fsigma <- 2

simpheno <- data.frame(sp=numeric(), mua_pop=numeric(), pop=numeric(), a_force=numeric(), F=numeric())

for (sp in 1:nsp){
  Fhere <- rnorm(nsp, Fmean, Fsigma)
  simphenoadd <- data.frame(sp=rep(sp, nsppop), mua_pop=a_pop, pop=rep(1:npop, nsp),
                            b_force=rep(b_force[sp], nsppop), F=Fhere)
  simpheno <- rbind(simpheno, simphenoadd)
}

b_force_pop <- rnorm(nrow(simpheno), simpheno$b_force, sigma_bforcepop)
simpheno$resp <- simpheno$mua_pop + simpheno$F*b_force_pop + rnorm(nrow(simpheno), 0, sigma_y)

table(simpheno$sp)

library(lme4)
lmer(resp ~ F + (F|sp) + (F|pop), data=simpheno)

N <- length(simpheno$resp)
forcepop <- list(y = simpheno$resp,
                 N = N, 
                 n_sp = nsp, 
                 n_pop = npop,
                 sp = simpheno$sp,
                 pop = simpheno$pop,
                 N = N, 
                 force = simpheno$F)



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
