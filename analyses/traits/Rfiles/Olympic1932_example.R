# September 9, 2021
# 1932 Olympics figure skating example 

# Data sent by Dr.Gelman
# Dataset has pairs and judges, with two y-variables, program and performance
# In this example I will only work on program 


if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/ospree/analyses/traits/")
} else{
  setwd("/home/deirdre/ospree")
}

library(rstan)
require(rstanarm)
require(shinystan)
require(bayesplot)
require(truncnorm)
library(ggplot2)
library(tidybayes)

rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

dat <- read.csv("input/OlympicGames_1932.csv")
head(dat)

prog <- subset(dat, criterion == "Program")

prog_data <- list(y = prog$score, 
                   N = dim(prog)[1], 
                   pair = prog$pair, 
                   judge = prog$judge, 
                   n_pair = length(unique(prog$pair)),
                   n_judge = length(unique(prog$judge))) 

mdl<- stan('stan/OlympicGames_1932mdl.stan',
                      data = prog_data)

mdl.ncp<- stan('stan/OlympicGames_1932mdl-non-centered.stan',
           data = prog_data,
           control = list(adapt_delta = 0.99))

ssm <-  as.shinystan(mdl.ncp)
launch_shinystan(ssm)

sum <- summary(mdl)$summary
post <- rstan::extract(mdl)

range(sum[, "n_eff"]) 
range(sum[, "Rhat"])

h1 <- hist(rnorm(1000, 1,10))
h2 <- hist(post$sigma_y)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

# ppc and trying to figure out what is going on! 
y<- as.numeric(prog$score)
yrep<-post$y_rep

ppc_dens_overlay(y, yrep[1:1000, ]) # hmm the yrep does not appear
plot(density(yrep))
plot(density(y))

pairs(mdl, pars = c("mu", "sigma_gamma", "sigma_delta", "sigma_y", "lp__"))
str(mdl)

##################################################
# trying the model runs with rstanarm

require(rstanarm)
mdl.arm <- stan_lmer(score ~ 1 + (1 | judge) + (1 |pair), data = prog)

ssm <-  as.shinystan(mdl.arm)
launch_shinystan(ssm)

prior_summary(object = mdl.arm)
summary(mdl.arm)
mdl.arm

y<- as.numeric(prog$score)
yrep <- posterior_predict(mdl.arm) 
ppc_dens_overlay(y, yrep[1:1000, ]) 

plot(density(yrep))
plot(density(y))

pairs(mdl.arm)

get_variables(mdl.arm)

pairs(mdl.arm, pars =c("(Intercept)","sigma","Sigma[judge:(Intercept),(Intercept)]","Sigma[pair:(Intercept),(Intercept)]" ))
