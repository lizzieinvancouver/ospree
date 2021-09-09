# September 9, 2021
# 1932 Olympics example 

# Data sent by A. Gelman
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

rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

dat <- read.csv("input/OlympicGames_1932.csv")
head(dat)

prog <- subset(dat, criterion == "Program")
str(prog)

prog_data <- list(y = prog$score, 
                   N = dim(prog)[1], 
                   pair = prog$pair, 
                   judge = prog$judge, 
                   n_pair = length(unique(prog$pair)),
                   n_judge = length(unique(prog$judge))) 

mdl<- stan('stan/OlympicGames_1932mdl.stan',
                      data = prog_data)
                      
ssm <-  as.shinystan(mdl)
launch_shinystan(ssm)

sum <- summary(mdl)$summary
post <- rstan::extract(mdl)

range(sum[, "n_eff"]) 
range(sum[, "Rhat"])

#############################################
perf <- subset(dat, criterion == "Performance")

perf_data <- list(y = perf$score, 
                  N = dim(perf)[1], 
                  pair = perf$pair, 
                  judge = perf$judge, 
                  n_pair = length(unique(perf$pair)),
                  n_judge = length(unique(perf$judge))) 

mdl2<- stan('stan/OlympicGames_1932mdl.stan',
           data = perf_data)

ssm <-  as.shinystan(mdl2)
launch_shinystan(ssm)

sum2 <- summary(mdl2)$summary
post2 <- rstan::extract(mdl2)

range(sum2[, "n_eff"]) 
range(sum2[, "Rhat"])
