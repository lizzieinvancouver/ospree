## Started 6 July 2017 ##
## By Cat and Dan and others ##
# Updated a tiny bit by Dan 19 June 2018
# Updated 8 Oct 2018
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses/phylogeny") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/phylogeny")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/phylogeny") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny")

library(shinystan)
library(caper)
library(brms)
library(pez)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillports = FALSE # change to true for using chillportions instead of utah units

# Default is species complex
use.allspp = FALSE
use.nocropspp = FALSE

# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE

source("source/bbstanleadin.phyla.R")

str(datalist.bb)
sum(is.na(datalist.bb$y))


####################################
#### Fitting Phylogenetic brms
####################################

## read and pre-process phylogeny
library(phytools)
phylo <- read.tree("../../data/phylogeny/ospreeFlynn.phylogeny.tre")
namesphy<-phylo$tip.label
namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)


## get phylogenetic covariance matrix
library(MCMCglmm)
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$spps<-bb.stan$phylo

## ad mean of predictor across species and within species
bb.stan$spec_mean_cf <- 
  with(bb.stan, sapply(split(force.z, phylo), mean)[phylo])

bb.stan$within_spec_cf <- 
  bb.stan$force.z - bb.stan$spec_mean_cf


## fit model for forcing 
model_phylo <- brm(
  resp ~ force.z + 
    (1 + within_spec_cf|species) + 
    (1|phylo) , 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 1000, warmup = 500
  )

## explore model fitted
summary(model_phylo)
plot(model_phylo, N = 5, ask = T)
plot(marginal_effects(model_phylo), points = TRUE) 

model_phylo$fit

## explore lambda
hyp <- paste(
  "sd_phylo__Intercept^2 /", 
  "(sd_phylo__Intercept^2 
  + sd_species__Intercept^2 
  + sd_species__within_spec_cf^2
  + cor_species__Intercept__within_spec_cf^2 
  + sigma^2) = 0.0"
)

(hyp <- hypothesis(model_phylo, hyp, class = NULL))
plot(hyp)


model_phylo$fit@inits



## fit model for chilling 
## 
bb.stan$spec_mean_cf <- 
  with(bb.stan, sapply(split(chill.z, phylo), mean)[phylo])

bb.stan$within_spec_cf <- 
  bb.stan$chill.z - bb.stan$spec_mean_cf


## 
model_phylo.chill <- brm(
  resp ~ chill.z + 
    (1 + within_spec_cf|species) + 
    (1|phylo) , 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 1000, warmup = 500
)

## explore model fitted
summary(model_phylo.chill)
plot(model_phylo.chill, N = 5, ask = T)
plot(marginal_effects(model_phylo.chill), points = TRUE) 

model_phylo.chill$fit

## explore lambda
hyp <- paste(
  "sd_phylo__Intercept^2 /", 
  "(sd_phylo__Intercept^2 
  + sd_species__Intercept^2 
  + sd_species__within_spec_cf^2
  + cor_species__Intercept__within_spec_cf^2 
  + sigma^2) = 0.0"
  )

(hyp <- hypothesis(model_phylo.chill, hyp, class = NULL))
plot(hyp)


## fit model for photo 
bb.stan$spec_mean_cf <- 
  with(bb.stan, sapply(split(photo.z, phylo), mean)[phylo])

bb.stan$within_spec_cf <- 
  bb.stan$photo.z - bb.stan$spec_mean_cf


## 
model_phylo.photo <- brm(
  resp ~ photo.z + 
    (1 + within_spec_cf|species) + 
    (1|phylo) , 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 1000, warmup = 500
)

## explore model fitted
summary(model_phylo.photo)
plot(model_phylo.photo, N = 5, ask = T)
plot(marginal_effects(model_phylo.photo), points = TRUE) 

model_phylo.photo$fit

## explore lambda
hyp <- paste(
  "sd_phylo__Intercept^2 /", 
  "(sd_phylo__Intercept^2 
  + sd_species__Intercept^2 
  + sd_species__within_spec_cf^2
  + cor_species__Intercept__within_spec_cf^2 
  + sigma^2) = 0.0"
  )

(hyp <- hypothesis(model_phylo.photo, hyp, class = NULL))
plot(hyp)
