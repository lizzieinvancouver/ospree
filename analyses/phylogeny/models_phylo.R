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

#### Nacho, put your brms code here using bb.stan. You can also use datalist.bb if you want to use a stan model.

## trying a simple model
model_simple <- brm(
  resp ~ force, data = bb.stan, 
  family = gaussian(), 
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sigma")
  )
)

summary(model_simple)
plot(model_simple, N = 3, ask = T)


## read phylogeny
library(phytools)
phylo <- read.tree("../../data/phylogeny/ospreeFlynn.phylogeny.tre")
namesphy<-phylo$tip.label
namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
#phylo<-multi2di(phylo)
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)

unique(phylo$node.label)
phylo

## get phylogenetic covariance matrix
library(MCMCglmm)
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")

model_phylo <- brm(
  resp ~ force + (1|phylo), data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
)

summary(model_phylo)

plot(marginal_effects(model_phylo), points = TRUE) 

hyp <- "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sigma^2) = 0"
(hyp <- hypothesis(model_phylo, hyp, class = NULL))
plot(hyp)

bb.stan$spss<-bb.stan$phylo

model_phylo2 <- brm(
  resp ~ force + chill + photo + (1|spss) + (1|phylo), 
  data = bb.stan, family = gaussian(), 
  cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0,10), "b"),
    prior(normal(0,50), "Intercept"),
    prior(student_t(3,0,20), "sd"),
    prior(student_t(3,0,20), "sigma")
  ),
  sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 4000, warmup = 1000
)

summary(model_phylo2)
hyp <- paste(
  "sd_phylo__Intercept^2 /", 
  "(sd_phylo__Intercept^2 + sd_species__Intercept^2 + sigma^2) = 0"
)
(hyp <- hypothesis(model_phylo2, hyp, class = NULL))



