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




## fitting models for forcing, chilling and photo, independently 
### FORCING
### ## ad mean of predictor across species and within species
bb.stan$species_mean <- 
  with(bb.stan, sapply(split(force.z, phylo), mean)[phylo])

bb.stan$within_species <- 
  bb.stan$force.z - bb.stan$species_mean

model_phylo <- brm(
  resp ~ species_mean + within_species +      ## fixed effs
    (1 + within_species|phylo) + (1|species),  ## rnd effs 
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

### CHILLING
### ## ad mean of predictor across species and within species

bb.stan$chillmeans <- 
  with(bb.stan, sapply(split(chill.z, phylo), mean)[phylo])

bb.stan$withinsp.chillmeans <- 
  bb.stan$chill.z - bb.stan$species_mean

## 
model_phylo.chill <- brm(
  resp ~ chillmeans + withinsp.chillmeans +      ## fixed effs
    (1 + withinsp.chillmeans|phylo) + (1|species),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 4, 
  iter = 1000, warmup = 500
)


### PHOTO
### ## ad mean of predictor across species and within species
bb.stan$photomeans <- 
  with(bb.stan, sapply(split(photo.z, phylo), mean)[phylo])

bb.stan$withinsp.photomeans <- 
  bb.stan$photo.z - bb.stan$species_mean

## 
model_phylo.photo <- brm(
  resp ~ photomeans + withinsp.photomeans +      ## fixed effs
    (1 + withinsp.photomeans|phylo) + (1|species),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 4, 
  iter = 1000, warmup = 500
)

###########################################
## explore fitted models, ppc, phylo-signal 
## ########################################

## save main results as table
#fixed spec_pars cor_pars random
forceeffs<-rbind(
summary(model_phylo)$fixed,
summary(model_phylo)$random$phylo,
summary(model_phylo)$random$species,
summary(model_phylo)$spec_pars
)

chilleffs<-rbind(
  summary(model_phylo.chill)$fixed,
  summary(model_phylo.chill)$random$phylo,
  summary(model_phylo.chill)$random$species,
  summary(model_phylo.chill)$spec_pars
)

photoeffs<-rbind(
  summary(model_phylo.photo)$fixed,
  summary(model_phylo.photo)$random$phylo,
  summary(model_phylo.photo)$random$species,
  summary(model_phylo.photo)$spec_pars
)

write.csv(forceeffs,"output/force_effects.csv")
write.csv(chilleffs,"output/chill_effects.csv")
write.csv(photoeffs,"output/photo_effects.csv")



## Plot and save main results for posterior distributions of coefficients
## and ppcs

# forcing
plot(model_phylo, N = 5, ask = F)
# chilling
plot(model_phylo.chill, N = 5, ask = T)
#model_phylo.chill$fit
# photo
plot(model_phylo.photo, N = 5, ask = T)

#plot marginal effs
plot(marginal_effects(model_phylo), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.chill), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.photo), points = TRUE,ask=T) 


#plot ppcs
par(mfrow=c(1,3))
pp_check(model_phylo)
pp_check(model_phylo.chill)
pp_check(model_phylo.photo)


############################
###### Plot and save main results for posterior distributions of phylosignal
## forcing
hyp.force <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__within_species^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__within_species^2
  + sd_species__Intercept
  + cor_phylo__Intercept__within_species^2 
  + sigma^2) = 0.0"
)
(lambda.force <- hypothesis(model_phylo, hyp.force, class = NULL))

## chill
hyp.chill <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__withinsp.chillmeans^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__withinsp.chillmeans^2
  + sd_species__Intercept
  + cor_phylo__Intercept__withinsp.chillmeans^2 
  + sigma^2) = 0.0"
  )
(lambda.chill <- hypothesis(model_phylo.chill, hyp.chill, class = NULL))

## photo
hyp.photo <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__withinsp.photomeans^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__withinsp.photomeans^2
  + sd_species__Intercept
  + cor_phylo__Intercept__withinsp.photomeans^2 
  + sigma^2) = 0.0"
)
(lambda.photo <- hypothesis(model_phylo.photo, hyp.photo, class = NULL))


## plotting posteriors
plot(lambda.force)
plot(lambda.chill)
plot(lambda.photo)





