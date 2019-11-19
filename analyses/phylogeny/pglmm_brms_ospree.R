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
library(rstan)
library(phytools)
library(MCMCglmm)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



#### get data through bbstanleadin ####
#######################################

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillports = FALSE # change to true for using chillportions instead of utah units

# Default is species complex
use.allspp = F
use.nocropspp = F

# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE

source("source/bbstanleadin.phyla.R")

namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
bb.stan$complex.wname




#### get phylogeny              ####
####################################

source("source/get_phylo_models.R")


## read and pre-process phylogeny ####
#phylo <- read.tree("../../data/phylogeny/SBphylo_62complex.tre")
#phylo <- read.tree("../../data/phylogeny/SBphylo_101sps.tre")
phylo <- phy.plants.ospree

namesphy<-phylo$tip.label
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)
plot(phylo,cex=0.7)



## get phylogenetic covariance matrix 
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$spps<-bb.stan$phylo




#### explore PGLMM results      ####
####################################


## model PGLMM_all (full BB model no interactions)
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
PGLMM_all<-MCMCglmm(resp~force.z+chill.z+photo.z, random=~spps, ginverse=list(spps=inv.phylo$Ainv),
                    data=bb.stan, prior=prior, verbose=FALSE, nitt=1300, burnin=300, thin=1)
summary(PGLMM_all)
plot(PGLMM_all)

# Get 95% quantiles and mean for Heredity (H^2)
h2_all <- PGLMM_all$VCV[,1]/apply(PGLMM_all$VCV,1,sum)
quants = round(as.numeric(quantile(h2_all,probs = c(0.025,0.5,0.975))),3)
h2_all <- data.frame(
  "H2_2.5"=c(quants[1],rep("",3)),
  "H2_50"=c(quants[2],rep("",3)),
  "H2_97.5"=c(quants[3],rep("",3)))


## model PGLMM_forcing (no interactions)
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
PGLMM_force<-MCMCglmm(resp~force.z, random=~spps, ginverse=list(spps=inv.phylo$Ainv),
                      data=bb.stan, prior=prior, verbose=FALSE, nitt=1300, burnin=300, thin=1)
summary(PGLMM_force)
plot(PGLMM_force)

# Get 95% quantiles and mean for Heredity (H^2)
h2_force <- PGLMM_force$VCV[,1]/apply(PGLMM_force$VCV,1,sum)
quants = round(as.numeric(quantile(h2_force,probs = c(0.025,0.5,0.975))),3)
h2_force <- data.frame(
  "H2_2.5"=c(quants[1],rep("",1)),
  "H2_50"=c(quants[2],rep("",1)),
  "H2_97.5"=c(quants[3],rep("",1)))



## model PGLMM_chill (no interactions)
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
PGLMM_chill<-MCMCglmm(resp~chill.z, random=~spps, ginverse=list(spps=inv.phylo$Ainv),
                      data=bb.stan, prior=prior, verbose=FALSE, nitt=1300, burnin=300, thin=1)
summary(PGLMM_chill)
plot(PGLMM_chill)

# Get 95% quantiles and mean for Heredity (H^2)
h2_chill <- PGLMM_chill$VCV[,1]/apply(PGLMM_chill$VCV,1,sum)
quants = round(as.numeric(quantile(h2_chill,probs = c(0.025,0.5,0.975))),3)
h2_chill <- data.frame(
  "H2_2.5"=c(quants[1],rep("",1)),
  "H2_50"=c(quants[2],rep("",1)),
  "H2_97.5"=c(quants[3],rep("",1)))


## model PGLMM_photo (no interactions)
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
PGLMM_photo<-MCMCglmm(resp~photo.z, random=~spps, ginverse=list(spps=inv.phylo$Ainv),
                      data=bb.stan, prior=prior, verbose=FALSE, nitt=1300, burnin=300, thin=1)
summary(PGLMM_photo)


# Get 95% quantiles and mean for Heredity (H^2)
h2_photo <- PGLMM_photo$VCV[,1]/apply(PGLMM_photo$VCV,1,sum)
quants = round(as.numeric(quantile(h2_photo,probs = c(0.025,0.5,0.975))),3)
h2_photo <- data.frame(
  "H2_2.5"=c(quants[1],rep("",1)),
  "H2_50"=c(quants[2],rep("",1)),
  "H2_97.5"=c(quants[3],rep("",1)))



## intercept only model PGLMM_forcing (no interactions)phylosig in treatments
#prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
#PGLMM_force_phylo<-MCMCglmm(force.z~1, random=~spps, ginverse=list(spps=inv.phylo$Ainv),
#                      data=bb.stan, prior=prior, verbose=FALSE, nitt=1300, burnin=300, thin=1)
#summary(PGLMM_force_phylo)



## Make summary table of PGLMM results
pglmmtable = cbind(
  rbind(summary(PGLMM_all)$solutions,
        summary(PGLMM_force)$solutions,
        summary(PGLMM_chill)$solutions,
        summary(PGLMM_photo)$solutions),
  rbind(h2_all,h2_force,h2_chill,h2_photo))

# according to PGLMM BB responses to all three cues are strongly phylogenetically structured/inherited
# in other words ~90% of the variance is explained by the (phylogenetically structured)
# differences across species. Phylogenetically close species are more likely to show
# similar responses to each of the cues (a little more so for forcing).
write.csv(pglmmtable, file = "output/PGLMM_results_ospree.csv")



#### explore BRMS results - Version 1 ####
##########################################

#Let's explore the results for different specifications of BRMS models. 

# The PGLS model - phylogeny as random effect on intercept - one measurement per species 
# our data has repeated measures for each species, so we need to synthetize or 
# aggregate the data.

bb.pgls <- aggregate(x = bb.stan, 
            by = list(phylo = bb.stan$phylo), 
            FUN = mean)


# the model specification would be $$y ~ \alpha_{phylo} + \beta \overline{x} + \varepsilon$$
model_phylo.PGLS.69obs <- brm(
  resp ~ force.z + chill.z + photo.z +      ## fixed effs
    (1 |phylo),  ## rnd effs 
  data = bb.pgls, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 1000, warmup = 250
)

# this model would not make much sense as we loose a lot of information and fail
# to retrieve whether or not sensitivities to the cues are phylogenetically structured

summary(model_phylo.PGLS.69obs)
plot(marginal_effects(model_phylo.PGLS.69obs), points = TRUE, ask=T) 
pp_check(model_phylo.PGLS.69obs)
PGLS69effs<-rbind(
  summary(model_phylo.PGLS.69obs)$fixed,
  summary(model_phylo.PGLS.69obs)$random$phylo,
  summary(model_phylo.PGLS.69obs)$spec_pars
)

## the phylogenetic signal
hyp.PGLS69effs <- paste(
  "(sd_phylo__Intercept^2)/", 
  "(sd_phylo__Intercept^2 
  + sigma^2) = 0.0"
)
(lambda.hyp.PGLS69 <- hypothesis(model_phylo.PGLS.69obs, 
                                 hyp.PGLS69effs, class = NULL))

# It still informs, howerver, that most variation is phylogenetically structured.
# A quick PGLS analyses reveals similar results

## generate a comparative.data object merging data and phylogeny
bb.pgls.caper = comparative.data(phylo, bb.pgls[,c("resp","force.z","chill.z","photo.z",
                                 "phylo")],names.col = "phylo",na.omit = T, vcv = T)
model_PGLS.69obs <- pgls(resp ~ force.z + chill.z + photo.z, data = bb.pgls.caper,
                         lambda='ML')
summary(model_PGLS.69obs)
save(model_PGLS.69obs,file = "output/model_PGLS.69obs.RData")
#load("output/model_PGLS.69obs.RData")




summary(model_phylo.PGLS)




## fitting models for forcing, chilling and photo, independently 
### FORCING
### ## add mean of predictor across species and within species
bb.stan$species_mean.force <- 
  with(bb.stan, sapply(split(force.z, phylo), mean)[phylo])

bb.stan$within_species <- 
  bb.stan$force.z - bb.stan$species_mean.force

model_phylo.force <- brm(
  resp ~ within_species +                      ## fixed effs
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
  iter = 500, warmup = 100
)

summary(model_phylo.force)
model_phylo.force$fit
plot(model_phylo.force)
launch_shinystan(model_phylo.force)


### CHILLING
### ## ad mean of predictor across species and within species

bb.stan$chillmeans <- 
  with(bb.stan, sapply(split(chill.z, phylo), mean)[phylo])

bb.stan$withinsp.chillmeans <- 
  bb.stan$chill.z - bb.stan$species_mean

## 
model_phylo.chill <- brm(
  resp ~ withinsp.chillmeans +      ## fixed effs
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
  resp ~  withinsp.photomeans +      ## fixed effs
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



#### FULL MODEL ####
### ## ad mean of predictor across species and within species
bb.stan$photomeans <- 
  with(bb.stan, sapply(split(photo.z, phylo), mean)[phylo])

bb.stan$withinsp.photomeans <- 
  bb.stan$photo.z - bb.stan$species_mean

## 
model_phylo.FULL <- brm(
  resp ~ within_species + withinsp.chillmeans + withinsp.photomeans +      ## fixed effs
    (1 + within_species + withinsp.chillmeans + withinsp.photomeans |phylo) +
    (1|species),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 4, 
  iter = 2000, warmup = 500
)



## explore fitted models, ppc, phylo-signal ####

## save main results as table
#fixed spec_pars cor_pars random
forceeffs<-rbind(
  summary(model_phylo.force)$fixed,
  summary(model_phylo.force)$random$phylo,
  summary(model_phylo.force)$random$species,
  summary(model_phylo.force)$spec_pars
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


FULLeffs<-rbind(
  summary(model_phylo.FULL)$fixed,
  summary(model_phylo.FULL)$random$phylo,
  summary(model_phylo.FULL)$random$species,
  summary(model_phylo.FULL)$spec_pars
)


PGLSeffs<-rbind(
  summary(model_phylo.PGLS)$fixed,
  summary(model_phylo.PGLS)$random$phylo,
  summary(model_phylo.PGLS)$random$species,
  summary(model_phylo.PGLS)$spec_pars
)


write.csv(forceeffs,"output/force_effects_onlyphy.csv")
write.csv(chilleffs,"output/chill_effects_onlyphy.csv")
write.csv(photoeffs,"output/photo_effects_onlyphy.csv")
write.csv(FULLeffs,"output/full_effects_onlyphy.csv")
write.csv(PGLSeffs,"output/pgls_effects_onlyphy.csv")


## provisional save
save(model_phylo.force,file = "output/force_phylomod_onlyphy.RData")
save(model_phylo.chill,file = "output/chill_phylomod_onlyphy.RData")
save(model_phylo.photo,file = "output/photo_phylomod_onlyphy.RData")
save(model_phylo.FULL,file = "output/full_phylomod_onlyphy.RData")
save(model_phylo.PGLS,file = "output/pgls_phylomod_onlyphy.RData")

#load("output/full_phylomod_onlyphy.RData")
#load("output/pgls_phylomod_onlyphy.RData")
#load("output/force_phylomod_onlyphy.RData")
#load("output/chill_phylomod_onlyphy.RData")
#load("output/photo_phylomod_onlyphy.RData")
model_phylo.chill_means = model_phylo.chill
model_phylo.force_means = model_phylo.force 
model_phylo.photo_means = model_phylo.photo

## Plot and save main results for posterior distributions of coefficients
## and ppcs

# forcing
plot(model_phylo.force, N = 5, ask = F)
# chilling
plot(model_phylo.chill, N = 5, ask = T)
#model_phylo.chill$fit
# photo
plot(model_phylo.photo, N = 5, ask = T)

#plot marginal effs
plot(marginal_effects(model_phylo.force), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.chill), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.photo), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.PGLS), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.FULL), points = TRUE,ask=T) 



#plot ppcs
par(mfrow=c(1,3))
pp_check(model_phylo.force)
pp_check(model_phylo.chill)
pp_check(model_phylo.photo)
pp_check(model_phylo.PGLS)
pp_check(model_phylo.FULL)


###### Plot and save main results for posterior distributions of phylosignal
## forcing
hyp.force <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__within_species^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__within_species^2
  + sd_species__Intercept
  + sigma^2) = 0.0"
)
(lambda.force <- hypothesis(model_phylo.force, hyp.force, class = NULL))
plot(lambda.force)

## chill
hyp.chill <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__withinsp.chillmeans^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__withinsp.chillmeans^2
  + sd_species__Intercept
  + sigma^2) = 0.0"
)
(lambda.chill <- hypothesis(model_phylo.chill, hyp.chill, class = NULL))

## photo
hyp.photo <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__withinsp.photomeans^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__withinsp.photomeans^2
  + sd_species__Intercept
  + sigma^2) = 0.0"
)
(lambda.photo <- hypothesis(model_phylo.photo, hyp.photo, class = NULL))


## pgls
hyp.pgls <- paste(
  "(sd_phylo__Intercept^2+ sd_species__Intercept^2) /", 
  "(sd_phylo__Intercept^2 
  + sd_species__Intercept^2
  + sigma^2) = 0.0"
)
(lambda.pgls <- hypothesis(model_phylo.PGLS, hyp.pgls, class = NULL))
plot(lambda.pgls)

    
 v     BV B
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
V 
     ## plotting posteriors
plot(lambda.force)
plot(lambda.chill)
plot(lambda.photo)



#### explore BRMS results - Version 2 - phylo on slopes ####
############################################################

## fitting models for forcing, chilling and photo, independently 
### FORCING
### ## add mean of predictor across species and within species
bb.stan$species_mean <- 
  with(bb.stan, sapply(split(force.z, phylo), mean)[phylo])

bb.stan$within_species <- 
  bb.stan$force.z - bb.stan$species_mean

model_phylo.force <- brm(
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

summary(model_phylo.force)
plot(model_phylo.force)
yshin<-bb.stan$resp
launch_shinystan(model_phylo.force)


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





## explore fitted models, ppc, phylo-signal 

## save main results as table
#fixed spec_pars cor_pars random
forceeffs<-rbind(
  summary(model_phylo.force)$fixed,
  summary(model_phylo.force)$random$phylo,
  summary(model_phylo.force)$random$species,
  summary(model_phylo.force)$spec_pars
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


## provisional save
#save(model_phylo.force,file = "output/force_phylomod.RData")
#save(model_phylo.chill,file = "output/chill_phylomod.RData")
#save(model_phylo.photo,file = "output/photo_phylomod.RData")



## Plot and save main results for posterior distributions of coefficients
## and ppcs

# forcing
plot(model_phylo.force, N = 5, ask = F)
# chilling
plot(model_phylo.chill, N = 5, ask = T)
#model_phylo.chill$fit
# photo
plot(model_phylo.photo, N = 5, ask = T)

#plot marginal effs
plot(marginal_effects(model_phylo.force), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.chill), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.photo), points = TRUE,ask=T) 


#plot ppcs
par(mfrow=c(1,3))
pp_check(model_phylo.force)
pp_check(model_phylo.chill)
pp_check(model_phylo.photo)


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

