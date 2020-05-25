#'######################################################
# Script and functions to:
# * Run phylogenetic analyses of cue-sensitivity for OSPREE dataset
#
# Started 17th April 2020
# by Ignacio Morales-Castilla, Lizzie Wolkovich and others 
#'######################################################


# This code:  
# 1) runs a BB model against 3-cues with partial pooling by species on the 
#     intercepts and slopes 
# 2) explores the phylogenetic signal in fitted species-level slopes
# 3) checks robustness across data subsets and models. data subsets:
#     - all species with crops
#     - all species with crops / without gymnosperms
#     - all species without crops
#     - all species without crops / without gymnosperms
#     - generic level
# 


#### remove objects (activate if needed) ####
rm(list=ls())
options(stringsAsFactors = FALSE)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/phylogeny")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/phylogeny") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny")

# Loading packages
library(shinystan)
library(caper)
library(brms)
library(pez)
library(rstan)
library(phytools)
library(MCMCglmm)
library(dplyr)
library(knitr)
library(broom)
options(mc.cores = parallel::detectCores())


#'######################################
#### get data through bbstanleadin ####
#'######################################

# Flags to choose for bbstanleadin.R #
setwd("..//bb_analysis") 


# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- F
use.flags.for.allsppmodel <- T
use.yourown.flagdesign <- F
nocrops <- T
agiosponly <- T

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
   use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
   & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp =FALSE # for the main model this is false
  use.multcuespp = FALSE
  use.cropspp = FALSE
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.allsppmodel){
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = F # change to false for using utah instead of chill portions (most models use chill portions z)
  use.zscore = TRUE # change to false to use raw predictors
  
  # Default is species complex and no crops
  use.allspp = F
  use.multcuespp = FALSE
  use.cropspp = FALSE
  
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  
  #Default is all chilling data
  use.expchillonly = FALSE # change to true for only experimental chilling 
  #note: with only exp chilling, there is only exp photo and force too.
  #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
}

source("..//bb_analysis/source/bbstanleadin.R")

namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
bb.stan$spps<-paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")


#'###################################
#### get phylogeny              ####
#'###################################

setwd("..//phylogeny") 
source("source/get_phylo_models.R")

## read and pre-process phylogeny
#phylo <- read.tree("../../data/phylogeny/SBphylo_62complex.tre")
#phylo <- read.tree("../../data/phylogeny/SBphylo_101sps.tre")
phylo <- phy.plants.ospree


namesphy<-phylo$tip.label
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)
plot(phylo, cex=0.7)
VCVPHY<-vcv.phylo(phylo,corr=T)



## deal with subgrouping
if(nocrops & agiosponly){
  gymno <- c("Metasequoia_glyptostroboides",  "Pseudotsuga_menziesii","Larix_laricina",
             "Larix_gmelinii", "Larix_decidua" ,"Larix_kaempferi",   
             "Pinus_nigra","Pinus_sylvestris","Pinus_banksiana",  
             "Pinus_contorta","Pinus_wallichiana","Pinus_strobus", 
             "Picea_abies"   ,"Picea_mariana" ,"Picea_glauca" ,
             "Cedrus_libani" ,"Abies_alba"    ,"Abies_homolepis","Ginkgo_biloba")
  croplist <- read.csv("../../data/croplist/agricultural_species.csv")
  cropgymno <- c(croplist$Species_name,gymno)
  bb.stan$crops <- ifelse(bb.stan$spps %in% cropgymno, "cropgymno","nocrop")
  cropspps <- unique(bb.stan$spps[which(bb.stan$crops=="cropgymno")])
  bb.stan <- subset(bb.stan, crops == "nocrop")
  phylo <- drop.tip(phylo, cropspps)
  VCVPHY<-vcv.phylo(phylo,corr=T)
} 



if(nocrops & !agiosponly){
  croplist <- read.csv("../../data/croplist/agricultural_species.csv")
  bb.stan$crops <- ifelse(bb.stan$spps %in% croplist$Species_name, "crop","nocrop")
  cropspps <- unique(bb.stan$spps[which(bb.stan$crops=="crop")])
  bb.stan <- subset(bb.stan, crops == "nocrop")
  phylo <- drop.tip(phylo, cropspps)
  VCVPHY<-vcv.phylo(phylo,corr=T)
} 


if(!nocrops & agiosponly){
  gymno <- c("Metasequoia_glyptostroboides",  "Pseudotsuga_menziesii","Larix_laricina",
             "Larix_gmelinii", "Larix_decidua" ,"Larix_kaempferi",   
             "Pinus_nigra","Pinus_sylvestris","Pinus_banksiana",  
             "Pinus_contorta","Pinus_wallichiana","Pinus_strobus", 
             "Picea_abies"   ,"Picea_mariana" ,"Picea_glauca" ,
             "Cedrus_libani" ,"Abies_alba"    ,"Abies_homolepis","Ginkgo_biloba")
  cropgymno <- c(gymno)
  bb.stan$crops <- ifelse(bb.stan$spps %in% cropgymno, "cropgymno","nocrop")
  cropspps <- unique(bb.stan$spps[which(bb.stan$crops=="cropgymno")])
  bb.stan <- subset(bb.stan, crops == "nocrop")
  phylo <- drop.tip(phylo, cropspps)
  VCVPHY<-vcv.phylo(phylo,corr=T)
} 


phylo.all235<-phylo
save(phylo.all235,file = '~/Data_Harvard/phylogeny/input/phylo.all235.RData')

phylo.angio216<-phylo
save(phylo.angio216,file = '~/Data_Harvard/phylogeny/input/phylo.all216.RData')

phylo.angionocrop195<-phylo
save(phylo.angionocrop195,file = '~/Data_Harvard/phylogeny/input/phylo.angionocrop195.RData')

phylo.allnocrop214<-phylo
save(phylo.allnocrop214,file = '~/Data_Harvard/phylogeny/input/phylo.allnocrop214.RData')

phylos=list(phylo.allnocrop214,phylo.angio216,phylo.angionocrop195)
save(phylos,file = '~/Data_Harvard/phylogeny/input/phylos.RData')

## TEST
# Step 1: Get spps and VCVPHY in same order
# bb.stan$spps[phylo$tip.label]
phylo$tip.label
d <- bb.stan[match(phylo$tip.label, bb.stan$spps),] # hmmm, only gives ONE match

phymatch <- data.frame(tip=phylo$tip.label, sppnum=c(1:length(phylo$tip.label)))
d <- merge(bb.stan, phymatch, by.x="spps", by.y="tip")
d <- d[order(d$sppnum),]
# Tilia_cordata versus Tilia_Cordata in phylo

# Step 2: Run a simple version of the model 
nspecies <- max(d$sppnum)
testme <- stan("stan/nointer_2level_force.stan",
                data=list(N=nrow(d), n_sp=nspecies, sp=d$sppnum,
                force=d$force.z, y=d$resp,
                Vphy=vcv(phylo, corr=TRUE)),
                iter=1000, chains=4, seed=123456)
summary(testme)$summary

testme2 <- stan("stan/nointer_2levelphyall.stan",
                data=list(N=nrow(d), n_sp=nspecies, sp=d$sppnum,
                force=d$force.z, chill=d$chill.z, photo=d$photo.z,
                y=d$resp,
                Vphy=vcv(phylo, corr=TRUE)),
                iter=2000, chains=4, seed=123456)
summary(testme2)$summary

testme3 <- stan("stan/nointer_2levelphy.stan",
                data=list(N=nrow(d), n_sp=nspecies, sp=d$sppnum,
                force=d$force.z, chill=d$chill.z, photo=d$photo.z,
                y=d$resp,
                Vphy=vcv(phylo, corr=TRUE)),
                iter=3000, chains=4, seed=123456)
summary(testme3)$summary
# use shinystan to see that this model is struggling on null_interceptsbf and null_interceptsbp
# save(testme3, file="stan/output/nointer_2levelphy.Rda")

source("source/bb_muplotphylo.R")
modelhere <- testme3
figpathmore <- "testme3"
library(RColorBrewer)
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
my.pch <- rep(15:18, each=12)
alphahere = 0.4

muplotfx(modelhere, "", 7, 8, c(0,3), c(-30, 10) , 12, 3.5)

par(mfrow=c(2,3))
hist(extract(modelhere)[["null_interceptsbf"]], main="force")
hist(extract(modelhere)[["null_interceptsbc"]], main="chill")
hist(extract(modelhere)[["null_interceptsbp"]], main="photo")

hist(extract(modelhere)[["lam_interceptsbf"]], main="lambda force")
hist(extract(modelhere)[["lam_interceptsbc"]], main="lambda chill")
hist(extract(modelhere)[["lam_interceptsbp"]], main="lambda photo")


lamf.int <- mean(extract(modelhere)[["lam_interceptsbf"]])
nullf.int <- mean(extract(modelhere)[["null_interceptsbf"]])
lamf.int / (nullf.int + lamf.int)

lamc.int <- mean(extract(modelhere)[["lam_interceptsbc"]])
nullc.int <- mean(extract(modelhere)[["null_interceptsbc"]])
lamc.int / (nullc.int + lamc.int)

lamp.int <- mean(extract(modelhere)[["lam_interceptsbp"]])
nullp.int <- mean(extract(modelhere)[["null_interceptsbp"]])
lamp.int / (nullp.int + lamp.int)


## END TEST


#'################################################
#### Running, saving, loading BRMS models    ####
#'################################################


#### First model: pooling by species on intercept ####
"$$Budbreak = \alpha_{species} + \beta_{1}forcing
+ \beta_{2}chilling + \beta_{3}photo + \varepsilon$$"

mod1_sp_int <- brm(
  resp ~ force.z + chill.z + photo.z + ## fixed 
    (1|spps),  ## rnd effs 
  data = bb.stan[1:100,], 
  family = gaussian(),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 4, 
  iter = 2000, warmup = 1000, control = list(adapt_delta = 0.99) 
)

#### Second model: pooling by species on intercept and slope####
"$$Budbreak = \alpha_{species} + \beta_{1,species}forcing
+ \beta_{2,species}chilling + \beta_{3,species}photo + \varepsilon$$"
mod2_sp_int_slope <- brm(
  resp ~ force.z + chill.z + photo.z + ## fixed 
    (1 + force.z + chill.z + photo.z|spps),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 4, 
  iter = 2000, warmup = 1000, control = list(adapt_delta = 0.99) 
)


#### Second model: pooling by species on intercept and slope####
"$$Budbreak = \alpha_{phylo} + \beta_{1,phylo}forcing
+ \beta_{2,phylo}chilling + \beta_{3,phylo}photo + \varepsilon$$"
mod3_phylo_int_slope <- brm(
  resp ~ force.z + chill.z + photo.z + ## fixed 
    (1 + force.z + chill.z + photo.z|phylo),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = VCVPHY),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 4, 
  iter = 2000, warmup = 1000, control = list(adapt_delta = 0.99) 
)


save(mod1_sp_int,file = "~/Data_Harvard/phylogeny/output/mod1_sp_int_232sps.RData")
save(mod2_sp_int_slope,file = "~/Data_Harvard/phylogeny/output/mod2_sp_int_slope_232sps.RData")
save(mod3_phylo_int_slope,file = "~/Data_Harvard/phylogeny/output/mod3_phylo_int_slope_232sps.RData")




#'############################################################
#### fitting phylogenetic models for cue-sensitivities    ####
#'############################################################

## function to summarize results
summ.phylo.mod <- function(mod,phylog){
  
  # get model summaries
  mod.summary <- tidy(mod)
  
  ## Cue-sensitivity model A: sensitivity to forcing - phylo structure
  
  # model specs
  "$$\beta_{1} = \alpha_{phylo} + \varepsilon$$"
  
  positions.force = grep(",force.z]", mod.summary$term)
  force.slopes <- mod.summary[positions.force,] 
  force.slopes$phylo <- sort(unique(mod$data$spps))
  phylog<-drop.tip(phylog,which(!phylog$tip.label%in%force.slopes$phylo))
  VCVPHY<-vcv.phylo(phylog,corr=T)
  
  
  # BRMS repeated measures, species as grouping on intercept MODEL 
  model_beta.force <- brm(
    estimate ~ 1 + ## fixed 
      (1|phylo),  ## rnd effs 
    data = force.slopes, 
    family = gaussian(), cov_ranef = list(phylo = VCVPHY),
    prior = c(
      #prior(normal(0, 20), "b"),
      prior(normal(0, 50), "Intercept"),
      prior(student_t(3, 0, 20), "sd"),
      prior(student_t(3, 0, 20), "sigma")
    )
    ,sample_prior = TRUE, chains = 2, cores = 2, 
    iter = 2000, warmup = 1000
  )
  
  
  ## Cue-sensitivity model B: sensitivity to chilling - phylo structure
  
  ## model B: sensitivity to chilling - phylo structure
  "$$\beta_{2} = \alpha_{phylo} + \varepsilon$$"
  
  positions.chill = grep(",chill.z]", mod.summary$term)
  chill.slopes <- mod.summary[positions.chill,] 
  chill.slopes$phylo <- sort(unique(mod$data$spps))
  
  
  # BRMS repeated measures, species as grouping on intercept MODEL 
  model_beta.chill <- brm(
    estimate ~ 1 + ## fixed 
      (1|phylo),  ## rnd effs 
    data = chill.slopes, 
    family = gaussian(), cov_ranef = list(phylo = VCVPHY),
    prior = c(
      #prior(normal(0, 20), "b"),
      prior(normal(0, 50), "Intercept"),
      prior(student_t(3, 0, 20), "sd"),
      prior(student_t(3, 0, 20), "sigma")
    )
    ,sample_prior = TRUE, chains = 4, cores = 2, 
    iter = 2000, warmup = 1000
  )
  
  
  
  ## Cue-sensitivity model C: sensitivity to photo - phylo structure
  "$$\beta_{3} = \alpha_{phylo} + \varepsilon$$"
  
  positions.photo = grep(",photo.z]", mod.summary$term)
  photo.slopes <- mod.summary[positions.photo,] 
  photo.slopes$phylo <- sort(unique(mod$data$spps))
  
  
  model_beta.photo <- brm(
    estimate ~ 1 + ## fixed 
      (1|phylo),  ## rnd effs 
    data = photo.slopes, 
    family = gaussian(), cov_ranef = list(phylo = VCVPHY),
    prior = c(
      #prior(normal(0, 20), "b"),
      prior(normal(0, 50), "Intercept"),
      prior(student_t(3, 0, 20), "sd"),
      prior(student_t(3, 0, 20), "sigma")
    )
    ,sample_prior = TRUE, chains = 4, cores = 2, 
    iter = 2000, warmup = 1000
  )
  
  
  #####################################################
  #### check phylogenetic signal in sensitivities  ####
  #####################################################
  
  
  ## the phylogenetic signal
  hyp.phylo.force <- paste(
    "(sd_phylo__Intercept^2) /", 
    "(sd_phylo__Intercept^2 
  + sigma^2) = 0.0"
  )
  (lambda.phylo.force <- hypothesis(model_beta.force, 
                                    hyp.phylo.force, class = NULL))
  
  hyp.phylo.chill <- paste(
    "(sd_phylo__Intercept^2) /", 
    "(sd_phylo__Intercept^2 
  + sigma^2) = 0.0"
  )
  (lambda.phylo.chill <- hypothesis(model_beta.chill, 
                                    hyp.phylo.chill, 
                                    class = NULL))
  
  hyp.phylo.photo <- paste(
    "(sd_phylo__Intercept^2) /", 
    "(sd_phylo__Intercept^2 
  + sigma^2) = 0.0"
  )
  (lambda.phylo.photo <- hypothesis(model_beta.photo, 
                                    hyp.phylo.photo, 
                                    class = NULL))
  
  
  
  
  ###########################################################
  #### Checking phylogenetic structure with PGLS - caper ####
  ###########################################################
  
  # generate a comparative data object for caper
  sensitivities <- data.frame(force = force.slopes$estimate,
                              chill = chill.slopes$estimate,
                              photo = photo.slopes$estimate,
                              species = force.slopes$phylo)
  
  phylog$node.label = 1:length(phylog$node.label)
  sensitivities.compdat <- comparative.data(phylog,sensitivities,
                                            names.col= "species",
                                            vcv = T)
  sensitivities.compdat$vcv <- VCVPHY
  # fit pgls model for each cue
  pgls.force.ml = pgls(force ~ 1,
                       data=sensitivities.compdat,lambda="ML")
  pgls.chill.ml = pgls(chill ~ 1,
                       data=sensitivities.compdat,lambda="ML")
  pgls.photo.ml = pgls(photo ~ 1,
                       data=sensitivities.compdat,lambda="ML")
  
  force.ml<-summary(pgls.force.ml)
  chill.ml<-summary(pgls.chill.ml)
  photo.ml<-summary(pgls.photo.ml)
  
  
  ######################################################
  #### Model evaluation - LOO - phylo vs. non-phylo ####
  ######################################################
  
  # inspect R2s
  R2table <- bayes_R2(mod)
  
  # inspect LOOs
  loo.mod <- loo(mod)
  
  
  #write.csv(loocomparisons,file = "output/LOO_model_comparison.csv")
  resulttable <- mod.summary[1:4,]
  resulttable$p_loo.SE <- resulttable$p_loo <-
    resulttable$elpd_loo.SE <- resulttable$elpd_loo <-
    resulttable$R2.error <- resulttable$R2 <-
    resulttable$lambda.low <- resulttable$lambda.up <- resulttable$lambda <-
    resulttable$H2.low <- resulttable$H2.up <- resulttable$H2 <- NA
  
  resulttable$H2[2:4] <- c(lambda.phylo.force$hypothesis$Estimate,
                           lambda.phylo.chill$hypothesis$Estimate,
                           lambda.phylo.photo$hypothesis$Estimate)
  resulttable$H2.up[2:4] <- c(lambda.phylo.force$hypothesis$CI.Upper,
                              lambda.phylo.chill$hypothesis$CI.Upper,
                              lambda.phylo.photo$hypothesis$CI.Upper)
  resulttable$H2.low[2:4] <- c(lambda.phylo.force$hypothesis$CI.Lower,
                               lambda.phylo.chill$hypothesis$CI.Lower,
                               lambda.phylo.photo$hypothesis$CI.Lower)
  resulttable$lambda[2:4] <- c(force.ml$param[2],
                               chill.ml$param[2],
                               photo.ml$param[2])
  resulttable$lambda.up[2:4] <- c(force.ml$param.CI$lambda$ci.val[2],
                                  chill.ml$param.CI$lambda$ci.val[2],
                                  photo.ml$param.CI$lambda$ci.val[2])
  resulttable$lambda.low[2:4] <- c(force.ml$param.CI$lambda$ci.val[1],
                                   chill.ml$param.CI$lambda$ci.val[1],
                                   photo.ml$param.CI$lambda$ci.val[1])
  
  
  resulttable$R2[1] <- R2table[1] 
  resulttable$R2.error[1] <- R2table[2]
  resulttable$elpd_loo[1] <- loo.mod$estimates[1,1] 
  resulttable$elpd_loo.SE[1] <- loo.mod$estimates[1,2] 
  resulttable$p_loo[1] <- loo.mod$estimates[2,1] 
  resulttable$p_loo.SE[1] <- loo.mod$estimates[2,2] 
  
  
  return(list(TableResults = resulttable,
              Slopes = cbind(force.slopes,chill.slopes,photo.slopes),
              lambdas = list(lambda.phylo.force=lambda.phylo.force,
                             lambda.phylo.chill=lambda.phylo.chill,
                             lambda.phylo.photo=lambda.phylo.photo)))
  
}


## load phylogenies
load('~/Data_Harvard/phylogeny/input/phylo.all235.RData')
load('~/Data_Harvard/phylogeny/input/phylo.all216.RData')
load('~/Data_Harvard/phylogeny/input/phylo.allnocrop214.RData')
load('~/Data_Harvard/phylogeny/input/phylo.angionocrop195.RData')


load('~/Data_Harvard/phylogeny/output/mod2_sp_int_slope_232sps.RData')
load('~/Data_Harvard/phylogeny/output/mod3_phylo_int_slope_232sps.RData')
result.all235.mod2 <- summ.phylo.mod(mod=mod2_sp_int_slope,phylog=phylo.all235)
result.all235.mod3 <- summ.phylo.mod(mod=mod3_phylo_int_slope,phylog=phylo.all235)

load('~/Data_Harvard/phylogeny/output/mod2_sp_int_slope_216allangiosps.RData')
load('~/Data_Harvard/phylogeny/output/mod3_phylo_int_slope_216allangiosps.RData')
result.angio216.mod2 <- summ.phylo.mod(mod=mod2_sp_int_slope,phylog=phylo.angio216)
result.angio216.mod3 <- summ.phylo.mod(mod=mod3_phylo_int_slope,phylog=phylo.angio216)

load('~/Data_Harvard/phylogeny/output/mod2_sp_int_slope_214nocropsps.RData')
load('~/Data_Harvard/phylogeny/output/mod3_phylo_int_slope_214nocropsps.RData')
result.allnocrop214.mod2 <- summ.phylo.mod(mod=mod2_sp_int_slope,phylog=phylo.allnocrop214)
result.allnocrop214.mod3 <- summ.phylo.mod(mod=mod3_phylo_int_slope,phylog=phylo.allnocrop214)

load('~/Data_Harvard/phylogeny/output/mod2_sp_int_slope_195nocropangiosps.RData')
load('~/Data_Harvard/phylogeny/output/mod3_phylo_int_slope_195nocropangiosps.RData')
result.angionocrop195.mod2 <- summ.phylo.mod(mod=mod2_sp_int_slope,phylog=phylo.angionocrop195)
result.angionocrop195.mod3 <- summ.phylo.mod(mod=mod3_phylo_int_slope,phylog=phylo.angionocrop195)


## combine and save tables

Full.table <- rbind(result.all235.mod2$TableResults,
                    result.all235.mod3$TableResults,
                    result.angio216.mod2$TableResults,
                    result.angio216.mod3$TableResults,
                    result.allnocrop214.mod2$TableResults,
                    result.allnocrop214.mod3$TableResults,
                    result.angionocrop195.mod2$TableResults,
                    result.angionocrop195.mod3$TableResults)

write.csv(Full.table,file = "output/final_phylosig_results.csv")




##############################################################
#### Visualize phylogenetic structure in cue-sensitivity  ####
##############################################################

dev.off()

plottingphylosig<-function(res.summ.phylo,phylo){

  if(length(phylo$tip.label)!=nrow(res.summ.phylo$Slopes)){
    phylo=drop.tip(phylo,which(!phylo$tip.label %in% res.summ.phylo$Slopes[,6]))
  }
  phylo=multi2di(phylo)
  
  ## forcing
forc = res.summ.phylo$Slopes[,2]
names(forc) = res.summ.phylo$Slopes[,6]
obj.force<-contMap(phylo,forc,plot=FALSE)
obj.force<-setMap(obj.force,invert=TRUE)
obj.force<-setMap(obj.force,colors=c("yellow","darkcyan","purple"))
#plot(obj.force,fsize=c(0.5,1),outline=FALSE,lwd=c(3,7),leg.txt="forcing")

## chilling
chill = res.summ.phylo$Slopes[,8]
names(chill) = res.summ.phylo$Slopes[,6]
obj.chill<-contMap(phylo,chill,plot=FALSE)
obj.chill<-setMap(obj.chill,invert=TRUE)
obj.chill<-setMap(obj.chill,colors=c("yellow","darkcyan","purple"))
#plot(obj.chill,fsize=c(0.4,1),outline=FALSE,lwd=c(3,7),leg.txt="chilling")


## photoperiod
photo = res.summ.phylo$Slopes[,14]
names(photo)= res.summ.phylo$Slopes[,6]
obj.photo<-contMap(phylo,photo,plot=FALSE)
obj.photo<-setMap(obj.photo,invert=TRUE)
obj.photo<-setMap(obj.photo,colors=c("yellow","darkcyan","purple"))
#plot(obj.photo,fsize=c(0.4,1),outline=FALSE,lwd=c(3,7),leg.txt="chilling")


## plotting

## combined plot of three phylogenies and cues
#par(mfrow=c(2,3),mar=c(1.5,1.5,1,1))
par(mfrow=c(2,3))
par(mar=c(2,2,1,1))
layout(matrix(c(1,2,3,4,5,6,4,5,6), nrow = 3, ncol = 3, byrow = TRUE))

## plot hypotheses on upper row
d <- density(res.summ.phylo$lambdas$lambda.phylo.force$samples[,1])
plot(d,main="",xlab="",
     xlim=c(0,1),col="darkblue")
polygon(d, col=adjustcolor("darkblue",0.4), border="darkblue")
abline(v=mean(res.summ.phylo$lambdas$lambda.phylo.force$samples[,1]),lty=2,col="blue")

d <- density(res.summ.phylo$lambdas$lambda.phylo.chill$samples[,1])
plot(d,main="",xlab="",
     xlim=c(0,1),col="darkblue")
polygon(d, col=adjustcolor("darkblue",0.4), border="darkblue")
abline(v=mean(res.summ.phylo$lambdas$lambda.phylo.chill$samples[,1]),lty=2,col="blue")

d <- density(res.summ.phylo$lambdas$lambda.phylo.photo$samples[,1])
plot(d,main="",xlab="",
     xlim=c(0,1),col="darkblue")
polygon(d, col=adjustcolor("darkblue",0.4), border="darkblue")
abline(v=mean(res.summ.phylo$lambdas$lambda.phylo.photo$samples[,1]),lty=2,col="blue")


plot.contMap(obj.force,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title(paste("Forcing (H² = ",round(res.summ.phylo$lambdas$lambda.phylo.force$hypothesis$Estimate,3),")",sep=""),xpd = T)
plot.contMap(obj.chill,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title(paste("Chilling (H² = ",round(res.summ.phylo$lambdas$lambda.phylo.chill$hypothesis$Estimate,3),")",sep=""),xpd = T)
plot.contMap(obj.photo,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title(paste("Photo (H² = ",round(res.summ.phylo$lambdas$lambda.phylo.photo$hypothesis$Estimate,3),")",sep=""),xpd = T)

}


plot1 <- plottingphylosig(res.summ.phylo=result.all235.mod2,phylo=phylo.all235)
plot2 <- plottingphylosig(res.summ.phylo=result.all235.mod3,phylo=phylo.all235)
plot3 <- plottingphylosig(res.summ.phylo=result.angio216.mod2,phylo=phylo.angio216)
plot4 <- plottingphylosig(res.summ.phylo=result.angio216.mod3,phylo=phylo.angio216)
plot5 <- plottingphylosig(res.summ.phylo=result.allnocrop214.mod2,phylo=phylo.allnocrop214)
plot6 <- plottingphylosig(res.summ.phylo=result.allnocrop214.mod3,phylo=phylo.allnocrop214)
plot7 <- plottingphylosig(res.summ.phylo=result.angionocrop195.mod2,phylo=phylo.angionocrop195)
plot8 <- plottingphylosig(res.summ.phylo=result.angionocrop195.mod3,phylo=phylo.angionocrop195)

dev.off()



###############################################
#### plot variation in model coefficients  ####
###############################################

par(mfrow=c(3,3),mar=c(4,4,1,1))
listmods<-list(result.all235.mod2,result.all235.mod3,
               result.angio216.mod2,result.angio216.mod3,
               result.allnocrop214.mod2,result.allnocrop214.mod3,
               result.angionocrop195.mod2,result.angionocrop195.mod3)


for(j in c(1,5,7)){
  print(j)
plot(listmods[[j]]$Slopes[,c(2)],listmods[[j+1]]$Slopes[,c(2)],
     xlab="forcing sens modA (days/C)",ylab="forcing sens modB (days/C)",
     pch=19,col=adjustcolor("grey",0.4))
abline(a=0,b=1,col="darkgrey")
abline(lm(listmods[[j+1]]$Slopes[,c(2)]~listmods[[j]]$Slopes[,c(2)]))
text(-9,20,paste(nrow(listmods[[j]]$Slopes),"spp"))

plot(listmods[[j]]$Slopes[,c(8)],listmods[[j+1]]$Slopes[,c(8)],
     xlab="chilling sens modA (days/C)",ylab="chilling sens modB (days/C)",
     pch=19,col=adjustcolor("grey",0.4))
abline(a=0,b=1,col="darkgrey")
abline(lm(listmods[[j+1]]$Slopes[,c(8)]~listmods[[j]]$Slopes[,c(8)]))

plot(listmods[[j]]$Slopes[,c(14)],listmods[[j+1]]$Slopes[,c(14)],
     xlab="photo sens modA (days/C)",ylab="photo sens modB (days/C)",
     pch=19,col=adjustcolor("grey",0.4))
abline(a=0,b=1,col="darkgrey")
abline(lm(listmods[[j+1]]$Slopes[,c(14)]~listmods[[j]]$Slopes[,c(14)]))

}


par(mfrow=c(3,3),mar=c(4,4,1,1))
listmods<-list(result.all235.mod2,result.all235.mod3,
               result.angio216.mod2,result.angio216.mod3,
               result.allnocrop214.mod2,result.allnocrop214.mod3,
               result.angionocrop195.mod2,result.angionocrop195.mod3)

par(mfrow=c(3,6),mar=c(4,4,1,1))
for(j in c(1,2,5:8)){
  print(j)
  plot(listmods[[j]]$Slopes[,c(2)],listmods[[j]]$Slopes[,c(8)],
       xlab="forcing sens (days/C)",ylab="chilling sens (days/C)",
       pch=19,col=adjustcolor("grey",0.4))
  abline(lm(listmods[[j]]$Slopes[,c(8)]~listmods[[j]]$Slopes[,c(2)]))
  
  if(j %in% c(1,5,7)){
  text(0,-80,paste(nrow(listmods[[j]]$Slopes),"spp","ModA"))
  } else {
  text(0,-80,paste(nrow(listmods[[j]]$Slopes),"spp","ModB"))
  }
  plot(listmods[[j]]$Slopes[,c(8)],listmods[[j]]$Slopes[,c(14)],
       xlab="chilling sens (days/C)",ylab="photo sens (days/C)",
       pch=19,col=adjustcolor("grey",0.4))
  abline(lm(listmods[[j]]$Slopes[,c(14)]~listmods[[j]]$Slopes[,c(8)]))
  
  plot(listmods[[j]]$Slopes[,c(2)],listmods[[j]]$Slopes[,c(14)],
       xlab="forcing sens (days/C)",ylab="photo sens (days/C)",
       pch=19,col=adjustcolor("grey",0.4))
  abline(lm(listmods[[j]]$Slopes[,c(14)]~listmods[[j]]$Slopes[,c(2)]))
  
}



##################################################
#### model MCMCglmm collapsing at genus ####
##################################################


## load phylogeny (collapse it to genus level)
phylo.all235

phylo <- phylo.all235
dat <- mod2_sp_int_slope$data
dat$animal <- unlist(lapply(strsplit(dat$spps,"\\_"),function(x)x[1]))
dat.genus<-dat
dat.species<-dat

dat.species$animal<-dat.species$spps


#get.genus level phylogeny
genus.phylo<-phylo
genus.list <- unlist(lapply(strsplit(genus.phylo$tip.label,"\\_"),function(x)x[1]))
genus.phylo$tip.label <- genus.list 
genus.phylo <- drop.tip(genus.phylo,which(duplicated(genus.list)))
genus.phylo$tip.label

dev.off()
plot(genus.phylo,cex=.7)
## load data

# # Parameters for troubleshooting
nspecies=length(unique(dat$animal))



# Obtain phylogenetic correlation structure (matrix)
spetree <- genus.phylo
inter.mat <- vcv(spetree,corr=TRUE)

dat$ind.name<-NA
popstruct <- list()
popnames <- list()
for(i in 1:nspecies){#i=1
  genus.i <- genus.phylo$tip.label[i]
  
  ninds.i <- table(dat$animal)[genus.i]
  
  matempty <- matrix(rep(0,ninds.i^2),ncol=ninds.i,nrow=ninds.i,)
  diag(matempty) <- rep(1, ninds.i)
  popstruct[[i]] <- matempty
  popnames[[i]] <- paste(rep(genus.i,ninds.i),".",seq(1,ninds.i,1),sep="")
  dat$ind.name[which(dat$animal==genus.i)]<-popnames[[i]]
}



# Make a block diagonal matrix representing the correlation structure of all species
mat.names <- unlist(popnames)
intra.mat <- matrix(bdiag(lapply(seq(1,length(popstruct)),function(c) 
  popstruct[[c]])),nrow=length(mat.names),dimnames=list(mat.names,mat.names))



###
# Prepare infraspecific correlation matrix for MCMCglmm
#
# Single value decomposition of the intraspecific structure
#   -> follow code of Stone et al. 2012
intra.svd <- svd(intra.mat)
intra.svd <- intra.svd$v %*% (t(intra.svd$u) * sqrt(intra.svd$d))
rownames(intra.svd) <- colnames(intra.svd) <- rownames(intra.mat)
# The following is important and MCMCglmm searches the variables in the 
# global environment
dat <<- dat
intra.svd <<- intra.svd

###
# Run the MCMCpglmm analyses with different correlation structures.
# For each, we will use a very diffuse prior.

# Model M.1 -> only phylogenetic random effects
priorpr.m1 <- list(R = list(V = 1, nu = 0.002), 
                   G = list(G1 = list(V = 1, nu = 0.002)))
M.1 <- MCMCglmm(resp ~ force.z + chill.z + photo.z, 
                random = ~ animal, pedigree = spetree,
                data=dat.genus, scale=TRUE,prior=priorpr.m1,
                nitt=21000,thin=10,burnin=1000,verbose=FALSE)

M.2 <- MCMCglmm(resp ~ force.z + chill.z + photo.z, 
                random = ~ animal, pedigree = phylo.all235,
                data=dat.species, scale=TRUE,prior=priorpr.m1,
                nitt=21000,thin=10,burnin=1000,verbose=FALSE)

# Model summaries
M.1.sum <- summary(M.1)
M.2.sum <- summary(M.2)

sums<-rbind(M.1.sum$solutions,M.2.sum$solutions)
write.csv(sums,file = "~/GitHub/ospree/analyses/phylogeny/output/MCMCglmm_genus.vs.sps.csv")


# Data frame
res <- data.frame(models=c("M.1","M.2","M.3","gls0","gls1","gls2","gls3"),
                  #random.effects=c("NA","inter","infra","inter+infra","ols","glsInter","glsIntra","glsInterIntra"),
                  DIC=c(M.0$DIC,M.1$DIC,M.2$DIC,M.3$DIC,-999,-999,-999,-999),
                  slope=c(M.0.sum$solutions[2,1],M.1.sum$solutions[2,1],
                          M.2.sum$solutions[2,1],M.3.sum$solutions[2,1],
                          gls.mod0$coefficients[2],gls.mod1$coefficients[2],
                          gls.mod2$coefficients[2],gls.mod3$coefficients[2]),
                  slope.sd=c(sd(M.0$Sol[,'x']),sd(M.1$Sol[,'x']),
                             sd(M.2$Sol[,'x']),sd(M.3$Sol[,'x']),
                             gls.mod0.sum$tTable[2,2],
                             gls.mod1.sum$tTable[2,2],
                             gls.mod2.sum$tTable[2,2],
                             gls.mod3.sum$tTable[2,2]),
                  slope.signif=c(ifelse(M.0.sum$solution[2,5]<0.05,1,0),
                                 ifelse(M.1.sum$solution[2,5]<0.05,1,0),
                                 ifelse(M.2.sum$solution[2,5]<0.05,1,0),
                                 ifelse(M.3.sum$solution[2,5]<0.05,1,0),
                                 ifelse(gls.mod0.sum$tTable[2,4]<0.05,1,0),
                                 ifelse(gls.mod1.sum$tTable[2,4]<0.05,1,0),
                                 ifelse(gls.mod2.sum$tTable[2,4]<0.05,1,0),
                                 ifelse(gls.mod3.sum$tTable[2,4]<0.05,1,0)))


## NEXT STEPS






