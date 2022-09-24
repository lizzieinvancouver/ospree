## Started 9 September 2021 ##
## By Nacho, copies Nacho's Phylo_ospree_reanalyses.R code ##

## But edits by Deirdre to be used for testing new stan model that should run faster. ##
# not that the R code doesn't change, just stan

# 
rm(list=ls())
options(stringsAsFactors = FALSE)
rstan_options(auto_write = TRUE)

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
library(pez)
library(rstan)
library(phytools)
library(plyr)
library(dplyr)

options(mc.cores = parallel::detectCores())


#'######################################
#### get data through bbstanleadin ####
#'######################################

# Flags to choose for bbstanleadin.R #
setwd("..//bb_analysis") 

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- FALSE
use.flags.for.allsppmodel <- TRUE
use.yourown.flagdesign <- FALSE
nocrops <- TRUE
agiosponly <- TRUE

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

namesdat <- unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
bb.stan$spps <- paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$phylo <- paste(bb.stan$genus,bb.stan$species,sep="_")


#'###################################
#### get phylogeny              ####
#'###################################

setwd("..//phylogeny") 
source("source/get_phylo_models.R")

## read and pre-process phylogeny
#phylo <- read.tree("../../data/phylogeny/SBphylo_62complex.tre")
#phylo <- read.tree("../../data/phylogeny/SBphylo_101sps.tre")
phylo <- phy.plants.ospree


namesphy <- phylo$tip.label
phylo <- force.ultrametric(phylo, method="extend")
phylo$node.label <- seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)
#plot(phylo, cex=0.7)
VCVPHY <- vcv.phylo(phylo,corr=TRUE)



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


# Step 1: Get spps and VCVPHY in same order
# bb.stan$spps[phylo$tip.label]
phylo$tip.label
d <- bb.stan[match(phylo$tip.label, bb.stan$spps),] # hmmm, only gives ONE match

phymatch <- data.frame(tip=phylo$tip.label, sppnum=c(1:length(phylo$tip.label)))
d <- merge(bb.stan, phymatch, by.x="spps", by.y="tip")
d <- d[order(d$sppnum),]
# Tilia_cordata versus Tilia_Cordata in phylo
nspecies <- max(d$sppnum)



## remove outliers
d$resp
head(d)
ff = subset(d,latbi %in% c("Populus_balsamifera","Populus_tremuloides"))
d = subset(d,!latbi %in% c("Populus_balsamifera","Populus_tremuloides"))
nspecies = 192
phylo <- drop.tip(phylo, c("Populus_balsamifera","Populus_tremuloides"))
d$sppnum <- as.numeric(as.factor(d$sppnum))





# Step 2: Run some version of the model 

# This model uses Geoff's new version of the stan code
# but it take ages to run, argh!


## Two slope, one intercept model
### Set priors
phypriors <- list(
  a_z_prior_mu = 30,
  a_z_prior_sigma = 10,
  lam_interceptsa_prior_alpha = 1, # 
  lam_interceptsa_prior_beta = 1, # 
  sigma_interceptsa_prior_mu = 40,
  sigma_interceptsa_prior_sigma = 20,
  b_zf_prior_mu = -4,
  b_zf_prior_sigma = 5,
  lam_interceptsbf_prior_alpha = 1, #
  lam_interceptsbf_prior_beta = 1, # 
  sigma_interceptsbf_prior_mu = 5,
  sigma_interceptsbf_prior_sigma = 5,
  b_zc_prior_mu = -8,
  b_zc_prior_sigma = 5,
  lam_interceptsbc_prior_alpha = 1, #
  lam_interceptsbc_prior_beta = 1, # 
  sigma_interceptsbc_prior_mu = 5,
  sigma_interceptsbc_prior_sigma = 5,
  b_zp_prior_mu = -3,
  b_zp_prior_sigma = 5,
  lam_interceptsbp_prior_alpha = 1, #
  lam_interceptsbp_prior_beta = 1, # 
  sigma_interceptsbp_prior_mu = 5,
  sigma_interceptsbp_prior_sigma = 5,
  sigma_y_mu_prior = 20,
  sigma_y_mu_sigma = 1)

# Function for generating "good" initial values
simu_inits <- function(chain_id) {
  a_z.temp <- rnorm(n = nspecies, mean = phypriors[["a_z_prior_mu"]], sd = phypriors[["a_z_prior_sigma"]])
  b_zf.temp <- rnorm(n = nspecies, mean = phypriors[["b_zf_prior_mu"]], sd = phypriors[["b_zf_prior_sigma"]])
  b_zc.temp <- rnorm(n = nspecies, mean = phypriors[["b_zc_prior_mu"]], sd = phypriors[["b_zc_prior_sigma"]])
  b_zp.temp <- rnorm(n = nspecies, mean = phypriors[["b_zp_prior_mu"]], sd = phypriors[["b_zp_prior_sigma"]])
  return(append(list(a = a_z.temp,
                     b_force = b_zf.temp,
                     b_chill = b_zc.temp,
                     b_photo = b_zp.temp),
                phypriors))
}

## Lizzie also tried to adapt a speedier PMM that Mike Betancourt sent ...
# ... a litte faster I think, but likely not critical to use
if(FALSE){
totallynew <- stan("stan/betan_threeslopeintercept_cp.stan",
               data=list(N=nrow(d),
                                n_sp=nspecies,
                                sp=d$sppnum,
                                x1=d$force.z,
                                x2 = d$chill.z,
                                x3=d$photo.z,
                                y=d$resp,
                                Vphy=vcv(phylo, corr = TRUE)),
               init = simu_inits,
               iter = 2000,
               warmup = 1000,
               chains = 4,
               seed = 117 
               )
}

## Fit model here ... using code that Lizzie updated priors for
# It ran for Lizzie in well under and hour on her laptop
testme <- stan("stan/uber_threeslopeintercept_modified_cholesky_updatedpriors.stan",
               data=list(N=nrow(d),
                                n_sp=nspecies,
                                sp=d$sppnum,
                                x1=d$force.z,
                                x2 = d$chill.z,
                                x3=d$photo.z,
                                y=d$resp,
                                Vphy=vcv(phylo, corr = TRUE)),
               init = simu_inits,
               iter = 2000,
               warmup = 1000, # half the iter as warmp is default FYI
               chains = 4,
               seed = 117 
)

## Save fitted posterior
saveRDS(testme, "output/testme_deirdre_noout.rds")

## Summarize full fit
summary(testme)$summary

## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
summary(testme, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary

## Compare prior and posterior
# Plotting f(x) from Mike Betancourt
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

plot_marginal <- function(values, name, display_xlims, title="") {
  bin_lims <- range(values)
  delta <- diff(bin_lims) / 50
  breaks <- seq(bin_lims[1], bin_lims[2] + delta, delta)
  
  hist(values, breaks=breaks, prob=T,
       main=title, xlab=name, xlim=display_xlims,
       ylab="", yaxt='n',
       col=c_dark, border=c_dark_highlight)
}

post_samples <- extract(testme)
plot_marginal(post_samples$b_zc, "b_zc", c(-20, 2))

# Prior for b_zc us normal(-2, 10) so I think the below works ... 
yhere <- dnorm(x=seq(-20, 2, by=0.01), mean=-2, sd=10)
points(x=seq(-20, 2, by=0.01), yhere, 
       col="darkblue", type="l", lwd=2, yaxt="n")

plot_marginal(post_samples$lam_interceptsbc, "Lambda for chilling", c(-0.1, 1.1))
yhere <- dbeta(x=seq(-0.1, 1.1, by=0.001), 1, 1)
points(x=seq(-0.1, 1.1, by=0.001), yhere, 
       col="darkblue", type="l", lwd=2, yaxt="n")
# We could, err, functionalize this or clean up in many ways ... but it's a start!
## END compare prior and posterior



# Reinstating some useful plotting code
# See also https://github.com/lizzieinvancouver/pmm/blob/5014539f8a7cfc659298d20d49a0935a8ced305d/analyses/phlyo_opsree_compact.R
source("source/stan_utility.R")
fit <- readRDS("output/testme.rds")
check_all_diagnostics(fit)

names(fit)[grep(pattern = "^a\\[", x = names(fit))] <- phylo$tip.label
names(fit)[grep(pattern = "^b_force", x = names(fit))] <- phylo$tip.label
names(fit)[grep(pattern = "^b_chill", x = names(fit))] <- phylo$tip.label
names(fit)[grep(pattern = "^b_photo", x = names(fit))] <- phylo$tip.label

pdf(file = "output/estimates1.pdf", onefile = TRUE, height = 35, width = 6)
plot(fit, pars = c("a_z", "a"))
plot(fit, pars = c("b_zf", "b_force"))
plot(fit, pars = c("b_zc", "b_chill"))
plot(fit, pars = c("b_zp", "b_photo"))
dev.off()
pdf(file = "output/estimates2.pdf", onefile = TRUE, height = 11.5, width = 8)
plot(fit, pars = c("lam_interceptsa", "lam_interceptsbf", "lam_interceptsbc", "lam_interceptsbp"))
plot(fit, pars = c("sigma_interceptsa", "sigma_interceptsbf", "sigma_interceptsbc", "sigma_interceptsbp", "sigma_y"))
dev.off()





source("source/bb_muplotphylo.R")
modelhere <- testme3
figpath <- "figures"
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
