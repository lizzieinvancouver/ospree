## Started 28 July 2026 ##
## By Lizzie, at the start I copy...
## phylo_ospree_compact4betan.R which copies Nacho's Phylo_ospree_reanalyses.R code...
## They're probably identical through there ##

# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
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

###############
# END of copying code (right before 'get phylogeny')
###############

fagsyl <- subset(bb.stan, latbi=="Fagus_sylvatica")
betpen <- subset(bb.stan, species=="pendula")

library(ggplot2)

unique(fagsyl$datasetID)

ggplot(fagsyl, aes(x=photo, y= resp, color=chill)) +
geom_point()+
facet_wrap(.~datasetID)

unique(betpen$latbi)
unique(betpen$datasetID)

ggplot(betpen, aes(x=photo, y= resp, color=chill)) +
geom_point()+
facet_wrap(.~datasetID)

if(FALSE){
  # Below run slow and with divergent transitions; I should cut soon. 
library(rstanarm)

fsmod <- stan_lmer(resp~photo+force+chill+1|datasetID, data=fagsyl)
bpmod <- stan_lmer(resp~photo+force+chill+1|datasetID, data=betpen)

fsmodsimple <- stan_glm(resp~photo+force+chill, data=fagsyl)
bpmodsimple <- stan_glm(resp~photo+force+chill, data=betpen)
}

##
library(rstan)
d <- fagsyl
fsmod <- stan("..//misc/photoperiodlooksee/threeslopeswstudy.stan",
               data=list(N=nrow(d),
                                n_study=length(unique(d$datasetID)),
                                study=as.numeric(as.factor(d$datasetID)),
                                force=d$force.z,
                                chill = d$chill.z,
                                photo=d$photo.z,
                                y=d$resp),
               iter = 2000,
               warmup = 1000,
               chains = 4
               )

summary(fsmod, pars = list("mu_a", "sigma_a_study", "b_force", "b_photo", "b_chill", "sigma_y"))$summary


fsmodsimple <- stan("..//misc/photoperiodlooksee/threeslope.stan",
               data=list(N=nrow(d),
                                force=d$force.z,
                                chill = d$chill.z,
                                photo=d$photo.z,
                                y=d$resp),
               iter = 2000,
               warmup = 1000,
               chains = 4
               )

summary(fsmodsimple, pars = list("a", "b_force", "b_photo", "b_chill", "sigma_y"))$summary
