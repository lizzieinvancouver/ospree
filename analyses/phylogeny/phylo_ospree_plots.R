## Started 9 September 2021 ##
## By Nacho, copies Nacho's Phylo_ospree_reanalyses.R code ##

## But edits it to be used for testing new stan models. ##

## This version has new attempts to run the code by Nacho:
## - tweaking priors
## - trying uncentered variables
## - adding chilling

#### remove objects (activate if needed)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden ####
rm(list=ls())
options(stringsAsFactors = FALSE)
rstan_options(auto_write = TRUE)/budreview/ospree/analyses/phylogeny") 
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
agiosponly <- T
gymnosonly <- F

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
  croplist <- read.csv("../../data/croplist/agricultural_species.csv")
  cropgymno <- c(gymno)
  bb.stan$crops <- ifelse(bb.stan$spps %in% cropgymno, "cropgymno","nocrop")
  cropspps <- unique(bb.stan$spps[which(bb.stan$crops=="cropgymno")])
  bb.stan <- subset(bb.stan, crops == "nocrop")
  phylo <- drop.tip(phylo, cropspps)
  VCVPHY<-vcv.phylo(phylo,corr=T)
} 

if(nocrops & gymnosonly){
  gymno <- c("Metasequoia_glyptostroboides",  "Pseudotsuga_menziesii","Larix_laricina",
             "Larix_gmelinii", "Larix_decidua" ,"Larix_kaempferi",   
             "Pinus_nigra","Pinus_sylvestris","Pinus_banksiana",  
             "Pinus_contorta","Pinus_wallichiana","Pinus_strobus", 
             "Picea_abies"   ,"Picea_mariana" ,"Picea_glauca" ,
             "Cedrus_libani" ,"Abies_alba"    ,"Abies_homolepis","Ginkgo_biloba")
  croplist <- read.csv("../../data/croplist/agricultural_species.csv")
  angio <- unique(bb.stan$spps)[which(!unique(bb.stan$spps)%in%gymno)]
  cropangio <- unique(c(angio,croplist$Species_name))
  bb.stan <- subset(bb.stan, ! spps %in% cropangio)
  dropforgymno <- which(!phylo$tip.label %in% gymno)
  phylo <- drop.tip(phylo, dropforgymno)
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


#### remove outliers ####
d$resp
head(d)
ff = subset(d,latbi %in% c("Populus_balsamifera","Populus_tremuloides"))
d = subset(d,!latbi %in% c("Populus_balsamifera","Populus_tremuloides"))
nspecies = 192
phylo <- drop.tip(phylo, c("Populus_balsamifera","Populus_tremuloides"))
d$sppnum <- as.numeric(as.factor(d$sppnum))


## remove names of species that are wrong (e.g. Acer pseudolatanus) Malyshev2018
idswrong = which(d$spps == "Acer_pseudolatauns")
d$spps[idswrong] = "Acer_pseudoplatanus"
d$species[idswrong] = "pseudoplatanus"
d$latbi[idswrong] = "Acer_pseudoplatanus"
d$phylo[idswrong] = "Acer_pseudoplatanus"

#d$sppnum[which(d$latbi=="Acer_pseudoplatanus")]
d$sppnum[idswrong] = 127
d$sppnum[which(d$sppnum>137)] = d$sppnum[which(d$sppnum>137)]-1

nspecies = 191
phylo <- drop.tip(phylo, "Acer_pseudolatauns")



## get ailene´s model table
#source("../../analyses/bb_analysis/maketables.forsupp/mod_table.R")



#'###################################
#### diagnose and make plots     ####
#'###################################
lambdazero <- F
#lambdazero <- T
agiosponly <- T
#agiosponly <- F

## load model

if(agiosponly & lambdazero){
  fit <- readRDS("output/fit_priorupdate_noout_angio191_lamb0.rds")
} else {
  fit <- readRDS("output/testme_lambdaest_nooutlier.rds")
}



if(agiosponly & !lambdazero){
    fit <- readRDS("output/fit_priorupdate_noout_angio191.rds")
} else {
  fit <- readRDS("output/testme_priorupdate_noout_gymno.rds")
}


## Summarize full fit
summary(fit)$summary

## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
tableresults = summary(fit, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults = summary(fit, pars = list("a_z", #"lam_interceptsa", 
                                        "sigma_interceptsa", "b_zf", 
                                        #"lam_interceptsbf", 
                                        "sigma_interceptsbf", "b_zc", 
                                        #"lam_interceptsbc", 
                                        "sigma_interceptsbc", "b_zp", 
                                        #"lam_interceptsbp", 
                                        "sigma_interceptsbp", "sigma_y"))$summary

#write.csv(tableresults[c(1,4,7,10,2,5,8,11,3,6,9,12,13),c(1,3,4,6,8:10)], file = "output/angio_noout_lambest191.csv")
#write.csv(tableresults[c(1,3,5,7,2,4,6,8,9),c(1,3,4,6,8:10)], file = "output/angio_noout_lamb0_191.csv")



## Diagnose model
source("source/stan_utility.R") ## to get diagnostics
check_all_diagnostics(fit)

## rename model to include species names

names(fit)[grep(pattern = "^a\\[", x = names(fit))] <- phylo$tip.label
names(fit)[grep(pattern = "^b_force", x = names(fit))] <- phylo$tip.label
names(fit)[grep(pattern = "^b_chill", x = names(fit))] <- phylo$tip.label
names(fit)[grep(pattern = "^b_photo", x = names(fit))] <- phylo$tip.label


## default plots by predictor
#pdf(file = "output/estimates1.pdf", onefile = TRUE, height = 35, width = 6)
#plot(fit, pars = c("a_z", "a"))
#plot(fit, pars = c("b_zf", "b_force"))
#plot(fit, pars = c("b_zc", "b_chill"))
#plot(fit, pars = c("b_zp", "b_photo"))
#dev.off()
#pdf(file = "output/estimates2.pdf", onefile = TRUE, height = 11.5, width = 8)
#plot(fit, pars = c("lam_interceptsa", "lam_interceptsbf", "lam_interceptsbc", "lam_interceptsbp"))
#plot(fit, pars = c("sigma_interceptsa", "sigma_interceptsbf", "sigma_interceptsbc", "sigma_interceptsbp", "sigma_y"))
#dev.off()




## mu plots all predictors and species clumped

source("source/bb_muplotphylo.R")
modelhere <- fit

library(RColorBrewer)
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 17)
my.pch <- rep(16, each=192)
alphahere = 0.4


names(fit)
row.names(summary(modelhere)$summary)

if(agiosponly & !lambdazero){
  
  posspsindata <- list(10:200,202:392,394:584)
  
} else {

  posspsindata <- list(6:196,198:388,390:580)
  

}

set.seed(117)
muplotfx_phylo(modelhere, "", 7, 8, c(0,3), c(-25, 15) , 18, 3.2, posspsindata)


#### get model estimates per species ####

## forcing
cueforce = summary(modelhere)$summary[posspsindata[[1]],"mean"]

## chill
cuechill = summary(modelhere)$summary[posspsindata[[2]],"mean"]

## photo
cuephoto = summary(modelhere)$summary[posspsindata[[3]],"mean"]


## forcing
cueforce_lam0 = summary(modelhere)$summary[posspsindata[[1]],"mean"]

## chill
cuechill_lam0 = summary(modelhere)$summary[posspsindata[[2]],"mean"]

## photo
cuephoto_lam0 = summary(modelhere)$summary[posspsindata[[3]],"mean"]



#'###########################################
#### combining mu plots with phylogenetic structuring ####
#'###########################################


dev.off()
if(agiosponly){
## for forcing
set.seed(117)
muplotfx_phylo_contmap(modelhere, "", 7, 8, 
                       c(0,191), c(-30, 5) , 18, 3.2, 
                       posspsindata,1,14)

## for chilling
set.seed(117)
muplotfx_phylo_contmap(modelhere, "", 7, 8, 
                       c(0,191), c(-30, 5) , 18, 3.2, 
                       posspsindata,2,14)

## for photoperiod
set.seed(117)
muplotfx_phylo_contmap(modelhere, "", 7, 8, 
                       c(0,191), c(-30, 5) , 18, 3.2, 
                       posspsindata,3,14)
}

if(gymnosperm){
  ## for forcing
  muplotfx_phylo_contmap(modelhere, "", 7, 8, 
                         c(1,19), c(-20, 2) , 18, 3.2, 
                         posspsindata,1,12.2)
  
  ## for chilling
  muplotfx_phylo_contmap(modelhere, "", 7, 8, 
                         c(1,19), c(-23, 15) , 18, 3.2, 
                         posspsindata,2,21.2)
  
  ## for photoperiod
  muplotfx_phylo_contmap(modelhere, "", 7, 8, 
                         c(1,19), c(-7, 14) , 18, 3.2, 
                         posspsindata,3,11.7)
}



#'###########################################
#### check correlations among estimates ####
#'###########################################


par(mfrow=c(1,3))


plot(cuechill_lam0, cuechill, 
     xlab="sensitivity to chilling (lambda=0)",
     ylab="sensitivity to chilling", pch=16, col=adjustcolor(1,0.5),cex=1.2, cex.lab=1.5)
abline(a=0,b=1, col='grey')  
abline(lm(cuechill~cuechill_lam0))


plot(cueforce_lam0, cueforce, 
     xlab="sensitivity to forcing (lambda=0)",
     ylab="sensitivity to forcing", pch=16, col=adjustcolor(1,0.5),cex=1.2, cex.lab=1.5)
abline(a=0,b=1, col='grey')  
abline(lm(cueforce~cueforce_lam0))



plot(cuephoto_lam0, cuephoto, 
     xlab="sensitivity to photo (lambda=0)",
     ylab="sensitivity to photo", pch=16, col=adjustcolor(1,0.5),cex=1.2, cex.lab=1.5)
abline(a=0,b=1, col='grey')  
abline(lm(cuephoto~cuephoto_lam0))


#'###############################################
#### comparing estimates lambda est vs 1 vs 0 ####
#'###############################################


## load models
agiosponly=T
if(agiosponly){
  fitlam0 <- readRDS("output/testme_priorupdate_noout_angio191_lam0.rds")
  fitlam1 <- readRDS("output/testme_priorupdate_noout_angio191_lam1.rds")
  fitlambest <- readRDS("output/testme_priorupdate_noout_angio191.rds")
} else {
  fitlam0 <- readRDS("output/testme_gymno_lambda0.rds")
  fitlam1 <- readRDS("output/testme_gymno_lambda1.rds")
  fitlambest <- readRDS("output/testmegymno.rds")
}



## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
tableresults.0 = summary(fitlam0, pars = list("a_z", "sigma_interceptsa", "b_zf", "sigma_interceptsbf", "b_zc", "sigma_interceptsbc", "b_zp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults.1 = summary(fitlam1, pars = list("a_z", "sigma_interceptsa", "b_zf", "sigma_interceptsbf", "b_zc", "sigma_interceptsbc", "b_zp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults.est = summary(fitlambest, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary

#write.csv(tableresults[c(1,4,7,10,2,5,8,11,3,6,9,12,13),], file = "output/angio_noout_lamest191.csv")
#write.csv(tableresults[c(1,4,7,10,2,5,8,11,3,6,9,12,13),], file = "output/gymno_noout_lamest.csv")



## rename model to include species names
names(fitlambest)[grep(pattern = "^a\\[", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_force", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_chill", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_photo", x = names(fitlambest))] <- phylo$tip.label

names(fitlam0)[grep(pattern = "^a\\[", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_force", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_chill", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_photo", x = names(fitlam0))] <- phylo$tip.label

names(fitlam1)[grep(pattern = "^a\\[", x = names(fitlam1))] <- phylo$tip.label
names(fitlam1)[grep(pattern = "^b_force", x = names(fitlam1))] <- phylo$tip.label
names(fitlam1)[grep(pattern = "^b_chill", x = names(fitlam1))] <- phylo$tip.label
names(fitlam1)[grep(pattern = "^b_photo", x = names(fitlam1))] <- phylo$tip.label



# get model estimates per species ----

## where species are

if(agiosponly){
posspsindata.est <- list(10:200,202:392,394:584,586:776)
posspsindata.01 <- list(6:196,198:388,389:579,581:771)
} else {
posspsindata.est <- list(10:28,30:48,50:68,70:88)
posspsindata.01 <- list(6:24,26:44,46:64,66:84)
}


## forcing
cueforce = summary(fitlambest)$summary[posspsindata.est[[1]],"mean"]
cueforcesdup = summary(fitlambest)$summary[posspsindata.est[[1]],"75%"]
cueforcesdlow = summary(fitlambest)$summary[posspsindata.est[[1]],"25%"]

cueforce0 = summary(fitlam0)$summary[posspsindata.01[[1]],"mean"]
cueforcesdup0 = summary(fitlam0)$summary[posspsindata.01[[1]],"75%"]
cueforcesdlow0 = summary(fitlam0)$summary[posspsindata.01[[1]],"25%"]

cueforce1 = summary(fitlam1)$summary[posspsindata.01[[1]],"mean"]
cueforcesdup1 = summary(fitlam1)$summary[posspsindata.01[[1]],"75%"]
cueforcesdlow1 = summary(fitlam1)$summary[posspsindata.01[[1]],"25%"]


## chill
cuechill = summary(fitlambest)$summary[posspsindata.est[[2]],"mean"]
cuechillsdup = summary(fitlambest)$summary[posspsindata.est[[2]],"75%"]
cuechillsdlow = summary(fitlambest)$summary[posspsindata.est[[2]],"25%"]

cuechill0 = summary(fitlam0)$summary[posspsindata.01[[2]],"mean"]
cuechillsdup0 = summary(fitlam0)$summary[posspsindata.01[[2]],"75%"]
cuechillsdlow0 = summary(fitlam0)$summary[posspsindata.01[[2]],"25%"]

cuechill1 = summary(fitlam1)$summary[posspsindata.01[[2]],"mean"]
cuechillsdup1 = summary(fitlam1)$summary[posspsindata.01[[2]],"75%"]
cuechillsdlow1 = summary(fitlam1)$summary[posspsindata.01[[2]],"25%"]

## photo
cuephoto = summary(fitlambest)$summary[posspsindata.est[[3]],"mean"]
cuephotosdup = summary(fitlambest)$summary[posspsindata.est[[3]],"75%"]
cuephotosdlow = summary(fitlambest)$summary[posspsindata.est[[3]],"25%"]

cuephoto0 = summary(fitlam0)$summary[posspsindata.01[[3]],"mean"]
cuephotosdup0 = summary(fitlam0)$summary[posspsindata.01[[3]],"75%"]
cuephotosdlow0 = summary(fitlam0)$summary[posspsindata.01[[3]],"25%"]

cuephoto1 = summary(fitlam1)$summary[posspsindata.01[[3]],"mean"]
cuephotosdup1 = summary(fitlam1)$summary[posspsindata.01[[3]],"75%"]
cuephotosdlow1 = summary(fitlam1)$summary[posspsindata.01[[3]],"25%"]

## alpha
cuealpha = summary(fitlambest)$summary[posspsindata.est[[4]],"mean"]
cuealphasdup = summary(fitlambest)$summary[posspsindata.est[[4]],"75%"]
cuealphasdlow = summary(fitlambest)$summary[posspsindata.est[[4]],"25%"]

cuealpha0 = summary(fitlam0)$summary[posspsindata.01[[4]],"mean"]
cuealphasdup0 = summary(fitlam0)$summary[posspsindata.01[[4]],"75%"]
cuealphasdlow0 = summary(fitlam0)$summary[posspsindata.01[[4]],"25%"]

cuealpha1 = summary(fitlam1)$summary[posspsindata.01[[4]],"mean"]
cuealphasdup1 = summary(fitlam1)$summary[posspsindata.01[[4]],"75%"]
cuealphasdlow1 = summary(fitlam1)$summary[posspsindata.01[[4]],"25%"]


##### comparing uncertainty around individual species estimations----

par(mfrow=c(1,3))

indvarest = cuechillsdup - cuechillsdlow 
indvarlam0 = cuechillsdup0 - cuechillsdlow0
#indvarlam1 = cuechillsdup1 - cuechillsdlow1
boxplot(cbind(lambdaest=indvarest,lambda0=indvarlam0),
        ylab="Estimate uncertainty (days/std units of chilling)",outline=F,ylim=c(0,10))

indvarest = cueforcesdup - cueforcesdlow 
indvarlam0 = cueforcesdup0 - cueforcesdlow0
#indvarlam1 = cueforcesdup1 - cueforcesdlow1
boxplot(cbind(lambdaest=indvarest,lambda0=indvarlam0),
        ylab="Estimate uncertainty (days/std units of forcing)",outline=F,ylim=c(0,10))

indvarest = cuephotosdup - cuephotosdlow 
indvarlam0 = cuephotosdup0 - cuephotosdlow0
#indvarlam1 = cuephotosdup1 - cuephotosdlow1
boxplot(cbind(lambdaest=indvarest,lambda0=indvarlam0),
        ylab="Estimate uncertainty (days/std units of photoperiod)",outline=F,ylim=c(0,4))

indvarest = cuealphasdup - cuealphasdlow 
indvarlam0 = cuealphasdup0 - cuealphasdlow0
indvarlam1 = cuealphasdup1 - cuealphasdlow1
boxplot(cbind(lambdaest=indvarest,lambda0=indvarlam0,lambda1=indvarlam1),
        ylab="Estimate uncertainty (days/std units of photoperiod)")


mean(indvarest);mean(indvarlam0)
t.test(indvarest,indvarlam0)
t.test(indvarest,indvarlam1)

67*4

##### comparing overall variances across species responses ----

var(cueforce)
var(cueforce0)
var(cueforce1)

var(cuechill)
var(cuechill0)
var(cuechill1)

var(cuephoto)
var(cuephoto0)
var(cuephoto1)


##### comparing variances ----

var(cueforce)
var(cueforce0)
var(cueforce1)

var(cuechill)
var(cuechill0)
var(cuechill1)

var(cuephoto)
var(cuephoto0)
var(cuephoto1)


##### comparing bias in average estimating ----


hist(cueforce-cueforce0)
mean(cuechill-cuechill0)
mean(cuephoto-cuephoto0)

summary(lm(cueforce~cueforce0))$coefficients[2,1]-1
summary(lm(cuechill~cuechill0))$coefficients[2,1]-1
summary(lm(cuephoto~cuephoto0))$coefficients[2,1]-1





mean(cueforce-cueforce1)
mean(cuechill-cuechill1)
mean(cuephoto-cuephoto1)
summary(lm(cueforce~cueforce1))$coefficients[2,1]-1
summary(lm(cuechill~cuechill1))$coefficients[2,1]-1
summary(lm(cuephoto~cuephoto1))$coefficients[2,1]-1


var(cueforce1)

var(cuechill)
var(cuechill0)
var(cuechill1)

var(cuephoto1)

## identify which species/clades have greater differences
sort(cueforce-cueforce0)
sort(cuechill-cuechill0)

sort((cueforce-cueforce0)+(cuechill-cuechill0))
sort()
which(names(cueforce)=="Quercus_robur")

mean((cueforce-cueforce0)[165:176]+(cuechill-cuechill0)[165:176])
mean(abs(cueforce-cueforce0)[108:111]+abs(cuechill-cuechill0)[108:111])

mean(cueforce0[165:176])
mean(cueforce[165:176])

mean(cuechill0[165:176])
mean(cuechill[165:176])

mean(abs(cueforce0)[108:111])
mean(abs(cueforce)[108:111])

mean(abs(cuechill0)[108:111])
mean(abs(cuechill)[108:111])

mean(abs(cueforce-cueforce0)[108:111]+abs(cuechill-cuechill0)[108:111])


##### comparing correlations among sensitivities ----

cuechill=cuechill0
cueforce=cueforce0
cuephoto=cuephoto0

par(mfrow=c(1,3),mar=c(5,5,1,1))
plot(cuechill,cueforce, pch=16)
abline(lm(cueforce~cuechill))
chilquad = cuechill^2
modquad = lm(cueforce~cuechill+chilquad)
summary(lm(cueforce~cuechill))
sqrt(0.09)
summary(modquad)
ff=predict(modquad,newdata=data.frame(cuechill=cuechill,chilquad=chilquad))
preddat = as.data.frame(cbind(cuechill,ff))
preddat2 = preddat[order(preddat$cuechill),]
lines(preddat2$cuechill,preddat2$ff, col="red")
cor.test(cuechill,cueforce)


plot(cuephoto,cueforce, pch=16)
abline(lm(cueforce~cuephoto))
chilquad = cuephoto^2
modquad = lm(cueforce~cuephoto+chilquad)
summary(lm(cueforce~cuephoto))
sqrt(0.09)
summary(modquad)
ff=predict(modquad,newdata=data.frame(cuechill=cuephoto,chilquad=chilquad))
preddat = as.data.frame(cbind(cuephoto,ff))
preddat2 = preddat[order(preddat$cuechill),]
lines(preddat2$cuechill,preddat2$ff, col="red")
cor.test(cuephoto,cueforce)

plot(cuephoto,cuechill, pch=16)
abline(lm(cuechill~cuephoto))
chilquad = cuephoto^2
modquad = lm(cuechill~cuephoto+chilquad)
summary(lm(cuechill~cuephoto))
sqrt(0.09)
summary(modquad)
ff=predict(modquad,newdata=data.frame(cuechill=cuephoto,chilquad=chilquad))
preddat = as.data.frame(cbind(cuephoto,ff))
preddat2 = preddat[order(preddat$cuechill),]
lines(preddat2$cuechill,preddat2$ff, col="red")
cor.test(cuephoto,cuechill)

plot(cuealpha,cuealpha0)
abline(a=0,b=1)
summary(lm(cuealpha~cuealpha0))

### plot correlations angio ----
dev.off()
par(mfrow=c(1,3))



plot(cuechill0, cuechill, 
     xlab="sensitivity to chilling (lambda=0)",
     ylab="sensitivity to chilling", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-30,5),ylim=c(-30,5))
for(i in 1:length(cueforce0)){
  lines(c(cuechillsdlow0[i],cuechillsdup0[i]),
        rep(cuechill[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuechill0[i],2),
        c(cuechillsdlow[i],cuechillsdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
  }
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuechill~cuechill0), lwd=1.5)
mtext("a)", side = 3, adj = 0)

plot(cueforce0, cueforce, 
     xlab="sensitivity to forcing (lambda=0)",
     ylab="sensitivity to forcing", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-20,5),ylim=c(-20,5))
for(i in 1:length(cueforce0)){
  lines(c(cueforcesdlow0[i],cueforcesdup0[i]),
        rep(cueforce[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cueforce0[i],2),
        c(cueforcesdlow[i],cueforcesdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}

abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cueforce~cueforce0), lwd=1.5)
mtext("b)", side = 3, adj = 0)

plot(cuephoto0, cuephoto, 
     xlab="sensitivity to photoperiod (lambda=0)",
     ylab="sensitivity to photoperiod", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-10,3),ylim=c(-10,3))
for(i in 1:length(cuephoto0)){
  lines(c(cuephotosdlow0[i],cuephotosdup0[i]),
        rep(cuephoto[i],2), col=adjustcolor("darkgrey",0.4))

  lines(rep(cuephoto0[i],2),
        c(cuephotosdlow[i],cuephotosdup[i]),
        col=adjustcolor("darkgrey",0.4))
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuephoto~cuephoto0), lwd=1.5)
mtext("c)", side = 3, adj = 0)


plot(cuealpha0, cuealpha, 
     xlab="intercept (lambda=0)",
     ylab="intercept", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     #xlim=c(-12,5)
     )
for(i in 1:length(cuealpha0)){
  lines(c(cuealphasdlow0[i],cuealphasdup0[i]),
        rep(cuealpha[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuealpha0[i],2),
        c(cuealphasdlow[i],cuealphasdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
  }
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuealpha~cuealpha0), lwd=1.5)
mtext("d)", side = 3, adj = 0)


plot(cueforce1, cueforce, 
     xlab="sensitivity to forcing (lambda=1)",
     ylab="sensitivity to forcing", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-20,5),ylim=c(-20,5))
for(i in 1:length(cueforce1)){
  lines(c(cueforcesdlow1[i],cueforcesdup1[i]),
        rep(cueforce[i],2), col=adjustcolor("darkgrey",0.4))

  lines(rep(cueforce1[i],2),
        c(cueforcesdlow[i],cueforcesdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
  }
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cueforce~cueforce1), lwd=1.5)
mtext("e)", side = 3, adj = 0)


plot(cuechill1, cuechill, 
     xlab="sensitivity to chilling (lambda=1)",
     ylab="sensitivity to chilling", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-30,5),ylim=c(-30,5))
for(i in 1:length(cueforce1)){
  lines(c(cuechillsdlow1[i],cuechillsdup1[i]),
        rep(cuechill[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuechill1[i],2),
        c(cuechillsdlow[i],cuechillsdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
  }
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuechill~cuechill1), lwd=1.5)
mtext("f)", side = 3, adj = 0)

plot(cuephoto1, cuephoto, 
     xlab="sensitivity to photoperiod (lambda=1)",
     ylab="sensitivity to photoperiod", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-10,3),ylim=c(-10,3))
for(i in 1:length(cuephoto1)){
  lines(c(cuephotosdlow1[i],cuephotosdup1[i]),
        rep(cuephoto[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuephoto1[i],2),
        c(cuephotosdlow[i],cuephotosdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuephoto~cuephoto1), lwd=1.5)
mtext("g)", side = 3, adj = 0)


plot(cuealpha1, cuealpha, 
     xlab="intercept (lambda=1)",
     ylab="intercept", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     #xlim=c(-12,5)
)
for(i in 1:length(cuealpha1)){
  lines(c(cuealphasdlow1[i],cuealphasdup1[i]),
        rep(cuealpha[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuealpha1[i],2),
        c(cuealphasdlow[i],cuealphasdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuealpha~cuealpha1), lwd=1.5)
mtext("h)", side = 3, adj = 0)



### plot correlations gymno ----
dev.off()
par(mfrow=c(2,4))

plot(cueforce0, cueforce, 
     xlab="sensitivity to forcing (lambda=0)",
     ylab="sensitivity to forcing", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-20,5),ylim=c(-20,5))
for(i in 1:length(cueforce0)){
  lines(c(cueforcesdlow0[i],cueforcesdup0[i]),
        rep(cueforce[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cueforce0[i],2),
        c(cueforcesdlow[i],cueforcesdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}

abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cueforce~cueforce0), lwd=1.5)
mtext("a)", side = 3, adj = 0)


plot(cuechill0, cuechill, 
     xlab="sensitivity to chilling (lambda=0)",
     ylab="sensitivity to chilling", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-24,0),ylim=c(-24,0))
for(i in 1:length(cueforce0)){
  lines(c(cuechillsdlow0[i],cuechillsdup0[i]),
        rep(cuechill[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuechill0[i],2),
        c(cuechillsdlow[i],cuechillsdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuechill~cuechill0), lwd=1.5)
mtext("b)", side = 3, adj = 0)

plot(cuephoto0, cuephoto, 
     xlab="sensitivity to photoperiod (lambda=0)",
     ylab="sensitivity to photoperiod", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-8,6),ylim=c(-8,6))
for(i in 1:length(cuephoto0)){
  lines(c(cuephotosdlow0[i],cuephotosdup0[i]),
        rep(cuephoto[i],2), col=adjustcolor("darkgrey",0.4))
  
  lines(rep(cuephoto0[i],2),
        c(cuephotosdlow[i],cuephotosdup[i]),
        col=adjustcolor("darkgrey",0.4))
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuephoto~cuephoto0), lwd=1.5)
mtext("c)", side = 3, adj = 0)


plot(cuealpha0, cuealpha, 
     xlab="intercept (lambda=0)",
     ylab="intercept", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(0,55),ylim=c(0,55)
)
for(i in 1:length(cuealpha0)){
  lines(c(cuealphasdlow0[i],cuealphasdup0[i]),
        rep(cuealpha[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuealpha0[i],2),
        c(cuealphasdlow[i],cuealphasdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuealpha~cuealpha0), lwd=1.5)
mtext("d)", side = 3, adj = 0)


plot(cueforce1, cueforce, 
     xlab="sensitivity to forcing (lambda=1)",
     ylab="sensitivity to forcing", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-20,0),ylim=c(-20,0))
for(i in 1:length(cueforce1)){
  lines(c(cueforcesdlow1[i],cueforcesdup1[i]),
        rep(cueforce[i],2), col=adjustcolor("darkgrey",0.4))
  
  lines(rep(cueforce1[i],2),
        c(cueforcesdlow[i],cueforcesdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cueforce~cueforce1), lwd=1.5)
mtext("e)", side = 3, adj = 0)


plot(cuechill1, cuechill, 
     xlab="sensitivity to chilling (lambda=1)",
     ylab="sensitivity to chilling", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-25,5),ylim=c(-25,5))
for(i in 1:length(cueforce1)){
  lines(c(cuechillsdlow1[i],cuechillsdup1[i]),
        rep(cuechill[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuechill1[i],2),
        c(cuechillsdlow[i],cuechillsdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuechill~cuechill1), lwd=1.5)
mtext("f)", side = 3, adj = 0)

plot(cuephoto1, cuephoto, 
     xlab="sensitivity to photoperiod (lambda=1)",
     ylab="sensitivity to photoperiod", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(-6,6),ylim=c(-6,6))
for(i in 1:length(cuephoto1)){
  lines(c(cuephotosdlow1[i],cuephotosdup1[i]),
        rep(cuephoto[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuephoto1[i],2),
        c(cuephotosdlow[i],cuephotosdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuephoto~cuephoto1), lwd=1.5)
mtext("g)", side = 3, adj = 0)


plot(cuealpha1, cuealpha, 
     xlab="intercept (lambda=1)",
     ylab="intercept", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(0,55),ylim=c(0,55)
)
for(i in 1:length(cuealpha1)){
  lines(c(cuealphasdlow1[i],cuealphasdup1[i]),
        rep(cuealpha[i],2), col=adjustcolor("darkgrey",0.4))
  lines(rep(cuealpha1[i],2),
        c(cuealphasdlow[i],cuealphasdup[i]),
        col=adjustcolor("darkgrey",0.4))
  
}
points
abline(a=0,b=1, col='black', lty=2, lwd=1.5)  
abline(lm(cuealpha~cuealpha1), lwd=1.5)
mtext("h)", side = 3, adj = 0)



### plotting lambdas and sigmas----
names(extract(modelhere))
dev.off()


mean(extract(fitlambest)[["lam_interceptsa"]],na.rm = T)

if(agiosponly){
  par(mfrow=c(1,3))
  plot(x=NULL,y=NULL, xlim=c(0,1), ylim=c(0,3),ylab="density",
       xlab="lambda", main="")
  
  #lines(density(extract(fitlambest)[["lam_interceptsa"]]),  col='grey',lwd=1.8)
  lines(density(extract(fitlambest)[["lam_interceptsbf"]]), col='indianred3',lwd=1.8)
  lines(density(extract(fitlambest)[["lam_interceptsbc"]]), col='cyan4',lwd=1.8)
  lines(density(extract(fitlambest)[["lam_interceptsbp"]]), col='orange',lwd=1.8)
  #text(0.50,3.5,"intercept",col='grey')
  text(0.8,2,"forcing",col='indianred3')
  text(0.6,2.7,"chilling",col='cyan4')
  text(0.15,1.6,"photoperiod",col='orange')
  text(0,3,"a",cex=1.5)
  
  
  plot(x=NULL,y=NULL, xlim=c(0,15), ylim=c(0,1),ylab="density",
       xlab="sigma", main="")
  
  #lines(density(extract(fitlambest)[["sigma_interceptsa"]]),  col='grey',lwd=1.8)
  lines(density(extract(fitlambest)[["sigma_interceptsbf"]]), col='indianred3',lwd=1.8)
  lines(density(extract(fitlambest)[["sigma_interceptsbc"]]), col='cyan4',lwd=1.8)
  lines(density(extract(fitlambest)[["sigma_interceptsbp"]]), col='orange',lwd=1.8)
  #text(19,0.3,"intercept",col='grey')
  text(5,0.45,"forcing",col='indianred3')
  text(9,0.4,"chilling",col='cyan4')
  text(4.5,0.8,"photoperiod",col='orange')
  text(0,1,"b",cex=1.5)
  
  
  plot(x=NULL,y=NULL, xlim=c(0,15), ylim=c(0,1),ylab="density",
       xlab="sigma (lambda = 0)", main="")
  
  #lines(density(extract(fitlam0)[["sigma_interceptsa"]]),  col='grey',lwd=1.8)
  lines(density(extract(fitlam0)[["sigma_interceptsbf"]]), col='indianred3',lwd=1.8)
  lines(density(extract(fitlam0)[["sigma_interceptsbc"]]), col='cyan4',lwd=1.8)
  lines(density(extract(fitlam0)[["sigma_interceptsbp"]]), col='orange',lwd=1.8)
  #text(19,0.3,"intercept",col='grey')
  text(5,0.5,"forcing",col='indianred3')
  text(9,0.4,"chilling",col='cyan4')
  text(4.5,0.8,"photoperiod",col='orange')
  text(0,1,"c",cex=1.5)
  
  #plot(x=NULL,y=NULL, xlim=c(0,20), ylim=c(0,1),ylab="density",
  #     xlab="sigma (lambda = 1)", main="")
  
  #lines(density(extract(fitlam1)[["sigma_interceptsa"]]),  col='grey',lwd=1.8)
  #lines(density(extract(fitlam1)[["sigma_interceptsbf"]]), col='indianred3',lwd=1.8)
  #lines(density(extract(fitlam1)[["sigma_interceptsbc"]]), col='cyan4',lwd=1.8)
  #lines(density(extract(fitlam1)[["sigma_interceptsbp"]]), col='orange',lwd=1.8)
  #text(19,0.3,"intercept",col='grey')
  #text(5,0.3,"forcing",col='indianred3')
  #text(9,0.37,"chilling",col='cyan4')
  #text(4.1,0.78,"photoperiod",col='orange')
  #text(0,1,"d",cex=1.5)
  
  
  
}

if(!agiosponly){
  par(mfrow=c(1,4))
  plot(x=NULL,y=NULL, xlim=c(0,1), ylim=c(0,2),ylab="density",
       xlab="lambda", main="")
  
  lines(density(extract(fitlambest)[["lam_interceptsa"]]),  col='grey',lwd=1.8)
  lines(density(extract(fitlambest)[["lam_interceptsbf"]]), col='indianred3',lwd=1.8)
  lines(density(extract(fitlambest)[["lam_interceptsbc"]]), col='cyan4',lwd=1.8)
  lines(density(extract(fitlambest)[["lam_interceptsbp"]]), col='orange',lwd=1.8)
  text(0.50,3.5,"intercept",col='grey')
  text(0.8,2,"force",col='indianred3')
  text(0.6,2.7,"chill",col='cyan4')
  text(0.8,0.75,"photo",col='orange')
  text(0,2,"e",cex=1.5)
  
  
  plot(x=NULL,y=NULL, xlim=c(0,45), ylim=c(0,0.25),ylab="density",
       xlab="sigma", main="")
  
  lines(density(extract(fitlambest)[["sigma_interceptsa"]]),  col='grey',lwd=1.8)
  lines(density(extract(fitlambest)[["sigma_interceptsbf"]]), col='indianred3',lwd=1.8)
  lines(density(extract(fitlambest)[["sigma_interceptsbc"]]), col='cyan4',lwd=1.8)
  lines(density(extract(fitlambest)[["sigma_interceptsbp"]]), col='orange',lwd=1.8)
  text(19,0.3,"intercept",col='grey')
  text(5,0.45,"force",col='indianred3')
  text(9,0.4,"chill",col='cyan4')
  text(5.1,0.8,"photo",col='orange')
  text(0,0.25,"f",cex=1.5)
  
  
  plot(x=NULL,y=NULL, xlim=c(0,45), ylim=c(0,0.25),ylab="density",
       xlab="sigma (lambda = 0)", main="")
  
  lines(density(extract(fitlam0)[["sigma_interceptsa"]]),  col='grey',lwd=1.8)
  lines(density(extract(fitlam0)[["sigma_interceptsbf"]]), col='indianred3',lwd=1.8)
  lines(density(extract(fitlam0)[["sigma_interceptsbc"]]), col='cyan4',lwd=1.8)
  lines(density(extract(fitlam0)[["sigma_interceptsbp"]]), col='orange',lwd=1.8)
  text(19,0.3,"intercept",col='grey')
  text(5,0.45,"force",col='indianred3')
  text(9,0.4,"chill",col='cyan4')
  text(5.1,0.8,"photo",col='orange')
  text(0,0.25,"g",cex=1.5)
  
  plot(x=NULL,y=NULL, xlim=c(0,85), ylim=c(0,0.25),ylab="density",
       xlab="sigma (lambda = 1)", main="")
  
  lines(density(extract(fitlam1)[["sigma_interceptsa"]]),  col='grey',lwd=1.8)
  lines(density(extract(fitlam1)[["sigma_interceptsbf"]]), col='indianred3',lwd=1.8)
  lines(density(extract(fitlam1)[["sigma_interceptsbc"]]), col='cyan4',lwd=1.8)
  lines(density(extract(fitlam1)[["sigma_interceptsbp"]]), col='orange',lwd=1.8)
  text(19,0.3,"intercept",col='grey')
  text(5,0.45,"force",col='indianred3')
  text(9,0.4,"chill",col='cyan4')
  text(5.1,0.8,"photo",col='orange')
  text(0,0.25,"h",cex=1.5)
  
  
  
}

## check model R2s ----


## load models
agiosponly=T
if(agiosponly){
  fitlam0 <- readRDS("output/testme_yhat_noout_lamb0.rds")
  fitlam1 <- readRDS("output/testme_yhat_noout_lamb1.rds")
  fitlambest <- readRDS("output/testme_yhat_noout.rds")
} else {
  fitlam0 <- readRDS("output/testme_yhat_noout_lamb0_gymno.rds")
  fitlam1 <- readRDS("output/testme_yhat_noout_lamb1_gymno.rds")
  fitlambest <- readRDS("output/testme_yhat_noout_gymno.rds")
}


## first we retrieve yhats

list_of_draws <- extract(fitlambest)
list_of_draws0 <- extract(fitlam0)
list_of_draws1 <- extract(fitlam1)


names(list_of_draws)

yhat = list_of_draws0$yhat

plot(density(yhat[1,]),col=adjustcolor("lightblue",0.05))
for(i in 1:100){
ff=sample(1:4000,1)
print(ff)
  lines(density(yhat[ff,]),col=adjustcolor("lightblue",0.05))
}
lines(density(d$resp),col="darkblue",lwd=2)  



bayes_R2_mine <- function(d,list_of_draws) {
  y <- d$resp
  ypred <- list_of_draws$yhat
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  R2post <- var_ypred / (var_ypred + var_e)
  return(R2post)
  }

mean(bayes_R2_mine(d,list_of_draws))
mean(bayes_R2_mine(d,list_of_draws0))
mean(bayes_R2_mine(d,list_of_draws1))

## compare R2
boxplot(cbind(bayes_R2_mine(d,list_of_draws),
          bayes_R2_mine(d,list_of_draws0),
          bayes_R2_mine(d,list_of_draws1)),ylab="Bayes R2")


## plot predictions
plot(colMeans(list_of_draws$yhat),d$resp)
abline(lm(d$resp~colMeans(list_of_draws$yhat)),col="grey")
summary(lm(d$resp~colMeans(list_of_draws$yhat)))

plot(colMeans(list_of_draws0$yhat),d$resp)
abline(lm(d$resp~colMeans(list_of_draws0$yhat)),col="grey")
summary(lm(d$resp~colMeans(list_of_draws0$yhat)))

plot(colMeans(list_of_draws1$yhat),d$resp)
abline(lm(d$resp~colMeans(list_of_draws1$yhat)),col="grey")
summary(lm(d$resp~colMeans(list_of_draws1$yhat)))




## load function
remotes::install_github("avehtari/ROS-Examples",subdir = "rpackage")
library("rosdata")
library("rstanarm")
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
library("latex2exp")
library("foreign")
library("bayesboot")
SEED <- 1800
set.seed(SEED)

bayes_R2_res <- function(fit) {
  y <- rstanarm::get_y(fit)
  ypred <- rstanarm::posterior_epred(fit)
  if (family(fit)$family == "binomial" && NCOL(y) == 2) {
    trials <- rowSums(y)
    y <- y[, 1]
    ypred <- ypred %*% diag(trials)
  }
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  var_ypred / (var_ypred + var_e)
}

bayes_R2 <- function(mu, sigma) {
  var_mu <- apply(mu, 1, var)
  sigma2 <- sigma^2
  var_mu / (var_mu + sigma2)
}


posterior_performance <- function(obj, prob=0.95, sub.idx=NULL, summary=TRUE,
                                  cores=getOption("mc.cores", 1)) {
  if (inherits(obj, "hsstan")) {
    obj <- list(fits=array(list(fit=obj, test.idx=1:nrow(obj$data)), c(1, 2)),
                data=obj$data)
    colnames(obj$fits) <- c("fit", "test.idx")
  } else if (inherits(obj, c("kfold", "loo"))) {
    if (is.null(obj[["fits"]]))
      stop("No fitted models found, run 'kfold' with store.fits=TRUE.")
  } else
    stop("Not an 'hsstan' or 'kfold' object.")
  
  if (is.null(sub.idx)) {
    sub.idx <- 1:nrow(obj$data)
    used.subset <- FALSE
  } else {
    validate.indices(sub.idx, nrow(obj$data), "sub.idx")
    sub.idx <- sort(sub.idx)
    used.subset <- length(sub.idx) < nrow(obj$data)
  }
  
  validate.samples(obj$fits[[1]])
  validate.probability(prob)
  logistic <- is.logistic(obj$fits[[1]])
  num.folds <- nrow(obj$fits)
  
  ## loop over the folds
  y <- mu <- llk <- NULL
  for (fold in 1:num.folds) {
    hs <- obj$fits[[fold]]
    test.idx <- intersect(obj$fits[, "test.idx"][[fold]], sub.idx)
    if (length(test.idx) == 0)
      next
    newdata <- obj$data[test.idx, ]
    y <- c(y, obj$data[test.idx, hs$model.terms$outcome])
    mu <- cbind(mu, posterior_linpred(hs, newdata=newdata, transform=TRUE))
    llk <- cbind(llk, log_lik(hs, newdata=newdata))
  }
  
  if (used.subset && logistic && length(unique(y)) != 2)
    stop("'sub.idx' must contain both outcome classes.")
  
  if (logistic) {
    par.auc <- function(i)
      as.numeric(pROC::roc(y, mu[i, ], direction="<", quiet=TRUE)$auc)
    if (.Platform$OS.type != "windows") {
      out <- parallel::mclapply(X=1:nrow(mu), mc.cores=cores,
                                mc.preschedule=TRUE, FUN=par.auc)
    } else { # windows
      cl <- parallel::makePSOCKcluster(cores)
      on.exit(parallel::stopCluster(cl))
      out <- parallel::parLapply(X=1:nrow(mu), cl=cl, fun=par.auc)
    }
  } else {
    out <- pmax(fastCor(y, mu), 0)^2
  }
  
  out <- cbind(perf=unlist(out), llk=rowSums(llk))
  colnames(out)[1] <- ifelse(logistic, "auc", "r2")
  if (summary)
    out <- posterior_summary(out, prob)
  attr(out, "type") <- paste0(if(num.folds == 1) "non ", "cross-validated")
  if (used.subset)
    attr(out, "subset") <- sub.idx
  return(out)
}


posterior_predict.my_linstan <- function(object, newdata=NULL, draws=NULL) {
  if (is.null(newdata)) {
    newdata = object$data
  }
  
  mm <- model.matrix(delete.response(object$terms), data=newdata)
  
  coef_matrix <- as.matrix(object$fit)
  if (!is.null(draws)) {
    coef_matrix <- coef_matrix[sample.int(nrow(coef_matrix), draws),]
  }
  
  point_preds <- coef_matrix[,colnames(mm)] %*% t(mm)
  # Note this could do the wrong thing if "sigma" is a coefficient
  preds <- rnorm_matrix(point_preds, coef_matrix[,"sigma"])
  
  preds
}




posterior_performance(fit)

bayes_R2(ext_pred2$height_hat , ext_pred2$sigma   ) %>% mean
bayes_R2(fitlambest$sigma_y,fitlambest$sigma_y) %>% mean
loo(fit)

fitlambest@inits


bayes_R2_res(fitlam0)
bayes_R2_res(fitlam1)
bayes_R2_res(fitlambest)
fit = fitlambest
?get_y
fit$[[""]]
rstanarm::get_x(fit)
rstanarm::posterior_epred(fit)

loo::extract_log_lik(stanfit = fit,
                     parameter_name = "log_lik",
                     merge_chains = FALSE)

?loo.function(as.array(fit))
?loo
pp_check(fit, ndraws=20)

class(fit)
?loo(fitlam0)


#'################
#### END TEST ####
#'################

 
