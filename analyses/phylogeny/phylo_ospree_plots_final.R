## Started 9 September 2021 ##
## By Nacho, copies Nacho's Phylo_ospree_reanalyses.R code ##

## But edits it to be used for testing new stan models. ##

## This version has new attempts to run the code by Nacho:
## - tweaking priors
## - trying uncentered variables
## - adding chilling

#### remove objects (activate if needed)
#### remove objects (activate if needed)
# rm(list=ls())
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny")
} else if (length(grep("ailene", getwd()))>0){
  setwd("/Users/aileneettinger/git/ospree/analyses/phylogeny")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/phylogeny") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
}else
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny")



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

#write.csv(as.data.frame(phylo$tip.label),file = "output/sps_list_phylo.csv")

## get ailene´s model table
#source("../../analyses/bb_analysis/maketables.forsupp/mod_table.R")


#'###############################################
#### comparing estimates lambda est vs 1 vs 0 ####
#'###############################################


## load models
agiosponly=T
if(agiosponly){
  fitlam0 <- readRDS("output/fit_priorupdate_noout_angio191_lamb0.rds")
  fitlambest <- readRDS("output/fit_priorupdate_noout_angio191.rds")
} else {
  fitlam0 <- readRDS("output/testme_gymno_lambda0.rds")
  fitlambest <- readRDS("output/testmegymno.rds")
}



## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
tableresults.0 = summary(fitlam0, pars = list("a_z", "sigma_interceptsa", "b_zf", "sigma_interceptsbf", "b_zc", "sigma_interceptsbc", "b_zp", "sigma_interceptsbp", "sigma_y"))$summary
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





# get model estimates per species ----

## where species are

  posspsindata.est <- list(10:200,202:392,394:584)
  posspsindata.01 <- list(6:196,198:388,390:580)

  
## make tables for species-level estimates (supp) ----
  tableresults.est = summary(fitlambest)$summary
  tableresults.est0 = summary(fitlam0)$summary
  
  spslevtable = cbind(tableresults.est[posspsindata.est[[2]],c("mean","2.5%","97.5%")],
                      tableresults.est[posspsindata.est[[1]],c("mean","2.5%","97.5%")],
                      tableresults.est[posspsindata.est[[3]],c("mean","2.5%","97.5%")])
  
  colnames(spslevtable) = c("b_chill","b_chill_low","b_chill_up",
                            "b_force","b_force_low","b_force_up",
                            "b_photo","b_photo_low","b_photo_up")
  rownames(spslevtable) = gsub("_"," ",rownames(spslevtable),fixed = T) 
    
  spslevtable0 = cbind(tableresults.est0[posspsindata.01[[2]],c("mean","2.5%","97.5%")],
                       tableresults.est0[posspsindata.01[[1]],c("mean","2.5%","97.5%")],
                       tableresults.est0[posspsindata.01[[3]],c("mean","2.5%","97.5%")])
  colnames(spslevtable0) = c("b_chill","b_chill_low","b_chill_up",
                            "b_force","b_force_low","b_force_up",
                            "b_photo","b_photo_low","b_photo_up")
  rownames(spslevtable0) = gsub("_"," ",rownames(spslevtable0),fixed = T) 
  
  
  #write.csv(spslevtable, file = "output/spslevestimates191.csv")
  #write.csv(spslevtable0, file = "output/spslevestimateslamb0_191.csv")
  
  
## forcing
cueforce = summary(fitlambest)$summary[posspsindata.est[[1]],"mean"]
cueforcesdup = summary(fitlambest)$summary[posspsindata.est[[1]],"75%"]
cueforcesdlow = summary(fitlambest)$summary[posspsindata.est[[1]],"25%"]

cueforce0 = summary(fitlam0)$summary[posspsindata.01[[1]],"mean"]
cueforcesdup0 = summary(fitlam0)$summary[posspsindata.01[[1]],"75%"]
cueforcesdlow0 = summary(fitlam0)$summary[posspsindata.01[[1]],"25%"]


## chill
cuechill = summary(fitlambest)$summary[posspsindata.est[[2]],"mean"]
cuechillsdup = summary(fitlambest)$summary[posspsindata.est[[2]],"75%"]
cuechillsdlow = summary(fitlambest)$summary[posspsindata.est[[2]],"25%"]

cuechill0 = summary(fitlam0)$summary[posspsindata.01[[2]],"mean"]
cuechillsdup0 = summary(fitlam0)$summary[posspsindata.01[[2]],"75%"]
cuechillsdlow0 = summary(fitlam0)$summary[posspsindata.01[[2]],"25%"]


## photo
cuephoto = summary(fitlambest)$summary[posspsindata.est[[3]],"mean"]
cuephotosdup = summary(fitlambest)$summary[posspsindata.est[[3]],"75%"]
cuephotosdlow = summary(fitlambest)$summary[posspsindata.est[[3]],"25%"]

cuephoto0 = summary(fitlam0)$summary[posspsindata.01[[3]],"mean"]
cuephotosdup0 = summary(fitlam0)$summary[posspsindata.01[[3]],"75%"]
cuephotosdlow0 = summary(fitlam0)$summary[posspsindata.01[[3]],"25%"]




##### comparing uncertainty around individual species estimations----

par(mfrow=c(1,3))

indvarest = cuechillsdup - cuechillsdlow 
indvarlam0 = cuechillsdup0 - cuechillsdlow0
boxplot(cbind(lambdaest=indvarest,lambda0=indvarlam0),
        ylab="Estimate uncertainty (days/std units of chilling)",outline=F,ylim=c(0,10),cex.lab=1.25)
mtext("a", side = 3, adj = 0.05,line=-2,cex=1.5)

indvarest = cueforcesdup - cueforcesdlow 
indvarlam0 = cueforcesdup0 - cueforcesdlow0
boxplot(cbind(lambdaest=indvarest,lambda0=indvarlam0),
        ylab="Estimate uncertainty (days/std units of forcing)",outline=F,ylim=c(0,10),cex.lab=1.25)
mtext("b", side = 3, adj = 0.05,line=-2,cex=1.5)

indvarest = cuephotosdup - cuephotosdlow 
indvarlam0 = cuephotosdup0 - cuephotosdlow0
boxplot(cbind(lambdaest=indvarest,lambda0=indvarlam0),
        ylab="Estimate uncertainty (days/std units of photoperiod)",
        outline=F,ylim=c(0,4),cex.lab=1.25)
mtext("c", side = 3, adj = 0.05,line=-2,cex=1.5)



mean(indvarest);mean(indvarlam0)



##### comparing overall variances across species responses ----

var(cueforce)
var(cueforce0)

var(cuechill)
var(cuechill0)

var(cuephoto)
var(cuephoto0)



##### comparing bias in average estimating ----


mean(cueforce-cueforce0)
mean(cuechill-cuechill0)
mean(cuephoto-cuephoto0)


summary(lm(cueforce~cueforce0))$coefficients[2,1]-1
summary(lm(cuechill~cuechill0))$coefficients[2,1]-1
summary(lm(cuephoto~cuephoto0))$coefficients[2,1]-1

par(mfrow=c(1,3))

hist(cuechill-cuechill0,30, main='',
     xlab="Chilling_phylo - Chilling_non-phylo")
box(which = "plot", lty = "solid")
mtext("a", side = 3, adj = 0.05,line=-2,cex=1.5)

hist(cueforce-cueforce0,30,main='',
     xlab="Forcing_phylo - Forcing_non-phylo")
box(which = "plot", lty = "solid")
mtext("b", side = 3, adj = 0.05,line=-2,cex=1.5)

hist(cuephoto-cuephoto0,30,main='',
     xlab="Photoperiod_phylo - Photoperiod_non-phylo")
box(which = "plot", lty = "solid")
mtext("c", side = 3, adj = 0.05,line=-2,cex=1.5)



## identify which species/clades have greater differences
sort(cueforce-cueforce0)
sort(cuechill-cuechill0)

sort((cueforce-cueforce0)+(cuechill-cuechill0))
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
abline(a=0,b=1, col='darkgrey', lty=2, lwd=1.5)  
abline(v=mean(cuechill0), col='black', lty=2, lwd=1.5)  
#abline(lm(cuechill~cuechill0), lwd=1.5)
mtext("a", side = 3, adj = 0.05,line=-2,cex=1.5)

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

abline(a=0,b=1, col='darkgrey', lty=2, lwd=1.5)  
abline(v=mean(cueforce0), col='black', lty=2, lwd=1.5)  
#abline(lm(cueforce~cueforce0), lwd=1.5)
mtext("b", side = 3, adj = 0.05,line=-2,cex=1.5)

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
abline(a=0,b=1, col='darkgrey', lty=2, lwd=1.5)  
abline(v=mean(cuephoto0), col='black', lty=2, lwd=1.5)  
#abline(lm(cuephoto~cuephoto0), lwd=1.5)
mtext("c", side = 3, adj = 0.05,line=-2,cex=1.5)



cbind(cuephoto0, cuephoto)
cbind(cueforce0, cueforce)


### make forecasting figure ----

## load climate data for EUsps (14Mar2023: I don't have these so cannot run this part of code -- Lizzie) 
load("../phylogeny/output/Forcechill.in.range.EUsp.RData")
Climate.in.range.list.all = Climate.in.range.list
load("../phylogeny/output/Forcechill.in.range.EU.AcecamBetpen.RData")
Climate.in.range.list.2sps = Climate.in.range.list


## load models
  fitlam0 <- readRDS("~/MEGA/Work_UAH_BeaGal/SIDE PROJECTS/OSPREE/Phylogeny/outputs/fit_priorupdate_noout_angio191_lamb0_nonz.rds")
  fitlambest <- readRDS("~/MEGA/Work_UAH_BeaGal/SIDE PROJECTS/OSPREE/Phylogeny/outputs/fit_priorupdate_noout_angio191_nonz.rds")


## rename models to include species names
names(fitlambest)[grep(pattern = "^a\\[", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_force", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_chill", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_photo", x = names(fitlambest))] <- phylo$tip.label

names(fitlam0)[grep(pattern = "^a\\[", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_force", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_chill", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_photo", x = names(fitlam0))] <- phylo$tip.label


## load species names
spslistranges = read.csv("figures/forecast_figure/sps_list_fromranges.csv") # (14Mar2023: Updated path, please check -- Lizzie)


## assign names to list of climate data
names(Climate.in.range.list.all) = spslistranges[,2]

## check species for which we have model parameters
spslistranges[,2][which(spslistranges[,2] %in% phylo$tip.label)] ## all 30


#

fit.sum <- summary(fitlambest)$summary

# Select the species and temperature change that you want
sp<-c("Betula_pendula","Fagus_sylvatica")

sp.numinlist <- c(7,12)
sp.numinphylo <- which(phylo$tip.label %in% sp)


if(FALSE){
##
## Checking by hand by Lizzie

# Step 1 for me is to check the model, I pulled what I think is the model I ran in January and has natural units
checkmodel  <- readRDS("output/fit_priorupdate_noout_angio191_lamb0_nonz.rds")
tail(fit.sum)
tail(summary(checkmodel)$summary)
# Hmm, a_z -- which is the grand mean of the intercept is higher in mine. It looks much more like what I expect
# Also, see Ettinger et al 2020: Table S6: Estimates from models fit with predictors on their natural scales, those means 60+ while Table S5: Estimates from models fit with standardized predictors -- has #s around 30
## So -- **Check your model is the natural units one!**

# Step 2: I will check the equations by just writing them out myself ... 
listofdraws <- extract(fitlamb0)
forcingsp7 <- listofdraws$b_force[,7]
chillingsp7 <- listofdraws$b_chill[,7]
asp7 <- listofdraws$a[,7]

forcingsp12 <- listofdraws$b_force[,12]
chillingsp12 <- listofdraws$b_chill[,12]
asp12 <- listofdraws$a[,12]

mean(forcingsp7)
mean(chillingsp7)
mean(asp7)

# sp7historical, then warmed (I just guessed at #s from email)
mean(asp7) + mean(forcingsp7)*5 + mean(chillingsp7)*(2000/240)
mean(asp7) + mean(forcingsp7)*7 + mean(chillingsp7)*(2400/240)
# sp12historical, then warmed 
mean(asp12) + mean(forcingsp12)*5 + mean(chillingsp12)*(2000/240)
mean(asp12) + mean(forcingsp12)*7 + mean(chillingsp12)*(2400/240)
## So -- these seem fine to me ...

# Step 3: try above with other model
listofdraws <- extract(checkmodel)
forcingsp7 <- listofdraws$b_force[,7]
chillingsp7 <- listofdraws$b_chill[,7]
asp7 <- listofdraws$a[,7]

forcingsp12 <- listofdraws$b_force[,12]
chillingsp12 <- listofdraws$b_chill[,12]
asp12 <- listofdraws$a[,12]

mean(asp7) + mean(forcingsp7)*5 + mean(chillingsp7)*(2000/240)
mean(asp7) + mean(forcingsp7)*7 + mean(chillingsp7)*(2400/240)
mean(asp12) + mean(forcingsp12)*5 + mean(chillingsp12)*(2000/240)
mean(asp12) + mean(forcingsp12)*7 + mean(chillingsp12)*(2400/240)
## So -- these numbers are more reasonable ...

## End Checking by hand by Lizzie
##
}


#Define the function we will use to estimate budburst
sps.i <- Climate.in.range.list[[sp.numinlist[1]]][[1]]

getspest.bb <- function(fit,fit0,sps.names, sps.i,sps.i2, photoper,i){
  #s=1
  #fit=fitlambest
  #fit0=fitlam0
  #i=1
  #sps.i =sps1
  dim(sps.i)
  #sps.i2 =sps2
  #photoper = 12
  listofdraws <- extract(fit)
  listofdraws0 <- extract(fit0)
  
  forcing <- sps.i[i,"Forcing"]
  chilling <- sps.i[i,"Chilling"]
  
  forcing2 <- sps.i2[i,"Forcing"]
  chilling2 <- sps.i2[i,"Chilling"]
  
  
  avgbbsps1 <- listofdraws$a[,sp.numinphylo[1]] + listofdraws$b_force[,sp.numinphylo[1]]*forcing +
    listofdraws$b_photo[,sp.numinphylo[1]]*photoper + listofdraws$b_chill[,sp.numinphylo[1]]*chilling/240
  
  avgbbsps2 <- listofdraws$a[,sp.numinphylo[2]] + listofdraws$b_force[,sp.numinphylo[2]]*forcing2 +
    listofdraws$b_photo[,sp.numinphylo[2]]*photoper + listofdraws$b_chill[,sp.numinphylo[2]]*chilling2/240
  
  avgbbsps1.0 <- listofdraws0$a[,sp.numinphylo[1]] + listofdraws0$b_force[,sp.numinphylo[1]]*forcing +
    listofdraws0$b_photo[,sp.numinphylo[1]]*photoper + listofdraws0$b_chill[,sp.numinphylo[1]]*chilling/240
  
  avgbbsps2.0 <- listofdraws0$a[,sp.numinphylo[2]] + listofdraws0$b_force[,sp.numinphylo[2]]*forcing2 +
    listofdraws0$b_photo[,sp.numinphylo[2]]*photoper + listofdraws0$b_chill[,sp.numinphylo[2]]*chilling2/240
  
  
  yebbest <- list(BBsps1lam=avgbbsps1,BBsps2lam=avgbbsps2,BBsps1lam0=avgbbsps1.0,BBsps2lam0=avgbbsps2.0)
  return(yebbest)
}

getspest.bb.maps <- function(fit,fit0,sps.names, sps.i,sps.i2, photoper,i){
  #s=1
  #fit=fitlambest
  #fit0=fitlam0
  #i=1
  #photoper = 14
  #sps1 <- Climate.in.range.list.all[[7]][[1]]
  #sps2 <- Climate.in.range.list.all[[1]][[1]]
  #sps.i =sps1
  #sps.i2 =sps2
  
  
  listofdraws <- extract(fit)
  listofdraws0 <- extract(fit0)
  
  forcing <- sps.i[,3,i]
  chilling <- sps.i[,4,i]
  
  forcing2 <- sps.i2[,3,i]
  chilling2 <- sps.i2[,4,i]
  
  alphasps1 <- mean(listofdraws$a[,sp.numinphylo[1]],na.rm = T)
  bforcesps1 <- mean(listofdraws$b_force[,sp.numinphylo[1]],na.rm = T)
  bchillsps1 <- mean(listofdraws$b_chill[,sp.numinphylo[1]],na.rm = T)
  bphotosps1 <- mean(listofdraws$b_photo[,sp.numinphylo[1]],na.rm = T)
  
  alphasps2 <- mean(listofdraws$a[,sp.numinphylo[2]],na.rm = T)
  bforcesps2 <- mean(listofdraws$b_force[,sp.numinphylo[2]],na.rm = T)
  bchillsps2 <- mean(listofdraws$b_chill[,sp.numinphylo[2]],na.rm = T)
  bphotosps2 <- mean(listofdraws$b_photo[,sp.numinphylo[2]],na.rm = T)
  
  alphasps10 <- mean(listofdraws0$a[,sp.numinphylo[1]],na.rm = T)
  bforcesps10 <- mean(listofdraws0$b_force[,sp.numinphylo[1]],na.rm = T)
  bchillsps10 <- mean(listofdraws0$b_chill[,sp.numinphylo[1]],na.rm = T)
  bphotosps10 <- mean(listofdraws0$b_photo[,sp.numinphylo[1]],na.rm = T)
  
  alphasps20 <- mean(listofdraws0$a[,sp.numinphylo[2]],na.rm = T)
  bforcesps20 <- mean(listofdraws0$b_force[,sp.numinphylo[2]],na.rm = T)
  bchillsps20 <- mean(listofdraws0$b_chill[,sp.numinphylo[2]],na.rm = T)
  bphotosps20 <- mean(listofdraws0$b_photo[,sp.numinphylo[2]],na.rm = T)
  
  avgbbsps1 <- alphasps1 + bforcesps1*forcing +
    bphotosps1*photoper + bchillsps1*chilling/240
  
  avgbbsps2 <- alphasps2 + bforcesps2*forcing2 +
    bphotosps2*photoper + bchillsps2*chilling2/240
  
  avgbbsps1.0 <- alphasps10 + bforcesps10*forcing +
    bphotosps10*photoper + bchillsps10*chilling/240
  
  avgbbsps2.0 <- alphasps20 + bforcesps20*forcing2 +
    bphotosps20*photoper + bchillsps20*chilling2/240
  

  
  yebbest <- list(BBsps1lam=avgbbsps1,BBsps2lam=avgbbsps2,BBsps1lam0=avgbbsps1.0,BBsps2lam0=avgbbsps2.0)
  return(yebbest)
}


## get BB estimate for the historical period for both species at selected site
## in the middle of europe (9.875, 48.125
# select data for one species and one year

## Betula pendula
sps1 <- Climate.in.range.list.all[[7]][[1]]
sps1 <- sps1[which(sps1[,1,1]==9.875 & sps1[,2,1]==48.125),3:4,1:11]
sps1 <- as.data.frame(t(sps1))

# Acer campestris
sps2 <- Climate.in.range.list.all[[1]][[1]]
sps2 <- sps2[which(sps2[,1,1]==9.875 & sps2[,2,1]==48.125),3:4,1:11]
sps2 <- as.data.frame(t(sps2))

## Betula pendula low chilling
sps1.lowchill <- Climate.in.range.list.2sps[[7]][[1]]
sps1.lowchill <- sps1.lowchill[which(sps1.lowchill[,1,1]==9.875 & sps1.lowchill[,2,1]==48.125),3:4,1:11]
sps1.lowchill <- as.data.frame(t(sps1.lowchill))
sps1.lowchill$Forcing <- sps1.lowchill$Forcing+2

# Acer campestris low chilling
sps2.lowchill <- Climate.in.range.list.2sps[[1]][[1]]
sps2.lowchill <- sps2.lowchill[which(sps2.lowchill[,1,1]==9.875 & sps2.lowchill[,2,1]==48.125),3:4,1:11]
sps2.lowchill <- as.data.frame(t(sps2.lowchill))
sps2.lowchill$Forcing <- sps2.lowchill$Forcing+2

sps.names = c("Betula_pendula","Acer_campestre")
sp.numinphylo <- which(phylo$tip.label %in% sps.names)

nyears = nrow(sps1)

listestsbb.1lam=
listestsbb.2lam=
listestsbb.10=
listestsbb.20=
listestsbb.lowch.1lam=
listestsbb.lowch.2lam=
listestsbb.lowch.10=
listestsbb.lowch.20= list()

## loop to predict for a single site
for (i in 1:nyears){ #i=1
print(i)
  listestsbb.1lam[[i]] = getspest.bb(fitlambest,fitlam0,sps.names, 
                                sps1,sps2, 14,i)$BBsps1lam
  listestsbb.2lam[[i]] = getspest.bb(fitlambest,fitlam0,sps.names, 
                                sps1,sps2, 14,i)$BBsps2lam
  listestsbb.10[[i]] = getspest.bb(fitlambest,fitlam0,sps.names, 
                                sps1,sps2, 14,i)$BBsps1lam0
  listestsbb.20[[i]] = getspest.bb(fitlambest,fitlam0,sps.names, 
                                sps1,sps2, 14,i)$BBsps2lam0
  
  listestsbb.lowch.1lam[[i]] = getspest.bb(fitlambest,fitlam0,sps.names, 
                                    sps1.lowchill,sps2.lowchill, 14,i)$BBsps1lam
  listestsbb.lowch.2lam[[i]] = getspest.bb(fitlambest,fitlam0,sps.names, 
                                           sps1.lowchill,sps2.lowchill, 14,i)$BBsps2lam
  listestsbb.lowch.10[[i]] = getspest.bb(fitlambest,fitlam0,sps.names, 
                                           sps1.lowchill,sps2.lowchill, 14,i)$BBsps1lam0
  listestsbb.lowch.20[[i]] = getspest.bb(fitlambest,fitlam0,sps.names, 
                                           sps1.lowchill,sps2.lowchill, 14,i)$BBsps2lam0
  
}


## loop to predict across multiple sites ----

nyears = 11
nrowallsitessps1 = dim(sps1)[1]
nrowallsitessps2 = dim(sps2)[1]
storeresultssps1 = array(NA, dim=c(nrowallsitessps1,nyears,4))
storeresultssps2 = array(NA, dim=c(nrowallsitessps2,nyears,4))

for (i in 1:nyears){ #i=1
  print(i)
  storeresultssps1[,i,1] = getspest.bb.maps(fitlambest,fitlam0,sps.names, 
                                     sps1,sps2, 14,i)$BBsps1lam
  storeresultssps2[,i,1] = getspest.bb.maps(fitlambest,fitlam0,sps.names, 
                                     sps1,sps2, 14,i)$BBsps2lam
  storeresultssps1[,i,2] = getspest.bb.maps(fitlambest,fitlam0,sps.names, 
                                   sps1,sps2, 14,i)$BBsps1lam0
  storeresultssps2[,i,2] = getspest.bb.maps(fitlambest,fitlam0,sps.names, 
                                   sps1,sps2, 14,i)$BBsps2lam0
  
  storeresultssps1[,i,3] = getspest.bb.maps(fitlambest,fitlam0,sps.names, 
                                           sps1.lowchill,sps2.lowchill, 14,i)$BBsps1lam
  storeresultssps2[,i,3] = getspest.bb.maps(fitlambest,fitlam0,sps.names, 
                                           sps1.lowchill,sps2.lowchill, 14,i)$BBsps2lam
  storeresultssps1[,i,4] = getspest.bb.maps(fitlambest,fitlam0,sps.names, 
                                         sps1.lowchill,sps2.lowchill, 14,i)$BBsps1lam0
  storeresultssps2[,i,4] = getspest.bb.maps(fitlambest,fitlam0,sps.names, 
                                         sps1.lowchill,sps2.lowchill, 14,i)$BBsps2lam0
  
}



## explore preliminary results

plot(sps1[,1,1],sps1[,2,1],col=storeresultssps1[,1,1])
plot(sps2[,1,1],sps2[,2,1],col=storeresultssps2[,1,1])


## explore results

##### make plot ----

normalchill.sps1lam.mean =mean(unlist(listestsbb.1lam))
normalchill.sps1lam0.mean = mean(unlist(listestsbb.10))
lowchill.sps1lam.mean = mean(unlist(listestsbb.lowch.1lam))
lowchill.sps1lam0.mean =mean(unlist(listestsbb.lowch.10))

normalchill.sps1lam.ci = quantile(unlist(listestsbb.1lam))[c(2,4)]
normalchill.sps1lam0.ci = quantile(unlist(listestsbb.10))[c(2,4)]
lowchill.sps1lam.ci = quantile(unlist(listestsbb.lowch.1lam))[c(2,4)]
lowchill.sps1lam0.ci = quantile(unlist(listestsbb.lowch.10))[c(2,4)]

normalchill.sps2lam0.mean = mean(unlist(listestsbb.20))
normalchill.sps2lam.mean =mean(unlist(listestsbb.2lam))
lowchill.sps2lam.mean = mean(unlist(listestsbb.lowch.2lam))
lowchill.sps2lam0.mean =mean(unlist(listestsbb.lowch.20))

normalchill.sps2lam.ci = quantile(unlist(listestsbb.2lam))[c(2,4)]
normalchill.sps2lam0.ci = quantile(unlist(listestsbb.20))[c(2,4)]
lowchill.sps2lam.ci = quantile(unlist(listestsbb.lowch.2lam))[c(2,4)]
lowchill.sps2lam0.ci = quantile(unlist(listestsbb.lowch.20))[c(2,4)]



dev.off()



plot(x=NA, 
     y=NA, 
     xlab="predicted budbreak (DOY)",
     ylab="species", 
     pch=16, col=adjustcolor("darkgrey",0.4),cex=1.2, cex.lab=1.5,
     xlim=c(0,50),ylim=c(0,8)
     )
points(c(normalchill.sps1lam.mean,
         lowchill.sps1lam.mean,
         normalchill.sps1lam0.mean,
         lowchill.sps1lam0.mean),8:5,pch=19,col="lightgreen")
points(c(normalchill.sps1lam.ci,
         lowchill.sps1lam.ci,
         normalchill.sps1lam0.ci,
         lowchill.sps1lam0.ci),sort(rep(8:5,2),decreasing = T),pch=19,col="grey")

points(c(normalchill.sps2lam.mean,
         lowchill.sps2lam.mean,
         normalchill.sps2lam0.mean,
         lowchill.sps2lam0.mean),4:1,pch=19,col="darkgreen")
points(c(normalchill.sps2lam.ci,
         lowchill.sps2lam.ci,
         normalchill.sps2lam0.ci,
         lowchill.sps2lam0.ci),sort(rep(4:1,2),decreasing = T),pch=19,col="grey")


## Betula pendula
plot(density(estbbyear.1$BBsps1lam),lwd=2)
lines(density(estbbyear.1$BBsps1lam0),lwd=2,col="grey")

# Acer campestris
plot(density(estbbyear.1$BBsps2lam),lwd=2)
lines(density(estbbyear.1$BBsps2lam0),lwd=2,col="grey")




# end ----
