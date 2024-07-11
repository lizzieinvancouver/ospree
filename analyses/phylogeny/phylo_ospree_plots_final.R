## Started 9 September 2021 ##
## By Nacho, copies Nacho's Phylo_ospree_reanalyses.R code ##

## But edits it to be used for testing new stan models. ##

## This version has new attempts to run the code by Nacho:
## - tweaking priors
## - trying uncentered variables
## - adding chilling

#### remove objects (activate if needed)
#### remove objects (activate if needed)
#### get ready ####

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
runbbstanleadin=T
if(runbbstanleadin){
  # Flags to choose for bbstanleadin.R #
  setwd("..//bb_analysis") 
  getwd()
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
  
  
  # Get spps and VCVPHY in same order
  # bb.stan$spps[phylo$tip.label]
  phylo$tip.label
  d <- bb.stan[match(phylo$tip.label, bb.stan$spps),] # hmmm, only gives ONE match
  
  phymatch <- data.frame(tip=phylo$tip.label, sppnum=c(1:length(phylo$tip.label)))
  d <- merge(bb.stan, phymatch, by.x="spps", by.y="tip")
  d <- d[order(d$sppnum),]
  # Tilia_cordata versus Tilia_Cordata in phylo
  nspecies <- max(d$sppnum)
  
  ## remove outliers
  # d$resp
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
  
  
  
  ## remove names of species that are wrong (e.g. Juglans spp) 
  idswrong = which(d$spps == "Juglans_spp")
  d <- d[-idswrong,]
  phylo <- drop.tip(phylo, "Juglans_spp")
  
  nspecies <- length(phylo$tip.label)
  phymatch2 <- data.frame(tip=phylo$tip.label, sppnum=c(1:length(phylo$tip.label)))
  d2 <- merge(d, phymatch2, by.x="spps", by.y="tip")
  d2 <- d2[order(d2$sppnum.y),]
  d2$sppnum <- d2$sppnum.y
  d <- d2
  
  # save for faster loading-compiling
  #write.csv(d,file = "input/datasetforphyloms.csv")
  #write.tree(phylo, file = "input/phyloforphyloms.tre")
  
}


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

  
#### combining mu plots (Fig1) with phylogenetic structuring ----
  
  
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

indvarestchill = indvarest = cuechillsdup - cuechillsdlow 
indvarlam0chill = indvarlam0 = cuechillsdup0 - cuechillsdlow0
boxplot(cbind(PMM=indvarest,HMM=indvarlam0),
        ylab="Estimate uncertainty (days/std units of chilling)",outline=F,ylim=c(0,10),cex.lab=1.25)
mtext("a", side = 3, adj = 0.05,line=-2,cex=1.5)

indvarestforce = indvarest = cueforcesdup - cueforcesdlow 
indvarlam0force = indvarlam0 = cueforcesdup0 - cueforcesdlow0
boxplot(cbind(PMM=indvarest,HMM=indvarlam0),
        ylab="Estimate uncertainty (days/std units of forcing)",outline=F,ylim=c(0,10),cex.lab=1.25)
mtext("b", side = 3, adj = 0.05,line=-2,cex=1.5)

indvarestphoto = indvarest = cuephotosdup - cuephotosdlow 
indvarlam0photo = indvarlam0 = cuephotosdup0 - cuephotosdlow0
boxplot(cbind(PMM=indvarest,HMM=indvarlam0),
        ylab="Estimate uncertainty (days/std units of photoperiod)",
        outline=F,ylim=c(0,4),cex.lab=1.25)
mtext("c", side = 3, adj = 0.05,line=-2,cex=1.5)



mean(indvarest);mean(indvarlam0)

## save uncertainty table (Extended Data Fig.3)
#write.csv(cbind(indvarestchill,indvarlam0chill,indvarestforce
#,indvarlam0force,indvarestphoto,indvarlam0photo),file = "output/Extended_Data_Fig3.csv")


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


## save uncertainty table (Extended Data Fig.7)
#write.csv(cbind(cueforce,cueforce0,cuechill,cuechill0,cuephoto,cuephoto0),
#          file = "output/Extended_Data_Fig7.csv")





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


### plot correlations angio (Supplementary Fig. S3)----
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
#sps1 <- sps1[which(sps1[,1,1]==9.875 & sps1[,2,1]==48.125),3:4,1:11]
#sps1 <- as.data.frame(t(sps1))

# Acer campestris
sps2 <- Climate.in.range.list.all[[1]][[1]]
#sps2 <- sps2[which(sps2[,1,1]==9.875 & sps2[,2,1]==48.125),3:4,1:11]
#sps2 <- as.data.frame(t(sps2))

## Betula pendula low chilling
sps1.lowchill <- Climate.in.range.list.2sps[[7]][[1]]
#sps1.lowchill <- sps1.lowchill[which(sps1.lowchill[,1,1]==9.875 & sps1.lowchill[,2,1]==48.125),3:4,1:11]
#sps1.lowchill <- as.data.frame(t(sps1.lowchill))
#sps1.lowchill$Forcing <- sps1.lowchill$Forcing+2
sps1.lowchill[,3,] <- ifelse(is.na(sps1.lowchill[,3,]),NA,sps1.lowchill[,3,]+2)

# Acer campestris low chilling
sps2.lowchill <- Climate.in.range.list.2sps[[1]][[1]]
#sps2.lowchill <- sps2.lowchill[which(sps2.lowchill[,1,1]==9.875 & sps2.lowchill[,2,1]==48.125),3:4,1:11]
#sps2.lowchill <- as.data.frame(t(sps2.lowchill))
#sps2.lowchill$Forcing <- sps2.lowchill$Forcing+2
sps2.lowchill[,3,] <- ifelse(is.na(sps2.lowchill[,3,]),NA,sps2.lowchill[,3,]+2)

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

## loop to predict for a single site ----
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



##### make plot (single points comparing dates)----

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




## explore preliminary results ----

# make dataframe with data

dfsps1 = data.frame(longitude = sps1[,1,1], latitude = sps1[,2,1],
                    pmm = rowMeans(storeresultssps1[,1:11,1],na.rm=T), 
                    pmm2C = rowMeans(storeresultssps1[,1:11,3],na.rm=T),
                    lmm = rowMeans(storeresultssps1[,1:11,2],na.rm=T), 
                    lmm2C = rowMeans(storeresultssps1[,1:11,4],na.rm=T))

dfsps2 = data.frame(longitude = sps2[,1,1], latitude = sps2[,2,1],
                    pmm = rowMeans(storeresultssps2[,1:11,1],na.rm=T), 
                    pmm2C = rowMeans(storeresultssps2[,1:11,3],na.rm=T),
                    lmm = rowMeans(storeresultssps2[,1:11,2],na.rm=T), 
                    lmm2C = rowMeans(storeresultssps2[,1:11,4],na.rm=T))

dfsps1 = na.omit(dfsps1)
dfsps2 = na.omit(dfsps2)
summary(dfsps1$pmm)

#write.csv(dfsps1, file = "output/SourceData_Betpen_Figs4_ED2.csv")
#write.csv(dfsps2, file = "output/SourceData_Acecam_Figs4_ED2.csv")

# make color scale for plotting
library(dichromat)
library(rworldmap)
#install.packages("dichromat")
#newmap <- getMap(resolution="high") 
MrWhite_palette <-  colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b",
                                                "#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"))

cols <- MrWhite_palette(50)[as.numeric(cut(c(dfsps1$pmm,
                                             dfsps1$pmm2C,
                                             dfsps1$lmm,
                                             dfsps1$lmm2C),breaks = 50))]
cols2 <- MrWhite_palette(50)[as.numeric(cut(c(dfsps2$pmm,
                                              dfsps2$pmm2C,
                                              dfsps2$lmm,
                                              dfsps2$lmm2C),breaks = 50))]
colspmm <- cols[1:length(dfsps1$pmm)]
colspmm2C <- cols[(length(dfsps1$pmm)+1):(length(dfsps1$pmm)*2)]
colslmm <- cols[(length(dfsps1$pmm)*2+1):(length(dfsps1$pmm)*3)]
colslmm2C <- cols[(length(dfsps1$pmm)*3+1):(length(dfsps1$pmm)*4)]

colspmmsp2 <- cols2[1:length(dfsps2$pmm)]
colspmm2Csp2 <- cols2[(length(dfsps2$pmm)+1):(length(dfsps2$pmm)*2)]
colslmmsp2 <- cols2[(length(dfsps2$pmm)*2+1):(length(dfsps2$pmm)*3)]
colslmm2Csp2 <- cols2[(length(dfsps2$pmm)*3+1):(length(dfsps2$pmm)*4)]



##### make maps ----

#library(rworldmap)
#newmap = getMap(resolution="high")
dev.off()
par(mfrow=c(2,2))

#sps1 pmm
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Betpen PMM")
points(dfsps1$longitude,dfsps1$latitude,pch=19,cex=0.6,
       col=adjustcolor(colspmm,0.4))

#sps1 pmm low chill
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Betpen PMM2C")
points(dfsps1$longitude,dfsps1$latitude,pch=19,cex=0.6,
       col=adjustcolor(colspmm2C,0.4))

#sps1 lmm
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Betpen LMM")
points(dfsps1$longitude,dfsps1$latitude,pch=19,cex=0.6,
       col=adjustcolor(colslmm,0.4))

#sps1 lmm low chill
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Betpen LMM (+2C)")
points(dfsps1$longitude,dfsps1$latitude,pch=19,cex=0.6,
       col=adjustcolor(colslmm2C,0.4))

# legend
points(c(-9,-6,-3,0,3),rep(65,5),pch=15,cex=3,
       col=c("#9e0142","#f46d43","#fee08b","#66c2a5","#5e4fa2"))

#quantile(c(dfsps1$pmm,dfsps1$pmm2C,dfsps1$lmm,dfsps1$lmm2C))
text(c(-9,-6,-3,0,3),rep(67.5,5),
     labels=c("6.9","","22.7","","31.7"),cex=0.9)


dev.off()
par(mfrow=c(2,2))

#sps2 pmm
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acecam PMM")
points(dfsps2$longitude,dfsps2$latitude,pch=19,cex=0.6,
       col=adjustcolor(colspmmsp2,0.4))

#sps1 pmm low chill
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acecam PMM2C")
points(dfsps2$longitude,dfsps2$latitude,pch=19,cex=0.6,
       col=adjustcolor(colspmm2Csp2,0.4))

#sps1 lmm
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acecam LMM")
points(dfsps2$longitude,dfsps2$latitude,pch=19,cex=0.6,
       col=adjustcolor(colslmmsp2,0.4))

#sps1 lmm low chill
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acecam LMM (+2C)")
points(dfsps2$longitude,dfsps2$latitude,pch=19,cex=0.6,
       col=adjustcolor(colslmm2Csp2,0.4))

# legend
points(c(-9,-6,-3,0,3),rep(65,5),pch=15,cex=3,
       col=c("#9e0142","#f46d43","#fee08b","#66c2a5","#5e4fa2"))

#quantile(c(dfsps2$pmm,dfsps2$pmm2C,dfsps2$lmm,dfsps2$lmm2C))
text(c(-9,-6,-3,0,3),rep(67.5,5),
     labels=c("4.7","","33.9","","77.9"),cex=0.9)
gradient.rect(7,0,9,6,col=smoothColors("red",38,"blue"),border=NA)


## Make final maps ----
par(mfrow=c(1,3))
library(viridis)

## absolute of forecast:(PMM - PMM+2) - (LMM-LMM+2) (4)
dfsps1$absforecast = (dfsps1$pmm - dfsps1$pmm2C) - (dfsps1$lmm - dfsps1$lmm2C)
dfsps2$absforecast = (dfsps2$pmm - dfsps2$pmm2C) - (dfsps2$lmm - dfsps2$lmm2C)

#write.csv(dfsps1, file = "output/Betpen_forecast_figures.csv")
#write.csv(dfsps2, file = "output/Acecam_forecast_figures.csv")
colsabsforecast <- MrWhite_palette(50)[as.numeric(cut(c(dfsps1$absforecast,
                                                        dfsps2$absforecast),
                                                      breaks = 50))]
colsabsforecast <- rev(viridis(50))[as.numeric(cut(c(dfsps1$absforecast,
                                                        dfsps2$absforecast),
                                                      breaks = 50))]

colsabsforecastsps1 <- colsabsforecast[1:length(dfsps1$absforecast)]
colsabsforecastsps2 <- colsabsforecast[(length(dfsps1$absforecast)+1):length(colsabsforecast)]

# Betpen absolute forecast
dev.off()
par(mfrow=c(1,3))

plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Betula pendula")
points(dfsps1$longitude, dfsps1$latitude, pch=19, cex=0.3,
       col=adjustcolor(colsabsforecastsps1,1))
# Acecam absolute forecast
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acer campestre")
points(dfsps2$longitude, dfsps2$latitude, pch=19, cex=0.3,
       col=adjustcolor(colsabsforecastsps2,1))
#pmm vs lmm curr-2C
MrWhite_palette30 <-  colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b",
                                                  "#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"))(30)[1:30]
MrWhite_palette30 <-  viridis(30)[30:1]

hist(dfsps2$absforecast,30,border=MrWhite_palette30,
col=adjustcolor(MrWhite_palette30,0.8),
xlim=c(-1.5,0.1),
#ylim=c(0,15000),
main="",xlab="difference PMM(0C-2C)-LMM(0C-2C)",cex.lab=1.2)
hist(dfsps1$absforecast,1,col=adjustcolor(MrWhite_palette30[30],0.6),
border="black",add=T)
                                                  


#LMM_Acer - LMM_betula  (2)
dfsps1$coords = paste(dfsps1$longitude,dfsps1$latitude)
dfsps2$coords = paste(dfsps2$longitude,dfsps2$latitude)
coincidsps1 = which(dfsps1$coords %in% dfsps2$coords)
coincidsps2 = which(dfsps2$coords %in% dfsps1$coords)

dfsps2$lmmacervslmmbet = NA
dfsps2$lmmacervslmmbet[coincidsps2] = dfsps2$lmm[coincidsps2] - dfsps1$lmm[coincidsps1]


#PMM_Acer - PMM_betula (3)
dfsps2$pmmacervspmmbet = NA
dfsps2$pmmacervspmmbet[coincidsps2] = dfsps2$pmm[coincidsps2] - dfsps1$pmm[coincidsps1]

colsacervsbet <- MrWhite_palette(50)[as.numeric(cut(c(dfsps2$pmmacervspmmbet,
                                                        dfsps2$lmmacervslmmbet),
                                                      breaks = 50))]
colsacervsbetpmm <- colsacervsbet[1:length(dfsps2$pmmacervspmmbet)]
colsacervsbetlmm <- colsacervsbet[(length(dfsps2$pmmacervspmmbet)+1):length(colsacervsbet)]

# Acecam - Betpen pmm
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acecam PMM vs Betpen PMM")
points(dfsps2$longitude, dfsps2$latitude, pch=19, cex=0.6,
       col=adjustcolor(colsacervsbetpmm,0.4))

# Acecam - Betpen lmm
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acecam LMM vs Betpen LMM")
points(dfsps2$longitude, dfsps2$latitude, pch=19, cex=0.6,
       col=adjustcolor(colsacervsbetlmm,0.4))
hist(dfsps2$pmmacervspmmbet,col="grey",border=NA,
     xlim=c(-5,30),main="",xlab="BB diff. Acer - Betula")
hist(dfsps2$lmmacervslmmbet,col=adjustcolor("black",0.6),border=NA,add=T)


## plotting coloured histograms as legends

##### plotting colored histograms ----
library(RColorBrewer)
library(colorspace)
## setting values for density lines


dfsps1$pmmvslmm
dfsps2$pmmvslmm


## explore histograms
par(mfrow=c(2,4),mar=c(5,5,2,2))

hist(dfsps1$pmm,main = "Betpen PMM", xlab="predicted BB DOY",xlim=c(0,75))
abline(v=mean(dfsps1$pmm),lty=2,lwd=2)
hist(dfsps1$lmm,main = "Betpen LMM", xlab="predicted BB DOY",xlim=c(0,75))
abline(v=mean(dfsps1$lmm),lty=2,lwd=2)
hist(dfsps1$pmm2C,main = "Betpen PMM 2C", xlab="predicted BB DOY",xlim=c(0,75))
abline(v=mean(dfsps1$pmm2C),lty=2,lwd=2)
hist(dfsps1$lmm2C,main = "Betpen LMM 2C", xlab="predicted BB DOY",xlim=c(0,75))
abline(v=mean(dfsps1$lmm2C),lty=2,lwd=2)
hist(dfsps2$pmm,main = "Acecam PMM", xlab="predicted BB DOY",xlim=c(0,75))
abline(v=mean(dfsps2$pmm),lty=2,lwd=2)
hist(dfsps2$lmm,main = "Acecam LMM", xlab="predicted BB DOY",xlim=c(0,75))
abline(v=mean(dfsps2$lmm),lty=2,lwd=2)
hist(dfsps2$pmm2C,main = "Acecam PMM 2C", xlab="predicted BB DOY",xlim=c(0,75))
abline(v=mean(dfsps2$pmm2C),lty=2,lwd=2)
hist(dfsps2$lmm2C,main = "Acecam LMM 2C", xlab="predicted BB DOY",xlim=c(0,75))
abline(v=mean(dfsps2$lmm2C),lty=2,lwd=2)


dev.off()



## plotting histograms
#par(mfrow=c(2,2),mar=c(5,5,2,5))
dev.off()


#pmm vs lmm                                                
MrWhite_palette30 <-  colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b",
                                                  "#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"))(42)[1:42]
                                                  
MrWhite_palette30 <-  viridis(42)[42:1]


par(mfrow=c(1,2),mar=c(5,5,2,5))

hist(dfsps2$pmmvslmm,35,border=MrWhite_palette30,col=MrWhite_palette30,
     xlim=c(-8,2),main="",xlab="Budbreak difference PMM - LMM")
#hist(dfsps1$pmmvslmm,col=MrWhite_palette30[38],border=MrWhite_palette30[38],add=T)
hist(dfsps1$pmmvslmm,xlim=c(-0.1,0.1),30,col=MrWhite_palette30[38],border=MrWhite_palette30[38],add=F,main="",xlab="Budbreak difference PMM - LMM")

#pmm vs lmm curr-2C
MrWhite_palette30 <-  colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b",
                                                  "#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"))(30)[1:30]
                                                  
hist(dfsps2$absforecast,30,border=MrWhite_palette30,col=MrWhite_palette30,
     xlim=c(-1.6,0.2),main="",xlab="PMM - LMM, current - 2C")
hist(dfsps1$absforecast,col=MrWhite_palette30[30],border=MrWhite_palette30[30],add=T)
hist(dfsps1$absforecast,xlim=c(-0.02,0.02),30,col=MrWhite_palette30[30],border=MrWhite_palette30[30],add=F,main="",xlab="PMM - LMM, current - 2C")



#pmm vs lmm curr-2C
MrWhite_palette30 <-  colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b",
                                                  "#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"))(20)[1:20]
                                                  
hist(dfsps2$absforecast.std,30,border=MrWhite_palette30,col=MrWhite_palette30,
     #xlim=c(-1.6,1.2),
     main="",xlab="PMM - LMM, current - 2C")
hist(dfsps1$absforecast.std,col=MrWhite_palette30[11],border=MrWhite_palette30[11],add=T)


#acecam vs betpen
MrWhite_palette30 <-  colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b",
                                                  "#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"))(38)[1:38]
                                                  
hist(dfsps2$pmmacervspmmbet,30,border=adjustcolor(MrWhite_palette30,1),
     col=adjustcolor(MrWhite_palette30,1),
     xlim=c(-5,40),main="",xlab="BB diff. Acer - Betula")
hist(dfsps2$lmmacervslmmbet,30,
     col=adjustcolor(MrWhite_palette30[6:38],0.6),
     border=adjustcolor("black",1),add=T)
hist(dfsps2$lmmacervslmmbet,30, xlim=c(-5,40),
     col=adjustcolor(MrWhite_palette30[6:38],1),
     border=MrWhite_palette30[6:38],add=F,
     main="",xlab="BB diff. Acer - Betula")




## Chill portions - Supp Analysis ----

## load data
#fitlam0 <- readRDS("output/fit_priorupdate_noout_angio191_lamb0chillports.rds")
#fitlambest <- readRDS("output/fit_priorupdate_noout_angio191chillports.rds")

fitlam0 <- readRDS("D:/Data_Harvard/git sandbox/fit_priorupdate_noout_angio191_lamb0chillports.rds")
fitlambest <- readRDS("D:/Data_Harvard/git sandbox/fit_priorupdate_noout_angio191chillports.rds")
fitlam0v2 <- readRDS("D:/Data_Harvard/git sandbox/fit_priorupdate_noout_angio191_lamb0chillports_v2.rds")
fitlambestv2 <- readRDS("D:/Data_Harvard/git sandbox/fit_priorupdate_noout_angio191chillports_v2.rds")
fitlam0orig <- readRDS("output/fit_priorupdate_noout_angio191_lamb0.rds")
fitlambestorig <- readRDS("output/fit_priorupdate_noout_angio191.rds")


## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
tableresults.0 = summary(fitlam0, pars = list("a_z", "sigma_interceptsa", "b_zf", "sigma_interceptsbf", "b_zc", "sigma_interceptsbc", "b_zp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults.est = summary(fitlambest, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults.0v2 = summary(fitlam0v2, pars = list("a_z", "sigma_interceptsa", "b_zf", "sigma_interceptsbf", "b_zc", "sigma_interceptsbc", "b_zp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults.estv2 = summary(fitlambestv2, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults.0orig = summary(fitlam0orig, pars = list("a_z", "sigma_interceptsa", "b_zf", "sigma_interceptsbf", "b_zc", "sigma_interceptsbc", "b_zp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults.estorig = summary(fitlambestorig, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary

## get table comparing results
cbind(tableresults.est[c(1,4,7,10,2,5,8,11,3,6,9,12,13),1],
      tableresults.estv2[c(1,4,7,10,2,5,8,11,3,6,9,12,13),1],
      tableresults.estorig[c(1,4,7,10,2,5,8,11,3,6,9,12,13),1])

## get table comparing results (non-phylo)
cbind(tableresults.0[c(1,3,5,7,2,4,6,8,9),1],
      tableresults.0v2[c(1,3,5,7,2,4,6,8,9),1],
      tableresults.0orig[c(1,3,5,7,2,4,6,8,9),1])

## save definitive tables

#write.csv(tableresults.estv2[c(1,4,7,10,2,5,8,11,3,6,9,12,13),c(1,3,4,6,8:10)], file = "output/angio_noout_lambest191_chillports.csv")
#write.csv(tableresults.0v2[c(1,3,5,7,2,4,6,8,9),c(1,3,4,6,8:10)], file = "output/angio_noout_lamb0_191_chillports.csv")


## Comparing R2 among PMM HMM - Supp Analysis ----

fitlambest <- readRDS("~/MEGA/Work_UAH_BeaGal/SIDE PROJECTS/OSPREE/Phylogeny/outputs/testme_yhat_noout.rds")

#fitlam0 <- readRDS("output/testme_yhat_noout_lamb0.rds")
#fitlambest <- readRDS("output/testme_yhat_noout.rds")
## Function to compute R2
bayes_R2_mine <- function(d,yhat) {
  y <- d$resp
  ypred <- yhat
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  R2post <- var_ypred / (var_ypred + var_e)
  return(R2post)
}

list_of_draws <- extract(fitlambest)
list_of_draws0 <- extract(fitlam0)

## compare R2 for supp figure
boxplot(cbind(bayes_R2_mine(d,list_of_draws$yhat),
              bayes_R2_mine(d,list_of_draws0$yhat)),ylab="Bayes R2")

write.csv(bayes_R2_mine(d,list_of_draws$yhat),file = "output/Extended_Data_Fig4.csv")

## Leave One Clade Out (LOCO) analyses - Supp Analysis ----


## load models, data, phylogeny
#d <- read.csv("E:/OSPREE/phylodata.csv")
#phylo = read.tree("input/phyloforphyloms.tre")
#rm(list=ls())
#memory.limit(size=32000)


## Load list of model outputs
#fitlist <- readRDS("E:/OSPREE/fit_list_leave_clade_out.rds")
#fitlist.orig <- readRDS("E:/OSPREE/fit_list_leave_clade_out_all.rds")
#fitlist <- readRDS("E:/OSPREE/fit_list_leave_clade_out_lamb0.rds")
#fitlist.orig <- readRDS("E:/OSPREE/fit_list_leave_clade_out_lamb0_all.rds")
names(fitlist[[1]])


##### Predict for held out clade in each LOCO run ----

#from Lizzie's email oct7th
# For the PMM: grab your full VCV (from the whole tree, 
# not the subset), and all your parameters (from the model based 
# on the subset) and then use those to get an intercept and slope 
# for each of your hold out species. Something like:
library(geiger)
#install.packages("Rphylopars")
#install.packages("doBy")
library(Rphylopars)


# grab parameters from subset model: 
fitlistall <- readRDS("E:/OSPREE/fit_list_leave_clade_out.rds")



## impute using Rphylopars for phylo model PMM.
listpredparam <- list()
generatoprune <- names(sort(table(d$genus), decreasing=T))[1:25]

for(i in 1:25){
  
  print(generatoprune[i])
  spsingenus_i <- unique(subset(d, genus == generatoprune[i])$spps)
  
  if(length(spsingenus_i)>1){
    fitlist = fitlistall[[i]]
    
    
    # retrieve and predict useful parameters
    interceptsa <- extract(fitlist)$lam_interceptsa
    a_z <- extract(fitlist)$a
    sigma_a <- extract(fitlist)$sigma_interceptsa
    lam_interceptsbf <- extract(fitlist)$lam_interceptsbf
    mean(interceptsa)
    b_zf <- extract(fitlist)$b_zf
    sigma_b <- extract(fitlist)$sigma_interceptsbf
    b_force <- extract(fitlist)$b_force
    
    
    # Generate intercept
    ## first rescale tree
    scaledtree_intercept_a <- rescale(phylo, model = "lambda", mean(interceptsa))
    scaledtree_intercept_b <- rescale(phylo, model = "lambda", mean(lam_interceptsbf))
    
    phylosub <- drop.tip(phylo, spsingenus_i)
    trait_data_sub <- data.frame(species= phylosub$tip.label, 
                                 V1=colMeans(a_z),
                                 V2=colMeans(b_force))
    spps  <- data.frame(species= phylo$tip.label)
    trait_data <- merge(spps, trait_data_sub, by.x="species", by.y = "species", all= T)
    
    PPE_a <- phylopars(trait_data = trait_data[,c(1,2)], tree = scaledtree_intercept_a, model = "lambda",
                       pheno_error = TRUE, phylo_correlated = TRUE, pheno_correlated = F)
    PPE_b <- phylopars(trait_data = trait_data[,c(1,3)], tree = scaledtree_intercept_b, model = "lambda",
                       pheno_error = TRUE, phylo_correlated = TRUE, pheno_correlated = F)
    
    listpredparam[[i]] <- data.frame(intercept=PPE_a$anc_recon[1:191,],
                                     slope=PPE_b$anc_recon[1:191,])
    # Ancestral state reconstruction and species mean prediction
    
  }
}


listpredparam.phylo <- readRDS("E:/OSPREE/LOCO.predicted.parameters.rds")

listpredparam[[1]]

## impute using random draws for non-phylo model HDM.
#For the DHM, you just have to take random draws from the intercept 
#normal (rnorm(n hold-out-spp, mu_a, sigma_a)) and slope normal (rnorm(n 
#hold-out-spp, mu_b, sigma_b)) then -- again -- use your new 
#species-level slopes and the hold out species forcing (x data) to 
#predict the hold out data.

# grab parameters from subset model: 
fitlistall <- readRDS("E:/OSPREE/fit_list_leave_clade_out_lamb0.rds")



## impute using Rphylopars for phylo model PMM.
listpredparam <- list()
generatoprune <- names(sort(table(d$genus), decreasing=T))[1:25]

for(i in 1:25){#i=1
  
  print(generatoprune[i])
  spsingenus_i <- unique(subset(d, genus == generatoprune[i])$spps)
  
  if(length(spsingenus_i)>1){
    fitlist = fitlistall[[i]]
    tableresults.0 = summary(fitlist, pars = list("a_z", "sigma_interceptsa", 
                                                  "b_zf", "sigma_interceptsbf",
                                                  "sigma_y"))$summary
    
    # retrieve and predict useful parameters
    a_z <- extract(fitlist)$a_z
    a_each <- extract(fitlist)$a
    sigma_a <- extract(fitlist)$sigma_interceptsa
    b_zf <- extract(fitlist)$b_zf
    sigma_b <- extract(fitlist)$sigma_interceptsbf
    b_force <- extract(fitlist)$b_force
    
    
    # Generate data to store
    phylosub <- drop.tip(phylo, spsingenus_i)
    trait_data_sub <- data.frame(species= phylosub$tip.label, 
                                 V1=colMeans(a_each),
                                 V2=colMeans(b_force))
    spps  <- data.frame(species= phylo$tip.label)
    trait_data <- merge(spps, trait_data_sub, by.x="species", by.y = "species", all= T)
    
    # Generate intercepts and slopes
    holder <- which(trait_data[,1]%in% spsingenus_i)
    trait_data[holder,2] <-  mean(rnorm(10000, mean(a_z),mean(sigma_a)))
    trait_data[holder,3] <-  mean(rnorm(10000, mean(b_zf),mean(sigma_b)))
    
    
    listpredparam[[i]] <- trait_data

  }
}

#saveRDS(listpredparam, file="E:/OSPREE/LOCO.predicted.parameterslamb0.rds")
listpredparam.nonphylo <- readRDS("E:/OSPREE/LOCO.predicted.parameterslamb0.rds")



#Either, way use your new species-level slopes and the hold out species 
#forcing (x data) to predict the hold out data.
storeobsvspred <- list()

for(i in 1:25){#i=1
  
  print(generatoprune[i])
  spsingenus_i <- unique(subset(d, genus == generatoprune[i])$spps)
  
  nsps <- length(spsingenus_i)
  if(nsps>1){
  predparam0 <- listpredparam.nonphylo[[i]]
  predparam <- listpredparam.phylo[[i]]
  storeobsvspred_j <- list()
  for(j in 1:nsps){#j=1
    
    d_j <- subset(d, spps == spsingenus_i[j])
    predparam0_j <- predparam0[which(predparam0[,1] == spsingenus_i[j]),]
    predparam_j <- predparam[which(row.names(predparam) == spsingenus_i[j]),]
    observed.resp <- d_j$resp
    
    predicted.respHDM <- as.numeric(predparam0_j[3])*d_j$force.z + as.numeric(predparam0_j[2])   
    predicted.respPMM <-  as.numeric(predparam_j[2])*d_j$force.z + as.numeric(predparam_j[1])
      
    storeobsvspred_j[[j]] <- data.frame(species = spsingenus_i[j], 
               obs = observed.resp,
               predHDM = predicted.respHDM,
               predPMM = predicted.respPMM)
    
  }
  storeobsvspred[[i]] <- do.call(rbind,storeobsvspred_j)
  
  }
}


LOO_obs_pred <- do.call(rbind,storeobsvspred)
#write.csv(LOO_obs_pred, file = "output/LOCO_observed_vs_predicted.csv")
LOO_obs_pred <- read.csv("output/LOCO_observed_vs_predicted.csv")[,-1]


## plot results
par(mfrow=c(1,2))
plot(LOO_obs_pred[,3],LOO_obs_pred[,2],
     ylab = "observed response (num. days)",
     xlab = "predicted by HMM (num. days)",
     pch=19, col=adjustcolor(1,0.5),
     ylim=c(0,150),xlim=c(0,70))
abline(lm(LOO_obs_pred[,2]~LOO_obs_pred[,3]),col="red")
mtext("a", side = 3, adj = 0.05,line=0.5,cex=1.5)
text(10,150, paste("r = ", round(cor(LOO_obs_pred[,2],LOO_obs_pred[,3]),3)))

plot(LOO_obs_pred[,4],LOO_obs_pred[,2],
     ylab = "observed response (num. days)",
     xlab = "predicted by PMM (num. days)",
     pch=19, col=adjustcolor(1,0.5),
     ylim=c(0,150),xlim=c(0,70))
abline(lm(LOO_obs_pred[,2]~LOO_obs_pred[,4]),col="red")
mtext("b", side = 3, adj = 0.05,line=0.5,cex=1.5)
text(10,150, paste("r = ", round(cor(LOO_obs_pred[,2],LOO_obs_pred[,4]),3)))




##### Loop to compare R2 for each LOCO run ----

## Loop to get posterior R2 for each LOO run
generatoprune <- names(sort(table(d$genus), decreasing=T))[1:25]
r2s_posterior <- array(NA, dim=c(8000, 25))
colnames(r2s_posterior)<-generatoprune

for(i in 1:25){#i=1
  print(generatoprune[i])
  d_i <- d
  spsingenus_i <- unique(subset(d_i, genus == generatoprune[i])$spps)

  if(length(spsingenus_i) > 1){
    
    ## subset and re-order data after removing each genus
    dsub <- subset(d_i, genus != generatoprune[i])
    phylosub <- drop.tip(phylo, spsingenus_i)
    nspecies <- length(phylosub$tip.label)
    phymatch <- data.frame(tip=phylosub$tip.label, sppnum=c(1:length(phylosub$tip.label)))
    d2 <- merge(dsub, phymatch, by.x="spps", by.y="tip")
    d2 <- d2[,-c(36:38)]
    d2 <- d2[order(d2$sppnum.y),]
    
    ## retrieve yhats
    yhat <- extract(fitlist[[i]])$yhat
    dim(yhat)
    yhat[1:10, 1:100]
    ## compute and save R2
    
    bayesR2_i <- bayes_R2_mine(d2,yhat)
    r2s_posterior[,i] <- bayesR2_i 
        
  }
}


# save outputs for quick comparative analyses
#write.csv(r2s_posterior, file = "output/posterior_r2_nonphyloLOO.csv")
#write.csv(r2s_posterior, file = "output/posterior_r2_phyloLOO.csv")

r2s_posterior0 <- read.csv("output/posterior_r2_nonphyloLOO.csv")
r2s_posterior <- read.csv("output/posterior_r2_phyloLOO.csv")

r2resultsummary <- data.frame(HMM = colMeans(r2s_posterior0)[2:26],
           PMM = colMeans(r2s_posterior)[2:26],
           leftoutclade = names(colMeans(r2s_posterior)[2:26]))

#write.csv(r2resultsummary, file = "output/posterior_r2_summary.csv")




##### Loop to compare estimates yhats and slopes ----
generatoprune <- names(sort(table(d$genus), decreasing=T))[1:25]
yhats_bf_posterior <- array(NA, dim=c(25, 2))
row.names(yhats_bf_posterior)<-generatoprune
colnames(yhats_bf_posterior)<-c("yhats","b_force")

for(i in 1:25){#i=1
  print(generatoprune[i])
  spsingenus_i <- unique(subset(d_i, genus == generatoprune[i])$spps)
  torem <- which(d$genus==generatoprune[i])
  spstorem <- which(phylo$tip.label %in% spsingenus_i)
  yhat.all <- extract(fitlist.orig)$yhat[,-torem]
  bforce.all <- extract(fitlist.orig)$b_force[,-spstorem]
  
  
  if(length(spsingenus_i) > 1){
    
    ## retrieve yhats
    yhat <- extract(fitlist[[i]])$yhat
    bforce <- extract(fitlist[[i]])$b_force
    
    yhats_bf_posterior[i,1] <- cor(colMeans(yhat),colMeans(yhat.all))
    yhats_bf_posterior[i,2] <- cor(colMeans(bforce),colMeans(bforce.all))
    
  }
}

# save outputs for quick comparative analyses
#write.csv(yhats_bf_posterior, file = "output/posterior_yhats_bf_phyloLOO.csv")
#write.csv(yhats_bf_posterior, file = "output/posterior_yhats_bf_nonphyloLOO.csv")

yhats_bf_posterior<-read.csv("output/posterior_yhats_bf_phyloLOO.csv")
yhats_bf_posterior0<-read.csv("output/posterior_yhats_bf_nonphyloLOO.csv")
dev.off()
boxplot(cbind(PMM=yhats_bf_posterior[,3],
              HMM=yhats_bf_posterior0[,3]),
        ylab="Correlation sensitivity to forcing vs. LOO")

#write.csv(cbind(PMM=yhats_bf_posterior[,3],
#HMM=yhats_bf_posterior0[,3]), file = "output/Extended_Data_Fig6.csv")


##### Loop to measure distance in days among slopes full vs LOO ----
generatoprune <- names(sort(table(d$genus), decreasing=T))[1:25]
bfdiffdays_posterior <- array(NA, dim=c(25, 4))
row.names(bfdiffdays_posterior)<-generatoprune
colnames(bfdiffdays_posterior)<-c("b_force_diffdaysavg",
                                  "b_force_absdiffdaysavg",
                                  "b_force_diffdayssum",
                                  "b_force_diffdaysSD")

for(i in 1:25){#i=1
  print(generatoprune[i])
  spsingenus_i <- unique(subset(d_i, genus == generatoprune[i])$spps)
  torem <- which(d$genus==generatoprune[i])
  spstorem <- which(phylo$tip.label %in% spsingenus_i)
  yhat.all <- extract(fitlist.orig)$yhat[,-torem]
  bforce.all <- extract(fitlist.orig)$b_force[,-spstorem]
  
  
  if(length(spsingenus_i) > 1){
    
    ## retrieve yhats
    yhat <- extract(fitlist[[i]])$yhat
    bforce <- extract(fitlist[[i]])$b_force
    
    bfdiffdays_posterior[i,1] <- mean(colMeans(bforce)-colMeans(bforce.all))
    bfdiffdays_posterior[i,2] <- mean(abs(colMeans(bforce)-colMeans(bforce.all)))
    bfdiffdays_posterior[i,3] <- sum(abs(colMeans(bforce)-colMeans(bforce.all)))
    bfdiffdays_posterior[i,2] <- sd(colMeans(bforce)-colMeans(bforce.all))
    
  }
}

# save outputs for quick comparative analyses
#write.csv(bfdiffdays_posterior, file = "output/posterior_bfdiffdays_phyloLOO.csv")
#write.csv(bfdiffdays_posterior, file = "output/posterior_bfdiffdays_nonphyloLOO.csv")

bfdiffdays_posterior <- read.csv("output/posterior_bfdiffdays_phyloLOO.csv")
bfdiffdays_posterior0 <- read.csv("output/posterior_bfdiffdays_nonphyloLOO.csv")

cbind(bfdiffdays_posterior[,3],
      bfdiffdays_posterior0[,3])

colSums(bfdiffdays_posterior[,2:4],na.rm = T)
colSums(bfdiffdays_posterior0[,2:4],na.rm = T)

colMeans(bfdiffdays_posterior[,2:4],na.rm = T)
colMeans(bfdiffdays_posterior0[,2:4],na.rm = T)


boxplot(bfdiffdays_posterior[,2],bfdiffdays_posterior0[,2])



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



plot(density(yhat[1,]),col=adjustcolor("lightblue",0.05))
for(i in 1:100){
  ff=sample(1:4000,1)
  print(ff)
  lines(density(yhat[ff,]),col=adjustcolor("lightblue",0.05))
}
lines(density(d$resp),col="darkblue",lwd=2)  



## Removing polytomies Sensitivity analyses  ----
## load models
  fitlambest <- readRDS("output/fit_priorupdate_noout_angio158_nopolyt.rds")

## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
tableresults.est = summary(fitlambest, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary

#write.csv(tableresults.est[c(1,4,7,10,2,5,8,11,3,6,9,12,13),], file = "output/angio_noout_lamest1158nopolit.csv")



## Old code for mapping ----

## PMM vs LMM comparison
dfsps1$pmmvslmm = dfsps1$pmm-dfsps1$lmm
dfsps2$pmmvslmm = dfsps2$pmm-dfsps2$lmm
colspmmvslmm <- MrWhite_palette(50)[as.numeric(cut(c(dfsps1$pmmvslmm,
                                                     dfsps2$pmmvslmm),
                                                   breaks = 50))]
colspmmvslmmsps1 <- colspmmvslmm[1:length(dfsps1$pmmvslmm)]
colspmmvslmmsps2 <- colspmmvslmm[(length(dfsps1$pmmvslmm)+1):length(colspmmvslmm)]

dev.off()
# Betpen PMM vs LMM
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Betpen PMM vs LMM")
points(dfsps1$longitude, dfsps1$latitude, pch=19, cex=0.6,
       col=adjustcolor(colspmmvslmmsps1,0.4))

# Acecam PMM vs LMM
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acecam PMM vs LMM")
points(dfsps2$longitude, dfsps2$latitude, pch=19, cex=0.6,
       col=adjustcolor(colspmmvslmmsps2,0.4))
hist(dfsps2$pmmvslmm,col="grey",border=NA,
     xlim=c(-8,2),main="",xlab="Budbreak difference PMM - LMM")
hist(dfsps1$pmmvslmm,col="black",border=NA,add=T)



#### 1.1. absolute of forecast ratio:(PMM - PMM+2)/(LMM-LMM+2) (4)----
dfsps1$absforecast.ratio = (dfsps1$pmm - dfsps1$pmm2C)/(dfsps1$lmm - dfsps1$lmm2C)
dfsps2$absforecast.ratio = (dfsps2$pmm - dfsps2$pmm2C)/(dfsps2$lmm - dfsps2$lmm2C)
qq = quantile(dfsps2$absforecast.ratio,c(0.025,0.975))
dfsps2$absforecast.ratio[which(dfsps2$absforecast.ratio<qq[1])] = NA
dfsps2$absforecast.ratio[which(dfsps2$absforecast.ratio>qq[2])] = NA
summary(dfsps1$absforecast.ratio)

hist(dfsps2$absforecast.ratio)
colsabsforecast <- MrWhite_palette(50)[as.numeric(cut(c(dfsps1$absforecast.ratio,
                                                        dfsps2$absforecast.ratio),
                                                      breaks = 50))]
colsabsforecastsps1 <- colsabsforecast[1:length(dfsps1$absforecast)]
colsabsforecastsps2 <- colsabsforecast[(length(dfsps1$absforecast)+1):length(colsabsforecast)]

# Betpen absolute forecast ratio
dev.off()
par(mfrow=c(1,3))

plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Betula pendula")
points(dfsps1$longitude, dfsps1$latitude, pch=19, cex=0.8,
       col=adjustcolor(colsabsforecastsps1,0.6))

# Acecam absolute forecast ratio
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acer campestris")
points(dfsps2$longitude, dfsps2$latitude, pch=19, cex=0.8,
       col=adjustcolor(colsabsforecastsps2,0.6))


#pmm vs lmm curr-2C
MrWhite_palette30 <-  colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b",
                                         "#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"))(42)[1:42]
hist(dfsps2$absforecast,30,border=MrWhite_palette30,
     col=adjustcolor(MrWhite_palette30,0.8),
     #xlim=c(-1.6,0.2),
     #ylim=c(0,15000),
     main="",xlab="ratio PMM(0C-2C)/LMM(0C-2C)",cex.lab=1.2)
hist(dfsps1$absforecast.ratio,30,col=adjustcolor(MrWhite_palette30[20:23],0.4),
     border="black",add=T)




## absolute of forecast:(PMM - PMM+2) - (LMM-LMM+2) (4)
dfsps1$absforecast = abs((dfsps1$pmm - dfsps1$pmm2C) - (dfsps1$lmm - dfsps1$lmm2C))
dfsps1$lmmcurrvs2C = abs(dfsps1$lmm - dfsps1$lmm2C)
dfsps2$absforecast = abs((dfsps2$pmm - dfsps2$pmm2C) - (dfsps2$lmm - dfsps2$lmm2C))
dfsps2$lmmcurrvs2C = abs(dfsps2$lmm - dfsps2$lmm2C)
dfsps1$absforecast.std = dfsps1$absforecast/dfsps1$lmmcurrvs2C 
dfsps2$absforecast.std = dfsps2$absforecast/dfsps2$lmmcurrvs2C 
summary(dfsps2$absforecast.std)

mean(dfsps1$lmmcurrvs2C)
mean(dfsps2$lmmcurrvs2C)


dfsps2$absforecast.std[dfsps2$absforecast.std>quantile(dfsps2$absforecast.std,0.975)]=NA

colsabsforecast <- MrWhite_palette(50)[as.numeric(cut(c(dfsps1$absforecast.std,
                                                        dfsps2$absforecast.std),
                                                      breaks = 50))]
colsabsforecastsps1 <- colsabsforecast[1:length(dfsps1$absforecast.std)]
colsabsforecastsps2 <- colsabsforecast[(length(dfsps1$absforecast.std)+1):length(colsabsforecast)]

# Betpen absolute forecast
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Betula pendula")
points(dfsps1$longitude, dfsps1$latitude, pch=19, cex=0.8,
       col=adjustcolor(colsabsforecastsps1,0.6))

# Acecam absolute forecast
plot(newmap, col="lightgrey",xlim=c(-9,40),ylim=c(35,69),border="lightgrey",
     main="Acer campestris")
points(dfsps2$longitude, dfsps2$latitude, pch=19, cex=0.8,
       col=adjustcolor(colsabsforecastsps2,0.6))
#pmm vs lmm curr-2C
MrWhite_palette30 <-  colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b",
                                         "#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"))(39)[1:39]
hist(dfsps2$absforecast.std,30,border=MrWhite_palette30,
     col=adjustcolor(MrWhite_palette30,0.8),
     #xlim=c(-1.6,0.2),
     #ylim=c(0,15000),
     main="",xlab="PMM(0C-2C)-LMM(0C-2C)/LMM(0C-2C)",cex.lab=1.2)
hist(dfsps1$absforecast.std,30,col=adjustcolor(MrWhite_palette30,0.6),
     border="black",add=T)


# end ----
