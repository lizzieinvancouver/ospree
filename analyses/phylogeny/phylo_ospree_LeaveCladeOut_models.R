## Started mid November 2022 ##
## From files started September 2021 (that copied Nacho's Phylo_ospree_reanalyses.R code)
## By Nacho, with some edits by Lizzie ##

## Runs (or reads) the phylogeny models, extracts some output
## Does some basic plotting

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
library(caper)
library(pez)
library(phytools)
library(rstan)
library(shinystan)
library(plyr)
library(dplyr)

options(mc.cores = parallel::detectCores())

#'###############################
# Flags for how to run the code #
#'###############################
runmodels <- T
runbbstanleadin <- T # leave as false to speed up Supp and ms. compilation

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
#d$chill.z = as.numeric(scale(d$chill.ports))


# save-load for faster loading-compiling
#write.csv(d,file = "input/datasetforphyloms.csv")
#write.tree(phylo, file = "input/phyloforphyloms.tre")
#d = read.csv("input/datasetforphyloms.csv")
#phylo = read.tree("input/phyloforphyloms.tre")

#d$chill.z = as.numeric(scale(d$chill.ports))


#'###################################
# Run or read in the models      ####
#'###################################

#remove genera with more than one species (top 25 genera)

generatoprune <- names(sort(table(d$genus), decreasing=T))
listmodels <- list()
listmodelslamb0 <- list()
runphylomodel=F
for(i in 1:25){#i=1
 print(generatoprune[i])
  d_i <- d
 spsingenus_i <- unique(subset(d_i, genus == generatoprune[i])$spps)
 
 if(length(spsingenus_i) > 1){
 dsub <- subset(d_i, genus != generatoprune[i])
 phylosub <- drop.tip(phylo, spsingenus_i)
 nspecies <- length(phylosub$tip.label)
 ## Fit model here ... using code for which Lizzie updated priors
 
 phymatch <- data.frame(tip=phylosub$tip.label, sppnum=c(1:length(phylosub$tip.label)))
 d2 <- merge(dsub, phymatch, by.x="spps", by.y="tip")
 d2 <- d2[order(d2$sppnum.y),]
 
 
 if(runphylomodel){
 fit <- stan("stan/uber_threeslopeintercept_modified_cholesky_updatedpriors_leavecladeout.stan",
             data=list(N=nrow(d2),
                       n_sp=nspecies,
                       sp=d2$sppnum.y,
                       x1=d2$force.z,
                       y=d2$resp,
                       Vphy=vcv(phylosub, corr = TRUE)),
             iter = 4000,
             warmup = 2000, # half the iter as warmp is default, but leaving in case we want to change
             chains = 4,
             seed = 117 
 )
 }
 ## Save fitted posterior
 #listmodels[[i]]<-fit
 #saveRDS(fit, "output/fit_priorupdate_noout_angio191chillports.rds")
 #saveRDS(listmodels, "output/fit_list_leave_clade_out.rds")
 #listmodels[[5]]
 
 
 
 fitlamb0 <- stan("stan/uber_threeslopeintercept_modified_cholesky_updatedpriors_lamb0_leavecladeout.stan",
                  data=list(N=nrow(d2),
                            n_sp=nspecies,
                            sp=d2$sppnum.y,
                            x1=d2$force.z,
                            y=d2$resp,
                            Vphy=vcv(phylosub, corr = TRUE)),
                  iter = 4000,
                  warmup = 2000, 
                  chains = 4,
                  seed = 117 
 )
 
 #saveRDS(fitlamb0, "output/fit_priorupdate_noout_angio191_lamb0chillports.rds")
 listmodelslamb0[[i]]<-fitlamb0
 }
 
 }
saveRDS(listmodelslamb0, "output/fit_list_leave_clade_out_lamb0.rds")




  

  
#'###################################
# Analyze model fit            ####
#'###################################

## Summarize full fit
# summary(fit)$summary

## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
fitsum <- summary(fit, pars = list("a_z", "sigma_interceptsa", 
                                   "b_zf", "sigma_interceptsbf", "lam_interceptsbf", 
                                   "b_zc", "sigma_interceptsbc", "lam_interceptsbc",
                                   "b_zp", "sigma_interceptsbp", "lam_interceptsbp","sigma_y"))$summary

fitsumdf <- as.data.frame(fitsum)

source("source/stan_utility.R")
check_all_diagnostics(fit)


## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
tableresults = summary(fit, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults0 = summary(fitlamb0, pars = list("a_z", #"lam_interceptsa", 
                                              "sigma_interceptsa", "b_zf", 
                                              #"lam_interceptsbf", 
                                              "sigma_interceptsbf", "b_zc", 
                                              #"lam_interceptsbc", 
                                              "sigma_interceptsbc", "b_zp", 
                                              #"lam_interceptsbp", 
                                              "sigma_interceptsbp", "sigma_y"))$summary

#write.csv(tableresults[c(1,4,7,10,2,5,8,11,3,6,9,12,13),c(1,3,4,6,8:10)], file = "output/angio_noout_lambest191.csv")
#write.csv(tableresults0[c(1,3,5,7,2,4,6,8,9),c(1,3,4,6,8:10)], file = "output/angio_noout_lamb0_191.csv")

# save citable values in text
chillsens = abs(round(tableresults["b_zc","mean"],1))
photosens = abs(round(tableresults["b_zp","mean"],1))
forcesens = abs(round(tableresults["b_zf","mean"],1))

phylosigchill = abs(round(tableresults["lam_interceptsbc","mean"],2))
phylosigphoto = abs(round(tableresults["lam_interceptsbp","mean"],2))
phylosigforce = abs(round(tableresults["lam_interceptsbf","mean"],2))



#'####################
#' Code to pull posteriors of each cue and get quantiles  ----
#' We may not actually need this now, but we may someday!
#'####################

postfit <- extract(fit)
whichquantiles <- c(0.05, 0.5, 0.95) # need to give three to work with below code

# grab the quantiles (giving df cols generic names so we can switch it up)
# extract posteriors for species-level estimates
chillarray <- postfit$b_chill
forcearray <- postfit$b_force
photoarray <- postfit$b_photo
intarray <- postfit$a

# create dataframe to fill (ALERT: goes chill, force, photo, intercept)
lengthhere <- ncol(chillarray)
modquant <- data.frame(spnum=c(paste("sp", c(1:(4*lengthhere)), sep="")),
                       cue=rep(c("chill", "force", "photo", "intercept"), each=lengthhere),
                       qlow=rep(0, 4*lengthhere), qmid=rep(0, 4*lengthhere),
                       qhigh=rep(0, 4*lengthhere))
arrays <- list(chillarray, forcearray, photoarray, intarray)

for(whicharray in c(1:length(arrays))){
  arrayhere <- arrays[[whicharray]]
  for(i in c(1:ncol(arrayhere))){
    quanthere <- quantile(arrayhere[,i], probs = whichquantiles)
    if(whicharray==1){start <- 0}
    if(whicharray==2){start <- lengthhere}
    if(whicharray==3){start <- lengthhere*2}
    if(whicharray==4){start <- lengthhere*3}
    modquant$qlow[start+i] <- quanthere[[1]]
    modquant$qmid[start+i] <- quanthere[[2]]
    modquant$qhigh[start+i] <- quanthere[[3]]
  }
}

# get min and max by cue ...
# Can use someday with \Sexpr{} to get actual y #s on comparisons we make in paper

minmaxcues <-
  ddply(modquant, c("cue"), summarise,
        max = max(qmid),
        min = min(qmid))

minmaxcues$diff <- minmaxcues$min - minmaxcues$max
minmaxcues$xdiff <- minmaxcues$min/minmaxcues$max

chillphotocue <- fitsumdf$mean[which(rownames(fitsumdf)=="b_zc")]/fitsumdf$mean[which(rownames(fitsumdf)=="b_zp")]

#'####################
#' Estimate uncertainty for each species -- but across them ----
#'###################

## Average the species posteriors ... (keep iterations together, again would be good if someone checked this...)
chillspmean <- rowSums(chillarray)/ncol(chillarray)
forcespmean <- rowSums(forcearray)/ncol(forcearray)
photospmean <- rowSums(photoarray)/ncol(photoarray)

postfitlamb0 <- extract(fitlamb0)

chillarray0 <- postfitlamb0$b_chill
forcearray0 <- postfitlamb0$b_force
photoarray0 <- postfitlamb0$b_photo
chillspmean0 <- rowSums(chillarray0)/ncol(chillarray0)
forcespmean0 <- rowSums(forcearray0)/ncol(forcearray0)
photospmean0 <- rowSums(photoarray0)/ncol(photoarray0)


# I think this should estimate overall uncertainity ... though would be good to check
quantchillsp <- quantile(chillspmean, probs = whichquantiles)
quantchill0sp <- quantile(chillspmean0, probs = whichquantiles)
quantforcesp <- quantile(forcespmean, probs = whichquantiles)
quantforce0sp <- quantile(forcespmean0, probs = whichquantiles)
quantphotosp <- quantile(photospmean, probs = whichquantiles)
quantphoto0sp <- quantile(photospmean0, probs = whichquantiles)

chillperimprove <- round(abs(1-((quantchillsp[1]-quantchillsp[3])/(quantchill0sp[1]-quantchill0sp[3])))*100, 0)
forceperimpove <- round(abs(1-((quantforcesp[1]-quantforcesp[3])/(quantforce0sp[1]-quantforce0sp[3])))*100, 0)
photoperimpove <- round(abs(1-((quantphotosp[1]-quantphotosp[3])/(quantphoto0sp[1]-quantphoto0sp[3])))*100, 0)

# change in mu values
bzfchange <- 100*round(1-(mean(postfit$b_zf)/mean(postfitlamb0$b_zf)), 2)
bzcchange <- 100*round(1-(mean(postfit$b_zc)/mean(postfitlamb0$b_zc)), 2)
bzpchange <- 100*round(1-(mean(postfit$b_zp)/mean(postfitlamb0$b_zp)), 2)



#'###################################
# Some plots ...               ####
#'###################################
plotting = F


source("source/bb_muplotphylo.R")
modelhere <- fit
figpath <- "figures"
figpathmore <- "fit"
library(RColorBrewer)
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
my.pch <- rep(15:18, each=12)
alphahere = 0.4

if(plotting){
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
         #ylab="", yaxt='n',
         col=c_dark, border=c_dark_highlight,
         cex.lab=1.25)
    box(which = "plot", lty = "solid")
    
  }
  
  
  ## make plots
  par(mfrow=c(2,3))
  
  postfit <- extract(fit) # if not done above
  plot_marginal(postfit$b_zc, "b_chilling", c(-20, 2))
  # Prior for b_zc us normal(-2, 10) so I think the below works ... 
  yhere <- dnorm(x=seq(-20, 2, by=0.01), mean=-2, sd=10)
  points(x=seq(-20, 2, by=0.01), yhere, 
         col="darkblue", type="l", lwd=2, yaxt="n")
  mtext("a", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  
  postfit <- extract(fit) # if not done above
  plot_marginal(postfit$b_zf, "b_forcing", c(-20, 2))
  # Prior for b_zc us normal(-2, 10) so I think the below works ... 
  yhere <- dnorm(x=seq(-20, 2, by=0.01), mean=-2, sd=10)
  points(x=seq(-20, 2, by=0.01), yhere, 
         col="darkblue", type="l", lwd=2, yaxt="n")
  mtext("b", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  
  postfit <- extract(fit) # if not done above
  plot_marginal(postfit$b_zp, "b_photoperiod", c(-20, 2))
  # Prior for b_zc us normal(-2, 10) so I think the below works ... 
  yhere <- dnorm(x=seq(-20, 2, by=0.01), mean=0, sd=5)
  points(x=seq(-20, 2, by=0.01), yhere, 
         col="darkblue", type="l", lwd=2, yaxt="n")
  mtext("c", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  
  plot_marginal(postfit$lam_interceptsbc, "Lambda for chilling", c(-0.1, 1.1))
  yhere <- dbeta(x=seq(-0.1, 1.1, by=0.001), 1, 1)
  points(x=seq(-0.1, 1.1, by=0.001), yhere, 
         col="darkblue", type="l", lwd=2, yaxt="n")
  mtext("d", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  plot_marginal(postfit$lam_interceptsbf, "Lambda for forcing", c(-0.1, 1.1))
  yhere <- dbeta(x=seq(-0.1, 1.1, by=0.001), 1, 1)
  points(x=seq(-0.1, 1.1, by=0.001), yhere, 
         col="darkblue", type="l", lwd=2, yaxt="n")
  mtext("e", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  plot_marginal(postfit$lam_interceptsbp, "Lambda for photoperiod", c(-0.1, 1.1))
  yhere <- dbeta(x=seq(-0.1, 1.1, by=0.001), 1, 1)
  points(x=seq(-0.1, 1.1, by=0.001), yhere, 
         col="darkblue", type="l", lwd=2, yaxt="n")
  mtext("f", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  
  
  par(mfrow=c(2,3))
  hist(extract(modelhere)[["sigma_interceptsbf"]], main="force")
  hist(extract(modelhere)[["sigma_interceptsbc"]], main="chill")
  hist(extract(modelhere)[["sigma_interceptsbp"]], main="photo")
  
  hist(extract(modelhere)[["lam_interceptsbf"]], main="lambda force")
  hist(extract(modelhere)[["lam_interceptsbc"]], main="lambda chill")
  hist(extract(modelhere)[["lam_interceptsbp"]], main="lambda photo")
}

lamf.int <- mean(extract(modelhere)[["lam_interceptsbf"]])
sigmaf.int <- mean(extract(modelhere)[["sigma_interceptsbf"]])
lamf.int / (sigmaf.int + lamf.int)

lamc.int <- mean(extract(modelhere)[["lam_interceptsbc"]])
sigmac.int <- mean(extract(modelhere)[["sigma_interceptsbc"]])
lamc.int / (sigmac.int + lamc.int)

lamp.int <- mean(extract(modelhere)[["lam_interceptsbp"]])
sigmap.int <- mean(extract(modelhere)[["sigma_interceptsbp"]])
lamp.int / (sigmap.int + lamp.int)



# end ----