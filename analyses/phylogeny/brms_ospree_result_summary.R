## Started 3 January 2020 ##
## By Nacho ##
## Code to summarize phylogenetic modelling results ##

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
library(dplyr)
library(knitr)
library(broom)
library(loo)


#######################################
#### get data through bbstanleadin ####
#######################################

# Flags to choose for bbstanleadin.R #
######################################
setwd("~/GitHub/ospree/analyses/bb_analysis") 


# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- T
use.flags.for.allsppmodel <- F
use.yourown.flagdesign <- FALSE

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
  use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
  use.zscore = TRUE # change to false to use raw predictors
  
  # Default is species complex and no crops
  use.allspp = FALSE
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
unique(bb.stan$complex.wname)


#######################################

####################################
#### get phylogeny              ####
####################################

setwd("~/GitHub/ospree/analyses/phylogeny") 
source("source/get_phylo_models.R")

## read and pre-process phylogeny
#phylo <- read.tree("../../data/phylogeny/SBphylo_62complex.tre")
#phylo <- read.tree("../../data/phylogeny/SBphylo_101sps.tre")

## fix species complexes for phylogeny
#sps.list = unique(bb.stan$complex.wname)
#sps.nocomplex = unique(bb.stan$spps)
#names.to.add = sps.list[which(!sps.list%in%phy.plants.ospree$tip.label)]
#phy.ospree.clean <- congeneric.merge(phy.plants.ospree,names.to.add,split="_")
#phy.plants.ospree<-drop.tip(phy.ospree.clean,
#                            which(!phy.ospree.clean$tip.label%in%sps.list))

phylo <- phy.plants.ospree

namesphy<-phylo$tip.label
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)
plot(phylo, cex=0.7)



## get phylogenetic covariance matrix
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
#bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")
#bb.stan$phylo<-bb.stan$complex.wname
#bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")

bb.stan$spps<-paste(bb.stan$genus,bb.stan$species,sep="_")



#######################################

#######################################
#### clean data for angiosperm only   ####
#######################################


## for angiosperms only:
gymnospp <- phy.plants.ospree$tip.label[57:63]
phylo <- drop.tip(phylo, 57:63)

gymnospp.allsp <- phy.plants.ospree$tip.label[136:151]
phylo.angio <- drop.tip(phylo, 136:151)
plot(phylo.angio,cex=.6)
#phylo<-phylo.angio

bb.stan <- subset(bb.stan, !spps %in% gymnospp.allsp)


#######################################



#######################################
#### load previous BRMS models     ####
#######################################

subset.type="complex"
subset.type="sps.bbms"
subset.type="sps.bbms.angio"
subset.type="all.sps"
subset.type="all.sps.angio"
subset.type="all.sps.HQ"



#Species list is built using analyses/bb_analysis/source/speciescomplex.multcues.R
#1 all three cues were manipulated in one study (i.e., two levels) - applies to species
#2 all three cues were manipulated somehow across >1 study (i.e., each study used must have two levels of at least one cue) - applies to species and complexes
#And then crops were removed: Actinidia deliciosa, Malus domestica, Vitis vinifera, Ribes nigrum


if(subset.type=="complex"){ ## 2675 obs; 52 complexes
load("output/full_nophylomod.RData")
load("output/full_nophylomod.b.RData")
load("output/model_phylo.FULL.c.RData")
load("output/model_phylo.int.only.complex.RData")
}
summary(model_NOphylo.FULL)
summary(model_NOphylo.FULL.b)
unique(model_NOphylo.FULL$data$complex.wname)==unique(model_NOphylo.FULL.b$data$complex.wname)
#rm(list=ls()) 


if(subset.type=="sps.bbms"){ ## 2675 obs; 117 species
  load("output/full_nophylomodsps.RData")
  load("output/full_nophylomodsps.b.RData")
  load("output/full_phylomod.csps.RData")
  load("output/model_phylo.int.only.RData")
}
summary(model_NOphylo.FULL.sps)
summary(model_NOphylo.FULL.b.sps)
summary(model_phylo.FULL.c.sps)
summary(model_phylo.int.only.sps)
#unique(model_NOphylo.FULL.sps$data$spps) == unique(model_NOphylo.FULL.b.sps$data$spps)
#unique(model_NOphylo.FULL.b.sps$data$spps) == unique(model_phylo.int.only.sps$data$spps)
#unique(model_phylo.FULL.c.sps$data$spps) == unique(model_phylo.int.only.sps$data$spps)
#rm(list=ls()) 

if(subset.type=="sps.bbms.angio"){ ## 2376 obs; 110 species
  load("output/full_nophylomodsps_angio.RData")
  load("output/full_nophylomodsps.b_angio.RData")
  load("output/full_phylomod.csps_angio.RData")
  #load("output/model_phylo.int.only_angio.RData")
}
summary(model_NOphylo.FULL.sps)
summary(model_NOphylo.FULL.b.sps)
summary(model_phylo.FULL.c.sps)
summary(model_phylo.int.only.sps)
#unique(model_NOphylo.FULL.sps$data$spps) == unique(model_NOphylo.FULL.b.sps$data$spps)
#rm(list=ls()) 


if(subset.type=="all.sps"){ ## 4083 obs; 231 species
  load("output/full_nophylomodsps_all231sps.RData")
  load("output/full_nophylomodsps.b_all231sps.RData")
  load("output/full_phylomod.csps_all231sps.RData")
  load("output/model_phylo.int.only_231spp.RData")
}
summary(model_NOphylo.FULL.sps)
summary(model_NOphylo.FULL.b.sps)
summary(model_phylo.FULL.c.sps)
summary(model_phylo.int.only.sps)
#unique(model_NOphylo.FULL.sps$data$spps) == unique(model_NOphylo.FULL.b.sps$data$spps)
#rm(list=ls()) 

if(subset.type=="all.sps.angio"){ ## 3569 obs; 215 species
  load("output/full_nophylomodsps_ang215sps.RData")
  load("output/full_nophylomodsps.b_ang215sps.RData")
  load("output/full_phylomod.csps_ang215sps.RData")
  load("output/model_phylo.int.only_215spp.RData")
}
summary(model_NOphylo.FULL.sps)
summary(model_NOphylo.FULL.b.sps)
summary(model_phylo.FULL.c.sps)
summary(model_phylo.int.only.sps)
#unique(model_NOphylo.FULL.sps$data$spps) == unique(model_NOphylo.FULL.b.sps$data$spps)
#rm(list=ls()) 





#######################################



#################################################
#### BRMS models for sensitivities (slopes)  ####
#################################################

# we first get back the coefficients from the BB models (fitted in BRMS)
# for complexes
mod.summary <- tidy(model_NOphylo.FULL.b)
summary(model_NOphylo.FULL.b)

# for other specifications
mod.summary <- tidy(model_NOphylo.FULL.b.sps)
summary(model_NOphylo.FULL.b.sps)



# We first fit a random-slope model for forcing, then get the slopes
# then check the following model
positions = grep(",force.z]", mod.summary$term)
force.slopes <- mod.summary[positions,] 
force.slopes$phylo <- sort(unique(bb.stan$spps))


# model A
"$$\beta_{1} = \alpha_{phylo} + \varepsilon$$"


#### First model: Non-phylogenetic model ####

# BRMS repeated measures, species as grouping on intercept MODEL 
model_beta.force.sps <- brm(
  estimate ~ 1 + ## fixed 
    (1|phylo),  ## rnd effs 
  data = force.slopes, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    #prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 2000, warmup = 1000,control = list(adapt_delta = 0.99) 
)

#model_beta.force.sps


## Fourth model B: sensitivity to chilling - phylo structure
"$$\beta_{2} = \alpha_{phylo} + \varepsilon$$"

positions = grep(",chill.z]", mod.summary$term)
chill.slopes <- mod.summary[positions,] 
chill.slopes$phylo <- sort(unique(bb.stan$spps))


# BRMS repeated measures, species as grouping on intercept MODEL 
model_beta.chill.sps <- brm(
  estimate ~ 1 + ## fixed 
    (1|phylo),  ## rnd effs 
  data = chill.slopes, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    #prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 2, 
  iter = 2000, warmup = 1000,control = list(adapt_delta = 0.99)
)

#summary(model_beta.chill.sps)


## Fourth model C: sensitivity to photo - phylo structure
"$$\beta_{3} = \alpha_{phylo} + \varepsilon$$"

positions = grep(",photo.z]", mod.summary$term)
photo.slopes <- mod.summary[positions,] 
photo.slopes$phylo <- sort(unique(bb.stan$spps))


model_beta.photo.sps <- brm(
  estimate ~ 1 + ## fixed 
    (1|phylo),  ## rnd effs 
  data = photo.slopes, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    #prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 2, 
  iter = 2000, warmup = 1000,control = list(adapt_delta = 0.99)
)


## saving
save(model_beta.force.sps,file = 'output/model_beta.force.sps_52complex.RData')
save(model_beta.chill.sps,file = 'output/model_beta.chill.sps_52complex.RData')
save(model_beta.photo.sps,file = 'output/model_beta.photo.sps_52complex.RData')

load('output/model_beta.force.sps.RData')
load('output/model_beta.chill.sps.RData')
load('output/model_beta.photo.sps.RData')


#####################################################
#### check phylogenetic signal in sensitivities  ####
#####################################################


## the phylogenetic signal
hyp.phylo.force <- paste(
  "(sd_phylo__Intercept^2) /", 
  "(sd_phylo__Intercept^2 
  + sigma^2) = 0.0"
)
(lambda.phylo.force <- hypothesis(model_beta.force.sps, 
                                  hyp.phylo.force, class = NULL))

hyp.phylo.chill <- paste(
  "(sd_phylo__Intercept^2) /", 
  "(sd_phylo__Intercept^2 
  + sigma^2) = 0.0"
)
(lambda.phylo.chill <- hypothesis(model_beta.chill.sps, 
                                  hyp.phylo.chill, 
                                  class = NULL))

hyp.phylo.photo <- paste(
  "(sd_phylo__Intercept^2) /", 
  "(sd_phylo__Intercept^2 
  + sigma^2) = 0.0"
)
(lambda.phylo.photo <- hypothesis(model_beta.photo.sps, 
                                  hyp.phylo.photo, 
                                  class = NULL))






##############################################################
#### Visualize phylogenetic structure in cue-sensitivity  ####
##############################################################


## forcing
forc = force.slopes$estimate
names(forc)=force.slopes$phylo
phylo=multi2di(phylo)
obj.force<-contMap(phylo,forc,plot=FALSE)
obj.force<-setMap(obj.force,invert=TRUE)
obj.force<-setMap(obj.force,colors=c("yellow","darkcyan","purple"))
#plot(obj.force,fsize=c(0.5,1),outline=FALSE,lwd=c(3,7),leg.txt="forcing")

## chilling
chill = chill.slopes$estimate
names(chill)=chill.slopes$phylo
obj.chill<-contMap(phylo,chill,plot=FALSE)
obj.chill<-setMap(obj.chill,invert=TRUE)
obj.chill<-setMap(obj.chill,colors=c("yellow","darkcyan","purple"))

#plot(obj.chill,fsize=c(0.4,1),outline=FALSE,lwd=c(3,7),leg.txt="chilling")


## photoperiod
photo = photo.slopes$estimate
names(photo)=photo.slopes$phylo
obj.photo<-contMap(phylo,photo,plot=FALSE)
obj.photo<-setMap(obj.photo,invert=TRUE)
obj.photo<-setMap(obj.photo,colors=c("yellow","darkcyan","purple"))

#plot(obj.photo,fsize=c(0.4,1),outline=FALSE,lwd=c(3,7),leg.txt="chilling")


## plotting

## combined plot of three phylogenies and cues
par(mfrow=c(2,3),mar=c(1.5,1.5,1,1))
#par(mfrow=c(2,3))
#par(mar=c(2,2,1,1))
layout(matrix(c(1,2,3,4,5,6,4,5,6), nrow = 3, ncol = 3, byrow = TRUE))

## plot hypotheses on upper row
d <- density(lambda.phylo.force$samples[,1])
plot(d,main="",xlab="",
     xlim=c(0,1),col="darkblue")
polygon(d, col=adjustcolor("darkblue",0.4), border="darkblue")
abline(v=mean(lambda.phylo.force$samples[,1]),lty=2,col="blue")

d <- density(lambda.phylo.chill$samples[,1])
plot(d,main="",xlab="",
     xlim=c(0,1),col="darkblue")
polygon(d, col=adjustcolor("darkblue",0.4), border="darkblue")
abline(v=mean(lambda.phylo.chill$samples[,1]),lty=2,col="blue")

d <- density(lambda.phylo.photo$samples[,1])
plot(d,main="",xlab="",
     xlim=c(0,1),col="darkblue")
polygon(d, col=adjustcolor("darkblue",0.4), border="darkblue")
abline(v=mean(lambda.phylo.photo$samples[,1]),lty=2,col="blue")


plot.contMap(obj.force,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title(paste("Forcing (H² = ",round(lambda.phylo.force$hypothesis$Estimate,2),")",sep=""),xpd = T)
plot.contMap(obj.chill,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title(paste("Chilling (H² = ",round(lambda.phylo.chill$hypothesis$Estimate,2),")",sep=""),xpd = T)
plot.contMap(obj.photo,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title(paste("Photo (H² = ",round(lambda.phylo.photo$hypothesis$Estimate,2),")",sep=""),
      xpd = T)

dev.off()

par(mfrow=c(1,3))
plot(chill.slopes$estimate,
     xlab="Chilling sensitivity",
     ylab="Photo sensitivity",
     
     #      force.slopes$estimate,
     photo.slopes$estimate,col="grey",pch=19)
abline(lm(photo.slopes$estimate~chill.slopes$estimate),col="black")

plot(chill.slopes$estimate,
     xlab="Chilling sensitivity",
     ylab="Forcing sensitivity",
     force.slopes$estimate,col="grey",pch=19)
abline(lm(force.slopes$estimate~chill.slopes$estimate),col="black")

plot(photo.slopes$estimate,
     xlab="Photo sensitivity",
     ylab="Forcing sensitivity",
     force.slopes$estimate,col="grey",pch=19)
abline(lm(force.slopes$estimate~photo.slopes$estimate),col="black")


###########################################################
#### Checking phylogenetic structure with PGLS - caper ####
###########################################################

# generate a comparative data object for caper
sensitivities <- data.frame(force = force.slopes$estimate,
                            chill = chill.slopes$estimate,
                            photo = photo.slopes$estimate,
                            species = force.slopes$phylo)

phylo$node.label = 1:length(phylo$node.label)
sensitivities.compdat <- comparative.data(phylo,sensitivities,
                                          names.col= "species",
                                          vcv = T)

# fit pgls model for each cue
pgls.force.ml = pgls(force ~ 1,
                     data=sensitivities.compdat,lambda="ML")
pgls.chill.ml = pgls(chill ~ 1,
                     data=sensitivities.compdat,lambda="ML")
pgls.photo.ml = pgls(photo ~ 1,
                     data=sensitivities.compdat,lambda="ML")

summary(pgls.force.ml)
summary(pgls.chill.ml)
summary(pgls.photo.ml)

PGLS.lambdas<-rbind(c(pgls.force.ml$param[2],
                      pgls.force.ml$param.CI$lambda$ci.val),
                    c(pgls.chill.ml$param[2],
                      pgls.chill.ml$param.CI$lambda$ci.val),
                    c(pgls.photo.ml$param[2],
                      pgls.photo.ml$param.CI$lambda$ci.val))
colnames(PGLS.lambdas)<-c("lambda","Lower95CI","Upper95CI")
rownames(PGLS.lambdas)<-c("forcing","chilling","photo")

BRMS.H2<-rbind(lambda.phylo.force$hypothesis[c(2,4,5)],
               lambda.phylo.chill$hypothesis[c(2,4,5)],
               lambda.phylo.photo$hypothesis[c(2,4,5)])
colnames(BRMS.H2)<-c("H2","Lower95CI","Upper95CI")
PhyloSigs<-cbind(PGLS.lambdas,BRMS.H2)
write.csv(PhyloSigs,file = "output/Phylosig_PGLS_BRMS_117spp.csv")


######################################################


######################################################
#### Model evaluation - LOO - phylo vs. non-phylo ####
######################################################

# inspect R2s for complex
R2table<-as.data.frame(rbind(
  bayes_R2(model_NOphylo.FULL),
  bayes_R2(model_phylo.int.only),
  bayes_R2(model_NOphylo.FULL.b),
  bayes_R2(model_phylo.FULL.c)
  #,bayes_R2(model_phylo.int.slope.sps)
))

# inspect R2s
R2table<-as.data.frame(rbind(
  bayes_R2(model_NOphylo.FULL.sps),
  bayes_R2(model_phylo.int.only.sps),
  bayes_R2(model_NOphylo.FULL.b.sps),
  bayes_R2(model_phylo.FULL.c.sps)
  #,bayes_R2(model_phylo.int.slope.sps)
))

rownames(R2table) = c("mod.sps.intercept","mod.sps.phylo.intercept",
                      "mod.sps.interc.slope","mod.sps.interc.slope.phy.int"
                      #,"mod.sps.phylo.interc.slope"
                      )
R2table
write.csv(R2table,file = "output/bayesR2_model_comparison52complex.csv")

# inspect LOOs for complexes
(loo1 <- loo(model_NOphylo.FULL))
(loo2 <- loo(model_phylo.int.only))
(loo3 <- loo(model_NOphylo.FULL.b))
(loo4 <- loo(model_phylo.FULL.c))

# inspect LOOs
(loo1 <- loo(model_NOphylo.FULL.sps))
(loo2 <- loo(model_phylo.int.only.sps))
(loo3 <- loo(model_NOphylo.FULL.b.sps))
(loo4 <- loo(model_phylo.FULL.c.sps))
#(loo5 <- loo(model_phylo.int.slope))

loos_52complex<-list(loo1,loo2,loo3,loo4)
save(loos_52complex,file = "output/loos_52complex.RData")

loos_215sps<-list(loo1,loo2,loo3,loo4)
save(loos_215sps,file = "output/loos_215sps.RData")

#load("output/loos_215sps.RData")

loocomparisons = loo_compare(loo1,loo2,loo3,loo4)
#write.csv(loocomparisons,file = "output/LOO_model_comparison52complex.csv")
######################################################


######################################################
#### Making predictions for two scenarios ####
######################################################

# make values for each scenario 
# 1) High chill, long-ish photoperiod, and moderate forcing (regular scenario)

forcereg <- quantile(model_NOphylo.FULL.b.sps$data$force.z,0.4)
chillingreg <- quantile(model_NOphylo.FULL.b.sps$data$chill.z,0.75)
photoreg <- quantile(model_NOphylo.FULL.b.sps$data$photo.z,0.75)

# 2) Low chill, shorter photoperiod, higher forcing (climate change scenario)

forceCC <- quantile(model_NOphylo.FULL.b.sps$data$force.z,0.75)
chillingCC <- quantile(model_NOphylo.FULL.b.sps$data$chill.z,0.4)
photoCC <- quantile(model_NOphylo.FULL.b.sps$data$photo.z,0.50)


# now generate new data for predictions:
spps = sort(unique(model_NOphylo.FULL.b.sps$data$spps))
rep(spps,2)

newdata<-data.frame(force.z=c(rep(forcereg,117),rep(forceCC,117)),
                    chill.z=c(rep(chillingreg,117),rep(chillingCC,117)),
                    photo.z=c(rep(photoreg,117),rep(photoCC,117)),
                    spps=rep(spps,2),
                    phylo=rep(spps,2))
head(newdata)


## make prediction according to the above scenarios
Phen.predictionsB<-predict(model_NOphylo.FULL.b.sps, newdata = newdata)
Phen.predictionsC<-predict(model_phylo.FULL.c.sps, newdata = newdata)

SavePreds.B<-data.frame(Species=spps,
                      bubburst_reg=Phen.predictionsB[1:117,1],
                      bubburst_reg_SE=Phen.predictionsB[1:117,2],
                      bubburst_CC=Phen.predictionsB[118:234,1],
                      bubburst_CC_SE=Phen.predictionsB[118:234,1])
SavePreds.C<-data.frame(Species=spps,
                        bubburst_reg=Phen.predictionsC[1:117,1],
                        bubburst_reg_SE=Phen.predictionsC[1:117,2],
                        bubburst_CC=Phen.predictionsC[118:234,1],
                        bubburst_CC_SE=Phen.predictionsC[118:234,1])

cor(SavePreds.B[,4],SavePreds.C[,4])
#SavePreds.B[,4]-SavePreds.B[,2]
#SavePreds.C[,4]-SavePreds.C[,2]

write.csv(SavePreds,file = "output/PhenForecasts_117sps_modb.csv")

## visualize results (from bbmuplot code)

dev.off()
par(mar=c(5,5,3,10))
plot(x=NULL,y=NULL, xlim=c(0,3), xaxt='n', ylim=c(0,55),
     xlab="Model estimate days to budburst", ylab="days to budburst",cex.lab=1.2)
axis(1, at=1:2, labels=c("Current", "Climate Change"), las=1)

#abline(v=0, lty=2, col="darkgrey")
  pos.x<-1:2
  
  for(spsi in 1:117){#spsi=1
    pos.sps.i<-spsi
    jitt<-runif(1,0.05,0.4)
    pos.x.sps.i<-pos.x-jitt
    pos.y.sps.i<-SavePreds.B[spsi,c(2,4)]
    lines(rep(pos.x.sps.i[1],2),c(SavePreds.B[spsi,2]+SavePreds.B[spsi,3],
          SavePreds.B[spsi,2]-SavePreds.B[spsi,3]),
          col=adjustcolor(spsi, 0.5))
    lines(rep(pos.x.sps.i[2],2),c(SavePreds.B[spsi,4]+SavePreds.B[spsi,5],
                                   SavePreds.B[spsi,4]-SavePreds.B[spsi,5]),
           col=adjustcolor(spsi, 0.5))
    
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=19, col=adjustcolor(spsi, 0.5))
    
  }

par(xpd=TRUE) # so I can plot legend outside
legend(leg1, leg2, sort(unique(gsub("_", " ", bb.stan$complex.wname))), pch=my.pch[1:spnum],
       col=alpha(my.pal[1:spnum], alphahere),
       cex=0.75, bty="n", text.font=3)




