## Started 6 July 2017 ##
## By Cat and Dan and others ##
# Updated a tiny bit by Dan 19 June 2018
# Updated 8 Oct 2018
# housekeeping
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



#######################################
#### get data through bbstanleadin ####
#######################################

# Flags to choose for bbstanleadin.R #
######################################
setwd("..//bb_analysis")

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- F
use.flags.for.allsppmodel <- T
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


####################################
#### get phylogeny              ####
####################################

setwd("..//phylogeny") 
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
#### clean data for angiosperm only   ####
#######################################


## for angiosperms only:
gymnospp <- phy.plants.ospree$tip.label[57:63]
phylo <- drop.tip(phylo, 57:63)

gymnospp.allsp <- phy.plants.ospree$tip.label[136:151]
phylo.angio <- drop.tip(phylo, 136:151)
plot(phylo.angio,cex=.6)

bb.stan <- subset(bb.stan, !spps %in% gymnospp.allsp)


#######################################
#### load and plot PGLMM results   ####
#######################################

PGLMM_results_ospree <- read.csv("output/PGLMM_results_ospree.csv")
PGLMM_results_ospree[is.na.data.frame(PGLMM_results_ospree)] = ""
#kable(PGLMM_results_ospree)
PGLMM_results_ospree


#According to PGLMM BB responses to all three cues are strongly phylogenetically structured/inherited. in other words ~90% of the variance is explained by the (phylogenetically structured) differences across species. Phylogenetically close species are more likely to show similar responses to each of the cues (a little more so for forcing).
#A comparison of PGLMM results against our previous results in the BB ms. reveal a shift in importance between chilling and forcing. Forcing shows a stronger effect size than chilling, at least according to PGLMM.

#######################################


#################################################
#### Running, saving, loading BRMS models    ####
#################################################

#### First model: Non-phylogenetic model ####
"$$Budbreak = \alpha_{species} + \beta_{1}forcing
+ \beta_{2}chilling + \beta_{3}photo + \varepsilon$$"


summary(model_NOphylo.FULL.sps)

# BRMS repeated measures, species as grouping on intercept MODEL 
model_NOphylo.FULL.sps <- brm(
  resp ~ force.z + chill.z + photo.z + ## fixed 
    (1|spps),  ## rnd effs 
    data = bb.stan, 
  family = gaussian(),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 2000, warmup = 500
)

## repeat but put random on slope too
model_NOphylo.FULL.b.sps <- brm(
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
  ,sample_prior = TRUE, chains = 4, cores = 2, 
  iter = 2000, warmup = 500
)

#bb.stan$phylo <- bb.stan$spps
## repeat but put random-phylo on intercept too
model_phylo.FULL.c.sps <- brm(
  resp ~ force.z + chill.z + photo.z + ## fixed 
    (1 |phylo) +
    (1 + force.z + chill.z + photo.z|spps),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 2, 
  iter = 2000, warmup = 1000, control = list(adapt_delta = 0.99) 
)

summary(model_NOphylo.FULL.sps)
summary(model_NOphylo.FULL.b.sps)
summary(model_phylo.FULL.c.sps)

#load model result
save(model_NOphylo.FULL.sps,file = "output/full_nophylomodsps_ang215sps.RData")
save(model_NOphylo.FULL.b.sps,file = "output/full_nophylomodsps.b_ang215sps.RData")
save(model_phylo.FULL.c.sps,file = "output/full_phylomod.csps_ang215sps.RData")

#load("output/full_nophylomod.RData")
#load("output/full_nophylomodsps.b.RData")
#load("output/model_phylo.FULL.c.RData")

model_NOphylo.FULL.b.sps
#model_NOphylo.FULL



#### Second model: Phylogenetic model ####
"$$Budbreak = \alpha_{phylo,species} + \beta_{1}forcing
+ \beta_{2}chilling + \beta_{3}photo + \varepsilon$$"

#This is the simple multilevel model for repeated measures, where intraspecific variation for the cues is considered. Grouping factors on the intercept consider the phylogenetic structure ($\alpha_{phylo}$) and other inter-specific variation independent from the phylogeny ($\alpha_{species}$).
#This model makes a bit more sense than the previous one, but we still lack information of interest. While this model informs us about the convenience of accounting for phylogenetic non-independence, and gives us a measure of how much the residual variation is phylogenetically structured, we still lack specific sensitivities to each cue (we'd need grouping on the slopes too). 
model_phylo.int.only.sps <- brm(
  resp ~ force.z + chill.z + photo.z +      ## fixed effs
    (1 |phylo) + (1|spps),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
 ,sample_prior = TRUE, chains = 4, cores = 2, 
  iter = 2000, warmup = 1000
)

#load model result
save(model_phylo.int.only.sps,file = "output/model_phylo.int.only_215spp.RData")
#load("output/model_phylo.int.only.RData")
#model_phylo.int.only



#### Third model: Phylogenetic model with rnd on slopes too ####
"$$Budbreak = \alpha_{phylo,species} + \beta_{1, phylo, species}forcing
+ \beta_{2, phylo, species}chilling + \beta_{3, phylo, species}photo + \varepsilon$$"


## this model is pretty slow
model_phylo.int.slope.sps <- brm(
  resp ~ force.z + chill.z + photo.z +      ## fixed effs
    (1 + force.z + chill.z + photo.z|phylo) + 
    (1 + force.z + chill.z + photo.z|complex.wname),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 2, 
  iter = 2000, warmup = 1000
)

#load model result
save(model_phylo.int.slope.sps,file = "output/model_phylo.int.slope.RData")
#load("output/model_phylo.int.only.RData")
#model_phylo.int.only


#### Fourth model: Slopes of non-phylo model as a response ####

## Fourth model A: sensitivity to forcing - phylo structure

# we first get back the coefficients from the BB models (fitted in BRMS)
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
  iter = 2000, warmup = 500
)

model_beta.force.sps


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
  iter = 2000, warmup = 1000
)

summary(model_beta.chill.sps)


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
  iter = 2000, warmup = 1000
)


## saving

save(model_beta.force.sps,file = 'output/model_beta.force.sps_231spp.RData')
save(model_beta.chill.sps,file = 'output/model_beta.chill.sps_231spp.RData')
save(model_beta.photo.sps,file = 'output/model_beta.photo.sps_231spp.RData')


#####################################################
#### check phylogenetic signal in sensitivities  ####
#####################################################
summary(model_beta.force.sps)
summary(model_beta.chill.sps)
summary(model_beta.photo.sps)




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
title("Forcing (H² = 0.43)",xpd = T)
plot.contMap(obj.chill,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title("Chilling (H² = 0.53)",xpd = T)
plot.contMap(obj.photo,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title("Photo (H² = 0.26)",xpd = T)

dev.off()

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




######################################################
#### Model evaluation - LOO - phylo vs. non-phylo ####
######################################################
model_NOphylo.FULL.b.sps
# inspect R2s
R2table<-as.data.frame(rbind(
bayes_R2(model_NOphylo.FULL.sps),
bayes_R2(model_phylo.int.only.sps),
bayes_R2(model_NOphylo.FULL.b.sps),
bayes_R2(model_phylo.FULL.c.sps)
#,bayes_R2(model_phylo.int.slope.sps)
))
rownames(R2table) = c("mod.sps.intercept","mod.sps.phylo.intercept",
                      "mod.sps.interc.slope","mod.sps.interc.slope.phy.int",
                      "mod.sps.phylo.interc.slope")
R2table
write.csv(R2table,file = "output/bayesR2_model_comparison.csv")

# inspect LOOs
(loo1 <- loo(model_NOphylo.FULL))
(loo2 <- loo(model_phylo.int.only))
(loo3 <- loo(model_NOphylo.FULL.b))
(loo4 <- loo(model_phylo.FULL.c))
(loo5 <- loo(model_phylo.int.slope))

loocomparisons = loo_compare(loo1,loo2,loo3,loo4,loo5)
#write.csv(loocomparisons,file = "output/LOO_model_comparison.csv")

## NEXT STEPS

#* Check model sensitivity to removing Gymnosperms - DONE

#* Calculate days to BB using our model output for each species 
# and a baseline scenario .... could help with structuring the paper.




