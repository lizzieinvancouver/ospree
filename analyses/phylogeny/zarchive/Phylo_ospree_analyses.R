#'######################################################
# Script and functions to:
# * Run phylogenetic analyses of cue-sensitivity for OSPREE dataset
#
# Started 17th April 2020
# by Ignacio Morales-Castilla, Lizzie Wolkovich and others 
#'######################################################

#### remove objects (activate if needed) ####
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



#'######################################
#### get data through bbstanleadin ####
#'######################################

# Flags to choose for bbstanleadin.R #
setwd("~/GitHub/ospree/analyses/bb_analysis") 


# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- TRUE
use.flags.for.allsppmodel <- FALSE
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


#'###################################
#### get phylogeny              ####
#'###################################

setwd("~/GitHub/ospree/analyses/phylogeny") 
source("source/get_phylo_models.R")

## read and pre-process phylogeny
#phylo <- read.tree("../../data/phylogeny/SBphylo_62complex.tre")
#phylo <- read.tree("../../data/phylogeny/SBphylo_101sps.tre")

## fix species complexes for phylogeny
#sps.list = unique(bb.stan$complex.wname)
#sps.nocomplex = unique(bb.stan$spps)
names.to.add = sps.list[which(!sps.list%in%phy.plants.ospree$tip.label)]
phy.ospree.clean <- congeneric.merge(phy.plants.ospree,names.to.add,split="_")
phy.plants.ospree<-drop.tip(phy.ospree.clean,
                            which(!phy.ospree.clean$tip.label%in%sps.list))
phylo <- phy.plants.ospree

namesphy<-phylo$tip.label
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)
plot(phylo, cex=0.7)


# get phylogenetic covariance matrix
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
#bb.stan$phylo<-bb.stan$complex.wname
#bb.stan$spps<-bb.stan$phylo
bb.stan$spps<-paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")

head(bb.stan)


# get phylogeny and data at generic level
bb.stan.genus<-aggregate(bb.stan,by=list(bb.stan$genus), FUN=mean,na.action = na.omit)
phy.genus.names<-unlist(lapply(strsplit(phylo$tip.label,"\\_"),function(x){x[1]}))
to.drop<-which(duplicated(phy.genus.names))
phylo.genus<-phylo
phylo.genus$tip.label<-phy.genus.names
phylo.genus<-drop.tip(phylo.genus,to.drop)
plot(phylo.genus)
  
  head(bb.stan.genus)
  
#'###################################
#### fit PGLMM models results   ####
#'###################################


# Model M.0 -> no random effects
priorpr.m0 <- list(R = list(V = 1, nu = 0.002))
M.0.f <- MCMCglmm(resp ~ force.z, 
                data=bb.stan, scale=TRUE, prior=priorpr.m0,
                nitt=21000, thin=10, burnin=1000 ,verbose=FALSE)
M.0.c <- MCMCglmm(resp ~ chill.z, 
                data=bb.stan, scale=TRUE, prior=priorpr.m0,
                nitt=21000, thin=10, burnin=1000 ,verbose=FALSE)
M.0.p <- MCMCglmm(resp ~ photo.z, 
                data=bb.stan, scale=TRUE, prior=priorpr.m0,
                nitt=21000, thin=10, burnin=1000 ,verbose=FALSE)



# Model M.1 -> adding phylogenetic random effects
bb.stan$animal=bb.stan$spps
priorpr.m1 <- list(R = list(V = 1, nu = 0.002), 
                   G = list(G1 = list(V = 1, nu = 0.002)))
M.1.f <- MCMCglmm(resp ~ force.z, random = ~ animal, pedigree = phylo,
                data=bb.stan, scale=TRUE,prior=priorpr.m1,
                nitt=21000,thin=10,burnin=1000,verbose=FALSE)
M.1.c <- MCMCglmm(resp ~ chill.z, random = ~ animal, pedigree = phylo,
                data=bb.stan, scale=TRUE,prior=priorpr.m1,
                nitt=21000,thin=10,burnin=1000,verbose=FALSE)
M.1.p <- MCMCglmm(resp ~ photo.z, random = ~ animal, pedigree = phylo,
                data=bb.stan, scale=TRUE,prior=priorpr.m1,
                nitt=21000,thin=10,burnin=1000,verbose=FALSE)


# explore results
summary(M.0.f)
summary(M.0.c)
summary(M.0.p)
summary(M.1.f)
summary(M.1.c)
summary(M.1.p)


#According to PGLMM BB responses to all three cues are strongly phylogenetically structured/inherited. in other words ~90% of the variance is explained by the (phylogenetically structured) differences across species. Phylogenetically close species are more likely to show similar responses to each of the cues (a little more so for forcing).



#'################################################
#### fit PGLMM models results at genus level  ####
#'################################################


# Model M.0 -> no random effects
priorpr.m0 <- list(R = list(V = 1, nu = 0.002))
M.0.f <- MCMCglmm(resp ~ force.z, 
                  data=bb.stan.genus, scale=TRUE, prior=priorpr.m0,
                  nitt=21000, thin=10, burnin=1000 ,verbose=FALSE)
M.0.c <- MCMCglmm(resp ~ chill.z, 
                  data=bb.stan.genus, scale=TRUE, prior=priorpr.m0,
                  nitt=21000, thin=10, burnin=1000 ,verbose=FALSE)
M.0.p <- MCMCglmm(resp ~ photo.z, 
                  data=bb.stan.genus, scale=TRUE, prior=priorpr.m0,
                  nitt=21000, thin=10, burnin=1000 ,verbose=FALSE)



# Model M.1 -> adding phylogenetic random effects
bb.stan.genus$animal=bb.stan.genus$Group.1
priorpr.m1 <- list(R = list(V = 1, nu = 0.002), 
                   G = list(G1 = list(V = 1, nu = 0.002)))
M.1.f <- MCMCglmm(resp ~ force.z, random = ~ animal, pedigree = phylo.genus,
                  data=bb.stan.genus, scale=TRUE,prior=priorpr.m1,
                  nitt=21000,thin=10,burnin=1000,verbose=FALSE)
M.1.c <- MCMCglmm(resp ~ chill.z, random = ~ animal, pedigree = phylo.genus,
                  data=bb.stan.genus, scale=TRUE,prior=priorpr.m1,
                  nitt=21000,thin=10,burnin=1000,verbose=FALSE)
M.1.p <- MCMCglmm(resp ~ photo.z, random = ~ animal, pedigree = phylo.genus,
                  data=bb.stan.genus, scale=TRUE,prior=priorpr.m1,
                  nitt=21000,thin=10,burnin=1000,verbose=FALSE)


# Model M.2 -> adding phylogenetic random effects - multi-predictor
bb.stan.genus$animal=bb.stan.genus$Group.1
priorpr.m2 <- list(R = list(V = 1, nu = 0.002), 
                   G = list(G1 = list(V = 1, nu = 0.002)))
M.2 <- MCMCglmm(resp ~ force.z + chill.z + photo.z, random = ~ animal, pedigree = phylo.genus,
                  data=bb.stan.genus, scale=TRUE,prior=priorpr.m2,
                  nitt=21000,thin=10,burnin=1000,verbose=FALSE)


# explore results
summary(M.0.f)
summary(M.0.c)
summary(M.0.p)
summary(M.1.f)
summary(M.1.c)
summary(M.1.p)
summary(M.2)


#'################################################
#### Running, saving, loading BRMS models    ####
#'################################################

#### First model: Non-phylogenetic model ####
"$$Budbreak = \alpha_{species} + \beta_{1}forcing
+ \beta_{2}chilling + \beta_{3}photo + \varepsilon$$"

# BRMS repeated measures, species as grouping on intercept MODEL 
model_NOphylo.FULL <- brm(
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
model_NOphylo.FULL.b <- brm(
  resp ~ force.z + chill.z + photo.z + ## fixed 
    (1 + force.z + chill.z + photo.z|animal),  ## rnd effs 
  data = bb.stan.genus, 
  family = gaussian(),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 2, 
  iter = 2000, warmup = 500,control = list(adapt_delta = 0.99) 
)

summary(model_NOphylo.FULL.b)


## repeat but put random-phylo on intercept too
model_phylo.FULL.c <- brm(
  resp ~ force.z + chill.z + photo.z + ## fixed 
    (1 |phylo) +
    (1 + force.z + chill.z + photo.z|complex.wname),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 4, cores = 2, 
  iter = 2000, warmup = 1000, control = list(adapt_delta = 0.99) 
)

summary(model_phylo.FULL.c)

#load model result
#save(model_NOphylo.FULL,file = "output/full_nophylomod.RData")
#save(model_NOphylo.FULL.b,file = "output/full_nophylomod.b.RData")
#save(model_phylo.FULL.c,file = "output/full_phylomod.c.RData")

load("output/full_nophylomod.RData")
load("output/full_nophylomod.b.RData")
load("output/model_phylo.FULL.c.RData")

#model_NOphylo.FULL


#inspect results
NOphyloeffs<-rbind(
  summary(model_NOphylo.FULL)$fixed,
  summary(model_NOphylo.FULL)$random$complex.wname,
  summary(model_NOphylo.FULL)$spec_pars
)
NOphyloeffs[,1:5] = round(NOphyloeffs[,1:5],3)
NOphyloeffs
#kable(NOphyloeffs)
plot(marginal_effects(model_NOphylo.FULL), points = TRUE, ask=T) 
pp_check(model_NOphylo.FULL,nsamples=50)


## the 'species signal
spp.signal.hyp <- paste(
  "(sd_complex.wname__Intercept^2) /", 
  "(sd_complex.wname__Intercept^2
  + sigma^2) = 0.0"
)
(spp.signal <- hypothesis(model_NOphylo.FULL, 
                          spp.signal.hyp, class = NULL))
plot(spp.signal)



#### Second model: Phylogenetic model ####
"$$Budbreak = \alpha_{phylo,species} + \beta_{1}forcing
+ \beta_{2}chilling + \beta_{3}photo + \varepsilon$$"

#This is the simple multilevel model for repeated measures, where intraspecific variation for the cues is considered. Grouping factors on the intercept consider the phylogenetic structure ($\alpha_{phylo}$) and other inter-specific variation independent from the phylogeny ($\alpha_{species}$).
#This model makes a bit more sense than the previous one, but we still lack information of interest. While this model informs us about the convenience of accounting for phylogenetic non-independence, and gives us a measure of how much the residual variation is phylogenetically structured, we still lack specific sensitivities to each cue (we'd need grouping on the slopes too). 
model_phylo.int.only <- brm(
  resp ~ force.z + chill.z + photo.z +      ## fixed effs
    (1 |phylo) + (1|complex.wname),  ## rnd effs 
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
save(model_phylo.int.only,file = "output/model_phylo.int.only.RData")
#load("output/model_phylo.int.only.RData")
#model_phylo.int.only

#inspect results
Phylomodeffs<-rbind(
  summary(model_phylo.int.only)$fixed,
  summary(model_phylo.int.only)$random$phylo,
  summary(model_phylo.int.only)$random$complex.wname,
  summary(model_phylo.int.only)$spec_pars
)
Phylomodeffs[,1:5] = round(Phylomodeffs[,1:5],3)
#kable(Phylomodeffs)
Phylomodeffs
plot(marginal_effects(model_phylo.int.only), points = TRUE, ask=T) 
pp_check(model_phylo.int.only,nsamples=50)


## the phylogenetic signal
hyp.phylo.int <- paste(
  "(sd_phylo__Intercept^2) /", 
  "(sd_phylo__Intercept^2 
  + sd_complex.wname__Intercept^2
  + sigma^2) = 0.0"
)
(lambda.phylo.int <- hypothesis(model_phylo.int.only, hyp.phylo.int, class = NULL))
plot(lambda.phylo.int)



#### Third model: Phylogenetic model with rnd on slopes too ####
"$$Budbreak = \alpha_{phylo,species} + \beta_{1, phylo, species}forcing
+ \beta_{2, phylo, species}chilling + \beta_{3, phylo, species}photo + \varepsilon$$"


## this model is pretty slow
model_phylo.int.slope <- brm(
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
save(model_phylo.int.slope,file = "output/model_phylo.int.slope.RData")
#load("output/model_phylo.int.only.RData")
#model_phylo.int.only

#inspect results
Phylomodeffsfull<-rbind(
  summary(model_phylo.int.slope)$fixed,
  summary(model_phylo.int.slope)$random$phylo,
  summary(model_phylo.int.slope)$random$complex.wname,
  summary(model_phylo.int.slope)$spec_pars
)
Phylomodeffsfull[,1:5] = round(Phylomodeffsfull[,1:5],3)
#kable(Phylomodeffs)
Phylomodeffsfull
plot(marginal_effects(model_phylo.int.slope), points = TRUE, ask=T) 
pp_check(model_phylo.int.slope,nsamples=50)


## the phylogenetic signal
hyp.phylo.all <- paste(
  "(sd_phylo__Intercept^2) /", 
  "(sd_phylo__Intercept^2 
  + sd_complex.wname__Intercept^2
  + sigma^2) = 0.0"
)
(lambda.phylo.all <- hypothesis(model_phylo.int.slope, hyp.phylo.all, class = NULL))
plot(lambda.phylo.all)





#### Fourth model: Slopes of non-phylo model as a response ####

## Fourth model A: sensitivity to forcing - phylo structure

# we first get back the coefficients from the BB models (fitted in BRMS)
mod.summary <- tidy(model_NOphylo.FULL.b)

summary(model_NOphylo.FULL.b)
# We first fit a random-slope model for forcing, then get the slopes
# then check the following model
positions = grep(",force.z]", mod.summary$term)
force.slopes <- mod.summary[positions,] 
force.slopes$phylo <- sort(unique(bb.stan$complex.wname))


# model A
"$$\beta_{1} = \alpha_{phylo} + \varepsilon$$"


#### First model: Non-phylogenetic model ####

# BRMS repeated measures, species as grouping on intercept MODEL 
model_beta.force <- brm(
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

model_beta.force


## Fourth model B: sensitivity to chilling - phylo structure
"$$\beta_{2} = \alpha_{phylo} + \varepsilon$$"

positions = grep(",chill.z]", mod.summary$term)
chill.slopes <- mod.summary[positions,] 
chill.slopes$phylo <- sort(unique(bb.stan$complex.wname))


# BRMS repeated measures, species as grouping on intercept MODEL 
model_beta.chill <- brm(
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

summary(model_beta.chill)


## Fourth model C: sensitivity to photo - phylo structure
"$$\beta_{3} = \alpha_{phylo} + \varepsilon$$"

positions = grep(",photo.z]", mod.summary$term)
photo.slopes <- mod.summary[positions,] 
photo.slopes$phylo <- sort(unique(bb.stan$complex.wname))


model_beta.photo <- brm(
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


## 

#####################################################
#### check phylogenetic signal in sensitivities  ####
#####################################################
summary(model_beta.force)
summary(model_beta.chill)
summary(model_beta.photo)




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
par(mfrow=c(2,3))
par(mar=c(2,2,1,1))
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
title("Forcing (H� = 0.37)",xpd = T)
plot.contMap(obj.chill,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title("Chilling (H� = 0.18)",xpd = T)
plot.contMap(obj.photo,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
             fsize = c(0.45, 0.7), outline=FALSE,lwd=2,mar = c(1,1,2,1))
title("Photo (H� = 0.68)",xpd = T)

dev.off()



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

# inspect R2s
R2table<-as.data.frame(rbind(
  bayes_R2(model_NOphylo.FULL),
  bayes_R2(model_phylo.int.only),
  bayes_R2(model_NOphylo.FULL.b),
  bayes_R2(model_phylo.FULL.c),
  bayes_R2(model_phylo.int.slope)
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

#* Check model sensitivity to removing Gymnosperms 

#* Calculate days to BB using our model output for each species and a baseline scenario .... could help with structuring the paper.




