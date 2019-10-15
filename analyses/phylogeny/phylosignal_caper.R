## Started 6 July 2017 ##
## By Cat and Dan and others ##
# Updated a tiny bit by Dan 19 June 2018
# Updated 8 Oct 2018
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

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillports = FALSE # change to true for using chillportions instead of utah units

# Default is species complex
use.allspp = FALSE
use.nocropspp = FALSE

# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE

source("source/bbstanleadin.phyla.R")

str(datalist.bb)
sum(is.na(datalist.bb$y))


############################################
#### Load sps list and get phylogeny
############################################

## load species list (beware this list has both species and complexes)
species.list <- read.csv("input/spslist.csv")
species.list <- sort(species.list$Species_binomial)

## getting a list of genera in our sps list
genus.list <- unlist(lapply(strsplit(species.list, "_"), 
                             function(x){
                               return(x[1])
                               }))

genus.list.unique <- unique(genus.list)


## load vascular plant phylogeny
phy.plants <- read.tree("../../data/phylogeny/Vascular_Plants_rooted.dated.tre")


## getting a list of genera in Zanne's phylo
phy.genera <- unlist(
  lapply(strsplit(phy.plants$tip.label, "_"),function(x){return(x[1])})
)

phy.genera.uniq <- sort(unique(phy.genera))



## how many ospree genera are in the phylogeny?
ospreegenus.inphylo <- genus.list.unique[which(genus.list.unique%in%phy.genera.uniq)]


## first prune the phylogeny to include only these genera
phy.genera.ospree <- drop.tip(phy.plants,
                            which(!phy.genera%in%ospreegenus.inphylo))

## we can add species that may not be present according to their genera
names.to.add = species.list[which(!species.list%in%phy.genera.ospree$tip.label)]
phy.ospree.clean <- congeneric.merge(phy.genera.ospree,names.to.add,split="_")


## prunning the generated phylogeny to include ospree species only
phy.plants.ospree<-drop.tip(phy.ospree.clean,
                            which(!phy.ospree.clean$tip.label%in%species.list))

# 62 species are in the phylogeny
#plot(phy.plants.ospree)
write.tree(phy.plants.ospree,file = "../../data/phylogeny/ospree.phylogeny62sp.tre")

############################################

############################################
#### Fitting Phylogenetic models with PGLS - Full Cleaned Ospree Data
############################################




############################################


############################################
#### Fitting Phylogenetic models with PGLS - Dan Flynn's Data
############################################

## read and pre-process phylogeny
library(phytools)
phylo <- read.tree("../../data/phylogeny/ospreeFlynn.phylogeny.tre")
namesphy<-phylo$tip.label
namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)



## using caper to fit the simplest possible PGLS models

## A) generate a subsetted dataframe and a summarized version
databb = bb.stan[,c(26,16,20,30:32)]

databbslopes = as.data.frame(array(NA, dim=c(65,6)))
for(i in unique(databb$name)){#i="Quercus_alba"
  print(i)
  subs.i=subset(databb,name==i)
  index.i=which(unique(databb$name)==i)
  databbslopes[index.i,1] = i 
  databbslopes[index.i,2] = mean(subs.i$provenance.lat,na.rm=T)
  databbslopes[index.i,3] = mean(subs.i$resp,na.rm=T)
  mod1 = lm(resp~force.z+photo.z+chill.z,data=subs.i)
  if(nrow(summary(mod1)$coefficients)==4){
  databbslopes[index.i,4:6] = summary(mod1)$coefficients[2:4,1]
 }
}

colnames(databbslopes)=colnames(databb)



## B) generate a comparative.data object merging data and phylogeny
databbslopesphy = comparative.data(phylo,databbslopes[,-2],names.col="name",
                                   na.omit=T,vcv=T)

## C) fit slope only models to check for phylogenetic structure in sensitivities
lambda.force = pgls(force.z~1,data = databbslopesphy,lambda='ML')
summary(lambda.force)
plot(pgls.profile(lambda.force))

lambda.chill = pgls(chill.z~1,data = databbslopesphy,lambda='ML')
plot(pgls.profile(lambda.chill))
summary(lambda.chill)

lambda.photo = pgls(photo.z~1,data = databbslopesphy,lambda='ML')
plot(pgls.profile(lambda.photo))
summary(lambda.photo)


## the below does not make much sense as resp is the mean across responses
lambda.resp = pgls(resp~1,data = databbslopesphy,lambda='ML')
plot(pgls.profile(lambda.resp))
summary(lambda.resp)

lambda.full = pgls(resp~force.z+chill.z+photo.z,data = databbslopesphy,lambda='ML')
plot(pgls.profile(lambda.full))
summary(lambda.full)
plot(lambda.full)



## repeat process with intra-specific variation
# obtain response and predictor named vectors
phy.Ives=multi2di(phylo)
respY=databb$resp
names(respY)=databb$name
forceX=databb$force.z
names(forceX)=databb$name
chillX=databb$chill.z
names(chillX)=databb$name
photoX=databb$photo.z
names(photoX)=databb$name


# fit models
force.Ives = pgls.Ives(phy.Ives,X=forceX, y=respY)
chill.Ives = pgls.Ives(phy.Ives,X=chillX, y=respY)
photo.Ives = pgls.Ives(phy.Ives,X=photoX, y=respY)




## get phylogenetic covariance matrix
library(MCMCglmm)
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$spps<-bb.stan$phylo




## fitting models for forcing, chilling and photo, independently 
### FORCING
### ## ad mean of predictor across species and within species
bb.stan$species_mean <- 
  with(bb.stan, sapply(split(force.z, phylo), mean)[phylo])

bb.stan$within_species <- 
  bb.stan$force.z - bb.stan$species_mean

model_phylo <- brm(
  resp ~ species_mean + within_species +      ## fixed effs
    (1 + within_species|phylo) + (1|species),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 1000, warmup = 500
)

### CHILLING
### ## ad mean of predictor across species and within species

bb.stan$chillmeans <- 
  with(bb.stan, sapply(split(chill.z, phylo), mean)[phylo])

bb.stan$withinsp.chillmeans <- 
  bb.stan$chill.z - bb.stan$species_mean

## 
model_phylo.chill <- brm(
  resp ~ chillmeans + withinsp.chillmeans +      ## fixed effs
    (1 + withinsp.chillmeans|phylo) + (1|species),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 4, 
  iter = 1000, warmup = 500
)


### PHOTO
### ## ad mean of predictor across species and within species
bb.stan$photomeans <- 
  with(bb.stan, sapply(split(photo.z, phylo), mean)[phylo])

bb.stan$withinsp.photomeans <- 
  bb.stan$photo.z - bb.stan$species_mean

## 
model_phylo.photo <- brm(
  resp ~ photomeans + withinsp.photomeans +      ## fixed effs
    (1 + withinsp.photomeans|phylo) + (1|species),  ## rnd effs 
  data = bb.stan, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 20), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
  ,sample_prior = TRUE, chains = 2, cores = 4, 
  iter = 1000, warmup = 500
)

####################################


###########################################
## explore fitted models, ppc, phylo-signal 
## ########################################

## save main results as table
#fixed spec_pars cor_pars random
forceeffs<-rbind(
  summary(model_phylo)$fixed,
  summary(model_phylo)$random$phylo,
  summary(model_phylo)$random$species,
  summary(model_phylo)$spec_pars
)

chilleffs<-rbind(
  summary(model_phylo.chill)$fixed,
  summary(model_phylo.chill)$random$phylo,
  summary(model_phylo.chill)$random$species,
  summary(model_phylo.chill)$spec_pars
)

photoeffs<-rbind(
  summary(model_phylo.photo)$fixed,
  summary(model_phylo.photo)$random$phylo,
  summary(model_phylo.photo)$random$species,
  summary(model_phylo.photo)$spec_pars
)

write.csv(forceeffs,"output/force_effects.csv")
write.csv(chilleffs,"output/chill_effects.csv")
write.csv(photoeffs,"output/photo_effects.csv")



## Plot and save main results for posterior distributions of coefficients
## and ppcs

# forcing
plot(model_phylo, N = 5, ask = F)
# chilling
plot(model_phylo.chill, N = 5, ask = T)
#model_phylo.chill$fit
# photo
plot(model_phylo.photo, N = 5, ask = T)

#plot marginal effs
plot(marginal_effects(model_phylo), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.chill), points = TRUE,ask=T) 
plot(marginal_effects(model_phylo.photo), points = TRUE,ask=T) 


#plot ppcs
par(mfrow=c(1,3))
pp_check(model_phylo)
pp_check(model_phylo.chill)
pp_check(model_phylo.photo)


############################
###### Plot and save main results for posterior distributions of phylosignal
## forcing
hyp.force <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__within_species^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__within_species^2
  + sd_species__Intercept
  + cor_phylo__Intercept__within_species^2 
  + sigma^2) = 0.0"
  )
(lambda.force <- hypothesis(model_phylo, hyp.force, class = NULL))

## chill
hyp.chill <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__withinsp.chillmeans^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__withinsp.chillmeans^2
  + sd_species__Intercept
  + cor_phylo__Intercept__withinsp.chillmeans^2 
  + sigma^2) = 0.0"
  )
(lambda.chill <- hypothesis(model_phylo.chill, hyp.chill, class = NULL))

## photo
hyp.photo <- paste(
  "(sd_phylo__Intercept^2 + sd_phylo__withinsp.photomeans^2)/", 
  "(sd_phylo__Intercept^2 
  + sd_phylo__withinsp.photomeans^2
  + sd_species__Intercept
  + cor_phylo__Intercept__withinsp.photomeans^2 
  + sigma^2) = 0.0"
  )
(lambda.photo <- hypothesis(model_phylo.photo, hyp.photo, class = NULL))


## plotting posteriors
plot(lambda.force)
plot(lambda.chill)
plot(lambda.photo)





