### Started Dec 5 2018 ###

## What species should we get trait data for? How much can we get for the good species in OSPREE? ##

#The aim of this code is to get a list of the speices for which there are more than one study, excluding the species that are in the species complex's
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/github/ospree/analyses")

library(rstan)
library(ggplot2)
library(shinystan)
# library(bayesplot)
library(dplyr)
# library(rstanarm)

#source('..//stan/savestan.R')
source("traits/source/trait.species.R")
source("bb_analysis/source/bbdataplease.R")
source("bb_analysis/source/commoncols.R")
source("bb_analysis/source/othertreats.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################################################
#Stealing code from the bbstanleadin.R file#
#Aim is to get a list of species that we want to use 
################################################################# 

## 3 steps to major cleaning: Get the data, merge in taxa info, subset down to what we want for:
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!

## (1) Get the data and slim down to correct response and no NAs ...
source("source/bbdataplease.R")
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
source("source/othertreats.R")
dim(bb.noNA)
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
dim(bb.noNA)
d <- bb.noNA
## (3) Get fewer columns for sanity
source("source/commoncols.R")
bb <- subset(d, select=c(columnstokeep, columnscentered, columnschillunits))

# remove the two values above 600
bb <- subset(bb, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb$chill <- bb$chill/240
length(unique(bb$datasetID))
length(unique(bb$genus))
head(bb.expphoto)

# deal with photo
#
unique(bb$photo_type)
bb.expphoto <- subset(bb, photo_type=="exp"); head(bb.expphoto)
bb.rampphoto <- subset(bb, photo_type=="ramped"); head(bb.rampphoto)
bb.exprampphoto <- subset(bb, photo_type=="exp" | photo_type=="ramped"); head(bb.exprampphoto)
#
bb.ambphoto <- subset(bb, photo_type=="amb" | photo_type=="none"); head(bb.ambphoto)
#sort(unique(bb.expphoto$datasetID))
#sort(unique(bb.rampphoto$datasetID))
#sort(unique(bb.ambphoto$datasetID))

# add in forcing
bb.exprampphotoforce <- subset(bb.exprampphoto, force_type=="exp"|force_type=="ramped")
bb.expphotoforce <- subset(bb.expphoto, force_type=="exp")
head(bb.expphotoforce)

temp<-spptraitfx(bb.expphotoforce)
temp

spp.list<-unique(temp$name); spp.list
length(spp.list)
#write.table(spp.list, file="Species.list.OSPREE.csv", row.names = FALSE, col.names=FALSE)

require(BIEN)

result<-vector()
for(i in 1:length(spp.list)){
  traittemp<-BIEN_trait_traitbyspecies(species=spp.list[1], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))
  result<-traittemp
}

####################################################################
## Get BIEN data for the species Nacho is using for the phylogeny
#This code is taken from models_phylo.R, written by Cat and Dan
setwd("~/Documents/github/ospree/analyses/phylogeny") 

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


####################################
#### Fitting Phylogenetic brms
####################################

## read and pre-process phylogeny
library(phytools)
phylo <- read.tree("../../data/phylogeny/ospreeFlynn.phylogeny.tre")
namesphy<-phylo$tip.label
namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)


## get phylogenetic covariance matrix
library(MCMCglmm)
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep=" ")
bb.stan$spps<-bb.stan$phylo

phylospp<-unique(bb.stan$spps)
phylossp[1]

##### Now getting the trait data from BIEN ##################################################
traitadd<-BIEN_trait_traitbyspecies(species=phylospp[1], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

for (i in c(2:length(phylospp))){
  
  traittemp<-BIEN_trait_traitbyspecies(species=phylospp[i], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))
  # df<-data.frame(traitadd)
  traitadd<-rbind(traitadd, traittemp)
}

require(tidyr)
#DBH swamps all other traits and I have no reason to believe it is a significant trait
BIEN_traitdata<-spread(traitadd, key="trait_name", value="trait_value")
head(BIEN_traitdata)
unique(BIEN_traitdata$scrubbed_species_binomial)


write.csv(BIEN_traitdata, file="Phylospp_BIEN_traitdata.csv")
# 
#


#<><><><><><><><><><><><><><><><>#
#Oct 15, 2019 update: new species list with many new species 
#Requesting new data

#redone Novemebr 9th with species from complexes
#<><><><><><><><><><><><><><><><>#
setwd("~/Documents/github/ospree/data")

newspp<-read.csv("traits/ospree_newsplist.csv", header=TRUE)

spp.list<-gsub("_", " ", newspp$Spp, fixed = TRUE)
str(spp.list)
#write.table(spp.list, file="Species.list.OSPREE.csv", row.names = FALSE, col.names=FALSE)

test<-head(spp.list,20)
require(BIEN)


traitadd<-BIEN_trait_traitbyspecies(species=test[1], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

result<-vector()
for (i in c(2:length(test))){
  
  traittemp<-BIEN_trait_traitbyspecies(species=test[i], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))
  # df<-data.frame(traitadd)
  result<-rbind(result,traittemp)
}

BIEN_traitdata<-rbind(result, traitadd)

head(traitadd)


require(tidyr)
#DBH swamps all other traits and I have no reason to believe it is a significant trait
BIEN_wide<-spread(BIEN_traitdata, key="trait_name", value="trait_value")
head(BIEN_wide)
unique(BIEN_wide$scrubbed_species_binomial)


write.csv(BIEN_traitdata, file="newspp_BIEN_traitdata_Nov11.csv")
