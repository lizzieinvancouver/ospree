## Script to build a phylogeny containing the species in Ospree departing from 
## the vascular plant megatree by Zanne et al. (2014);Nature

## Started by Ignacio Morales-Castilla on November 2018

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse)
library(stringr)
library(ape)
library(phytools)
library(geiger)
library(pez)
library(caper)
library(phangorn)

## set your wd here:
setwd("~/GitHub/ospree/")
source("analyses/bb_analysis/source/speciescomplex.multcues.R")


# get the data 
d <- read.csv("analyses/output/ospree_clean_withchill_BB.csv", header=TRUE)
bb.all <- sppcomplexfx.multcue(d)
str(bb.all)

columnstokeep <- c("datasetID", "study", "genus", "species", "varetc", "woody", "forcetemp", "material",
                   "photoperiod_day", "respvar", "respvar.simple", "response", "response.time", "fieldsample.date",
                   "Total_Chilling_Hours","Total_Utah_Model", "Total_Chill_portions",
                   "Exp_Chilling_Hours",  "Exp_Utah_Model","Exp_Chill_portions","chilldays","field.chill.units",
                   "figure.table..if.applicable.","complex.wname")

bb <- subset(bb.all, select=columnstokeep)


# geting a list of all species in ospree
bb$latbi <- paste(bb$genus, bb$species, sep="_")
#sort(unique(bb$latbi))

sps.list=sort(unique(bb$latbi))
genus.list=sort(unique(bb$genus))

complex.list=sort(unique(bb$complex.wname))


## correcting the list (removing sps names that are wrong or incomplete)
sps.list=sps.list[-c(18,63)]


## load phylogeny

## load phylo (from Zanne et al. 2014)
#phy.plants<-read.tree("data/phylogeny/Vascular_Plants_rooted.dated.tre")
#phy.plants<-read.tree("../../data/phylogeny/Vascular_Plants_rooted.dated.tre")

## load phylo (from Smith and Brown 2019)
phy.plants<-read.tree("data/phylogeny/ALLMB.tre")


## getting a list of genera in Zanne's phylo
phy.genera<-unlist(
  lapply(strsplit(phy.plants$tip.label, "_"),function(x){return(x[1])})
)

phy.genera.uniq<-sort(unique(phy.genera))



## how many ospree genera are in the phylogeny?
ospreegenus.inphylo<-genus.list[which(genus.list%in%phy.genera.uniq)]


## first prune the phylogeny to include only these genera
phy.genera.ospree<-drop.tip(phy.plants,
                            which(!phy.genera%in%ospreegenus.inphylo))
rm(phy.plants)

## we can add species that may not be present according to their genera
names.to.add=sps.list[which(!sps.list%in%phy.genera.ospree$tip.label)]
phy.ospree.clean<-congeneric.merge(phy.genera.ospree,names.to.add,split="_")


complexes.to.add=complex.list[which(!complex.list%in%phy.genera.ospree$tip.label)]
phy.ospree.clean.complex<-congeneric.merge(phy.genera.ospree,complexes.to.add,split="_")

## prunning the generated phylogeny to include ospree species only
phy.plants.ospree<-drop.tip(phy.ospree.clean,
                            which(!phy.ospree.clean$tip.label%in%sps.list))
# only 172 species are in the phylogeny

phy.plants.ospree.complexes<-drop.tip(phy.ospree.clean.complex,
                                      which(!phy.ospree.clean.complex$tip.label%in%complex.list))


plot(phy.plants.ospree,cex=.5)
plot(phy.plants.ospree.complexes,cex=.5)


## save phylogeny
#write.tree(phy.plants.ospree,"data/phylogeny/SBphylo_101sps.tre")
#write.tree(phy.plants.ospree.complexes,"data/phylogeny/SBphylo_62complex.tre")




