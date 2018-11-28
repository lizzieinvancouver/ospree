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


# get the data 
bb.all <- read.csv("analyses/output/ospree_clean_withchill_BB.csv", header=TRUE)
# bb.all <- read.csv("output/ospree_clean_withchill_BB_2017Jun22.csv", header=TRUE)

bb.some <- subset(bb.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(bb.some, response.time!="")

columnstokeep <- c("datasetID", "study", "genus", "species", "varetc", "woody", "forcetemp", "material",
                   "photoperiod_day", "respvar", "respvar.simple", "response", "response.time", "fieldsample.date",
                   "Total_Chilling_Hours","Total_Utah_Model", "Total_Chill_portions",
                   "Exp_Chilling_Hours",  "Exp_Utah_Model","Exp_Chill_portions","chilldays","field.chill.units","figure.table..if.applicable.")

bb <- subset(bbdat, select=columnstokeep)

# geting a list of all species in ospree
bb$latbi <- paste(bb$genus, bb$species, sep="_")
#sort(unique(bb$latbi))

sps.list=sort(unique(bb$latbi))
genus.list=sort(unique(bb$genus))


## correcting the list (removing sps names that are wrong or incomplete)
sps.list=sps.list[-c(21,95)]


## load phylogeny

## load phylo (from Zanne et al. 2014)
phy.plants<-read.tree("data/phylogeny/Vascular_Plants_rooted.dated.tre")


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


## we can add species that may not be present according to their genera
names.to.add=sps.list[which(!sps.list%in%phy.genera.ospree$tip.label)]
phy.ospree.clean<-congeneric.merge(phy.genera.ospree,names.to.add,split="_")


## prunning the generated phylogeny to include ospree species only
phy.plants.ospree<-drop.tip(phy.ospree.clean,
                            which(!phy.ospree.clean$tip.label%in%sps.list))
# only 172 species are in the phylogeny

plot(phy.ospree.clean)

## save phylogeny
write.tree(phy.plants.ospree,"data/phylogeny/ospree.phylogeny.tre")


