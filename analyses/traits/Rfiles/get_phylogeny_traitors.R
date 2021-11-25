# started Nov 25 2021 by Deirdre

# aim to get a tree from the Smith & Brown (2019) tree to use for a PGLS of the OSPREE traitors species

# Started Nov 25, 2021 by Deirdre 

# aim of this code is to get a phylogenetic tree for my pheno_bc species
# code adapted from Get.ospree.phylo.R
#https://github.com/FePhyFoFum/big_seed_plant_trees/releases

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(stringr)
library(ape)
library(phytools)
library(geiger)
library(pez)
library(caper)
library(phangorn)

## set your wd here:
setwd("~/Documents/github/ospree/analyses")

bb <- read.csv("traits/notes/traits_26species.csv", header=TRUE)
bb$temp <- bb$species.name
temp <- str_split_fixed(bb$temp, " ", 2)
bb$phylo.name <- paste(temp[,1], temp[,2], sep="_")
bb$genus <- temp[,1]
bb$species <- temp[,2]

# # specify a few subsp and different names:
bb$phylo.name [bb$phylo.name == "Rhamnus_carthartica"] <- "Rhamnus_cathartica"
bb$phylo.name [bb$phylo.name  == "Fagus_grandifolia"] <- "Fagus_grandifolia_var._caroliniana"
bb$phylo.name [bb$phylo.name  == "Fagus_sylvatica"] <- "Fagus_sylvatica_var._atropunicea"

bb$phylo.name [bb$phylo.name == "Quercus_ilex"] <- "Quercus_ilex_subsp._ilex"
bb$phylo.name [bb$phylo.name == "Quercus_robur"] <- "Quercus_robur_subsp._robur"
bb$phylo.name [bb$phylo.name == "Quercus_petraea"] <- "Quercus_petraea_subsp._petraea"

bb$phylo.name [bb$phylo.name  == "Spiraea_alba"] <- "Spiraea_alba_var._latifolia"
bb$phylo.name [bb$phylo.name  == "Rhamnus_frangula"] <- "Rhamnus_arguta"

sps.list <- sort(unique(bb$phylo.name))
genus.list=sort(unique(bb$genus))

## load phylo (from Smith and Brown 2019)
phy.plants<-read.tree("../data/phylogeny/ALLMB.tre")

## getting a list of genera in S&B's phylo
phy.genera<-unlist(
  lapply(strsplit(phy.plants$tip.label, "_"),function(x){return(x[1])})
)
phy.genera.uniq<-sort(unique(phy.genera))

#temp <- sort(unique(phy.plants$tip.label)) #356305 species

## how many trait species are in the phylogeny?
phenosp.genus.inphylo<-genus.list[which(genus.list%in%phy.genera.uniq)]


## first prune the phylogeny to include only these genera
phy.genera.trait<-drop.tip(phy.plants,
                             which(!phy.genera %in% phenosp.genus.inphylo)) #8814 tips
rm(phy.plants)
View(sort(phy.genera.trait$tip.label))
# now prune just the species I want
phy.plants.trait<- drop.tip(phy.genera.trait,
                              which(!phy.genera.trait$tip.label %in% sps.list))

length(phy.plants.trait$tip.label)
sort(phy.plants.trait$tip.label)
# only 172 species are in the phylogeny

pdf("figures/phylogeny.pdf")
plot(phy.plants.trait,cex=.5)
dev.off()

# save phylogeny
write.tree(phy.plants.trait,"data/SBphylo_trait.tre")

