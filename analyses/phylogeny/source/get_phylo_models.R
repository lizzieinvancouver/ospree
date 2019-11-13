## Script to build a phylogeny containing the species in Ospree departing from 
## the vascular plant megatree by Zanne et al. (2014);Nature

## Started by Ignacio Morales-Castilla on November 2018

#complex.list=sort(unique(bb.stan$complex.wname))


## correcting the list (removing sps names that are wrong or incomplete)
sps.list=sort(unique(bb.stan$name))
genus.list =sort(unique(bb.stan$genus))
## load phylogeny

## load phylo (from Zanne et al. 2014)
#phy.plants<-read.tree("data/phylogeny/Vascular_Plants_rooted.dated.tre")
#phy.plants<-read.tree("../../data/phylogeny/Vascular_Plants_rooted.dated.tre")

## load phylo (from Smith and Brown 2019)
phy.plants<-read.tree("../../data/phylogeny/ALLMB.tre")


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



## prunning the generated phylogeny to include ospree species only
phy.plants.ospree<-drop.tip(phy.ospree.clean,
                            which(!phy.ospree.clean$tip.label%in%sps.list))
# only 172 species are in the phylogeny


plot(phy.plants.ospree,cex=.5)


## save phylogeny
#write.tree(phy.plants.ospree,"data/phylogeny/SBphylo_101sps.tre")
#write.tree(phy.plants.ospree.complexes,"data/phylogeny/SBphylo_62complex.tre")




