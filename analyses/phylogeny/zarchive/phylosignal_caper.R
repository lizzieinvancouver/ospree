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
library(phytools)
library(geiger)
library(RColorBrewer)


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

str(bb.stan.alltypes)
sum(is.na(datalist.bb$y))
str(dat.wothertreats)

############################################
#### Load species list and get phylogeny
############################################

## load species list (beware this list has both species and complexes)
#species.list <- read.csv("input/spslist.csv")
#species.list <- sort(species.list$Species_binomial)
bb.stan.alltypes$binomial <-paste(bb.stan.alltypes$genus,
                                  bb.stan.alltypes$species,sep="_") 
species.list <- sort(unique(bb.stan.alltypes$binomial))



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

phylo<-force.ultrametric(phy.plants.ospree, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)

# 62 species are in the phylogeny
#plot(phy.plants.ospree,cex=0.7)
#write.tree(phy.plants.ospree,file = "../../data/phylogeny/ospree.phylogeny62sp.tre")

############################################

############################################
#### Fitting Phylogenetic models with PGLS - Full Cleaned Ospree Data
############################################

# read and summarize ospree data

  ## A) generate a subsetted dataframe and a summarized version
  databb = bb.stan[,c(26,16,20,30:32)]

databbslopes = as.data.frame(array(NA, dim=c(65,6)))
colnames(databbslopes)=colnames(databb)
for(i in unique(databb$name)){#i="Abies_alba"
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




## B) generate a comparative.data object merging data and phylogeny
databbslopesphy = comparative.data(phylo,databbslopes[,-2],names.col="name",
                                   na.omit=T,vcv=T)


## C) Map sensitivities to each cue along the phylogeny
phyloplot = databbslopesphy$phy
x = databbslopesphy$data$force.z
y = databbslopesphy$data$chill.z
z = databbslopesphy$data$photo.z
names(x) = names(y) = names(z) = databbslopesphy$phy$tip.label

par(mfrow=c(1,3))
force <- contMap(phyloplot, x, lwd = 2.5, outline = F,fsize = c(0.8,1))
chill <- contMap(phyloplot, y, lwd = 2.5, outline = F,fsize = c(0.8,1))
photo <- contMap(phyloplot, z, lwd = 2.5, outline = F,fsize = c(0.8,1))

X <- data.frame(forcing = x,
                chilling = y,
                photoperiod = z)

dev.off()
library(RColorBrewer)
cols=brewer.pal(11, name = "Spectral")
phylo.heatmap(phyloplot,X,standardize = F, fsize = c(0.65,1,0.8),
              split = c(0.65,0.35), col = cols)


## D) fit intercept only models to check for phylogenetic structure in sensitivities
lambda.force = pgls(force.z~1,data = databbslopesphy,lambda='ML')
summary(lambda.force)

lambda.chill = pgls(chill.z~1,data = databbslopesphy,lambda='ML')
summary(lambda.chill)

lambda.photo = pgls(photo.z~1,data = databbslopesphy,lambda='ML')
summary(lambda.photo)

dev.off()
par(mfrow=c(1,3),mar=c(4,5,3,2))
plot(pgls.profile(lambda.force),
     main="forcing")
plot(pgls.profile(lambda.chill),
     main="chilling")
plot(pgls.profile(lambda.photo),
     main="photoperiod")


## resp is the mean across responses and is modelled according
## the sensitivities of each species to each cue
lambda.full = pgls(resp~force.z+chill.z+photo.z,data = databbslopesphy,lambda='ML')
plot(pgls.profile(lambda.full))
plot(lambda.full)
summary(lambda.full)





############################################


############################################
#### Check phylogenetic signal of each cue - Ospree Data
############################################

# read and summarize ospree data


## A) retrieve vectors and phylogeny
#phyloplot = databbslopesphy$phy
#x = databbslopesphy$data$force.z
#y = databbslopesphy$data$chill.z
#z = databbslopesphy$data$photo.z
#names(x) = names(y) = names(z) = databbslopesphy$phy$tip.label
resp = databbslopesphy$data$resp
names(resp) = databbslopesphy$phy$tip.label


## B) make the phylogeny dichotomous (otherwise fitContinuous does not run)
databbslopesphy$phy = multi2di(databbslopesphy$phy)

## C) fit and store phylogenetic signal models for sensitivities to each cue
store.phylosig <- array(NA, dim = c(3,7))
colnames(store.phylosig)<-c("sigma2","White.Lik","BM.Lik","Lambda","Lambda.Lik","alpha","OU.Lik")
row.names(store.phylosig)<-c("forcing","chilling","photoperiod")

phy.OU<-rescaleTree(databbslopesphy$phy,1)
list.dat<-list(x,y,z)

for(i in 1:3){#i=1
print(i)
trait = list.dat[[i]]
OU <- fitContinuous(phy.OU,trait,model="OU",bounds=list(alpha=c(0.0005,100)))
BM <- fitContinuous(databbslopesphy$phy,trait,model="BM")
lambda <- fitContinuous(databbslopesphy$phy,trait,model="lambda")
white <- fitContinuous(databbslopesphy$phy,trait,model="white")

store.phylosig[i,1] <- BM$opt$sigsq
store.phylosig[i,2] <- white$opt$lnL
store.phylosig[i,3] <- BM$opt$lnL
store.phylosig[i,4] <- lambda$opt$lambda
store.phylosig[i,5] <- lambda$opt$lnL
store.phylosig[i,6] <- log(OU$opt$alpha,10)*(-1)
store.phylosig[i,7] <- OU$opt$lnL

}

write.csv(store.phylosig,file = "output/Phyl.sig.each.cue.csv")

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

############################################


