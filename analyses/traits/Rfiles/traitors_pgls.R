#started Nov 26, 2021 by Deirdre

# aim of this code is to test for phylogenetic effects on cues from the trait model
# using the casper package
# test for mean trait values and means of the cue posteriors
# code adapted from phylosignal_caper.R

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
}else setwd("~/Documents/github/ospree/analyses/traits")

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
library(reshape2)


  if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
  } else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
  } else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
  } 
  
  dat1 <- read.csv("input/try_bien_nodups_1.csv") 
  dat2 <- read.csv("input/try_bien_nodups_2.csv") 
  dat <- rbind(dat1, dat2)
  names(dat)
  setwd("..//bb_analysis") 
  
  # Flags to choose for bbstanleadin.R #
  
  # Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
  use.flags.for.mainmodel <- F
  use.flags.for.allsppmodel <- T
  use.yourown.flagdesign <- F
  nocrops <- T
  agiosponly <- T
  
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
    use.chillports = F # change to false for using utah instead of chill portions (most models use chill portions z)
    use.zscore = TRUE # change to false to use raw predictors
    
    # Default is species complex and no crops
    use.allspp = F
    use.multcuespp = FALSE
    use.cropspp = FALSE
    
    # Default is species complex use  alltypes of designs
    use.expramptypes.fp = TRUE
    use.exptypes.fp = FALSE
    
    #Default is all chilling dataN
    use.expchillonly = FALSE # change to true for only experimental chilling 
    #note: with only exp chilling, there is only exp photo and force too.
    #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
  }
  source("..//bb_analysis/source/bbstanleadin.R")
  namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
  bb.stan$spps<-paste(bb.stan$genus,bb.stan$species,sep="_")
  bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")
  bb.stan$speciesname<-paste(bb.stan$genus,bb.stan$species,sep="_")
  
  
  traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", 
                   "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", 
                   "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", 
                   "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")
  
  ospreeTraitors <- bb.stan[bb.stan$spps %in% traitors.sp,]
  meanResp <- aggregate(ospreeTraitors$resp, by = list(ospreeTraitors$speciesname), FUN = mean)
  names(meanResp) <- c("speciesname", "meanresp")
  head(meanResp)
  
  #Get Trait data ready for merging with ospree 
  #Rename some trait names to be more weildy 
  dat$traitname[which(dat$traitname == "seed mass")] <- "Seed_mass"
  dat$traitname[which(dat$traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass")] <- "LNC"
  dat$traitname[which(dat$traitname == "Specific_leaf_area")] <- "SLA"
  dat$traitname[which(dat$traitname == "Stem_specific_density")] <- "SSD"
  
  #select traits we are interested in
  triatSelect <- c("Seed_mass", "SLA", "SSD", "LNC", "Plant_height_vegetative")
  selectData <- dat[dat$traitname %in% triatSelect,]
  
  #Calculate mean values for each species
  meanTrait <- aggregate(selectData$traitvalue, by = list(selectData$traitname, selectData$speciesname), FUN = mean)
  names(meanTrait) <- c("traitname", "speciesname", "traitvalue")
  
  meanSSD <- meanTrait[meanTrait$traitname == "SSD",]
  nrow(meanSSD)
  meanSLA <- meanTrait[meanTrait$traitname == "SLA",]
  nrow(meanSLA)
  meanLNC <- meanTrait[meanTrait$traitname == "LNC",]
  nrow(meanLNC)
  meanSeed <- meanTrait[meanTrait$traitname == "Seed_mass",]
  nrow(meanSeed)
  meanHeight <- meanTrait[meanTrait$traitname == "Plant_height_vegetative",]
  nrow(meanHeight)
  
  #Merge ospree and traits data 
  meanTraitWide <- dcast(meanTrait, speciesname ~ traitname)
  
  #select species
  traitSelect <- meanTraitWide[meanTraitWide$species %in% traitors.sp,]
  
  #traitOspree <- merge(ospreeTraitors,traitSelect, by.y = "speciesname", by.x = "spps")

setwd("..//traits")
# get the mean cue estimates for a given trait
load("Rfiles/traitOspreeData.Rdata")
Model <- readRDS("output/SLA_stanfit.RDS")

ModelFit <- rstan::extract(Model)

muGrandSp <- data.frame(ModelFit$mu_grand_sp)
muGrandSpMean <- colMeans(muGrandSp)

betaForceSp <- data.frame(ModelFit$betaForceSp)
betaForceSpMean <- colMeans(betaForceSp)

quantile2575 <- function(x){
  returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
  return(returnQuanilte)
}

bf_quan <- apply(betaForceSp, 2, quantile2575)
mugrand_quan <- apply(muGrandSp, 2, quantile2575)

bfs <- rbind(betaForceSpMean, bf_quan)
bfs_t <- t(bfs)
bfs_df <- data.frame(bfs_t)
colnames(bfs_df)[colnames(bfs_df) == "X25."] <- "force25"
colnames(bfs_df)[colnames(bfs_df) == "X75."] <- "force75"

betaChillSp <- data.frame(ModelFit$betaChillSp)
betaChillSpMean <- colMeans(betaChillSp)
bc_quan <- apply(betaChillSp, 2, quantile2575)

bcs <- rbind(betaChillSpMean, bc_quan)
bcs_t <- t(bcs)
bcs_df <- data.frame(bcs_t)
colnames(bcs_df)[colnames(bcs_df) == "X25."] <- "chill25"
colnames(bcs_df)[colnames(bcs_df) == "X75."] <- "chill75"

betaPhotoSp <- data.frame(ModelFit$betaPhotoSp)
betaPhotoSpMean <- colMeans(betaPhotoSp)
bp_quan <- apply(betaPhotoSp, 2, quantile2575)

bps <- rbind(betaPhotoSpMean, bp_quan)
bps_t <- t(bps)
bps_df <- data.frame(bps_t)
colnames(bps_df)[colnames(bps_df) == "X25."] <- "photo25"
colnames(bps_df)[colnames(bps_df) == "X75."] <- "photo75"


cues <- cbind(meanResp, traitSelect, bfs_df, bcs_df, bps_df)
# write.csv(cues, "input/sla_cue_means.csv", row.names = F)
####################################
#### get phylogeny              ####
####################################

phylo <- read.tree("data/SBphylo_trait.tre")

namesphy<-phylo$tip.label
phylo<-force.ultrametric(phylo, method="extend")
phylo$node.label<-seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)
plot(phylo, cex=0.7)

## get phylogenetic covariance matrix
inv.phylo <- inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
ospreeTraitors$spps<-paste(ospreeTraitors$genus,ospreeTraitors$species,sep="_")
unique(ospreeTraitors$spps)
############################################################################
## B) generate a comparative.data object merging data and phylogeny
databbslopesphy = comparative.data(phylo,cues,names.col="speciesname",
                                   na.omit=TRUE,vcv=TRUE, warn.dropped = TRUE)
head(databbslopesphy)
databbslopesphy$dropped
phyloplot = databbslopesphy$phy
x = databbslopesphy$data$betaForceSpMean
y = databbslopesphy$data$betaChillSpMean
z = databbslopesphy$data$betaPhotoSpMean
names(x) = names(y) = names(z) = databbslopesphy$phy$tip.label

pdf("figures/sla_phylo.pdf", width = 10)
par(mfrow=c(1,3))
force <- contMap(phyloplot, x, lwd = 2.5, outline = F,fsize = c(0.8,1))
chill <- contMap(phyloplot, y, lwd = 2.5, outline = F,fsize = c(0.8,1))
photo <- contMap(phyloplot, z, lwd = 2.5, outline = F,fsize = c(0.8,1))
dev.off()

# X <- data.frame(forcing = x,
#                 chilling = y,
#                 photoperiod = z)
# 
# library(RColorBrewer)
# cols=brewer.pal(11, name = "Spectral")
# phylo.heatmap(phyloplot,X,standardize = F, fsize = c(0.65,1,0.8),
#               split = c(0.65,0.35), col = cols)

## 
lambda.force = pgls(betaForceSpMean ~ SLA, data = databbslopesphy,lambda='ML')
summary(lambda.force)

lambda.chill = pgls(betaChillSpMean~ SLA,data = databbslopesphy,lambda='ML')
summary(lambda.chill)

lambda.photo = pgls(betaPhotoSpMean~ SLA,data = databbslopesphy,lambda='ML')
summary(lambda.photo)

lambda.sla = pgls(SLA~1,data = databbslopesphy,lambda='ML')
summary(lambda.sla)

#other traits
# lambda.LNC = pgls(LNC~1,data = databbslopesphy,lambda='ML')
# summary(lambda.LNC)
# 
# lambda.ht = pgls(Plant_height_vegetative~1,data = databbslopesphy,lambda='ML')
# summary(lambda.ht)
# 
# lambda.seed = pgls(Seed_mass~1,data = databbslopesphy,lambda='ML')
# summary(lambda.seed)

lambda.full = pgls(meanresp ~ betaForceSpMean + betaChillSpMean + betaPhotoSpMean ,data = databbslopesphy,lambda='ML')
summary(lambda)

databbslopesphy
 par(mfrow=c(1,3),mar=c(4,5,3,2))
plot(pgls.profile(lambda.force),
     main="forcing")
plot(pgls.profile(lambda.chill),
     main="chilling")
plot(pgls.profile(lambda.photo),
     main="photoperiod")


## resp is the mean across responses and is modelled according
## the sensitivities of each species to each cue
lambda.full = pgls(meanresp ~ betaForceSpMean + betaChillSpMean + betaPhotoSpMean, data = databbslopesphy,lambda='ML')
plot(pgls.profile(lambda.full))
plot(lambda.full)
summary(lambda.full)

###################################
