#started Nov 26, 2021 by Deirdre

# aim of this code is to test for phylogenetic effects on cues from the trait model
# using the casper package
# test for mean trait values and means of the cue posteriors

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

# Set Midge Flag
Midge <- FALSE

color_scheme_set("viridis")

#Source ospree trators data if thsi runs on Faith's section of Midge
if(Midge == TRUE){
  setwd("~/traits")
  load("traitOspreeData.Rdata")
  
} 

#Code taken from Phylo_ospree_reanalyses.R to feed in the Ospree data
#----------------------------------------------------------------------

if(Midge == FALSE){
  # Anyone else working with this code should add their info/path here
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
  
  
  traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", 
                   "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", 
                   "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", 
                   "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")
  
  ospree_traitors <- bb.stan[bb.stan$spps %in% traitors.sp,]
  
  #Get Trait data ready for merging with ospree 
  #Rename some trait names to be more weildy 
  dat$traitname[which(dat$traitname == "seed mass")] <- "Seed_mass"
  dat$traitname[which(dat$traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass")] <- "LNC"
  dat$traitname[which(dat$traitname == "Specific_leaf_area")] <- "SLA"
  dat$traitname[which(dat$traitname == "Stem_specific_density")] <- "SSD"
  
  #Exploring the data 
  unique(dat$traitname)
  table(dat$speciesname, dat$traitname)
  
  densityData <- dat[dat$traitname == "SSD",]
  table(densityData$speciesname)
  unique(densityData$speciesname)
  unique(densityData$datasetid)
  
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
  
  #combine data 
  traitOspree <- merge(ospree_traitors,traitSelect, by.y = "speciesname", by.x = "spps")
  
  setwd("..//traits") 
  #save(traitOspree, file = "Rfiles/traitOspreeData.Rdata")
}
getwd()

####################################
#### get phylogeny              ####
####################################
setwd("traits") 
phylo <- read.tree("data/SBphylo_trait.tre")

load("output/joint_SLA.RData")
