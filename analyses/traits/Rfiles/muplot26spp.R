#started Dec 15, 2021 by Deirdre
# aim is to adapt the ospree bb ms muplot code to generate a mu plot for the 
# 26 species of the traitors mdl, how different are the estimates?

rm(list=ls())
options(stringsAsFactors = FALSE)

library(RColorBrewer)
library(rstan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if
(length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits")
}

# aim of this code is to adapt the mu plot code from the bb ms for the 26 spp. we are using in the traitors ms. 
figpath <- "figures"
figpathmore <- "traitors26spp"
xlim=c(-32,10)

files <- list.files(path = "output/raw", pattern =".Rda" )
files

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

## functions for plotting 

source("Rfiles/source/bb_muplot_trait.R")
source("Rfiles/source/plotletfx_trait.R")
#load("output/SeedMass_log10_stanfit_raw.Rda")
#Model <- 
#Model <- readRDS(paste("output/", files[3], sep = ""))
for(i in 1:length(files)){
load(paste("output/raw/", files[1], sep = ""))

sumer.ni <- summary(mdl.traitphen)$summary
sumer.ni[grep("beta", rownames(sumer.ni)),]
modelhere <- mdl.traitphen

# sumer.ni <- summary(m2l.ni)$summary
# sumer.ni[grep("mu_", rownames(sumer.ni)),]
# modelhere <- m2l.ni

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

#function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2)
muplotfx(modelhere, files[i], 7, 8, c(0.5,3), xlim , 12, 3)
}
###### Pheno only/ospree bb modle with just 26 species

figpathmore <- "ospree26spp"
xlim=c(-32,10)
load("output/m2lni_traitors_utah_z_26spp.Rda")
sumer.ni <- summary(m2l.ni)$summaryfigpath <- "figures"
sumer.ni[grep("mu_", rownames(sumer.ni)),]
modelhere <- m2l.ni

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

#function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2)
muplotfx(modelhere, "ospree26sp", 7, 8, c(0,3), xlim , 12, 3)

##########################################################################
# get mean trait data:
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