#Running teh phenology model with a potential effect of trait, but feeding in mean trait valus rather than using another model to estimate trait for each species
#We are doing this so we can have a backup if teh joint models dont work, and so we can sanity check the joint model results

#Started by Faith Jones Sep 23 2021

rm(list = ls()) 
options(stringsAsFactors = FALSE)

library(tidyr)
library(plyr)
library(dplyr)
library(vegan)
library(reshape2)

# Set working directory: 

# Set working directory: 
# Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
} else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
} 

# Get the data trait data
dat1 <- read.csv("input/try_bien_nodups_1.csv") 
dat2 <- read.csv("input/try_bien_nodups_2.csv") 
dat <- rbind(dat1, dat2)
names(dat)



#Code taken from Phylo_ospree_reanalyses.R to feed in the Ospree data
#----------------------------------------------------------------------
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
  
  #Default is all chilling data
  use.expchillonly = FALSE # change to true for only experimental chilling 
  #note: with only exp chilling, there is only exp photo and force too.
  #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
}

source("..//bb_analysis/source/bbstanleadin.R")

namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
bb.stan$spps<-paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")

setwd("..//traits") 


#Subset ospree data to certain species
#-------------------------------------------------------

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


#Run stan model for SLA
#---------------------------------------------------------------------

#Remove rows without SLA data
SLAData <- traitOspree[!is.na(traitOspree$SLA),]

names(SLAData)
str(SLAData)

#Nnumber of species 
Nssp <- length(unique(SLAData$spps))

#Number of observations
N <- nrow(SLAData)

#species column as an integer 
species <- as.integer(as.factor(SLAData$spps))

#Cues
force.i <- SLAData$force.z
photo.i <- SLAData$photo.z 
chill.i <- SLAData$chill.z 

#budburst date
yPhenoi <- SLAData$resp

## pehnology and mean trait stan model ###########################################################
pheno_data <- list(alphaTraitSp = SLAData$SLA, #mean species trait value 
                   N = N, 
                   n_spec = Nspp, 
                   species = species, 
                   yPhenoi = yPhenoi, 
                   forcei = force.i,
                   photoi = photo.i, 
                   chilli = chill.i,

                #Priors

                   prior_sigmaphenoy_mu = 5,  #mean of prior distribution of the general error (sigma_y) around the mean predicted value
                   prior_sigmaphenoy_sigma = 3, # variance of the prior distribution of the general error sigma)y around the mean predicted value
                   
                   prior_muForceSp_mu = 0, # mean of the prior distribution of the varience around the mean effect of forcing 
                   prior_muForceSp_sigma = 30, # vareince of the prior distribution of the varience around the mean effect of forcing 

                   prior_muChillSp_mu = 0,# mean of the prior distribution of the varience around the mean effect of chilling 
                   prior_muChillSp_sigma = 30,# varience of the prior distribution of the varience around the mean effect of chilling 

                   prior_muPhotoSp_mu = 0,# mean of the prior distribution of the varience around the mean effect of photoperiod 
                   prior_muPhotoSp_sigma = 10,# varience of the prior distribution of the varience around the mean effect of photoperiod 

                   prior_muPhenoSp_mu = 150, # mean of prior distribution of the mean (grand alpha) value of the phenology model
                   prior_muPhenoSp_sigma = 10, # variance of prior distribution of the mean (grand alpha) value of the phenology model
                   prior_sigmaPhenoSp_mu = 0,#the mean of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                   prior_sigmaPhenoSp_sigma = 10,  #the varience of the prior of the spread of species phenology values around teh grand mean muPhenoSp 


                   prior_sigma_sp_sigma = 10,
                   prior_mu_study = 0,
                   prior_sigma_study_mu = 10,
                   prior_sigma_study_sigma = 10,

                   #prior_sigmaForceSp_mu = , # Faith doesn't knwo what these do
                   #prior_sigmaForceSp_sigma = ,
                   #prior_sigmaChillSp_mu =,
                   #prior_sigmaChillSp_sigma=,
                   #prior_sigmaPhotoSp_mu=,
                   #prior_sigmaPhotoSp_sigma=,

                   prior_betaTraitxForce_mu=0, # the mean of the prior distribution of the effect of trait on the slope of forcing 
                   prior_betaTraitxForce_sigma=1, # the varience of the prior distribution of the effect of trait on the slope of forcing 
                   prior_betaTraitxChill_mu=0,# the mean of the prior distribution of the effect of trait on the slope of chilling 
                   prior_betaTraitxChill_sigma=1,# the varience of the prior distribution of the effect of trait on the slope of chilling 
                   prior_betaTraitxPhoto_mu=0,# the mean of the prior distribution of the effect of trait on the slope of photo period 
                   prior_betaTraitxPhoto_sigma=1,# the varience of the prior distribution of the effect of trait on the slope of photo period 


                   ) 

prior_betaTraitxForce_mu
prior_sigmaPhenoSp_sigma

 #Stan code: joint_3cue_phenonly.Stan   real prior_sigmaphenoy_mu;

