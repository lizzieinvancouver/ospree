#Running teh phenology model with a potential effect of trait, but feeding in mean trait valus rather than using another model to estimate trait for each species
#We are doing this so we can have a backup if teh joint models dont work, and so we can sanity check the joint model results

#Started by Faith Jones Sep 23 2021

rm(list = ls()) 
options(stringsAsFactors = FALSE)

library(tidyr)
library(plyr)
library(dplyr)
library(vegan)

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


#Get the ospree bb data (taken from bb basic analysis R script)
bb.all <- read.csv("../output/ospree_clean_withchill_BB.csv", header=TRUE)
# bb.all <- read.csv("output/ospree_clean_withchill_BB_2017Jun22.csv", header=TRUE)
bb.some <- subset(bb.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(bb.some, response.time!="")

columnstokeep <- c("datasetID", "study", "genus", "species", "varetc", "woody", "forcetemp", "material",
    "photoperiod_day", "respvar", "respvar.simple", "response", "response.time", "fieldsample.date",
    "Total_Chilling_Hours","Total_Utah_Model", "Total_Chill_portions",
    "Exp_Chilling_Hours",  "Exp_Utah_Model","Exp_Chill_portions","chilldays","field.chill.units","figure.table..if.applicable.")
    
bb <- subset(bbdat, select=columnstokeep)

bb$speciesname <- paste(bb$genus, bb$species, sep = "_")
names(bb)

# make a bunch of things numeric (eek!)
bb$force <- as.numeric(bb$forcetemp)
bb$photo <- as.numeric(bb$photoperiod_day)
bb$resp <- as.numeric(bb$response.time)
bb$chillhrs <- as.numeric(bb$Total_Chilling_Hours)
bb$chillpor <- as.numeric(bb$Total_Chill_portions)
bb$utah <- as.numeric(bb$Total_Utah_Model)
bb$expchillhrs <- as.numeric(bb$Exp_Chilling_Hours)
bb$expchillpor <- as.numeric(bb$Exp_Chill_portions)
bb$exputah <- as.numeric(bb$Exp_Utah_Model)

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

#Select only a subset of species
traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")
traitSp <- dat[dat$speciesname %in% traitors.sp,]


#Sort out ospree phenology data





#combine data 


#Run stan model for SLA



## pehnology and mean trait stan model ###########################################################
pheno_data <- list(yTraiti = SLAData$traitvalue, 
                   N = Ntrt, 
                   n_spec = Nspp, 
                   species = SLAData$species, 
                   
                   prior_sigmaphenoy_mu = 5,
                   prior_sigmaphenoy_sigma = 3,
                   
                   prior_muForceSp_mu = 0,
                   prior_muForceSp_sigma = 30,
                   prior_muChillSp_mu = 0,
                   prior_muChillSp_sigma = 30,
                   prior_muPhotoSp_mu = 0,
                   prior_muPhotoSp_sigma = 10,
                   prior_muPhenoSp_mu = 150,
                   prior_muPhenoSp_sigma = 10,
                   
                   prior_sigma_sp_sigma = 10,
                   prior_mu_study = 0,
                   prior_sigma_study_mu = 10,
                   prior_sigma_study_sigma = 10,




                   ) 


    real prior_sigmaForceSp_mu;
    real prior_sigmaForceSp_sigma;
    real prior_sigmaChillSp_mu;
    real prior_sigmaChillSp_sigma;
    real prior_sigmaPhotoSp_mu;
    real prior_sigmaPhotoSp_sigma;
    real prior_sigmaPhenoSp_mu;
    real prior_sigmaPhenoSp_sigma;
    real prior_betaTraitxForce_mu;
    real prior_betaTraitxForce_sigma;
    real prior_betaTraitxChill_mu;
    real prior_betaTraitxChill_sigma;
    real prior_betaTraitxPhoto_mu;
    real prior_betaTraitxPhoto_sigma;


 #Stan code: joint_3cue_phenonly.Stan