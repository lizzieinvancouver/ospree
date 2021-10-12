
## Load libraries
library(rstan)
require(shinystan)

## Set number of cores
options(mc.cores = 4)

## Set seed
set.seed(202109)

#specify if this code should be run on Midge or on your own computer.
MidgeFlag <- TRUE

if (MidgeFlag == TRUE){
	traitsData1 <- read.csv("../../data/Ospree_traits/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
	traitsData2 <- read.csv("../../data/Ospree_traits/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
} else if(MidgeFlag == FALSE) {
	setwd("/home/faith/Documents/github/ospree/analyses/traits/")
	traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
	traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
}

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# SLA trait only
slaData <- traitsData[traitsData$traitname == "Specific_leaf_area",]

# Read Ospree data and subset
ospree <- read.csv("bbstan_allspp_utah.csv", header = TRUE)
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospreeData$speciesname %in% traitors.sp)

# Sorted species and study list
specieslist <- sort(unique(slaData$speciesname))
studylist <- sort(unique(slaData$datasetid))

## Prepare all data for Stan
all.data <- list(yTraiti = slaData$traitvalue,
                 N = nrow(slaData),
                 n_spec = length(specieslist),
                 trait_species = as.numeric(as.factor(slaData$speciesname)),
                 n_study = length(studylist),
                 study = as.numeric(as.factor(slaData$datasetid)),
                 prior_mu_grand_mu = 17,
                 prior_mu_grand_sigma = 2,
                 prior_sigma_sp_mu = 10,
                 prior_sigma_sp_sigma = 2,
                 prior_sigma_study_mu = 5,
                 prior_sigma_study_sigma = 2,
                 prior_sigma_traity_mu = 2,
                 prior_sigma_traity_sigma = .5,
                 ## Phenology
                 Nph = nrow(ospreeData),
                 phenology_species = as.numeric(as.factor(ospreeData$speciesname)),
                 yPhenoi = ospreeData$response.time,
                 forcei = ospreeData$force.z,
                 chilli = ospreeData$chill.z,
                 photoi = ospreeData$photo.z,
                 prior_muForceSp_mu = 0,
                 prior_muForceSp_sigma = 2,
                 prior_muChillSp_mu = 0,
                 prior_muChillSp_sigma = 2,
                 prior_muPhotoSp_mu = 0,
                 prior_muPhotoSp_sigma = 2,
                 prior_muPhenoSp_mu = 40,
                 prior_muPhenoSp_sigma = 2,
                 prior_sigmaForceSp_mu = 5,
                 prior_sigmaForceSp_sigma = 2,
                 prior_sigmaChillSp_mu = 5,
                 prior_sigmaChillSp_sigma = 2,
                 prior_sigmaPhotoSp_mu = 5,
                 prior_sigmaPhotoSp_sigma = 2,
                 prior_sigmaPhenoSp_mu = 5,
                 prior_sigmaPhenoSp_sigma = 2,
                 prior_betaTraitxForce_mu = 0,
                 prior_betaTraitxForce_sigma = 2,
                 prior_betaTraitxChill_mu = 0,
                 prior_betaTraitxChill_sigma = 2,
                 prior_betaTraitxPhoto_mu = 0,
                 prior_betaTraitxPhoto_sigma = 2,
                 prior_sigmaphenoy_mu = 2,
                 prior_sigmaphenoy_sigma = 2
                   ) 

mdl.traitphen <- stan("stan/phenology_combined.stan",
                      data = all.data,
                      iter = 2000,
                      warmup = 1000,
                      chains = 4,
                      include = FALSE, pars = c("y_hat"),
                      seed = 202109)

## N effective?
summary(mdl.traitphen)$summary[, "n_eff"]

### Add species and study names to Stan object
names(mdl.traitphen)[grep(pattern = "^muSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")
names(mdl.traitphen)[grep(pattern = "^muStudy", x = names(mdl.traitphen))] <- paste(studylist, sep = "")
##
names(mdl.traitphen)[grep(pattern = "^alphaForceSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")
names(mdl.traitphen)[grep(pattern = "^alphaChillSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")
names(mdl.traitphen)[grep(pattern = "^alphaPhotoSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")
names(mdl.traitphen)[grep(pattern = "^alphaPhenoSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")
names(mdl.traitphen)[grep(pattern = "^betaForceSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")
names(mdl.traitphen)[grep(pattern = "^betaChillSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")
names(mdl.traitphen)[grep(pattern = "^betaPhotoSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")
names(mdl.traitphen)[grep(pattern = "^betaPhenoSp", x = names(mdl.traitphen))] <- paste(specieslist, sep = "")

pdf(file = "SLA_estimates.pdf", onefile = TRUE)
plot(mdl.traitphen, pars = c("mu_grand", "muSp"))
plot(mdl.traitphen, pars = c("muStudy"))
plot(mdl.traitphen, pars = c("muPhenoSp", "alphaPhenoSp"))
plot(mdl.traitphen, pars = c("muForceSp", "alphaForceSp"))
plot(mdl.traitphen, pars = c("muChillSp", "alphaChillSp"))
plot(mdl.traitphen, pars = c("muPhotoSp", "alphaPhotoSp"))
plot(mdl.traitphen, pars = c("betaTraitxForce", "betaForceSp"))
plot(mdl.traitphen, pars = c("betaTraitxChill", "betaChillSp"))
plot(mdl.traitphen, pars = c("betaTraitxPhoto", "betaPhotoSp"))
dev.off()
