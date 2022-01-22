
## Load libraries
library(rstan)
require(shinystan)

## Set number of cores
rm(list=ls())
options(stringsAsFactors = FALSE)

options(mc.cores = 4)

## Set seed
set.seed(2021)

#specify if this code should be run on Midge or on your own computer.
MidgeFlag <-T

if (MidgeFlag == TRUE){
  traitsData1 <- read.csv("../../data/Ospree_traits/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
  traitsData2 <- read.csv("../../data/Ospree_traits/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
} else if(MidgeFlag == FALSE) {
  setwd("/home/deirdreloughnan/Documents/github/ospree/analyses/traits/")
  traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
  traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
} else if(MidgeFlag == FALSE) {
  setwd("~/Documents/github/ospree/analyses/traits")
  traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
  traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
}

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

# traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

heightData <- read.csv("input/height_subsampled.csv")
heightData  <- subset(heightData , heightData $speciesname %in% traitors.sp)


# Read Ospree data and subset
ospree <- read.csv("input/bbstan_allspp_utah_37spp.csv", header = TRUE)
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

# Sorted species and study list
specieslist <- sort(unique(heightData$speciesname))
studylist <- sort(unique(heightData$datasetid))

#write.csv(heightData, "input/height_subsampled.csv", row.names = F)

## Prepare all data for Stan
all.data <- list(yTraiti = heightData$traitvalue,
                 N = nrow(heightData),
                 n_spec = length(specieslist),
                 trait_species = as.numeric(as.factor(heightData$speciesname)),
                 n_study = length(studylist),
                 study = as.numeric(as.factor(heightData$datasetid)),
                 prior_mu_grand_mu = 20,
                 prior_mu_grand_sigma = 10,
                 prior_sigma_sp_mu = 4,
                 prior_sigma_sp_sigma = 5,
                 prior_sigma_study_mu = 2,
                 prior_sigma_study_sigma = 5,
                 prior_sigma_traity_mu = 3,
                 prior_sigma_traity_sigma = 5,
                 ## Phenology
                 Nph = nrow(ospreeData),
                 phenology_species = as.numeric(as.factor(ospreeData$speciesname)),
                 yPhenoi = ospreeData$response.time,
                 forcei = ospreeData$force.z,
                 chilli = ospreeData$chill.z,
                 photoi = ospreeData$photo.z,
                 prior_muForceSp_mu = 0,
                 prior_muForceSp_sigma = 1,
                 prior_muChillSp_mu = 0,
                 prior_muChillSp_sigma = 1,
                 prior_muPhotoSp_mu = 0,
                 prior_muPhotoSp_sigma = 1 ,
                 prior_muPhenoSp_mu = 80,
                 prior_muPhenoSp_sigma = 20,
                 prior_sigmaForceSp_mu = 4,
                 prior_sigmaForceSp_sigma = 3,
                 prior_sigmaChillSp_mu = 4,
                 prior_sigmaChillSp_sigma = 3,
                 prior_sigmaPhotoSp_mu = 4,
                 prior_sigmaPhotoSp_sigma = 3,
                 prior_sigmaPhenoSp_mu = 0,
                 prior_sigmaPhenoSp_sigma = 10,
                 prior_betaTraitxForce_mu = 0,
                 prior_betaTraitxForce_sigma = 0.5,
                 prior_betaTraitxChill_mu = 0,
                 prior_betaTraitxChill_sigma = 0.5,
                 prior_betaTraitxPhoto_mu = 0,
                 prior_betaTraitxPhoto_sigma = 0.5,
                 prior_sigmaphenoy_mu = 20,
                 prior_sigmaphenoy_sigma = 5) 



mdl.traitphen <- stan("stan/phenology_combined.stan",
                      data = all.data,
                      iter = 4000,
                      warmup = 2000,
                      chains = 4,
                      include = FALSE, pars = c("y_hat"))

save(mdl.traitphen, file = "output/height_raw_37spp.Rda")
#saveRDS(object = mdl.traitphen, file = "height_stanfit.RDS")

# N effective?
range(summary(mdl.traitphen)$summary[, "n_eff"])
range(summary(mdl.traitphen)$summary[, "Rhat"])

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

pdf(file = "height_estimates_37spp.pdf", onefile = TRUE)
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


saveRDS(object = mdl.traitphen, file = "height_stanfit_37spp.RDS")
