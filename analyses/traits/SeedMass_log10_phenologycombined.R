
## Load libraries
library(rstan)
require(shinystan)

## Set number of cores
options(mc.cores = 4)

## Set seed
set.seed(202109)

#specify if this code should be run on Midge or on your own computer.
MidgeFlag <- FALSE

if (MidgeFlag == TRUE){
	traitsData1 <- read.csv("../../data/Ospree_traits/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
	traitsData2 <- read.csv("../../data/Ospree_traits/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
} else if(MidgeFlag == FALSE) {

      if(length(grep("Lizzie", getwd()))>0) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits/")
        }else if(length(grep("faith", getwd()))>0){ setwd("/home/faith/Documents/github/ospree/analyses/traits")
        }else setwd("/home/deirdre/ospree/")

    traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
    traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
  ospree <- read.csv("input/bbstan_allspp_utah_37spp.csv", header = TRUE)
}

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

# traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# Seed mass trait only
seedData <- traitsData[traitsData$traitname == "seed mass",]

# Read Ospree data and subset
ospree <- read.csv("input/bbstan_allspp_utah_37spp.csv", header = TRUE)
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

# Exclude 12_bien as a study due to one data point
aggregate(seedData$traitvalue, by = list(seedData$datasetid), FUN = length) # check
## aggregate(seedData$traitvalue, by = list(seedData$datasetid, seedData$speciesname), FUN = length)
### Subset
seedData <- subset(seedData, !(seedData$datasetid == "12_bien"))

# Sorted species and study list
specieslist <- sort(unique(seedData$speciesname))
studylist <- sort(unique(seedData$datasetid))

## Prepare all data for Stan
all.data <- list(yTraiti = log10(seedData$traitvalue),
                 N = nrow(seedData),
                 n_spec = length(specieslist),
                 trait_species = as.numeric(as.factor(seedData$speciesname)),
                 n_study = length(studylist),
                 study = as.numeric(as.factor(seedData$datasetid)),
                 prior_mu_grand_mu = log10(100),
                 prior_mu_grand_sigma = 0.5,
                 prior_sigma_sp_mu = 1,
                 prior_sigma_sp_sigma = 1,
                 prior_sigma_study_mu = 0.5,
                 prior_sigma_study_sigma = 0.1,
                 prior_sigma_traity_mu = 0.2,
                 prior_sigma_traity_sigma = 0.1,
                 ## Phenology
                 Nph = nrow(ospreeData),
                 phenology_species = as.numeric(as.factor(ospreeData$speciesname)),
                 yPhenoi = ospreeData$response.time,
                 forcei = ospreeData$force.z,
                 chilli = ospreeData$chill.z,
                 photoi = ospreeData$photo.z,
                 prior_muForceSp_mu = -15,
                 prior_muForceSp_sigma = 10,
                 prior_muChillSp_mu = -15,
                 prior_muChillSp_sigma = 10,
                 prior_muPhotoSp_mu = -15,
                 prior_muPhotoSp_sigma = 10,
                 prior_muPhenoSp_mu = 40,
                 prior_muPhenoSp_sigma = 2,
                 prior_sigmaForceSp_mu = 5,
                 prior_sigmaForceSp_sigma = 2,
                 prior_sigmaChillSp_mu = 5,
                 prior_sigmaChillSp_sigma = 2,
                 prior_sigmaPhotoSp_mu = 5,
                 prior_sigmaPhotoSp_sigma = 5,
                 prior_sigmaPhenoSp_mu = 10,
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
                      iter = 4000,
                      warmup = 2000,
                      chains = 4,
                      include = FALSE, pars = c("y_hat"))

save(mdl.traitphen, file = "output/seedmasslog10_raw_37spp.Rda")
## N effective?
head(summary(mdl.traitphen)$summary[order(summary(mdl.traitphen)$summary[, "n_eff"]), "n_eff"])

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

pdf(file = "SeedMass_log10_estimates_37spp.pdf", onefile = TRUE)
plot(mdl.traitphen, pars = c("mu_grand", "muSp"))
plot(mdl.traitphen, pars = c("muStudy"))
plot(mdl.traitphen, pars = c("muPhenoSp", "alphaPhenoSp"))
plot(mdl.traitphen, pars = c("muForceSp", "alphaForceSp"))
plot(mdl.traitphen, pars = c("muChillSp", "alphaChillSp"))
plot(mdl.traitphen, pars = c("muPhotoSp", "alphaPhotoSp"))
plot(mdl.traitphen, pars = c("betaTraitxForce", "betaTraitxChill", "betaTraitxPhoto"))
plot(mdl.traitphen, pars = c("betaTraitxForce","betaForceSp"))
plot(mdl.traitphen, pars = c("betaTraitxChill","betaChillSp"))
plot(mdl.traitphen, pars = c("betaTraitxPhoto","betaPhotoSp"))
plot(mdl.traitphen, pars = c("sigma_traity", "sigma_study", "sigma_sp", "sigmaPhenoSp", "sigmapheno_y"))
dev.off()

saveRDS(object = mdl.traitphen, file = "SeedMass_log10_stanfit_37spp.RDS")


 postSLA<- extract(mdl.traitphen)
 postSLAdf <- data.frame(postSLA)
# 

 cueEffects <- postSLAdf[,colnames(postSLAdf) %in% c("muForceSp", "muChillSp", "muPhotoSp")]
# 
   cueEffectPlot <- mcmc_intervals(cueEffects) + 
     theme_classic() + 
      labs(title = "intercep part of species cue slopes")

hist( postSLA$muForceSp, main = paste("muForceSp is " , signif(mean( postSLA$muForceSp),3), sep = ""))
       abline(v = mean( postSLA$muForceSp), col="red", lwd=3, lty=2)

hist(postSLA$betaTraitxForce, main = paste("betaTraitxForce is " , signif(mean( postSLA$betaTraitxForce),3), sep = ""))
       abline(v = mean( postSLA$betaTraitxForce), col="red", lwd=3, lty=2)
       