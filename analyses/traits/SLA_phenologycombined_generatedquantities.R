
## Load libraries
library(rstan)

## Set number of cores
options(mc.cores = 1)

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
ospree <- read.csv("input/bbstan_allspp_utah.csv", header = TRUE)
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

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

mdl.traitphen <- stan("stan/phenology_combined_generatedquantities.stan",
                      data = all.data,
                      iter = 2000,
                      warmup = 1000,
                      chains = 1,
                      include = TRUE, pars = c("yPhenoi_pred"),
                      seed = 202109)

saveRDS(object = mdl.traitphen, file = "SLA_stanfit_generatedquantities.RDS")

mdl.traitphen.ext <- extract(mdl.traitphen)

pdf(file = "figures/Posterior_prediction_check_SLA_1.pdf", width = 10, height = 6)
par(mar = c(5, 6, 2, 2))
plot(density(all.data$yPhenoi), col = "black", lwd = 2, xlab = "yPheno, Occurrence time (post treatment)", ylab = "Density", main = "", cex.lab = 1.3)
post.samples <- sample(x = 1:1000, size = 100, replace = TRUE)
for(i in 1:length(post.samples)){
    points(density(mdl.traitphen.ext$yPhenoi_pred[post.samples[i], ]), col = rgb(0, 0, 1, alpha = .2), type = "l")
}
legend("topright", legend = c("Data", "Posterior prediction"), col = c("black", "blue"), lwd = c(3, 1), cex = 1.3, inset = 0.05)
dev.off()


## png(file = "Posterior_check.png", height = 600, width = 1600)

pdf(file = "figures/Posterior_prediction_check_SLA_2.pdf", width = 15, height = 6)
par(mfrow = c(1, 3), mar = c(5, 5, 2, 2))
plot(all.data$yPhenoi ~ all.data$forcei, xlab = "Forcing", ylab = "Occurrence time (post treatment)", main = "", cex.lab = 1.5, pch = 4, cex = 1.3, ylim = c(-50, 200))
post.samples <- sample(x = 1:1000, size = 100, replace = FALSE)
for(i in 1:length(post.samples)){
    points(mdl.traitphen.ext$yPhenoi_pred[i, ] ~ jitter(x = all.data$forcei, factor = 0.1), type = "p", pch = 16, cex = 0.7, col = rgb(0, 0, 1, alpha = .01))
}
abline(h = 0, lty = "dotted")
legend("topright", legend = c("Data", "Posterior prediction"), col = c("black", "blue"), pch = c(4, 16), cex = 1.3, inset = 0.05)
##
plot(all.data$yPhenoi ~ all.data$chilli, xlab = "Chilling", ylab = "Occurrence time (post treatment)", main = "", cex.lab = 1.5, pch = 4, cex = 1.3, ylim = c(-50, 200))
post.samples <- sample(x = 1:1000, size = 100, replace = FALSE)
for(i in 1:length(post.samples)){
    points(mdl.traitphen.ext$yPhenoi_pred[i, ] ~ jitter(x = all.data$chilli, factor = 0.1), type = "p", pch = 16, cex = 0.7, col = rgb(0, 0, 1, alpha = .01))
}
abline(h = 0, lty = "dotted")
legend("topright", legend = c("Data", "Posterior prediction"), col = c("black", "blue"), pch = c(4, 16), cex = 1.3, inset = 0.05)
##
plot(all.data$yPhenoi ~ all.data$photoi, xlab = "Photoperiod", ylab = "Occurrence time (post treatment)", main = "", cex.lab = 1.5, pch = 4, cex = 1.3, ylim = c(-50, 200))
post.samples <- sample(x = 1:1000, size = 100, replace = FALSE)
for(i in 1:length(post.samples)){
    points(mdl.traitphen.ext$yPhenoi_pred[i, ] ~ jitter(x = all.data$photoi, factor = 0.1), type = "p", pch = 16, cex = 0.7, col = rgb(0, 0, 1, alpha = .01))
}
abline(h = 0, lty = "dotted")
legend("topright", legend = c("Data", "Posterior prediction"), col = c("black", "blue"), pch = c(4, 16), cex = 1.3, inset = 0.05)
dev.off()
