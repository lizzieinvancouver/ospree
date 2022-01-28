# Started January 27, 2022

# the purpose of this code is to have source code for building tables for the traitors ms

library(rstan)
require(shinystan)
library(hdrcde) ## better quantiles
library(tidybayes)
# setwd("~/Documents/github/ospree/analyses/traits")
## Set seed
set.seed(202109)

# Specify if this code should be run on Midge or on your own computer.
MidgeFlag <- FALSE

if(MidgeFlag == TRUE){
  traitsData1 <- read.csv("../../data/Ospree_traits/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
  traitsData2 <- read.csv("../../data/Ospree_traits/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
  ospree <- read.csv("../../data/Ospree_traits/bbstan_allspp.utah.csv", stringsAsFactors = FALSE, header = TRUE)
  posterior <- extract(readRDS(file = "../../data/Ospree_traits/SeedMass_log10_stanfit.RDS"))
} else{
  traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
  traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
  ospree <- read.csv("input/bbstan_allspp_utah_37spp.csv", stringsAsFactors = FALSE, header = TRUE)
  posterior <- extract(readRDS(file = "output/SeedMass_log10_stanfit_37spp.RDS"))
  posteriorOld <- extract(readRDS(file = "output/SeedMass_log10_stanfit.RDS"))
  
}

specieslist <-  c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

load("output/raw/height_raw_37spp.Rda")
get_variables(mdl.traitphen)

sumt <- summary(mdl.traitphen)$summary

col4table <- c("mean","sd","2.5%","50%","97.5%","Rhat")

mu_params <-   c("mu_grand",
                 "muPhenoSp",
                 "muForceSp",
                 "muChillSp",
                 "muPhotoSp",
                 "betaTraitxForce",
                 "betaTraitxChill",
                 "betaTraitxPhoto")
esti <- sumt[mu_params, col4table]

temp <- c(mugrandtrait, muStudy, muGrandSpname, betaForceSpname)
rownames(esti) =c("Grand trait mean",
                  "Grand Species mean",
                     "Beta Forcing",
                     "Beta Chilling",
                     "Beta Photoperiod",
                     "Beta Trait x Forcing",
                     "Beta Trait x Chilling",
                     "Beta Trait x Photoperiod")

esti.table <- sumt[mu_params, col4table]
row.names(esti.table) <- row.names(esti)

write.csv(esti.table, "seedmassMdlOutput.csv", row.names = T)


# Long tables:
muStudy <- names(mdl.traitphen)[grep(pattern = "^muStudy", x = names(mdl.traitphen))]
mugrandsp <- names(mdl.traitphen)[grep(pattern = "^mu_grand_sp", x = names(mdl.traitphen))]
betaforcesp <- names(mdl.traitphen)[grep(pattern = "^betaForceSp", x = names(mdl.traitphen))]
betachillsp <- names(mdl.traitphen)[grep(pattern = "^betaChillSp", x = names(mdl.traitphen))]
betaphotosp <- names(mdl.traitphen)[grep(pattern = "^betaPhotoSp", x = names(mdl.traitphen))]
mu_params <-   c("mu_grand",
                 muStudy,
                 mugrandsp,
                 "muForceSp",
                 "muChillSp",
                 "muPhotoSp",
                 "betaTraitxForce",
                 "betaTraitxChill",
                 "betaTraitxPhoto",
                 betaforcesp,
                 betachillsp,
                 betaphotosp)
esti <- sumt[mu_params, col4table]

mugrandtrait<- "Grand trait mean"
# <- paste("mu_Study", studylist, sep = " ")
muGrandSpname <- paste("mu_Grand_Sp", specieslist, sep = " ")
betaForceSpname <- paste("betaForceSp", specieslist, sep = " ")
betaChillSpname <- paste("betaChillSp", specieslist, sep = " ")
betaPhotoSpname <- paste("betaPhotoSp", specieslist, sep = " ")

temp <- c(mugrandtrait, muStudy, muGrandSpname, betaForceSpname)
rownames(esti) =c("Grand trait mean",
                  muStudy,
                  muGrandSpname,
                  "Forcing",
                  "Photoperiod",
                  "Chilling",
                  "Trait x Force effect",
                  "Trait x Chill effect",
                  "Trait x Photoperiod effect",
                  betaForceSpname,
                  betaChillSpname,
                  betaPhotoSpname
)
