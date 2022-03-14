# Started January 27, 2022

# the purpose of this code is to have source code for building tables for the traitors ms
# rm(list=ls()) 
# options(stringsAsFactors = FALSE)

library(rstan)
require(shinystan)
library(hdrcde) ## better quantiles
library(tidybayes)
library(rethinking)
# setwd("~/Documents/github/ospree/analyses/traits")
## Set seed
set.seed(202109)

#setwd("~/Documents/github/ospree/analyses/traits")

specieslist <-  c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

###################################
# calculating the 90% intervals for the slopes
# HPDI95 <- function(x){
#   #Thsi function used teh UPDI functtion from the rethingink package 
#   xhpdi <- HPDI(x, prob = 0.90)
#   return(xhpdi)
# }
files <- list.files(path = "output", pattern ="_37spp_wp.RDS" )
files

htModel <- readRDS(paste("../../analyses/traits/output/", "height_stanfit_37spp_wp.RDS", sep = ""))
htModelFit <- rstan::extract(htModel)

htBFSpMean <- as.numeric(round(mean(htModelFit$betaForceSp),1))
lower_htBFSpMean <- as.numeric(round(HPDI(data.frame(htModelFit$betaForceSp), prob = 0.90)[1],1))
upper_htBFSpMean <- as.numeric(round(HPDI(data.frame(htModelFit$betaForceSp), prob = 0.90)[2],1))
htBFSpMean; lower_htBFSpMean; upper_htBFSpMean

htBCSpMean <- as.numeric(round(mean(htModelFit$betaChillSp),1))
lower_htBCSpMean <- as.numeric(round(HPDI(data.frame(htModelFit$betaChillSp), prob = 0.90)[1],1))
upper_htBCSpMean <- as.numeric(round(HPDI(data.frame(htModelFit$betaChillSp), prob = 0.90)[2],1))
htBCSpMean; lower_htBCSpMean; upper_htBCSpMean

htBPSpMean <- as.numeric(round(mean(htModelFit$betaPhotoSp),1))
lower_htBPSpMean <- as.numeric(round(HPDI(data.frame(htModelFit$betaPhotoSp), prob = 0.90)[1],1))
upper_htBPSpMean <- as.numeric(round(HPDI(data.frame(htModelFit$betaPhotoSp), prob = 0.90)[2],1))
htBPSpMean; lower_htBPSpMean; upper_htBPSpMean

##### SLA ###############################
slaModel <- readRDS(paste("../../analyses/traits/output/", "SLA_stanfit_37spp_wp.RDS", sep = ""))
slaModelFit <- rstan::extract(slaModel)

slaBFSpMean <- as.numeric(round(mean(slaModelFit$betaForceSp),1))
lower_slaBFSpMean <- as.numeric(round(HPDI(data.frame(slaModelFit$betaForceSp), prob = 0.90)[1],1))
upper_slaBFSpMean <- as.numeric(round(HPDI(data.frame(slaModelFit$betaForceSp), prob = 0.90)[2],1))
slaBFSpMean; lower_slaBFSpMean; upper_slaBFSpMean

slaBCSpMean <- as.numeric(round(mean(slaModelFit$betaChillSp),1))
lower_slaBCSpMean <- as.numeric(round(HPDI(data.frame(slaModelFit$betaChillSp), prob = 0.90)[1],1))
upper_slaBCSpMean <- as.numeric(round(HPDI(data.frame(slaModelFit$betaChillSp), prob = 0.90)[2],1))
slaBCSpMean; lower_slaBCSpMean; upper_slaBCSpMean

slaBPSpMean <- as.numeric(round(mean(slaModelFit$betaPhotoSp),1))
lower_slaBPSpMean <- as.numeric(round(HPDI(data.frame(slaModelFit$betaPhotoSp), prob = 0.90)[1],1))
upper_slaBPSpMean <- as.numeric(round(HPDI(data.frame(slaModelFit$betaPhotoSp), prob = 0.90)[2],1))
slaBPSpMean; lower_slaBPSpMean; upper_slaBPSpMean

##### LNC ###############################
lncModel <- readRDS(paste("../../analyses/traits/output/", "LNC_stanfit_37spp_wp.RDS", sep = ""))
lncModelFit <- rstan::extract(lncModel)

lncBFSpMean <- as.numeric(round(mean(lncModelFit$betaForceSp),1))
lower_lncBFSpMean <- as.numeric(round(HPDI(data.frame(lncModelFit$betaForceSp), prob = 0.90)[1],1))
upper_lncBFSpMean <- as.numeric(round(HPDI(data.frame(lncModelFit$betaForceSp), prob = 0.90)[2],1))
lncBFSpMean; lower_lncBFSpMean; upper_lncBFSpMean

lncBCSpMean <- as.numeric(round(mean(lncModelFit$betaChillSp),1))
lower_lncBCSpMean <- as.numeric(round(HPDI(data.frame(lncModelFit$betaChillSp), prob = 0.90)[1],1))
upper_lncBCSpMean <- as.numeric(round(HPDI(data.frame(lncModelFit$betaChillSp), prob = 0.90)[2],1))
lncBCSpMean; lower_lncBCSpMean; upper_lncBCSpMean

lncBPSpMean <- as.numeric(round(mean(lncModelFit$betaPhotoSp),1))
lower_lncBPSpMean <- as.numeric(round(HPDI(data.frame(lncModelFit$betaPhotoSp), prob = 0.90)[1],1))
upper_lncBPSpMean <- as.numeric(round(HPDI(data.frame(lncModelFit$betaPhotoSp), prob = 0.90)[2],1))
lncBPSpMean; lower_lncBPSpMean; upper_lncBPSpMean

##### seed mass  ###############################
smModel <- readRDS(paste("../../analyses/traits/output/", "SeedMass_log10_stanfit_37spp_wp.RDS", sep = ""))
smModelFit <- rstan::extract(smModel)

smBFSpMean <- as.numeric(round(mean(smModelFit$betaForceSp),1))
lower_smBFSpMean <- as.numeric(round(HPDI(data.frame(smModelFit$betaForceSp), prob = 0.90)[1],1))
upper_smBFSpMean <- as.numeric(round(HPDI(data.frame(smModelFit$betaForceSp), prob = 0.90)[2],1))
smBFSpMean; lower_smBFSpMean; upper_smBFSpMean

smBCSpMean <- as.numeric(round(mean(smModelFit$betaChillSp),1))
lower_smBCSpMean <- as.numeric(round(HPDI(data.frame(smModelFit$betaChillSp), prob = 0.90)[1],1))
upper_smBCSpMean <- as.numeric(round(HPDI(data.frame(smModelFit$betaChillSp), prob = 0.90)[2],1))
smBCSpMean; lower_smBCSpMean; upper_smBCSpMean

smBPSpMean <- as.numeric(round(mean(smModelFit$betaPhotoSp),1))
lower_smBPSpMean <- as.numeric(round(HPDI(data.frame(smModelFit$betaPhotoSp), prob = 0.90)[1],1))
upper_smBPSpMean <- as.numeric(round(HPDI(data.frame(smModelFit$betaPhotoSp), prob = 0.90)[2],1))
smBPSpMean; lower_smBPSpMean; upper_smBPSpMean

##############################################################

htMean <- as.numeric(round(mean(htModelFit$mu_grand),1))
lower_htMean <- as.numeric(round(HPDI(data.frame(htModelFit$mu_grand), prob = 0.90)[1],1))
upper_htMean <- as.numeric(round(HPDI(data.frame(htModelFit$mu_grand), prob = 0.90)[2],1))

htSD <- as.numeric(round(mean(htModelFit$sigma_sp),1))
lower_htSD <- as.numeric(round(HPDI(data.frame(htModelFit$sigma_sp), prob = 0.90)[1],1))
upper_htSD <- as.numeric(round(HPDI(data.frame(htModelFit$sigma_sp), prob = 0.90)[2],1))

htStudySD <- as.numeric(round(mean(htModelFit$sigma_study),1))
lower_htStudySD <- as.numeric(round(HPDI(data.frame(htModelFit$sigma_study), prob = 0.90)[1],1))
upper_htStudySD <- as.numeric(round(HPDI(data.frame(htModelFit$sigma_study), prob = 0.90)[2],1))

htMax <- as.numeric(round(max(htModelFit$mu_grand),1))
lower_htMax <- as.numeric(round(HPDI(data.frame(htModelFit$mu_grand), prob = 0.90)[1],1))
upper_htMax <- as.numeric(round(HPDI(data.frame(htModelFit$mu_grand), prob = 0.90)[2],1))

htMin <- as.numeric(round(min(htModelFit$mu_grand),1))
lower_htMin <- as.numeric(round(HPDI(data.frame(htModelFit$mu_grand), prob = 0.90)[1],1))
upper_htMin <- as.numeric(round(HPDI(data.frame(htModelFit$mu_grand), prob = 0.90)[2],1))

############################################################
#sla
slaMean <- as.numeric(round(mean(slaModelFit$mu_grand),1))
lower_slaMean <- as.numeric(round(HPDI(data.frame(slaModelFit$mu_grand), prob = 0.90)[1],1))
upper_slaMean <- as.numeric(round(HPDI(data.frame(slaModelFit$mu_grand), prob = 0.90)[2],1))

slaSD <- as.numeric(round(mean(slaModelFit$sigma_sp),1))
lower_slaSD <- as.numeric(round(HPDI(data.frame(slaModelFit$sigma_sp), prob = 0.90)[1],1))
upper_slaSD <- as.numeric(round(HPDI(data.frame(slaModelFit$sigma_sp), prob = 0.90)[2],1))

slaStudySD <- as.numeric(round(mean(slaModelFit$sigma_study),1))
lower_slaStudySD <- as.numeric(round(HPDI(data.frame(slaModelFit$sigma_study), prob = 0.90)[1],1))
upper_slaStudySD <- as.numeric(round(HPDI(data.frame(slaModelFit$sigma_study), prob = 0.90)[2],1))

slaMax <- as.numeric(round(max(slaModelFit$mu_grand),1))
lower_slaMax <- as.numeric(round(HPDI(data.frame(slaModelFit$mu_grand), prob = 0.90)[1],1))
upper_slaMax <- as.numeric(round(HPDI(data.frame(slaModelFit$mu_grand), prob = 0.90)[2],1))

slaMin <- as.numeric(round(min(slaModelFit$mu_grand),1))
lower_slaMin <- as.numeric(round(HPDI(data.frame(slaModelFit$mu_grand), prob = 0.90)[1],1))
upper_slaMin <- as.numeric(round(HPDI(data.frame(slaModelFit$mu_grand), prob = 0.90)[2],1))

##############################################################

lncMean <- as.numeric(round(mean(lncModelFit$mu_grand),1))
lower_lncMean <- as.numeric(round(HPDI(data.frame(lncModelFit$mu_grand), prob = 0.90)[1],1))
upper_lncMean <- as.numeric(round(HPDI(data.frame(lncModelFit$mu_grand), prob = 0.90)[2],1))

lncSD <- as.numeric(round(mean(lncModelFit$sigma_sp),1))
lower_lncSD <- as.numeric(round(HPDI(data.frame(lncModelFit$sigma_sp), prob = 0.90)[1],1))
upper_lncSD <- as.numeric(round(HPDI(data.frame(lncModelFit$sigma_sp), prob = 0.90)[2],1))

lncStudySD <- as.numeric(round(mean(lncModelFit$sigma_study),1))
lower_lncStudySD <- as.numeric(round(HPDI(data.frame(lncModelFit$sigma_study), prob = 0.90)[1],1))
upper_lncStudySD <- as.numeric(round(HPDI(data.frame(lncModelFit$sigma_study), prob = 0.90)[2],1))

lncMax <- as.numeric(round(max(lncModelFit$mu_grand),1))
lower_lncMax <- as.numeric(round(HPDI(data.frame(lncModelFit$mu_grand), prob = 0.90)[1],1))
upper_lncMax <- as.numeric(round(HPDI(data.frame(lncModelFit$mu_grand), prob = 0.90)[2],1))

lncMin <- as.numeric(round(min(lncModelFit$mu_grand),1))
lower_lncMin <- as.numeric(round(HPDI(data.frame(lncModelFit$mu_grand), prob = 0.90)[1],1))
upper_lncMin <- as.numeric(round(HPDI(data.frame(lncModelFit$mu_grand), prob = 0.90)[2],1))

##############################################################

smMean <- as.numeric(round(mean(smModelFit$mu_grand),1))
lower_smMean <- as.numeric(round(HPDI(data.frame(smModelFit$mu_grand), prob = 0.90)[1],1))
upper_smMean <- as.numeric(round(HPDI(data.frame(smModelFit$mu_grand), prob = 0.90)[2],1))

smSD <- as.numeric(round(mean(smModelFit$sigma_sp),1))
lower_smSD <- as.numeric(round(HPDI(data.frame(smModelFit$sigma_sp), prob = 0.90)[1],1))
upper_smSD <- as.numeric(round(HPDI(data.frame(smModelFit$sigma_sp), prob = 0.90)[2],1))

smStudySD <- as.numeric(round(mean(smModelFit$sigma_study),1))
lower_smStudySD <- as.numeric(round(HPDI(data.frame(smModelFit$sigma_study), prob = 0.90)[1],1))
upper_smStudySD <- as.numeric(round(HPDI(data.frame(smModelFit$sigma_study), prob = 0.90)[2],1))

smMax <- as.numeric(round(max(smModelFit$mu_grand),1))
lower_smMax <- as.numeric(round(HPDI(data.frame(smModelFit$mu_grand), prob = 0.90)[1],1))
upper_smMax <- as.numeric(round(HPDI(data.frame(smModelFit$mu_grand), prob = 0.90)[2],1))

smMin <- as.numeric(round(min(smModelFit$mu_grand),1))
lower_smMin <- as.numeric(round(HPDI(data.frame(smModelFit$mu_grand), prob = 0.90)[1],1))
upper_smMin <- as.numeric(round(HPDI(data.frame(smModelFit$mu_grand), prob = 0.90)[2],1))

# load("output/height_raw_37spp_wp.Rda")
# #get_variables(mdl.traitphen)
# 
# sumt <- summary(mdl.traitphen)$summary
# 
# col4table <- c("mean","sd","2.5%","50%","97.5%","Rhat")
# 
# mu_params <-   c("mu_grand",
#                  "muPhenoSp",
#                  "muForceSp",
#                  "muChillSp",
#                  "muPhotoSp",
#                  "betaTraitxForce",
#                  "betaTraitxChill",
#                  "betaTraitxPhoto")
# esti <- sumt[mu_params, col4table]
# 
# #temp <- c(mugrandtrait, muStudy, muGrandSpname, betaForceSpname)
# rownames(esti) =c("Grand trait mean",
#                   "Grand Species mean",
#                      "Beta Forcing",
#                      "Beta Chilling",
#                      "Beta Photoperiod",
#                      "Beta Trait x Forcing",
#                      "Beta Trait x Chilling",
#                      "Beta Trait x Photoperiod")
# 
# esti.table <- sumt[mu_params, col4table]
# row.names(esti.table) <- row.names(esti)
# 
# write.csv(esti.table, "heightMdlOutput.csv", row.names = T)
# 
# 
# # Long tables:
# muStudy <- names(mdl.traitphen)[grep(pattern = "^muStudy", x = names(mdl.traitphen))]
# mugrandsp <- names(mdl.traitphen)[grep(pattern = "^mu_grand_sp", x = names(mdl.traitphen))]
# betaforcesp <- names(mdl.traitphen)[grep(pattern = "^betaForceSp", x = names(mdl.traitphen))]
# betachillsp <- names(mdl.traitphen)[grep(pattern = "^betaChillSp", x = names(mdl.traitphen))]
# betaphotosp <- names(mdl.traitphen)[grep(pattern = "^betaPhotoSp", x = names(mdl.traitphen))]
# mu_params <-   c("mu_grand",
#                  muStudy,
#                  mugrandsp,
#                  "muForceSp",
#                  "muChillSp",
#                  "muPhotoSp",
#                  "betaTraitxForce",
#                  "betaTraitxChill",
#                  "betaTraitxPhoto",
#                  betaforcesp,
#                  betachillsp,
#                  betaphotosp)
# esti <- sumt[mu_params, col4table]
# 
# mugrandtrait<- "Grand trait mean"
# # <- paste("mu_Study", studylist, sep = " ")
# muGrandSpname <- paste("mu_Grand_Sp", specieslist, sep = " ")
# betaForceSpname <- paste("betaForceSp", specieslist, sep = " ")
# betaChillSpname <- paste("betaChillSp", specieslist, sep = " ")
# betaPhotoSpname <- paste("betaPhotoSp", specieslist, sep = " ")
# 
# temp <- c(mugrandtrait, muStudy, muGrandSpname, betaForceSpname)
# rownames(esti) =c("Grand trait mean",
#                   muStudy,
#                   muGrandSpname,
#                   "Forcing",
#                   "Photoperiod",
#                   "Chilling",
#                   "Trait x Force effect",
#                   "Trait x Chill effect",
#                   "Trait x Photoperiod effect",
#                   betaForceSpname,
#                   betaChillSpname,
#                   betaPhotoSpname
# )


