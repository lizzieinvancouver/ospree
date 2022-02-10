# Started January 27, 2022

# the purpose of this code is to have source code for building tables for the traitors ms

library(rstan)
require(shinystan)
library(hdrcde) ## better quantiles
library(tidybayes)
# setwd("~/Documents/github/ospree/analyses/traits")
## Set seed
set.seed(202109)

rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/github/ospree/analyses/traits")

specieslist <-  c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

load("output/raw/sla_raw_37spp.Rda")
#get_variables(mdl.traitphen)

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

#temp <- c(mugrandtrait, muStudy, muGrandSpname, betaForceSpname)
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

write.csv(esti.table, "slaMdlOutput.csv", row.names = T)


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


###################################
# calculating the 90% intervals for the slopes
# HPDI95 <- function(x){
#   #Thsi function used teh UPDI functtion from the rethingink package 
#   xhpdi <- HPDI(x, prob = 0.90)
#   return(xhpdi)
# }
files <- list.files(path = "output", pattern ="_37spp.RDS" )
files

htModel <- readRDS(paste("output/", "Height_stanfit_37spp.RDS", sep = ""))
htModelFit <- rstan::extract(htModel)

htBFSpMean <- round(mean(htModelFit$betaForceSp),1)
lower_htBFSpMean <- round(HPDI(data.frame(htModelFit$betaForceSp), prob = 0.90)[1],1)
upper_htBFSpMean <- round(HPDI(data.frame(htModelFit$betaForceSp), prob = 0.90)[2],1)
htBFSpMean; lower_htBFSpMean; upper_htBFSpMean

htBCSpMean <- round(mean(htModelFit$betaChillSp),1)
lower_htBCSpMean <- round(HPDI(data.frame(htModelFit$betaChillSp), prob = 0.90)[1],1)
upper_htBCSpMean <- round(HPDI(data.frame(htModelFit$betaChillSp), prob = 0.90)[2],1)
htBCSpMean; lower_htBCSpMean; upper_htBCSpMean

htBPSpMean <- round(mean(htModelFit$betaPhotoSp),1)
lower_htBPSpMean <- round(HPDI(data.frame(htModelFit$betaPhotoSp), prob = 0.90)[1],1)
upper_htBPSpMean <- round(HPDI(data.frame(htModelFit$betaPhotoSp), prob = 0.90)[2],1)
htBPSpMean; lower_htBPSpMean; upper_htBPSpMean

##### SLA ###############################
slaModel <- readRDS(paste("output/", "SLA_stanfit_37spp.RDS", sep = ""))
slaModelFit <- rstan::extract(slaModel)

slaBFSpMean <- round(mean(slaModelFit$betaForceSp),1)
lower_slaBFSpMean <- round(HPDI(data.frame(slaModelFit$betaForceSp), prob = 0.90)[1],1)
upper_slaBFSpMean <- round(HPDI(data.frame(slaModelFit$betaForceSp), prob = 0.90)[2],1)
slaBFSpMean; lower_slaBFSpMean; upper_slaBFSpMean

slaBCSpMean <- round(mean(slaModelFit$betaChillSp),1)
lower_slaBCSpMean <- round(HPDI(data.frame(slaModelFit$betaChillSp), prob = 0.90)[1],1)
upper_slaBCSpMean <- round(HPDI(data.frame(slaModelFit$betaChillSp), prob = 0.90)[2],1)
slaBCSpMean; lower_slaBCSpMean; upper_slaBCSpMean

slaBPSpMean <- round(mean(slaModelFit$betaPhotoSp),1)
lower_slaBPSpMean <- round(HPDI(data.frame(slaModelFit$betaPhotoSp), prob = 0.90)[1],1)
upper_slaBPSpMean <- round(HPDI(data.frame(slaModelFit$betaPhotoSp), prob = 0.90)[2],1)
slaBPSpMean; lower_slaBPSpMean; upper_slaBPSpMean

##### LNC ###############################
lncModel <- readRDS(paste("output/", "LNC_stanfit_37spp.RDS", sep = ""))
lncModelFit <- rstan::extract(lncModel)

lncBFSpMean <- round(mean(lncModelFit$betaForceSp),1)
lower_lncBFSpMean <- round(HPDI(data.frame(lncModelFit$betaForceSp), prob = 0.90)[1],1)
upper_lncBFSpMean <- round(HPDI(data.frame(lncModelFit$betaForceSp), prob = 0.90)[2],1)
lncBFSpMean; lower_lncBFSpMean; upper_lncBFSpMean

lncBCSpMean <- round(mean(lncModelFit$betaChillSp),1)
lower_lncBCSpMean <- round(HPDI(data.frame(lncModelFit$betaChillSp), prob = 0.90)[1],1)
upper_lncBCSpMean <- round(HPDI(data.frame(lncModelFit$betaChillSp), prob = 0.90)[2],1)
lncBCSpMean; lower_lncBCSpMean; upper_lncBCSpMean

lncBPSpMean <- round(mean(lncModelFit$betaPhotoSp),1)
lower_lncBPSpMean <- round(HPDI(data.frame(lncModelFit$betaPhotoSp), prob = 0.90)[1],1)
upper_lncBPSpMean <- round(HPDI(data.frame(lncModelFit$betaPhotoSp), prob = 0.90)[2],1)
lncBPSpMean; lower_lncBPSpMean; upper_lncBPSpMean

##### seed mass  ###############################
smModel <- readRDS(paste("output/", "SeedMass_log10_stanfit_37spp.RDS", sep = ""))
smModelFit <- rstan::extract(smModel)

smBFSpMean <- round(mean(smModelFit$betaForceSp),1)
lower_smBFSpMean <- round(HPDI(data.frame(smModelFit$betaForceSp), prob = 0.90)[1],1)
upper_smBFSpMean <- round(HPDI(data.frame(smModelFit$betaForceSp), prob = 0.90)[2],1)
smBFSpMean; lower_smBFSpMean; upper_smBFSpMean

smBCSpMean <- round(mean(smModelFit$betaChillSp),1)
lower_smBCSpMean <- round(HPDI(data.frame(smModelFit$betaChillSp), prob = 0.90)[1],1)
upper_smBCSpMean <- round(HPDI(data.frame(smModelFit$betaChillSp), prob = 0.90)[2],1)
smBCSpMean; lower_smBCSpMean; upper_smBCSpMean

smBPSpMean <- round(mean(smModelFit$betaPhotoSp),1)
lower_smBPSpMean <- round(HPDI(data.frame(smModelFit$betaPhotoSp), prob = 0.90)[1],1)
upper_smBPSpMean <- round(HPDI(data.frame(smModelFit$betaPhotoSp), prob = 0.90)[2],1)
smBPSpMean; lower_smBPSpMean; upper_smBPSpMean
