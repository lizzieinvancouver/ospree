# Started January 27, 2022

# the purpose of this code is to have source code for building tables for the traitors ms
# rm(list=ls())
# options(stringsAsFactors = FALSE)

library(rstan)
library(hdrcde) ## better quantiles
# library(tidybayes)
library(reshape2)
# setwd("~/Documents/github/ospree/analyses/traits")
## Set seed
set.seed(202109)

#setwd("~/Documents/github/ospree/analyses/traits")

specieslist <-  c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

###################################
# calculating the 90% intervals for the slopes
# HPDI95 <- function(x){
#   #Thsi function used teh UPDI functtion from the rethingink package 
#   xhpdi <- HPDI(x, prob = 0.95)
#   return(xhpdi)
# }
# files <- list.files(path = "output", pattern ="_37spp.RDS" )
# files

htModel <- readRDS(paste("../../analyses/traits/output/", "height_stanfit_37spp_Aug252025.RDS", sep = ""))
htModelFit <- rstan::extract(htModel)

htBFSpMean <- as.numeric(round(mean(htModelFit$betaTraitxForce),1))
lower_htBFSpMean <- as.numeric(round(quantile(htModelFit$betaTraitxForce, prob = 0.05),1))
upper_htBFSpMean <- as.numeric(round(quantile(htModelFit$betaTraitxForce, prob = 0.95),1))
htBFSpMean; lower_htBFSpMean; upper_htBFSpMean

htBCSpMean <- round(as.numeric(mean(htModelFit$betaTraitxChill)),1)
lower_htBCSpMean <- format(as.numeric(round(quantile(htModelFit$betaTraitxChill, prob = 0.05),1)), nsmall = 1)
upper_htBCSpMean <- as.numeric(round(quantile(htModelFit$betaTraitxChill, prob = 0.95),1))
htBCSpMean; lower_htBCSpMean; upper_htBCSpMean

htBPSpMean <- round(as.numeric(mean(htModelFit$betaTraitxPhoto)),1)
lower_htBPSpMean <- as.numeric(round(quantile(htModelFit$betaTraitxPhoto, prob = 0.05),1))
upper_htBPSpMean <- format(as.numeric(round(quantile(htModelFit$betaTraitxPhoto, prob = 0.95),1)), nsmall = 1)
htBPSpMean; lower_htBPSpMean; upper_htBPSpMean

htchill <- data.frame(htModelFit$alphaChillSp)
htChillSpMean <- colMeans(htchill)
htChillMax <- max(htChillSpMean)
# species with the largest chill response - 7 and 29
# "Betula_papyrifera", "Quercus_ilex" 

#muSp <- apply(posterior_sm$muSp, MARGIN = 2, FUN = mean)

#species with the smallest chill response - "Betula_populifolia" 
##### SLA ###############################
slaModel <- readRDS(paste("../../analyses/traits/output/", "SLA_stanfit_37spp.RDS", sep = ""))
slaModelFit <- rstan::extract(slaModel)

slaBFSpMean <- as.numeric(round(mean(slaModelFit$betaTraitxForce),1))
# round(quantile(slaModelFit$betaTraitxForce, prob = 0.05),1)
# round(quantile(slaModelFit$betaTraitxForce, prob = 0.95),1)
lower_slaBFSpMean <- as.numeric(round(quantile(slaModelFit$betaTraitxForce, prob = 0.05),1))
upper_slaBFSpMean <- as.numeric(round(quantile(slaModelFit$betaTraitxForce, prob = 0.95),1))
slaBFSpMean; lower_slaBFSpMean; upper_slaBFSpMean

slaBCSpMean <- as.numeric(round(mean(slaModelFit$betaTraitxChill),1))
lower_slaBCSpMean <- as.numeric(round(quantile(slaModelFit$betaTraitxChill, prob = 0.05),1))
upper_slaBCSpMean <- as.numeric(round(quantile(slaModelFit$betaTraitxChill, prob = 0.95),1))
slaBCSpMean; lower_slaBCSpMean; upper_slaBCSpMean

slaBPSpMean <- as.numeric(round(mean(slaModelFit$betaTraitxPhoto),1))
lower_slaBPSpMean <- as.numeric(round(quantile(slaModelFit$betaTraitxPhoto, prob = 0.05),1))
upper_slaBPSpMean <- format(round(quantile(slaModelFit$betaTraitxPhoto, prob = 0.95),1), nsmall =1)
slaBPSpMean; lower_slaBPSpMean; upper_slaBPSpMean

##### LNC ###############################
lncModel <- readRDS(paste("../../analyses/traits/output/", "LNC_stanfit_37spp.RDS", sep = ""))
lncModelFit <- rstan::extract(lncModel)

lncBFSpMean <- as.numeric(round(mean(lncModelFit$betaTraitxForce),1))
lower_lncBFSpMean <- format(as.numeric(round(quantile(lncModelFit$betaTraitxForce, prob = 0.05),1)), nsmall =1)
upper_lncBFSpMean <- as.numeric(round(quantile(lncModelFit$betaTraitxForce, prob = 0.95),1))
lncBFSpMean; lower_lncBFSpMean; upper_lncBFSpMean

lncBCSpMean <- as.numeric(round(mean(lncModelFit$betaTraitxChill),1))
lower_lncBCSpMean <- as.numeric(round(quantile(lncModelFit$betaTraitxChill, prob = 0.05),1))
upper_lncBCSpMean <- as.numeric(round(quantile(lncModelFit$betaTraitxChill, prob = 0.95),1))
lncBCSpMean; lower_lncBCSpMean; upper_lncBCSpMean

lncBPSpMean <- as.numeric(round(mean(lncModelFit$betaTraitxPhoto),1))
lower_lncBPSpMean <- format(as.numeric(round(quantile(lncModelFit$betaTraitxPhoto, prob = 0.05),1)), nsmall =1)
upper_lncBPSpMean <- as.numeric(round(quantile(lncModelFit$betaTraitxPhoto, prob = 0.95),1))
lncBPSpMean; lower_lncBPSpMean; upper_lncBPSpMean

#lncmuSp <- apply(posterior_sm$muSp, MARGIN = 2, FUN = mean)


##### seed mass  ###############################
smModel <- readRDS(paste("../../analyses/traits/output/", "SeedMass_log10_stanfit_37spp.RDS", sep = ""))
smModelFit <- rstan::extract(smModel)

smBFSpMean <- as.numeric(round(mean(smModelFit$betaTraitxForce),1))
lower_smBFSpMean <- as.numeric(round(quantile(smModelFit$betaTraitxForce, prob = 0.05),1))
upper_smBFSpMean <- as.numeric(round(quantile(smModelFit$betaTraitxForce, prob = 0.95),1))
smBFSpMean; lower_smBFSpMean; upper_smBFSpMean

smBCSpMean <- as.numeric(round(mean(smModelFit$betaTraitxChill),1))
lower_smBCSpMean <- as.numeric(round(quantile(smModelFit$betaTraitxChill, prob = 0.05),1))
upper_smBCSpMean <- as.numeric(round(quantile(smModelFit$betaTraitxChill, prob = 0.95),1))
smBCSpMean; lower_smBCSpMean; upper_smBCSpMean

smBPSpMean <- as.numeric(round(mean(smModelFit$betaTraitxPhoto),1))
lower_smBPSpMean <- as.numeric(round(quantile(smModelFit$betaTraitxPhoto, prob = 0.05),1))
upper_smBPSpMean <- as.numeric(round(quantile(smModelFit$betaTraitxPhoto, prob = 0.95),1))
smBPSpMean; lower_smBPSpMean; upper_smBPSpMean


smchill <- data.frame(smModelFit$alphaChillSp)
smChillSpMean <- colMeans(smchill)
smChillMax <- max(smChillSpMean)
# species with the largest chill response - 7 and 29
# "Betula_papyrifera", "Quercus_ilex" 

#species with the smallest chill response - "Betula_populifolia" 
##############################################################
# Get the values that Faith added to the results:


mu_grandDf <- data.frame(htModelFit$mu_grand_sp)
colnames(mu_grandDf) <- specieslist
longMeans <- melt(mu_grandDf)
colnames(longMeans) <- c("speciesname", "traitMean")

speciesMeans <-  aggregate(longMeans$traitMean, by = list(longMeans$speciesname), FUN = mean)
names(speciesMeans) <- c("speciesname", "traitMean")

htMean <- as.numeric(round(mean(htModelFit$mu_grand),1))
lower_htMean <- as.numeric(round(quantile(htModelFit$mu_grand, prob = 0.05),1))
upper_htMean <- as.numeric(round(quantile(htModelFit$mu_grand, prob = 0.95),1))

htSD <- as.numeric(round(mean(htModelFit$sigma_sp),1))
lower_htSD <- as.numeric(round(quantile(htModelFit$sigma_sp, prob = 0.05),1))
upper_htSD <- as.numeric(round(quantile(htModelFit$sigma_sp, prob = 0.95),1))

htStudySD <- as.numeric(round(mean(htModelFit$sigma_study),1))
lower_htStudySD <- as.numeric(round(quantile(htModelFit$sigma_study, prob = 0.05),1))
upper_htStudySD <- as.numeric(round(quantile(htModelFit$sigma_study, prob = 0.95),1))

#Max trait value
htMax <- as.numeric(round(max(speciesMeans$traitMean)))
#Uncertainty around max species
lower_htMax<- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Quercus_petraea"] , prob=0.05 ),1))
upper_htMax <- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Quercus_petraea"] , prob=0.95 ),1))

htMin <- as.numeric(round(min(speciesMeans$traitMean)))
#Uncertainty around max species
lower_htMin<- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Rhamnus_cathartica"] , prob=0.05 ),1))
upper_htMin <- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Rhamnus_cathartica"] , prob=0.95 ),1))


############################################################
#sla
mu_grandDf <- data.frame(slaModelFit$mu_grand_sp)
colnames(mu_grandDf) <- specieslist
longMeans <- melt(mu_grandDf)
colnames(longMeans) <- c("speciesname", "traitMean")

speciesMeans <-  aggregate(longMeans$traitMean, by = list(longMeans$speciesname), FUN = mean)
names(speciesMeans) <- c("speciesname", "traitMean")


slaMean <- as.numeric(round(mean(slaModelFit$mu_grand),1))
lower_slaMean <- as.numeric(round(quantile(slaModelFit$mu_grand, prob = 0.05),1))
upper_slaMean <- as.numeric(round(quantile(slaModelFit$mu_grand, prob = 0.95),1))

slaSD <- as.numeric(round(mean(slaModelFit$sigma_sp),1))
lower_slaSD <- as.numeric(round(quantile(slaModelFit$sigma_sp, prob = 0.05),1))
upper_slaSD <- as.numeric(round(quantile(slaModelFit$sigma_sp, prob = 0.95),1))

slaStudySD <- as.numeric(round(mean(slaModelFit$sigma_study),1))
lower_slaStudySD <- as.numeric(round(quantile(slaModelFit$sigma_study, prob = 0.05),1))
upper_slaStudySD <- as.numeric(round(quantile(slaModelFit$sigma_study, prob = 0.95),1))


slaMax <- as.numeric(round(max(speciesMeans$traitMean))); speciesMeans
#Uncertainty around max species
lower_slaMax<- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Acer_pensylvanicum"] , prob=0.05 ),1))
upper_slaMax <- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Acer_pensylvanicum"] , prob=0.95 ),1))

slaMin <- as.numeric(round(min(speciesMeans$traitMean))); speciesMeans
#Uncertainty around max species
lower_slaMin<- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Quercus_coccifera"] , prob=0.05 ),1))
upper_slaMin <- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Quercus_coccifera"] , prob=0.95 ),1))

##############################################################
mu_grandDf <- data.frame(lncModelFit$mu_grand_sp)
colnames(mu_grandDf) <- specieslist
longMeans <- melt(mu_grandDf)
colnames(longMeans) <- c("speciesname", "traitMean")

speciesMeans <-  aggregate(longMeans$traitMean, by = list(longMeans$speciesname), FUN = mean)
names(speciesMeans) <- c("speciesname", "traitMean")

lncMean <- as.numeric(round(mean(lncModelFit$mu_grand),1))
lower_lncMean <- as.numeric(round(quantile(lncModelFit$mu_grand, prob = 0.05),1))
upper_lncMean <- as.numeric(round(quantile(lncModelFit$mu_grand, prob = 0.95),1))

lncSD <- as.numeric(round(mean(lncModelFit$sigma_sp),1))
lower_lncSD <- as.numeric(round(quantile(lncModelFit$sigma_sp, prob = 0.05),1))
upper_lncSD <- as.numeric(round(quantile(lncModelFit$sigma_sp, prob = 0.95),1))

lncStudySD <- as.numeric(round(mean(lncModelFit$sigma_study),1))
lower_lncStudySD <- as.numeric(round(quantile(lncModelFit$sigma_study, prob = 0.05),1))
upper_lncStudySD <- as.numeric(round(quantile(lncModelFit$sigma_study, prob = 0.95),1))


lncMax <- as.numeric(round(max(speciesMeans$traitMean))); speciesMeans
#Uncertainty around max species
lower_lncMax<- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Prunus_persica"] , prob=0.05 ),1))
upper_lncMax <- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Prunus_persica"] , prob=0.95 ),1))

lncMin <-  as.numeric(round(min(speciesMeans$traitMean))); speciesMeans
#Uncertainty around max species
lower_lncMin<- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Hamamelis_virginiana"] , prob=0.05 ),1))
upper_lncMin <- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Hamamelis_virginiana"] , prob=0.95 ),1))

##############################################################
mu_grandDf <- data.frame(smModelFit$mu_grand_sp)
colnames(mu_grandDf) <- specieslist
longMeans <- melt(mu_grandDf)
colnames(longMeans) <- c("speciesname", "traitMean")

speciesMeans <-  aggregate(longMeans$traitMean, by = list(longMeans$speciesname), FUN = mean)
names(speciesMeans) <- c("speciesname", "traitMean")

smMean <- as.numeric(round(mean(smModelFit$mu_grand),1))
lower_smMean <- as.numeric(round(quantile(smModelFit$mu_grand, prob = 0.05),1))
upper_smMean <- as.numeric(round(quantile(smModelFit$mu_grand, prob = 0.95),1))

smSD <- as.numeric(round(mean(smModelFit$sigma_sp),1))
lower_smSD <- as.numeric(round(quantile(smModelFit$sigma_sp, prob = 0.05),1))
upper_smSD <- as.numeric(round(quantile(smModelFit$sigma_sp, prob = 0.95),1))

smStudySD <- as.numeric(round(mean(smModelFit$sigma_study),1))
lower_smStudySD <- as.numeric(round(quantile(smModelFit$sigma_study, prob = 0.05),1))
upper_smStudySD <- as.numeric(round(quantile(smModelFit$sigma_study, prob = 0.95),1))

smMax <- as.numeric(round(max(speciesMeans$traitMean))); speciesMeans
#Uncertainty around max species
lower_smMax<- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Juglans_cinerea"] , prob=0.05 ),1))
upper_smMax <- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Juglans_cinerea"] , prob=0.95 ),1))

smMin <- as.numeric(round(min(speciesMeans$traitMean))); speciesMeans
#Uncertainty around max species
lower_smMin<- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Betula_populifolia"] , prob=0.05 ),1))
upper_smMin <- as.numeric(round(quantile(longMeans$traitMean[longMeans$speciesname == "Betula_populifolia"] , prob=0.95 ),1))

###########################################################
load("../../analyses/traits/output/height_raw_37spp.Rda")
htSum <- summary(mdl.traitphen)$summary 

htSigmaSp <- as.numeric(round((htSum[grep("sigma_sp", rownames(htSum)), "mean"]),1))
lower_htSigmaSp<- round(quantile(htModelFit$sigma_sp , prob=0.05 ),1)
upper_htSigmaSp <- round(quantile(htModelFit$sigma_sp , prob=0.95 ),1)

htSigmaStudy <- as.numeric(round((htSum[grep("sigma_study", rownames(htSum)), "mean"]),1))
lower_htSigmaStudy<- round(quantile(htModelFit$sigma_study , prob=0.05 ),1)
upper_htSigmaStudy <- round(quantile(htModelFit$sigma_study , prob=0.95 ),1)

#SLA
load("../../analyses/traits/output/sla_raw_37spp.Rda")
slaSum <- summary(mdl.traitphen)$summary 

slaSigmaSp <- as.numeric(round((slaSum[grep("sigma_sp", rownames(slaSum)), "mean"]),1))
lower_slaSigmaSp<- round(quantile(slaModelFit$sigma_sp , prob=0.05 ),1)
upper_slaSigmaSp <- round(quantile(slaModelFit$sigma_sp , prob=0.95 ),1)


slaSigmaStudy <- as.numeric(round((slaSum[grep("sigma_study", rownames(slaSum)), "mean"]),1))
lower_slaSigmaStudy<- round(quantile(slaModelFit$sigma_study , prob=0.05 ),1)
upper_slaSigmaStudy <- round(quantile(slaModelFit$sigma_study , prob=0.95 ),1)

#seed
load("../../analyses/traits/output/seedmasslog10_raw_37spp.Rda")
seedSum <- summary(mdl.traitphen)$summary 

seedSigmaSp <- as.numeric(round((seedSum[grep("sigma_sp", rownames(seedSum)), "mean"]),1))
lower_smSigmaSp <- round(quantile(smModelFit$sigma_study , prob=0.05 ),1)
upper_smSigmaSp <- round(quantile(smModelFit$sigma_study , prob=0.95 ),1)

seedSigmaStudy <- as.numeric(round((seedSum[grep("sigma_study", rownames(seedSum)), "mean"]),1))
lower_smSigmaStudy<- round(quantile(smModelFit$sigma_study , prob=0.05 ),1)
upper_smSigmaStudy <- round(quantile(smModelFit$sigma_study , prob=0.95 ),1)


#LNC
load("../../analyses/traits/output/lnc_raw_37spp.Rda")
lncSum <- summary(mdl.traitphen)$summary 

lncSigmaSp <- as.numeric(round((lncSum[grep("sigma_sp", rownames(lncSum)), "mean"]),1))
lower_lncSigmaSp <- round(quantile(lncModelFit$sigma_study , prob=0.05 ),1)
upper_lncSigmaSp <- round(quantile(lncModelFit$sigma_study , prob=0.95 ),1)

lncSigmaStudy <- as.numeric(round((lncSum[grep("sigma_study", rownames(lncSum)), "mean"]),1))
lower_lncSigmaStudy<- round(quantile(lncModelFit$sigma_study , prob=0.05 ),1)
upper_lncSigmaStudy <- round(quantile(lncModelFit$sigma_study , prob=0.95 ),1)

# Values for methods: #####################################
# PCA:

pca1 <- 32.0
pca2 <- 24.2

#Values for the methods:
traitsData1 <- read.csv("../../analyses/traits/input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
traitsData2 <- read.csv("../../analyses/traits/input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# SLA trait only
nStudy <- length(unique(traitsData$reference))

slaData <- traitsData[traitsData$traitname == "Specific_leaf_area",]
nSLA <- nrow(slaData)

htData <- traitsData[traitsData$traitname == "Plant_height_vegetative",]
nHt <- nrow(htData)

lncData <- traitsData[traitsData$traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass",]
nLNC <- nrow(lncData)

smData <- traitsData[traitsData$traitname == "seed mass",]
nSM <- nrow(smData)

#27318)
heightData <- read.csv("../../analyses/traits/input/height_37spp_subsampled.csv")
nHtSub <- nrow(heightData)

nHt - nHtSub

# How much more were the six spp sampled compared to others?
htData$count <-1
htSp <- aggregate(htData["count"], htData[c("speciesname")], FUN = sum)

few <- subset(htSp, count < 1500)
meanFew <- mean(few$count)

many <- subset(htSp, count > 1500)
meanMany <- mean(many$count)

(meanFew/meanMany) * 100

htMany <- round((meanMany/meanFew),0)


# Make table of references:
smTab37 <- read.csv("../../analyses/traits/input/sm_table_37spp.csv")

# tried sourcing code and extracting numbers, but always got the error message:
#cannot rescale a constant/zero column to unit variance.

# source("../../analyses/traits/rfiles/traitors_PCA.R")
# 
# trtGeoPca <- stats::prcomp(mat2, center = TRUE, scale. = TRUE)
# explVar <- trtGeoPca$sdev^2 / sum(trtGeoPca$sdev^2)
# 
# pca1 <- format(round(explVar[1] *100, 1), nsmall =1)
# pca2 <- round(explVar[2] *100, 1)
# 
# str(mat2)
# load("output/sla_raw_37spp.Rda")
# #get_variables(mdl.traitphen)
# 
# sumt <- summary(mdl.traitphen)$summary
# 
# col4table <- c("mean","sd","2.5%","50%","97.5%","Rhat")
# 
# mu_params <-   c("mu_grand",
#                  "muPhenoSp",
#                  "muForceSp", # muAlphaForce
#                  "muChillSp",
#                  "muPhotoSp",
#                  "betaTraitxForce",
#                  "betaTraitxChill",
#                  "betaTraitxPhoto",
#                  "sigma_sp",
#                  "sigma_study",
#                  "sigma_traity",
#                  "sigmaPhenoSp",
#                  "sigmaForceSp",
#                  "sigmaChillSp",
#                  "sigmaPhotoSp", #sigma_alpha_photo
#                  "sigmapheno_y")
# esti <- sumt[mu_params, col4table]
# 
# #temp <- c(mugrandtrait, muStudy, muGrandSpname, betaForceSpname)
# rownames(esti) =c("mu_grand",
#                   "muPhenoSp",
#                   "muForceSp",
#                   "muChillSp",
#                   "muPhotoSp",
#                   "betaTraitxForce",
#                   "betaTraitxChill",
#                   "betaTraitxPhoto",
#                   "sigma_sp",
#                   "sigma_study",
#                   "sigma_traity",
#                   "sigmaPhenoSp",
#                   "sigmaForceSp",
#                   "sigmaChillSp",
#                   "sigmaPhotoSp",
#                   "sigmapheno_y")
# 
# esti.table <- sumt[mu_params, col4table]
# row.names(esti.table) <- row.names(esti)

# write.csv(esti.table, "slaMdlOutput.csv", row.names = T)


# # what studies had the largest study effects for each trait?
# load("..//..//analyses/traits/output/height_raw_37spp.Rda")
# # get_variables(mdl.traitphen)
# 
# traitsData1 <- read.csv("..//..//analyses/traits/input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
# traitsData2 <- read.csv("..//..//analyses/traits/input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
# traitsData <- rbind(traitsData1,traitsData2)
# 
# # sla:
# sum <- summary(mdl.traitphen)$summary
# slaStudy <- as.data.frame(sum[grep("muStudy", rownames(sum)), ]) # 15 studies
# # study 14= "65_try": 5.2269
# study14 <- subset(traitsData, datasetid == "65_try"); sort(unique(study14$speciesname)) #sorbus_aucuparia spp
# # study 2= "186_try": -5.029
# study2 <- subset(traitsData, datasetid == "186_try"); sort(unique(study2$speciesname)) #"Betula_pendula" "Quercus_ilex"   "Quercus_robur"
# 
# # a lot of trait variation in Q. ilex and F. grandifolia
# qilex <- subset(traitsData, speciesname == "Quercus_ilex"); length(unique(qilex$datasetid)) #in 17 studies
# fgrand <- subset(traitsData, speciesname == "Fagus_grandifolia"); length(unique(fgrand$datasetid)) #in 11 studies
# 
# # lnc:
# sum <- summary(mdl.traitphen)$summary
# lncStudy <- as.data.frame(sum[grep("muStudy", rownames(sum)), ]) # 12 studies
# # study 4 = 181_try: 3.00
# study4 <- subset(traitsData, datasetid == "181_try"); sort(unique(study4$speciesname))
# #"Betula_ermanii", "Juglans_regia", "Prunus_padus", "Rhododendron_dauricum", "Sorbus_pohuashanensis", "Ulmus_parvifolia","Ulmus_pumila"
# 
# # study 7 = 240_try: -6.64
# study7 <- subset(traitsData, datasetid == "240_try"); sort(unique(study7$speciesname)) #29 species
# # appears to be a lot of variation in Acer saccharum, and fagus grandifolia
# asacc <- subset(traitsData, speciesname == "Acer_saccharum"); length(unique(asacc$datasetid)) #in 16 studies
# 
# # ht:
# sum <- summary(mdl.traitphen)$summary
# htStudy <- as.data.frame(sum[grep("muStudy", rownames(sum)), ]) # 22 studies
# # study 5 ="18_bien": 13.66
# study5 <- subset(traitsData, datasetid == "18_bien"); sort(unique(study5$speciesname)) #22 species
# 
# # study 6 = "186_try": -13.29
# study5 <- subset(traitsData, datasetid == "186_try"); sort(unique(study5$speciesname)) #3 species
# 
# # what is going on with R.carthartica?
# rcart <- subset(traitsData, speciesname == "Rhamnus_cathartica"); length(unique(rcart$datasetid)) #in 16 studies
# hist(rcart$traitvalue)
# 
# psero <- subset(traitsData, speciesname == "Prunus_serotina"); length(unique(psero$datasetid)) #in 15 studies
# 
# # seed:
# sum <- summary(mdl.traitphen)$summary
# seedStudy <- as.data.frame(sum[grep("muStudy", rownames(sum)), ]) #4 studies
# # study 1= 17_bien: -0.26
# study1 <- subset(seedData, datasetid == "17_bien"); sort(unique(study1$speciesname)) #3 species
# # "Alnus_incana"        "Sorbus_aucuparia"
# 
# saucu <- subset(seedData, speciesname == "Sorbus_"); sort(unique(study1$speciesname)) #3 species
