# Started Feb 10, 2022 by deirdre

# the purpose of this code is to have simple code that makes the main figure for the traits ms
rm(list=ls())
options(stringsAsFactors = FALSE)

library(rstan)
require(shinystan)
library(hdrcde) ## better quantiles


setwd("~/Documents/github/ospree/analyses/traits")

traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
ospree <- read.csv("input/bbstan_allspp_utah.csv", stringsAsFactors = FALSE, header = TRUE)

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# Read Ospree data and subset
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

pdf("slopesConsAcqu.pdf", width = 12, height = 16)
par(mar = c(5, 5, 2, 2), mfrow=c(4,3))

source("results_Height_plot.R")
source("results_SLA_plot.R")
source("results_SeedMass_plot.R")
source("results_LNC_plot.R")
dev.off()
