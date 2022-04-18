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

put.fig.letter <- function(label, location="topleft", x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  if(length(label) > 1) {
    warning("length(label) > 1, using label[1]")
  }
  if(is.null(x) | is.null(y)) {
    coords <- switch(location,
                     topleft = c(0.05,0.98),
                     topcenter = c(0.5525,0.98),
                     topright = c(0.985, 0.98),
                     bottomleft = c(0.015, 0.02), 
                     bottomcenter = c(0.5525, 0.02), 
                     bottomright = c(0.985, 0.02),
                     c(0.015, 0.98) )
  } else {
    coords <- c(x,y)
  }
  this.x <- grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- grconvertY(coords[2] + offset[2], from="nfc", to="user")
  text(labels=label[1], x=this.x, y=this.y, xpd=T, cex = 2, ...)
}

# pdf("figures/slopesConsAcqu_maintext.pdf", width = 12, height = 8)
# par(mar = c(5, 5, 2, 2), mfrow=c(2,3))
# source("results_Height_plot.R")
# source("results_SLA_plot.R")
# dev.off()


pdf("figures/slopesConsAcqu_supp.pdf", width = 12, height = 8)
par(mar = c(5, 5, 2, 2), mfrow=c(2,3))
source("results_SeedMass_plot.R")
source("results_LNC_plot.R")
dev.off()
