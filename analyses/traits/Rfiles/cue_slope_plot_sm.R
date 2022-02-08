rm(list=ls())
options(stringsAsFactors = FALSE)

require(dplyr)
library(stringr)
library(plyr)
library(rstan)

# Set working directory:
# Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if
(length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits")
}

# Get the data
traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
traitsData2 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")
# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

trt4 <- c("seed mass","Leaf_nitrogen_.N._content_per_leaf_dry_mass","Plant_height_vegetative","Specific_leaf_area")

traitsData <- subset(traitsData, traitsData$traitname %in% trt4)

traitsMean <- aggregate(traitsData["traitvalue"], traitsData[c("speciesname","traitname")], FUN = mean)

# get mean trait values:

# qrobur
# qilex
# qcoccifera <- subset(traitsData, speciesname == "Quercus_coccifera")
# qcoccifera <- subset(qcoccifera, traitname == "Plant_height_vegetative")
# dim(qcoccifera)
# sort(unique(qcoccifera$piname))
# range(qcoccifera$traitvalue)
# Read Ospree data and subset
# ospree <- read.csv("bbstan_allspp_utah.csv", header = TRUE)
# ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
# ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

# Write a loop to run all the different traits plots:
################################
files <- list.files(path = "output", pattern ="_37spp.RDS" )
files
  
  Model <- readRDS(paste("output/", files[1], sep = ""))
  #slaModel <- readRDS("output/height_stanfit.RDS")
  # sum <- summary(slaModel)$summary
  # test <- sum[grep("betaForceSp", rownames(sum)), "mean"]
  # rownames(sum)
  # ssm <-  as.shinystan(slaModel)
  # launch_shinystan(ssm)
  # 
  # str(slaModel)
  
  ModelFit <- rstan::extract(Model)
  
  muGrandSp <- data.frame(ModelFit$mu_grand_sp)
  muGrandSpMean <- colMeans(muGrandSp)
  
  betaForceSp <- data.frame(ModelFit$betaForceSp)
  betaForceSpMean <- colMeans(betaForceSp)
  
  quantile2575 <- function(x){
    returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
    return(returnQuanilte)
  }
  
  bf_quan <- apply(betaForceSp, 2, quantile2575) 
  mugrand_quan <- apply(muGrandSp, 2, quantile2575)
  
  bfs <- rbind(betaForceSpMean, bf_quan)
  bfs_t <- t(bfs)
  bfs_df <- data.frame(bfs_t)
  colnames(bfs_df)[colnames(bfs_df) == "X25."] <- "force25"
  colnames(bfs_df)[colnames(bfs_df) == "X75."] <- "force75"
  
  mg<- rbind(muGrandSpMean, mugrand_quan)
  mg_t <- t(mg)
  mg_df <- data.frame(mg_t)
  colnames(mg_df)[colnames(mg_df) == "X25."] <- "trait25"
  colnames(mg_df)[colnames(mg_df) == "X75."] <- "trait75"
  
  
  muForceSp <- data.frame(ModelFit$muForceSp)
  muForceSpMean <- colMeans(muForceSp)
  
  betaTraitxForce<- data.frame(ModelFit$betaTraitxForce)
  betaTraitxForceMean <- colMeans(betaTraitxForce)
  
  #pdf(paste("figures/force", files[i], ".pdf", sep = ""))
pdf(paste("figures/cue", "trait", ".pdf", sep = ""), height = 16, width = 12)
  par(mar = c(5, 5, 2, 2), mfrow = c(4,3))
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75)), ylab = "Species level forcing slope", xlab = "Estimated trait effect", cex.lab = 1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bfs_df[,"force25"], # y 25
    mg_df[,"muGrandSpMean"],
    bfs_df[,"force75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bfs_df[,"betaForceSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bfs_df[,"betaForceSpMean"],
    length = 0
  )
  mtext(side = 3, text = "Height, Forcing", adj = 0, cex = 1.25)
  # for(j in 1:length(muForceSp[,1])){
  #   abline(a = muForceSp[j,], b = betaTraitxForceMean, col=alpha("lightpink", 0.015))
  # }
  # abline(a=muForceSpMean, b=betaTraitxForceMean, col = "grey")
  #dev.off()
  #------------------------------------------------------------------------------#
  betaChillSp <- data.frame(ModelFit$betaChillSp)
  betaChillSpMean <- colMeans(betaChillSp)
  bc_quan <- apply(betaChillSp, 2, quantile2575)
  
  bcs <- rbind(betaChillSpMean, bc_quan)
  bcs_t <- t(bcs)
  bcs_df <- data.frame(bcs_t)
  colnames(bcs_df)[colnames(bcs_df) == "X25."] <- "chill25"
  colnames(bcs_df)[colnames(bcs_df) == "X75."] <- "chill75"
  
  muChillSp <- data.frame(ModelFit$muChillSp)
  muChillSpMean <- colMeans(muChillSp)
  
  betaTraitxChill<- data.frame(ModelFit$betaTraitxChill)
  betaTraitxChillMean <- colMeans(betaTraitxChill)
  
  #pdf(paste("figures/chill", files[i], ".pdf", sep = ""))
  #pdf(paste("figures/chill", "lnc", ".pdf", sep = ""), height = 5, width = 5)
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75)), ylab = "Species level chilling slope", xlab = "Estimated trait effect", cex.lab = 1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bcs_df[,"chill25"], # y 25
    mg_df[,"muGrandSpMean"],
    bcs_df[,"chill75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bcs_df[,"betaChillSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bcs_df[,"betaChillSpMean"],
    length = 0
  )
  mtext(side = 3, text = "Height, Chilling", adj = 0, cex = 1.25)
  # for(j in 1:length(muChillSp[,1])){
  #   abline(a = muChillSp[j,], b = betaTraitxChillMean, col=alpha("lightpink", 0.015))
  # }
  # abline(a=muChillSpMean, b=betaTraitxChillMean, col = "grey")
  #dev.off()
  #------------------------------------------------------------------------------#
  betaPhotoSp <- data.frame(ModelFit$betaPhotoSp)
  betaPhotoSpMean <- colMeans(betaPhotoSp)
  bp_quan <- apply(betaPhotoSp, 2, quantile2575)
  
  bps <- rbind(betaPhotoSpMean, bp_quan)
  bps_t <- t(bps)
  bps_df <- data.frame(bps_t)
  colnames(bps_df)[colnames(bps_df) == "X25."] <- "photo25"
  colnames(bps_df)[colnames(bps_df) == "X75."] <- "photo75"
  
  muPhotoSp <- data.frame(ModelFit$muPhotoSp)
  muPhotoSpMean <- colMeans(muPhotoSp)
  
  betaTraitxPhoto<- data.frame(ModelFit$betaTraitxPhoto)
  betaTraitxPhotoMean <- colMeans(betaTraitxPhoto)
  
  #pdf(paste("figures/photo", files[i], ".pdf", sep = ""))
  #pdf(paste("figures/photo", "height", ".pdf", sep = ""), height = 5, width = 5)
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75)), ylab = "Species level photoperiod slope", xlab = "Estimated trait effect", cex.lab = 1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bps_df[,"photo25"], # y 25
    mg_df[,"muGrandSpMean"],
    bps_df[,"photo75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bps_df[,"betaPhotoSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bps_df[,"betaPhotoSpMean"],
    length = 0
  )
  mtext(side = 3, text = "Height, Photoperiod", adj = 0, cex = 1.25)
  # for(j in 1:length(muPhotoSp[,1])){
  #   abline(a = muPhotoSp[i,], b = betaTraitxPhotoMean, col=alpha("lightpink", 0.015))
  # }
  # abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "grey")

  ####################
  # LNC
  Model <- readRDS(paste("output/", files[2], sep = ""))
  
  ModelFit <- rstan::extract(Model)
  
  muGrandSp <- data.frame(ModelFit$mu_grand_sp)
  muGrandSpMean <- colMeans(muGrandSp)
  
  betaForceSp <- data.frame(ModelFit$betaForceSp)
  betaForceSpMean <- colMeans(betaForceSp)
  
  quantile2575 <- function(x){
    returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
    return(returnQuanilte)
  }
  
  bf_quan <- apply(betaForceSp, 2, quantile2575) 
  mugrand_quan <- apply(muGrandSp, 2, quantile2575)
  
  bfs <- rbind(betaForceSpMean, bf_quan)
  bfs_t <- t(bfs)
  bfs_df <- data.frame(bfs_t)
  colnames(bfs_df)[colnames(bfs_df) == "X25."] <- "force25"
  colnames(bfs_df)[colnames(bfs_df) == "X75."] <- "force75"
  
  mg<- rbind(muGrandSpMean, mugrand_quan)
  mg_t <- t(mg)
  mg_df <- data.frame(mg_t)
  colnames(mg_df)[colnames(mg_df) == "X25."] <- "trait25"
  colnames(mg_df)[colnames(mg_df) == "X75."] <- "trait75"
  
  
  muForceSp <- data.frame(ModelFit$muForceSp)
  muForceSpMean <- colMeans(muForceSp)
  
  betaTraitxForce<- data.frame(ModelFit$betaTraitxForce)
  betaTraitxForceMean <- colMeans(betaTraitxForce)
  
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75)), ylab = "Species level forcing slope", xlab = "Estimated trait effect", cex.lab = 1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bfs_df[,"force25"], # y 25
    mg_df[,"muGrandSpMean"],
    bfs_df[,"force75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bfs_df[,"betaForceSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bfs_df[,"betaForceSpMean"],
    length = 0
  )
  mtext(side = 3, text = "LNC, Forcing", adj = 0, cex = 1.25)
  
  betaChillSp <- data.frame(ModelFit$betaChillSp)
  betaChillSpMean <- colMeans(betaChillSp)
  bc_quan <- apply(betaChillSp, 2, quantile2575)
  
  bcs <- rbind(betaChillSpMean, bc_quan)
  bcs_t <- t(bcs)
  bcs_df <- data.frame(bcs_t)
  colnames(bcs_df)[colnames(bcs_df) == "X25."] <- "chill25"
  colnames(bcs_df)[colnames(bcs_df) == "X75."] <- "chill75"
  
  muChillSp <- data.frame(ModelFit$muChillSp)
  muChillSpMean <- colMeans(muChillSp)
  
  betaTraitxChill<- data.frame(ModelFit$betaTraitxChill)
  betaTraitxChillMean <- colMeans(betaTraitxChill)
  
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75)), ylab = "Species level chilling slope", xlab = "Estimated trait effect", cex.lab =1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bcs_df[,"chill25"], # y 25
    mg_df[,"muGrandSpMean"],
    bcs_df[,"chill75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bcs_df[,"betaChillSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bcs_df[,"betaChillSpMean"],
    length = 0
  )
  mtext(side = 3, text = "LNC, Chilling", adj = 0, cex = 1.25)

  
  betaPhotoSp <- data.frame(ModelFit$betaPhotoSp)
  betaPhotoSpMean <- colMeans(betaPhotoSp)
  bp_quan <- apply(betaPhotoSp, 2, quantile2575)
  
  bps <- rbind(betaPhotoSpMean, bp_quan)
  bps_t <- t(bps)
  bps_df <- data.frame(bps_t)
  colnames(bps_df)[colnames(bps_df) == "X25."] <- "photo25"
  colnames(bps_df)[colnames(bps_df) == "X75."] <- "photo75"
  
  muPhotoSp <- data.frame(ModelFit$muPhotoSp)
  muPhotoSpMean <- colMeans(muPhotoSp)
  
  betaTraitxPhoto<- data.frame(ModelFit$betaTraitxPhoto)
  betaTraitxPhotoMean <- colMeans(betaTraitxPhoto)

  
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75)), ylab = "Species level photoperiod slope", xlab = "Estimated trait effect", cex.lab = 1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bps_df[,"photo25"], # y 25
    mg_df[,"muGrandSpMean"],
    bps_df[,"photo75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bps_df[,"betaPhotoSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bps_df[,"betaPhotoSpMean"],
    length = 0
  )
  mtext(side = 3, text = "LNC, Photoperiod", adj = 0, cex = 1.25)

  ####################
  # Seed mass
  Model <- readRDS(paste("output/", files[3], sep = ""))
  
  ModelFit <- rstan::extract(Model)
  
  muGrandSp <- data.frame(ModelFit$mu_grand_sp)
  muGrandSpMean <- colMeans(muGrandSp)
  
  betaForceSp <- data.frame(ModelFit$betaForceSp)
  betaForceSpMean <- colMeans(betaForceSp)
  
  quantile2575 <- function(x){
    returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
    return(returnQuanilte)
  }
  
  bf_quan <- apply(betaForceSp, 2, quantile2575) 
  mugrand_quan <- apply(muGrandSp, 2, quantile2575)
  
  bfs <- rbind(betaForceSpMean, bf_quan)
  bfs_t <- t(bfs)
  bfs_df <- data.frame(bfs_t)
  colnames(bfs_df)[colnames(bfs_df) == "X25."] <- "force25"
  colnames(bfs_df)[colnames(bfs_df) == "X75."] <- "force75"
  
  mg<- rbind(muGrandSpMean, mugrand_quan)
  mg_t <- t(mg)
  mg_df <- data.frame(mg_t)
  colnames(mg_df)[colnames(mg_df) == "X25."] <- "trait25"
  colnames(mg_df)[colnames(mg_df) == "X75."] <- "trait75"
  
  
  muForceSp <- data.frame(ModelFit$muForceSp)
  muForceSpMean <- colMeans(muForceSp)
  
  betaTraitxForce<- data.frame(ModelFit$betaTraitxForce)
  betaTraitxForceMean <- colMeans(betaTraitxForce)
  
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75)), ylab = "Species level forcing slope", xlab = "Estimated trait effect",  cex.lab =1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bfs_df[,"force25"], # y 25
    mg_df[,"muGrandSpMean"],
    bfs_df[,"force75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bfs_df[,"betaForceSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bfs_df[,"betaForceSpMean"],
    length = 0
  )
  mtext(side = 3, text = "Seed mass, Forcing", adj = 0, cex = 1.25)
  
  betaChillSp <- data.frame(ModelFit$betaChillSp)
  betaChillSpMean <- colMeans(betaChillSp)
  bc_quan <- apply(betaChillSp, 2, quantile2575)
  
  bcs <- rbind(betaChillSpMean, bc_quan)
  bcs_t <- t(bcs)
  bcs_df <- data.frame(bcs_t)
  colnames(bcs_df)[colnames(bcs_df) == "X25."] <- "chill25"
  colnames(bcs_df)[colnames(bcs_df) == "X75."] <- "chill75"
  
  muChillSp <- data.frame(ModelFit$muChillSp)
  muChillSpMean <- colMeans(muChillSp)
  
  betaTraitxChill<- data.frame(ModelFit$betaTraitxChill)
  betaTraitxChillMean <- colMeans(betaTraitxChill)
  
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75)), ylab = "Species level chilling slope", xlab = "Estimated trait effect", cex.lab = 1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bcs_df[,"chill25"], # y 25
    mg_df[,"muGrandSpMean"],
    bcs_df[,"chill75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bcs_df[,"betaChillSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bcs_df[,"betaChillSpMean"],
    length = 0
  )
  mtext(side = 3, text = "Seed mass, Chilling", adj = 0, cex = 1.25)

  
  betaPhotoSp <- data.frame(ModelFit$betaPhotoSp)
  betaPhotoSpMean <- colMeans(betaPhotoSp)
  bp_quan <- apply(betaPhotoSp, 2, quantile2575)
  
  bps <- rbind(betaPhotoSpMean, bp_quan)
  bps_t <- t(bps)
  bps_df <- data.frame(bps_t)
  colnames(bps_df)[colnames(bps_df) == "X25."] <- "photo25"
  colnames(bps_df)[colnames(bps_df) == "X75."] <- "photo75"
  
  muPhotoSp <- data.frame(ModelFit$muPhotoSp)
  muPhotoSpMean <- colMeans(muPhotoSp)
  
  betaTraitxPhoto<- data.frame(ModelFit$betaTraitxPhoto)
  betaTraitxPhotoMean <- colMeans(betaTraitxPhoto)
  
  
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75)), ylab = "Species level photoperiod slope", xlab = "Estimated trait effect", cex.lab =1.5 ) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bps_df[,"photo25"], # y 25
    mg_df[,"muGrandSpMean"],
    bps_df[,"photo75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bps_df[,"betaPhotoSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bps_df[,"betaPhotoSpMean"],
    length = 0
  )
  mtext(side = 3, text = "Seed mass, Photoperiod", adj = 0, cex = 1.25)  
  
###############################################
  # SLA
  Model <- readRDS(paste("output/", files[4], sep = ""))
  
  ModelFit <- rstan::extract(Model)
  
  muGrandSp <- data.frame(ModelFit$mu_grand_sp)
  muGrandSpMean <- colMeans(muGrandSp)
  
  betaForceSp <- data.frame(ModelFit$betaForceSp)
  betaForceSpMean <- colMeans(betaForceSp)
  
  quantile2575 <- function(x){
    returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
    return(returnQuanilte)
  }
  
  bf_quan <- apply(betaForceSp, 2, quantile2575) 
  mugrand_quan <- apply(muGrandSp, 2, quantile2575)
  
  bfs <- rbind(betaForceSpMean, bf_quan)
  bfs_t <- t(bfs)
  bfs_df <- data.frame(bfs_t)
  colnames(bfs_df)[colnames(bfs_df) == "X25."] <- "force25"
  colnames(bfs_df)[colnames(bfs_df) == "X75."] <- "force75"
  
  mg<- rbind(muGrandSpMean, mugrand_quan)
  mg_t <- t(mg)
  mg_df <- data.frame(mg_t)
  colnames(mg_df)[colnames(mg_df) == "X25."] <- "trait25"
  colnames(mg_df)[colnames(mg_df) == "X75."] <- "trait75"
  
  
  muForceSp <- data.frame(ModelFit$muForceSp)
  muForceSpMean <- colMeans(muForceSp)
  
  betaTraitxForce<- data.frame(ModelFit$betaTraitxForce)
  betaTraitxForceMean <- colMeans(betaTraitxForce)
  
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75)), ylab = "Species level forcing slope", xlab = "Estimated trait effect", cex.lab = 1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bfs_df[,"force25"], # y 25
    mg_df[,"muGrandSpMean"],
    bfs_df[,"force75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bfs_df[,"betaForceSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bfs_df[,"betaForceSpMean"],
    length = 0
  )
  mtext(side = 3, text = "SLA, Forcing", adj = 0, cex = 1.25)
  
  betaChillSp <- data.frame(ModelFit$betaChillSp)
  betaChillSpMean <- colMeans(betaChillSp)
  bc_quan <- apply(betaChillSp, 2, quantile2575)
  
  bcs <- rbind(betaChillSpMean, bc_quan)
  bcs_t <- t(bcs)
  bcs_df <- data.frame(bcs_t)
  colnames(bcs_df)[colnames(bcs_df) == "X25."] <- "chill25"
  colnames(bcs_df)[colnames(bcs_df) == "X75."] <- "chill75"
  
  muChillSp <- data.frame(ModelFit$muChillSp)
  muChillSpMean <- colMeans(muChillSp)
  
  betaTraitxChill<- data.frame(ModelFit$betaTraitxChill)
  betaTraitxChillMean <- colMeans(betaTraitxChill)
  
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75)), ylab = "Species level chilling slope", xlab = "Estimated trait effect", cex.lab =1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bcs_df[,"chill25"], # y 25
    mg_df[,"muGrandSpMean"],
    bcs_df[,"chill75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bcs_df[,"betaChillSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bcs_df[,"betaChillSpMean"],
    length = 0
  )
  mtext(side = 3, text = "SLA, Chilling", adj = 0, cex = 1.25)
  
  
  betaPhotoSp <- data.frame(ModelFit$betaPhotoSp)
  betaPhotoSpMean <- colMeans(betaPhotoSp)
  bp_quan <- apply(betaPhotoSp, 2, quantile2575)
  
  bps <- rbind(betaPhotoSpMean, bp_quan)
  bps_t <- t(bps)
  bps_df <- data.frame(bps_t)
  colnames(bps_df)[colnames(bps_df) == "X25."] <- "photo25"
  colnames(bps_df)[colnames(bps_df) == "X75."] <- "photo75"
  
  muPhotoSp <- data.frame(ModelFit$muPhotoSp)
  muPhotoSpMean <- colMeans(muPhotoSp)
  
  betaTraitxPhoto<- data.frame(ModelFit$betaTraitxPhoto)
  betaTraitxPhotoMean <- colMeans(betaTraitxPhoto)
  
  
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75)), ylab = "Species level photoperiod slope", xlab = "Estimated trait effect",cex.lab = 1.5) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  arrows(
    mg_df[,"muGrandSpMean"], # x mean
    bps_df[,"photo25"], # y 25
    mg_df[,"muGrandSpMean"],
    bps_df[,"photo75"],
    length = 0
  )
  
  arrows(
    mg_df[,"trait25"], # x mean
    bps_df[,"betaPhotoSpMean"], # y 25
    mg_df[,"trait75"], # x mean
    bps_df[,"betaPhotoSpMean"],
    length = 0
  )
  mtext(side = 3, text = "SLA, Photoperiod", adj = 0, cex = 1.25)
  
  dev.off()