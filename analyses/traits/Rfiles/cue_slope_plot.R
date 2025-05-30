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

#Function for labels:
put.fig.letter <- function(label, location="topleft", x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  if(length(label) > 1) {
    warning("length(label) > 1, using label[1]")
  }
  if(is.null(x) | is.null(y)) {
    coords <- switch(location,
                     topleft = c(0.15,0.97),
                     topcenter = c(0.5525,0.98),
                     topright = c(0.985, 0.98),
                     bottomleft = c(0.1, 0.02), 
                     bottomcenter = c(0.5525, 0.02), 
                     bottomright = c(0.985, 0.02),
                     c(0.1, 0.98) )
  } else {
    coords <- c(x,y)
  }
  this.x <- grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- grconvertY(coords[2] + offset[2], from="nfc", to="user")
  text(labels=label[1], x=this.x, y=this.y, xpd=T, cex = 2, ...)
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

heightData <- read.csv("input/height_37spp_subsampled.csv")
# Read Ospree data and subset

# Sorted species and study list
specieslist <- sort(unique(heightData$speciesname))

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
  int_quan <- apply(muForceSp, 2, quantile2575)
  
  int_fifty <- subset(muForceSp, ModelFit.muForceSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muForceSp < int_quan[2,])
  
  betaTraitxForce<- data.frame(ModelFit$betaTraitxForce)
  betaTraitxForceMean <- colMeans(betaTraitxForce)
  
  # Add coloured ones for the two species:"Corylus_avellana" = 11, "Acer_pseudoplatanus" = 2
  col.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8))
  col1.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.2), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.14))
  col2.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4))
  
  c_blue <- c("#8F2727")
  c_yellow <- c("#487575")
  
  c_yellow <- c("#ffac38")
  c_blue <- c("#025196")
  
  #pdf(paste("figures/force", files[i], ".pdf", sep = ""))
pdf(paste("figures/cue", "trait_wtrend_maintext_Qrubra", ".pdf", sep = ""), height = 16, width = 12)
  par(mar = c(5.5, 5, 2, 2), mfrow = c(4,3))
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75)), ylab = expression(paste("Response to forcing (", beta[forcing], ")")), xlab = "Height (m)", cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
#  mtext(side = 3, text = "Forcing", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxForceMean, col=alpha("gray", 0.1))
  }
  abline(a=muForceSpMean, b=betaTraitxForceMean, col = "black")
 #  "Populus_tremula" = acquis "Acer_pseudoplatanus"= conservative 
  ## Old spp = Corylus avellana
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bfs_df[6,"force25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bfs_df[6,"force75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
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
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bfs_df[6,"betaForceSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bfs_df[6,"betaForceSpMean"],
    length = 0, lwd = 4, col = c_blue)

  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bfs_df[32,"force25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bfs_df[32,"force75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bfs_df[32,"betaForceSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bfs_df[32,"betaForceSpMean"],
    length = 0, lwd = 4, col = c_yellow
    )
  
  my.label <- paste("(a", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font=1)

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
  
  int_quan <- apply(muChillSp, 2, quantile2575)
  int_fifty <- subset(muChillSp, ModelFit.muChillSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muChillSp < int_quan[2,])
  
  #pdf(paste("figures/chill", files[i], ".pdf", sep = ""))
  #pdf(paste("figures/chill", "lnc", ".pdf", sep = ""), height = 5, width = 5)
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(-40, 5), ylab = expression(paste("Response to cue chilling (", beta[chilling], ")")), xlab = "Height (m)", cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
  #mtext(side = 3, text = "Chilling", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxChillMean, col=alpha("gray", 0.1))
  }
  abline(a=muChillSpMean, b=betaTraitxChillMean, col = "black")
  
  # Corylus avellana
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
  
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bcs_df[6,"chill25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bcs_df[6,"chill75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bcs_df[6,"betaChillSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bcs_df[6,"betaChillSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bcs_df[32,"chill25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bcs_df[32,"chill75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bcs_df[32,"betaChillSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bcs_df[32,"betaChillSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(b", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
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
  
  int_quan <- apply(muPhotoSp, 2, quantile2575)
  int_fifty <- subset(muPhotoSp, ModelFit.muPhotoSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muPhotoSp < int_quan[2,])
  
  #pdf(paste("figures/photo", files[i], ".pdf", sep = ""))
  #pdf(paste("figures/photo", "height", ".pdf", sep = ""), height = 5, width = 5)
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(-10,5), ylab = expression(paste("Response to photoperiod (", beta[photoperiod], ")")), xlab = "Height (m)", cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
  #mtext(side = 3, text = "Photoperiod", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxPhotoMean, col=alpha("gray", 0.1))
  }
  abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "black")
 
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
  
  # Corylus avellana
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    
    bps_df[6,"photo25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bps_df[6,"photo75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bps_df[6,"betaPhotoSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bps_df[6,"betaPhotoSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bps_df[32,"photo25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bps_df[32,"photo75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bps_df[32,"betaPhotoSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bps_df[32,"betaPhotoSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(c", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
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
  
  int_quan <- apply(muForceSp, 2, quantile2575)
  int_fifty <- subset(muForceSp, ModelFit.muForceSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muForceSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75)), ylab = expression(paste("Response to forcing (", beta[forcing], ")")), xlab = expression(paste("SLA (", mm^2, mg^-1, ")")), cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
 
  #mtext(side = 3, text = "Forcing", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxForceMean, col=alpha("gray", 0.1))
  }
  abline(a=muForceSpMean, b=betaTraitxForceMean, col = "black")
  
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
  
  # Fagus grandifolia and Quercus ilex 
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bfs_df[6,"force25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bfs_df[6,"force75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bfs_df[6,"betaForceSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bfs_df[6,"betaForceSpMean"],
    length = 0, lwd = 4, col = c_blue)
  

  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bfs_df[32,"force25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bfs_df[32,"force75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bfs_df[32,"betaForceSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bfs_df[32,"betaForceSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(d", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  
  ######################################################
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
  
  int_quan <- apply(muChillSp, 2, quantile2575)
  int_fifty <- subset(muChillSp, ModelFit.muChillSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muChillSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(-40,5), ylab = expression(paste("Response to chilling (", beta[chilling], ")")), xlab = expression(paste("SLA (", mm^2, mg^-1, ")")), cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
#  mtext(side = 3, text = "Chilling", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxChillMean, col=alpha("gray", 0.1))
  }
  abline(a=muChillSpMean, b=betaTraitxChillMean, col = "black")
  
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
  
  # Corylus avellana
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bcs_df[6,"chill25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bcs_df[6,"chill75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bcs_df[6,"betaChillSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bcs_df[6,"betaChillSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bcs_df[32,"chill25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bcs_df[32,"chill75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bcs_df[32,"betaChillSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bcs_df[32,"betaChillSpMean"],
    length = 0, lwd =4, col = c_yellow)
  
  my.label <- paste("(e", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  #######################################################################
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
  
  int_quan <- apply(muPhotoSp, 2, quantile2575)
  int_fifty <- subset(muPhotoSp, ModelFit.muPhotoSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muPhotoSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75)), ylab = expression(paste("Response to photoperiod (", beta[photoperiod], ")")), xlab = expression(paste("SLA (", mm^2, mg^-1, ")")),cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
  #mtext(side = 3, text = "Photoperiod", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxPhotoMean, col=alpha("gray", 0.1))
  }
  abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "black")
  
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

  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bps_df[6,"photo25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bps_df[6,"photo75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bps_df[6,"betaPhotoSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bps_df[6,"betaPhotoSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bps_df[32,"photo25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bps_df[32,"photo75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bps_df[32,"betaPhotoSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bps_df[32,"betaPhotoSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(f", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  #dev.off()
  ####################
  # Seed mass
 # pdf(paste("figures/cue", "trait_wtrend_supp", ".pdf", sep = ""), height = 8, width = 12)
#  par(mar = c(5, 5, 2, 2), mfrow = c(2,3))
  
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
  
  int_quan <- apply(muForceSp, 2, quantile2575)
  int_fifty <- subset(muForceSp, ModelFit.muForceSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muForceSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75)), ylab = expression(paste("Response to forcing (", beta[forcing], ")")), xlab = "Seed mass (mg)",  cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
 # mtext(side = 3, text = "Forcing", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxForceMean, col=alpha("gray", 0.1))
  }
  abline(a=muForceSpMean, b=betaTraitxForceMean, col = "black")
  
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
  # Populus and acer
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bfs_df[6,"force25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bfs_df[6,"force75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bfs_df[6,"betaForceSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bfs_df[6,"betaForceSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bfs_df[32,"force25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bfs_df[32,"force75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bfs_df[32,"betaForceSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bfs_df[32,"betaForceSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(g", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  ###############################################################
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
  
  int_quan <- apply(muChillSp, 2, quantile2575)
  int_fifty <- subset(muChillSp, ModelFit.muChillSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muChillSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75)), ylab = expression(paste("Response to chilling (", beta[chilling], ")")), xlab = "Seed mass (mg)", cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
 # mtext(side = 3, text = "Chilling", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxChillMean, col=alpha("gray", 0.1))
  }
  abline(a=muChillSpMean, b=betaTraitxChillMean, col = "black")
  
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
  
  # Corylus avellana
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bcs_df[6,"chill25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bcs_df[6,"chill75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bcs_df[6,"betaChillSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bcs_df[6,"betaChillSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bcs_df[32,"chill25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bcs_df[32,"chill75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bcs_df[32,"betaChillSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bcs_df[32,"betaChillSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(h", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  ###############################################################
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
  
  int_quan <- apply(muPhotoSp, 2, quantile2575)
  int_fifty <- subset(muPhotoSp, ModelFit.muPhotoSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muPhotoSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75)), ylab = expression(paste("Response to photoperiod (", beta[photoperiod], ")")), xlab = "Seed mass (mg)", cex.lab = 1.75 ) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
  #mtext(side = 3, text = "Photoperiod", adj = 0, cex = 1.25)  
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxPhotoMean, col=alpha("gray", 0.1))
  }
  abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "black")
  
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
  
  # Corylus avellana
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bps_df[6,"photo25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bps_df[6,"photo75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bps_df[6,"betaPhotoSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bps_df[6,"betaPhotoSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bps_df[32,"photo25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bps_df[32,"photo75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bps_df[32,"betaPhotoSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bps_df[32,"betaPhotoSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(i", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  ###############################################################
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
  
  int_quan <- apply(muForceSp, 2, quantile2575)
  int_fifty <- subset(muForceSp, ModelFit.muForceSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muForceSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75)), ylab = expression(paste("Response to forcing (", beta[forcing], ")")), xlab = expression(paste("LNC (mg ",g^-1, ")")), cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
 
  #mtext(side = 3, text = "Forcing", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxForceMean, col=alpha("gray", 0.1))
  }
  abline(a=muForceSpMean, b=betaTraitxForceMean, col = "black")
  
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
  
  # Fagus grandifolia and Quercus ilex
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bfs_df[6,"force25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bfs_df[6,"force75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bfs_df[6,"betaForceSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bfs_df[6,"betaForceSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bfs_df[32,"force25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bfs_df[32,"force75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bfs_df[32,"betaForceSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bfs_df[32,"betaForceSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(j", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  ###############################################################
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
  
  int_quan <- apply(muChillSp, 2, quantile2575)
  int_fifty <- subset(muChillSp, ModelFit.muChillSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muChillSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75)), ylab = expression(paste("Response to chilling (", beta[chilling], ")")), xlab = expression(paste("LNC (mg ",g^-1, ")")), cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
  #mtext(side = 3, text = "Chilling", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxChillMean, col=alpha("gray", 0.1))
  }
  abline(a=muChillSpMean, b=betaTraitxChillMean, col = "black")
  
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
  
  # Corylus avellana
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bcs_df[6,"chill25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bcs_df[6,"chill75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bcs_df[6,"betaChillSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bcs_df[6,"betaChillSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bcs_df[32,"chill25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bcs_df[32,"chill75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bcs_df[32,"betaChillSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bcs_df[32,"betaChillSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(k", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  ###############################################################
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
  
  int_quan <- apply(muPhotoSp, 2, quantile2575)
  int_fifty <- subset(muPhotoSp, ModelFit.muPhotoSp > int_quan[1,])
  int_fifty <- subset( int_fifty, ModelFit.muPhotoSp < int_quan[2,])
  
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75)), ylab = expression(paste("Response to photoperiod (", beta[photoperiod], ")")), xlab = expression(paste("LNC (mg ",g^-1, ")")), cex.lab = 1.75) # blank plot with x range 
  # 3 columns, mean, quantile
  # min and max defined by quantiles
  
  #mtext(side = 3, text = "Photoperiod", adj = 0, cex = 1.25)
  for(j in 1:length(int_fifty[,1])){
    abline(a = int_fifty[j,], b = betaTraitxPhotoMean, col=alpha("gray", 0.1))
  }
  abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "black")
  
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
  
  # Corylus avellana
  arrows(
    mg_df[6,"muGrandSpMean"], # x mean
    bps_df[6,"photo25"], # y 25
    mg_df[6,"muGrandSpMean"],
    bps_df[6,"photo75"],
    length = 0 , lwd = 4, col = c_blue
  )
  
  arrows(
    mg_df[6,"trait25"], # x mean
    bps_df[6,"betaPhotoSpMean"], # y 25
    mg_df[6,"trait75"], # x mean
    bps_df[6,"betaPhotoSpMean"],
    length = 0, lwd = 4, col = c_blue)
  
  # Acer pseudoplatanus
  arrows(
    mg_df[32,"muGrandSpMean"], # x mean
    bps_df[32,"photo25"], # y 25
    mg_df[32,"muGrandSpMean"],
    bps_df[32,"photo75"],
    length = 0 , lwd = 4, col = c_yellow
  )
  
  arrows(
    mg_df[32,"trait25"], # x mean
    bps_df[32,"betaPhotoSpMean"], # y 25
    mg_df[32,"trait75"], # x mean
    bps_df[32,"betaPhotoSpMean"],
    length = 0, lwd = 4, col = c_yellow)
  
  my.label <- paste("(l", ")", sep="")
  put.fig.letter(label=my.label, location= "topleft", font =1)
  ###############################################################
  dev.off()
  
  