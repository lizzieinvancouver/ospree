# Started Nov 8, 2021 by DL
# Plotting the phenology cues against the trait estimates

#housekeeping
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

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)
# 
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
files <- list.files(path = "output", pattern =".RDS" )
files
for (i in 1:length(files)){
  
  Model <- readRDS(paste("output/", files[i], sep = ""))
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
  
  pdf(paste("figures/force", files[i], ".pdf", sep = ""))
  plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75))) # blank plot with x range 
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
  
  for(j in 1:length(muForceSp[,1])){
    abline(a = muForceSp[j,], b = betaTraitxForceMean, col=alpha("lightpink", 0.015))
  }
  abline(a=muForceSpMean, b=betaTraitxForceMean, col = "grey")
  dev.off()
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
  
  pdf(paste("figures/chill", files[i], ".pdf", sep = ""))
  plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75))) # blank plot with x range 
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
  
  for(j in 1:length(muChillSp[,1])){
    abline(a = muChillSp[j,], b = betaTraitxChillMean, col=alpha("lightpink", 0.015))
  }
  abline(a=muChillSpMean, b=betaTraitxChillMean, col = "grey")
  dev.off()
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
  
  pdf(paste("figures/photo", files[i], ".pdf", sep = ""))
  plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75))) # blank plot with x range 
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
  
  for(j in 1:length(muPhotoSp[,1])){
    abline(a = muPhotoSp[i,], b = betaTraitxPhotoMean, col=alpha("lightpink", 0.015))
  }
  abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "grey")
  dev.off()
  
}

############################################################
# minus alphaForceSp - if the alpha is really strong then it could be driving the relationship
for (i in 1:length(files)){
Model <- readRDS(paste("output/", files[i], sep = ""))
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
alphaForceSp <- data.frame(ModelFit$alphaForceSp)

betaForceSpMean <- colMeans(betaForceSp)
alphaForceSpMean <- colMeans(alphaForceSp)
diff <- betaForceSp - alphaForceSp
diffForceMean <- colMeans(diff)

quantile2575 <- function(x){
  returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
  return(returnQuanilte)
}

diff_quan <- apply(diff, 2, quantile2575)
alpha_quan <- apply(alphaForceSp, 2, quantile2575)
beta_quan <- apply(betaForceSp, 2, quantile2575)
mugrand_quan <- apply(muGrandSp, 2, quantile2575)

df <- rbind(diffForceMean, diff_quan)
df_t <- t(df)
df_df <- data.frame(df_t)
colnames(df_df)[colnames(df_df) == "X25."] <- "force25"
colnames(df_df)[colnames(df_df) == "X75."] <- "force75"

alpha <- rbind(alphaForceSpMean, alpha_quan)
a_t <- t(alpha)
a_df <- data.frame(a_t)
colnames(a_df)[colnames(a_df) == "X25."] <- "force25"
colnames(a_df)[colnames(a_df) == "X75."] <- "force75"

beta <- rbind(betaForceSpMean, beta_quan)
b_t <- t(beta)
b_df <- data.frame(b_t)
colnames(b_df)[colnames(b_df) == "X25."] <- "force25"
colnames(b_df)[colnames(b_df) == "X75."] <- "force75"

mg<- rbind(muGrandSpMean, mugrand_quan)
mg_t <- t(mg)
mg_df <- data.frame(mg_t)
colnames(mg_df)[colnames(mg_df) == "X25."] <- "trait25"
colnames(mg_df)[colnames(mg_df) == "X75."] <- "trait75"


muForceSp <- data.frame(ModelFit$muForceSp)
muForceSpMean <- colMeans(muForceSp)

betaTraitxForce<- data.frame(ModelFit$betaTraitxForce)
betaTraitxForceMean <- colMeans(betaTraitxForce)

pdf(paste("figures/force_bdecomp", files[i], ".pdf", sep = ""), height = 5, width =15)
par(mfrow = c(1,3))
plot( x= mg_df$muGrandSpMean, y = df_df$diffForceMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(df_df$force25), max(df_df$force75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  df_df[,"force25"], # y 25
  mg_df[,"muGrandSpMean"],
  df_df[,"force75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  df_df[,"diffForceMean"], # y 25
  mg_df[,"trait75"], # x mean
  df_df[,"diffForceMean"],
  length = 0
)

for(r in 1:length(betaTraitxForce[,1])){
  abline(a = 0, b = betaTraitxForce[r,], col=alpha("lightpink", 0.015))
}
abline(a=0, b=betaTraitxForceMean, col = "grey")
# dev.off()
#________________________________________________________________#
plot( x= mg_df$muGrandSpMean, y = a_df$alphaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(a_df$force25), max(a_df$force75))) # blank plot with x range 

# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  a_df[,"force25"], # y 25
  mg_df[,"muGrandSpMean"],
  a_df[,"force75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  a_df[,"alphaForceSpMean"], # y 25
  mg_df[,"trait75"], # x mean
  a_df[,"alphaForceSpMean"],
  length = 0
)

#________________________________________________________________#
plot( x= mg_df$muGrandSpMean, y = b_df$betaForceSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(b_df$force25), max(a_df$force75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  b_df[,"force25"], # y 25
  mg_df[,"muGrandSpMean"],
  b_df[,"force75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  b_df[,"betaForceSpMean"], # y 25
  mg_df[,"trait75"], # x mean
  b_df[,"betaForceSpMean"],
  length = 0
)
for(r in 1:length(muPhotoSp[,1])){
  abline(a = muPhotoSp[r,], b = betaTraitxForceMean, col=alpha("lightpink", 0.015))
}
abline(a=muPhotoSpMean, b=betaTraitxForceMean, col = "grey")
dev.off()

#------------------------------------------------------------------------------#
betaChillSp <- data.frame(ModelFit$betaChillSp)
alphaChillSp <- data.frame(ModelFit$alphaChillSp)

betaChillSpMean <- colMeans(betaChillSp)
alphaChillSpMean <- colMeans(alphaChillSp)
diff <- betaChillSp - alphaChillSp
diffChillMean <- colMeans(diff)

quantile2575 <- function(x){
  returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
  return(returnQuanilte)
}

diff_quan <- apply(diff, 2, quantile2575)
alpha_quan <- apply(alphaChillSp, 2, quantile2575)
beta_quan <- apply(betaChillSp, 2, quantile2575)
mugrand_quan <- apply(muGrandSp, 2, quantile2575)

df <- rbind(diffChillMean, diff_quan)
df_t <- t(df)
df_df <- data.frame(df_t)
colnames(df_df)[colnames(df_df) == "X25."] <- "chill25"
colnames(df_df)[colnames(df_df) == "X75."] <- "chill75"

alpha <- rbind(alphaChillSpMean, alpha_quan)
a_t <- t(alpha)
a_df <- data.frame(a_t)
colnames(a_df)[colnames(a_df) == "X25."] <- "chill25"
colnames(a_df)[colnames(a_df) == "X75."] <- "chill75"

beta <- rbind(betaChillSpMean, beta_quan)
b_t <- t(beta)
b_df <- data.frame(b_t)
colnames(b_df)[colnames(b_df) == "X25."] <- "chill25"
colnames(b_df)[colnames(b_df) == "X75."] <- "chill75"

mg<- rbind(muGrandSpMean, mugrand_quan)
mg_t <- t(mg)
mg_df <- data.frame(mg_t)
colnames(mg_df)[colnames(mg_df) == "X25."] <- "trait25"
colnames(mg_df)[colnames(mg_df) == "X75."] <- "trait75"


muChillSp <- data.frame(ModelFit$muChillSp)
muChillSpMean <- colMeans(muChillSp)

betaTraitxChill<- data.frame(ModelFit$betaTraitxChill)
betaTraitxChillMean <- colMeans(betaTraitxChill)

pdf(paste("figures/chill_bdecomp", files[i], ".pdf", sep = ""), height = 5, width =15)
par(mfrow = c(1,3))
plot( x= mg_df$muGrandSpMean, y = df_df$diffChillMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(df_df$chill25), max(df_df$chill75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  df_df[,"chill25"], # y 25
  mg_df[,"muGrandSpMean"],
  df_df[,"chill75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  df_df[,"diffChillMean"], # y 25
  mg_df[,"trait75"], # x mean
  df_df[,"diffChillMean"],
  length = 0
)

for(r in 1:length(betaTraitxChill[,1])){
  abline(a = 0, b = betaTraitxChill[r,], col=alpha("lightpink", 0.015))
}
abline(a = 0, b=betaTraitxChillMean, col = "grey")
# dev.off()
#________________________________________________________________#
plot( x= mg_df$muGrandSpMean, y = a_df$alphaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(a_df$chill25), max(a_df$chill75))) # blank plot with x range 

# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  a_df[,"chill25"], # y 25
  mg_df[,"muGrandSpMean"],
  a_df[,"chill75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  a_df[,"alphaChillSpMean"], # y 25
  mg_df[,"trait75"], # x mean
  a_df[,"alphaChillSpMean"],
  length = 0
)

#________________________________________________________________#
plot( x= mg_df$muGrandSpMean, y = b_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(b_df$chill25), max(a_df$chill75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  b_df[,"chill25"], # y 25
  mg_df[,"muGrandSpMean"],
  b_df[,"chill75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  b_df[,"betaChillSpMean"], # y 25
  mg_df[,"trait75"], # x mean
  b_df[,"betaChillSpMean"],
  length = 0
)
for(r in 1:length(muPhotoSp[,1])){
  abline(a = muPhotoSp[r,], b = betaTraitxChillMean, col=alpha("lightpink", 0.015))
}
abline(a=muPhotoSpMean, b=betaTraitxChillMean, col = "grey")  
dev.off()

#------------------------------------------------------------------------------#
betaPhotoSp <- data.frame(ModelFit$betaPhotoSp)
alphaPhotoSp <- data.frame(ModelFit$alphaPhotoSp)

betaPhotoSpMean <- colMeans(betaPhotoSp)
alphaPhotoSpMean <- colMeans(alphaPhotoSp)
diff <- betaPhotoSp - alphaPhotoSp
diffPhotoMean <- colMeans(diff)

quantile2575 <- function(x){
  returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
  return(returnQuanilte)
}

diff_quan <- apply(diff, 2, quantile2575)
alpha_quan <- apply(alphaPhotoSp, 2, quantile2575)
beta_quan <- apply(betaPhotoSp, 2, quantile2575)
mugrand_quan <- apply(muGrandSp, 2, quantile2575)

df <- rbind(diffPhotoMean, diff_quan)
df_t <- t(df)
df_df <- data.frame(df_t)
colnames(df_df)[colnames(df_df) == "X25."] <- "photo25"
colnames(df_df)[colnames(df_df) == "X75."] <- "photo75"

alpha <- rbind(alphaPhotoSpMean, alpha_quan)
a_t <- t(alpha)
a_df <- data.frame(a_t)
colnames(a_df)[colnames(a_df) == "X25."] <- "photo25"
colnames(a_df)[colnames(a_df) == "X75."] <- "photo75"

beta <- rbind(betaPhotoSpMean, beta_quan)
b_t <- t(beta)
b_df <- data.frame(b_t)
colnames(b_df)[colnames(b_df) == "X25."] <- "photo25"
colnames(b_df)[colnames(b_df) == "X75."] <- "photo75"

mg<- rbind(muGrandSpMean, mugrand_quan)
mg_t <- t(mg)
mg_df <- data.frame(mg_t)
colnames(mg_df)[colnames(mg_df) == "X25."] <- "trait25"
colnames(mg_df)[colnames(mg_df) == "X75."] <- "trait75"


muPhotoSp <- data.frame(ModelFit$muPhotoSp)
muPhotoSpMean <- colMeans(muPhotoSp)

betaTraitxPhoto<- data.frame(ModelFit$betaTraitxPhoto)
betaTraitxPhotoMean <- colMeans(betaTraitxPhoto)

pdf(paste("figures/photo_bdecomp", files[i], ".pdf", sep = ""), height = 5, width =15)
par(mfrow = c(1,3))
plot( x= mg_df$muGrandSpMean, y = df_df$diffPhotoMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(df_df$photo25), max(df_df$photo75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  df_df[,"photo25"], # y 25
  mg_df[,"muGrandSpMean"],
  df_df[,"photo75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  df_df[,"diffPhotoMean"], # y 25
  mg_df[,"trait75"], # x mean
  df_df[,"diffPhotoMean"],
  length = 0
)

for(r in 1:length(betaTraitxPhoto[,1])){
  abline(a = 0, b = betaTraitxPhoto[r,], col=alpha("lightpink", 0.015))
}
abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "grey")
# dev.off()
#________________________________________________________________#
plot( x= mg_df$muGrandSpMean, y = a_df$alphaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(a_df$photo25), max(a_df$photo75))) # blank plot with x range 

# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  a_df[,"photo25"], # y 25
  mg_df[,"muGrandSpMean"],
  a_df[,"photo75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  a_df[,"alphaPhotoSpMean"], # y 25
  mg_df[,"trait75"], # x mean
  a_df[,"alphaPhotoSpMean"],
  length = 0
)

#________________________________________________________________#
plot( x= mg_df$muGrandSpMean, y = b_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(b_df$photo25), max(a_df$photo75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  b_df[,"photo25"], # y 25
  mg_df[,"muGrandSpMean"],
  b_df[,"photo75"],
  length = 0
)

arrows(
  mg_df[,"trait25"], # x mean
  b_df[,"betaPhotoSpMean"], # y 25
  mg_df[,"trait75"], # x mean
  b_df[,"betaPhotoSpMean"],
  length = 0
)
for(r in 1:length(muPhotoSp[,1])){
  abline(a = muPhotoSp[r,], b = betaTraitxPhotoMean, col=alpha("lightpink", 0.015))
}
abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "grey")
dev.off()
}
