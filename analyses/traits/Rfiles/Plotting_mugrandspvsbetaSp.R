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

# Read Ospree data and subset
# ospree <- read.csv("bbstan_allspp_utah.csv", header = TRUE)
# ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
# ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

# Sorted species and study listtry_bien_nodups_2.csv")

load("output/joint_height.RData")

sum <- summary(mdl.traitphen)$summary

head(sum)
trait_esti <- as.data.frame(sum[grep("mu_grand_sp", rownames(sum)),])
trait_esti <- trait_esti[,c("mean", "25%", "75%")]
colnames(trait_esti)[colnames(trait_esti) == "mean"] <- "height"
colnames(trait_esti)[colnames(trait_esti) == "25%"] <- "height25"
colnames(trait_esti)[colnames(trait_esti) == "75%"] <- "height75"

force_esti <- sum[grep("betaForceSp", rownames(sum)),]
force_esti <- force_esti[,c("mean", "25%", "75%")]
colnames(force_esti)[colnames(force_esti) == "mean"] <- "force"
colnames(force_esti)[colnames(force_esti) == "25%"] <- "force25"
colnames(force_esti)[colnames(force_esti) == "75%"] <- "force75"

chill_esti <- sum[grep("betaChillSp", rownames(sum)),]
chill_esti <- chill_esti[,c("mean", "25%", "75%")]
colnames(chill_esti)[colnames(chill_esti) == "mean"] <- "chill"
colnames(chill_esti)[colnames(chill_esti) == "25%"] <- "chill25"
colnames(chill_esti)[colnames(chill_esti) == "75%"] <- "chill75"

photo_esti <- sum[grep("betaPhotoSp", rownames(sum)),]
photo_esti <- photo_esti[,c("mean", "25%", "75%")]
colnames(photo_esti)[colnames(photo_esti) == "mean"] <- "photo"
colnames(photo_esti)[colnames(photo_esti) == "25%"] <- "photo25"
colnames(photo_esti)[colnames(photo_esti) == "75%"] <- "photo75"

mdl_out <- cbind(trait_esti, force_esti, chill_esti, photo_esti)

cheight <- ggplot(mdl_out, aes(x = height, y = chill)) +
  geom_point()+ labs(y="chilling cue") +
  geom_errorbar(aes(xmin = height25, xmax = height75)) +
  geom_errorbar(aes(ymin = chill25, ymax = chill75)) +
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

fheight <- ggplot(mdl_out, aes(x = height, y = force)) +
  geom_point()+ labs(y="forcing cue") +
  geom_errorbar(aes(xmin = height25, xmax = height75)) +
  geom_errorbar(aes(ymin = force25, ymax = force75)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

pheight <- ggplot(mdl_out, aes(x = height, y = photo)) +
  geom_point()+ labs(y="photo cue") +
  geom_errorbar(aes(xmin = height25, xmax = height75)) +
  geom_errorbar(aes(ymin = photo25, ymax = photo75)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

##############################################
#SLA
slaModel <- readRDS("output/SLA_stanfit.RDS")
# sum <- summary(slaModel)$summary
# test <- sum[grep("betaForceSp", rownames(sum)), "mean"]
# rownames(sum)
# ssm <-  as.shinystan(slaModel)
# launch_shinystan(ssm)
# 
# str(slaModel)

slaModelFit <- rstan::extract(slaModel)

muGrandSp <- data.frame(slaModelFit$mu_grand_sp)
muGrandSpMean <- colMeans(muGrandSp)

betaForceSp <- data.frame(slaModelFit$betaForceSp)
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
colnames(mg_df)[colnames(mg_df) == "X25."] <- "sla25"
colnames(mg_df)[colnames(mg_df) == "X75."] <- "sla75"


muForceSp <- data.frame(slaModelFit$muForceSp)
muForceSpMean <- colMeans(muForceSp)

betaTraitxForce<- data.frame(slaModelFit$betaTraitxForce)
betaTraitxForceMean <- colMeans(betaTraitxForce)

pdf("figures/force_sla.pdf")
plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$sla25), max(mg_df$sla75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75))) # blank plot with x range 
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
  mg_df[,"sla25"], # x mean
  bfs_df[,"betaForceSpMean"], # y 25
  mg_df[,"sla75"], # x mean
  bfs_df[,"betaForceSpMean"],
  length = 0
)

for(i in 1:length(muForceSp[,1])){
  abline(a = muForceSp[i,], b = betaTraitxForceMean, col=alpha("lightpink", 0.025))
}
abline(a=muForceSpMean, b=betaTraitxForceMean, col = "grey")
dev.off()
#------------------------------------------------------------------------------#
betaChillSp <- data.frame(slaModelFit$betaChillSp)
betaChillSpMean <- colMeans(betaChillSp)
bc_quan <- apply(betaChillSp, 2, quantile2575)

bcs <- rbind(betaChillSpMean, bc_quan)
bcs_t <- t(bcs)
bcs_df <- data.frame(bcs_t)
colnames(bcs_df)[colnames(bcs_df) == "X25."] <- "chill25"
colnames(bcs_df)[colnames(bcs_df) == "X75."] <- "chill75"

muChillSp <- data.frame(slaModelFit$muChillSp)
muChillSpMean <- colMeans(muChillSp)

betaTraitxChill<- data.frame(slaModelFit$betaTraitxChill)
betaTraitxChillMean <- colMeans(betaTraitxChill)

pdf("figures/chill_sla.pdf")
plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$sla25), max(mg_df$sla75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75))) # blank plot with x range 
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
  mg_df[,"sla25"], # x mean
  bcs_df[,"betaChillSpMean"], # y 25
  mg_df[,"sla75"], # x mean
  bcs_df[,"betaChillSpMean"],
  length = 0
)

for(i in 1:length(muChillSp[,1])){
  abline(a = muChillSp[i,], b = betaTraitxChillMean, col=alpha("lightpink", 0.025))
}
abline(a=muChillSpMean, b=betaTraitxChillMean, col = "grey")
dev.off()
#------------------------------------------------------------------------------#
betaPhotoSp <- data.frame(slaModelFit$betaPhotoSp)
betaPhotoSpMean <- colMeans(betaPhotoSp)
bp_quan <- apply(betaPhotoSp, 2, quantile2575)

bps <- rbind(betaPhotoSpMean, bp_quan)
bps_t <- t(bps)
bps_df <- data.frame(bps_t)
colnames(bps_df)[colnames(bps_df) == "X25."] <- "photo25"
colnames(bps_df)[colnames(bps_df) == "X75."] <- "photo75"

muPhotoSp <- data.frame(slaModelFit$muPhotoSp)
muPhotoSpMean <- colMeans(muPhotoSp)

betaTraitxPhoto<- data.frame(slaModelFit$betaTraitxPhoto)
betaTraitxPhotoMean <- colMeans(betaTraitxPhoto)

pdf("figures/photo_sla.pdf")
plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$sla25), max(mg_df$sla75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75))) # blank plot with x range 
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
  mg_df[,"sla25"], # x mean
  bps_df[,"betaPhotoSpMean"], # y 25
  mg_df[,"sla75"], # x mean
  bps_df[,"betaPhotoSpMean"],
  length = 0
)

for(i in 1:length(muPhotoSp[,1])){
  abline(a = muPhotoSp[i,], b = betaTraitxPhotoMean, col=alpha("lightpink", 0.025))
}
abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "grey")
dev.off()
##############################################
##############################################
#SeedMass
smModel <- readRDS("output/SeedMass_stanfit.RDS")
# sum <- summary(smModel)$summary
# test <- sum[grep("betaForceSp", rownames(sum)), "mean"]
# rownames(sum)
# ssm <-  as.shinystan(smModel)
# launch_shinystan(ssm)
# 
# str(smModel)

smModelFit <- rstan::extract(smModel)

muGrandSp <- data.frame(smModelFit$mu_grand_sp)
muGrandSpMean <- colMeans(muGrandSp)

betaForceSp <- data.frame(smModelFit$betaForceSp)
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
colnames(mg_df)[colnames(mg_df) == "X25."] <- "sm25"
colnames(mg_df)[colnames(mg_df) == "X75."] <- "sm75"


muForceSp <- data.frame(smModelFit$muForceSp)
muForceSpMean <- colMeans(muForceSp)

betaTraitxForce<- data.frame(smModelFit$betaTraitxForce)
betaTraitxForceMean <- colMeans(betaTraitxForce)

pdf("figures/force_sm.pdf")
plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n", xlim = c(min(mg_df$sm25), max(mg_df$sm75)), ylim = c(min(bfs_df$force25), max(bfs_df$force75))) # blank plot with x range 
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
  mg_df[,"sm25"], # x mean
  bfs_df[,"betaForceSpMean"], # y 25
  mg_df[,"sm75"], # x mean
  bfs_df[,"betaForceSpMean"],
  length = 0
)

for(i in 1:length(muForceSp[,1])){
  abline(a = muForceSp[i,], b = betaTraitxForceMean, col=alpha("lightpink", 0.025))
}
abline(a=muForceSpMean, b=betaTraitxForceMean, col = "grey")
dev.off()
#------------------------------------------------------------------------------#
betaChillSp <- data.frame(smModelFit$betaChillSp)
betaChillSpMean <- colMeans(betaChillSp)
bc_quan <- apply(betaChillSp, 2, quantile2575)

bcs <- rbind(betaChillSpMean, bc_quan)
bcs_t <- t(bcs)
bcs_df <- data.frame(bcs_t)
colnames(bcs_df)[colnames(bcs_df) == "X25."] <- "chill25"
colnames(bcs_df)[colnames(bcs_df) == "X75."] <- "chill75"

muChillSp <- data.frame(smModelFit$muChillSp)
muChillSpMean <- colMeans(muChillSp)

betaTraitxChill<- data.frame(smModelFit$betaTraitxChill)
betaTraitxChillMean <- colMeans(betaTraitxChill)

pdf("figures/chill_sm.pdf")
plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$sm25), max(mg_df$sm75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75))) # blank plot with x range 
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
  mg_df[,"sm25"], # x mean
  bcs_df[,"betaChillSpMean"], # y 25
  mg_df[,"sm75"], # x mean
  bcs_df[,"betaChillSpMean"],
  length = 0
)

for(i in 1:length(muChillSp[,1])){
  abline(a = muChillSp[i,], b = betaTraitxChillMean, col=alpha("lightpink", 0.025))
}
abline(a=muChillSpMean, b=betaTraitxChillMean, col = "grey")
dev.off()
#------------------------------------------------------------------------------#
betaPhotoSp <- data.frame(smModelFit$betaPhotoSp)
betaPhotoSpMean <- colMeans(betaPhotoSp)
bp_quan <- apply(betaPhotoSp, 2, quantile2575)

bps <- rbind(betaPhotoSpMean, bp_quan)
bps_t <- t(bps)
bps_df <- data.frame(bps_t)
colnames(bps_df)[colnames(bps_df) == "X25."] <- "photo25"
colnames(bps_df)[colnames(bps_df) == "X75."] <- "photo75"

muPhotoSp <- data.frame(smModelFit$muPhotoSp)
muPhotoSpMean <- colMeans(muPhotoSp)

betaTraitxPhoto<- data.frame(smModelFit$betaTraitxPhoto)
betaTraitxPhotoMean <- colMeans(betaTraitxPhoto)

pdf("figures/photo_sm.pdf")
plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n", xlim = c(min(mg_df$sm25), max(mg_df$sm75)), ylim = c(min(bps_df$photo25), max(bps_df$photo75))) # blank plot with x range 
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
  mg_df[,"sm25"], # x mean
  bps_df[,"betaPhotoSpMean"], # y 25
  mg_df[,"sm75"], # x mean
  bps_df[,"betaPhotoSpMean"],
  length = 0
)

for(i in 1:length(muPhotoSp[,1])){
  abline(a = muPhotoSp[i,], b = betaTraitxPhotoMean, col=alpha("lightpink", 0.025))
}
abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "grey")
dev.off()


################################
files <- list.files(path = "output", pattern =".RDS" )
files[2]
for (i in length(file)){
  Model <- readRDS(paste("output/", files[i], sep = ""))
  #slaModel <- readRDS("output/SLA_stanfit.RDS")
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
  
  for(i in 1:length(muForceSp[,1])){
    abline(a = muForceSp[i,], b = betaTraitxForceMean, col=alpha("lightpink", 0.025))
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
  
  for(i in 1:length(muChillSp[,1])){
    abline(a = muChillSp[i,], b = betaTraitxChillMean, col=alpha("lightpink", 0.025))
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
  
  for(i in 1:length(muPhotoSp[,1])){
    abline(a = muPhotoSp[i,], b = betaTraitxPhotoMean, col=alpha("lightpink", 0.025))
  }
  abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "grey")
  dev.off()
  
}

