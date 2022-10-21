####ranges plotting based on pop up
###run the rangeleadin_osp.R or load save models on your own machine
# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

# libraries
library(shinystan)
library(reshape2)
library(rstan)
library(rstanarm)
library(dplyr)
library(ggplot2)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")


load("popupmods.Rda")

ModelFit <- rstan::extract(threeparam_jnt.gdd)
tidybayes::get_variables(threeparam_jnt.gdd)
ggdlf$Temp.SD.z<-(ggdlf$Temp.SD-mean(ggdlf$Temp.SD))/sd(ggdlf$Temp.SD)
muGrandSp <-ggdlf$Temp.SD.z#data.frame(ModelFit$)### in there model this is  mu_grand + muSp[i]; its about the trait
muGrandSpMean <- muGrandSp

betaForceSp <- data.frame(ModelFit$betaForcingSp)
betaForceSpMean <- colMeans(betaForceSp)

quantile2575 <- function(x){
  returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
  return(returnQuanilte)
}

bf_quan <- apply(betaForceSp, 2, quantile2575) 
mugrand_quan <-muGrandSp #apply(muGrandSp, 2, quantile2575)



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

betaTraitxForce<- data.frame(ModelFit$betaTraitxForcing)
betaTraitxForceMean <- colMeans(betaTraitxForce)

###data


pdf(paste("figures", "trait_decomp", ".pdf", sep = ""), height = 16, width = 12)
col.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8))

par(mar = c(5, 5, 2, 2), mfrow = c(2,3))
plot( x= mg_df$muGrandSpMean, y = bfs_df$betaForceSpMean, type="n" , ylab = expression("Response to cue (standardized)"), xlab = "Variation in GDD to last frost", cex.lab = 1.5) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  bfs_df[,"force25"], # y 25
  mg_df[,"muGrandSpMean"],
  bfs_df[,"force75"],
  length = 0
)

#arrows(
 points(mg_df[,"muGrandSpMean"], # x mean
  bfs_df[,"betaForceSpMean"] # y 25
 # mg_df[,"muGrandSpMean"], # x mean
  #bfs_df[,"betaForceSpMean"],
  #length = 0
)
mtext(side = 3, text = "Forcing", adj = 0, cex = 1.25)

for(j in 1:length(int_fifty[,1])){
  abline(a = int_fifty[j,], b = betaTraitxForceMean, col=alpha("lightpink", 0.015))
}
abline(a=muForceSpMean, b=betaTraitxForceMean, col = "grey")
#-----
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
#plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n", xlim = c(min(mg_df$trait25), max(mg_df$trait75)), ylim = c(min(bcs_df$chill25), max(bcs_df$chill75)), ylab = expression("Response to cue (standardized)"), xlab = "Height (m)", cex.lab = 1.5) # blank plot with x range 
plot( x= mg_df$muGrandSpMean, y = bcs_df$betaChillSpMean, type="n" , ylab = expression("Response to cue (standardized)"), xlab = "Variation in GDD to last frost", cex.lab = 1.5) # blank plot with x range 
# 3 columns, mean, quantile
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  bcs_df[,"chill25"], # y 25
  
  mg_df[,"muGrandSpMean"],
  bcs_df[,"chill75"],
  length = 0
)

points(
  mg_df[,"muGrandSpMean"], # x mean
  bcs_df[,"betaChillSpMean"] # y 25
  #mg_df[,"trait75"], # x mean
  #bcs_df[,"betaChillSpMean"],
  #length = 0
)
mtext(side = 3, text = "Chilling", adj = 0, cex = 1.25)
for(j in 1:length(int_fifty[,1])){
  abline(a = int_fifty[j,], b = betaTraitxChillMean, col=alpha("lightpink", 0.015))
}
abline(a=muChillSpMean, b=betaTraitxChillMean, col = "grey")

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
plot( x= mg_df$muGrandSpMean, y = bps_df$betaPhotoSpMean, type="n",ylab = expression("Response to cue (standardized)"), xlab = "Variation in GDD to last frost", cex.lab = 1.5) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  bps_df[,"photo25"], # y 25
  mg_df[,"muGrandSpMean"],
  bps_df[,"photo75"],
  length = 0
)
points(
  mg_df[,"muGrandSpMean"], # x mean
  bps_df[,"betaPhotoSpMean"] # y 25
  #mg_df[,"trait75"], # x mean
  #bps_df[,"betaPhotoSpMean"],
  #length = 0
)
mtext(side = 3, text = "Photoperiod", adj = 0, cex = 1.25)
for(j in 1:length(int_fifty[,1])){
  abline(a = int_fifty[j,], b = betaTraitxPhotoMean, col=alpha("lightpink", 0.015))
}
abline(a=muPhotoSpMean, b=betaTraitxPhotoMean, col = "grey")

####
alphaForceSp <- data.frame(ModelFit$alphaForcingSp)
alphaForceSpMean <- colMeans(alphaForceSp)



af_quan <- apply(alphaForceSp, 2, quantile2575) 
mugrand_quan <-muGrandSp #apply(muGrandSp, 2, quantile2575)



afs <- rbind(alphaForceSpMean, af_quan)
afs_t <- t(afs)
afs_df <- data.frame(afs_t)
colnames(afs_df)[colnames(afs_df) == "X25."] <- "force25"
colnames(afs_df)[colnames(afs_df) == "X75."] <- "force75"

plot( x= mg_df$muGrandSpMean, y = afs_df$alphaForceSpMean, type="n",ylab = expression("Leftover"), xlab = "Variation in GDD to last frost", cex.lab = 1.5) # blank plot with x range 
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  afs_df[,"force25"], # y 25
  mg_df[,"muGrandSpMean"],
  afs_df[,"force75"],
  length = 0
)

points(
  mg_df[,"muGrandSpMean"], # x mean
  afs_df[,"alphaForceSpMean"] # y 25
  #mg_df[,"trait75"], # x mean
  #bps_df[,"betaPhotoSpMean"],
  #length = 0
)

alphaChillSp <- data.frame(ModelFit$alphaChillSp)
alphaChillSpMean <- colMeans(alphaChillSp)



ac_quan <- apply(alphaChillSp, 2, quantile2575) 




acs <- rbind(alphaChillSpMean, ac_quan)
acs_t <- t(acs)
acs_df <- data.frame(acs_t)
colnames(acs_df)[colnames(acs_df) == "X25."] <- "chill25"
colnames(acs_df)[colnames(acs_df) == "X75."] <- "chill75"

plot( x= mg_df$muGrandSpMean, y = acs_df$alphaChillSpMean, type="n",ylab = expression("Leftover"), xlab = "Variation in GDD to last frost", cex.lab = 1.5) # blank plot with x range 
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  acs_df[,"chill25"], # y 25
  mg_df[,"muGrandSpMean"],
  acs_df[,"chill75"],
  length = 0
)

points(
  mg_df[,"muGrandSpMean"], # x mean
  acs_df[,"alphaChillSpMean"] # y 25
  #mg_df[,"trait75"], # x mean
  #bps_df[,"betaPhotoSpMean"],
  #length = 0
)


alphaPhotoSp <- data.frame(ModelFit$alphaPhotoSp)
alphaPhotoSpMean <- colMeans(alphaPhotoSp)



ap_quan <- apply(alphaPhotoSp, 2, quantile2575) 




aps <- rbind(alphaPhotoSpMean, ap_quan)
aps_t <- t(aps)
aps_df <- data.frame(aps_t)
colnames(aps_df)[colnames(aps_df) == "X25."] <- "photo25"
colnames(aps_df)[colnames(aps_df) == "X75."] <- "photo75"

plot( x= mg_df$muGrandSpMean, y = aps_df$alphaPhotoSpMean, type="n",ylab = expression("Leftover"), xlab = "Variation in GDD to last frost", cex.lab = 1.5) # blank plot with x range 
arrows(
  mg_df[,"muGrandSpMean"], # x mean
  aps_df[,"photo25"], # y 25
  mg_df[,"muGrandSpMean"],
  aps_df[,"photo75"],
  length = 0
)

points(
  mg_df[,"muGrandSpMean"], # x mean
  aps_df[,"alphaPhotoSpMean"] # y 25
  #mg_df[,"trait75"], # x mean
  #bps_df[,"betaPhotoSpMean"],
  #length = 0
)
dev.off()
