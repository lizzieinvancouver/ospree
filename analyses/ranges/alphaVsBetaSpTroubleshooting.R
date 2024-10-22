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
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/ranges")
} else if
(length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits")
}

bb.stan <- read.csv("output/bbStanRanges.csv")
bb.stan.nam <- filter(bb.stan, continent=="N. America")
bb.stan.eu <- filter(bb.stan, continent!="N. America")

#specieslist <- sort(unique(bb.stan.nam$latbi))
head(bb.stan)
# Write a loop to run all the different traits plots:
################################
files <- list.files(path = "output", pattern =".Rda" )
files
rangeMetric <- c("Temp.Mean.GDD","STV","SD.lastfrost", "Temp.Mean.CP")

#for (i in 1:length(files)){
  
  Model <- load(paste("output/", files[5], sep = ""))
  ModelFit <- rstan::extract(GDD_jnt_nam)
  
  range <- unique(bb.stan.nam[,c("latbi", "Temp.Mean.GDD","STV","SD.lastfrost", "Temp.Mean.CP")])

  betaForceSp <- data.frame(ModelFit$betaForcingSp)
  betaForceSpMean <- colMeans(betaForceSp)
  
  quantile2575 <- function(x){
    returnQuanilte <- quantile(x, prob = c(0.25, 0.75))
    return(returnQuanilte)
  }
  
  bf_quan <- apply(betaForceSp, 2, quantile2575)

  bfs <- rbind(betaForceSpMean, bf_quan)
  bfs_t <- t(bfs)
  bfs_df <- data.frame(bfs_t)
  colnames(bfs_df)[colnames(bfs_df) == "X25."] <- "force25"
  colnames(bfs_df)[colnames(bfs_df) == "X75."] <- "force75"
  
  muForceSp <- data.frame(ModelFit$muForceSp)
  muForceSpMean <- colMeans(muForceSp)
  
  betaTraitxForce<- data.frame(ModelFit$betaTraitxForcing)
  betaTraitxForceMean <- colMeans(betaTraitxForce)
  
  
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
  #------------------------------------------------------------------------------#

alphaForceSp <- data.frame(ModelFit$alphaForcingSp)
alphaForceSpMean <- colMeans(alphaForceSp)
diff <- betaForceSp - alphaForceSp
diffForceMean <- colMeans(diff)


diff_quan <- apply(diff, 2, quantile2575)
alpha_quan <- apply(alphaForceSp, 2, quantile2575)
#beta_quan <- apply(betaForceSp, 2, quantile2575)
#mugrand_quan <- apply(muGrandSp, 2, quantile2575)

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

pdf(paste("figures/force_bdecomp", "GDD_Nam", ".pdf", sep = ""), height = 5, width =15)
par(mfrow = c(1,3))
plot( x= range$Temp.Mean.GDD, y = df_df$diffForceMean, type="n", xlim = c(10,500), ylim = c(-15,5)) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
for(r in 1:length(betaTraitxForce[,1])){
  abline(a = 0, b = betaTraitxForce[r,], col=alpha("lightpink", 0.15))
}
abline(a=0, b=betaTraitxForceMean, col = "grey")

arrows(
 range[,"Temp.Mean.GDD"], # x mean
  df_df[,"force25"], # y 25
 range[,"Temp.Mean.GDD"],
  df_df[,"force75"],
  length = 0
)


# dev.off()
#________________________________________________________________#
plot( x= range$Temp.Mean.GDD, y = a_df$alphaForceSpMean, type="n", xlim = c(10,500), ylim = c(-15,5)) # blank plot with x range 

# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
 range[,"Temp.Mean.GDD"], # x mean
  a_df[,"force25"], # y 25
 range[,"Temp.Mean.GDD"],
  a_df[,"force75"],
  length = 0
)

# arrows(
#   mg_df[,"trait25"], # x mean
#   a_df[,"alphaForceSpMean"], # y 25
#   mg_df[,"trait75"], # x mean
#   a_df[,"alphaForceSpMean"],
#   length = 0
# )

#________________________________________________________________#
plot( x= range$Temp.Mean.GDD, y = bfs_df$betaForceSpMean, type="n", xlim = c(10,500), ylim = c(-15,5)) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
for(r in 1:length(muForceSp[,1])){
  abline(a = muForceSp[r,], b = betaTraitxForceMean, col=alpha("lightpink", 0.15))
}
abline(a=muForceSpMean, b=betaTraitxForceMean, col = "grey")

arrows(
 range[,"Temp.Mean.GDD"], # x mean
  bfs_df[,"force25"], # y 25
 range[,"Temp.Mean.GDD"],
  bfs_df[,"force75"],
  length = 0
)

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
plot( x= range$Temp.Mean.GDD, y = df_df$diffChillMean, type="n", xlim = c(min(range$Temp.Mean.GDD), max(range$Temp.Mean.GDD)), ylim = c(min(df_df$chill25), max(df_df$chill75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
 range[,"Temp.Mean.GDD"], # x mean
  df_df[,"chill25"], # y 25
 range[,"Temp.Mean.GDD"],
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
plot( x= range$Temp.Mean.GDD, y = a_df$alphaChillSpMean, type="n", xlim = c(min(range$Temp.Mean.GDD), max(range$Temp.Mean.GDD)), ylim = c(min(a_df$chill25), max(a_df$chill75))) # blank plot with x range 

# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
 range[,"Temp.Mean.GDD"], # x mean
  a_df[,"chill25"], # y 25
 range[,"Temp.Mean.GDD"],
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
plot( x= range$Temp.Mean.GDD, y = b_df$betaChillSpMean, type="n", xlim = c(min(range$Temp.Mean.GDD), max(range$Temp.Mean.GDD)), ylim = c(min(b_df$chill25), max(a_df$chill75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
 range[,"Temp.Mean.GDD"], # x mean
  b_df[,"chill25"], # y 25
 range[,"Temp.Mean.GDD"],
  b_df[,"chill75"],
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
plot( x= gdd$Temp.Mean.GDD, y = df_df$diffPhotoMean, type="n", xlim = c(min(gdd$Temp.Mean.GDD), max(gdd$Temp.Mean.GDD)), ylim = c(min(df_df$photo25), max(df_df$photo75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
 gdd[,"Temp.Mean.GDD"], # x mean
  df_df[,"photo25"], # y 25
 gdd[,"Temp.Mean.GDD"],
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
plot( x= gdd$Temp.Mean.GDD, y = a_df$alphaPhotoSpMean, type="n", xlim = c(min(gdd$Temp.Mean.GDD), max(gdd$Temp.Mean.GDD)), ylim = c(min(a_df$photo25), max(a_df$photo75))) # blank plot with x range 

# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
 gdd[,"Temp.Mean.GDD"], # x mean
  a_df[,"photo25"], # y 25
 gdd[,"Temp.Mean.GDD"],
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
plot( x= gdd$Temp.Mean.GDD, y = b_df$betaPhotoSpMean, type="n", xlim = c(min(gdd$Temp.Mean.GDD), max(gdd$Temp.Mean.GDD)), ylim = c(min(b_df$photo25), max(a_df$photo75))) # blank plot with x range 
# 3 columns, mean, quantile
# min and max defined by quantiles
arrows(
 gdd[,"Temp.Mean.GDD"], # x mean
  b_df[,"photo25"], # y 25
 gdd[,"Temp.Mean.GDD"],
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
