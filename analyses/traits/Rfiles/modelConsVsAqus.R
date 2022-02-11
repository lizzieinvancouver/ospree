# Started Feb 10, 2022 by deirdre

# the purpose of this code is to have simple code that makes the main figure for the traits ms

library(rstan)
require(shinystan)
library(hdrcde) ## better quantiles

## Set seed
set.seed(202109)

# Specify if this code should be run on Midge or on your own computer.
MidgeFlag <- FALSE

if(MidgeFlag == TRUE){
  traitsData1 <- read.csv("../../data/Ospree_traits/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
  traitsData2 <- read.csv("../../data/Ospree_traits/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
  ospree <- read.csv("../../data/Ospree_traits/bbstan_allspp.utah.csv", stringsAsFactors = FALSE, header = TRUE)
  posterior <- extract(readRDS(file = "../../data/Ospree_traits/height_stanfit.RDS"))
} else{
  traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
  traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
  ospree <- read.csv("input/bbstan_allspp_utah.csv", stringsAsFactors = FALSE, header = TRUE)
}

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# Read Ospree data and subset
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

# Sorted species and study list
specieslist <- sort(unique(ospreeData$speciesname))


#################################################################################
# height
height <- traitsData[traitsData$traitname == "Plant_height_vegetative", ]
heightData <- read.csv("input/height_377spp_subsampled.csv")
ht_studylist <- sort(unique(heightData$datasetid))
# Sorted species and study list
htspecieslist <- sort(unique(heightData$speciesname))
htstudylist <- sort(unique(heightData$datasetid))

ht_posterior <- rstan::extract(readRDS(file = "output/height_stanfit_37spp.RDS"))

ht_forceeff <- apply(ht_posterior$betaForceSp, MARGIN = 2, FUN = mean)
ht_chilleff <- apply(ht_posterior$betaChillSp, MARGIN = 2, FUN = mean)
ht_photoeff <- apply(ht_posterior$betaPhotoSp, MARGIN = 2, FUN = mean)
ht_mugrandeff <- apply(ht_posterior$mu_grand_sp, MARGIN = 2, FUN = mean)
ht_betaTraitForceeff <- mean(ht_posterior$betaTraitxForce) # -0.4194243
ht_betaTraitChilleff <- mean(ht_posterior$betaTraitxChill) #-0.6746085
ht_betaTraitPhotoeff <- mean(ht_posterior$betaTraitxPhoto) #-0.1920812

## Species to plot and other plotting parameters
ht_plot.sp <- c("Corylus_avellana", "Acer_pseudoplatanus") 
ht_col.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8))
ht_col1.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.2), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.14))
ht_col2.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4))

#pdf(file = "figures/results_37spp_ac.pdf", width = 15, height = 5)
## Plotting
### Forcing
par(mar = c(5, 5, 2, 2), mfrow=c(4,3))
xrange <- seq(-2.5, 2.5, by = 0.25)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-20, 100),
     xlab = "Forcing (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.2)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 0.9)
axis(side = 2, at = seq(-20, 100, by = 20), tcl = -.5, las = 1, cex.axis = 0.9)
mtext(side = 3, text = "Height, Forcing", adj = 0, cex = 0.75)
## Add species to plot
for(i in 1:length(ht_plot.sp)){
  stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  for(k in 1:4000){
    stor1[k, ] <- rnorm(n = length(xrange), mean = ht_posterior$alphaPhenoSp[k, which(specieslist == ht_plot.sp[i])] + ht_posterior$alphaForceSp[k, which(specieslist == ht_plot.sp[i])] * xrange, sd = ht_posterior$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean =ht_posterior$alphaPhenoSp[k, which(specieslist == ht_plot.sp[i])] + ht_posterior$betaForceSp[k, which(specieslist == ht_plot.sp[i])] * xrange, sd = ht_posterior$sigmapheno_y[k])
  }
  temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
  temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
  polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = ht_col1.sp[i], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = ht_col2.sp[i], border = NA)
}
for(i in 1:length(ht_plot.sp)){
  ospree.temp <- subset(ospreeData, ospreeData$speciesname == ht_plot.sp[i])
  ## Add adjusted columns
  ospree.temp$forceadj1 <- ospree.temp$response.time
  for(j in 1:nrow(ospree.temp)){
    ospree.temp$forceadj1[j] = ospree.temp$response.time[j] - ht_chilleff[which(specieslist == ht_plot.sp[i])] * ospree.temp$chill.z[j] - ht_photoeff[which(specieslist == ht_plot.sp[i])] * ospree.temp$photo.z[j]
  }
  points(forceadj1 ~ jitter(force.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = ht_col.sp[i], cex = 1)
}
# legend("topright", legend = c(expression(paste("Acquisitive  (", italic("Corylus avellana"), ")")),
#                               expression(paste("Conservative  (", italic("Acer pseudoplatanus"), ")")),
#                               expression(paste("Trait effect", " = 0", "  (50% interval)", sep = "")),
#                               expression(paste("Full model", "  (50% interval)"))),
#        col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp, NA, NA),
#        inset = 0.02, pch = c(21, 21, 15, 15), cex = 0.85, bty = "n")
# dev.off()

# pdf(file = "figures/results_height_chilling_37spp_ac.pdf", width = 7, height = 6)
## Plotting
### Chilling
# par(mar = c(5, 5, 2, 2))
xrange <- seq(-2, 5, by = 0.25)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-20, 100),
     xlab = "Chilling (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.2)
axis(side = 1, at = seq(min(xrange), max(xrange), by = 1), tcl = -.5, cex.axis = 0.9)
axis(side = 2, at = seq(-20, 100, by = 20), tcl = -.5, las = 1, cex.axis = 0.9)
mtext(side = 3, text = "Height, Chilling", adj = 0, cex = 0.75)
## Add species to plot
for(i in 1:length(ht_plot.sp)){
  stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  for(k in 1:4000){
    stor1[k, ] <- rnorm(n = length(xrange), mean = ht_posterior$alphaPhenoSp[k, which(specieslist == ht_plot.sp[i])] + ht_posterior$alphaChillSp[k, which(specieslist == ht_plot.sp[i])] * xrange, sd = ht_posterior$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = ht_posterior$alphaPhenoSp[k, which(specieslist == ht_plot.sp[i])] + ht_posterior$betaChillSp[k, which(specieslist == ht_plot.sp[i])] * xrange, sd = ht_posterior$sigmapheno_y[k])
  }
  temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
  temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
  polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = ht_col1.sp[i], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = ht_col2.sp[i], border = NA)
}
for(i in 1:length(ht_plot.sp)){
  ospree.temp <- subset(ospreeData, ospreeData$speciesname == ht_plot.sp[i])
  ## Add adjusted columns
  ospree.temp$chilladj1 <- ospree.temp$response.time
  for(j in 1:nrow(ospree.temp)){
    ospree.temp$chilladj1[j] = ospree.temp$response.time[j] - ht_forceeff[which(specieslist == ht_plot.sp[i])] * ospree.temp$force.z[j] - ht_photoeff[which(specieslist == ht_plot.sp[i])] * ospree.temp$photo.z[j]
  }
  points(chilladj1 ~ jitter(chill.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = ht_col.sp[i], cex = 1)
}
# legend("topright", legend = c(expression(paste("Acquisitive  (", italic("Corylus avellana"), ")")),
#                               expression(paste("Conservative  (", italic("Acer pseudoplatanus"), ")")),
#                               expression(paste("Trait effect", " = 0", "  (50% interval)", sep = "")),
#                               expression(paste("Full model", "  (50% interval)"))),
#        col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp, NA, NA),
#        inset = 0.02, pch = c(21, 21, 15, 15), cex = 0.85, bty = "n")
# dev.off()
# 
# 
# pdf(file = "figures/results_height_photoperiod_37spp_ac.pdf", width = 7, height = 6)
# ## Plotting
# ### Photoperiod
# par(mar = c(5, 5, 2, 2))
xrange <- seq(-1.5, 2.5, by = 0.25)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-20, 100),
     xlab = "Photoperiod (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.2)
axis(side = 1, at = seq(min(xrange), max(xrange), by = 0.5), tcl = -.5, cex.axis = 0.9)
axis(side = 2, at = seq(-20, 100, by = 20), tcl = -.5, las = 1, cex.axis = 0.9)
mtext(side = 3, text = "Height, Photoperiod", adj = 0, cex = 0.75)
## Add species to plot
for(i in 1:length(ht_plot.sp)){
  stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  for(k in 1:4000){
    stor1[k, ] <- rnorm(n = length(xrange), mean = ht_posterior$alphaPhenoSp[k, which(specieslist == ht_plot.sp[i])] + ht_posterior$alphaPhotoSp[k, which(specieslist == ht_plot.sp[i])] * xrange, sd = ht_posterior$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = ht_posterior$alphaPhenoSp[k, which(specieslist == ht_plot.sp[i])] + ht_posterior$betaPhotoSp[k, which(specieslist == ht_plot.sp[i])] * xrange, sd = ht_posterior$sigmapheno_y[k])
  }
  temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
  temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
  polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = ht_col1.sp[i], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = ht_col2.sp[i], border = NA)
}
for(i in 1:length(ht_plot.sp)){
  ospree.temp <- subset(ospreeData, ospreeData$speciesname == ht_plot.sp[i])
  ## Add adjusted columns
  ospree.temp$photoadj1 <- ospree.temp$response.time
  for(j in 1:nrow(ospree.temp)){
    ospree.temp$photoadj1[j] = ospree.temp$response.time[j] - ht_forceeff[which(specieslist == ht_plot.sp[i])] * ospree.temp$force.z[j] - ht_chilleff[which(specieslist == ht_plot.sp[i])] * ospree.temp$chill.z[j]
  }
  points(photoadj1 ~ jitter(photo.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = ht_col.sp[i], cex = 1)
}
legend("topright", legend = c(expression(paste("Acquisitive  (", italic("Corylus avellana"), ")")),
                              expression(paste("Conservative  (", italic("Acer pseudoplatanus"), ")")),
                              expression(paste("Trait effect", " = 0", "  (50% interval)", sep = "")),
                              expression(paste("Full model", "  (50% interval)"))),
       col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(ht_col.sp, NA, NA),
       inset = 0.02, pch = c(21, 21, 15, 15), cex = 0.5, bty = "n")
