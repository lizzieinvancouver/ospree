rm(list=ls())
options(stringsAsFactors = FALSE)

# ## Load libraries
# library(rstan)
# require(shinystan)
# library(hdrcde) ## better quantiles
# 
# ## Set seed
# set.seed(202109)
# 
# # Specify if this code should be run on Midge or on your own computer.
# MidgeFlag <- FALSE
# 
# if(MidgeFlag == TRUE){
#     traitsData1 <- read.csv("../../data/Ospree_traits/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
#     traitsData2 <- read.csv("../../data/Ospree_traits/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
#     ospree <- read.csv("../../data/Ospree_traits/bbstan_allspp.utah.csv", stringsAsFactors = FALSE, header = TRUE)
#     posterior <- extract(readRDS(file = "../../data/Ospree_traits/SeedMass_log10_stanfit.RDS"))
# } else{
    traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
    traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
    ospree <- read.csv("input/bbstan_allspp_utah_37spp.csv", stringsAsFactors = FALSE, header = TRUE)
    posterior <- extract(readRDS(file = "output/SeedMass_log10_stanfit_37spp_wp.RDS"))
#     posteriorOld <- extract(readRDS(file = "output/SeedMass_log10_stanfit.RDS"))
#     
# }
# 
traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")
# 
# # traitors.26 <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")
# 
# # Subset data to traitors species list
# traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# Seed mass trait only
seedData <- traitsData[traitsData$traitname == "seed mass",]

# Read Ospree data and subset
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)
#ospreeData.26 <- subset(ospree, ospree$speciesname %in% traitors.26)

# Exclude 12_bien as a study due to one data point
aggregate(seedData$traitvalue, by = list(seedData$datasetid), FUN = length) # check
## aggregate(seedData$traitvalue, by = list(seedData$datasetid, seedData$speciesname), FUN = length)
### Subset
seedData <- subset(seedData, !(seedData$datasetid == "12_bien"))

# Sorted species and study list
specieslist <- sort(unique(seedData$speciesname))
studylist <- sort(unique(seedData$datasetid))

## Obtain mean effect of forcing, chilling, photoperiod, interaction
forceeff <- apply(posterior$betaForceSp, MARGIN = 2, FUN = mean)
chilleff <- apply(posterior$betaChillSp, MARGIN = 2, FUN = mean)
photoeff <- apply(posterior$betaPhotoSp, MARGIN = 2, FUN = mean)
mugrandeff <- apply(posterior$mu_grand_sp, MARGIN = 2, FUN = mean)
betaTraitForceeff <- mean(posterior$betaTraitxForce) #-1.306722
betaTraitChilleff <- mean(posterior$betaTraitxChill) #-2.57087
betaTraitPhotoeff <- mean(posterior$betaTraitxPhoto) # -0.6908688

# forceeff.26 <- apply(posteriorOld$betaForceSp, MARGIN = 2, FUN = mean)
# chilleff.26 <- apply(posteriorOld$betaChillSp, MARGIN = 2, FUN = mean)
# photoeff.26 <- apply(posteriorOld$betaPhotoSp, MARGIN = 2, FUN = mean)
# mugrandeff.26 <- apply(posteriorOld$mu_grand_sp, MARGIN = 2, FUN = mean)
# betaTraitForceeff.26 <- mean(posteriorOld$betaTraitxForce) #-1.799758
# betaTraitChilleff.26 <- mean(posteriorOld$betaTraitxChill) #-3.269759
# betaTraitPhotoeff.26 <- mean(posteriorOld$betaTraitxPhoto) #-0.6908688

## Species to plot and other plotting parameters
plot.sp <- c("Populus_tremula", "Aesculus_hippocastanum")
col.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8))
col1.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.2),rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.14))
col2.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5),rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4))

#pdf(file = "figures/results_seedmass_37spp_ac.pdf", width = 15, height = 5)
## Plotting
### Forcing
#par(mar = c(5, 5, 2, 2), mfrow = c(1,3))
xrange <- seq(-2.5, 2.5, by = 0.25)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-30, 90),
     xlab = "Forcing (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.2)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 0.9)
axis(side = 2, at = seq(-30, 90, by = 30), tcl = -.5, las = 1, cex.axis = 0.9)
mtext(side = 3, text = "Seed Mass, Forcing", adj = 0, cex = 1.2)
## Add species to plot
for(i in 1:length(plot.sp)){
    stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor1[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$alphaForceSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
        stor2[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$betaForceSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
    }
    temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = col1.sp[i], border = NA)
    polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = col2.sp[i], border = NA)
}
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
    ## Add adjusted columns
    ospree.temp$forceadj1 <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$forceadj1[j] = ospree.temp$response.time[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
    }
    points(forceadj1 ~ jitter(force.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.sp[i], cex = 1)
}
# legend("topleft", legend = c(expression(paste("Acquisitive  (", italic("Populus tremula"), ")")),
#                              expression(paste("Conservative  (", italic("Aesculus hippocastanum"), ")")),                              expression(paste("Trait effect", " = 0", "  (50% interval)", sep = "")),
#                               expression(paste("Full model", "  (50% interval)"))),
#        col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp, NA, NA),
#        inset = 0.02, pch = c(21, 21, 15, 15), cex = 0.85, bty = "n")
# dev.off()

#pdf(file = "figures/results_seedmass_chilling_37spp_ac.pdf", width = 7, height = 6)
## Plotting
### Chilling
# par(mar = c(5, 5, 2, 2))
xrange <- seq(-2, 5, by = 0.25)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-30, 90),
     xlab = "Chilling (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.2)
axis(side = 1, at = seq(min(xrange), max(xrange), by = 1), tcl = -.5, cex.axis = 0.9)
axis(side = 2, at = seq(-30, 90, by = 30), tcl = -.5, las = 1, cex.axis = 0.9)
mtext(side = 3, text = "Seed Mass, Chilling", adj = 0, cex = 1.2)
## Add species to plot
for(i in 1:length(plot.sp)){
    stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor1[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$alphaChillSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
        stor2[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$betaChillSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
    }
    temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = col1.sp[i], border = NA)
    polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = col2.sp[i], border = NA)
}
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
    ## Add adjusted columns
    ospree.temp$chilladj1 <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$chilladj1[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
    }
    points(chilladj1 ~ jitter(chill.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.sp[i], cex = 1)
}
# legend("topleft", legend = c(expression(paste("Acquisitive  (", italic("Populus tremula"), ")")),
#                              expression(paste("Conservative  (", italic("Aesculus hippocastanum"), ")")),                              expression(paste("Trait effect", " = 0", "  (50% interval)", sep = "")),
#                               expression(paste("Full model", "  (50% interval)"))),
#        col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp, NA, NA),
#        inset = 0.02, pch = c(21, 21, 15, 15), cex = 0.85, bty = "n")
#dev.off()


#pdf(file = "figures/results_seedmass_photoperiod_37spp_ac.pdf", width = 7, height = 6)
## Plotting
### Photoperiod
#par(mar = c(5, 5, 2, 2))
xrange <- seq(-1.5, 2.5, by = 0.25)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-30, 90),
     xlab = "Photoperiod (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.2)
axis(side = 1, at = seq(min(xrange), max(xrange), by = 0.5), tcl = -.5, cex.axis = 0.9)
axis(side = 2, at = seq(-30, 90, by = 30), tcl = -.5, las = 1, cex.axis = 0.9)
mtext(side = 3, text = "Seed Mass, Photoperiod", adj = 0, cex = 1.2)
## Add species to plot
for(i in 1:length(plot.sp)){
    stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor1[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$alphaPhotoSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
        stor2[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$betaPhotoSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
    }
    temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = col1.sp[i], border = NA)
    polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = col2.sp[i], border = NA)
}
for(i in 1:length(plot.sp)){
        ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
    ## Add adjusted columns
    ospree.temp$photoadj1 <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$photoadj1[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j]
    }
    points(photoadj1 ~ jitter(photo.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.sp[i], cex = 1)
}
legend("topright", legend = c(expression(paste("Acquisitive  (", italic("Populus tremula"), ")")),
                              expression(paste("Conservative  (", italic("Aesculus hippocastanum"), ")")),
                              expression(paste("Trait effect", " = 0", "  (50% interval)", sep = "")),
                              expression(paste("Full model", "  (50% interval)"))),
       col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp, NA, NA),
       inset = 0.02, pch = c(21, 21, 15, 15), cex = 1, bty = "n")
#dev.off()

pdf("figures/seedmass_prior_post_dist.pdf", width = 15, height = 25)
par(mfrow = c(4,4))
#plot priors against posteriors
h1 <- hist(rnorm(1000, -15,10), col=rgb(1,0,1,1/4))
hist(posterior$muForceSp,add=T,col=rgb(0,0,1,1/4))

h1 <- hist(rnorm(1000, -15,10), col = rgb(1,0,1,1/4))
hist(posterior$muChillSp,add=T,col=rgb(0,0,1,1/4))

h1 <- hist(rnorm(1000, -15,10), col = rgb(1,0,1,1/4))
hist(posterior$muPhotoSp,add=T,col=rgb(0,0,1,1/4))

h1 <- hist(rnorm(1000, log10(100),1), col = rgb(1,0,1,1/4))
hist(posterior$mu_grand,add=T,col=rgb(0,0,1,1/4))

h1 <- hist(rnorm(1000,40,10), col = rgb(1,0,1,1/4))
hist(posterior$muPhenoSp,add=T,col=rgb(0,0,1,1/4))

h1 <- hist(rnorm(1000, 0,2), col = rgb(1,0,1,1/4))
hist(posterior$betaTraitxForce,add=T,col=rgb(0,0,1,1/4))

h1 <- hist(rnorm(1000, 0,2), col = rgb(1,0,1,1/4))
hist(posterior$betaTraitxChill,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000, 0,2), col = rgb(1,0,1,1/4))
hist(posterior$betaTraitxPhoto,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000, 1,1), col = rgb(1,0,1,1/4))
hist(posterior$sigma_sp,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000,1,0.5), col = rgb(1,0,1,1/4))
hist(posterior$sigma_study,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000, 0.2,0.1), col = rgb(1,0,1,1/4))
hist(posterior$sigma_traity,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000, 5,2), col = rgb(1,0,1,1/4))
hist(posterior$sigmaForceSp,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000, 10,5), col = rgb(1,0,1,1/4))
hist(posterior$sigmaChillSp,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000, 5,2), col = rgb(1,0,1,1/4))
hist(posterior$sigmaPhotoSp,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000, 10,5), col = rgb(1,0,1,1/4))
hist(posterior$sigmaPhenoSp,col=rgb(0,0,1,1/4),add=T)

h1 <- hist(rnorm(1000, 10,5), col = rgb(1,0,1,1/4))
hist(posterior$sigmapheno_y,add=T,col=rgb(0,0,1,1/4))

dev.off()
