# 
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
#     posterior <- extract(readRDS(file = "../../data/Ospree_traits/height_stanfit.RDS"))
# } else{
#     traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
#     traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
#     ospree <- read.csv("input/bbstan_allspp_utah.csv", stringsAsFactors = FALSE, header = TRUE)
     posterior <- rstan::extract(readRDS(file = "output/height_stanfit_37spp.RDS"))
#     posteriorOld <- rstan::extract(readRDS(file = "output/height_stanfit.RDS"))
#     
# }
# 
# traitsData <- rbind(traitsData1,traitsData2)
# 
# traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

# traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# HEIGHT trait only
# height <- traitsData[traitsData$traitname == "Plant_height_vegetative", ]

## Subsampling code from height_phenologycombined_DL.R
#### Removing dups and resampling height #########################
# small <- subset(height, traitvalue < 1.42) # this is 2.1% of the data 
# adult <- subset(height, traitvalue > 1.42)
# 
# # Remove duplicated height values:
# adult$dup <- duplicated(adult[, c("traitname", "traitvalue","unitname","latitude","longitude","piname","speciesname","database" )])
# temp <- subset(adult, dup == "TRUE") # 3456
# adultNoDup<- subset(adult, dup != "TRUE") #629094
# ## sampling height: start by separating out the species with few obs
# a.lot.ht <- c("Quercus_ellipsoidalis", "Alnus_rubra", "Fraxinus_nigra", "Populus_grandidentata", "Betula_lenta", "Betula_alleghaniensis", "Betula_papyrifera", "Fagus_grandifolia", "Quercus_velutina", "Prunus_serotina", "Quercus_rubra", "Acer_saccharum", "Quercus_alba")
# 
# few <- adultNoDup[!adultNoDup$speciesname %in% a.lot.ht, ] 
# 
# alot <- adultNoDup[adultNoDup$speciesname %in% a.lot.ht, ] #769505
# 
# # now sampling for species with a lot of data
# ht <- data.frame(speciesname = character(), height = numeric())
# species <- unique(alot$speciesname)
# 
# for (sp in 1: length(species)){
#   testsp <- subset(alot, speciesname == species[1])
#   mysample <- sample_n(testsp, 5000)
#   #dfadd <- data.frame(speciesname = species[sp], traitvalue = mysample)
#   # mysample$speciesname <- species[sp]
#   ht <- rbind(ht, mysample)
# }
# 
# heightData <- rbind(few, ht)
heightData <- read.csv("input/height_377spp_subsampled.csv")
# Read Ospree data and subset
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

# Sorted species and study list
specieslist <- sort(unique(heightData$speciesname))
studylist <- sort(unique(heightData$datasetid))

## Obtain mean effect of forcing, chilling, photoperiod, interaction
forceeff <- apply(posterior$betaForceSp, MARGIN = 2, FUN = mean)
chilleff <- apply(posterior$betaChillSp, MARGIN = 2, FUN = mean)
photoeff <- apply(posterior$betaPhotoSp, MARGIN = 2, FUN = mean)
mugrandeff <- apply(posterior$mu_grand_sp, MARGIN = 2, FUN = mean)
betaTraitForceeff <- mean(posterior$betaTraitxForce) # -0.4194243
betaTraitChilleff <- mean(posterior$betaTraitxChill) #-0.6746085
betaTraitPhotoeff <- mean(posterior$betaTraitxPhoto) #-0.1920812

# forceeff.26 <- apply(posteriorOld$betaForceSp, MARGIN = 2, FUN = mean)
# chilleff.26 <- apply(posteriorOld$betaChillSp, MARGIN = 2, FUN = mean)
# photoeff.26 <- apply(posteriorOld$betaPhotoSp, MARGIN = 2, FUN = mean)
# mugrandeff.26 <- apply(posteriorOld$mu_grand_sp, MARGIN = 2, FUN = mean)
# betaTraitForceeff.26 <- mean(posteriorOld$betaTraitxForce) # -0.3894519
# betaTraitChilleff.26 <- mean(posteriorOld$betaTraitxChill) #-0.5460526
# betaTraitPhotoeff.26 <- mean(posteriorOld$betaTraitxPhoto) #-0.2202021

## Species to plot and other plotting parameters
plot.sp <- c("Corylus_avellana", "Acer_pseudoplatanus") 
col.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8))
col1.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.2), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.14))
col2.sp <- c( rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5), rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4))

# pdf(file = "figures/results_height_37spp_ac.pdf", width = 15, height = 5)
# ## Plotting
# ### Forcing
# par(mar = c(5, 5, 2, 2), mfrow=c(1,3))
xrange <- seq(-2.5, 2.5, by = 0.25)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-20, 100),
     xlab = "Forcing (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.2)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 0.9)
axis(side = 2, at = seq(-20, 100, by = 20), tcl = -.5, las = 1, cex.axis = 0.9)
mtext(side = 3, text = "Height, Forcing", adj = 0, cex = 1.2)
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
mtext(side = 3, text = "Height, Chilling", adj = 0, cex = 1.2)
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
mtext(side = 3, text = "Height, Photoperiod", adj = 0, cex = 1.2)
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
legend("topright", legend = c(expression(paste("Acquisitive  (", italic("Corylus avellana"), ")")),
                              expression(paste("Conservative  (", italic("Acer pseudoplatanus"), ")")),
                              expression(paste("Trait effect", " = 0", "  (50% interval)", sep = "")),
                              expression(paste("Full model", "  (50% interval)"))),
       col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp, NA, NA),
       inset = 0.02, pch = c(21, 21, 15, 15), cex = 1, bty = "n")
# dev.off()
