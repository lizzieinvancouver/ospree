
## Load libraries
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
    posterior <- extract(readRDS(file = "../../data/Ospree_traits/SeedMass_log10_stanfit.RDS"))
} else{
    traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
    traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
    ospree <- read.csv("input/bbstan_allspp_utah.csv", stringsAsFactors = FALSE, header = TRUE)
    posterior <- extract(readRDS(file = "output/SeedMass_log10_stanfit.RDS"))
}

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# Seed mass trait only
seedData <- traitsData[traitsData$traitname == "seed mass",]

# Read Ospree data and subset
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

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

## Species to plot and other plotting parameters
plot.sp <- c("Populus_tremula", "Acer_pensylvanicum", "Aesculus_hippocastanum")
col.sp <- c("red", "blue", "orange")

pdf(file = "SeedMassVisual1.pdf", width = 6, height = 6)
## par(mfrow = c(1, 3))
## Plotting
### Forcing
xrange <- seq(-2.5, 3, by = 0.1)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-30, 90),
     xlab = "Forcing (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.3)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 1)
axis(side = 2, at = seq(-30, 90, by = 30), tcl = -.5, las = 1, cex.axis = 1)
mtext(side = 3, text = "Seed Mass, Forcing", adj = 0, cex = 1.3)
## Add species to plot
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
## Add adjusted column
    ospree.temp$forceadj <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$forceadj[j] = ospree.temp$response.time[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
    }
    points(forceadj ~ jitter(force.z, factor = 0.25), data = ospree.temp, pch = 21, col = col.sp[i], cex = 1.2)
    stor <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$betaForceSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
    }
    temp.hdr <- apply(stor, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp.hdr[1, ], rev(temp.hdr[2, ])), col = rgb(col2rgb(col.sp[i])[1] / 255, col2rgb(col.sp[i])[2] / 255, col2rgb(col.sp[i])[3] / 255, alpha = .2), border = NA)
}
legend("topright", legend = c(plot.sp, "Data", "50% prediction interval"), col = c(col.sp, "black", "black"), inset = 0.05, pch = c(15, 15, 15, 21, 15), cex = 0.85)
dev.off()

pdf(file = "SeedMassVisual2.pdf", width = 6, height = 6)
### Chilling
xrange <- seq(-2, 5, by = 0.1)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-30, 90),
     xlab = "Chilling (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.3)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 1)
axis(side = 2, at = seq(-30, 90, by = 30), tcl = -.5, las = 1, cex.axis = 1)
mtext(side = 3, text = "Seed Mass, Chilling", adj = 0, cex = 1.3)
## Add species to plot
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
## Add adjusted column
    ospree.temp$chilladj <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$chilladj[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
    }
    points(chilladj ~ jitter(chill.z, factor = 0.25), data = ospree.temp, pch = 21, col = col.sp[i], cex = 1.2)
    stor <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$betaChillSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
    }
    temp.hdr <- apply(stor, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp.hdr[1, ], rev(temp.hdr[2, ])), col = rgb(col2rgb(col.sp[i])[1] / 255, col2rgb(col.sp[i])[2] / 255, col2rgb(col.sp[i])[3] / 255, alpha = .2), border = NA)
}
legend("topright", legend = c(plot.sp, "Data", "50% prediction interval"), col = c(col.sp, "black", "black"), inset = 0.05, pch = c(15, 15, 15, 21, 15), cex = 0.85)
dev.off()

pdf(file = "SeedMassVisual3.pdf", width = 6, height = 6)
### Photoperiod
xrange <- seq(-1.5, 2.5, by = 0.1)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-30, 90),
     xlab = "Photoperiod (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.3)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 1)
axis(side = 2, at = seq(-30, 90, by = 30), tcl = -.5, las = 1, cex.axis = 1)
mtext(side = 3, text = "Seed Mass, Photoperiod", adj = 0, cex = 1.3)
## Add species to plot
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
## Add adjusted column
    ospree.temp$photoadj <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$photoadj[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j]
    }
    points(photoadj ~ jitter(photo.z, factor = 0.25), data = ospree.temp, pch = 21, col = col.sp[i], cex = 1.2)
    stor <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor[k, ] <- rnorm(n = length(xrange), mean = posterior$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior$betaPhotoSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior$sigmapheno_y[k])
    }
    temp.hdr <- apply(stor, MARGIN = 2, FUN = function(X) hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp.hdr[1, ], rev(temp.hdr[2, ])), col = rgb(col2rgb(col.sp[i])[1] / 255, col2rgb(col.sp[i])[2] / 255, col2rgb(col.sp[i])[3] / 255, alpha = .2), border = NA)
}
legend("topright", legend = c(plot.sp, "Data", "50% prediction interval"), col = c(col.sp, "black", "black"), inset = 0.05, pch = c(15, 15, 15, 21, 15), cex = 0.85)
dev.off()
