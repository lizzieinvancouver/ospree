
## Load libraries
library(rstan)
library(shinystan)
library(hdrcde) ## better quantiles
library(dplyr)

## Set seed
set.seed(202109)

# Specify if this code should be run on Midge or on your own computer.
MidgeFlag <- TRUE

if(MidgeFlag == TRUE){
    traitsData1 <- read.csv("../../data/Ospree_traits/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
    traitsData2 <- read.csv("../../data/Ospree_traits/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
    ospree <- read.csv("../../data/Ospree_traits/bbstan_allspp.utah.csv", stringsAsFactors = FALSE, header = TRUE)
    posterior <- extract(readRDS(file = "../../data/Ospree_traits/height_stanfit.RDS"))
} else{
    traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
    traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
    ospree <- read.csv("input/bbstan_allspp_utah.csv", stringsAsFactors = FALSE, header = TRUE)
    posterior <- extract(readRDS(file = "output/height_stanfit.RDS"))
}

traitsData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# HEIGHT trait only
height <- traitsData[traitsData$traitname == "Plant_height_vegetative", ]

## Subsampling code from height_phenologycombined_DL.R
#### Removing dups and resampling height #########################
small <- subset(height, traitvalue < 1.42) # this is 2.1% of the data 
adult <- subset(height, traitvalue > 1.42)

# Remove duplicated height values:
adult$dup <- duplicated(adult[, c("traitname", "traitvalue","unitname","latitude","longitude","piname","speciesname","database" )])
temp <- subset(adult, dup == "TRUE") # 3456
adultNoDup<- subset(adult, dup != "TRUE") #629094
## sampling height: start by separating out the species with few obs
a.lot.ht <- c("Quercus_ellipsoidalis", "Alnus_rubra", "Fraxinus_nigra", "Populus_grandidentata", "Betula_lenta", "Betula_alleghaniensis", "Betula_papyrifera", "Fagus_grandifolia", "Quercus_velutina", "Prunus_serotina", "Quercus_rubra", "Acer_saccharum", "Quercus_alba")

few <- adultNoDup[!adultNoDup$speciesname %in% a.lot.ht, ] 

alot <- adultNoDup[adultNoDup$speciesname %in% a.lot.ht, ] #769505

# now sampling for species with a lot of data
ht <- data.frame(speciesname = character(), height = numeric())
species <- unique(alot$speciesname)

for (sp in 1: length(species)){
  testsp <- subset(alot, speciesname == species[1])
  mysample <- sample_n(testsp, 5000)
  #dfadd <- data.frame(speciesname = species[sp], traitvalue = mysample)
  # mysample$speciesname <- species[sp]
  ht <- rbind(ht, mysample)
}

heightData <- rbind(few, ht)

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

## Species to plot and other plotting parameters
plot.sp <- c("Corylus_avellana", "Populus_tremula", "Acer_pseudoplatanus") 
col.sp <- c("red", "blue", "orange")

pdf(file = "HeightVisual1.pdf", width = 7, height = 6)
## par(mfrow = c(1, 3))
## Plotting
### Forcing
xrange <- seq(-2, 2, by = 0.1)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(0, 100),
     xlab = "Forcing (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.3)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 1)
axis(side = 2, at = seq(0, 100, by = 20), tcl = -.5, las = 1, cex.axis = 1)
mtext(side = 3, text = "Height, Forcing", adj = 0, cex = 1.3)
## Add species to plot
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
## Add adjusted column
    ospree.temp$forceadj <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$forceadj[j] = ospree.temp$response.time[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
    }
    print(range(ospree.temp$forceadj))
    print(range(ospree.temp$force.z))
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

pdf(file = "HeightVisual2.pdf", width = 7, height = 6)
### Chilling
xrange <- seq(-.5, 2, by = 0.1)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(0, 100),
     xlab = "Chilling (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.3)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 1)
axis(side = 2, at = seq(0, 100, by = 20), tcl = -.5, las = 1, cex.axis = 1)
mtext(side = 3, text = "Height, Chilling", adj = 0, cex = 1.3)
## Add species to plot
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
## Add adjusted column
    ospree.temp$chilladj <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$chilladj[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
    }
    print(range(ospree.temp$chilladj))
    print(range(ospree.temp$chill.z))
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

pdf(file = "HeightVisual3.pdf", width = 7, height = 6)
### Photoperiod
xrange <- seq(-1, 2, by = 0.1)
plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(0, 100),
     xlab = "Photoperiod (z-scored)", ylab = "Day of phenological event",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.3)
axis(side = 1, at = seq(min(xrange), max(xrange), by = .5), tcl = -.5, cex.axis = 1)
axis(side = 2, at = seq(0, 100, by = 20), tcl = -.5, las = 1, cex.axis = 1)
mtext(side = 3, text = "Height, Photoperiod", adj = 0, cex = 1.3)
## Add species to plot
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
## Add adjusted column
    ospree.temp$photoadj <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$photoadj[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j]
    }
    print(range(ospree.temp$photoadj))
    print(range(ospree.temp$photo.z))
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
