# rm(list=ls())
# options(stringsAsFactors = FALSE)
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
#     posterior_sla <- extract(readRDS(file = "../../data/Ospree_traits/SLA_stanfit.RDS"))
# } else{
    # traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
    # traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
    # ospree <- read.csv("input/bbstan_allspp_utah.csv", stringsAsFactors = FALSE, header = TRUE)
    posterior_sla <- rstan::extract(readRDS(file = "output/SLA_stanfit_37spp.RDS"))
#     posterior_slaOld <- extract(readRDS(file = "output/SLA_stanfit.RDS"))
#     
# }
# 
# traitsData <- rbind(traitsData1,traitsData2)
# 
# traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")
# # 
# # traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")
# 
# # Subset data to traitors species list
# traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

# SLA trait only
slaData <- traitsData[traitsData$traitname == "Specific_leaf_area", ]

# Read Ospree data and subset
ospree$speciesname <- paste(ospree$genus, ospree$species, sep = "_")
ospreeData <- subset(ospree, ospree$speciesname %in% traitors.sp)

# Sorted species and study list
specieslist <- sort(unique(slaData$speciesname))
studylist <- sort(unique(slaData$datasetid))


colnames(posterior_sla$alphaPhenoSp) <- specieslist
colnames(posterior_sla$alphaForceSp) <- specieslist
colnames(posterior_sla$alphaChillSp) <- specieslist
colnames(posterior_sla$alphaPhotoSp) <- specieslist

    
## Obtain mean effect of forcing, chilling, photoperiod, interaction
forceeff <- apply(posterior_sla$betaForceSp, MARGIN = 2, FUN = mean)
chilleff <- apply(posterior_sla$betaChillSp, MARGIN = 2, FUN = mean)
photoeff <- apply(posterior_sla$betaPhotoSp, MARGIN = 2, FUN = mean)
mugrandeff <- apply(posterior_sla$mu_grand_sp, MARGIN = 2, FUN = mean)
betaTraitForceeff <- mean(posterior_sla$betaTraitxForce) #-0.2087023
betaTraitChilleff <- mean(posterior_sla$betaTraitxChill) #-0.3064649
betaTraitPhotoeff <- mean(posterior_sla$betaTraitxPhoto) # -0.1439842

# forceeff.26 <- apply(posterior_slaOld$betaForceSp, MARGIN = 2, FUN = mean)
# chilleff.26 <- apply(posterior_slaOld$betaChillSp, MARGIN = 2, FUN = mean)
# photoeff.26 <- apply(posterior_slaOld$betaPhotoSp, MARGIN = 2, FUN = mean)
# mugrandeff.26 <- apply(posterior_slaOld$mu_grand_sp, MARGIN = 2, FUN = mean)
# betaTraitForceeff.26 <- mean(posterior_slaOld$betaTraitxForce) # -0.2053369
# betaTraitChilleff.26 <- mean(posterior_slaOld$betaTraitxChill) #-0.2446075
# betaTraitPhotoeff.26 <- mean(posterior_slaOld$betaTraitxPhoto) #-0.1767991

## Species to plot and other plotting parameters
plot.sp <- c( "Quercus_rubra", "Alnus_incana") 
#  col.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9))
#  colAlpha <- c(rgb(107 / 255, 142 / 255, 142 / 255, alpha = 0.8), rgb(220 / 255, 188 / 255, 188 / 255, alpha = 0.8))
# # col2.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5))
# 
#  par(bg = "white")
#  col.sp <- c("#6B8E8E","#DCBCBC")
#  col1.sp <- c( "#1D4F4F", "#8F2727")
#  col.pt <- c("#1D4F4F", "#8F2727")

# c_light <- c("#DCBCBC")
# c_light_highlight <- c("#C79999")
# c_mid <- c("#B97C7C")
# c_mid_highlight <- c("#A25050")
# c_dark <- c("#8F2727")
# c_dark_highlight <- c("#7C0000")
# 
# c_light_teal <- c("#6B8E8E")
# c_mid_teal <- c("#487575")
# c_dark_teal <- c("#1D4F4F")


#pdf(file = "figures/results_sla_37spp_ac.pdf", width = 15, height = 5)
## Plotting
### Forcing
#par(mar = c(5, 5, 2, 2), mfrow =c(1,3))
xrange <- seq(-2.5, 2.5, by = 0.25)

ospreeBB <- ospreeData
ospreeBB$forceadj1 <- ospreeBB$response.time

for(j in 1:nrow(ospreeBB)){
    ospreeBB$forceadj1[j] = ospreeBB$response.time[j] - mean(chilleff) * ospreeBB$chill.z[j] - mean(photoeff) * ospreeBB$photo.z[j]
}

plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(-20, 120),
     xlab = expression("Forcing (z-scored"*~degree*C*")"), ylab = "Day of budburst",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.75)
axis(side = 1, at = seq(-3,3, by = .5), tcl = -.5, cex.axis = 1.25, las = 2)
axis(side = 2, at = seq(-40,220, by = 20), tcl = -.5, las = 1, cex.axis = 1.25)
mtext(side = 3, text = "SLA", adj = 0, cex = 1.2)

## Add species to plot
for(i in 1:length(plot.sp)){
    stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor1[k, ] <- rnorm(n = length(xrange), mean = posterior_sla$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior_sla$alphaForceSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior_sla$sigmapheno_y[k])
        stor2[k, ] <- rnorm(n = length(xrange), mean = posterior_sla$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior_sla$betaForceSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior_sla$sigmapheno_y[k])
    }
    temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdrcde::hdr(X, prob = c(50))$hdr[1, ])
    temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdrcde::hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = col1.sp[i], border = NA)
    polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = colAlpha[i], border = NA)
}

for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
    ## Add adjusted columns
    ospree.temp$chilladj1 <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$forceadj1[j] = ospree.temp$response.time[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
    }
    points(forceadj1 ~ jitter(force.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.pt[i], cex = 1.75)
}
my.label <- paste("(d", ")", sep="")
put.fig.letter(label=my.label, location= "topleft", font=1)

# legend(-.10,115, c(expression(paste("(Acquisitive  (", italic("A. incana"), ")")),
#                    expression(paste("(Conservative  (", italic("Q. rubra"), ")"))),
#        lty = c(1,1), col = c(col1.sp[2], col1.sp[1]),
#        bty = "n", 
#        lwd = 10,  text.width = 2)

# text(0.3,117, "Full model")
# 
# legend(-1.5, 115, c("",""), lty = c(1,1), col = c(colAlpha[2], colAlpha[1]),  text.width = 3.3, bty = "n", cex = 1, lwd =10)
# text(-1,117, "Trait effect = 0")
# 

# legend(-2.5,120, c("",""), pch = c(21, 21), pt.bg = c(col1.sp[2], col1.sp[1]),  text.width = 4.8, bty = "n", cex = 1.5)
# text(-2.2,117, "Raw data")

# legend("topright", legend = c(expression(paste("(Acquisitive  (", italic("Alnus incana"), ")")),
#                               expression(paste("(Conservative  (", italic("Quercus rubra"), ")")),
#                               expression(paste("(Trait effect", " = 0", "  (50% interval)", sep = "")),
#                               expression(paste("(Full model", "  (50% interval)"))),
#        col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col1.sp, NA, NA),
#        inset = 0.02, pch = c(21, 21, 15, 15), cex = 1.75, bty = "n")

# legend("topright", legend = c(expression(paste("(Acquisitive  (", italic("Quercus ilex"), ")")),
#                               expression(paste("(Conservative  (", italic("Fagus grandifolia"), ")")),
#                               expression(paste("(Trait effect", " = 0", "  (50% interval)", sep = "")),
#                               expression(paste("(Full model", "  (50% interval)"))),
#        col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp, NA, NA),
#        inset = 0.02, pch = c(21, 21, 15, 15), cex = 0.85, bty = "n")
#dev.off()

#pdf(file = "figures/results_sla_chilling_37spp_ac.pdf", width = 7, height = 6)
## Plotting
### Chilling
par(mar = c(5, 5, 2, 2))
xrange <- seq(-2.5, 2.5, by = 0.25)
ospreeBB <- ospreeData
ospreeBB$chilladj1 <- ospreeBB$response.time
for(j in 1:nrow(ospree.temp)){
    ospree.temp$chilladj1[j] = ospree.temp$response.time[j] - forceeff * ospree.temp$force.z[j] - photoeff * ospree.temp$photo.z[j]
}

plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(min(ospreeBB$chilladj1), 120),
     xlab = expression("Chilling (z-scored"*~degree*C*")"), ylab = "Day of budburst",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.75)
axis(side = 1, at = seq(-3,3, by = 0.5), tcl = -.5, cex.axis = 1.25, las = 2)
axis(side = 2, at = seq(-20,200, by = 20), tcl = -.5, las = 1, cex.axis = 1.25)
#mtext(side = 3, text = "SLA", adj = 0, cex = 1.2)
## Add species to plot
for(i in 1:length(plot.sp)){
    stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor1[k, ] <- rnorm(n = length(xrange), mean = posterior_sla$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior_sla$alphaChillSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior_sla$sigmapheno_y[k])
        stor2[k, ] <- rnorm(n = length(xrange), mean = posterior_sla$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior_sla$betaChillSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior_sla$sigmapheno_y[k])
    }
    temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdrcde::hdr(X, prob = c(50))$hdr[1, ])
    temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdrcde::hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = col1.sp[i], border = NA)
    polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = colAlpha[i], border = NA)
}
for(i in 1:length(plot.sp)){
    ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
    ## Add adjusted columns
    ospree.temp$chilladj1 <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$chilladj1[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
    }
    points(chilladj1 ~ jitter(chill.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.pt[i], cex = 1.75)
}
my.label <- paste("(e", ")", sep="")
put.fig.letter(label=my.label, location= "topleft", font=1)
# legend("topright", legend = c(expression(paste("(Acquisitive  (", italic("Quercus ilex"), ")")),
#                               expression(paste("(Conservative  (", italic("Fagus grandifolia"), ")")),
#                               expression(paste("(Trait effect", " = 0", "  (50% interval)", sep = "")),
#                               expression(paste("(Full model", "  (50% interval)"))),
#        col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp, NA, NA),
#        inset = 0.02, pch = c(21, 21, 15, 15), cex = 0.85, bty = "n")
#dev.off()


#pdf(file = "figures/results_sla_photoperiod_37spp_ac.pdf", width = 7, height = 6)
## Plotting
### Photoperiod
par(mar = c(5, 5, 2, 2))
xrange <- seq(-2.5, 2.5, by = 0.25)
ospreeBB <- ospreeData
ospreeBB$photoadj1 <- ospreeBB$response.time
for(j in 1:nrow(ospree.temp)){
    ospree.temp$photoadj1[j] = ospree.temp$response.time[j] - forceeff * ospree.temp$force.z[j] - chilleff * ospree.temp$chill.z[j]
}

plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(min(ospreeBB$photoadj1), 120),
     xlab = "Photoperiod (z-scored hours)", ylab = "Day of budburst",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.75)
axis(side = 1, at = seq(-3,3, by = 0.5), tcl = -.5, cex.axis = 1.25, las = 2)
axis(side = 2, at = seq(-20, 200, by = 20), tcl = -.5, las = 1, cex.axis = 1.25)
#mtext(side = 3, text = "SLA", adj = 0, cex = 1.2)
## Add species to plot
for(i in 1:length(plot.sp)){
    stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
    for(k in 1:4000){
        stor1[k, ] <- rnorm(n = length(xrange), mean = posterior_sla$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior_sla$alphaPhotoSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior_sla$sigmapheno_y[k])
        stor2[k, ] <- rnorm(n = length(xrange), mean = posterior_sla$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior_sla$betaPhotoSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior_sla$sigmapheno_y[k])
    }
    temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdrcde::hdr(X, prob = c(50))$hdr[1, ])
    temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdrcde::hdr(X, prob = c(50))$hdr[1, ])
    polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = col1.sp[i], border = NA)
    polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = colAlpha[i], border = NA)
}
for(i in 1:length(plot.sp)){
        ospree.temp <- subset(ospreeData, ospreeData$speciesname == plot.sp[i])
    ## Add adjusted columns
    ospree.temp$photoadj1 <- ospree.temp$response.time
    for(j in 1:nrow(ospree.temp)){
        ospree.temp$photoadj1[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j]
    }
    points(photoadj1 ~ jitter(photo.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.pt[i], cex = 1.75)
}
# legend("topright", legend = c(expression(paste("(Acquisitive  (", italic("Alnus incana"), ")")),
#                               expression(paste("(Conservative  (", italic("Quercus rubra"), ")")),
#                               expression(paste("(Trait effect", " = 0", "  (50% interval)", sep = "")),
#                               expression(paste("(Full model", "  (50% interval)"))),
#        col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col.sp[2], col.sp[1], NA, NA),
#        inset = 0.02, pch = c(21, 21, 15, 15), cex = 1, bty = "n")
my.label <- paste("(f", ")", sep="")
put.fig.letter(label=my.label, location= "topleft", font=1)
#dev.off()

# pdf("figures/sla_prior_post_dist_wide.pdf", width = 15, height = 25)
# par(mfrow = c(4,4))
# #plot priors against posterior_slas
# h1 <- hist(rnorm(1000, -15,10), col = rgb(1,0,1,1/4))
# hist(posterior_sla$muForceSp,add=T,col=rgb(0,0,1,1/4))
# 
# h1 <- hist(rnorm(1000, -15,10), col = rgb(1,0,1,1/4))
# hist(posterior_sla$muChillSp,add=T,col=rgb(0,0,1,1/4))
# 
# h1 <- hist(rnorm(1000, -15,10), col = rgb(1,0,1,1/4))
# hist(posterior_sla$muPhotoSp,add=T,col=rgb(0,0,1,1/4))
# 
# h1 <- hist(rnorm(1000, 17,5), col = rgb(1,0,1,1/4))
# hist(posterior_sla$mu_grand,add=T,col=rgb(0,0,1,1/4))
# 
# h1 <- hist(rnorm(1000,40,10), col = rgb(1,0,1,1/4))
# hist(posterior_sla$muPhenoSp,add=T,col=rgb(0,0,1,1/4))
# 
# h1 <- hist(rnorm(1000, 0,2), col = rgb(1,0,1,1/4))
# hist(posterior_sla$betaTraitxForce,add=T,col=rgb(0,0,1,1/4))
# 
# h1 <- hist(rnorm(1000, 0,2), col = rgb(1,0,1,1/4))
# hist(posterior_sla$betaTraitxChill,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000, 0,2), col = rgb(1,0,1,1/4))
# hist(posterior_sla$betaTraitxPhoto,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000, 10,5), col = rgb(1,0,1,1/4))
# hist(posterior_sla$sigma_sp,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000,5,2), col = rgb(1,0,1,1/4))
# hist(posterior_sla$sigma_study,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000, 5,2), col = rgb(1,0,1,1/4))
# hist(posterior_sla$sigma_traity,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000, 5,2), col = rgb(1,0,1,1/4))
# hist(posterior_sla$sigmaForceSp,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000, 10,5), col = rgb(1,0,1,1/4))
# hist(posterior_sla$sigmaChillSp,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000, 5,2), col = rgb(1,0,1,1/4))
# hist(posterior_sla$sigmaPhotoSp,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000, 10,5), col = rgb(1,0,1,1/4))
# hist(posterior_sla$sigmaPhenoSp,col=rgb(0,0,1,1/4),add=T)
# 
# h1 <- hist(rnorm(1000, 10,5), col = rgb(1,0,1,1/4))
# hist(posterior_sla$sigmapheno_y,add=T,col=rgb(0,0,1,1/4))
# 
# dev.off()