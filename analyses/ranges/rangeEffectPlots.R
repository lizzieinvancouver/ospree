# Started by Deirdre August 2024
# aim of this code is to plot the effect of including ranges size or climatvar on species estimated budburst
#reproducing similar plots to those made for the traitors ms

# rm(list=ls())
# options(stringsAsFactors = FALSE)
## Load libraries
library(rstan)

bb.stan.nam <- read.csv("inputDL/bbStanNAm.csv")
bb.stan.eu <- read.csv("inputDL/bbStanEuro.csv")

load("output/stv_jnt.nam.Rda")
posterior_sla <- rstan::extract(stv_jnt.nam)


specieslist <- sort(unique(bb.stan.nam$latbi))

colnames(posterior_sla$alphaPhenoSp) <- specieslist
colnames(posterior_sla$alphaForcingSp) <- specieslist
colnames(posterior_sla$alphaChillSp) <- specieslist
colnames(posterior_sla$alphaPhotoSp) <- specieslist


## Obtain mean effect of forcing, chilling, photoperiod, interaction
forceeff <- apply(posterior_sla$betaForcingSp, MARGIN = 2, FUN = mean)
chilleff <- apply(posterior_sla$betaChillSp, MARGIN = 2, FUN = mean)
photoeff <- apply(posterior_sla$betaPhotoSp, MARGIN = 2, FUN = mean)
# mugrandeff <- apply(posterior_sla$mu_grand_sp, MARGIN = 2, FUN = mean)
betaTraitForceeff <- mean(posterior_sla$betaTraitxForcing) #-0.2087023
betaTraitChilleff <- mean(posterior_sla$betaTraitxChill) #-0.3064649
betaTraitPhotoeff <- mean(posterior_sla$betaTraitxPhoto) # -0.1439842


## Species to plot and other plotting parameters
# as an illustrative example, I subsetted the data to the min and max STV
# Robinia	pseudoacacia	
plot.sp <- c( "Robinia_pseudoacacia", "Acer_rubrum") 
col.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9))
colAlpha <- c(rgb(107 / 255, 142 / 255, 142 / 255, alpha = 0.8), rgb(220 / 255, 188 / 255, 188 / 255, alpha = 0.8))
# col2.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5))

par(bg = "white")
col.sp <- c("#6B8E8E","#DCBCBC")
col1.sp <- c( "#1D4F4F", "#8F2727")
col.pt <- c("#1D4F4F", "#8F2727")

xrange <- seq(-2.5, 2.5, by = 0.25)


for(j in 1:nrow(bb.stan.nam)){
  bb.stan.nam$forceadj1[j] = bb.stan.nam$response.time[j] - mean(chilleff) * bb.stan.nam$chill.z[j] - mean(photoeff) * bb.stan.nam$photo.z[j]
}

plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(0, 160),
     xlab = expression("Forcing (z-scored"*~degree*C*")"), ylab = "Day of budburst",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.5)
axis(side = 1, at = seq(-3,3, by = .5), tcl = -.5, cex.axis = 1.25, las = 2)
axis(side = 2, at = seq(-20,220, by = 20), tcl = -.5, las = 1, cex.axis = 1.25)
mtext(side = 3, text = "STV", adj = 0, cex = 1.2)

## Add species to plot
for(i in 1:length(plot.sp)){
  stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  for(k in 1:4000){
    stor1[k, ] <- rnorm(n = length(xrange), mean = posterior_sla$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior_sla$alphaForcingSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior_sla$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = posterior_sla$alphaPhenoSp[k, which(specieslist == plot.sp[i])] + posterior_sla$betaForcingSp[k, which(specieslist == plot.sp[i])] * xrange, sd = posterior_sla$sigmapheno_y[k])
  }
  temp1.hdr <- apply(stor1, MARGIN = 2, FUN = function(X) hdrcde::hdr(X, prob = c(50))$hdr[1, ])
  temp2.hdr <- apply(stor2, MARGIN = 2, FUN = function(X) hdrcde::hdr(X, prob = c(50))$hdr[1, ])
  polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = colAlpha[i], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = col1.sp[i], border = NA)
}

for(i in 1:length(plot.sp)){
  ospree.temp <- subset(bb.stan.nam, bb.stan.nam$latbi == plot.sp[i])
  for(j in 1:nrow(ospree.temp)){
    ospree.temp$forceadj1[j] = ospree.temp$response.time[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
  }
  points(forceadj1 ~ jitter(force.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.pt[i], cex = 1.75)
}
#my.label <- paste("a", ".", sep="")
#put.fig.letter(label=my.label, location= "topleft", font=2)
legend("topright", legend = c(expression(paste("Small range  (", italic("Robinia pseudoacacia"), ")")),
                              expression(paste("Large range  (", italic("Acer rubrum"), ")")),
                              expression(paste("Range effect", " = 0", "  (50% interval)", sep = "")),
                              expression(paste("Full model", "  (50% interval)"))),
       col = c("black", "black", rgb(0, 0, 0, alpha = 0.18), rgb(0, 0, 0, alpha = 0.85)), pt.bg = c(col1.sp[1], col1.sp[2], NA, NA),
       inset = 0.02, pch = c(21, 21, 15, 15), cex = 0.85, bty = "n")
#dev.off()

#pdf(file = "figures/results_sla_chilling_37spp_ac.pdf", width = 7, height = 6)
## Plotting
### Chilling
par(mar = c(5, 5, 2, 2))
xrange <- seq(-2.5, 2.5, by = 0.25)

for(j in 1:nrow(bb.stan.nam)){
  bb.stan.nam$chilladj1[j] = bb.stan.nam$response.time[j] - mean(forceeff) * bb.stan.nam$force.z[j] - mean(photoeff) * bb.stan.nam$photo.z[j]
}

plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(min(bb.stan.nam$chilladj1), 160),
     xlab = expression("Chilling (z-scored"*~degree*C*")"), ylab = "Day of budburst",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.5)
axis(side = 1, at = seq(-3,3, by = 0.5), tcl = -.5, cex.axis = 1.25, las = 2)
axis(side = 2, at = seq(-20,200, by = 20), tcl = -.5, las = 1, cex.axis = 1.25)
mtext(side = 3, text = "STV", adj = 0, cex = 1.2)
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
  polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = colAlpha[i], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = col1.sp[i], border = NA)
}

for(i in 1:length(plot.sp)){
  ospree.temp <- subset(bb.stan.nam, bb.stan.nam$latbi == plot.sp[i])
  for(j in 1:nrow(ospree.temp)){
    ospree.temp$chilladj1[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
  }
  points(chilladj1 ~ jitter(chill.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.pt[i], cex = 1.75)
}

### Photoperiod
par(mar = c(5, 5, 2, 2))
xrange <- seq(-2.5, 2.5, by = 0.25)

for(j in 1:nrow(bb.stan.nam)){
  bb.stan.nam$photoadj1[j] = bb.stan.nam$response.time[j] - mean(forceeff) * bb.stan.nam$force.z[j] - mean(chilleff) * bb.stan.nam$chill.z[j]
}

plot(NA, xlim = c(min(xrange), max(xrange)), ylim = c(min(bb.stan.nam$photoadj1), 160),
     xlab = "Photoperiod (z-scored hours)", ylab = "Day of budburst",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.5)
axis(side = 1, at = seq(-3,3, by = 0.5), tcl = -.5, cex.axis = 1.25, las = 2)
axis(side = 2, at = seq(-20, 200, by = 20), tcl = -.5, las = 1, cex.axis = 1.25)
mtext(side = 3, text = "SLA", adj = 0, cex = 1.2)
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
  polygon(x = c(xrange, rev(xrange)), y = c(temp1.hdr[1, ], rev(temp1.hdr[2, ])), col = colAlpha[i], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2.hdr[1, ], rev(temp2.hdr[2, ])), col = col1.sp[i], border = NA)
}
for(i in 1:length(plot.sp)){
  ospree.temp <- subset(bb.stan.nam, bb.stan.nam$latbi == plot.sp[i])
  ## Add adjusted columns
  ospree.temp$photoadj1 <- ospree.temp$response.time
  for(j in 1:nrow(ospree.temp)){
    ospree.temp$photoadj1[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - chilleff[which(specieslist == plot.sp[i])] * ospree.temp$chill.z[j]
  }
  points(photoadj1 ~ jitter(photo.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.pt[i], cex = 1.75)
}
my.label <- paste("c", ".", sep="")
put.fig.letter(label=my.label, location= "topleft", font=2)
#dev.off()

