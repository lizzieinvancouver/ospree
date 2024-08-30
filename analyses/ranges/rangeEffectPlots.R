# Started by Deirdre August 2024
# aim of this code is to plot the effect of including ranges size or climatvar on species estimated budburst
#reproducing similar plots to those made for the traitors ms

# rm(list=ls())
# options(stringsAsFactors = FALSE)
## Load libraries
library(rstan)

# bb.stan.nam <- read.csv("inputDL/bbStanNAm.csv")
# bb.stan.eu <- read.csv("inputDL/bbStanEuro.csv")
specieslist <- sort(unique(bb.stan.nam$latbi))

plot.sp <- c( "Robinia_pseudoacacia", "Acer_rubrum") 
col.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9))
colAlpha <- c(rgb(107 / 255, 142 / 255, 142 / 255, alpha = 0.3), rgb(220 / 255, 188 / 255, 188 / 255, alpha = 0.4))
# col2.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5))

par(bg = "white", mfrow = c(1,1), mar = c(5, 5, 2, 2))
col.sp <- c("#6B8E8E","#DCBCBC")
col1.sp <- c( "#1D4F4F", "#8F2727")
col.pt <- c("#1D4F4F", "#8F2727")

xrange <- seq(-2.5, 2.5, by = 0.25)

climVar <- c("stv", "lf", "GDD", "CP")

load("output/stv_jnt_nam.Rda")
post <- rstan::extract(stv_jnt_nam)

colnames(post$alphaPhenoSp) <- specieslist
colnames(post$alphaForcingSp) <- specieslist
colnames(post$alphaChillSp) <- specieslist
colnames(post$alphaPhotoSp) <- specieslist

## Obtain mean effect of forcing, chilling, photoperiod, interaction
forceeff <- apply(post$betaForcingSp, MARGIN = 2, FUN = mean)
chilleff <- apply(post$betaChillSp, MARGIN = 2, FUN = mean)
photoeff <- apply(post$betaPhotoSp, MARGIN = 2, FUN = mean)
# mugrandeff <- apply(post$mu_grand_sp, MARGIN = 2, FUN = mean)
betaTraitForceeff <- mean(post$betaTraitxForcing)
betaTraitChilleff <- mean(post$betaTraitxChill) 
betaTraitPhotoeff <- mean(post$betaTraitxPhoto) 

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
    stor1[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[1])] + post$alphaChillSp[k, which(specieslist == plot.sp[1])] * xrange, sd = post$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[2])] + post$alphaChillSp[k, which(specieslist == plot.sp[2])] * xrange, sd = post$sigmapheno_y[k])
  }
  temp1 <- apply(stor1, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  temp2 <- apply(stor2, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  polygon(x = c(xrange, rev(xrange)), y = c(temp1[1, ], rev(temp1[2, ])), col = colAlpha[1], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2[1, ], rev(temp2[2, ])), col = colAlpha[2], border = NA)
}

for(i in 1:length(plot.sp)){
  stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  for(k in 1:4000){
    stor1[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[1])] + post$betaChillSp[k, which(specieslist == plot.sp[1])] * xrange, sd = post$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[2])] + post$betaChillSp[k, which(specieslist == plot.sp[2])] * xrange, sd = post$sigmapheno_y[k])
  }
  temp1 <- apply(stor1, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  temp2 <- apply(stor2, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  polygon(x = c(xrange, rev(xrange)), y = c(temp1[1, ], rev(temp1[2, ])), col = col1.sp[1], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2[1, ], rev(temp2[2, ])), col = col1.sp[2], border = NA)
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

### Chilling
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
    stor1[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[1])] + post$alphaChillSp[k, which(specieslist == plot.sp[1])] * xrange, sd = post$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[2])] + post$alphaChillSp[k, which(specieslist == plot.sp[2])] * xrange, sd = post$sigmapheno_y[k])
  }
  temp1 <- apply(stor1, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  temp2 <- apply(stor2, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  polygon(x = c(xrange, rev(xrange)), y = c(temp1[1, ], rev(temp1[2, ])), col = colAlpha[1], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2[1, ], rev(temp2[2, ])), col = colAlpha[2], border = NA)
}

for(i in 1:length(plot.sp)){
  stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  for(k in 1:4000){
    stor1[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[1])] + post$betaChillSp[k, which(specieslist == plot.sp[1])] * xrange, sd = post$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[2])] + post$betaChillSp[k, which(specieslist == plot.sp[2])] * xrange, sd = post$sigmapheno_y[k])
  }
  temp1 <- apply(stor1, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  temp2 <- apply(stor2, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  polygon(x = c(xrange, rev(xrange)), y = c(temp1[1, ], rev(temp1[2, ])), col = col1.sp[1], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2[1, ], rev(temp2[2, ])), col = col1.sp[2], border = NA)
}


for(i in 1:length(plot.sp)){
  ospree.temp <- subset(bb.stan.nam, bb.stan.nam$latbi == plot.sp[i])
  for(j in 1:nrow(ospree.temp)){
    ospree.temp$chilladj1[j] = ospree.temp$response.time[j] - forceeff[which(specieslist == plot.sp[i])] * ospree.temp$force.z[j] - photoeff[which(specieslist == plot.sp[i])] * ospree.temp$photo.z[j]
  }
  points(chilladj1 ~ jitter(chill.z, factor = 0.75), data = ospree.temp, pch = 21, col = "black", bg = col.pt[i], cex = 1.75)
}

### Photoperiod

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
    stor1[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[1])] + post$alphaChillSp[k, which(specieslist == plot.sp[1])] * xrange, sd = post$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[2])] + post$alphaChillSp[k, which(specieslist == plot.sp[2])] * xrange, sd = post$sigmapheno_y[k])
  }
  temp1 <- apply(stor1, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  temp2 <- apply(stor2, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  polygon(x = c(xrange, rev(xrange)), y = c(temp1[1, ], rev(temp1[2, ])), col = colAlpha[1], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2[1, ], rev(temp2[2, ])), col = colAlpha[2], border = NA)
}

for(i in 1:length(plot.sp)){
  stor1 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  stor2 <- matrix(NA, ncol = length(xrange), nrow = 4000)
  for(k in 1:4000){
    stor1[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[1])] + post$betaChillSp[k, which(specieslist == plot.sp[1])] * xrange, sd = post$sigmapheno_y[k])
    stor2[k, ] <- rnorm(n = length(xrange), mean = post$alphaPhenoSp[k, which(specieslist == plot.sp[2])] + post$betaChillSp[k, which(specieslist == plot.sp[2])] * xrange, sd = post$sigmapheno_y[k])
  }
  temp1 <- apply(stor1, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  temp2 <- apply(stor2, MARGIN = 2, FUN = function(X) quantile(X, prob = c(0.25, 0.75)))
  polygon(x = c(xrange, rev(xrange)), y = c(temp1[1, ], rev(temp1[2, ])), col = col1.sp[1], border = NA)
  polygon(x = c(xrange, rev(xrange)), y = c(temp2[1, ], rev(temp2[2, ])), col = col1.sp[2], border = NA)
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

