# started Jan 31, 2022 by Deirdre
# rm(list=ls())
# options(stringsAsFactors = FALSE)

# aim of this code is to make a conceptual figure for the traitors ms:

#setwd("~/Documents/github/ospree/analyses/traits")

# col.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9))
# col1.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.14), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.2))
# col2.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5))

col.sp <- c("#1D4F4F","#8F2727")
col1.sp <- c("#6B8E8E")
col2.sp <- c("#DCBCBC")

put.fig.letter <- function(label, location="topleft", x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  if(length(label) > 1) {
    warning("length(label) > 1, using label[1]")
  }
  if(is.null(x) | is.null(y)) {
    coords <- switch(location,
                     topleft = c(0.05,0.98),
                     topcenter = c(0.5525,0.98),
                     topright = c(0.985, 0.98),
                     bottomleft = c(0.015, 0.02), 
                     bottomcenter = c(0.5525, 0.02), 
                     bottomright = c(0.985, 0.02),
                     c(0.015, 0.98) )
  } else {
    coords <- c(x,y)
  }
  this.x <- grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- grconvertY(coords[2] + offset[2], from="nfc", to="user")
  text(labels=label[1], x=this.x, y=this.y, xpd=T, cex = 2, ...)
}

# pdf("figures/conceptFig.pdf", width = 18, height = 5)
# par(bg = "pink", mfrow = c(1,3))
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2)
# segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 35,lwd =5, lty =2, col = col1.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 75, y1 = 5,lwd =5, lty =2, col = col2.sp[2])
# segments(x0 = -1.65, x1 = 1.65, y0 = 150, y1 = 17, lwd =5, lty = 1, col = col.sp[1])
# segments(x0 = -1.85, x1 = 1.45, y0 = 85, y1 = 0,lwd =5, lty = 1, col = col.sp[2])
# 
# my.label <- paste("a", ".", sep="")
# put.fig.letter(label=my.label, location= "topleft", font=2)
# 
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2)
# segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 45,lwd =5, lty =2, col = col1.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 100, y1 = 15,lwd =5, lty =2, col = col2.sp[2])
# segments(x0 = -1.85, x1 = 1.85, y0 = 134, y1 = 49, lwd =5, lty = 1, col = col.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 104, y1 = 19,lwd =5, lty = 1, col = col.sp[2])
# 
# my.label <- paste("b", ".", sep="")
# put.fig.letter(label=my.label, location= "topleft", font=2)
# 
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2)
# segments(x0 = -1.60, x1 = 1.75, y0 = 147, y1 = 30, lwd =5, lty = 2, col = col2.sp[2])
# segments(x0 = -1.85, x1 = 1.45, y0 = 90, y1 = 10,lwd =5, lty = 2, col = col2.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 135, y1 = 45,lwd =5, lty = 1, col = col.sp[2])
# segments(x0 = -1.85, x1 = 1.85, y0 = 80, y1 = 13,lwd =5, lty = 1, col = col.sp[1])
# 
# legend("topright", legend = c(expression("Conservative"),
#                               expression("Acquisitive"),
#                               expression(paste("Trait effect", " = 0", sep = "")),
#                               expression(paste("Full model"))),
#        col = c(col.sp[1], col.sp[2],col2.sp[1], col2.sp[2]), pt.bg = c(col.sp, NA, NA),
#        inset = 0.02, lty = c(1,1,3,3), lwd = c(5,5,5,5), cex = 1, bty = "n")
# 
# my.label <- paste("c", ".", sep="")
# put.fig.letter(label=my.label, location= "topleft", font=2)
# 
# # legend("topright", legend = c(expression(paste("Low trait")),
# #                               expression(paste("High trait")),
# #                               expression(paste("Trait effect", " = 0", sep = "")),
# #                               expression(paste("Full model"))))
# 
# #dev.off()
# par(bg = "white")  


# Trying simulations to generate figures:
# My general thoughts is that we would want to show a high and a low trait indiviudal, one for which the trait effect is set to zero and one where it varies:

# The stan model has two parts:
# 1. Trait part:
# for(i in 1:n_spec){
#   mu_grand_sp[i] = mu_grand + muSp[i];
# }
# for (i in 1:N){
#   y_hat[i] = mu_grand + muSp[trait_species[i]] + muStudy[study[i]];
# }
#
# 2. Phenology part 
# for (isp in 1:n_spec){
#    betaChillSp[isp] = alphaChillSp[isp] + betaTraitxChill * (mu_grand_sp[isp]);
# }
# for (i in 1:Nph){
#   yPhenoi[i] ~ normal(alphaPhenoSp[phenology_species[i]] + betaForceSp[phenology_species[i]] * forcei[i] + betaPhotoSp[phenology_species[i]] * photoi[i] + betaChillSp[phenology_species[i]] * chilli[i], sigmapheno_y);
# }

# For these conceptual figures, we want one cue on x-axis and the respons (day of bb) on the y, I am focusing on chilling effects, so other cues are kept constant.
# the effect of different trait values comes into play in the diff of betaTraitxChill, and this value would be set to zero for the dashed lines
# For traits like height, we would expect tall trees to bb earlier with more chilling, which would require a larger trait effect - ie larger values of betaTraitxChill

# Define constant parameters:
# alphaChillSp <- -0.5
# mu_grand_sp <- 5
# betaTraitxChill <- c(-0.5,-1, 0, 0.5, 1) # Start with a positive value, I also want to try: negative values, larger values, smaller values
# 
# alphaPhenoSp <- 5
# betaForceSp <- 5
# forcei <- 2
# 
# betaPhotoSp <- 1
# photoi <- 3
# 
# chilli <- rnorm(2000, 20, 10) #x-axis
# # parameters to simulate/that vary:
# 
# colors <- c("#cc6a70ff", "#7e4e90ff","#55C667FF","#287D8EFF", "cyan4")
# 
# plot(chilli,yPhenoi, type = "n", xlim = c(-15,15), ylim =c(-150, 100))
# 
# for (b in 1:length(betaTraitxChill)){
# betaChillSp = alphaChillSp + betaTraitxChill[b] * mu_grand_sp
# 
# yPhenoi <- alphaPhenoSp + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSp * chilli
# 
# points(chilli,yPhenoi, type = "l", col= colors[b], lwd = 3)
# }
# 
# legend("bottomleft",legend = c(expression("betaTraitxChill = -0.5"),
#                             expression("betaTraitxChill = -1"),
#                             expression("betaTraitxChill = 0"),
#                             expression("betaTraitxChill = 0.5"),
#                             expression("betaTraitxChill = 1")),
#        #col = c("black", "black", "black", "black","black"),
#        #pt.bg = c("#042333ff","#cc6a70ff","#593d9cff","#f9b641ff","#13306dff","#efe350ff","#eb8055ff"),
#        col = c( "#cc6a70ff", "#7e4e90ff","#55C667FF","#287D8EFF", "cyan4"),
#        inset = 0.02, pch = c(21, 21, 21, 21,21 ), cex = 0.75, bty = "n")

#par(mfrow = c(1,3))
## Ok, if the above is correct, then yes the intercepts can vary
#pdf("figures/conceptualFigure3.pdf", width = 12, height = 5)
layout(matrix(c(1,4,7,2,5,8,3,6,9), nrow = 3, ncol = 3))

alphaChillSp <- -2
mu_grand_sp <- c(2,10)
#betaTraitxChill <- c(-0.5, -1, 0, 0.5, 1) # Start with a positive value, I also want to try: negative values, larger values, smaller values
betaTraitxChillT0 <- 0

alphaPhenoSp <- c(10,50)
betaForceSp <- 5
forcei <- 2

betaPhotoSp <- 1
photoi <- 3

chilli <- rnorm(2000, 20, 10) #x-axis
# parameters to simulate/that vary:

#colors <- c("#cc6a70ff", "#7e4e90ff","#55C667FF","#287D8EFF", "cyan4")

#plot(chilli,yPhenoi, type = "n", xlim = c(-15,15), ylim =c(-150, 100))
#par(mfrow = c(1,3))

betaTraitxChill <- c(-0.45, -0.85, 0, 0.45, 0.85)
  b <- 1
  gm <- 1
  a <- 1
  
  betaChillSp = alphaChillSp + betaTraitxChill[b] * mu_grand_sp[gm]
  yPhenoi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSp * chilli
  
  betaChillSpT0 = alphaChillSp + betaTraitxChillT0 * mu_grand_sp[gm]
  yPhenoiT0 <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSpT0 * chilli
  
  b<- 2
  gm <- 1
  a <- 2
  betaChillSpHi = alphaChillSp + betaTraitxChill[b] * mu_grand_sp[gm]
  yPhenoiHi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSpHi * chilli
  
  betaChillSpT0Hi = alphaChillSp + betaTraitxChillT0 * mu_grand_sp[gm]
  yPhenoiT0Hi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSpT0 * chilli
 
  plot(chilli,yPhenoi, type = "n", xlim = c(-11,17), ylim =c(0, 100), ylab = "Day of budburst", xlab = "Standardized cue", cex.lab = 1.5, cex.axis = 1.25)
  my.label <- paste("a", ".", sep="")
  put.fig.letter(label=my.label, location= "topleft", font=2)
  u <- par("usr") # The coordinates of the plot area
  rect(u[1], u[3], u[2], u[4], col="grey88", border=NA)
  
  
  points(chilli,yPhenoi, type = "l", col= col.sp[1], lwd = 3)
  points(chilli,yPhenoiT0, type = "l", col= col1.sp, lwd = 3)
  points(chilli,yPhenoiHi, type = "l", col= col.sp[2], lwd = 3)
  points(chilli,yPhenoiT0Hi, type = "l", col= col2.sp, lwd = 3)
  
  legend("topright",legend = c(expression("Conservative (- betaTraitxCue)"),
                               expression("Conservative (trait effect = 0)"),
                               expression("Acquisitive (- betaTraitxCue)"),
                               expression("Acquisitive (trait effect = 0)")),
         
         #col = c("black", "black", "black", "black"),
         #pt.bg = c("#042333ff","#cc6a70ff","#593d9cff","#f9b641ff","#13306dff","#efe350ff","#eb8055ff"),
         col = c(col.sp[1],col1.sp,col.sp[2],col2.sp[1]),
         inset = 0.02, lty = c(1,1,1,1), lwd = c(5,5,5,5), cex = 1, bty = "n")
  
## Now the slope is 0
  b <- 3
  gm <- 1
  a <- 1
  
  betaChillSp = alphaChillSp + betaTraitxChill[b] * mu_grand_sp[gm]
  yPhenoi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSp * chilli
  
  betaChillSpT0 = alphaChillSp + betaTraitxChillT0 * mu_grand_sp[gm]
  yPhenoiT0 <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSpT0 * chilli
  
  gm <- 1
  a <- 2
  betaChillSpHi = alphaChillSp + betaTraitxChill[b] * mu_grand_sp[gm]
  yPhenoiHi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSp * chilli
  
  betaChillSpT0Hi = alphaChillSp + betaTraitxChillT0 * mu_grand_sp[gm]
  yPhenoiT0Hi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSpT0 * chilli
  
  plot(chilli,yPhenoi, type = "n", xlim = c(-11,17), ylim =c(0, 100), ylab = "Day of budburst", xlab = "Standardized cue", cex.lab = 1.5, cex.axis = 1.25)
  my.label <- paste("b", ".", sep="")
  put.fig.letter(label=my.label, location= "topleft", font=2)
  u <- par("usr") # The coordinates of the plot area
  rect(u[1], u[3], u[2], u[4], col="grey88", border=NA)
  
  
  points(chilli,yPhenoiT0, type = "l", col= col.sp[1], lwd = 3)
  points(chilli,yPhenoi, type = "l", col= col1.sp, lwd = 3)
  points(chilli,yPhenoiT0Hi, type = "l", col= col.sp[2], lwd = 3)
  points(chilli,yPhenoiHi, type = "l", col= col2.sp, lwd = 3)
  
  # points(chilli,yPhenoiT0, type = "l", col= "mediumpurple1", lwd = 3)
  # points(chilli,yPhenoi, type = "l", col= "purple4", lwd = 3)
  # points(chilli,yPhenoiT0Hi, type = "l", col= "palegreen3", lwd = 3)
  # points(chilli,yPhenoiHi, type = "l", col= "forestgreen", lwd = 3)
  # 
  legend("topright",legend = c(expression("Conservative (betaTraitxCue = 0)"),
                               expression("Conservative (trait effect = 0)"),
                               expression("Acquisitive (betaTraitxCue = 0)"),
                               expression("Acquisitive (trait effect = 0)")),
         
         #col = c("black", "black", "black", "black"),
         #pt.bg = c("#042333ff","#cc6a70ff","#593d9cff","#f9b641ff","#13306dff","#efe350ff","#eb8055ff"),
         col = c(col.sp[1],col1.sp,col.sp[2],col2.sp[1]),
         inset = 0.02, lty = c(1,1,1,1), lwd = c(5,5,5,5), cex = 1, bty = "n")
  
  ## Now the slope is +ve
  betaTraitxChill <- c(-0.45, -0.85, 0, 0.2,0.4)
  b <- 5
  gm <- 1
  a <- 1
  
  betaChillSp = alphaChillSp + betaTraitxChill[b] * mu_grand_sp[gm]
  yPhenoi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSp * chilli
  
  betaChillSpT0 = alphaChillSp + betaTraitxChillT0 * mu_grand_sp[gm]
  yPhenoiT0 <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSpT0 * chilli
  
 
  b <- 4
  gm <- 1
  a <- 2
  betaChillSpHi = alphaChillSp + betaTraitxChill[b] * mu_grand_sp[gm]
  yPhenoiHi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSpHi * chilli
  
  betaChillSpT0Hi = alphaChillSp + betaTraitxChillT0 * mu_grand_sp[gm]
  yPhenoiT0Hi <- alphaPhenoSp[a] + betaForceSp * forcei + betaPhotoSp * photoi + betaChillSpT0 * chilli
  
  plot(chilli,yPhenoi, type = "n", xlim = c(-10,10), ylim =c(0, 100), ylab = "Day of budburst", xlab = "Standardized cue", cex.lab = 1.5, cex.axis = 1.25)
  my.label <- paste("c", ".", sep="")
  put.fig.letter(label=my.label, location= "topleft", font=2)
  u <- par("usr") # The coordinates of the plot area
  rect(u[1], u[3], u[2], u[4], col="grey88", border=NA)
  
  points(chilli,yPhenoi, type = "l", col= col.sp[1], lwd = 3)
  points(chilli,yPhenoiT0, type = "l", col= col1.sp, lwd = 3)
  points(chilli,yPhenoiHi, type = "l", col= col.sp[2], lwd = 3)
  points(chilli,yPhenoiT0Hi, type = "l", col= col2.sp, lwd = 3)
  # 
  # points(chilli,yPhenoi, type = "l", col= "purple4", lwd = 3, lty = 1)
  # points(chilli,yPhenoiT0, type = "l", col= "mediumpurple1", lwd = 3)
  # points(chilli,yPhenoiHi, type = "l", col= "forestgreen", lwd = 3)
  # points(chilli,yPhenoiT0Hi, type = "l", col= "palegreen3", lwd = 3)
  
  legend("topright",legend = c(expression("Conservative (+ betaTraitxCue)"),
                               expression("Conservative (trait effect = 0)"),
                               expression("Acquisitive (+ betaTraitxCue)"),
                               expression("Acquisitive (trait effect = 0)")),
         
         #col = c("black", "black", "black", "black"),
         #pt.bg = c("#042333ff","#cc6a70ff","#593d9cff","#f9b641ff","#13306dff","#efe350ff","#eb8055ff"),
         col = c(col.sp[1],col1.sp,col.sp[2],col2.sp[1]),
         inset = 0.02, lty = c(1,1,1,1), lwd = c(5,5,5,5), cex = 1, bty = "n")

#dev.off()
  
#   x <- seq(1,100, 5)
#   y <- seq(1,100, 5)
#   
#   pdf("defenceConcept.pdf", width = 8 , height = 7)
#   plot(x,y, type = "n", xlim = c(0,15), ylim =c(-30, 5), ylab = "Species-level cue slope", xlab = "Trait value", cex.lab = 1.8, xaxt = "n", yaxt = "n")
#   
#   abline(a= -5, b = -0.8, col = "cyan4", lwd =5)
#   abline(a= -12, b = -0.8, col = "goldenrod", lwd =5)
#   abline(a= -17, b = -0.8, col = "#CC6677", lwd =5)
#   
#   legend("topright",legend = c(expression("Chilling"),
#                             expression("Forcing"),
#                             expression("Photoperiod")),
#          col = c("#CC6677", "goldenrod","cyan4"), lty = c(1,1,1,1), lwd = c(5,5,5,5), cex = 2, bty = "n")
# dev.off()
                              
         