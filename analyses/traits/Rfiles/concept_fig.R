# started Jan 31, 2022 by Deirdre

# aim of this code is to make a conceptual figure for the traitors ms:

#setwd("~/Documents/github/ospree/analyses/traits")

col.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9))
col1.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.14), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.2))
col2.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5))

# pdf("figures/conceptFig.pdf", width = 18, height = 5)
# par(bg = "pink", mfrow = c(1,3))
plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2)
segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 35,lwd =5, lty =2, col = col1.sp[1])
segments(x0 = -1.85, x1 = 1.85, y0 = 75, y1 = 5,lwd =5, lty =2, col = col2.sp[2])
segments(x0 = -1.65, x1 = 1.65, y0 = 150, y1 = 17, lwd =5, lty = 1, col = col.sp[1])
segments(x0 = -1.85, x1 = 1.45, y0 = 85, y1 = 0,lwd =5, lty = 1, col = col.sp[2])

my.label <- paste("a", ".", sep="")
put.fig.letter(label=my.label, location= "topleft", font=2)

plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2)
segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 45,lwd =5, lty =2, col = col1.sp[1])
segments(x0 = -1.85, x1 = 1.85, y0 = 100, y1 = 15,lwd =5, lty =2, col = col2.sp[2])
segments(x0 = -1.85, x1 = 1.85, y0 = 134, y1 = 49, lwd =5, lty = 1, col = col.sp[1])
segments(x0 = -1.85, x1 = 1.85, y0 = 104, y1 = 19,lwd =5, lty = 1, col = col.sp[2])

my.label <- paste("b", ".", sep="")
put.fig.letter(label=my.label, location= "topleft", font=2)

plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2)
segments(x0 = -1.60, x1 = 1.75, y0 = 147, y1 = 30, lwd =5, lty = 2, col = col2.sp[2])
segments(x0 = -1.85, x1 = 1.45, y0 = 90, y1 = 10,lwd =5, lty = 2, col = col2.sp[1])
segments(x0 = -1.85, x1 = 1.85, y0 = 135, y1 = 45,lwd =5, lty = 1, col = col.sp[2])
segments(x0 = -1.85, x1 = 1.85, y0 = 80, y1 = 13,lwd =5, lty = 1, col = col.sp[1])

legend("topright", legend = c(expression("Conservative"),
                              expression("Acquisitive"),
                              expression(paste("Trait effect", " = 0", sep = "")),
                              expression(paste("Full model"))),
       col = c(col.sp[1], col.sp[2],col2.sp[1], col2.sp[2]), pt.bg = c(col.sp, NA, NA),
       inset = 0.02, lty = c(1,1,3,3), lwd = c(5,5,5,5), cex = 1, bty = "n")

my.label <- paste("c", ".", sep="")
put.fig.letter(label=my.label, location= "topleft", font=2)

# legend("topright", legend = c(expression(paste("Low trait")),
#                               expression(paste("High trait")),
#                               expression(paste("Trait effect", " = 0", sep = "")),
#                               expression(paste("Full model"))))

#dev.off()
par(bg = "white")  



# Let's make the conceptual figures intervals: This seems like a hyper inefficient way
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
# segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 35,lwd =3, lty = 1, col = col1.sp[1])
# polygon(x = c(-1.85,1.85,  1.8, -2),                           # X-Coordinates of polygon
#         y = c(130,35, 31,132),                             # Y-Coordinates of polygon
#         col = col.sp[1], border = NA)     
# 
# segments(x0 = -1.85, x1 = 1.85, y0 = 75, y1 = 5,lwd =3, lty = 1, col = col2.sp[2])
# segments(x0 = -1.65, x1 = 1.65, y0 = 150, y1 = 17, lwd =3, lty = 1, col = col.sp[1])
# segments(x0 = -1.85, x1 = 1.45, y0 = 85, y1 = 0,lwd =3, lty = 1, col = col.sp[2])
# 
# xrange <- seq(-2.5, 2.5, by = 0.25)
# yrange <- seq(30,50, length =21)
# 
# 
# polygon(x = c(xrange, rev(xrange)), y = c(yrange, rev(yrange)), col = "red", border = NA)
# 
# plot(1, 1, col = "white", xlab = "X", ylab = "Y")  
# polygon(x = c(0.7, 1.3, 1.2, 0.8),                           # X-Coordinates of polygon
#         y = c(0.6, 0.8, 1.4, 1),                             # Y-Coordinates of polygon
#         col = "#1b98e0")       

# par(mfrow = c(2,3))
# ## Take 2 of the conceptual fig:
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2, main = "spp. diff indep of cue or trait")
# segments(x0 = -1.85, x1 = 1.85, y0 = 107, y1 = 107,lwd =5, lty =2, col = col1.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 75, y1 = 75,lwd =5, lty =2, col = col2.sp[2])
# segments(x0 = -1.85, x1 = 1.85, y0 = 110, y1 = 110, lwd =5, lty = 1, col = col.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 78, y1 = 78,lwd =5, lty = 1, col = col.sp[2])
# 
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2, main = "spp. diff w. cue indpe of trait")
# segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 45,lwd =5, lty =2, col = col1.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 100, y1 = 15,lwd =5, lty =2, col = col2.sp[2])
# segments(x0 = -1.85, x1 = 1.85, y0 = 134, y1 = 49, lwd =5, lty = 1, col = col.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 104, y1 = 19,lwd =5, lty = 1, col = col.sp[2])
# 
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2, main = "traits cause cues to advance bb")
# segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 35,lwd =5, lty =2, col = col1.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 75, y1 = 5,lwd =5, lty =2, col = col2.sp[2])
# segments(x0 = -1.65, x1 = 1.65, y0 = 150, y1 = 17, lwd =5, lty = 1, col = col.sp[1])
# segments(x0 = -1.85, x1 = 1.45, y0 = 85, y1 = 0,lwd =5, lty = 1, col = col.sp[2])
# 
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2, main = "traits cause cues to delay bb")
# segments(x0 = -1.60, x1 = 1.75, y0 = 147, y1 = 30, lwd =5, lty = 2, col = col2.sp[1])
# segments(x0 = -1.85, x1 = 1.45, y0 = 90, y1 = 10,lwd =5, lty = 2, col = col2.sp[2])
# segments(x0 = -1.85, x1 = 1.85, y0 = 135, y1 = 45,lwd =5, lty = 1, col = col.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 80, y1 = 13,lwd =5, lty = 1, col = col.sp[2])
# 
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2, main = "traits opposite of predictions")
# segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 35, lwd =5, lty = 2, col = col2.sp[2])
# segments(x0 = -1.85, x1 = 1.85, y0 = 75, y1 = 5,lwd =5, lty = 2, col = col2.sp[1])
# segments(x0 = -1.65, x1 = 1.65, y0 = 150, y1 = 17,lwd =5, lty = 1, col = col.sp[2])
# segments(x0 = -1.85, x1 = 1.45, y0 = 85, y1 = 0,lwd =5, lty = 1, col = col.sp[1])
# 
# plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150), cex =2, cex.axis = 1.5, cex.lab = 1.2, main = "traits + cue = interxn with growth strat")
# segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 35,lwd =5, lty =2, col = col1.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 95, y1 = 25,lwd =5, lty =2, col = col2.sp[2])
# segments(x0 = -1.65, x1 = 1.45, y0 = 150, y1 = 7, lwd =5, lty = 1, col = col.sp[1])
# segments(x0 = -1.85, x1 = 1.85, y0 = 105, y1 = 20,lwd =5, lty = 1, col = col.sp[2])
