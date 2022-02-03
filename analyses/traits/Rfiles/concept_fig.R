# started Jan 31, 2022 by Deirdre

# aim of this code is to make a conceptual figure for the traitors ms:

setwd("~/Documents/github/ospree/analyses/traits")

col.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.8), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.9))
col1.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.14), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.2))
col2.sp <- c(rgb(72 / 255, 38 / 255, 119 / 255, alpha = 0.4), rgb(149 / 255, 216 / 255, 64 / 255, alpha = 0.5))

pdf("figures/conceptFig.pdf", width = 18, height = 5)
par(mfrow=c(1,3))  
plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 35,lwd =3, lty = 2, col = col1.sp[1])
segments(x0 = -1.85, x1 = 1.85, y0 = 75, y1 = 5,lwd =3, lty = 2, col = col2.sp[2])
segments(x0 = -1.65, x1 = 1.65, y0 = 150, y1 = 17, lwd =3, lty = 1, col = col.sp[1])
segments(x0 = -1.85, x1 = 1.45, y0 = 85, y1 = 0,lwd =3, lty = 1, col = col.sp[2])

plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 45,lwd =3, lty = 2, col = col1.sp[1])
segments(x0 = -1.85, x1 = 1.85, y0 = 100, y1 = 15,lwd =3, lty = 2, col = col2.sp[2])
segments(x0 = -1.85, x1 = 1.85, y0 = 132, y1 = 47, lwd =3, lty = 1, col = col.sp[1])
segments(x0 = -1.85, x1 = 1.85, y0 = 102, y1 = 17,lwd =3, lty = 1, col = col.sp[2])

plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
segments(x0 = -1.60, x1 = 1.75, y0 = 147, y1 = 30, lwd =3, lty = 2, col = col1.sp[1])
segments(x0 = -1.85, x1 = 1.45, y0 = 90, y1 = 10,lwd =3, lty = 2, col = col2.sp[2])
segments(x0 = -1.85, x1 = 1.85, y0 = 135, y1 = 45,lwd =3, lty = 1, col = col.sp[1])
segments(x0 = -1.85, x1 = 1.85, y0 = 80, y1 = 13,lwd =3, lty = 1, col = col.sp[2])

legend("topright", legend = c(expression("Conservative"),
                              expression("Acquisitive"),
                              expression(paste("Trait effect", " = 0", sep = "")),
                              expression(paste("Full model"))),
       col = c(col.sp[1], col.sp[2],"black", "black"), pt.bg = c(col.sp, NA, NA),
       inset = 0.02, lty = c(1,1,2,1), lwd = c(3,3,1,1), cex = 2, bty = "n")

# legend("topright", legend = c(expression(paste("Low trait")),
#                               expression(paste("High trait")),
#                               expression(paste("Trait effect", " = 0", sep = "")),
#                               expression(paste("Full model"))))

dev.off()


plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
abline(50,-25, lwd = 3)
abline(70,-25, lwd = 3)
abline(52,-25, lwd = 3, lty = 2)
abline(72,-25, lwd = 3, lty = 2)
