# started Jan 31, 2022 by Deirdre

# aim of this code is to make a conceptual figure for the traitors ms:

pdf("conceptFig.pdf", width = 4, height = 8)
par(mfrow=c(3,1))  
plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 25,lwd =3, lty = 2, col = "darkgreen")
segments(x0 = -1.85, x1 = 1.85, y0 = 100, y1 = 5,lwd =3, lty = 2, col = "purple")
segments(x0 = -1.85, x1 = 1.85, y0 = 110, y1 = 37, lwd =3, lty = 1, col = "darkgreen")
segments(x0 = -1.85, x1 = 1.85, y0 = 80, y1 = 80,lwd =3, lty = 1, col = "purple")

plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
segments(x0 = -1.85, x1 = 1.85, y0 = 50,lwd =3, lty = 2, col = "darkgreen")
segments(x0 = -1.85, x1 = 1.85, y0 = 70,lwd =3, lty = 2, col = "purple")
segments(x0 = -1.85, x1 = 1.85, y0 = 52,lwd =3, lty = 1,  col = "darkgreen")
segments(x0 = -1.85, x1 = 1.85, y0 = 72,lwd =3, lty = 1, col = "purple")

plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
segments(x0 = -1.85, x1 = 1.85, y0 = 130, y1 = 45,lwd =3, lty = 2, col = "darkgreen")
segments(x0 = -1.85, x1 = 1.85, y0 = 100, y1 = 15,lwd =3, lty = 2, col = "purple")
segments(x0 = -1.85, x1 = 1.85, y0 = 132, y1 = 47, lwd =3, lty = 1, col = "darkgreen")
segments(x0 = -1.85, x1 = 1.85, y0 = 102, y1 = 17,lwd =3, lty = 1, col = "purple")
dev.off()
# legend("topright", legend = c(expression(paste("Low trait")),
#                               expression(paste("High trait")),
#                               expression(paste("Trait effect", " = 0", sep = "")),
#                               expression(paste("Full model")))) 


plot(0, type="n", ylab="Day of budburst", xlab = "Cue", xlim =c(-2,2), ylim =c(0, 150))
abline(50,-25, lwd = 3)
abline(70,-25, lwd = 3)
abline(52,-25, lwd = 3, lty = 2)
abline(72,-25, lwd = 3, lty = 2)
