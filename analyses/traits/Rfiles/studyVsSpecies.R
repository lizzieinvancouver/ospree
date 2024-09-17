rm(list=ls())
options(stringsAsFactors = FALSE)

library(RColorBrewer)
library(rstan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if
(length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits")
}

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


posterior_ht <- rstan::extract(readRDS(file = "output/height_stanfit_37spp.RDS"))

posterior_sla <- rstan::extract(readRDS(file = "output/SLA_stanfit_37spp.RDS"))

posterior_lnc <- rstan::extract(readRDS(file = "output/LNC_stanfit_37spp.RDS"))

posterior_sm <- rstan::extract(readRDS(file = "output/SeedMass_log10_stanfit_37spp.RDS"))


hist(posterior_ht$sigma_study)

# pdf("figures/studySpNew.pdf", width = 15, height = 4.5)
# par(mfrow = c(1,4), mar = c(5.1, 4.5, 4.1, 2.1))
# plot(density(posterior_ht$sigma_study), xlim = c(0, 20), ylim = c(0, 0.7), col = "goldenrod", lwd = 3, xlab = "Variance", ylab = "Density", cex.lab = 1.75, main = "", cex.axis = 1.5)
# lines(density(posterior_ht$sigma_sp), col = "sienna3", lwd =3)
# text(3, 0.68, "Height", cex = 2)
# my.label <- paste("a", ".", sep="")
# put.fig.letter(label=my.label, location= "topleft", font=2)
# 
# plot(density(posterior_sla$sigma_study), xlim = c(0, 20), ylim = c(0, 0.7), col = "goldenrod", lwd = 3, xlab = "Variance", ylab = "Density", cex.lab = 1.75, main = "", cex.axis = 1.5)
# lines(density(posterior_sla$sigma_sp), col = "sienna3", lwd =3)
# text(2, 0.68, "SLA", cex = 2)
# my.label <- paste("b", ".", sep="")
# put.fig.letter(label=my.label, location= "topleft", font=2)
# 
# plot(density(posterior_lnc$sigma_study), xlim = c(0, 20), ylim = c(0, 0.7), col = "goldenrod", lwd = 3, xlab = "Variance", ylab = "Density", cex.lab = 1.75, main = "", cex.axis = 1.5)
# lines(density(posterior_lnc$sigma_sp), col = "sienna3", lwd =3)
# text(2, 0.68, "LNC", cex = 2)
# my.label <- paste("c", ".", sep="")
# put.fig.letter(label=my.label, location= "topleft", font=2)
# 
# plot(density(posterior_sm$sigma_study), xlim = c(0, 20), ylim = c(0, 4.5), col = "goldenrod", lwd = 3, xlab = "Variance", ylab = "Density", cex.lab = 1.75, main = "", cex.axis = 1.5)
# lines(density(posterior_sm$sigma_sp), col = "sienna3", lwd =3)
# text(4, 4.3, "Seed Mass", cex = 2)
# my.label <- paste("d", ".", sep="")
# put.fig.letter(label=my.label, location= "topleft", font=2)
# legend("topright",legend = c("Species variation", "Study variation"),
#        col = c("sienna3", "goldenrod"),
#        lwd = 5, bty = "n", cex = 1.25)
# 
# dev.off()


# But maybe we don't want to be using a density plot, making a histogram plot instead
col1.sp <-c( rgb(204 / 255, 105 / 255, 112 / 255, alpha = 0.8)) # pink
col2.sp <- c( rgb(205 / 255, 122 / 255, 0 / 255, alpha = 0.5)) # yellow
# col3.sp <-c( rgb(0 / 255, 166 / 255, 167 / 255, alpha = 0.05))
col4.sp <- c( rgb(34 / 255, 166 / 255, 167 / 255, alpha = 0.5))
col5.sp <- c( rgb(141 / 255, 34 / 255, 171 / 255, alpha = 0.5))


pdf("figures/studySpHist.pdf", width = 15, height = 4)
par(mfrow = c(1,4), mar = c(5.1, 4.5, 4.1, 2.1))
hist(posterior_ht$sigma_study, col = col2.sp, prob = T, xlim = c(0, 18), ylim = c(0, 1.5), main = "", xlab = "Variance", ylab = "Posterior bin probabilities", cex.lab = 1.75, breaks = 25, xaxt="n", yaxt = "n")
axis(side = 1, at = seq(-20,30, by = 2), cex.axis =1.75)
axis(side = 2, at = seq(-1,2, by = 0.1), cex.axis =1.75)

hist(posterior_ht$sigma_sp, col = col1.sp, prob = T, add =T)
text(3, 1.5, "Height", cex = 2)

legend("topright",legend = c("Species variation", "Study variation"),
       col = c("sienna3", "goldenrod"),
       lwd = 5, bty = "n", cex = 1.5)

my.label <- paste("a", ".", sep="")
put.fig.letter(label=my.label, location= "topleft", font=2)

hist(posterior_sla$sigma_study, col = col2.sp, prob = T, xlim = c(0, 18), ylim = c(0, 1.5), main = "", xlab = "Variance", ylab = "Posterior bin probabilities", cex.lab = 1.75, breaks = 25, xaxt="n", yaxt = "n")
axis(side = 1, at = seq(-20,30, by = 2), cex.axis =1.75)
axis(side = 2, at = seq(-1,2, by = 0.1), cex.axis =1.75)

hist(posterior_sla$sigma_sp, col = col1.sp, prob = T, add =T, breaks = 25)
text(2, 1.5, "SLA", cex = 2)
my.label <- paste("b", ".", sep="")
put.fig.letter(label=my.label, location= "topleft", font=2)

hist(posterior_lnc$sigma_study, col = col2.sp, prob = T, xlim = c(0, 18), ylim = c(0, 1.5), main = "", xlab = "Variance", ylab = "Posterior bin probabilities", cex.lab = 1.75, breaks = 25, xaxt="n", yaxt = "n")
axis(side = 1, at = seq(-20,30, by = 2), cex.axis =1.75)
axis(side = 2, at = seq(-1,2, by = 0.1), cex.axis =1.75)

hist(posterior_lnc$sigma_sp, col = col1.sp, prob = T, add =T, breaks = 15)
text(2, 1.5, "LNC", cex = 2)
my.label <- paste("c", ".", sep="")
put.fig.letter(label=my.label, location= "topleft", font=2)

hist(posterior_sm$sigma_study, col = col2.sp, prob = T, xlim = c(0, 18), ylim = c(0, 1.5), main = "", xlab = "Variance", ylab = "Posterior bin probabilities", cex.lab = 1.75, breaks = 2, xaxt="n", yaxt = "n")
axis(side = 1, at = seq(-20,30, by = 2), cex.axis =1.75)
axis(side = 2, at = seq(-1,2, by = 0.1), cex.axis =1.75)

hist(posterior_sm$sigma_sp, col = col1.sp, prob = T, add =T, breaks = 5)
text(4, 1.5, "Seed mass", cex = 2)
my.label <- paste("d", ".", sep="")
put.fig.letter(label=my.label, location= "topleft", font=2)

# legend("topright",legend = c("Species variation", "Study variation"),
#        col = c("sienna3", "goldenrod"),
#        lwd = 5, bty = "n", cex = 1.5)
dev.off()

