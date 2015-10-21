# Budburst Lit review analysis.
# started for 'data_simple', now working with data_detailed from the main xlsx sheet

library(gdata) # for read.xls
library(scales) # for alpha

#d <- read.xls("~/Dropbox/Work/Harvard/budburst review/Archive/growthchambers_litreview_2015-09-25.xlsx", sheet = 6)

setwd("~/Documents/git/budreview")
d <- read.csv("growthchambers_litreview_2015-09-25.csv")

tapply(d$temp_day, d$datasetID, mean)
tapply(d$temp_night, d$datasetID, mean) 

d.cutt <- d[d$material == "cuttings",]
d.cutt <- d.cutt[order(d.cutt$temp_day),]

d.seed <- d[d$material != "cuttings",]
d.seed <- d.seed[order(d.seed$temp_day),]

cols = alpha(c("red", "blue"), 0.5)


pdf("Lit review check.pdf", height = 8, width = 6)

par(mfrow = c(2, 1))

plot(d.cutt$temp_day, pch = "-", col = cols[1], cex = 2, 
	ylim = c(-5, 35),
	xlab = "Study",
	ylab = "Temp")
points(d.cutt$temp_night, pch = "-", col = cols[2], cex = 2)
abline(h = mean(d.cutt[d.cutt$temp_day != d.cutt$temp_night,"temp_day"]), col = cols[1], lty = 2)
abline(h = mean(d.cutt[d.cutt$temp_day != d.cutt$temp_night,"temp_night"]), col = cols[2], lty = 2)

title(main = "Day/night temps by study")
legend("topleft", bty = "n", "Cuttings")
 
plot(d.seed$temp_day, pch = "-", col = cols[1], cex = 2,
	ylim = c(-5, 35),
	xlab = "Study",
	ylab = "Temp")
points(d.seed$temp_night, pch = "-", col = cols[2], cex = 2)
abline(h = mean(d.seed[d.seed$temp_day != d.seed$temp_night,"temp_day"]), col = cols[1], lty = 2)
abline(h = mean(d.seed[d.seed$temp_day != d.seed$temp_night,"temp_night"]), col = cols[2], lty = 2)
legend("topleft", bty = "n", "Seedlings or other material")



## summary statistics

# Cuttings with day/night diff

summary(d.cutt[d.cutt$temp_day != d.cutt$temp_night,])
# day: 15, night 7

summary(d.cutt[d.cutt$temp_day == d.cutt$temp_night,])
# day & night 16.5

# Daylength
d.cutt <- d.cutt[order(d.cutt$photoperiod_day),]
d.seed <- d.seed[order(d.seed$photoperiod_day),]
cols = alpha(c("red", "blue"), 0.5)

par(mfrow = c(2, 1))

plot(d.cutt$photoperiod_day, pch = "-", col = cols[1], cex = 2, 
	ylim = c(0, 24),
	xlab = "Study",
	ylab = "Daylength")
#points(d.cutt$photoperiod_night, pch = "-", col = cols[2], cex = 2)
abline(h = mean(d.cutt[,"photoperiod_day"],na.rm=T), col = cols[1], lty = 2)

title(main = "Daylength by study")
legend("topleft", bty = "n", "Cuttings")
 
plot(d.seed$photoperiod_day, pch = "-", col = cols[1], cex = 2, 
	ylim = c(0, 24),
	xlab = "Study",
	ylab = "Daylength")
#points(d.seed$photoperiod_night, pch = "-", col = cols[2], cex = 2)
abline(h = mean(d.seed[,"photoperiod_day"],na.rm=T), col = cols[1], lty = 2)
legend("topleft", bty = "n", "Seedlings or other material")

dev.off()

# Short days: 10 hr, 8:00 to 16:00
# long days: 12 hr, 7:00 to 19:00

# constant over experiemnt


# SH: 5 degrees cooler in winter, 2 degrees cooler in summer
# temp end may: 
# Warm: 10 - 20

# temp mid may
# Cool: 7 - 17
# early may: 5 - 15

