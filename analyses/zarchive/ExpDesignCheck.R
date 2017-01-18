# Budburst Lit review initial look
# By Dan Flynn, notes by Lizzie (emw)

# Updated 18 January 2017 after Lizzie looked at it #

## About this file (by Lizzie): This file reads in data from Feb 2015
# and makes some simple plots of what the data look at
# Since it's so old, lots of things have changed in the datafile
# so we could update the code to make it all run, but ... 
# after looking through it I don't think it's doing anything we couldn't
# recreate quickly, when and if we need to! ##


# library(gdata) # for read.xls
library(scales) # for alpha

d <- read.csv("~/Documents/git/projects/treegarden/budreview/ospree/mergearchive/growthchambers_litreview_2016-06-16.csv", header=TRUE)

#######################
## Changes by Lizzie ##
#######################

# Dan was reading:
# d <- read.xls("~/Dropbox/Work/Harvard/budburst review/Archive/growthchambers_litreview_2015-02-02.xlsx", sheet = 6)
# As of 18 Jan 2017 we don't have this file and I cannot find a similarly named xlsx with 6 sheets! For ease I just made a csv of data_detailed the most similar recent version I could find, which was:
# growthchambers_litreview_2016-06-16.xlsx

# temp_day to forcetemp
# temp_night to forcetemp_night

tapply(d$forcetemp, d$datasetID, mean)
tapply(d$forcetemp_night, d$datasetID, mean) 

d.cutt <- d[d$material == "cuttings",]
d.cutt <- d.cutt[order(d.cutt$forcetemp),]

d.seed <- d[d$material != "cuttings",]
d.seed <- d.seed[order(d.seed$forcetemp),]

cols = alpha(c("red", "blue"), 0.5)


pdf("Lit review check.pdf", height = 8, width = 6)

par(mfrow = c(2, 1))

plot(d.cutt$forcetemp, pch = "-", col = cols[1], cex = 2, 
	ylim = c(-5, 35),
	xlab = "Study",
	ylab = "Temp")
points(d.cutt$forcetemp_night, pch = "-", col = cols[2], cex = 2)
abline(h = mean(d.cutt[d.cutt$forcetemp != d.cutt$forcetemp_night,"forcetemp"]), col = cols[1], lty = 2)
abline(h = mean(d.cutt[d.cutt$forcetemp != d.cutt$forcetemp_night,"forcetemp_night"]), col = cols[2], lty = 2)

title(main = "Day/night temps by study")
legend("topleft", bty = "n", "Cuttings")
 
plot(d.seed$forcetemp, pch = "-", col = cols[1], cex = 2,
	ylim = c(-5, 35),
	xlab = "Study",
	ylab = "Temp")
points(d.seed$forcetemp_night, pch = "-", col = cols[2], cex = 2)
abline(h = mean(d.seed[d.seed$forcetemp != d.seed$forcetemp_night,"forcetemp"]), col = cols[1], lty = 2)
abline(h = mean(d.seed[d.seed$forcetemp != d.seed$forcetemp_night,"forcetemp_night"]), col = cols[2], lty = 2)
legend("topleft", bty = "n", "Seedlings or other material")



## summary statistics

# Cuttings with day/night diff

summary(d.cutt[d.cutt$forcetemp != d.cutt$forcetemp_night,])
# day: 15, night 7

summary(d.cutt[d.cutt$forcetemp == d.cutt$forcetemp_night,])
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

