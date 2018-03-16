## Taken from wine.maps.networks.R ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/teaching/misc/networks/arcs")

library(maptools)
library(mapdata)
library(geosphere)

## get a file
allmatched <- read.csv("winenetworks.csv", header=TRUE)
dater <- read.csv("dater.csv", header=TRUE)

##
##
## map of great arc links -- everything, pretty messy
wrld <- map('world',col='gray90',fill=TRUE)
inter2 <- gcIntermediate(c(allmatched$long[1], allmatched$lat[1]), c(allmatched$long_org[1], allmatched$lat_org[1]), n=50, addStartEnd=TRUE)
lines(inter2, col="red")

for (i in c(1:nrow(allmatched))){
    inter2 <- gcIntermediate(c(allmatched$long[i], allmatched$lat[i]),
        c(allmatched$long_org[i], allmatched$lat_org[i]), n=50, addStartEnd=TRUE)
     lines(inter2, col="red")
}


## trying some stuff to improve visualization
allmatched$pertotal <- allmatched$hectare/sum(dater$hectare)
allmatched <- subset(allmatched, pertotal>0.00001) # from 17,000 to 4,700 (to 1,460 if you use 0.0001)
allmatched <- allmatched[order(allmatched$hectare),]

## better version of the above -- it removes some stuff and plots the smaller hecatres first and lighter
# get some colors
pal <- colorRampPalette(c("#f2f2f2", "black"))
pal <- colorRampPalette(c("#f2f2f2", "red"))
colors <- pal(5000)

wrld <- map('world',col='gray90',fill=TRUE)

for (i in c(1:nrow(allmatched))){
    inter2 <- gcIntermediate(c(allmatched$long[i], allmatched$lat[i]),
        c(allmatched$long_org[i], allmatched$lat_org[i]), n=100, addStartEnd=TRUE)
    lines(inter2, col=colors[i], lwd=0.8)
}
