## Started 28 August 2017 ##
## By Lizzie ##

## Working on figures for OSPREE concept paper on interactive cues ##

## housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else
  print("set your working directory")

# Get packages
library(ggplot2)
library(rgdal)
library(RColorBrewer) 
library(plyr)
library(dplyr)

## a bunch of this code is taken from cleaning/cleanup_checksmaps.R
# Get packages
d <- read.csv("output/ospree_clean.csv") 

d <- d[d$woody=="yes",]
head(d)

yr <- as.numeric(d$year)

pdf("interactivecues/figures/pubyear.pdf", width = 8, height = 5)
hist(yr, breaks = 50, xaxt="n", col = "lightblue", main = "Years of Publication")
axis(1, at = seq(min(yr, na.rm=T), max(yr, na.rm=TRUE), by = 3))
dev.off()

# Counting
d$latbi <- paste(d$genus, d$species)
length(unique(d$latbi)) # unique species
d$datasetIDstudy <- paste(d$datasetID, d$study)
length(unique(d$datasetIDstudy)) # unique studies

# ranges
range(as.numeric(as.character(d$forcetemp)), na.rm=TRUE) 
range(as.numeric(as.character(d$chilltemp)), na.rm=TRUE)
range(as.numeric(as.character(d$photoperiod_day)), na.rm=TRUE)
hist(as.numeric(as.character(d$photoperiod_day)),
     main = "Photoperiod frequency", xlab = "Hour"
     )

##
## get map shape file (see map_notes.R for details)
##

wmap <- readOGR("..//..//..//..//general/maps/ne_110m_land") # on Lizzie's computer
wmap.df <- fortify(wmap)

##
## maps
##

length(unique(d$datasetID))
length(unique(paste(d$datasetID, d$study)))

length(unique(paste(d$genus, d$species)))
sort(summary(factor(paste(d$genus, d$species))), decreasing = TRUE)

unique(d$respvar)
sort(summary(d$respvar))

# summary data by study, with n responses (from Dan F.)
nd <- data.frame(n=tapply(d$datasetID, d$datasetID, length))
nd$lat <- as.numeric(as.character(d[match(rownames(nd), d$datasetID), "provenance.lat"]))
nd$long <- as.numeric(as.character(d[match(rownames(nd), d$datasetID), "provenance.long"]))
nd$resp <- d[match(rownames(nd), d$datasetID), "respvar"]

sppsummmap <-
      ddply(d, c("datasetID", "provenance.lat", "provenance.long"), summarise,
      n=length(unique(latbi)),
      photo=mean(as.numeric(photoperiod_day)))

theme.tanmap <- list(theme(panel.grid.minor = element_blank(),
                        # panel.grid.major = element_blank(),
                        panel.background = element_rect(fill = "grey90",colour = NA),
                        # plot.background = element_rect(fill=NA),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22),
                        legend.position = "left"))

pdf("interactivecues/figures/maps/mappy.pdf", width=12, height=4)

ggplot() + 
    geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
    # coord_cartesian(ylim=c(5, 70), xlim=c(-120, 40)) +
    geom_point(data=sppsummmap, 
        aes(x=provenance.long, y=provenance.lat, size=sqrt(n), fill=photo), 
        colour="black", pch=21) +
        scale_size(name=expression(paste('species ', italic('n')))) +
            # range = c(1, 12), breaks=c(10,100,200,400
            scale_fill_gradient(name="mean photoperiod",
            low="thistle2", high="dodgerblue4", guide="colorbar", na.value="NA") +
  theme.tanmap
# low: thistle2 or pink or papayawhip khaki1?
# high: darkred
dev.off()

