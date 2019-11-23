## 4 November 2019 - by Cat
# Let's double check the new climate data
## Build fake data with list of more coordinates and years to better compare climate datasets 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/chilling/princetontest") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/chilling/princetontest")
}else 
  setwd("~/Documents/git/ospree/analyses/chilling/princetontest")


## Load Libraries
library(ggplot2)
library(dplyr)
library(egg)
library(RColorBrewer)
library(maptools)
library(grid)


d<-read.csv("livnehvprinceton_fakecoords.csv", header=TRUE)
d$long <- as.numeric(gsub("^\\S+\\s+|\\s+\\S+$", "", d$id))
d$lat <- as.numeric(gsub(" .*","",d$id))

#Using GGPLOT, plot the Base World Map
mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35)) # create a layer of borders
site<-d%>%dplyr::select(lat, long, gdd_prince, gdd_liv)
site<-site[!duplicated(site),]
site$gdd_diff <- site$gdd_prince - site$gdd_liv
site <- site[!is.na(site$gdd_diff),]
myPalette <- colorRampPalette(brewer.pal(9, "Spectral")) #### Gives us a heat map look
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-1000, 1200)) ### this is the range of budburst data we have
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
diff <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) + ### This creates the base map
  geom_jitter(width=3,aes(x=site$long, y=site$lat, col=site$gdd_diff), size=1.2) + theme_classic() + ### this removes extra background features from ggplot2
  coord_cartesian(ylim=c(30,60),xlim=c(-125, -60))+  ### zooms in on N. America
  theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill="grey95")) + ### to make sure the continent doesn't blend in with the ocean
  sc + labs(color="GDD diff (princeton - livneh)") + ggtitle("")


quartz()
diff

