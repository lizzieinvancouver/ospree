## Map showing OSPREE data for Limiting Cues paper
# Goal of the map is to introduce readers to the data
# Have datasetID color coded and number of species by size
# Cat - 31 Aug 2018

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(grid)
library(gridExtra)
library(maptools)
library(ggplotify)

## a bunch of this code is taken from cleaning/cleanup_checksmaps.R
# Get packages
d<-read.csv("output/ospree_clean_withchill_BB.csv")

d<-subset(d, select=c("datasetID", "genus", "species", "provenance.lat", "provenance.long"))
d<-d[!duplicated(d),]

d$complex<-paste(d$genus, d$species, sep="_")
d$numspp<-ave(d$complex, d$datasetID, FUN=length)
d$numspp<-as.numeric(d$numspp)

d$provenance.long<-ifelse(d$datasetID=="charrier11" & d$provenance.lat==45.77220, 3.147220, d$provenance.long)
d<-na.omit(d)

sp<-subset(d, select=c("datasetID", "provenance.long", "provenance.lat", "numspp"))
sp<-sp[!duplicated(sp),]

sp<-sp%>%dplyr::rename(long=provenance.long)%>%rename(lat=provenance.lat)

sp$numspp<-ifelse(sp$numspp>=1 & sp$numspp<=5, 1, sp$numspp)
sp$numspp<-ifelse(sp$numspp>5 & sp$numspp<=10, 2, sp$numspp)
sp$numspp<-ifelse(sp$numspp>10 & sp$numspp<=20, 3, sp$numspp)
sp$numspp<-ifelse(sp$numspp>20 & sp$numspp<=30, 4, sp$numspp)
sp$numspp<-ifelse(sp$numspp>30, 5, sp$numspp) #& sp$numspp<=100
#sp$numspp<-ifelse(sp$numspp>100, 6, sp$numspp)

my.pal<-rep(brewer.pal(n=12, name="Set3"),7)
my.pch<-rep(c(4,6,8,15:18), each=12)


#mapWorld <- borders("world", colour="gray94", fill="gray92") # create a layer of borders
boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
mapWorld<-fortify(boundars)
mp <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
            color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=1.5,aes(x=sp$long, y=sp$lat, color=sp$datasetID, size=as.factor(sp$numspp), shape=sp$datasetID)) + theme_classic() +
  theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.position=c(0.95, 0.85),
        #legend.position = "none",
        legend.key = element_rect(fill="white", color="white"),
        legend.box.background = element_rect(fill="white"),legend.text = element_text(size=6), legend.key.size = unit(0.1,"cm"),
        legend.title = element_text(size=8))+
  guides(color=FALSE, shape=FALSE)  +
  scale_colour_manual(name="DatasetID", values=my.pal,
                      labels=sort(unique(sp$datasetID))) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sort(unique(sp$datasetID))) +
  scale_size_manual(values=c(1,2,3,4,5), labels = c("1-5","6-10","11-20","21-30","31-100+"), name="Number of Species") +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-125, 190), ylim=c(-55, 100))


euro<-ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=1.5,aes(x=sp$long, y=sp$lat, color=sp$datasetID, size=as.factor(sp$numspp), shape=sp$datasetID)) + theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        plot.margin = unit(c(0,0,-0.5,-0.5), "lines"), panel.background = element_rect(colour="black")) + 
  guides(color=FALSE, shape=FALSE)  + 
  scale_colour_manual(name="DatasetID", values=my.pal,
                      labels=sort(unique(sp$datasetID))) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sort(unique(sp$datasetID))) +
  scale_size_manual(values=c(1,2,3,4,5), labels = c("1-5","6-10","11-20","21-30","31-100+"), name="Number of Species") +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-13,40), ylim=c(34,72)) + labs(x=NULL, y=NULL)

vp <- viewport(width = 0.25, height = 0.4, x = 0.864, y = 0.315)
#Just draw the plot twice
quartz()
print(mp)
print(euro, vp = vp)

### Now to make the legend 
d$numspp <- ifelse(d$datasetID=="basler12", 14, d$numspp)
d$datasetID <- paste(d$datasetID, d$numspp, sep="; ")

legendplot <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2) +
  geom_jitter(width=1.5,aes(x=d$provenance.long, y=d$provenance.lat, color=d$datasetID, size=as.factor(d$numspp), shape=d$datasetID)) + theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        #legend.position=c(0.95, 0.85),
        #legend.position = "none",
        legend.key = element_rect(fill="white", color="white"),
        legend.box.background = element_rect(fill="white"),legend.text = element_text(size=6), legend.key.size = unit(0.1,"cm"),
        legend.title = element_text(size=8))+
  guides(size=FALSE)  +
  scale_colour_manual(name="DatasetID", values=my.pal,
                      labels=sort(unique(d$datasetID))) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sort(unique(d$datasetID))) +
  #scale_size_manual(values=c(1,2,3,4,5), labels = c("1-5","6-10","11-20","21-30","31-100+"), name="Number of Species") +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-125, 190), ylim=c(-55, 100))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

datasets<-g_legend(legendplot)

quartz()
pdf("limitingcues/figures/maps/map_studyspp_legend.pdf", 
    width=convertWidth(sum(datasets$width), "in", valueOnly=TRUE),
    height=convertHeight(sum(datasets$heights), "in", valueOnly=TRUE))
grid.draw(datasets)
dev.off()
