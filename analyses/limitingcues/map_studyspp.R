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
sp$numspp<-ifelse(sp$numspp>30 & sp$numspp<=100, 5, sp$numspp)
sp$numspp<-ifelse(sp$numspp>100, 6, sp$numspp)

my.pal<-rep(brewer.pal(n=12, name="Set3"),7)
my.pch<-rep(c(4,6,8,15:18), each=12)


mapWorld <- borders("world", colour="gray94", fill="gray92") # create a layer of borders
mp <- ggplot(sp, aes(x=long, y=lat, color=datasetID, size=as.factor(numspp), shape=datasetID)) +   mapWorld +
  theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'white'),
        legend.position=c(0.00, 0.16),
        #legend.position = "none",
        legend.key = element_rect(fill="white"),
        legend.box.background = element_rect(fill="white"),legend.text = element_text(size=7), legend.key.size = unit(0.3,"cm"),
        legend.title = element_text(size=8)) + geom_jitter(width=1.5,aes(color=datasetID, size=as.factor(numspp), shape=datasetID)) +
  guides(color=FALSE, shape=FALSE)  +
  scale_colour_manual(name="DatasetID", values=my.pal,
                      labels=sort(unique(sp$datasetID))) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sort(unique(sp$datasetID))) +
  scale_size_manual(values=c(1,2,3,4,5,6), labels = c("1-5","6-10","11-20","21-30","31-100",">100"), name="Number of Species") +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-125, 190), ylim=c(-55, 100))

#g_legend<-function(a.gplot){
#  tmp <- ggplot_gtable(ggplot_build(a.gplot))
#  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
 # legend <- tmp$grobs[[leg]]
#  return(legend)}

#datasets<-g_legend(mp)

#quartz()
#pdf("limitingcues/figures/maps/map_studyspp_legend.pdf", 
#    width=convertWidth(sum(datasets$width), "in", valueOnly=TRUE),
#    height=convertHeight(sum(datasets$heights), "in", valueOnly=TRUE))
#grid.draw(datasets)
#dev.off()

euro<-ggplot(sp, aes(x=long, y=lat, color=datasetID, size=as.factor(numspp), shape=datasetID)) +   mapWorld +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'white'),
        legend.position="none",
        legend.key = element_rect(fill="white"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,-0.5), "lines")) +
        panel_border(colour = "black", size=1, remove=FALSE) + geom_jitter(width=1.5, aes(color=datasetID, size=as.factor(numspp), shape=datasetID)) +
  guides(color=FALSE, shape=FALSE)  +
  scale_colour_manual(name="DatasetID", values=my.pal,
                      labels=sort(unique(sp$datasetID))) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sort(unique(sp$datasetID))) +
  scale_size_manual(values=c(1,2,3,4,5,6), labels = c("1-5","6-10","11-20","21-30","31-100",">100"), name="Number of Species") +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-13,40), ylim=c(34,72)) + labs(x=NULL, y=NULL)

vp <- viewport(width = 0.25, height = 0.4, x = 0.864, y = 0.32)
#Just draw the plot twice
quartz()
print(mp)
print(euro, vp = vp)



