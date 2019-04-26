#Started April 11, 2019

#Aim is to map the trait data and see the distribution of where samples were collected from

#This code is based off of Cat's "map_studyspp.R" 

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("deirdreloughnan", getwd())>0)) { 
  setwd("~/Desktop/trait_analysis") 
} #else setwd("~/Documents/git/ospree/analyses")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(grid)
library(gridExtra)
library(maptools)
library(ggplotify)

d<-read.csv("input/BIEN_TraitData.csv")
require(reshape2)

boundars<-rgdal::readOGR("input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
require(rgdal)

head(d)

colnames(d)[colnames(d)=="scrubbed_species_binomial"] <- "spp"
d<-subset(d, select=c("spp","latitude","longitude","project_pi"))

#remove the data that doesn't include lat & long (a lot) and duplicates
d <- d[ complete.cases(d), ]
d<-d[!duplicated(d),]

head(d)
d$numspp<-ave(d$spp, d$project_pi, FUN=length)
d$numspp<-as.numeric(d$numspp)

noas<-subset(d, spp !="Acer saccharum")
nopa<-subset(noas, spp !="Picea abies")


library(maptools)
# mapWorld = st_read("input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
# head(mapWorld)
mapWorld<-fortify(boundars)
my.pal<-rep(brewer.pal(n=12, name="Set3"),7)
my.pch<-rep(c(4,6,8), each=5)

pdf("map.pdf", width=10)

mp <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2)  +
  geom_jitter(width=1.5,aes(x=d$longitude, y=d$latitude, color=d$spp, size=as.factor(d$numspp))) + theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.position=c(0.075, 0.163),
        #legend.position = "none",
        legend.key = element_rect(fill="white", color="white"),
        legend.box.background = element_rect(fill="white"),legend.text = element_text(size=7), legend.key.size = unit(0.3,"cm"),
        legend.title = element_text(size=8))+
  guides(color=FALSE, shape=FALSE)  +
  scale_colour_manual(name="DatasetID", values=my.pal,
                      labels=sort(unique(d$spp))) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sort(unique(unique(d$spp)))) +
  scale_size_manual(values=c(1,2,3,4,5,6), labels = c("1","2-5","6-10","11-30","31-100",">100"), name="Number of Observations") +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-125, 190), ylim=c(-55, 100))
mp

dev.off()

###########################################

length(unique(d$spp))
my.pch<-rep(c(4,6,8), each=5)
pdf("map2.pdf", width=10)
?brewer.pal
mp <- ggplot() +
  geom_polygon(aes(x = mapWorld$long, y = mapWorld$lat, group = mapWorld$group),
               color = 'gray', fill="lightgrey", size = .2)  +
  geom_jitter(width=1,aes(x=d$longitude, y=d$latitude, color=d$spp)) + theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        #legend.position=c(0.075, 0.163),
        #legend.position = "none",
        #legend.key = element_rect(fill="white", color="white"),
       # legend.box.background = element_rect(fill="white"),legend.text = element_text(size=7), legend.key.size = unit(0.3,"cm"),
        legend.title = element_text(size=8))+
  guides(color=FALSE, shape=FALSE)  +
  # scale_colour_manual(name="DatasetID",
  #                     labels=sort(unique(d$spp))) + scale_shape_manual(name="DatasetID", values=my.pch, labels=sort(unique(unique(d$spp)))) +
  # scale_size_manual(values=c(1,2,3,4,5,6), labels = c("1-5","6-10","11-20","21-30","31-100",">100"), name="Number of Observations") +
  xlab("") + ylab("") + coord_cartesian(xlim=c(-125, 190), ylim=c(-55, 100))
mp

dev.off()

#######################
library(maps)
library(mapdata)
box()

map(database = 'world', col="gray90", resolution = 10,fill=TRUE, xlim=c(-141,70), ylim=c(-50,100) )

points(d$longitude,
       d$latitude,
       pch = 19,
       cex = 1,
       col = 'darkgreen'
)

install.packages("bcmaps")
install.packages('bcmapsdata', repos='https://bcgov.github.io/drat/')

library(bcmaps)
available_layers()

library(sf)

bc <- bc_bound()
plot(st_geometry(bc))

map("world",interior=TRUE,resolution=1,ylim=c(40,56),xlim=c(-128,-104)) #by changing ylim and xlim can give range of lats & longs
map.axes()
for(i in 17:32){points(dat$LON[i],dat$LAT[i], pch=19,cex=0.2)}
for(i in 17:32){floating.pie(dat$LON[i],dat$LAT[i],as.numeric(dat[i,3:4]),radius=0.7, col=c("grey90","grey40"))}
#segments(-110,45,-115,40) #this is pretty much torture. taking to illustrator, when I need a break from the other things!
# dev.off()
# pdf("NApies.pdf")

lon<-c(-126.89,-122.1353,-120.8558) #fake longitude vector
lat<-c(54.89, 52.1269,49.115) #fake latitude vector
#converting your points
coord<-cbind(lon,lat)
library(maps)
library(mapdata)
pdf("map_test.pdf")
map("world",interior=TRUE,resolution=1,ylim=c(45,60),xlim=c(-140,-115), fill=TRUE,col="grey80") #by changing ylim and xlim can give range of lats & longs
map.axes()

points(coord,
       pch=19,cex=2, col="darkgreen")

dev.off()






lon<-c(-126.89,-122.1353,-120.8558, -72.1899, -71.1250,-71.4326,-74.0248) #fake longitude vector
lat<-c(54.89, 52.1269,49.115,42.5315, 44.9142,44.1644, 45.9310) #fake latitude vector
#converting your points
coord<-cbind(lon,lat)
# library(maps)
# library(mapdata)
pdf("map_test_2.pdf")
map("world",interior=TRUE,resolution=1,ylim=c(40,75),xlim=c(-140,-50), fill=TRUE,col="grey80") #by changing ylim and xlim can give range of lats & longs
map.axes()

points(coord,
       pch=19,cex=1.5, col="darkgreen")

dev.off()
