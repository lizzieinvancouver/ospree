rm(list=ls())
options(stringsAsFactors = FALSE)

require(ggbiplot)
require(gridExtra)
require(ggplot2)
require(bayesplot)
require(dplyr)

# maping packages:
library(tmap)
library(tmaptools)
library(sf)
library(raster)
library(viridis)

if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
} else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
} 

traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
ospree <- read.csv("input/bbstan_allspp_utah_37spp.csv", header = TRUE)
traitData <- rbind(traitsData1,traitsData2)

#subset to our four traits:
traitName <- c("Plant_height_vegetative", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Specific_leaf_area", "seed mass")
fourTrt <- traitData[traitData$traitname %in% traitName,]

ospree <- read.csv("input/bbstan_allspp_utah_37spp.csv", header = TRUE)

traitLL <- unique(fourTrt[,c("traitname", "latitude", "longitude")])
traitLL <- traitLL[complete.cases(traitLL),]

phenoLL <- unique(ospree[,c("provenance.lat", "provenance.long")])
phenoLL <- phenoLL[complete.cases(phenoLL),]

coord <- st_as_sf(traitLL, coords = c("longitude","latitude"), agr = "traitname", crs = 4326)
coordPheno <- st_as_sf(phenoLL, coords = c("provenance.long", "provenance.lat"), crs = 4326)

#coord$continent <- as.factor(coord$continent)
data("World")

trt_map <-  tm_shape(World)  +
  tm_polygons(col = "gray72", border.col = "gray72") +
  #tm_fill() +
  tm_shape(subset(coord, traitname == traitName[1])) +
  tm_dots(fill= "#648fff", shape = 21) +
  tm_shape(subset(coord, traitname == traitName[2])) +
  tm_dots(fill= "#785ef0",  shape = 21) +
  tm_shape(subset(coord, traitname == traitName[3])) +
  tm_dots(fill= "#dc267f",  shape = 21) + 
  tm_shape(coordPheno) +
  tm_dots(fill= "#fe6100", shape = 21) + 
tm_add_legend( "bottomleft",fill = c( "#648fff","#785ef0","#dc267f", "#fe6100"),bg = c(21,21,21,21), labels = c("LNC", "SLA", "Height", "Budburst")); trt_map


tm_shape(World)  +
  tm_polygons(col = "gray72", border.col = "gray72") +
  tm_dots(fill= "#fe6100", shape = 21) 

  tm_shape(World) +
    tm_polygons()+
    tm_shape(coord)+
    tm_dots(fill= "traitname", shape = 21)+
    tm_legend("bottomleft",show=TRUE)
  