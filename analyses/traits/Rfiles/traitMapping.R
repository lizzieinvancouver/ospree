rm(list=ls())
options(stringsAsFactors = FALSE)

#require(ggbiplot)
# require(gridExtra)
require(ggplot2)
# require(bayesplot)
# require(dplyr)

# maping packages:
# library(tmap)
# library(tmaptools)
library(rnaturalearth)
library(rnaturalearthdata)

library(sf)
# library(raster)
library(viridis)



if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
} else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
} 

traitsData1 <- read.csv("input/try_bien_nodups_1.csv", stringsAsFactors = FALSE)
traitsData2 <- read.csv("input/try_bien_nodups_2.csv", stringsAsFactors = FALSE)
ospree <- read.csv("input/bbstan_allspp_utah_37spp.csv", header = TRUE)
traitData <- rbind(traitsData1,traitsData2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")

traitData  <- subset(traitData , traitData$speciesname %in% traitors.sp)

#subset to our four traits:
traitName <- c("Plant_height_vegetative", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Specific_leaf_area", "seed mass")
fourTrt <- traitData[traitData$traitname %in% traitName,]

traitLL <- fourTrt[,c("traitname", "latitude", "longitude")]
traitLL <- traitLL[complete.cases(traitLL),]
 
phenoLL <- ospree[,c("provenance.lat", "provenance.long")]
phenoLL <- phenoLL[complete.cases(phenoLL),]
phenoLL$traitname <- "Budburst"
colnames(phenoLL)[colnames(phenoLL) == "provenance.lat"] <- "latitude"
colnames(phenoLL)[colnames(phenoLL) == "provenance.long"] <- "longitude"

trtLL <- unique(rbind(traitLL, phenoLL))
# 
# coord <- st_as_sf(traitLL, coords = c("longitude","latitude"), agr = "traitname", crs = 4326)
# coordPheno <- st_as_sf(phenoLL, coords = c("provenance.long", "provenance.lat"), crs = 4326)

world <- ne_countries(scale = "medium", returnclass = "sf")

pdf("figures/traitMap.pdf", width = 8, height =4)
mapPlot <- ggplot(data = world) +
  geom_sf(color = "black", fill = "white") +
 geom_point(data = trtLL, mapping = aes(y = latitude, x = longitude, fill = traitname), pch = 21) + 
  labs(fill = "") +
  xlab ("") + ylab ("") +
  theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(1, "cm"),
    legend.position=c(.1,.4), legend.text = element_text(size = 15)) + 
    scale_fill_manual(values = c("Plant_height_vegetative"="#648fff",
    "Leaf_nitrogen_.N._content_per_leaf_dry_mass"="#B12A90FF",
    "Specific_leaf_area"= "#E16462FF",
    "Budburst"="#FCA636FF"),
    breaks = c("Plant_height_vegetative", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Specific_leaf_area", "Budburst"), label = c("Height", "LNC", "SLA", "Budburst"),guide = guide_legend(override.aes = list(size = 3)))
mapPlot
dev.off()


# #coord$continent <- as.factor(coord$continent)
# data("World")
# 
# pdf("figures/traitMap.pdf", width = 8, height =4)
# tm_shape(World)  +
#   tm_polygons(fill = "gray72" , border.col = "gray72") +
#   tm_legend(show = T) +
#  # tm_borders(col = "gray72")+ 
#   tm_shape(subset(coord, traitname == traitName[1])) +
#   tm_dots(fill= "#648fff", shape = 21) +
#   tm_shape(subset(coord, traitname == traitName[2])) +
#   tm_dots(fill= "#785ef0",  shape = 21) +
#   tm_shape(subset(coord, traitname == traitName[3])) +
#   tm_dots(fill= "#dc267f",  shape = 21) + 
#   tm_shape(coordPheno) +
#   tm_dots(fill= "#FFB000", shape = 21) + 
#   tm_layout(legend.outside = F, scale =1.25) +
# tm_add_legend("topright", fill = c( "#648fff","#785ef0","#dc267f",  "#FFB000"),bg = c(21,21,21,21), labels = c(expression("Height ("*italic("n")*"= 42781)"), expression("LNC ("*italic("n")*"= 3853)"), expression("LNC ("*italic("n")*"= 7656)"), expression("Budbusrt ("*italic("n")*"= 1670)")), orientation ="landscape", frame = F,legend.width =108 ,legend.text.size =30)
# dev.off()
# 
# s = tm_shape(World, crs = "+proj=eqearth")
# s + tm_polygons(
#   fill = "HPI",
#   fill.scale = tm_scale_continuous(values = "pu_gn"),
#   fill.legend = 
#     tm_legend(
#       title = "Happy Planex Index", 
#       orientation = "landscape", 
#       width = 60))
# 
# tm_shape(World)  +
#   tm_polygons(col = "gray72", border.col = "gray72") +
#   tm_dots(fill= "#fe6100", shape = 21) 
# 
#   tm_shape(World) +
#     tm_polygons()+
#     tm_dots(col = coord)
#   
#   
# tm_shape(World)  +
#     tm_polygons(fill = "gray72" , border.col = "gray72") +
#     tm_legend(show = T) +
#     # tm_borders(col = "gray72")+ 
#     tm_dots(subset(coord, traitname == traitName[2]), fill = "#648fff") +
#   tm_dots(subset(coord, traitname == traitName[2]), fill = "#785ef0") +
#   tm_dots(subset(coord, traitname == traitName[3]), fill = "#dc267f") +
#   tm_dots(coordPheno,fill= "#fe6100", shape = 21) + 
#   tm_layout(legend.outside = F) +
#   tm_add_legend("topright", fill = c( "#648fff","#785ef0","#dc267f", "#fe6100"),bg = c(21,21,21,21), labels = c("Leaf nitrogen content", "Specific leaf area", "Height", "Budburst"), orientation ="landscape", frame = F, height =3)

    
  