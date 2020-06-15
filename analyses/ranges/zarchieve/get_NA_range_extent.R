### extract the same range parameters for north American species than was done in get_range_extent.R for Europe
##Started  1 June 2020 by Da



# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses/ranges/") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/ranges/")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges/") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges/") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges/") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

library(rgdal)
library(maps)
library(broom)
library(ggplot2)
library("raster")

Acepen <- readOGR(dsn = "NA_range_files/NA_ranges/acerpens/acerpens.shp",verbose = FALSE) 
Acerub <- readOGR(dsn = "NA_range_files/NA_ranges/acerrubr/acerrubr.shp",verbose = FALSE)
Acesac <- readOGR(dsn = "NA_range_files/NA_ranges/acersacr/acersacr.shp",verbose = FALSE)
Alnrub <- readOGR(dsn = "NA_range_files/NA_ranges/alnurubr/alnurubr.shp",verbose = FALSE)
Alninc <- readOGR(dsn = "NA_range_files/NA_ranges/alnurugo/alnurugo.shp",verbose = FALSE)
Betall<- readOGR(dsn = "NA_range_files/NA_ranges/betualle/betualle.shp",verbose = FALSE)
Betlen<- readOGR(dsn = "NA_range_files/NA_ranges/betulent/betulent.shp",verbose = FALSE)
Betpap<- readOGR(dsn = "NA_range_files/NA_ranges/betupapy/betupapy.shp",verbose = FALSE)
Corcor<- readOGR(dsn = "NA_range_files/NA_ranges/corycorn/corycorn.shp",verbose = FALSE)
Faggra<- readOGR(dsn = "NA_range_files/NA_ranges/fagugran/fagugran.shp",verbose = FALSE)
Franig<- readOGR(dsn = "NA_range_files/NA_ranges/fraxnigr/fraxnigr.shp",verbose = FALSE)
Picgla<- readOGR(dsn = "NA_range_files/NA_ranges/piceglau/piceglau.shp",verbose = FALSE)
Popgra<- readOGR(dsn = "NA_range_files/NA_ranges/popugran/popugran.shp",verbose = FALSE)
Prupen<- readOGR(dsn = "NA_range_files/NA_ranges/prunpens/prunpens.shp",verbose = FALSE)
Psumen<- readOGR(dsn = "NA_range_files/NA_ranges/pseumenz/pseumenz.shp",verbose = FALSE)
Querub<- readOGR(dsn = "NA_range_files/NA_ranges/querrubr/querrubr.shp",verbose = FALSE)  

listtostore<-c(Acepen,Acerub,Acesac,Faggra,Querub,Betall,Betlen,Betpap,Alnrub,Alninc,Corcor,Picgla,Psumen,Franig,Prupen,Popgra)
rangeofrange<-data.frame(min.x=numeric(),max.x=numeric(),min.y=numeric(),max.y=numeric())


##have nacho check the projection below to make sure it is compatible with Europe, Dan thinks its wrong
#but doesnt ahve the climate data to check
for(i in c(1:length(listtostore))){
  proj4string(listtostore[[i]]) <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs  ")
  min.x<-xmin(listtostore[[i]])
  max.x<-ymax(listtostore[[i]])
  min.y<-ymin(listtostore[[i]])
  max.y<-ymax(listtostore[[i]])
  dfhere <- data.frame(min.x=min.x,max.x=max.x,min.y=min.y,max.y=max.y)
  rangeofrange <- rbind(rangeofrange, dfhere) ## rbind it here for safty
}
rangeofrange$species=c("Acer_pensylvanicum" ,
                       "Acer_rubrum", "Acer_saccharum","Fagus_grandifolia","Quercus rubra","Betula_alleghaniensis",
                       "Betula_lenta", "Betula_papyrifera",
                       "Alnus rubra","Alnus_incana",
                       "Corylus_cornuta","Picea_glauca","Pseudotsuga_menziesii","Fraxinus_nigra","Prunus_pensylvanica","Populus_grandidentata")

rangeofrange<-rangeofrange[c(5,1,2,3,4)]
write.csv(rangeofrange,"output/range_extent.nasps_corr.csv")



## remove aux unnecessary files
unlink("NA_range_data/*", recursive = T)
