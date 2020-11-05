## Started 5 November 2020 by Cat
## Evaluate range areas to compare congenerics


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



## load packages
library('raster')
library('ncdf4')
library('abind')
library('chillR')
library('RColorBrewer')


# First we'll start with NAM species

species.list <- read.csv("~/Documents/git/ospree/analyses/output/masterspecieslist.csv")
species.list <- as.vector(species.list$x)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
zipped_names <- grep('\\.shp', unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

# generate a list of species with maps in the .zip  
species.list.maps <- unlist(zipped_names)
species.list.maps <- gsub(pattern = "(.*/)(.*)(.shp.*)", replacement = "\\2", x = species.list.maps)
species.list.clean <- species.list.maps

rmspp <- c("alnurubr", "._robipseu", "._poputrem", "._alnurugo", "._picemari")
species.list.clean <- species.list.clean[!species.list.clean%in%rmspp]

## Now I need to rename these folders to match the ospree info
names(species.list.clean) <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra", 
                               "Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_nigra", "Picea_mariana", "Robinia_pseudoacacia",
                               "Pseudotsuga_menziesii", "Prunus_pensylvanica", "Populus_tremuloides", "Betula_alleghaniensis",
                               "Acer_saccharum", "Alnus_incana", "Acer_rubrum", "Corylus_cornuta", "Picea_glauca")

# get a list of species in ospree for which we have EU maps
ospreespslist <- species.list[which(species.list %in% names(species.list.clean))]
spslist <- unname(species.list.clean)


tmin1980 <- brick("~/Desktop/Misc/Ospree misc/tmincrop1980.nc")

## function to extract/correct the shape for a given species
getspsshape<-function(spslist,sps.num,ras.numpixels){
  
  i<-sps.num #sps.num=1  
  spsi<-spslist[i]
  print(spsi)

  ## load shape
  
  path.source.i <- "~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip"
  
  # get the file address for target file
  unzipped <- unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list = TRUE)$Name
  
  shpsource <-"NA_ranges"
  
  zipped_name.i <- grep(paste(shpsource, spsi, spsi, sep="/"), unzipped, ignore.case = TRUE, value = TRUE)
  
  if(length(zipped_name.i)==0){
    
    specific <- unlist(strsplit(spsi,"_"))[2]
    unzipped <- unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list = TRUE)$Name
    
  }
  
  # extract target file
  unzip(path.source.i, files=zipped_name.i)
  
  # load shapefile
  spsshape <- shapefile(zipped_name.i[1])
  
  ## need to re-project shape from lamber equal area to geographic
  if(i==8){
    proj4string(spsshape) <- CRS("+proj=aea  
                                 +lat_1=38 +lat_2=42 +lat_0=40 +lon_0=-82 +x_0=0 +y_0=0
                                 +units=m +datum=NAD27 +ellps=clrk66 +no_defs")
  } else{
    proj4string(spsshape) <- CRS("+proj=longlat +init=epsg:4326")
  }
  
  spsshapeproj<-spTransform(spsshape,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 "))

  
  return(spsshapeproj)
}


area.data <- function(splist){
  
  list.synthesis<-list()
  
  for(i in 1:length(splist)){  #i=1
    
    spsshape <- getspsshape(splist,i,tmin1980[[1]])
    
    ## plot base map + range map
    extent.sps.i <- extent(spsshape)+3
    
    ras.numpixels<-tmin1980[[1]]
    values(ras.numpixels)<-1:ncell(ras.numpixels)
    
    spsshapeproj<-spTransform(spsshape,proj4string(ras.numpixels))
    
    storing = array(NA, dim=c(1,1))
    colnames(storing) = c("range_area")
    
    ### Now we need to get the area weighted average across grid cells. See Issue #387
    sps.area <- area(spsshapeproj) / 1000000

    storing[,1] <- sum(sps.area)
    
    
    list.synthesis[[i]]<-storing
    
  }
  
  return(list.synthesis)
  
}


list.allsps.nam<-area.data(spslist[1:18]) 

nams<-list()
for(i in 1:18){
  nams[[i]]=rep(names(species.list.clean)[i],1)
}
nams=unlist(nams)

## join values from the list and save
list.allspsareas.nam <- as.data.frame(do.call(rbind,list.allsps.nam))
list.allspsareas.nam$species <- nams
list.allspsareas.nam$continent <- "north america"



#### Now it's Europe time!
tmin.eur <- brick("~/Desktop/Big Data Files/tn_0.25deg_reg_v16.0.nc")

## load species list (beware this list has both species and complexes and is not the final one
## please change accordingly to read final species list)
species.list <- read.csv("../phylogeny/input/spslist.csv")
species.list <- sort(species.list$Species_binomial)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
zipped_names <- grep('\\_plg.shp$', unzip("../../data/distributiondata/chorological_maps_dataset_20170220.zip",
                                          list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

# generate a list of species with maps in the .zip  
species.list.maps <- unlist(lapply(strsplit(sub(".*/", "", zipped_names),"_"),
                                   function(x){return(paste(x[1],x[2],sep="_"))}))

# get a list of species in ospree for which we have EU maps
spslist.eur <- species.list[which(species.list %in% species.list.maps)]

getspsshape.eur<-function(spslist.eur,sps.num,ras.numpixels){
  
  i<-sps.num #sps.num=1
  spsi<-spslist.eur[i]
  print(spsi)
  
  #fullnamei<-fullnames[i]
  
  ## load shape
  
  path.source.i <- "../../data/distributiondata/chorological_maps_dataset_20170220.zip"
  
  # get the file address for target file
  zipped_name.i <- grep(paste(spsi,'_plg',sep=""), 
                        unzip(path.source.i,
                              list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
  
  if(length(zipped_name.i)==0){
    
    specific <- unlist(strsplit(spsi,"_"))[2]
    zipped_name.i <- grep(paste(spsi,specific,'plg',sep="_"), 
                          unzip(path.source.i,
                                list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
    
  }
  
  # extract target file
  unzip(path.source.i, files=zipped_name.i)
  
  # load shapefile
  spsshape <- shapefile(zipped_name.i[3])
  
  ## need to re-project shape from lamber equal area to geographic
  ras.numpixels<-tmin.eur[[1]]
  values(ras.numpixels)<-1:ncell(ras.numpixels)
  spsshapeproj <- spTransform(spsshape,proj4string(ras.numpixels))
  #lines(spsshapeproj)
  #
  
  return(spsshapeproj)
}


area.data.eur <- function(spslist.eur){
  
  list.synthesis<-list()
  
  for(i in 1:length(spslist.eur)){  #i=2
    
    spsshape <- getspsshape.eur(spslist.eur,i,tmin.eur[[1]])
    
    ## plot base map + range map
    extent.sps.i <- extent(spsshape)+3
    
    ras.numpixels<-tmin.eur[[1]]
    values(ras.numpixels)<-1:ncell(ras.numpixels)
    
    spsshapeproj<-spTransform(spsshape,proj4string(ras.numpixels))
    
    storing = array(NA, dim=c(1,1))
    colnames(storing) = c("range_area")
    
    ### Now we need to get the area weighted average across grid cells. See Issue #387
    sps.area <- area(spsshapeproj) / 1000000
    
    storing[,1] <- sum(sps.area)
    
    
    list.synthesis[[i]]<-storing
    
  }
  
  return(list.synthesis)
  
}


list.allsps.eur<-area.data.eur(spslist.eur[1:length(spslist.eur)]) 

eurs<-list()
for(i in 1:length(spslist.eur)){
  eurs[[i]]=rep(spslist.eur[i],1)
}
eurs=unlist(eurs)

## join values from the list and save
list.allspsareas.eur <- as.data.frame(do.call(rbind,list.allsps.eur))
list.allspsareas.eur$species <- eurs
list.allspsareas.eur$continent <- "europe"


list.allspsjoint <- rbind(list.allspsareas.nam, list.allspsareas.eur)


write.csv(list.allspsjoint,file = "~/Documents/git/ospree/analyses/ranges/output/rangeareas.csv", row.names=FALSE)


