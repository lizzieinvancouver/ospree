## Script to extract geographic distribution for European Tree species out of
## published shapefiles  
## 
# Started by Nacho
# Date: 23th Oct 2019



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
library('dismo')


# the below is copied from code to extract NAM data

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

## function to load the climate extracted for each species' range and
## to summarize it in a short dataframe

files.out <- dir("~/Documents/git/ospree/analyses/ranges/climoutput/")

dat <- read.csv("~/Desktop/Misc/Ospree Misc/Nam_allspp_fullextract.csv")
dat <- dat[-1]

tmin1980 <- brick("~/Desktop/Misc/Ospree misc/tmincrop1980.nc")

## function to extract/correct the shape for a given species
getspsshape<-function(spslist,sps.num,ras.numpixels){
  
  i<-sps.num #sps.num=14  
  spsi<-spslist[i]
  print(spsi)
  
  #fullnamei<-fullnames[i]
  
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
  
  #spsshapeproj<-spTransform(spsshape,CRS("+proj=longlat +datum=WGS84
  #                                      +ellps=WGS84 +towgs84=0,0,0"))
  #lines(spsshapeproj)
  #
  
  return(spsshapeproj)
}


synth.data <- function(splist){
  
  list.synthesis<-list()
  
  for(i in 1:length(splist)){  #i=7
    
      spsshape <- getspsshape(splist,i,tmin1980[[1]])
      
      ## plot base map + range map
      extent.sps.i <- extent(spsshape)+3
      
      ras.numpixels<-tmin1980[[1]]
      values(ras.numpixels)<-1:ncell(ras.numpixels)
      
      spsshapeproj<-spTransform(spsshape,proj4string(ras.numpixels))
      
      # get list of pixels to extract data (speeds things up)
      pixels.sps.i<-unique(sort(unlist(raster::extract(ras.numpixels,spsshapeproj))))
      
      chcoord <- as.data.frame(coordinates(tmin1980)[pixels.sps.i,])
      chcoord$lat.long <- paste(chcoord$x, chcoord$y)
      
      dat$lat.long <- paste(dat$x, dat$y)
    
      sps.i <- dat[(dat$lat.long%in%chcoord$lat.long),]
    
    
      year1<-subset(sps.i,year==1980)
    
    
      years = unique(sps.i$year)
      nyears = length(years)
      sps.i$ID = paste(sps.i$long,sps.i$lat)
    
      storing = array(NA, dim=c(7,4))
      row.names(storing) = colnames(sps.i)[3:9]
      colnames(storing) = c("Temp.Mean","Temp.SD","Geo.Mean","Geo.SD")
    
    
      means.years <- aggregate(sps.i,by=list(Year = sps.i$year),FUN = mean,na.rm=T)
      SDs.years <- aggregate(sps.i,by=list(Year = sps.i$year),FUN = sd,na.rm=T)
      means.sites <- aggregate(sps.i,by=list(Year = sps.i$ID),FUN = mean,na.rm=T)
      SDs.sites <- aggregate(sps.i,by=list(Year = sps.i$ID),FUN = sd,na.rm=T)
    
      storing[,1] <- colMeans(means.years[,4:10], na.rm = T)
      storing[,2] <- colMeans(SDs.years[,4:10], na.rm = T)
      storing[,3] <- colMeans(means.sites[,4:10], na.rm = T)
      storing[,4] <- colMeans(SDs.sites[,4:10], na.rm = T)
    
      
    list.synthesis[[i]]<-storing

  }
  
  return(list.synthesis)

}


list.allsps<-synth.data(spslist[1:18]) #1:17

nams<-list()
for(i in 1:18){
  nams[[i]]=rep(spslist[i],7)
}
nams=unlist(nams)

## join values from the list and save
list.allspsjoint <- as.data.frame(do.call(rbind,list.allsps))
list.allspsjoint$species <- nams
list.allspsjoint$variable <- rep(row.names(list.allspsjoint)[1:7],18)

write.csv(list.allspsjoint,file = "~/Documents/git/ospree/analyses/ranges/output/Synthesis_climate_Namsps.csv", row.names=FALSE)

#checksynth <- read.csv("~/Documents/git/ospree/analyses/ranges/output/Synthesis_climate_Namsps.csv")


