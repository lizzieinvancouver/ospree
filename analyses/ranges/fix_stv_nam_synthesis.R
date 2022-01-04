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

dat <- read.csv("~/Desktop/allnam_fullextract.csv")
dat <- dat[-1]
dat$year <- rep(1980:2016, each=44622)

tmin1980 <- brick("~/Desktop/Misc/Ospree misc/tmincrop1980.nc")
tmin <- tmin1980

## function to extract/correct the shape for a given species
getspsshape<-function(spslist,sps.num,ras.numpixels){
  
  i<-sps.num #sps.num=8  
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
  
  for(i in 1:length(splist)){  #i=1
    
      spsshape <- getspsshape(splist,i,tmin1980[[1]])
      
      ## plot base map + range map
      extent.sps.i <- extent(spsshape)+3
      
      ras.numpixels<-tmin1980[[1]]
      values(ras.numpixels)<-1:ncell(ras.numpixels)
      
      spsshapeproj<-spTransform(spsshape,proj4string(ras.numpixels))
      
      # get list of pixels to extract data (speeds things up)
      pixels.sps.i<-unique(sort(unlist(raster::extract(ras.numpixels,spsshapeproj, weights=TRUE, normalizeWeights=FALSE)))) ### by making weights=TRUE and normalizeWeights=FALSE then we are averaging values by grid cell size
      
      chcoord <- as.data.frame(coordinates(tmin1980)[pixels.sps.i,])
      chcoord$lat.long <- paste(chcoord$x, chcoord$y)
      
      dat$lat.long <- paste(dat$x, dat$y)
    
      sps.i <- dat[(dat$lat.long%in%chcoord$lat.long),]
      sps.i$lat.long <- NULL
      sps.i$ID = as.numeric(as.factor(paste(sps.i$x,sps.i$y)))
    
      storing = array(NA, dim=c(1,4))
      row.names(storing) = colnames(sps.i)[3]
      colnames(storing) = c("Temp.Mean","Temp.SD","Geo.Mean","Geo.SD")
      
      ### Now we need to get the area weighted average across grid cells. See Issue #387
      ras.areas <- area(tmin[[1]]) ## this step generates a raster file with vales for each pixel corresponding to its area
      
      values(ras.areas) <- (values(ras.areas) - min(values(ras.areas),na.rm=T))/
        (max(values(ras.areas),na.rm=T)-min(values(ras.areas),na.rm=T)) ## this step (optional) would re-scale pixel's area values to range between 0 and 1, but it is easy to do 1-2 by adding 1.
      
      ## then you'd need to add a column with either areas or rescaled areas:
      sps.i$weights <- extract(ras.areas,sps.i[,1:2])
    
      means.years <- aggregate(sps.i[,-5],by=list(year = sps.i$year), FUN = function(x) mean(x*sps.i$weights, na.rm=TRUE))
      SDs.years <- aggregate(sps.i[,-5],by=list(year = sps.i$year), FUN = function(x) sd(x*sps.i$weights, na.rm=TRUE))
      means.sites <- aggregate(sps.i[,-4],by=list(id = sps.i$ID), FUN = function(x) mean(x*sps.i$weights, na.rm=TRUE))
      SDs.sites <- aggregate(sps.i[,-4],by=list(id = sps.i$ID), FUN = function(x) sd(x*sps.i$weights, na.rm=TRUE))
    
      storing[,1] <- mean(means.years[,4], na.rm = T)
      storing[,2] <- mean(SDs.years[,4], na.rm = T)
      storing[,3] <- mean(means.sites[,4], na.rm = T)
      storing[,4] <- mean(SDs.sites[,4], na.rm = T)
    
      
    list.synthesis[[i]]<-storing

  }
  
  return(list.synthesis)

}


list.allsps1<-synth.data(spslist[1:4])
list.allsps2<-synth.data(spslist[c(5,7,9)])
list.allsps3<-synth.data(spslist[c(10,13:14)])
list.allsps4<-synth.data(spslist[16:17])# splist <- spslist[1]    #1:17
list.allsps5<-synth.data(spslist[6])
list.allsps6<-synth.data(spslist[11])
list.allsps7<-synth.data(spslist[12])
list.allsps8<-synth.data(spslist[15]) ## still need
list.allsps9<-synth.data(spslist[18]) ## still need
list.allsps10 <- storing ## ran through by hand

### need to fix picabi (8) bad crs... 

list.allsps <- c(list.allsps1, list.allsps2, list.allsps3, 
                     list.allsps4, list.allsps5, list.allsps6, list.allsps7,
                 list.allsps8)#, list.allsps9, list.allsps10)

write.csv(list.allsps, file="~/Desktop/osp_stv_save.csv", row.names=FALSE)

nams<-list()
for(i in 1:18){
  nams[[i]]=rep(names(species.list.clean)[i],1)
}
nams=unlist(nams)

nams = c(nams[1:5], nams[7], nams[9], nams[c(10, 13:14)], nams[16:17], nams[6], nams[11],
         nams[12], nams[15], nams[18], nams[8]) ### need to reorder! 

## join values from the list and save
list.allspsjoint <- as.data.frame(do.call(rbind,list.allsps))
list.allspsjoint <- rbind(list.allspsjoint, list.allsps9, list.allsps10)
list.allspsjoint$species <- nams
list.allspsjoint$variable <- rep(row.names(list.allspsjoint)[1],18)
rownames(list.allspsjoint) <- 1:nrow(list.allspsjoint)

### Now need to add to the rest of the parameters since just updated STV!!! 
old <- read.csv("~/Documents/git/ospree/analyses/ranges/output/Synthesis_climate_Namsps_weighted.csv")
save <- old
foo <- list.allspsjoint

for(i in c(1:nrow(old))){ #i=5
  for(j in c(1:nrow(list.allspsjoint))){ #j=1
    
    old$Temp.Mean[i] <- ifelse(old$species[i]==list.allspsjoint$species[j] & old$variable[i]==list.allspsjoint$variable[j],
                            list.allspsjoint$Temp.Mean[j], old$Temp.Mean[i])
    old$Temp.SD[i] <- ifelse(old$species[i]==list.allspsjoint$species[j] & old$variable[i]==list.allspsjoint$variable[j],
                            list.allspsjoint$Temp.SD[j], old$Temp.SD[i])
    old$Geo.Mean[i] <- ifelse(old$species[i]==list.allspsjoint$species[j] & old$variable[i]==list.allspsjoint$variable[j],
                          list.allspsjoint$Geo.Mean[j], old$Geo.Mean[i])
    old$Geo.SD[i] <- ifelse(old$species[i]==list.allspsjoint$species[j] & old$variable[i]==list.allspsjoint$variable[j],
                           list.allspsjoint$Geo.SD[j], old$Geo.SD[i])
    
  }
}


#write.csv(old,file = "~/Documents/git/ospree/analyses/ranges/output/Synthesis_climate_Namsps_weighted.csv", row.names=FALSE)



