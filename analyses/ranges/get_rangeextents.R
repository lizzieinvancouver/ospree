#### Get Range data information
# We want the bottom 10% of minimum latitudes for each species,
# the top 10% of maximum latitudes for each species,
# and the middle 10% of centroid latitudes for each species for the joint model
## Started by Cat - 9 June 2020

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
library('dplyr')
library('raster')
library('ncdf4')



#### Start with European species!! ####

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
spslist <- species.list[which(species.list %in% species.list.maps)]


## define array to store results
nsps<-length(spslist)

minlatspecies <- c()
maxlatspecies <- c()
meanlatspecies <- c()

## commence loop  
for (i in 1:nsps){#i=1
  spsi<-spslist[i]
  print(spsi)
  
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
  ## 
  allcoords <- as.data.frame(coordinates(spTransform(spsshape, CRS("+proj=longlat +datum=WGS84"))))
  colnames(allcoords) <- c("long", "lat")
  
  min.threshold <- quantile(allcoords$lat, probs=c(0.10))
  minlats.df <- allcoords[(allcoords$lat<=min.threshold),]
  
  max.threshold <- quantile(allcoords$lat, probs=c(0.90))
  maxlats.df <- allcoords[(allcoords$lat>=max.threshold),]
  
  meanmin.threshold <- quantile(allcoords$lat, probs=c(0.40))
  meanmax.threshold <- quantile(allcoords$lat, probs=c(0.60))
  meanlats.df <- allcoords[(allcoords$lat<=meanmax.threshold & allcoords$lat>=meanmin.threshold),]
  
  minlats.spsi <- cbind(minlats.df, rep(spsi, nrow(minlats.df)))
  minlatspecies <- rbind(minlatspecies, minlats.spsi)
  
  maxlats.spsi <- cbind(maxlats.df, rep(spsi, nrow(maxlats.df)))
  maxlatspecies <- rbind(maxlatspecies, maxlats.spsi)
  
  meanlats.spsi <- cbind(meanlats.df, rep(spsi, nrow(meanlats.df)))
  meanlatspecies <- rbind(meanlatspecies, meanlats.spsi)
  
  
}  
  

minlats.eur <- as.data.frame(minlatspecies)
colnames(minlats.eur) <- c("minlong","minlat", "species")
minlats.eur$continent <- "europe"

maxlats.eur <- as.data.frame(maxlatspecies)
colnames(maxlats.eur) <- c("maxlong", "maxlat", "species")
maxlats.eur$continent <- "europe"

minmax.eur <- cbind(minlats.eur, maxlats.eur)

if(FALSE){
minlats.eur <- as.data.frame(minlatspecies)
colnames(minlats.eur) <- c("long","lat", "species")
minlats.eur$continent <- "europe"
minlats.eur$type <- "min"

maxlats.eur <- as.data.frame(maxlatspecies)
colnames(maxlats.eur) <- c("long", "lat", "species")
maxlats.eur$continent <- "europe"
maxlats.eur$type <- "max"

eur.lats <- full_join(minlats.eur, maxlats.eur)

meanlats.eur <- as.data.frame(meanlatspecies)
colnames(meanlats.eur) <- c("long", "lat", "species")
meanlats.eur$continent <- "europe"
meanlats.eur$type <- "mean"

eur.lats <- full_join(eur.lats, meanlats.eur)
}

####### Now it's North American species time! #######

species.list <- read.csv("~/Documents/git/ospree/analyses/output/masterspecieslist.csv")
species.list <- as.vector(species.list$x)

zipped_names <- grep('\\.shp', unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", 
                                     list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

species.list.maps <- unlist(zipped_names)
species.list.maps <- gsub(pattern = "(.*/)(.*)(.shp.*)", replacement = "\\2", x = species.list.maps)
species.list.clean <- species.list.maps

## Now I need to rename these folders to match the ospree info
names(species.list.clean) <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra", 
                               "Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_excelsior", "Alnus_rubra",
                               "Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis",
                               "Acer_saccharum", "Alnus_incana", "Acer_rubrum", "Cornus_cornuta", "Picea_glauca")

# get a list of species in ospree for which we have EU maps
ospreespslist <- species.list[which(species.list %in% names(species.list.clean))]
## This takes out:
# Alnus rubra
ospreespslist <- c(ospreespslist, "Alnus_rubra")

ospreefolder <- species.list.maps

## define array to store results
nsps<-length(ospreefolder)

minlatspecies <- c()
maxlatspecies <- c()
meanlatspecies <- c()

## commence loop  
for (i in 1:nsps){#i=1
  spsi<-ospreefolder[i]
  print(spsi)
  
  ## load shape
  
  path.source.i <- "~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip"
  
  unzipped <- unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", 
                    list = TRUE)$Name
  
  # get the file address for target file
  shpsource <-"NA_ranges"
  zipped_name.i <- grep(paste(shpsource, spsi, spsi, sep="/"), 
                        unzipped, ignore.case = TRUE, value = TRUE)
  
  # extract target file
  unzip(path.source.i, files=zipped_name.i)
  
  # load shapefile
  spsshape <- raster::shapefile(zipped_name.i[1])
  proj4string(spsshape) <- CRS('+proj=longlat +datum=WGS84')
  
  ## need to re-project shape from lamber equal area to geographic
  ## 
  allcoords <- as.data.frame(coordinates(spTransform(spsshape, CRS("+proj=longlat +datum=WGS84"))))
  colnames(allcoords) <- c("long", "lat")
  
  min.threshold <- quantile(allcoords$lat, probs=c(0.10))
  minlats.df <- allcoords[(allcoords$lat<=min.threshold),]
  
  max.threshold <- quantile(allcoords$lat, probs=c(0.90))
  maxlats.df <- allcoords[(allcoords$lat>=max.threshold),]
  
  meanmin.threshold <- quantile(allcoords$lat, probs=c(0.42))
  meanmax.threshold <- quantile(allcoords$lat, probs=c(0.58))
  meanlats.df <- allcoords[(allcoords$lat<=meanmax.threshold & allcoords$lat>=meanmin.threshold),]
  
  minlats.spsi <- cbind(minlats.df, rep(spsi, nrow(minlats.df)))
  minlatspecies <- rbind(minlatspecies, minlats.spsi)
  
  maxlats.spsi <- cbind(maxlats.df, rep(spsi, nrow(maxlats.df)))
  maxlatspecies <- rbind(maxlatspecies, maxlats.spsi)
  
  meanlats.spsi <- cbind(meanlats.df, rep(spsi, nrow(meanlats.df)))
  meanlatspecies <- rbind(meanlatspecies, meanlats.spsi)
  
  
}  


minlats.nam <- as.data.frame(minlatspecies)
colnames(minlats.nam) <- c("minlong","minlat", "species")
minlats.nam$continent <- "north america"

maxlats.nam <- as.data.frame(maxlatspecies)
colnames(maxlats.nam) <- c("maxlong", "maxlat", "species")
maxlats.nam$continent <- "north america"

minmax.nam <- cbind(minlats.nam, maxlats.nam)


minmaxs <- rbind(minmax.eur, minmax.nam)



minmaxs$species <- ifelse(minmaxs$species=="betulent", "Betula_lenta", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="popugran", "Populus_grandidentata", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="fagugran", "Fagus_grandifolia", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="querrubr", "Quercus_rubra", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="acerpens", "Acer_pensylvanicum", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="betupapy", "Betula_papyrifera", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="fraxnigr", "Fraxinus_excelsior", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="alnurubr", "Alnus_rubra", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="pseumenz", "Pseudotsuga_menziesii", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="prunpens", "Prunus_pensylvanica", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="betualle", "Betula_alleghaniensis", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="acersacr", "Acer_saccharum", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="alnurugo", "Alnus_incana", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="acerrubr", "Acer_rubrum", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="corycorn", "Cornus_cornuta", minmaxs$species)
minmaxs$species <- ifelse(minmaxs$species=="piceglau", "Picea_glauca", minmaxs$species)

minmaxs<- subset(minmaxs, select=c("minlong", "minlat", "species", "continent", "maxlong", "maxlat"))

write.csv(minmaxs, file="output/minmax_rangeextent.csv", row.names=FALSE)




##### TO SAVE FOR NOW!!! INCLUDES CENTROID DATA! #########
minlats.nam <- as.data.frame(minlatspecies)
colnames(minlats.nam) <- c("long","lat", "species")
minlats.nam$continent <- "north america"
minlats.nam$type <- "min"

maxlats.nam <- as.data.frame(maxlatspecies)
colnames(maxlats.nam) <- c("long", "lat", "species")
maxlats.nam$continent <- "north america"
maxlats.nam$type <- "max"

nam.lats <- full_join(minlats.nam, maxlats.nam)

meanlats.nam <- as.data.frame(meanlatspecies)
colnames(meanlats.nam) <- c("long", "lat", "species")
meanlats.nam$continent <- "north america"
meanlats.nam$type <- "mean"

nam.lats <- full_join(nam.lats, meanlats.nam)


allspplats <- full_join(eur.lats, nam.lats)


#### Now need to fix the North American species names

allspplats$species <- ifelse(allspplats$species=="betulent", "Betula_lenta", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="popugran", "Populus_grandidentata", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="fagugran", "Fagus_grandifolia", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="querrubr", "Quercus_rubra", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="acerpens", "Acer_pensylvanicum", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="betupapy", "Betula_papyrifera", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="fraxnigr", "Fraxinus_excelsior", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="alnurubr", "Alnus_rubra", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="pseumenz", "Pseudotsuga_menziesii", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="prunpens", "Prunus_pensylvanica", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="betualle", "Betula_alleghaniensis", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="acersacr", "Acer_saccharum", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="alnurugo", "Alnus_incana", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="acerrubr", "Acer_rubrum", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="corycorn", "Cornus_cornuta", allspplats$species)
allspplats$species <- ifelse(allspplats$species=="piceglau", "Picea_glauca", allspplats$species)

write.csv(allspplats, file="~/Documents/git/ospree/analyses/ranges/output/latitudeextents_allspp.csv", row.names=FALSE)



######## JUST PREPPING FOR LAT MODELS NOW WITHOUT CENTROID DATA TO START ###########




