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



## load climate data rasters (these data are not currently in the ospree folder 
## as they are heavy - 2.2Gb) - E-OBS at http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.50regular/catalog.html
tmin<-brick("~/Data_Harvard/EU trees/tn_0.50deg_reg_v17.0.nc", varname="tn", sep="")



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


# get right coordinates extents:

## set function

## set function

getrightcoords<-function(spslist,tmin){
  
  ## define array to store results
  nsps<-length(spslist)
  coords_wgs<-as.data.frame(array(NA,dim=c(nsps,5)))
  colnames(coords_wgs)<-c("species","min.x","max.x","min.y","max.y")
  
  chillsub1<-tmin[[1]] ## load for reference only

  ## commence loop  
  for (i in 1:nsps){#i=18
    print(i)
    spsi<-spslist[i]
    coords_wgs[i,1] <- spsi
    
    ## load shape
    
    path.source.i <- "../../data/distributiondata/chorological_maps_dataset_20170220.zip"
    
    # get the file address for target file
    # 
    if(!i %in% c(5,11,13,18) )
    zipped_name.i <- grep(paste(spsi,'_plg',sep=""), 
                          unzip(path.source.i,
                                list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
    
    if(i==5){
      
      zipped_name.i <- grep(paste(spsi,'_incana_plg',sep=""), 
                            unzip(path.source.i,
                                  list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
      
    }
    if(i==11){
      
      zipped_name.i <- grep(paste(spsi,'_sylvatica_plg',sep=""), 
                            unzip(path.source.i,
                                  list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
      
    }
    if(i==13){
      
      zipped_name.i <- grep(paste(spsi,'_decidua_plg',sep=""), 
                            unzip(path.source.i,
                                  list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
      
    }
    if(i==18){
      
      zipped_name.i <- grep(paste(spsi,'_ilex_plg',sep=""), 
                            unzip(path.source.i,
                                  list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
      
    }
    # extract target file
    unzip(path.source.i, files=zipped_name.i)
    
    # load shapefile
    spsshape <- shapefile(zipped_name.i[3])
    
    ## need to re-project shape from lamber equal area to geographic
    ## 
    spsshapeproj<-spTransform(spsshape,proj4string(chillsub1[[1]]))
    
    coords_wgs[i,2:5] <- as.vector(extent(spsshapeproj))
     
  }  
  
  return(coords_wgs)
  
}


## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
Coords.extent<-getrightcoords(spslist,tmin)




## saving outputs
write.csv(Coords.extent,file = "output/range_extent.eusps.corr.csv")

## remove aux unnecessary files
unlink("chorological_maps_dataset/*", recursive = T)

## end


