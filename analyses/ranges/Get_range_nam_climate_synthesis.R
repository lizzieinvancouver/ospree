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

species.list <- read.csv("~/GitHub/ospree/analyses/output/masterspecieslist.csv")
species.list <- as.vector(species.list$x)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
#zipped_names <- grep('\\.shp', unzip("/n/wolkovich_lab/Lab/Cat/NA_range_files/NA_ranges.zip",
#                                    list=TRUE)$Name,ignore.case=TRUE, value=TRUE)
zipped_names <- grep('\\.shp', unzip("~/GitHub/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

# generate a list of species with maps in the .zip  
species.list.maps <- unlist(zipped_names)
species.list.maps <- gsub(pattern = "(.*/)(.*)(.shp.*)", replacement = "\\2", x = species.list.maps)
species.list.clean <- species.list.maps

## Now I need to rename these folders to match the ospree info
names(species.list.clean) <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra", 
                               "Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_excelsior",
                               "Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis",
                               "Acer_saccharum", "Alnus_incana", "Acer_rubrum", "Corylus_cornuta", "Picea_glauca")

# get a list of species in ospree for which we have EU maps
ospreespslist <- species.list[which(species.list %in% names(species.list.clean))]
spslist <- species.list.maps


## function to load the climate extracted for each species' range and
## to summarize it in a short dataframe

files.out <- dir("climoutput/")
splist <- species.list.clean[1:15]



synth.data <- function(splist){
  
  list.synthesis<-list()
  
  for(i in 1:length(splist)){  #i=2
    sps.i <- names(splist[i])
  
    sps.out <- files.out[which(grepl(sps.i,files.out)&
                                 grepl("in.range.",files.out)&
                                 grepl("1980.2016.",files.out))]
  
    dat.1 <- read.csv(paste("climoutput/",sps.out,sep=""))
   
    if (i ==2){
      dat.1 <- dat.1[,-1]
    }
      
    #colnames(dat.1)[seq(3,ncol(dat.1),9)]
    
    storing = as.data.frame(array(NA, dim=c(7,4)))
    row.names(storing) = colnames(dat.1)[3:9]
    colnames(storing) = c("Geo.Mean","Geo.SD","Temp.Mean","Temp.SD")
    
    
    for(var in 1:7){
      print(paste(i,var))
      dat.i.j<-dat.1[,seq(var+2,ncol(dat.1),9)]
      
      storing[var,1] <- mean(rowMeans(dat.i.j,na.rm=T))
      storing[var,2] <- sd(rowMeans(dat.i.j,na.rm=T))
      storing[var,3] <- mean(colMeans(dat.i.j,na.rm=T))
      storing[var,4] <- sd(colMeans(dat.i.j,na.rm=T))
    
      }      
    
    storing$species <- sps.i
    if(i != 2){
    storing$variable <- unlist(strsplit(colnames(dat.1)[3:9],"1980"))
    } else {
    storing$variable <- unlist(strsplit(colnames(dat.1)[3:9],"1"))
      
    }
    
    list.synthesis[[i]] <- storing
    
  }
  
  return(list.synthesis)

}


try.synth.nam <- synth.data(splist)

synth.NAM <- do.call(rbind,try.synth.nam)

write.csv(synth.NAM, file = 'output/Synthesis_climate_NAMsps.csv')


