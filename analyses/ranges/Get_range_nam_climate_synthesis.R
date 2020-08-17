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
species.list.clean <- species.list.clean[-13]

## Now I need to rename these folders to match the ospree info
names(species.list.clean) <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra", 
                               "Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_nigra","Alnus_incana",
                               "Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis",
                               "Acer_saccharum", "Acer_rubrum", "Corylus_cornuta", "Picea_glauca")

# get a list of species in ospree for which we have EU maps
ospreespslist <- species.list[which(species.list %in% names(species.list.clean))]
spslist <- species.list.maps


## function to load the climate extracted for each species' range and
## to summarize it in a short dataframe

files.out <- dir("climoutput/")
splist <- species.list.clean[1:15]



synth.data <- function(splist){
  
  list.synthesis<-list()
  
  for(i in 1:length(splist)){  #i=7
    sps.i <- names(splist[i])
  print(sps.i)
    sps.out <- files.out[which(grepl(sps.i,files.out)&
                                 grepl("in.range.",files.out)&
                                 grepl("1980.2016.",files.out))]
    
    dat.1 <- read.csv(paste("climoutput/",sps.out[1],sep=""))
    
    
    year1<-subset(dat.1,year==1980)
    
    
    years = unique(dat.1$year)
    nyears = length(years)
    dat.1$ID = paste(dat.1$long,dat.1$lat)
    
    storing = array(NA, dim=c(7,4))
    row.names(storing) = colnames(dat.1)[3:9]
    colnames(storing) = c("Geo.Mean","Geo.SD","Temp.Mean","Temp.SD")
    
    
    means.years <- aggregate(dat.1,by=list(Year = dat.1$year),FUN = mean,na.rm=T)
    SDs.years <- aggregate(dat.1,by=list(Year = dat.1$year),FUN = sd,na.rm=T)
    means.sites <- aggregate(dat.1,by=list(Year = dat.1$ID),FUN = mean,na.rm=T)
    SDs.sites <- aggregate(dat.1,by=list(Year = dat.1$ID),FUN = sd,na.rm=T)
    
    storing[,1] <- colMeans(means.years[,4:10], na.rm = T)
    storing[,2] <- colMeans(SDs.years[,4:10], na.rm = T)
    storing[,3] <- colMeans(means.sites[,4:10], na.rm = T)
    storing[,4] <- colMeans(SDs.sites[,4:10], na.rm = T)
    
    list.synthesis[[i]]<-storing



  }
  
  return(list.synthesis)

}


list.allsps<-synth.data(splist[1:15])

nams<-list()
for(i in 1:15){
  nams[[i]]=rep(splist[i],7)
}
nams=unlist(nams)

## join values from the list and save
list.allspsjoint <- as.data.frame(do.call(rbind,list.allsps))
list.allspsjoint$species <- nams
list.allspsjoint$variable <- rep(row.names(list.allspsjoint)[1:7],15)

write.csv(list.allspsjoint,file = "output/Synthesis_climate_Namsps.csv")




try.synth.nam <- synth.data(splist)

synth.NAM <- do.call(rbind,try.synth.nam)

write.csv(synth.NAM, file = 'output/Synthesis_climate_NAMsps.csv')


