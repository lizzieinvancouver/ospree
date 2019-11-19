## Script to extract geographic distribution for North American Trees species out of
## published shapefiles  
## 
# Started by Dan, Base on Nacho's Get_range_ospree.R
# Date: 11 Nov 2019

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

##subset of list from github issue (make new speciescomplex for high-quality species)
#that matches avaialbe NA shape files from https://www.fs.fed.us/nrs/atlas/littlefia/species_table.html
NAsps<- c("Acer_pensylvanicum" ,
"Acer_rubrum", "Acer_saccharum",
 "Alnus_incana", "Alnus rubra",
 "Betula_alleghaniensis" ,
 "Betula_lenta", "Betula_papyrifera", 
"Cornus_cornuta",# should be COrylus
 "Fagus_grandifolia" , "Fraxinus_nigra",
 "Picea_glauca",
 "Populus_grandidentata",  "Prunus_pensylvanica", "Pseudotsuga_menziesii",
"Quercus rubra")
