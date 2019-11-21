###plot Europe ranges
## Script to extract geographic distribution for European Tree species out of
## published shapefiles  


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
library('foreach')
library('doParallel')

species.list <- read.csv("../phylogeny/input/spslist.csv")
species.list <- sort(species.list$Species_binomial)

zipped_names <- grep('\\_plg.shp$', unzip("../../data/distributiondata/chorological_maps_dataset_20170220.zip",
                                          list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

# generate a list of species with maps in the .zip  
species.list.maps <- unlist(lapply(strsplit(sub(".*/", "", zipped_names),"_"),
                                   function(x){return(paste(x[1],x[2],sep="_"))}))

# get a list of species in ospree for which we have EU maps
ospreespslist <- species.list[which(species.list %in% species.list.maps)]
##remove alnus incana cause of subssp
ospreespslist=ospreespslist[c(1,2,3,6,7,10,14,20)]
nsps<-length(ospreespslist)

listtostore = list()
for (i in 1:nsps){#i=1
  print(i)
  spsi<-ospreespslist[i]
  #fullnamei<-fullnames[i]
  
  ## load shape
  
  path.source.i <- "../../data/distributiondata/chorological_maps_dataset_20170220.zip"

  # get the file address for target file
  zipped_name.i <- grep(paste(spsi,'_plg',sep=""), 
                        unzip(path.source.i,
                              list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
  
  # extract target file
  unzip(path.source.i, files=zipped_name.i)
  
  # load shapefile
  spsshape <- shapefile(zipped_name.i[3])
  listtostore[[i]] <- shapefile(zipped_name.i[3])
  
  ## need to re-project shape from lamber equal area to geographic
  ## 
  #spsshapeproj<-spTransform(spsshape,proj4string(chillsub1[[1]]))

par(mar = c(0.5, .5, .5, .5))

jpeg("..//ranges/overlay_8sp.jpeg",width = 8.6, height = 4, units = 'in', res=200)
plot(listtostore[[4]])
plot(listtostore[[1]],add=TRUE,col=rgb(1,.5,1,alpha = 0.3))
plot(listtostore[[2]],add=TRUE,col=rgb(0,.5,1,alpha = 0.3))
plot(listtostore[[3]],add=TRUE,col=rgb(1,0,0,alpha = 0.3))
plot(listtostore[[5]],add=TRUE,col=rgb(1,0,1,alpha = 0.3))
plot(listtostore[[6]],add=TRUE,col=rgb(1,0,.5,alpha = 0.3))
plot(listtostore[[7]],add=TRUE,col=rgb(0,0,1,alpha = 0.3))
plot(listtostore[[8]],add=TRUE,col=rgb(.1,1,1,alpha = 0.3))
dev.off()

## remove aux unnecessary files
unlink("chorological_maps_dataset/*", recursive = T)
