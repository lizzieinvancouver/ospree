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
ospreespslist2=ospreespslist[-c(5,11,13,18)]
nsps<-length(ospreespslist2)

listtostore = list()
for (i in 1:nsps){#i=1
  print(i)
  spsi<-ospreespslist2[i]
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
}


rangeofrange<-data.frame(complex=character(),min=numeric(),max=numeric())

for(i in c(1:length(listtostore))){ 
min<-ymin(listtostore[[i]])
max<-ymax(listtostore[[i]])
dfhere <- data.frame(complex=paste(ospreespslist2[[i]]),min=min,max=max)
rangeofrange <- rbind(rangeofrange, dfhere) ## rbind it here for safty
}
rangeofrange$distance<-rangeofrange$max-rangeofrange$min

write.csv(rangeofrange,"range_extent.eusps.csv",row.names = FALSE)
## remove aux unnecessary files
unlink("chorological_maps_dataset/*", recursive = T)
