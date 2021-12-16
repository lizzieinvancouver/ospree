### Fix STV error in initial Range extraction code
# Started 16 Dec 2021 by Cat
# But fixed originally by Nacho!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## load packages
require(sp)
require(raster)
require(rgeos)
require(rgdal)
require(ncdf4)
require(abind)
require(chillR)
require(lubridate)

library(rworldmap)
#install.packages("rworldxtra")
library(rworldxtra)

worldmap <- getMap(resolution="high")

NamMap=worldmap[worldmap@data$NAME %in% c("Canada", "United States", "Mexico"),]
#plot(NamMap)

e <- c( -170, -60, 15, 75)
NamMap <- crop(NamMap, e)

#climatedrive = "/n/wolkovich_lab/Lab/Cat" # Cat's climate drive
climatedrive = "/Volumes/climdata/" # Cat's climate drive
#climatedrive = "~/Desktop" 
## load climate data rasters (these data are not currently in the ospree folder 
nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]

allclimyrs <- 1979:2016 #1979:2016
tminlist <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmincrop",allclimyrs,collapse="|"), full.names = TRUE)
tminlist.tobrick <- lapply(tminlist, function(x)
{x <- brick(x);
return(x)})

for (i in seq(tminlist.tobrick))
  assign(paste0("tmin", i+1978), tminlist.tobrick[[i]])


extractchillforce<-function(spslist){ 
  
  ## define array to store results
  nyears<-length(period)
  mintemps.eachsps <- list()
  
  # load shapefile
  spsshape <- NamMap
  
  ras.numpixels<-tmin1980[[1]]
  values(ras.numpixels)<-1:ncell(ras.numpixels)
  
  spsshapeproj<-spTransform(spsshape,proj4string(ras.numpixels))
  
  # get list of pixels to extract data (speeds things up)
  pixels.sps.i<-unique(sort(unlist(raster::extract(ras.numpixels,spsshapeproj))))
  npix<-length(pixels.sps.i) # number of pixels
  
  # create an array to store results
  yearlyresults<-array(NA,dim=c(npix,9,length(period)))
  colnames(yearlyresults)<-c("x","y", "SDev.Tmins")
  
  for(j in period) { # j = 1980
    print(j)
    
    if(TRUE){
      tmin <- tminlist.tobrick[[j-1978]]
      tminprev <- tminlist.tobrick[[j-1979]]
    }
    
    leapyears <- seq(1952, 2020, by=4)
    forcestart <- ifelse(j%in%leapyears, 61, 60)
    forceend <- ifelse(j%in%leapyears, 152, 151)
    
    ## subset climate by months & days (1Mar - 31st May)
    forcesub <- subset(tmin, forcestart:forceend)
    
    ## find if there are NAs in some pixels (due to overlap with lakes or sea)
    nas <- which(is.na(values(forcesub)[pixels.sps.i]))
    
    
    ## remove NAs if necessary
    if(length(nas)>0){
      # extract values and format to compute means and sdevs
      ff <- forcesub[pixels.sps.i][-nas,]
      
      # add coordinates and names
      ffcoord <- coordinates(ras.numpixels)[pixels.sps.i[-nas],]
      yearlyresults[-nas,1:2,] <- ffcoord
      
    } else {
      
      ff <- forcesub1[pixels.sps.i]
      
      # add coordinates and names
      ffcoord <- coordinates(ras.numpixels)[pixels.sps.i,]
      yearlyresults[,1:2,] <- ffcoord
      
    }
    
    # build final data to extract climate for forcing 
    ff <- cbind(chcoord,ff[,1:ncol(ff)])
  
    ## dates in data
    colnames(ff) <- c("x", "y", c(forcestart:forceend))
    ffmindates<-ff[,3:ncol(ff)]
    days<-as.numeric(colnames(ffmindates))
    datesff<-as.Date(days,origin=paste0(j,"-01-01"))
    
    
    if(length(nas)==0){
      
      yearlyresults[,3,which(period == j)] <- apply(ff,1,sd,na.rm=T)

    } else {
      
      yearlyresults[-nas,3,which(period == j)] <- apply(ff,1,sd,na.rm=T)
    }
    
  }
  
  mintemps.eachsps[[i]] <- yearlyresults
  
  
  
  return(mintemps.eachsps)
  
}


## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
#climaterangecheck <- extractchillforce("Alnus_rubra", tmin, tmax, period)

period <- 1980:2016

Climate.in.range.list<-extractchillforce(period)

save(Climate.in.range.list,file = "~/Desktop/Climate.in.range.allyearsstacked.RData")

if(FALSE){
  for(i in 1:length(spslist)){ #i=1
    Climate.in.range<-extractchillforce(spslist[6]) ## 1, 4, 5 
    
    write.csv(Climate.in.range, file = paste("~/Documents/git/ospree/analyses/ranges/climoutput/Climate.in.range",spslist[i],
                                             period[1],max(period),"csv",sep="."))
    
    
  }
}


if(FALSE){
  load("~/Desktop/Misc/Ospree Misc/Climate.in.range.allyearsstacked.RData")
  
  ## corrections
  ##ff<-extractchillforce(spslist[13],tmin,tmax,period)
  ##Climate.in.range.list[[13]] <- ff
  
  
  for(i in 1:length(Climate.in.range.list)){ #i=38
    print(paste(i,
                sum(is.na(Climate.in.range.list[[i]][[1]][,3,1]))==nrow(Climate.in.range.list[[i]][[1]][,,1])))
    
  }
}


## Code to save individual species
#Climate.in.range.list[[3]][[1]][,,1]
#j=16
period=1980:2016
for(i in 1:length(period)){  
  sps.1 <- as.data.frame(Climate.in.range.list[[38]][,,i])
  
  to.rem.nas <- which(apply(sps.1,1,function(x)sum(is.na(x)))>5)
  if(length(to.rem.nas)>0){
    sps.1 <- sps.1[-to.rem.nas,]
  }
  
  sps.1$year<-1980
  sps.1$ID<-1:nrow(sps.1)
  
  for(i in 2:37){
    print(paste(i))
    temp.sps<-as.data.frame(Climate.in.range.list[[38]][,,i])
    to.rem.nas.j <- which(apply(temp.sps,1,function(x)sum(is.na(x)))>5)
    if(length(to.rem.nas.j)>0){
      temp.sps <- temp.sps[-to.rem.nas.j,]
    }
    temp.sps$year<-c(1980)[i]
    temp.sps$ID<-1:nrow(temp.sps)
    sps.1<-rbind(sps.1,temp.sps)
    
  }
  
  namesave <- paste("~/Desktop/allnam_fullextract.csv",sep="")
  write.csv(sps.1,file = namesave)
}

sps.1$year <- rep(1980:2016, each=44622)
#write.csv(sps.1, file = "~/Desktop/Misc/Ospree Misc/Nam_allspp_fullextract.csv")

#spslist
#write.csv(sps.1,file = "output/Abies_alba_fullextract.csv")



## synthetize and summarize data geographically and temporally
dat = read.csv("~/Desktop/Misc/Ospree Misc/allnam_fullextract.csv")
dat <- dat[,-1]
dat$year <- rep(1980:2016, each=44622)


#'######################################
#### plotting a few example species ####
#'######################################

library(rworldmap)
library(RColorBrewer)

worldmap <- getMap(resolution="high") 
#plot(worldmap,col="grey",border="grey",xlim=c(-10,50),ylim=c(32,72))

## load species list 
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

#tmin1980[[1]] <- projectRaster(tmintest, crs=crs("+proj=longlat +init=epsg:4326")) ### just added 7 Sept 2:29pm

## examples of application
betlen <- getspsshape(spslist,1,tmin1980[[1]])
spsshape <- getspsshape(spslist,1,tmin1980[[1]])
#sorauc <- getspsshape(spslist,15,tmin[[1]])
#cornmas <- getspsshape(spslist,9,tmin[[1]])

tmin1980 <- brick("~/Desktop/Misc/Ospree misc/tminclean1980.nc")


## plot shape with data on top
dir.fig = "~/Documents/git/ospree/analyses/ranges/figures/nam_sps_climate_maps/"
dir.out <- "~/Documents/git/ospree/analyses/ranges/output/"

library(RColorBrewer)
library(ggplot2)

#save <- spslist
#spslist <- spslist[[14]]
#spslist <- save

#dat <- sps.1

plot.shape.data<-function(spsshape,sps.name,
                          dir.out,dir.fig,
                          type=c("means","sds")){
  
  spsshape <- getspsshape(spslist,i,tmin1980[[1]]) # i=1
  
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
  
  ## retrieve and format data
  ## code to plot within range climate interannual variation
  dir.out <- "~/Documents/git/ospree/analyses/ranges/output/"
  files.out <- dir(dir.out)
  
  sps.i <- dat[(dat$lat.long%in%chcoord$lat.long),]
  sps.i <- na.omit(sps.i)
  sps.i <- sps.i[!duplicated(sps.i),]
  
  ### Now we need to get the area weighted average across grid cells. See Issue #387
  sps.area <- area(spsshapeproj) / 10000
  
  means.sites <- aggregate(sps.i,by=list(Year = sps.i$lat.long), FUN = function(x) sum(mean(x, na.rm=TRUE)*sps.area)/sum(sps.area))
  means.sites$lat.long <- NULL
  SDs.sites <- aggregate(sps.i, by=list(Year = sps.i$lat.long), FUN = function(x) sum(mean(x, na.rm=TRUE)*sps.area)/sum(sps.area))
  SDs.sites$lat.long <- NULL
  
  #dat <- sps.1
  
  if(type=="means"){
    pdf(paste(dir.fig,sps.name,'.means.pdf',sep="")
        ,width = 12
    )
  }
  
  
  if(type=="sds"){
    pdf(paste(dir.fig,sps.name,'.sds.pdf',sep="")
        ,width = 12
    )
  }
  
  par(mfrow=c(2,3),mar=c(1,1,1,1))
  
  
  for(i in c(4,5,7:10)){#i=5
    
    
    if(type=="means"){
      cols1<-colorRampPalette(brewer.pal(9,"RdYlBu"))(100)[as.numeric(cut(-means.sites[,i],breaks = 100))]
    }
    
    if(type=="sds"){
      cols1<-colorRampPalette(brewer.pal(9,"RdYlBu"))(100)[as.numeric(cut(-SDs.sites[,i],breaks = 100))]
    }
    
    
    plot(worldmap,col="lightgrey",border="lightgrey",
         xlim=c(extent.sps.i[1],extent.sps.i[2]),
         ylim=c(extent.sps.i[3],extent.sps.i[4]))
    text(extent.sps.i[2]-1,extent.sps.i[4],
         colnames(SDs.sites)[i],pos=2)
    plot(spsshapeproj,col=adjustcolor('lightgrey',0),add=T,
         border=adjustcolor('lightgrey',0.5))
    
    points(means.sites$x,means.sites$y,col=cols1,pch=19,cex=0.8)
    
  }
  dev.off()
  
  
  
}

# example of usage
#plot.shape.data(spsshape,spslist[1],dir.out,dir.fig,"means")

tmin1980 <- brick("~/Desktop/Misc/Ospree misc/tmincrop1980.nc")

#spslist <- save
#spslist <- spslist[1]

#### loop across species ####

for (i in 1:length(spslist)){ #i=1
  
  print(spslist[i]) 
  
  spsshape <- getspsshape(spslist,i,tmin1980[[1]])
  #spsshape <- NamMap
  
  plot.shape.data(spsshape,names(species.list.clean)[i],
                  dir.out,dir.fig,'sds')
  plot.shape.data(spsshape,names(species.list.clean)[i],
                  dir.out,dir.fig,'means')
  
  #plot.shape.data(spsshape,"all",
  #               dir.out,dir.fig,'sds')
  #plot.shape.data(spsshape,"all",
  #               dir.out,dir.fig,'means')
}


dev.off()



if(FALSE){
  #load("~/Desktop/Climate.in.range.allyears.RData")
  
  quartz()
  plot(tmin1980[[80]], col=cols1)
  rect(xleft=-105, ybottom=45, xright=-98, ytop=72)
  
  
  library(RColorBrewer)
  library(ggplot2)
  cols1<-colorRampPalette(brewer.pal(9,"RdYlBu"))(100)[as.numeric(cut(-means.sites[,i],breaks = 100))]
  
  #mapWorld <- borders("world", colour="gray72", fill="gray65",ylim=c(30,70),xlim=c(-10,35)) # create a layer of borders
  site<-dplyr::select(SDs.sites, GDD, coord)
  coords <- dplyr::select(means.sites, coord, y, x)
  site <- dplyr::full_join(site, coords)
  site<-site[!duplicated(site),]
  myPalette <- colorRampPalette(brewer.pal(9, "RdYlBu"))
  sc <- scale_colour_gradientn(colours = myPalette(100)) ### this is the range of budburst data we have
  
  #a.site<-filter(site, species=="AESHIP")
  #boundars<-readShapeSpatial("~/Documents/git/regionalrisk/analyses/input/natural_earth_vector/50m_cultural/ne_50m_admin_0_countries.shp")
  NamMap1<-fortify(NamMap)
  aes <- ggplot() + 
    geom_polygon(aes(x = NamMap1$long, y = NamMap1$lat, group = NamMap1$group),
                 color = 'gray', fill="lightgrey", size = .2) + ### This creates the base map
    geom_point(aes(x=site$x, y=site$y, color=site$GDD), size=0.6, alpha=0.4) + theme_classic() + 
    theme(panel.border = element_blank(), ### extra tweaks to background and plot to make sure it doesn't have grid lines, etc.
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(), 
          plot.title=element_text(size = 10, face="bold.italic"),
          legend.position = "none",
          axis.title = element_blank(),
          panel.background = element_rect(fill="grey95")) + ### to make sure the continent doesn't blend in with the ocea
    sc + 
    labs(color="GDDs")
  
  aes
  
  save(sps.1, file="~/Documents/git/ospree/analyses/ranges/output/Nam_allspp_fullextract.RData")
  
  #sps.1a <- sps.1[1:412753,]
  #sps.1a <- sps.1[1:412753,]
  #sps.1b <- sps.1[412754:825507,]
  #sps.1c <- sps.1[825508:1238261,]
  #sps.1d <- sps.1[1238262:1651014,]
  
  #write.csv(sps.1a, file="~/Documents/git/ospree/analyses/ranges/output/Nam_allspp_fullextract_p1.csv")
  #write.csv(sps.1b, file="~/Documents/git/ospree/analyses/ranges/output/Nam_allspp_fullextract_p2.csv")
  #write.csv(sps.1c, file="~/Documents/git/ospree/analyses/ranges/output/Nam_allspp_fullextract_p3.csv")
  #write.csv(sps.1d, file="~/Documents/git/ospree/analyses/ranges/output/Nam_allspp_fullextract_p4.csv")
}

period=1980
synth.data<-function(Climate.in.range.list){
  list.synthesis<-list()
  
  for(i in 1:length(period)){  #i=1
    sps.1<-as.data.frame(Climate.in.range.list[[i]][,,i])
    sps.1$year<-1980
    
    for(i in 2:37){
      print(paste(i))
      temp.sps<-as.data.frame(Climate.in.range.list[[i]][,,i])
      temp.sps$year<-c(1980:2016)[i]
      sps.1<-rbind(sps.1,temp.sps)
      
    }
    
    dat<-sps.1
    dat$year <- 1980
    year1<-subset(dat,year==1980)
    
    for(j in 1:length(spslist)){ #j=1
      spsshape <- getspsshape(spslist,j,tmin1980[[1]])
      
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
    }
    
    
    years = unique(dat$year)
    nyears = length(years)
    dat$ID = paste(dat$x,dat$y)
    
    storing = array(NA, dim=c(7,4))
    row.names(storing) = colnames(dat)[3:9]
    colnames(storing) = c("Geo.Mean","Geo.SD","Temp.Mean","Temp.SD")
    
    
    means.years <- aggregate(dat,by=list(Year = dat$year),FUN = mean,na.rm=T)
    SDs.years <- aggregate(dat,by=list(Year = dat$year),FUN = sd,na.rm=T)
    means.sites <- aggregate(dat,by=list(dat$ID, dat$year),FUN = mean,na.rm=T)
    SDs.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = sd,na.rm=T)
    
    storing[,1] <- colMeans(means.years[,4:10], na.rm = T)
    storing[,2] <- colMeans(SDs.years[,4:10], na.rm = T)
    storing[,3] <- colMeans(means.sites[,4:10], na.rm = T)
    storing[,4] <- colMeans(SDs.sites[,4:10], na.rm = T)
    
    list.synthesis[[i]]<-storing
  }
  
  return(list.synthesis)
}


list.allsps<-synth.data(Climate.in.range.list)


## join values from the list and save
list.allspsjoint <- as.data.frame(do.call(rbind,list.allsps))
list.allspsjoint$species <- sort(rep(spslist,7))
list.allspsjoint$variable <- rep(row.names(list.allspsjoint)[1:7],22)

#write.csv(list.allspsjoint,file = "output/Synthesis_climate_NAMsps.csv")

head(list.allspsjoint)
## plot geographic vs. temporal variation

seqgdd<-seq(1,nrow(list.allspsjoint),7)
seqgdd.frost<-seq(2,nrow(list.allspsjoint),7)
seqmintemp<-seq(4,nrow(list.allspsjoint),7)

par(mfrow=c(1,3),mar=c(4,4,1,1))
plot(list.allspsjoint$Geo.Mean[seqgdd],
     list.allspsjoint$Temp.Mean[seqgdd],xlim=c(360,430),ylim=c(360,430),
     xlab="GDD - geographic mean",ylab="GDD - temporal mean",
     pch=16,col=adjustcolor(1,0.4))
abline(a=0,b=1,col='red',lty=2)

plot(list.allspsjoint$Geo.Mean[seqgdd.frost],
     list.allspsjoint$Temp.Mean[seqgdd.frost],#xlim=c(360,430),ylim=c(360,430),
     xlab="GDD last frost - geographic mean",ylab="GDD last frost - temporal mean",
     pch=16,col=adjustcolor(1,0.4))
abline(a=0,b=1,col='red',lty=2)

plot(list.allspsjoint$Geo.Mean[seqmintemp],
     list.allspsjoint$Temp.Mean[seqmintemp],xlim=c(-3,-2),
     xlab="Min Temp - geographic mean",ylab="Min Temp - temporal mean",
     pch=16,col=adjustcolor(1,0.4))
abline(a=0,b=1,lty=2,col='red')

dev.off()

## For some species, GDDs and Min Temps tend to have sensibly lower 
## temporal means than geographic means (i.e. lower values are reached
## when averaging across years than averaging across sites). 
## Which are these species?

## for GDDs
spslist[rank(list.allspsjoint$Temp.Mean[seqgdd]/
               list.allspsjoint$Geo.Mean[seqgdd])][1:2]

## for minTemps
spslist[rank(list.allspsjoint$Temp.Mean[seqmintemp]/
               list.allspsjoint$Geo.Mean[seqmintemp])][1:8]

## we should check if these species share something in common

