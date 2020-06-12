## Script to extract geographic distribution for North American Tree species out of
## published shapefiles  
## 
# Based off Nacho's code "Get_range_nam_parallel.R"
# Date: 14 Jan 2020 by Cat and Dan

#install.packages("chillR", lib = "/n/home00/cchamberlain/apps/R/")

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## load packages
require(sp)
require(raster)
require(rgdal)
require(ncdf4)
require(abind)
require(chillR)
require(lubridate)


#climatedrive = "/n/wolkovich_lab/Lab/Cat/" # Cat's climate drive
climatedrive = "/Volumes/climdata" # Cat's climate drive
## load climate data rasters (these data are not currently in the ospree folder 
nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]

## load species list 
#species.list <- read.csv("/n/wolkovich_lab/Lab/Cat/masterspecieslist.csv")
species.list <- read.csv("~/Documents/git/ospree/analyses/output/masterspecieslist.csv")
species.list <- as.vector(species.list$x)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
#zipped_names <- grep('\\.shp', unzip("/n/wolkovich_lab/Lab/Cat/NA_range_files/NA_ranges.zip",
 #                                    list=TRUE)$Name,ignore.case=TRUE, value=TRUE)
zipped_names <- grep('\\.shp', unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

# generate a list of species with maps in the .zip  
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



if(FALSE){
  ### Attempt to stack raster layers for princeton to maybe make more streamlined...
  allclimyrs <- 1980:2016
  tmaxlist <- sapply(list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",allclimyrs, collapse="|"), full.names = TRUE), raster)
  tmaxstack <- stack(tmaxlist)
  tmaxall <- brick(tmaxstack)
  #plot(tmax[[1]])
  tminlist <- sapply(list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",allclimyrs,collapse="|"), full.names = TRUE), raster)
  tminstack <- stack(tminlist)
  tminall <- brick(tminstack)
  #tmaxtest <- raster::stack(tmaxlist, varname="tmax",sep="")
  
  tmaxall <- rotate(tmaxall)
  tminall <- rotate(tminall)
  
  e <- extent(-180, -50, 25, 80)
  tmaxall <- crop(tmaxall, e)
  tminall <- crop(tminall, e)
}

# define period
#period<-1999:2016
period<-1980:1998
#period<-1986:1998


## set function
extractchillforce<-function(spslist){
  
  ## define array to store results
  nsps<-length(spslist)
  nyears<-length(period)
  minmaxtemps.eachsps <- list()
    
    ## commence loop  
    for (i in 1:nsps){#i=1 #spslist=ospreefolder[i]
      print(c(i, j))
      spsi<-spslist
      
      ## load shape
      #path.source.i <- "/n/wolkovich_lab/Lab/Cat/NA_range_files/betupapy.zip"
      path.source.i <- "~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip"
      #unzipped <- unzip("/n/wolkovich_lab/Lab/Cat/NA_range_files/betupapy.zip",
       #                 list = TRUE)$Name
      unzipped <- unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list = TRUE)$Name
      
      shpsource <-"NA_ranges"
      
      zipped_name.i <- grep(paste(shpsource, spsi, spsi, sep="/"), unzipped, ignore.case = TRUE, value = TRUE)
      
      # load shapefile
      unzip(path.source.i, files=zipped_name.i)
      
      # load shapefile
      spsshape <- shapefile(zipped_name.i[1])
      
      for(j in c(period)) { # j = 1980
        print(j)
        
        if(TRUE){
          tmaxthisyr <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",j), full.names = TRUE)
          tmaxprev <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",j-1), full.names = TRUE)
          tminthisyr <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",j), full.names = TRUE)
          tminprev <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",j-1), full.names = TRUE)
          #jx <- nc_open(tmax)
          tmax <- brick(tmaxthisyr)
          tmax <- rotate(tmax)
          tmaxprev <- brick(tmaxprev)
          tmaxprev <- rotate(tmaxprev)
          tmin <- brick(tminthisyr)
          tmin <- rotate(tmin)
          tminprev <- brick(tminprev)
          tminprev <- rotate(tminprev)
        }
        
        leapyears <- seq(1952, 2020, by=4)
        chillstart <- ifelse((j-1)%in%leapyears,275,274)
        chillend <- ifelse(j%in%leapyears,60,59)
        forcestart <- ifelse(j%in%leapyears,61,60)
        forceend <- ifelse(j%in%leapyears,152,151)
        yrend <- ifelse((j-1)%in%leapyears,366,365)
      
      e <- extent(spsshape)
      tmaxshpforce <- crop(tmax[[forcestart:forceend]], e)
      values(tmaxshpforce)<-values(tmaxshpforce)-273.15
      tminshpforce <- crop(tmin[[forcestart:forceend]], e)
      values(tminshpforce)<-values(tminshpforce)-273.15
      
      tmaxshpchill1 <- crop(tmaxprev[[chillstart:yrend]], e)
      tmaxshpchill2 <- crop(tmaxprev[[1:chillend]], e)
      tmaxshpchill <- stack(c(tmaxshpchill1, tmaxshpchill2))
      values(tmaxshpchill)<-values(tmaxshpchill)-273.15
      
      tminshpchill1 <- crop(tminprev[[chillstart:yrend]], e)
      tminshpchill2 <- crop(tminprev[[1:chillend]], e)
      tminshpchill <- stack(c(tminshpchill1, tminshpchill2))
      values(tminshpchill)<-values(tminshpchill)-273.15
      
      ## need to re-project shape from lamber equal area to geographic
      spsshapeproj<-spTransform(spsshape,proj4string(tminshpchill[[1]]))
      
      ras.numpixels<-tminshpchill[[1]]
      values(ras.numpixels)<-1:ncell(ras.numpixels)
      
      # get list of pixels to extract data (speeds things up)
      pixels.sps.i<-unique(sort(unlist(extract(ras.numpixels,spsshapeproj))))
      npix<-length(pixels.sps.i) # number of pixels
      
      
      # create an array to store results
      yearlyresults<-array(NA,dim=c(npix,9,length(period)))
      colnames(yearlyresults)<-c("x","y",
                                 "GDD","GDD.lastfrost",
                                 "DayLastFrost","MeanTmins","SDev.Tmins",
                                 "Mean.Chill.Utah","Mean.Chill.Portions")
      
      
      #spsshapeproj <- spsshape
      #proj4string(spsshapeproj) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")
      
      # extract values and format to compute means and sdevs
      forcesub1<-extract(tminshpforce,spsshapeproj,cellnumbers=TRUE)
      forcesub2<-extract(tmaxshpforce,spsshapeproj,cellnumbers=TRUE)
      chillsub1<-extract(tminshpchill,spsshapeproj,cellnumbers=TRUE)
      chillsub2<-extract(tmaxshpchill,spsshapeproj,cellnumbers=TRUE)
      #tempsforces<-extract(yearsforce,spsshapeproj,cellnumbers=T)
      
      ## find if there are NAs in some pixels (due to overlap with lakes or sea)
      nas<-which(is.na(values(forcesub1)[pixels.sps.i]))
      
      
      ## remove NAs if necessary
      if(length(nas)>0){
        # extract values and format to compute means and sdevs
        ch<-chillsub1[pixels.sps.i][-nas,]
        ch2<-chillsub2[pixels.sps.i][-nas,]
        ff<-forcesub1[pixels.sps.i][-nas,]
        ff2<-forcesub2[pixels.sps.i][-nas,]
        
        # add coordinates and names
        chcoord<-coordinates(yearschillmin[[1]])[pixels.sps.i[-nas],]
        yearlyresults[-nas,1:2,]<-chcoord
        
      } else {
        
        ch<-chillsub1[pixels.sps.i]
        ch2<-chillsub2[pixels.sps.i]
        ff<-forcesub1[pixels.sps.i]
        ff2<-forcesub2[pixels.sps.i]
        
        # add coordinates and names
        chcoord<-coordinates(yearschillmin[[1]])[pixels.sps.i,]
        yearlyresults[,1:2,]<-chcoord
        
      }
      
      # build final data to extract climate for chilling and forcing 
      ch<-cbind(chcoord,ch[,1:ncol(ch)])
      ch2<-cbind(chcoord,ch2[,1:ncol(ch2)])
      ff<-cbind(chcoord,ff[,1:ncol(ff)])
      ff2<-cbind(chcoord,ff2[,1:ncol(ff2)])
      
      # correct if row numbers do not agree
      if(nrow(ch)!=nrow(ch2)){
        namcoo1<-apply(chcoord,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        namcoo2<-apply(chcoord2,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        
        torem<-which(!namcoo1%in%namcoo2)
        torem2<-which(!namcoo2%in%namcoo1)
        
        if(length(torem)>0){
          ch=ch[-torem,]    
        }
        
        if(length(torem2)>0){
          ch2=ch2[-torem2,]    
        }
      }
      
      ## dates in data
      datesch<-as.Date(colnames(ch),format="X%Y.%m.%d")[3:ncol(ch)]
      
      ## calculate chilling (Utah) and GDD across the period
      
      ## GDDs
      gddseachcelleachday<-apply(ff2[,3:ncol(ff2)],2,function(x){
        Tb<-10
        gdd<-ifelse((x-Tb)<0,0,x-Tb)
        return(gdd)})
      gddssum<-rowSums(gddseachcelleachday)
      #hist(gddssum)
      
      
      ## GDDs till day of last frost
      ## calculate last date of frost and GDD until then
      last.frost<-apply(ff[,3:ncol(ff)],1,function(x){
        a<-which(x<(-5))
        return(ifelse(length(a)>0,max(a),NA))}) 
      
      ff3<-cbind(last.frost,ff2[,-c(1:2)])
      
      gddseachcelleachdaylastfrost<-apply(ff3,1,function(x){
        #x<-ff3[922,]
        elems<-length(x)-1
        daylastfrost<-x[1]
        if(!is.na(daylastfrost) & daylastfrost>1){
          temps<-x[2:daylastfrost]
          Tb<-10
          gdd<-ifelse((temps-Tb)<0,0,temps-Tb)
          gdd<-c(gdd,rep(0,elems-x[1]+1))
          names(gdd)<-colnames(ff3[,2:152])      
        } else {
          gdd<-rep(0,elems)
          names(gdd)<-colnames(ff3[,2:152])
        }
        return(gdd)})
      
      gddssumlastfrost<-rowSums(t(gddseachcelleachdaylastfrost),na.rm = T)
      
      
      #library(abind)
      minmaxtemp<-abind(ch,ch2, along = 3)
      
      #for(i in 1:366){print(sum(is.na(minmaxtemp[i,,])))}
      
      ## compute chilling
      nodata<-which(apply(minmaxtemp,1,function(x){return(ifelse(sum(is.na(x[,1:2]))>0,T,F))}))
      if(length(nodata)>0){minmaxtemp=minmaxtemp[-nodata,,]}
      
      chillunitseachcelleachday<-do.call(rbind,
                                         apply(minmaxtemp,1,function(x){
                                           #x<-minmaxtemp[300,,]
                                           #if(sum(is.na(x[3:nrow(x),2]))<151){
                                           extracweathdf<-data.frame(
                                             Year=as.numeric(format(datesch,"%Y")),
                                             Month=as.numeric(format(datesch,"%m")),
                                             Day=as.numeric(format(datesch,"%d")),
                                             Tmax=x[3:nrow(x),2],
                                             Tmin=x[3:nrow(x),1]
                                           )
                                           weather<-fix_weather(extracweathdf)
                                           hourtemps<-stack_hourly_temps(weather,latitude=x[2])
                                           chll<-chilling(hourtemps,275,60)
                                           
                                           
                                           #}
                                           return(chll)
                                         }
                                         ))
      
      ## store results
      yearlyresults[-nas,3,which(period == j)] <- gddssum
      yearlyresults[-nas,4,which(period == j)] <- gddssumlastfrost
      yearlyresults[-nas,5,which(period == j)] <- last.frost
      yearlyresults[-nas,6,which(period == j)] <- rowMeans(ch,na.rm=T)
      yearlyresults[-nas,7,which(period == j)] <- apply(ch,1,sd,na.rm=T)
      if(length(nodata)>0){
        yearlyresults[c(c(1:npix)[-nas])[-nodata],8,which(period == j)] <- chillunitseachcelleachday$Utah_Model
        yearlyresults[c(c(1:npix)[-nas])[-nodata],9,which(period == j)] <- chillunitseachcelleachday$Chill_portions
      } else {
        yearlyresults[c(c(1:npix)[-nas]),8,which(period == j)] <- chillunitseachcelleachday$Utah_Model
        yearlyresults[c(c(1:npix)[-nas]),9,which(period == j)] <- chillunitseachcelleachday$Chill_portions
        
      }
      }
      
      minmaxtemps.eachsps[[i]] <- yearlyresults
      
    }  
  
  return(minmaxtemps.eachsps)
  
}


## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
#climaterangecheck <- extractchillforce("Alnus_rubra", tmin, tmax, period)
Climate.in.range.list<-list()
period <- 1980:1999
for(i in 1:length(spslist)){ #i=1
  Climate.in.range[[i]]<-extractchillforce(ospreefolder[i])
  
  write.csv(Climate.in.range[[i]], file = paste("~/Documents/git/ospree/analyses/ranges/output/Climate.in.range",ospreefolder[i],
                                           period[1],max(period),"csv",sep="."))
}




## saving outputs
#save(Climate.in.range, file = paste("output/Climate.in.range",ospreespslist[4],
#                                    period[1],max(period),"RData",sep="."))



if(FALSE){
  ## attempt to parallelize code
  n = 2 # modify according to your RAM memory
  cl <- makeCluster(n)
  registerDoParallel(cl)
  
  Sys.time()
  Climate.in.range.i<-foreach(spslist = ospreespslist[4:7], .packages=c("raster","ncdf4","abind","chillR"),
                              .verbose=T,.errorhandling="pass")  %dopar%  
    extractchillforce(spslist,tmin,tmax,period) ## profiling function only / and all paralell process
  Sys.time()
  
  
  ## saving outputs1         
  for(i in 1:length(Climate.in.range.i)){
    
    Climate.in.range = Climate.in.range.i[[i]][,,1]  
    #save(Climate.in.range, file = paste("output/Climate.in.range",ospreespslist[i],
    #                                     period[1],max(period),"RData",sep="."))
    write.csv(Climate.in.range, file = paste("output/Climate.in.range",ospreespslist[i],
                                             period[1],max(period),"csv",sep="."))
    
    
  }
  
  ## remove aux unnecessary files
  unlink("chorological_maps_dataset/*", recursive = T)
}
