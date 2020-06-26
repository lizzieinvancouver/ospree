## Script to extract geographic distribution for North American Tree species out of
## published shapefiles  
## 
# Based off Nacho's code "Get_range_nam_parallel.R"
# Date: 14 Jan 2020 by Cat and Dan

#withr::with_makevars(c(PKG_LIBS = "-liconv"), install.packages("chillR"), assignment = "+=")

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


climatedrive = "/n/wolkovich_lab/Lab/Cat" # Cat's climate drive
#climatedrive = "/Volumes/climdata" # Cat's climate drive
## load climate data rasters (these data are not currently in the ospree folder 
nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]

## load species list 
species.list <- read.csv("/n/wolkovich_lab/Lab/Cat/masterspecieslist.csv")
#species.list <- read.csv("~/Documents/git/ospree/analyses/output/masterspecieslist.csv")
species.list <- as.vector(species.list$x)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
zipped_names <- grep('\\.shp', unzip("/n/wolkovich_lab/Lab/Cat/NA_range_files/NA_ranges.zip",
                                     list=TRUE)$Name,ignore.case=TRUE, value=TRUE)
#zipped_names <- grep('\\.shp', unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

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

spslist <- species.list.maps

if(FALSE){
  ### Attempt to stack raster layers for princeton to maybe make more streamlined...
  allclimyrs <- 1979:2016
  e <- extent(190, 300, 15, 75)
  tmaxlist <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",allclimyrs,collapse="|"), full.names = TRUE)
  # Now, let's make sure all of the dataframes have the same column names
  tmaxlist.nam <- lapply(tmaxlist, function(x)
    {x <- brick(x);
  return(x)})
  tmaxlist.nam <- lapply(tmaxlist.nam, function(x) 
  {x <- crop(x, e) ; 
  return(x)})
  newextent <- extent(-170, -60, 15, 75)
  tmaxlist.nam <- lapply(tmaxlist.nam, function(x) 
    {extent(x) <- newextent ; 
  return(x)})
  tmaxlist.nam <- lapply(tmaxlist.nam, function(x)
    {values(x)<-values(x)-273.15;
  return(x)})
  
  maxL <- setNames(tmaxlist.nam, paste0("tmaxcrop",c(1979:2016)))
  
  for(i in 1:(length(maxL))){
    writeRaster(maxL[[i]], filename=paste0("/n/wolkovich_lab/Lab/Cat/princetonclimdata/tmaxcrop",i+1978), overwrite=TRUE, format="CDF")
  }
  
  #for (i in seq(tmaxlist.nam))
   # assign(paste0("tmax", i+1979), tmaxlist.nam[[i]])
  

  tminlist <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",allclimyrs, collapse="|"), full.names = TRUE)
  # Now, let's make sure all of the dataframes have the same column names
  tminlist.nam <- lapply(tminlist, function(x)
  {x <- brick(x);
  return(x)})
  tminlist.nam <- lapply(tminlist.nam, function(x) 
  {x <- crop(x, e) ; 
  return(x)})
  newextent <- extent(-170, -60, 15, 75)
  tminlist.nam <- lapply(tminlist.nam, function(x)
  {extent(x) <- newextent;
  return(x)})
  tminlist.nam <- lapply(tminlist.nam, function(x)
  {values(x)<-values(x)-273.15;
  return(x)})
  
  minL <- setNames(tminlist.nam, paste0("tmincrop",c(1979:2016)))
  
  for(i in 1:(length(minL))){
    writeRaster(minL[[i]], filename=paste0("/n/wolkovich_lab/Lab/Cat/princetonclimdata/tmincrop",i+1978), overwrite=TRUE, format="CDF")
  }
}


allclimyrs <- 1979:2016
tmaxlist <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmaxcrop",allclimyrs,collapse="|"), full.names = TRUE)
tmaxlist.tobrick <- lapply(tmaxlist, function(x)
    {x <- brick(x);
  return(x)})

for (i in seq(tmaxlist.tobrick))
  assign(paste0("tmax", i+1978), tmaxlist.tobrick[[i]])

pdf(file="/n/wolkovich_lab/Lab/Cat/testraster.pdf")
plot(tmax1980[[1]])
#plot(spsshapeproj, add=TRUE)
dev.off()

tminlist <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmincrop",allclimyrs,collapse="|"), full.names = TRUE)
tminlist.tobrick <- lapply(tminlist, function(x)
    {x <- brick(x);
  return(x)})

for (i in seq(tminlist.tobrick))
  assign(paste0("tmin", i+1978), tminlist.tobrick[[i]])

# define period
#period<-1999:2016
#period<-1980:2016
#period<-1986:1998

#spslist=ospreefolder
## set function
extractchillforce<-function(spslist){ 
  
  ## define array to store results
  nsps<-length(spslist)
  nyears<-length(period)
  minmaxtemps.eachsps <- list()
    
    ## commence loop  
    for (i in 1:nsps){#i=1 #spslist=2
      #print(c(i, j))
      #spslist=ospreefolder[i]
      spsi<-spslist[i]
      
      ## load shape
      path.source.i <- "/n/wolkovich_lab/Lab/Cat/NA_range_files/NA_ranges.zip"
      #path.source.i <- "~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip"
      unzipped <- unzip("/n/wolkovich_lab/Lab/Cat/NA_range_files/NA_ranges.zip",
                        list = TRUE)$Name
      #unzipped <- unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list = TRUE)$Name
      
      shpsource <-"NA_ranges"
      
      zipped_name.i <- grep(paste(shpsource, spsi, spsi, sep="/"), unzipped, ignore.case = TRUE, value = TRUE)
      
      # load shapefile
      unzip(path.source.i, files=zipped_name.i)
      
      # load shapefile
      spsshape <- shapefile(zipped_name.i[1])
      
      ## need to re-project shape from lamber equal area to geographic
      #spsshapeproj <- spsshape
      proj4string(spsshape) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")
      
      spsshapeproj<-spTransform(spsshape,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
      
      ras.numpixels<-tmin1980[[1]]
      values(ras.numpixels)<-1:ncell(ras.numpixels)
    
      
      # get list of pixels to extract data (speeds things up)
      pixels.sps.i<-unique(sort(unlist(extract(ras.numpixels,spsshapeproj))))
      npix<-length(pixels.sps.i) # number of pixels
      npix <- npix-2
      
      # create an array to store results
      yearlyresults<-array(NA,dim=c(npix,9,length(period)))
      colnames(yearlyresults)<-c("x","y",
                                 "GDD","GDD.lastfrost",
                                 "DayLastFrost","MeanTmins","SDev.Tmins",
                                 "Mean.Chill.Utah","Mean.Chill.Portions")
      
      for(j in period) { # j = 1981
        print(j)
        
        if(TRUE){
          tmax <- tmaxlist.tobrick[[j-1978]]
          tmaxprev <- tmaxlist.tobrick[[j-1979]]
          tmin <- tminlist.tobrick[[j-1978]]
          tminprev <- tminlist.tobrick[[j-1979]]
        }
        
        leapyears <- seq(1952, 2020, by=4)
        chillstart <- ifelse((j-1)%in%leapyears,275,274)
        chillend <- ifelse(j%in%leapyears,60,59)
        forcestart <- ifelse(j%in%leapyears,61,60)
        forceend <- ifelse(j%in%leapyears,152,151)
        yrend <- ifelse((j-1)%in%leapyears,366,365)
      
        
      e <- extent(spsshapeproj)
      tmaxshpforce <- crop(tmax[[forcestart:forceend]], e)
      #values(tmaxshpforce)<-values(tmaxshpforce)-273.15
      
      tminshpforce <- crop(tmin[[forcestart:forceend]], e)
      #values(tminshpforce)<-values(tminshpforce)-273.15
      #tminshpforce <- tminshpforce[pixels.sps.i] ### ADDING THIS ROUND !!
      
      tmaxshpchill1 <- crop(tmaxprev[[chillstart:yrend]], e)
      tmaxshpchill2 <- crop(tmaxprev[[1:chillend]], e)
      tmaxshpchill <- stack(c(tmaxshpchill1, tmaxshpchill2))
      #values(tmaxshpchill)<-values(tmaxshpchill)-273.15
      #tmaxshpchill <- tmaxshpchill[pixels.sps.i] ### ADDING THIS ROUND !!
      
      tminshpchill1 <- crop(tminprev[[chillstart:yrend]], e)
      tminshpchill2 <- crop(tminprev[[1:chillend]], e)
      tminshpchill <- stack(c(tminshpchill1, tminshpchill2))
      #values(tminshpchill)<-values(tminshpchill)-273.15
      #tminshpchill <- tminshpchill[pixels.sps.i] ### ADDING THIS ROUND !!
      
      
      # extract values and format to compute means and sdev
      forcesub1<-extract(tminshpforce,spsshapeproj,cellnumbers=TRUE)
      forcesub2<-extract(tmaxshpforce,spsshapeproj,cellnumbers=TRUE)
      chillsub1<-extract(tminshpchill,spsshapeproj,cellnumbers=TRUE)
      chillsub2<-extract(tmaxshpchill,spsshapeproj,cellnumbers=TRUE)
      
      
      chmin<-do.call("rbind",chillsub1)
      chmin<-as.data.frame(chmin)
      names(chmin) <- c("z", c(chillstart:yrend), c(1:chillend))
      chmin <- chmin[!is.na(chmin$z),]
      chmin <- chmin[!duplicated(chmin$z),]
      chmax<-do.call("rbind",chillsub2)
      chmax<-as.data.frame(chmax)
      names(chmax) <- c("z", c(chillstart:yrend), c(1:chillend))
      chmax <- chmax[!is.na(chmax$z),]
      chmax <- chmax[!duplicated(chmax$z),]
      
      # get coordinates and names
      chcoordmin<-coordinates(tmin1980[[1]])[pixels.sps.i,]
      yearlyresults[,1:2,]<-chcoordmin
      chcoordmax<-coordinates(tmin1980[[1]])[pixels.sps.i,]
      chmin<-cbind(chcoordmin,chmin[,2:ncol(chmin)])
      chmax<-cbind(chcoordmax,chmax[,2:ncol(chmax)])
      
      if(nrow(chmin)!=nrow(chmax)){
        namcoo1<-apply(chcoordmin,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        namcoo2<-apply(chcoordmax,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        
        torem<-which(!namcoo1%in%namcoo2)
        torem2<-which(!namcoo2%in%namcoo1)
        
        
        if(length(torem)>0){
          chmin=chmin[-torem,]    
        }
        
        if(length(torem2)>0){
          chmax=chmax[-torem2,]    
        }
        
        
      }
      
      wamin<-do.call("rbind",forcesub1)
      wamin<-as.data.frame(wamin)
      names(wamin) <- c("z",forcestart:forceend)
      wamin <- wamin[!is.na(wamin$z),]
      wamin <- wamin[!duplicated(wamin$z),]
      wamax<-do.call("rbind",forcesub2)
      wamax<-as.data.frame(wamax)
      names(wamax) <- c("z",forcestart:forceend)
      wamax <- wamax[!is.na(wamax$z),]
      wamax <- wamax[!duplicated(wamax$z),]
      
      ffcoordmin<-coordinates(tmin1980[[1]])[wamax[,1],]
      ffcoordmax<-coordinates(tmin1980[[1]])[wamax[,1],]
      ffmin<-cbind(ffcoordmin,wamin[,1:ncol(wamin)])
      ffmax<-cbind(ffcoordmin,wamax[,1:ncol(wamax)])
      
      wamin<-wamin[,2:93]
      wamax<-wamax[,2:93]
      
      if(nrow(ffmin)!=nrow(ffmax)){
        namcoo1<-apply(ffcoordmin,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        namcoo2<-apply(ffcoordmax,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        
        torem<-which(!namcoo1%in%namcoo2)
        torem2<-which(!namcoo2%in%namcoo1)
        
        
        if(length(torem)>0){
          ffmin=ffmin[-torem,]    
        }
        
        if(length(torem2)>0){
          ffmax=ffmax[-torem2,]    
        }
        
        
      }
      
      minmaxtemp.warm<-abind(ffmin,ffmax, along = 3)
      dateswa<-as.Date(as.numeric(colnames(wamin)),origin=paste0(j,"-01-01"))
      
      ## calculate chilling (Utah) and GDD across the period
      
      ## GDDs
      gddseachcelleachday<-apply(ffmax[,3:ncol(ffmax)],2,function(x){
        Tb<-10
        gdd<-ifelse((x-Tb)<0,0,x-Tb)
        return(gdd)})
      gddssum<-rowSums(gddseachcelleachday)
      names(gddssum) <- NULL
      
      
      ## GDDs till day of last frost
      ## calculate last date of frost and GDD until then
      last.frost<-apply(ffmin[,3:ncol(ffmin)],1,function(x){
        a<-which(x<(-5))
        return(ifelse(length(a)>0,max(a),NA))}) 
      
      ff3<-cbind(last.frost,ffmax[,-c(1:2)])
      
      gddseachcelleachdaylastfrost<-apply(ff3,1,function(x){
        #x<-ff3[922,]
        elems<-length(x)-1
        daylastfrost<-x[1]
        if(!is.na(daylastfrost) & daylastfrost>1){
          temps<-x[2:daylastfrost]
          Tb<-10
          gdd<-ifelse((temps-Tb)<0,0,temps-Tb)
          gdd<-c(gdd,rep(0,elems-x[1]+1))
          names(gdd)<-colnames(ff3[,2:93])      
        } else {
          gdd<-rep(0,elems)
          names(gdd)<-colnames(ff3[,2:93])
        }
        return(gdd)})
      
      gddssumlastfrost<-rowSums(t(gddseachcelleachdaylastfrost),na.rm = T)
      names(gddssumlastfrost) <- NULL
      
      
      #library(abind)
      minmaxtemp<-abind(chmin,chmax, along = 3)
      chmindates<-chmin[,3:ncol(chmin)]
      days<-as.numeric(colnames(chmindates))
      jfordates<-ifelse(days>=270, j-1, j)
      datesch<-as.Date(days,origin=paste0(jfordates,"-01-01"))
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
      yearlyresults[,3,which(period == j)] <- gddssum
      yearlyresults[,4,which(period == j)] <- gddssumlastfrost
      yearlyresults[,5,which(period == j)] <- last.frost
      yearlyresults[,6,which(period == j)] <- rowMeans(chmin,na.rm=T)
      yearlyresults[,7,which(period == j)] <- apply(chmin,1,sd,na.rm=T)
      if(length(nodata)>0){
        yearlyresults[c(c(1:npix))[-nodata],8,which(period == j)] <- chillunitseachcelleachday$Utah_Model
        yearlyresults[c(c(1:npix))[-nodata],9,which(period == j)] <- chillunitseachcelleachday$Chill_portions
      } else {
        yearlyresults[c(c(1:npix)),8,which(period == j)] <- chillunitseachcelleachday$Utah_Model
        yearlyresults[c(c(1:npix)),9,which(period == j)] <- chillunitseachcelleachday$Chill_portions
        
      }
      }
      
      minmaxtemps.eachsps[[i]] <- yearlyresults
      
      #write.csv(minmaxtemps.eachsps[[i]], file = paste("/n/wolkovich_lab/Lab/Cat/Climate.in.range",spslist[i],
       #                                             period[1],max(period),"csv",sep="."))
      
    }  
  
  return(minmaxtemps.eachsps)
  
}


## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
#climaterangecheck <- extractchillforce("Alnus_rubra", tmin, tmax, period)
Climate.in.range<-list()
period <- 1980:2016  ### for 3 did 1985:2005, 1980:1985, 
spslist=spslist[16]
for(i in 1:length(spslist)){ #i=1
  Climate.in.range<-extractchillforce(spslist[i])
  
  write.csv(Climate.in.range, file = paste("/n/wolkovich_lab/Lab/Cat/Climate.in.range",spslist[i],
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
