## Script to extract geographic distribution for North American Tree species out of
## published shapefiles  
## 
# Based off Nacho's code "Get_range_nam_parallel.R"
# Date: 14 Jan 2020 by Cat and Dan



# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## load packages
library(sp)
library('raster')
library(rgdal)
library('ncdf4')
library('abind')
library('chillR')
library(lubridate)


climatedrive = "/n/wolkovich_lab/Lab/Cat/" # Cat's climate drive
## load climate data rasters (these data are not currently in the ospree folder 
nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]

## load species list 
species.list <- read.csv("/n/wolkovich_lab/Lab/Cat/masterspecieslist.csv")
species.list <- as.vector(species.list$x)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
zipped_names <- grep('\\.shp', unzip("/n/wolkovich_lab/Lab/Cat/NA_range_files/NA_ranges.zip",
                                     list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

# generate a list of species with maps in the .zip  
species.list.maps <- unlist(zipped_names)
species.list.maps <- gsub(pattern = "(.*/)(.*)(.shp.*)", replacement = "\\2", x = species.list.maps)

## Now I need to rename these folders to match the ospree info
names(species.list.maps) <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra", 
                              "Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_excelsior", "Alnus_rubra",
                              "Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis",
                              "Acer_saccharum", "Alnus_incana", "Acer_rubrum", "Cornus_cornuta", "Picea_glauca")

# get a list of species in ospree for which we have EU maps
ospreespslist <- species.list[which(species.list %in% names(species.list.maps))]
## This takes out:
# Alnus rubra
ospreespslist <- c(ospreespslist, "Alnus_rubra")

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
period<-1980:2016
#period<-2009:2010


## set function
extractchillforce<-function(spslist,tmin,tmax,period){
  
  ## define array to store results ## i=1
  nsps<-length(ospreespslist) #nsps<-length(spslist)
  nyears<-length(period)
  chillforcespsyears<-array(NA,dim=c(nyears,6,nsps))
  row.names(chillforcespsyears)<-period
  colnames(chillforcespsyears)<-c("GDD", "GDD.sd", "UtahChill", "UtahChill.sd", "ChillPortions",
                                  "ChillPortions.sd")
  #dimnames(chillforcespsyears)<-spslist
  
  mins <- maxs <- vector()
  
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
    
    
    yearlyresults<-array(NA,dim=c(length(period),6))
    ## commence loop  
    for (i in 1:nsps){#i=1 #spslist=ospreespslist[i]
      print(c(i, j))
      
      ## load shape
      path.source.i <- "NA_range_files/NA_ranges.zip"
      
      zipped_name.i <- grep('\\.shp', unzip(path.source.i,
                                            list=TRUE)$Name,ignore.case=TRUE, value=TRUE)
      # load shapefile
      spsshape <- shapefile(zipped_name.i[i])
      
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
      
      spsshapeproj <- spsshape
      proj4string(spsshapeproj) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")
      
      # extract values and format to compute means and sdevs
      tempsforcemin<-extract(tminshpforce,spsshapeproj,cellnumbers=TRUE)
      tempsforcemax<-extract(tmaxshpforce,spsshapeproj,cellnumbers=TRUE)
      tempschillmin<-extract(tminshpchill,spsshapeproj,cellnumbers=TRUE)
      tempschillmax<-extract(tmaxshpchill,spsshapeproj,cellnumbers=TRUE)
      #tempsforces<-extract(yearsforce,spsshapeproj,cellnumbers=T)
      
      chmin<-do.call("rbind",tempschillmin)
      chmin<-subset(chmin,!is.na(rowSums(chmin)))
      chmin<-as.data.frame(chmin)
      names(chmin) <- c("z", c(chillstart:yrend), c(1:chillend))
      chmax<-do.call("rbind",tempschillmax)
      chmax<-subset(chmax,!is.na(rowSums(chmax)))
      chmax<-as.data.frame(chmax)
      names(chmax) <- c("z", c(chillstart:yrend), c(1:chillend))
      
      # get coordinates and names
      chcoordmin<-coordinates(tminshpchill[[1]])[chmin[,1],]
      chcoordmax<-coordinates(tmaxshpchill[[1]])[chmax[,1],]
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
      
      wamin<-do.call("rbind",tempsforcemin)
      wamin<-subset(wamin,!is.na(rowSums(wamin)))
      wamin<-as.data.frame(wamin)
      names(wamin) <- c("z",forcestart:forceend)
      wamax<-do.call("rbind",tempsforcemax)
      wamax<-subset(wamax,!is.na(rowSums(wamax)))
      wamax<-as.data.frame(wamax)
      names(wamax) <- c("z",forcestart:forceend)
      
      ffcoordmin<-coordinates(tminshpforce[[1]])[wamin[,1],]
      ffcoordmax<-coordinates(tmaxshpforce[[1]])[wamax[,1],]
      ffmin<-cbind(ffcoordmin,wamin[,2:ncol(wamin)])
      ffmax<-cbind(ffcoordmin,wamax[,2:ncol(wamax)])
      
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
      
      warmunitseachcelleachday<-do.call(rbind,
                                        apply(minmaxtemp.warm,1,function(x){
                                          #x<-minmaxtemp[300,,]
                                          extracweathdf<-data.frame(
                                            Year=as.numeric(format(dateswa,"%Y")),
                                            Month=as.numeric(format(dateswa,"%m")),
                                            Day=as.numeric(format(dateswa,"%d")),
                                            Tmax=x[3:nrow(x),2],
                                            Tmin=x[3:nrow(x),1]
                                          )
                                          weather<-fix_weather(extracweathdf)
                                          hourtemps<-stack_hourly_temps(weather,latitude=x[2])
                                          wrm<-chilling(hourtemps,forcestart,forceend)
                                          
                                          return(wrm)
                                          
                                        }
                                        ))
      
      #chmin<-chmin[,3:151]
      #chmax<-chmax[,3:151]
      minmaxtemp<-abind(chmin,chmax, along = 3)
      chmindates<-chmin[,3:154]
      days<-as.numeric(colnames(chmindates))
      jfordates<-ifelse(days>=270, j-1, j)
      datesch<-as.Date(days,origin=paste0(jfordates,"-01-01"))
      
      chillunitseachcelleachday<-do.call(rbind,
                                         apply(minmaxtemp,1,function(x){
                                           #x<-minmaxtemp[300,,]
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
                                           
                                           return(chll)
                                           
                                         }
                                         ))
      
      
      
      yearlyresults[which(period==j),1]<-mean((warmunitseachcelleachday$GDH)/24,na.rm=T)
      yearlyresults[which(period==j),2]<-sd((warmunitseachcelleachday$GDH)/24,na.rm=T)
      yearlyresults[which(period==j),3]<-mean(chillunitseachcelleachday$Utah_Model,na.rm=T)
      yearlyresults[which(period==j),4]<-sd(chillunitseachcelleachday$Utah_Model,na.rm=T)
      yearlyresults[which(period==j),5]<-mean(chillunitseachcelleachday$Chill_portions,na.rm=T)
      yearlyresults[which(period==j),6]<-sd(chillunitseachcelleachday$Chill_portions,na.rm=T)
      
    }
    
    chillforcespsyears[,,i]<-yearlyresults
    
  }  
  
  return(chillforcespsyears)
  
}

## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
#climaterangecheck <- extractchillforce("Alnus_rubra", tmin, tmax, period)
Climate.in.range<-extractchillforce(ospreespslist,tmin,tmax,period)




## saving outputs
#save(Climate.in.range, file = paste("output/Climate.in.range",ospreespslist[4],
#                                    period[1],max(period),"RData",sep="."))


write.csv(Climate.in.range, file = "/n/wolkovich_lab/Lab/Cat/Climate.in.range.csv", row.names=FALSE)
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
