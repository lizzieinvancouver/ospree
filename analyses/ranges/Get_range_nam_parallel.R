## Script to extract geographic distribution for North American Tree species out of
## published shapefiles  
## 
# Based off Nacho's code "Get_range_nam_parallel.R"
# Date: 14 Jan 2020 by Cat and Dan



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
library(rgdal)
library('ncdf4')
library('abind')
library('chillR')
library('foreach')
library('doParallel')
library(lubridate)


climatedrive = "/Volumes/climdata" # Cat's climate drive
## load climate data rasters (these data are not currently in the ospree folder 
nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]


## load species list 
species.list <- read.csv("..//output/masterspecieslist.csv")
species.list <- as.vector(species.list$x)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
zipped_names <- grep('\\.shp', unzip("NA_range_files/NA_ranges.zip",
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
  colnames(chillforcespsyears)<-c("UtahChill","ChillPortions",
                                  "GDD", "MeanTemp", "Site")
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
    
    
    yearlyresults<-array(NA,dim=c(length(period),5))
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
      
      #spsshapeproj <- spsshape
      #proj4string(spsshapeproj) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")
      
      # extract values and format to compute means and sdevs
      tempsforcemin<-extract(tminshpforce,spsshape)
      tempsforcemax<-extract(tmaxshpforce,spsshape)
      tempschillmin<-extract(tminshpchill,spsshape)
      tempschillmax<-extract(tmaxshpchill,spsshape)
      #tempsforces<-extract(yearsforce,spsshapeproj,cellnumbers=T)
      
      chmin<-do.call("rbind",tempschillmin)
      chmin<-subset(chmin,!is.na(rowSums(chmin)))
      chmin<-as.data.frame(chmin)
      names(chmin) <- c(c(chillstart:yrend), c(1:chillend))
      chmax<-do.call("rbind",tempschillmax)
      chmax<-subset(chmax,!is.na(rowSums(chmax)))
      chmax<-as.data.frame(chmax)
      names(chmax) <- c(c(chillstart:yrend), c(1:chillend))
      
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
      
      minmaxtemp<-abind(chmin,chmax, along = 3)
      
      tmaxchill <- as.vector(apply(minmaxtemp, 1, function(x){x[3:nrow(x),2]}))
      tminchill <- as.vector(apply(minmaxtemp, 1, function(x){x[3:nrow(x),1]}))
      meandaily <- (tmaxchill+tminchill)/2
      
      hrly.temp=
        data.frame(
          Temp = c(rep(meandaily, each = 24)), ##length=3178248
          Year = c(rep(as.numeric(j), times=3178248)),
          #JDay = sort(c(rep(seq(1:length(colnames(meandaily))), times = 24)))
          JDay = sort(c(rep(seq(1:length(colnames(minmaxtemp))), times=3178248)))
        )
      
      #turn into data frame and remove NAs
      wamin<-do.call("rbind",tempsforcemin)
      wamin<-subset(wamin,!is.na(rowSums(wamin)))
      wamin<-as.data.frame(wamin)
      names(wamin) <- c(c(forcestart:forceend))
      wamax<-do.call("rbind",tempsforcemax)
      wamax<-subset(wamax,!is.na(rowSums(wamax)))
      wamax<-as.data.frame(wamax)
      names(wamax) <- c(c(forcestart:forceend))
      
      wamin<-cbind(chcoordmin,wamin[,2:ncol(wamin)])
      wamax<-cbind(chcoordmax,wamax[,2:ncol(wamax)])
      
      if(nrow(wamin)!=nrow(wamax)){
        namcoo1<-apply(chcoordmin,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        namcoo2<-apply(chcoordmax,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        
        torem<-which(!namcoo1%in%namcoo2)
        torem2<-which(!namcoo2%in%namcoo1)
        
        
        if(length(torem)>0){
          wamin=wamin[-torem,]    
        }
        
        if(length(torem2)>0){
          wamax=wamax[-torem2,]    
        }
        
        
      }
      
      minmaxtemp.force<-abind(wamin,wamax, along = 3)
      
      tmaxforce <- as.vector(apply(minmaxtemp.force, 1, function(x){x[3:nrow(x),2]}))
      tminforce <- as.vector(apply(minmaxtemp.force, 1, function(x){x[3:nrow(x),1]}))
      meandaily.force <- (tmaxforce+tminforce)/2
      
      hrly.temp.force=
        data.frame(
          Temp = c(rep(meandaily.force, each = 24)), ##length=1915368
          Year = c(rep(as.numeric(j), times=1915368)),
          #JDay = sort(c(rep(seq(1:length(colnames(meandaily))), times = 24)))
          JDay = sort(c(rep(seq(1:length(colnames(minmaxtemp.force))), times=1915368)))
        )
      
      chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp[1])])
      warmcalc.mn<-chilling(hrly.temp.force, hrly.temp.force$JDay[1], hrly.temp.force$JDay[nrow(hrly.temp.force[1])])
      
      yearlyresults[which(period==j),1]<-chillcalc.mn$Utah_Model[which(chillcalc.mn$End_year==j)]
      yearlyresults[which(period==j),2]<-chillcalc.mn$Chill_portions[which(chillcalc.mn$End_year==j)]
      yearlyresults[which(period==j),3]<-(warmcalc.mn$GDH[which(warmcalc.mn$End_year==j)])/24
      yearlyresults[which(period==j),4]<-mean(hrly.temp.warm$Temp, na.rm=TRUE)
      
      yearlyresults[which(period==j),5]<-sites$siteslist[i]
      
    }
    
    climateyears[,,i]<-yearlyresults
    
  } 
  
  return(climateyears)
  
}


## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
climaterangecheck <- extractchillforce(ospreelist[[1]], tmin, tmax, 1980)
Climate.in.range<-extractchillforce(ospreespslist[i],tmin,tmax,period)




## saving outputs
#save(Climate.in.range, file = paste("output/Climate.in.range",ospreespslist[4],
#                                    period[1],max(period),"RData",sep="."))


write.csv(Climate.in.range4, file = paste("output/Climate.in.range",ospreespslist[4],
                                         period[1],max(period),"csv",sep="."))

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
  
