## Script to extract geographic distribution and climate within
## for European Tree species out of
## published shapefiles  
## 
## this code modifies the Get_ranges_EU file in ospree/analyses/ranges/
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
  setwd("~/GitHub/ospree/analyses/phylogeny/") 
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

## load climate data ----
## load climate data rasters (these data are not currently in the ospree folder 
## as they are heavy - 2.2Gb) - E-OBS at http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.50regular/catalog.html
#tmin<-brick("D:/Data_Harvard/EU trees/tn_0.50deg_reg_v17.0.nc", varname="tn", sep="")
#tmax<-brick("D:/Data_Harvard/EU trees/tx_0.50deg_reg_v17.0.nc", varname="tx", sep="")
#tmin<-brick("D:/Data_Harvard/EU trees/tn_0.25deg_reg_v15.0.nc", varname="tn", sep="")
#tmax<-brick("D:/Data_Harvard/EU trees/tx_0.25deg_reg_v15.0.nc", varname="tx", sep="")
tmin<-brick("D:/Data_Harvard/EU trees/tn_ens_mean_0.25deg_reg_v25.0e.nc", varname="tn", sep="")
tmax<-brick("D:/Data_Harvard/EU trees/tx_ens_mean_0.25deg_reg_v25.0e.nc", varname="tx", sep="")
tavg<-brick("D:/Data_Harvard/EU trees/tg_ens_mean_0.25deg_reg_v25.0e.nc", varname="tg", sep="")

#tmin <- brick("~/Desktop/Big Data Files/tn_0.25deg_reg_v16.0.nc")
#tmin <- brick("~/Desktop/Big Data Files/tx_0.25deg_reg_v16.0.nc")

## load species list (beware this list has both species and complexes and is not the final one
## please change accordingly to read final species list)
species.list <- read.csv("output/sps_list_phylo.csv")
species.list <- sort(species.list$phylo.tip.label)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
zipped_names <- grep('\\_plg.shp$', unzip("../../data/distributiondata/chorological_maps_dataset_20170220.zip",
                                          list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

# generate a list of species with maps in the .zip  
species.list.maps <- unlist(lapply(strsplit(sub(".*/", "", zipped_names),"_"),
                                   function(x){return(paste(x[1],x[2],sep="_"))}))

# get a list of species in ospree for which we have EU maps
spslist <- species.list[which(species.list %in% species.list.maps)]


# define period
period<-1950:1961
#period<-2009:2010
#spslist<-spslist[1]


## define extracting function ----

## set function (modified to include weighting already - has not been run)
## The below section is changed with respect to the one in ranges
## based on Lizzie's email from Jan18th: 
##    - Utah units from from 1 September through 30 April 
##    - mean spring forcingmean temperature from 1 March through 30 April 



extractchillforce <- function(spslist,tmin,tmax,tavg,period){
  
  ## define array to store results
  nsps <- length(spslist)
  nyears <- length(period)
  minmaxtemps.eachsps <- list()
  
  
  ## subset climate years
  yearsinclim <- as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
  
  ras.numpixels <- tmin[[1]]
  values(ras.numpixels) <- 1:ncell(ras.numpixels)
  
  
  ## commence loop  
  for (i in 1:nsps){#i=1
    spsi<-spslist[i]
    print(spsi)
    
    #fullnamei<-fullnames[i]
    
    ## load shape
    
    path.source.i <- "../../data/distributiondata/chorological_maps_dataset_20170220.zip"
    
    # get the file address for target file
    zipped_name.i <- grep(paste(spsi,'_plg',sep=""), 
                          unzip(path.source.i,
                                list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
    
    if(length(zipped_name.i)==0){
      
      specific <- unlist(strsplit(spsi,"_"))[2]
      zipped_name.i <- grep(paste(spsi,specific,'plg',sep="_"), 
                            unzip(path.source.i,
                                  list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
      
    }
    
    # extract target file
    unzip(path.source.i, files=zipped_name.i)
    
    # load shapefile
    spsshape <- shapefile(zipped_name.i[3])
    
    ## need to re-project shape from lamber equal area to geographic
    spsshapeproj <- spTransform(spsshape,proj4string(ras.numpixels))
    #lines(spsshapeproj)
    
    # get list of pixels to extract data (speeds things up)
    pixels.sps.i <- unique(sort(unlist(extract(ras.numpixels,spsshapeproj))))
    npix <- length(pixels.sps.i) # number of pixels
    #area.pixels.sps.i <- values(ras.areas)[pixels.sps.i]
    
    
    
    # create an array to store results
    yearlyresults <- array(NA,dim=c(npix,4,length(period)))
    colnames(yearlyresults) <- c("x","y",
                                 "Forcing","Chilling")
    
    
    
    ## loop across years to extract each years averages and stddevs
    
    for(j in 1950:1960){#j=1950
      print(paste(i,j))
      
      ## load two consecutive years each time
      yearsinperiod <- which(yearsinclim%in%c(j,j+1))
      climsub.min <- subset(tmin,yearsinperiod)
      climsub.max <- subset(tmax,yearsinperiod)
      climsub.avg <- subset(tavg,yearsinperiod)
      
      ## subset climate by months & days (1st Sep - 30Apr; 1March - 30April)
      chillsubmin <- subset(climsub.min,244:485)
      chillsubmax <- subset(climsub.max,244:485)
      forcesub <- subset(climsub.avg,425:485)
      
      
      ## find if there are NAs in some pixels (due to overlap with lakes or sea)
      nas <- which(is.na(values(chillsubmin)[pixels.sps.i]))
      #nas2 <- which(is.na(values(chillsubmax)[pixels.sps.i]))
      #nasf <- which(is.na(values(forcesub)[pixels.sps.i]))
      
      
      ## remove NAs if necessary
      if(length(nas)>0){
        # extract values and format to compute means and sdevs
        ch1 <- chillsubmin[pixels.sps.i][-nas,]
        ch2 <- chillsubmax[pixels.sps.i][-nas,]
        ff <- forcesub[pixels.sps.i][-nas,]
        
        # add coordinates and names
        chcoord <- coordinates(ras.numpixels)[pixels.sps.i[-nas],]
        yearlyresults[-nas,1:2,] <- chcoord
        
      } else {
        
        ch1 <- chillsubmin[pixels.sps.i]
        ch2 <- chillsubmax[pixels.sps.i]
        ff <- forcesub[pixels.sps.i]

        # add coordinates and names
        chcoord <- coordinates(ras.numpixels)[pixels.sps.i,]
        yearlyresults[,1:2,] <- chcoord
        
      }
      
      # build final data to extract climate for chilling and forcing 
      ch1 <- cbind(chcoord,ch1[,1:ncol(ch1)])
      ch2 <- cbind(chcoord,ch2[,1:ncol(ch2)])
      ff <- cbind(chcoord,ff[,1:ncol(ff)])
      
      
      
      ## dates in data
      datesch <- as.Date(colnames(ch1),format="X%Y.%m.%d")[3:ncol(ch1)]
      datesff <- as.Date(colnames(ff),format="X%Y.%m.%d")[3:ncol(ff)]
      
      
      
      ## calculate chilling (Utah) and Forcing across the period
      
      ## forcing
      
      #gddseachcelleachday <- apply(ff[,3:ncol(ff)],2,function(x){
       # Tb <- 10
        #gdd <- ifelse((x-Tb)<0,0,x-Tb)
        #return(gdd)})
      #gddssum <- rowSums(gddseachcelleachday)
      forcing <- rowMeans(ff[,3:ncol(ff)])
      #hist(forcing)
      

      ## chilling
      #library(abind)
      minmaxtemp <- abind(ch1,ch2, along = 3)
      
      ## compute chilling
      nodata <- which(apply(minmaxtemp,1,function(x){return(ifelse(sum(is.na(x[,1:2]))>0,T,F))}))
      if(length(nodata)>0){minmaxtemp=minmaxtemp[-nodata,,]}
      
      chillunitseachcelleachday <- do.call(rbind,
                                           apply(minmaxtemp,1,function(x){
                                             #x <- minmaxtemp[300,,]
                                             #if(sum(is.na(x[3:nrow(x),2]))<151){
                                             extracweathdf <- data.frame(
                                               Year=as.numeric(format(datesch,"%Y")),
                                               Month=as.numeric(format(datesch,"%m")),
                                               Day=as.numeric(format(datesch,"%d")),
                                               Tmax=x[3:nrow(x),2],
                                               Tmin=x[3:nrow(x),1]
                                             )
                                             weather <- fix_weather(extracweathdf)
                                             hourtemps <- stack_hourly_temps(weather,latitude=x[2])
                                             chll <- chilling(hourtemps,244,120)
                                             
                                             
                                             #}
                                             return(chll)
                                           }
                                           ))
      
      

      ## store results
      ## 
      if(length(nas)==0){
        
        yearlyresults[,3,which(period == j)] <- forcing
        yearlyresults[,4,which(period == j)] <- chillunitseachcelleachday$Utah_Model
      }
      
      if(length(nas)>0 & length(forcing)==length(chillunitseachcelleachday$Utah_Model)){  

        yearlyresults[-nas,3,which(period == j)] <- forcing
        yearlyresults[-nas,4,which(period == j)] <- chillunitseachcelleachday$Utah_Model
      }
      if(length(nas)>0 & length(forcing)>length(chillunitseachcelleachday$Utah_Model)){  
        
        yearlyresults[-nas,3,which(period == j)] <- forcing
        yearlyresults[-nas,4,which(period == j)][1:length(chillunitseachcelleachday$Utah_Model)] <- chillunitseachcelleachday$Utah_Model
      }
      
      
    }    
    
    minmaxtemps.eachsps[[i]] <- yearlyresults
    
  }  
  
  return(minmaxtemps.eachsps)
  
}


## apply extracting function ----

## apply function (beware this function takes a couple mins per year, consider 
## parallelizing)
Climate.in.range.list<-list()
for(i in 1:length(spslist)){
  Climate.in.range.list[[i]]<-extractchillforce(spslist[i],tmin,tmax,tavg,period)
}

length(Climate.in.range.list)
save(Climate.in.range.list,file = "../phylogeny/output/Forcechill.in.range.EUsp.RData")
#load("output/Climate.in.range.EUspFULLstvfinal.RData")
#load("~/Desktop/Climate.in.range.EUspFULL.RData")


## get each pixels areas to weight data aggregate ----

ras.numpixels <- tmin[[1]]
values(ras.numpixels) <- 1:ncell(ras.numpixels)

## get each pixel area to weight in climate values and normalize (1,2)
ras.areas <- area(ras.numpixels)
values(ras.areas) <- 1+(values(ras.areas) - min(values(ras.areas),na.rm=T))/
  (max(values(ras.areas),na.rm=T)-min(values(ras.areas),na.rm=T))

hist(values(ras.areas))


## Code to save individual species ----
#Climate.in.range.list[[3]][[1]][,,1]
#j=1
for(j in 1:length(Climate.in.range.list)){  
  sps.1 <- as.data.frame(Climate.in.range.list[[j]][[1]][,,1])


to.rem.nas <- which(apply(sps.1,1,function(x)sum(is.na(x)))>5)
if(length(to.rem.nas)>0){
  sps.1 <- sps.1[-to.rem.nas,]
}

sps.1$year<-1980
sps.1$ID<-1:nrow(sps.1)
sps.1$weights <- extract(ras.areas,sps.1[,1:2])


for(i in 2:37){
  print(paste(j,i))
  temp.sps<-as.data.frame(Climate.in.range.list[[j]][[1]][,,i])
  to.rem.nas.j <- which(apply(temp.sps,1,function(x)sum(is.na(x)))>5)
  if(length(to.rem.nas.j)>0){
    temp.sps <- temp.sps[-to.rem.nas.j,]
  }
  temp.sps$year<-c(1980:2016)[i]
  temp.sps$ID<-1:nrow(temp.sps)
  temp.sps$weights <- extract(ras.areas,temp.sps[,1:2])
  sps.1<-rbind(sps.1,temp.sps)
  
}

namesave <- paste("output/",spslist[j],"_fullextract_stv.csv",sep="")
write.csv(sps.1,file = namesave)
}




## function to get data synthesis ----

## synthetize and summarize data geographically and temporally
library(collapse)


synth.data<-function(Climate.in.range.list,ras.areas){
  list.synthesis<-list()
  
  for(j in 1:length(Climate.in.range.list)){  #j=1
    sps.1<-as.data.frame(Climate.in.range.list[[j]][[1]][,,1])
    sps.1$year<-1980
    sps.1$weights <- extract(ras.areas,sps.1[,1:2])
    
    
    for(i in 2:37){#i=1
      print(paste(spslist[j],i))
      temp.sps<-as.data.frame(Climate.in.range.list[[j]][[1]][,,i])
      temp.sps$year<-c(1980:2016)[i]
      temp.sps$weights <- extract(ras.areas,temp.sps[,1:2])
      
      sps.1<-rbind(sps.1,temp.sps)
      
    }
    
    dat<-sps.1
    year1<-subset(dat,year==1980)
    dat$ID = paste(dat$x,dat$y,sep="_")
    #head(dat)
    
    years = unique(dat$year)
    nyears = length(years)
    dat$ID = paste(dat$x,dat$y)
    
    storing = array(NA, dim=c(5,4))
    row.names(storing) = colnames(dat)[3:7]
    colnames(storing) = c("Temp.Mean","Temp.SD", "Geo.Mean","Geo.SD")

    #collap(dat, c(species,nana) ~ year, fsd, 
    #w = weights, keep.w = T)
    #aggregate(dat[,-12],by=list(Year = dat$year), 
     #         FUN = function(z) mean(z, na.rm=TRUE))[1:3,]
    #collap(dat[,-12], ~ year, fmean)[1:3,]
    
    
    
    means.years <-     collap(dat[,-12], ~ year, fmean, 
                              w = ~ weights, keep.w = T)
    
    SDs.years <-     collap(dat[,-12], ~ year, fsd, 
                            w = ~ weights, keep.w = T)
    
    means.sites <-     collap(dat[,-12], ~ ID, fmean, 
                              w = ~ weights, keep.w = T)
    
    SDs.sites <-     collap(dat[,-12], ~ ID, fsd, 
                            w = ~ weights, keep.w = T)
    
    

    storing[,1] <- colMeans(means.years[,3:7], na.rm = T)
    storing[,2] <- colMeans(SDs.years[,3:7], na.rm = T)
    storing[,3] <- colMeans(means.sites[,3:7], na.rm = T)
    storing[,4] <- colMeans(SDs.sites[,3:7], na.rm = T)
    
    list.synthesis[[j]]<-storing
  }
  
  return(list.synthesis)
}


list.allsps<-synth.data(Climate.in.range.list,ras.areas)


## join values from the list and save
list.allspsjoint <- as.data.frame(do.call(rbind,list.allsps))
list.allspsjoint$species <- sort(rep(spslist,5))
list.allspsjoint$variable <- rep(row.names(list.allspsjoint)[1:5],22)
head(list.allspsjoint)
#write.csv(list.allspsjoint,file = "output/Synthesis_climate_EUsps_STVfinal.csv")
euspssynth = read.csv("output/Synthesis_climate_EUsps_STVfinal.csv")



head(list.allspsjoint)


## plot geographic vs. temporal variation ----

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


## get data ready to make forecasts 
load("../phylogeny/output/Forcechill.in.range.EUsp.RData")





## end ----