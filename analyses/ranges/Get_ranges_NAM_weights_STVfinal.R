## Script to extract geographic distribution for NAmerican Tree species out of
## published shapefiles and climate data 
## 
# Started by Nacho
# Date: 12th June 2022
# Attempt at replicating Cat's results with Nacho's previous code



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

## load climate data ----
## load climate data rasters (these data are not currently in the ospree folder 
## as they are heavy - 2.2Gb) - E-OBS at http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.50regular/catalog.html
#tmin<-brick("D:/Data_Harvard/EU trees/tn_ens_mean_0.25deg_reg_v25.0e.nc", varname="tn", sep="")
#tmax<-brick("D:/Data_Harvard/EU trees/tx_ens_mean_0.25deg_reg_v25.0e.nc", varname="tx", sep="")
#tavg<-brick("D:/Data_Harvard/EU trees/tg_ens_mean_0.25deg_reg_v25.0e.nc", varname="tg", sep="")




climatedrive = "E:/princetonclimate/" # Cat's climate drive
climatedrive = "D:/Data_Harvard/princetonclimate/" # Cat's climate drive

nafiles <- dir(climatedrive)[grep("daily", dir(climatedrive))]

### Attempt to stack raster layers for princeton to maybe make more streamlined...
allclimyrs <- 1979:2016
e <- extent(190, 300, 15, 75)
namaxfiles <- dir(climatedrive)[grep("tmax_daily", dir(climatedrive))]
tmaxlist <- paste(climatedrive, namaxfiles, sep="")
  
 

#tmin <- brick("~/Desktop/Big Data Files/tn_0.25deg_reg_v16.0.nc")
#tmin <- brick("~/Desktop/Big Data Files/tx_0.25deg_reg_v16.0.nc")



## load species list ----
## please change accordingly to read final species list)

species.list <- read.csv("~/GitHub/ospree/analyses/output/masterspecieslist.csv")
species.list <- as.vector(species.list$x)


## read in list of species with distribution shapefiles
# get a list of the polygon shapefiles in the .zip with the maps
zipped_names <- grep('\\.shp', unzip("NA_range_files/NA_ranges.zip", list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

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




# define period
period<-1980:2016
#period<-2009:2010
#spslist<-spslist[1]


## define extracting function ----

## set function (modified to include weighting already - has not been run)
extractchillforce <- function(spslist,tmin,tmax,tavg,period){
  
  ## define array to store results
  nsps <- length(spslist)
  nyears <- length(period)
  minmaxtemps.eachyear <- list()
  
  
  ## subset climate years
  #yearsinclim <- as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
  allclimyrs <- 1979:2016
  e <- extent(190, 300, 15, 75)
  
  
  namaxfiles <- dir(climatedrive)[grep("tmax_daily", dir(climatedrive))]
  tmaxlist <- paste(climatedrive, namaxfiles, sep="")
  naminfiles <- dir(climatedrive)[grep("tmin_daily", dir(climatedrive))]
  tminlist <- paste(climatedrive, naminfiles, sep="")
  
  
  
  ## loop across years to extract each years averages and stddevs
  
  for(j in 1:37){#j=1
    print(j)
    minmaxtemps.eachyear[[j]] <- list()
    # Now, let's make sure all of the dataframes have the same column names
    tmax <- brick(tmaxlist[j+1])
    tmax <- crop(tmax, e) 
    ff <- rotate(tmax) 
    values(ff) <- values(ff)-273.15
    tmax <- ff
    
    tmin <- brick(tminlist[j+1])
    tmin <- crop(tmin, e) 
    ff <- rotate(tmin) 
    values(ff) <- values(ff)-273.15
    tmin <- ff
    

    ## subset climate by months & days (10th Oct - 28Feb; 1Jan - 31st May)
    #chillsub1 <- subset(climsub.min,274:424)
    #chillsub2 <- subset(climsub.max,274:424)
    forcesub1 <- subset(tmin,1:151)
    forcesub2 <- subset(tmax,1:151)
    forceavg <- forcesub2
    for(k in 1:151){
      print(k)
      forceavg[[k]] = (forcesub1[[k]]+forcesub2[[k]])/2
    }
    
    stvsub1 <- subset(tmin,60:151)
    stvsub2 <- subset(tmax,60:151)
    
    ras.numpixels <- stvsub1[[1]]
    values(ras.numpixels) <- 1:ncell(ras.numpixels)
    ## commence loop  
  
    
    for (i in c(1:7,9:18)){#i=9
    spsi<-spslist[i]
    print(spsi)
    
    #fullnamei<-fullnames[i]
    
    ## load shape
    
    
    # get the file address for target file
    zipped_name.i <- grep(paste(spsi,'.shp',sep=""), 
                          zipped_names, ignore.case = TRUE, value = TRUE)
    
    
    # load shapefile
    spsshape <- shapefile(zipped_name.i)[1]
    
    if(i==8){
      proj4string(spsshape) <- CRS("+proj=aea  
                                   +lat_1=38 +lat_2=42 +lat_0=40 +lon_0=-82 +x_0=0 +y_0=0
                                   +units=m +datum=NAD27 +ellps=clrk66 +no_defs")
    } else{
      proj4string(spsshape) <- CRS("+proj=longlat +init=epsg:4326")
    }
    
    spsshapeproj<-spTransform(spsshape,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 "))
    
    
    # get list of pixels to extract data (speeds things up)
    pixels.sps.i <- unique(sort(unlist(extract(ras.numpixels,spsshapeproj))))
    npix <- length(pixels.sps.i) # number of pixels
    #area.pixels.sps.i <- values(ras.areas)[pixels.sps.i]
    
    
    yearlyresults <- array(NA,dim=c(npix,7))
    colnames(yearlyresults) <- c("x","y",
                                 "GDD","GDD.lastfrost",
                                 "DayLastFrost","MeanTmins","SDev.Tmins")
    
      ## find if there are NAs in some pixels (due to overlap with lakes or sea)
      nas <- which(is.na(values(forcesub1)[pixels.sps.i]))
      
      
      ## remove NAs if necessary
        
        ff <- forcesub1[pixels.sps.i]
        ffavg <- forceavg[pixels.sps.i]
        stv <- stvsub1[pixels.sps.i]
        stv2 <- stvsub2[pixels.sps.i]
        
        # add coordinates and names
        chcoord <- coordinates(ras.numpixels)[pixels.sps.i,]
        yearlyresults[,1:2] <- chcoord
        
      
      
      # build final data to extract climate for chilling and forcing 
      ff <- cbind(chcoord,ff[,1:ncol(ff)])
      #ff2 <- cbind(chcoord,ff2[,1:ncol(ff2)])
      ffavg <- cbind(chcoord,ffavg[,1:ncol(ffavg)])
      stv <- cbind(chcoord,stv[,1:ncol(stv)])
      #stv2 <- cbind(chcoord,stv2[,1:ncol(stv2)])
      
      
      
      ## dates in data
      datesch <- as.Date(colnames(stv),format="X%Y.%m.%d")[3:ncol(stv)]
      
      
      
      ## calculate chilling (Utah) and GDD across the period
      
      ## GDDs
      gddseachcelleachday <- apply(ffavg[,3:ncol(ffavg)],2,function(x){
        Tb <- 10
        gdd <- ifelse((x-Tb)<0,0,x-Tb)
        return(gdd)})
      gddssum <- rowSums(gddseachcelleachday)
      #hist(gddssum)
      
      
      ## GDDs till day of last frost
      ## calculate last date of frost and GDD until then
      last.frost <- apply(ff[,3:ncol(ff)],1,function(x){
        a <- which(x<(-5))
        return(ifelse(length(a)>0,max(a),NA))}) 
      
      ff3 <- cbind(last.frost,ffavg[,-c(1:2)])
      
      gddseachcelleachdaylastfrost <- apply(ff3,1,function(x){
        #x <- ff3[922,]
        elems <- length(x)-1
        daylastfrost <- x[1]
        if(!is.na(daylastfrost) & daylastfrost>1){
          temps <- x[2:daylastfrost]
          Tb <- 10
          gdd <- ifelse((temps-Tb)<0,0,temps-Tb)
          gdd <- c(gdd,rep(0,elems-x[1]+1))
          names(gdd) <- colnames(ff3[,2:152])      
        } else {
          gdd <- rep(0,elems)
          names(gdd) <- colnames(ff3[,2:152])
        }
        return(gdd)})
      
      gddssumlastfrost <- rowSums(t(gddseachcelleachdaylastfrost),na.rm = T)
      
      #library(abind)
      #minmaxtemp <- abind(ch,ch2, along = 3)
      
      #for(i in 1:366){print(sum(is.na(minmaxtemp[i,,])))}
      
      
      
      ## store results
      ## 
      if(length(nas)==0){
        
        yearlyresults[,3] <- gddssum
        yearlyresults[,4] <- gddssumlastfrost
        yearlyresults[,5] <- last.frost
        yearlyresults[,6] <- rowMeans(stv,na.rm=T)
        yearlyresults[,7] <- apply(stv,1,sd,na.rm=T)
        
      } 
      
      minmaxtemps.eachyear[[j]][[i]]<- yearlyresults
    }
    

  }  
  
  return(minmaxtemps.eachyear)
  
}


## apply extracting function ----

## apply function (beware this function takes a couple mins per year, consider 
## parallelizing)

Climate.in.range.list <- extractchillforce(spslist,tmin,tmax,tavg,period)

#save(Climate.in.range.list,file = "output/Climate.in.range.NAMspFULLstvfinal.RData")
#load("output/Climate.in.range.EUspFULLstvfinal.RData")

## The dimensions of the object: 37 years (1980:2016) x 18 sps x [ncells,7vars]
dim(Climate.in.range.list[[1]][[1]])




## get each pixels areas to weight data aggregate ----

ras.numpixels <- tmax[[1]]
values(ras.numpixels) <- 1:ncell(ras.numpixels)

## get each pixel area to weight in climate values and normalize (1,2)
ras.areas <- area(ras.numpixels)
values(ras.areas) <- 1+(values(ras.areas) - min(values(ras.areas),na.rm=T))/
  (max(values(ras.areas),na.rm=T)-min(values(ras.areas),na.rm=T))

hist(values(ras.areas))


## The below code needs to be checked
if(FALSE){
## Code to save individual species ----
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

  
}



## function to get data synthesis ----
library(collapse)
synth.data<-function(Climate.in.range.list,ras.areas){
  list.synthesis<-list()
  
  for(i in c(1:7,9:18)){#i=1
    
    sps.1.name <- spslist[i]
    sps.1 <- as.data.frame(Climate.in.range.list[[1]][[i]])
    sps.1$year <- 1980
    weightss <- extract(ras.areas,sps.1[,1:2])
    sps.1$weights <- weightss
    
    for(j in 2:length(Climate.in.range.list)){  #j=1
      year.j = c(1980:2016)[j]
      print(paste(sps.1.name,year.j))
      
      temp.sps<-as.data.frame(Climate.in.range.list[[j]][[i]])
      temp.sps$year<-year.j
      temp.sps$weights <- weightss
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
    
    
    
    means.years <-     collap(dat, ~ year, fmean, 
                              w = ~ weights, keep.w = T)
    
    SDs.years <-     collap(dat, ~ year, fsd, 
                            w = ~ weights, keep.w = T)
    
    means.sites <-     collap(dat, ~ ID, fmean, 
                              w = ~ weights, keep.w = T)
    
    SDs.sites <-     collap(dat, ~ ID, fsd, 
                            w = ~ weights, keep.w = T)
    
    
    head(means.years)
    storing[,1] <- colMeans(means.years[,3:7], na.rm = T)
    storing[,2] <- colMeans(SDs.years[,3:7], na.rm = T)
    storing[,3] <- colMeans(means.sites[,3:7], na.rm = T)
    storing[,4] <- colMeans(SDs.sites[,3:7], na.rm = T)
    
    list.synthesis[[i]]<-storing
  }
  
  return(list.synthesis)
}


## synthetize and summarize data geographically and temporally ----

list.allsps<-synth.data(Climate.in.range.list,ras.areas)



## join values from the list and save
list.allspsjoint <- as.data.frame(do.call(rbind,list.allsps))
list.allspsjoint$species <- unlist(lapply(spslist[c(1:7,9:18)],function(x)rep(x,5)))
list.allspsjoint$variable <- rep(row.names(list.allspsjoint)[1:5],17)
head(list.allspsjoint)
write.csv(list.allspsjoint,file = "output/Synthesis_climate_NAMsps_STVfinal_nacho.csv")

head(list.allspsjoint)


## plot geographic vs. temporal variation ----

seqgdd<-seq(1,nrow(list.allspsjoint),5)
seqgdd.frost<-seq(2,nrow(list.allspsjoint),5)
seqmintemp<-seq(4,nrow(list.allspsjoint),5)

par(mfrow=c(1,3),mar=c(4,4,1,1))
plot(list.allspsjoint$Geo.Mean[seqgdd],
     list.allspsjoint$Temp.Mean[seqgdd],#xlim=c(360,430),ylim=c(360,430),
     xlab="GDD - geographic mean",ylab="GDD - temporal mean",
     pch=16,col=adjustcolor(1,0.4))
abline(a=0,b=1,col='red',lty=2)

plot(list.allspsjoint$Geo.Mean[seqgdd.frost],
     list.allspsjoint$Temp.Mean[seqgdd.frost],#xlim=c(360,430),ylim=c(360,430),
     xlab="GDD last frost - geographic mean",ylab="GDD last frost - temporal mean",
     pch=16,col=adjustcolor(1,0.4))
abline(a=0,b=1,col='red',lty=2)

plot(list.allspsjoint$Geo.Mean[seqmintemp],
     list.allspsjoint$Temp.Mean[seqmintemp],
     xlab="Min Temp - geographic mean",ylab="Min Temp - temporal mean",
     pch=16,col=adjustcolor(1,0.4))
abline(a=0,b=1,lty=2,col='red')

dev.off()



## end ----