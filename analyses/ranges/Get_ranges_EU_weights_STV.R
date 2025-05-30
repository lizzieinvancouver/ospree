## Script to extract geographic distribution for European Tree species out of
## published shapefiles  
## 
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


## load climate data rasters (these data are not currently in the ospree folder 
## as they are heavy - 2.2Gb) - E-OBS at http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.50regular/catalog.html
tmin<-brick("D:/Data_Harvard/EU trees/tn_0.50deg_reg_v17.0.nc", varname="tn", sep="")
tmax<-brick("D:/Data_Harvard/EU trees/tx_0.50deg_reg_v17.0.nc", varname="tx", sep="")

#tmin <- brick("~/Desktop/Big Data Files/tn_0.25deg_reg_v16.0.nc")
#tmin <- brick("~/Desktop/Big Data Files/tx_0.25deg_reg_v16.0.nc")

## load species list (beware this list has both species and complexes and is not the final one
## please change accordingly to read final species list)
species.list <- read.csv("../phylogeny/input/spslist.csv")
species.list <- sort(species.list$Species_binomial)


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
period<-1980:2016
#period<-2009:2010
#spslist<-spslist[1]

## set function (modified to include weighting already - has not been run)
extractchillforce <- function(spslist,tmin,tmax,period){
  
  ## define array to store results
  nsps <- length(spslist)
  nyears <- length(period)
  minmaxtemps.eachsps <- list()
  
  
  ## subset climate years
  yearsinclim <- as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
  
  ras.numpixels <- tmin[[1]]
  values(ras.numpixels) <- 1:ncell(ras.numpixels)
  
  ## get each pixel area to weight in climate values and normalize (1,2)
  ras.areas <- area(ras.numpixels)
  values(ras.areas) <- (values(ras.areas) - min(values(ras.areas),na.rm=T))/
    (max(values(ras.areas),na.rm=T)-min(values(ras.areas),na.rm=T))+1
  
  
  
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
    area.pixels.sps.i <- values(ras.areas)[pixels.sps.i]
    
    # create an array to store results
    yearlyresults <- array(NA,dim=c(npix,7,length(period)))
    colnames(yearlyresults) <- c("x","y",
                                 "GDD","GDD.lastfrost",
                                 "DayLastFrost","MeanTmins","SDev.Tmins")
    
    
    
    ## loop across years to extract each years averages and stddevs
    
    for(j in period){#j=1980
      print(paste(i,j))
      
      ## load two consecutive years each time
      yearsinperiod <- which(yearsinclim%in%c(j,j+1))
      climsub.min <- subset(tmin,yearsinperiod)
      climsub.max <- subset(tmax,yearsinperiod)
      
      ## subset climate by months & days (10th Oct - 28Feb; 1Jan - 31st May)
      chillsub1 <- subset(climsub.min,274:424)
      chillsub2 <- subset(climsub.max,274:424)
      forcesub1 <- subset(climsub.min,1:151)
      forcesub2 <- subset(climsub.max,1:151)
      stvsub1 <- subset(climsub.min,60:151)
      stvsub2 <- subset(climsub.max,60:151)
      
      ## find if there are NAs in some pixels (due to overlap with lakes or sea)
      nas <- which(is.na(values(forcesub1)[pixels.sps.i]))
      
      
      ## remove NAs if necessary
      if(length(nas)>0){
        # extract values and format to compute means and sdevs
        ch <- chillsub1[pixels.sps.i][-nas,]*area.pixels.sps.i[-nas]
        ch2 <- chillsub2[pixels.sps.i][-nas,]*area.pixels.sps.i[-nas]
        ff <- forcesub1[pixels.sps.i][-nas,]*area.pixels.sps.i[-nas]
        ff2 <- forcesub2[pixels.sps.i][-nas,]*area.pixels.sps.i[-nas]
        stv <- stvsub1[pixels.sps.i][-nas,]*area.pixels.sps.i[-nas]
        stv2 <- stvsub2[pixels.sps.i][-nas,]*area.pixels.sps.i[-nas]
        
        # add coordinates and names
        chcoord <- coordinates(ras.numpixels)[pixels.sps.i[-nas],]
        yearlyresults[-nas,1:2,] <- chcoord
        
      } else {
        
        ch <- chillsub1[pixels.sps.i]*area.pixels.sps.i
        ch2 <- chillsub2[pixels.sps.i]*area.pixels.sps.i
        ff <- forcesub1[pixels.sps.i]*area.pixels.sps.i
        ff2 <- forcesub2[pixels.sps.i]*area.pixels.sps.i
        stv <- stvsub1[pixels.sps.i]*area.pixels.sps.i
        stv2 <- stvsub2[pixels.sps.i]*area.pixels.sps.i
        
        # add coordinates and names
        chcoord <- coordinates(ras.numpixels)[pixels.sps.i,]
        yearlyresults[,1:2,] <- chcoord
        
      }
      
      # build final data to extract climate for chilling and forcing 
      ch <- cbind(chcoord,ch[,1:ncol(ch)])
      ch2 <- cbind(chcoord,ch2[,1:ncol(ch2)])
      ff <- cbind(chcoord,ff[,1:ncol(ff)])
      ff2 <- cbind(chcoord,ff2[,1:ncol(ff2)])
      stv <- cbind(chcoord,stv[,1:ncol(stv)])
      stv2 <- cbind(chcoord,stv2[,1:ncol(stv2)])
      
      
      # correct if row numbers do not agree
      if(nrow(ch)!=nrow(ch2)){
        namcoo1 <- apply(chcoord,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        namcoo2 <- apply(chcoord2,1,function(x){return(paste(x[1],x[2],sep="_"))})  
        
        torem <- which(!namcoo1%in%namcoo2)
        torem2 <- which(!namcoo2%in%namcoo1)
        
        if(length(torem)>0){
          ch=ch[-torem,]    
        }
        
        if(length(torem2)>0){
          ch2=ch2[-torem2,]    
        }
      }
      
      ## dates in data
      datesch <- as.Date(colnames(ch),format="X%Y.%m.%d")[3:ncol(ch)]
      
      
      
      ## calculate chilling (Utah) and GDD across the period
      
      ## GDDs
      gddseachcelleachday <- apply(ff2[,3:ncol(ff2)],2,function(x){
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
      
      ff3 <- cbind(last.frost,ff2[,-c(1:2)])
      
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
      minmaxtemp <- abind(ch,ch2, along = 3)
      
      #for(i in 1:366){print(sum(is.na(minmaxtemp[i,,])))}
      
      
      
      ## store results
      ## 
      if(length(nas)==0){
        
        yearlyresults[,3,which(period == j)] <- gddssum
        yearlyresults[,4,which(period == j)] <- gddssumlastfrost
        yearlyresults[,5,which(period == j)] <- last.frost
        yearlyresults[,6,which(period == j)] <- rowMeans(stv,na.rm=T)
        yearlyresults[,7,which(period == j)] <- apply(stv,1,sd,na.rm=T)
        
      } else {
        
        
        yearlyresults[-nas,3,which(period == j)] <- gddssum
        yearlyresults[-nas,4,which(period == j)] <- gddssumlastfrost
        yearlyresults[-nas,5,which(period == j)] <- last.frost
        yearlyresults[-nas,6,which(period == j)] <- rowMeans(stv,na.rm=T)
        yearlyresults[-nas,7,which(period == j)] <- apply(stv,1,sd,na.rm=T)
        
      }
    }
    
    minmaxtemps.eachsps[[i]] <- yearlyresults
    
  }  
  
  return(minmaxtemps.eachsps)
  
}


## apply function (beware this function takes a couple mins per year, consider 
## parallelizing)
Climate.in.range.list<-list()
for(i in 1:length(spslist)){
  Climate.in.range.list[[i]]<-extractchillforce(spslist[i],tmin,tmax,period)
}

#save(Climate.in.range.list,file = "output/Climate.in.range.EUspFULLstv.RData")
load("output/Climate.in.range.EUspFULLstv.RData")
#load("~/Desktop/Climate.in.range.EUspFULL.RData")

## corrections
##ff<-extractchillforce(spslist[13],tmin,tmax,period)
##Climate.in.range.list[[13]] <- ff


for(i in 1:length(Climate.in.range.list)){
  print(paste(i,
              sum(is.na(Climate.in.range.list[[i]][[1]][,3,1]))==nrow(Climate.in.range.list[[i]][[1]][,,1])))
  
}



ras.numpixels <- tmin[[1]]
values(ras.numpixels) <- 1:ncell(ras.numpixels)

## get each pixel area to weight in climate values and normalize (1,2)
ras.areas <- area(ras.numpixels)
values(ras.areas) <- (values(ras.areas) - min(values(ras.areas),na.rm=T))/
  (max(values(ras.areas),na.rm=T)-min(values(ras.areas),na.rm=T))




## Code to save individual species
#Climate.in.range.list[[3]][[1]][,,1]
#j=1
for(j in 1:length(Climate.in.range.list)){  
  sps.1 <- as.data.frame(Climate.in.range.list[[j]][[1]][,,1])
}

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
#}


#spslist
#write.csv(sps.1,file = "output/Abies_alba_fullextract.csv")



## synthetize and summarize data geographically and temporally
#dat = read.csv("~/GitHub/ospree/analyses/ranges/output/Abies_alba_fullextract.csv")

synth.data<-function(Climate.in.range.list,ras.areas){
  list.synthesis<-list()
  
  for(j in 1:length(Climate.in.range.list)){  #j=1
    sps.1<-as.data.frame(Climate.in.range.list[[j]][[1]][,,1])
    sps.1$year<-1980
    sps.1$weights <- extract(ras.areas,sps.1[,1:2])
    
    
    for(i in 2:37){
      print(paste(spslist[j],i))
      temp.sps<-as.data.frame(Climate.in.range.list[[4]][[1]][,,i])
      temp.sps$year<-c(1980:2016)[i]
      temp.sps$weights <- extract(ras.areas,temp.sps[,1:2])
      
      sps.1<-rbind(sps.1,temp.sps)
      
    }
    
    dat<-sps.1
    year1<-subset(dat,year==1980)
    #head(dat)
    
    years = unique(dat$year)
    nyears = length(years)
    dat$ID = paste(dat$x,dat$y)
    
    storing = array(NA, dim=c(5,4))
    row.names(storing) = colnames(dat)[3:7]
    colnames(storing) = c("Temp.Mean","Temp.SD", "Geo.Mean","Geo.SD")
    
    means.years <- aggregate(dat[,-12],by=list(Year = dat$year), FUN = function(z) mean(z, na.rm=TRUE)*mean(dat$weights, na.rm=TRUE))
    SDs.years <- aggregate(dat[,-12],by=list(Year = dat$year), FUN = function(z) sd(z, na.rm=TRUE)*mean(dat$weights, na.rm=TRUE))
    means.sites <- aggregate(dat[,-10],by=list(Year = dat$ID), FUN = function(z) mean(z, na.rm=TRUE)*mean(dat$weights, na.rm=TRUE)) ## Something is wrong here...
    SDs.sites <- aggregate(dat[,-10],by=list(Year = dat$ID), FUN = function(z) sd(z, na.rm=TRUE)*mean(dat$weights, na.rm=TRUE))
    
    if(FALSE){
      ### ADDED BY NACHO 17 NOVEMBER 2020: applying different weighting scheme based
      ### on grid-cells areas
      #weigh in variables before computing means
      datmult <- dat
      datmult[,3:9] <- dat[,3:9]*dat[,11]
      
      means.yearsa <- aggregate(datmult[-12],by=list(Year = datmult$year),
                                sum, na.rm=TRUE)
      means.yearsb <- means.yearsa[,4:10]/means.yearsa[,12]
      
      
      means.sitesa <- aggregate(datmult[-12],by=list(ID = datmult$ID),
                                sum, na.rm=TRUE)
      means.sitesb <- means.sitesa[,4:10]/means.sitesa[,12]
      
      ## here's where I got stuck as I didn't manage to find a straightforward way  
      ## to get weighted sds from aggregate, any help here is much appreciated 
      
      means.years <- aggregate(dat,by=list(Year = dat$year),FUN = mean,na.rm = T) 
      SDs.years <- aggregate(dat,by=list(Year = dat$year),FUN = sd,na.rm=T)
      means.sites <- aggregate(datmult,by=list(Year = dat$ID),FUN = mean,na.rm=T)
      SDs.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = sd,na.rm=T)
    }
    
    storing[,1] <- colMeans(means.years[,4:8], na.rm = T)
    storing[,2] <- colMeans(SDs.years[,4:8], na.rm = T)
    storing[,3] <- colMeans(means.sites[,4:8], na.rm = T)
    storing[,4] <- colMeans(SDs.sites[,4:8], na.rm = T)
    
    list.synthesis[[j]]<-storing
  }
  
  return(list.synthesis)
}


list.allsps<-synth.data(Climate.in.range.list,ras.areas)


## join values from the list and save
list.allspsjoint <- as.data.frame(do.call(rbind,list.allsps))
list.allspsjoint$species <- sort(rep(spslist,5))
list.allspsjoint$variable <- rep(row.names(list.allspsjoint)[1:5],22)

write.csv(list.allspsjoint,file = "output/Synthesis_climate_EUsps_STV.csv")

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




#'######################################
#### plotting a few example species ####
#'######################################

library(rworldmap)
library(RColorBrewer)

worldmap <- getMap(resolution="high") 
#plot(worldmap,col="grey",border="grey",xlim=c(-10,50),ylim=c(32,72))


## function to extract/correct the shape for a given species
getspsshape<-function(spslist,sps.num,ras.numpixels){
  
  i<-sps.num
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
  #
  
  return(spsshapeproj)
}

## examples of application
abialb <- getspsshape(spslist,1,tmin[[1]])
sorauc <- getspsshape(spslist,21,tmin[[1]])
cornmas <- getspsshape(spslist,9,tmin[[1]])



## plot shape with data on top
dir.fig = "figures/eu_sps_climate_maps/"
dir.out <- "~/GitHub/ospree/analyses/ranges/output/"

plot.shape.data<-function(spsshape,sps.name,
                          dir.out,dir.fig,
                          type=c("means","sds")){
  
  #sps.name<-spslist[3]
  
  ## plot base map + range map
  extent.sps.i <- extent(spsshape)+3
  
  if(extent.sps.i[2]>50){extent.sps.i[2] = 50}
  if(extent.sps.i[3]<32){extent.sps.i[3] = 32}
  
  ## retrieve and format data
  ## code to plot within range climate interannual variation
  #dir.out <- "~/GitHub/ospree/analyses/ranges/output/"
  files.out <- dir(dir.out)
  
  sps.out <- files.out[which(grepl(sps.name,files.out)&grepl("fullextract",files.out))]
  
  dat = read.csv(paste(dir.out,sps.out,sep=""))
  dat = as.data.frame(na.omit(dat))
  
  ### Now we need to get the area weighted average across grid cells. See Issue #387
  sps.area <- area(spsshape / 10000)
  
  means.sites <- aggregate(dat,by=list(Year = dat$ID), FUN = function(x) sum(mean(x, na.rm=TRUE)*sps.area)/sum(sps.area))
  SDs.sites <- aggregate(dat,by=list(Year = dat$ID), FUN = function(x) sum(mean(x, na.rm=TRUE)*sps.area)/sum(sps.area))
  
  
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
  
  
  for(i in c(5,6,8:11)){#i=5
    
    
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
         paste(sps.name,colnames(SDs.sites)[i]),pos=2)
    plot(spsshape,col=adjustcolor('black',0),add=T,
         border=adjustcolor('black',0.5))
    
    points(means.sites$x,means.sites$y,col=cols1,pch=19,cex=0.8)
    
  }
  dev.off()
  
  
  
}

# example of usage
plot.shape.data(cornmas,spslist[9],dir.out,dir.fig,"means")


#### loop across species ####

for (i in 1:length(spslist)){
  
  print(spslist[i])
  
  spsshape <- getspsshape(spslist,i,tmin[[1]])
  
  plot.shape.data(spsshape,spslist[i],
                  dir.out,dir.fig,'sds')
  plot.shape.data(spsshape,spslist[i],
                  dir.out,dir.fig,'means')
  
}

dev.off()


## saving outputs
#save(Climate.in.range, file = paste("output/climate.in.range",
#                                   period[1],max(period),"RData",sep="."))

## remove aux unnecessary files
unlink("chorological_maps_dataset/*", recursive = T)


### Need to fix column names for synthesis data:
synthdat <- read.csv("output/Synthesis_climate_EUsps.csv")

colnames(synthdat) <- c("X", "Temp.Mean", "Temp.SD", "Geo.Mean", "Geo.SD", "species", "variable")

write.csv(synthdat, file="output/Synthesis_climate_EUsps.csv", row.names=FALSE)

