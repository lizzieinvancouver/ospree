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



## load climate data rasters (these data are not currently in the ospree folder 
## as they are heavy - 2.2Gb) - E-OBS at http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.50regular/catalog.html
tmin<-brick("~/Data_Harvard/EU trees/tn_0.50deg_reg_v17.0.nc", varname="tn", sep="")
tmax<-brick("~/Data_Harvard/EU trees/tx_0.50deg_reg_v17.0.nc", varname="tx", sep="")


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


## set function
extractchillforce<-function(spslist,tmin,tmax,period){
  
  ## define array to store results
  nsps<-length(spslist)
  nyears<-length(period)
  minmaxtemps.eachsps <- list()
  
  
  ## subset climate years
  yearsinclim<-as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
  
  ras.numpixels<-tmin[[1]]
  values(ras.numpixels)<-1:ncell(ras.numpixels)
  
  
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
    spsshapeproj<-spTransform(spsshape,proj4string(ras.numpixels))
    
    # get list of pixels to extract data (speeds things up)
    pixels.sps.i<-unique(sort(unlist(extract(ras.numpixels,spsshapeproj))))
    npix<-length(pixels.sps.i) # number of pixels
    
    # create an array to store results
    yearlyresults<-array(NA,dim=c(npix,9,length(period)))
    colnames(yearlyresults)<-c("x","y",
                               "GDD","GDD.lastfrost",
                               "DayLastFrost","MeanTmins","SDev.Tmins",
                               "Mean.Chill.Utah","Mean.Chill.Portions")
    
    
    
    ## loop across years to extract each years averages and stddevs
    
    for(j in period){#j=1980
      print(paste(i,j))
      
      ## load two consecutive years each time
      yearsinperiod<-which(yearsinclim%in%c(j,j+1))
      climsub.min<-subset(tmin,yearsinperiod)
      climsub.max<-subset(tmax,yearsinperiod)
      
      ## subset climate by months & days (10th Oct - 28Feb; 1Jan - 31st May)
      chillsub1<-subset(climsub.min,274:424)
      chillsub2<-subset(climsub.max,274:424)
      forcesub1<-subset(climsub.min,1:151)
      forcesub2<-subset(climsub.max,1:151)
      
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
      chcoord<-coordinates(ras.numpixels)[pixels.sps.i[-nas],]
      yearlyresults[-nas,1:2,]<-chcoord
      
      } else {
        
        ch<-chillsub1[pixels.sps.i]
        ch2<-chillsub2[pixels.sps.i]
        ff<-forcesub1[pixels.sps.i]
        ff2<-forcesub2[pixels.sps.i]
        
        # add coordinates and names
        chcoord<-coordinates(ras.numpixels)[pixels.sps.i,]
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


## apply function (beware this function takes a couple mins per year, consider 
## parallelizing)
Climate.in.range.list<-list()
for(i in 1:length(spslist)){
  Climate.in.range.list[[i]]<-extractchillforce(spslist[i],tmin,tmax,period)
}

#save(Climate.in.range.list,file = "output/Climate.in.range.EUspFULL.RData")
#load("output/Climate.in.range.EUspFULL.RData")

ff<-extractchillforce(spslist[3],tmin,tmax,period)
ff[[1]][,,1]

## Code to save individual species
#Climate.in.range.list[[1]][[1]][,,20]
#j=3
for(j in 1:length(Climate.in.range.list)){  
sps.1 <- as.data.frame(Climate.in.range.list[[j]][[1]][,,1])
sps.1 <- as.data.frame(Climate.in.range.list[[j]][[1]][,,15])

to.rem.nas <- which(apply(sps.1,1,function(x)sum(is.na(x)))>6)
if(length(to.rem.nas)>0){
  sps.1 <- sps.1[-to.rem.nas,]
}

sps.1$year<-1980
sps.1$ID<-1:nrow(sps.1)
  
for(i in 2:37){
  print(paste(j,i))
  temp.sps<-as.data.frame(Climate.in.range.list[[j]][[1]][,,i])
  to.rem.nas.j <- which(apply(temp.sps,1,function(x)sum(is.na(x)))==9)
  if(length(to.rem.nas.j)>0){
  temp.sps <- temp.sps[-to.rem.nas.j,]
  }
  temp.sps$year<-c(1980:2016)[i]
  temp.sps$ID<-1:nrow(temp.sps)
  sps.1<-rbind(sps.1,temp.sps)
  
}

namesave <- paste("output/",spslist[j],"_fullextract.csv",sep="")
write.csv(sps.1,file = namesave)
}


#spslist
#write.csv(sps.1,file = "output/Abies_alba_fullextract.csv")



## synthetize and summarize data geographically and temporally
#dat = read.csv("~/GitHub/ospree/analyses/ranges/output/Abies_alba_fullextract.csv")

synth.data<-function(Climate.in.range.list){
  list.synthesis<-list()
  
  for(j in 1:length(Climate.in.range.list)){  #j=1
    sps.1<-as.data.frame(Climate.in.range.list[[j]][[1]][,,1])
    sps.1$year<-1980
    
    for(i in 2:37){
      print(paste(spslist[j],i))
      temp.sps<-as.data.frame(Climate.in.range.list[[4]][[1]][,,i])
      temp.sps$year<-c(1980:2016)[i]
      sps.1<-rbind(sps.1,temp.sps)
      
    }
    
  dat<-sps.1
  year1<-subset(dat,year==1980)
  
  
  years = unique(dat$year)
  nyears = length(years)
  dat$ID = paste(dat$x,dat$y)
  
  storing = array(NA, dim=c(7,4))
  row.names(storing) = colnames(dat)[3:9]
  colnames(storing) = c("Geo.Mean","Geo.SD","Temp.Mean","Temp.SD")
  
  
  means.years <- aggregate(dat,by=list(Year = dat$year),FUN = mean,na.rm=T)
  SDs.years <- aggregate(dat,by=list(Year = dat$year),FUN = sd,na.rm=T)
  means.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = mean,na.rm=T)
  SDs.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = sd,na.rm=T)
  
  storing[,1] <- colMeans(means.years[,4:10], na.rm = T)
  storing[,2] <- colMeans(SDs.years[,4:10], na.rm = T)
  storing[,3] <- colMeans(means.sites[,4:10], na.rm = T)
  storing[,4] <- colMeans(SDs.sites[,4:10], na.rm = T)
  
  list.synthesis[[j]]<-storing
  }
  
  return(list.synthesis)
}


list.allsps<-synth.data(Climate.in.range.list)


## join values from the list and save
list.allspsjoint <- as.data.frame(do.call(rbind,list.allsps))
list.allspsjoint$species <- sort(rep(spslist,7))
list.allspsjoint$variable <- rep(row.names(list.allspsjoint)[1:7],22)

write.csv(list.allspsjoint,file = "output/Synthesis_climate_EUsps.csv")

head(list.allspsjoint)
## plot geographic vs. temporal variation

seqgdd<-seq(1,nrow(list.allspsjoint),7)
seqgdd.frost<-seq(2,nrow(list.allspsjoint),7)
seqmintemp<-seq(4,nrow(list.allspsjoint),7)

plot(list.allspsjoint$Geo.Mean[seqgdd],
     list.allspsjoint$Temp.Mean[seqgdd],xlim=c(360,430),ylim=c(360,430),
     pch=16,xlab="GDD - geographic mean",ylab="GDD - temporal mean")


plot(list.allspsjoint$Geo.Mean[seqgdd.frost],
     list.allspsjoint$Temp.Mean[seqgdd.frost],#xlim=c(360,430),ylim=c(360,430),
     pch=16,xlab="GDD last frost - geographic mean",ylab="GDD last frost - temporal mean")

plot(list.allspsjoint$Geo.Mean[seqmintemp],
     list.allspsjoint$Temp.Mean[seqmintemp],xlim=c(-3,-2),
     pch=16,xlab="Min Temp - geographic mean",ylab="Min Temp - temporal mean")
abline(a=0,b=1,lty=2,col='red')



### plotting a few example species
# get the file address for target file
# spsi = "Abies_alba"

# load shapefile
spsshape <- shapefile("~/GitHub/ospree/analyses/ranges/chorological_maps_dataset/Abies alba/shapefiles/Abies_alba_plg.shp")

## need to re-project shape from lamber equal area to geographic
## 
spsshapeproj<-spTransform(spsshape,proj4string(tmin[[1]]))
## code to plot within range climate interannual variation

dat = read.csv("~/GitHub/ospree/analyses/ranges/output/Abies_alba_fullextract.csv")
dat = as.data.frame(na.omit(dat))
unique(paste(dat$x,dat$y))

means.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = mean,na.rm=T)
SDs.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = sd,na.rm=T)

dev.off()
par(mfrow=c(2,3))
for(i in c(5,6,8:11)){
  
  cols1<-colorRampPalette(brewer.pal(9,"RdYlBu"))(100)[as.numeric(cut(-SDs.sites[,i],breaks = 100))]
  
  plot(means.sites$x,means.sites$y,col=cols1,pch=16,cex=1.5,
       main=colnames(SDs.sites)[i])
  lines(spsshapeproj)
}





## saving outputs
#save(Climate.in.range, file = paste("output/climate.in.range",
 #                                   period[1],max(period),"RData",sep="."))

## remove aux unnecessary files
unlink("chorological_maps_dataset/*", recursive = T)




## load packages
library('raster')
library('ncdf4')
library('abind')
library('chillR')

dat = read.csv("~/GitHub/ospree/analyses/ranges/output/Abies_alba_fullextract.csv")

year1<-subset(dat,year==1981)
head(year1)
dev.off()
plot(year1$x,year1$y)
lines(spsshapeproj,col='red')

synth.data<-function(dat){
  
  years = unique(dat$year)
  nyears = length(years)
  dat$ID = rep(1:nrow(year1),nyears)
  
  storing = array(NA, dim=c(7,4))
  row.names(storing) = colnames(dat)[4:10]
  colnames(storing) = c("Geo.Mean","Geo.SD","Temp.Mean","Temp.SD")
  
  
  means.years <- aggregate(dat,by=list(Year = dat$year),FUN = mean,na.rm=T)
  SDs.years <- aggregate(dat,by=list(Year = dat$year),FUN = sd,na.rm=T)
  means.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = mean,na.rm=T)
  SDs.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = sd,na.rm=T)
  
  storing[,1] <- colMeans(means.years, na.rm = T)[5:11]
  storing[,2] <- colMeans(SDs.years, na.rm = T)[5:11]
  storing[,3] <- colMeans(means.sites, na.rm = T)[5:11]
  storing[,4] <- colMeans(SDs.sites, na.rm = T)[5:11]
  
  return(storing)
}


sps.1<-synth.data(dat)




## load climate data rasters (these data are not currently in the ospree folder 
tmin<-brick("~/Data_Harvard/EU trees/tn_0.50deg_reg_v17.0.nc", varname="tn", sep="")


# get the file address for target file
# spsi = "Abies_alba"

# load shapefile
spsshape <- shapefile("~/GitHub/ospree/analyses/ranges/chorological_maps_dataset/Abies alba/shapefiles/Abies_alba_plg.shp")

## need to re-project shape from lamber equal area to geographic
## 
spsshapeproj<-spTransform(spsshape,proj4string(tmin[[1]]))


## code to plot within range climate interannual variation
means.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = mean,na.rm=T)
SDs.sites <- aggregate(dat,by=list(Year = dat$ID),FUN = sd,na.rm=T)

par(mfrow=c(2,3))
for(i in c(5,6,8:11)){
  
  cols1<-colorRampPalette(brewer.pal(9,"RdYlBu"))(100)[as.numeric(cut(-SDs.sites[,i],breaks = 100))]
  
  plot(means.sites$x,means.sites$y,col=cols1,pch=16,cex=1.5,
       main=colnames(SDs.sites)[i])
  lines(spsshapeproj)
}

par(mfrow=c(2,3))
for(i in c(5,6,8:11)){
  
  cols1<-colorRampPalette(brewer.pal(9,"RdYlBu"))(100)[as.numeric(cut(-SDs.years[,i],breaks = 100))]
  
  plot(means.years$x,means.years$y,#col=cols1,pch=16,cex=1.5,
       main=colnames(SDs.sites)[i])
  lines(spsshapeproj)
}





