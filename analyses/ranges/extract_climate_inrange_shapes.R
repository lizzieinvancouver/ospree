## Script to extract daily climate for European Trees based on 
## locations.
# Started by Nacho
# Date: 4th Dec 2018


## to start
rm(list=ls())
options(stringsAsFactors=FALSE)


## load packages
library('raster')
library('ncdf4')


## setwd
setwd("~/GitHub/ospree/")


## read in climate variable (can be repeated for other variables)
## the resulting brick has XX layers corresponding to Avg temperatures 
## from 1950 to 2017 / you need to download/unzip the climate from:
##https://eca.knmi.nl/download/ensembles/data/Grid_0.25deg_reg/

tavg<-brick("~/Data_Harvard/EU trees/tg_0.25deg_reg_v17.0.nc", varname="tg", sep="")
tavg<-brick("~/tg_0.25deg_reg_v17.0.nc", varname="tg", sep="")


## read in distribution data
trees<-read.csv("data/distributiondata/EU trees/Trees_EU_116sps.csv")
unique()

## example to extract climate for one species "Betula Pendula" (can be
## replicated or looped to include any other species)

BePu<-"BetPub" # select Betula pubescnes
FaSy<-"FagSyl" #select Fagus
PiAb<-"PicAb1" #speciee picea
BePe<-"BetPen" #Betula pendula
CoAv<-"CorAve" #Corylus avenula
QuRo<-"QurRo1" #Quercus robur

## create a species list to apply the function to
spslist<-c("BetPub" # select Betula pubescnes
           ,"FagSyl" #select Fagus
           ,"PicAb1" #speciee picea
           ,"BetPen" #Betula pendula
           ,"CorAve" #Corylus avenula
           ,"QurRo1")

fullnames<-c("Betula pubescens","Fagus sylvatica","Picea abies", "Betula pendula",
             "Corylus avellana", "Quercus robur")


# define period
period<-1980:2017

## set function
extractchillforce<-function(spslist,fullnames,trees,tavg,period){

  ## define array to store results
  nsps<-length(spslist)
  nyears<-length(period)
  chillforcespsyears<-array(NA,dim=c(nsps,4,nyears))
  row.names(chillforcespsyears)<-spslist
  colnames(chillforcespsyears)<-c("Mean.Chill","SDev.Chill","Mean.GDD",
                                  "SDev.GDD.sites")
  ## subset climate years
  yearsinclim<-as.numeric(format(as.Date(names(tavg),format="X%Y.%m.%d"),"%Y"))
  yearsinperiod<-which(yearsinclim%in%period)
  climsub<-subset(tavg,yearsinperiod)
  
  ## subset climate days
  monthsinclim<-as.numeric(format(as.Date(names(climsub),format="X%Y.%m.%d"),"%m"))
  chillmonths<-c(10:12,1:2)
  forcingmonths<-c(3:5)
  monthsinchill<-which(monthsinclim%in%chillmonths)
  monthsinforce<-which(monthsinclim%in%forcingmonths)
  chillsub<-subset(climsub,monthsinchill)
  forcesub<-subset(climsub,monthsinforce)
  
  
  ## commence loop  
  for (i in 1:nsps){#i=1
    print(i)
    spsi<-spslist[i]
    fullnamei<-fullnames[i]
    
    ## load shape
    direct<-dir(paste("data/distributiondata/",
                      fullnamei,"/","shapefiles/",sep=""))
    directname<-direct[which(grepl(direct,fullnamei))]
    spsshape<-shapefile(paste("data/distributiondata/",
                              fullnamei,"/","shapefiles/",
                              paste(gsub(" ","_",fullnamei),"_plg.shp",sep="")
                              ,sep=""))
    
    ## need to re-project shape from lamber equal area to geographic
    ## 
    spsshapeproj<-spTransform(spsshape,proj4string(chillsub[[1]]))
    
    ## loop across years to extract each years averages and stddevs
    # save first an array to store results
    yearlyresults<-array(NA,dim=c(length(period),5))
      
    for(j in period){#j=1980
      print(paste(i,j))
      
      # select year's layer
      chillyears<-which(as.numeric(format(as.Date(
        names(chillsub),format="X%Y.%m.%d"),"%Y"))==j)
      forceyears<-which(as.numeric(format(as.Date(
        names(forcesub),format="X%Y.%m.%d"),"%Y"))==j)
      
      yearschill<-subset(chillsub,chillyears)
      yearsforce<-subset(forcesub,forceyears)
      
      # extract values and format to compute means and sdevs
      tempschills<-extract(yearschill,spsshapeproj)
      tempsforces<-extract(yearsforce,spsshapeproj)
      
      #turn into data frame and remove NAs
      ch<-do.call("rbind",tempschills)
      ch<-subset(ch,!is.na(rowSums(ch)))
      
      ff<-do.call("rbind",tempsforces)
      ff<-subset(ff,!is.na(rowSums(ff)))
      
      ## calculate chilling (Utah) and GDD across the period
      chillunitseachcelleachday<-t(apply(ff,1,function(x){
        utah<-x ## need to apply the correct version of the function to obtain chilling units
        return(utah)}))
      utahssum<-rowSums(gddseachcelleachday)
      
      gddseachcelleachday<-t(apply(ff,1,function(x){return(x-10)}))
      gddssum<-rowSums(gddseachcelleachday)
      
      
      
      }
    
    
    ## extract data for shapefile
    
    aa<-chillsub[[c(1:3)]]
    x=aa[[1]]
    calc(aa,fun=function(x){
      rr<-unlist(extract(x,spsshapeproj))
      return(rr)
     },forcefun=T)
    calc(aa,fun=function(x){
      function(x){x * 10}
      },na.rm=T)
    
    
    
    
    ?calc
    
    ff<-extract(chillsub[[c(1:3)]],spsshapeproj)
  str(ff)  
     
  }  
  
}

coords.FS<-subset(trees,select=c("X","Y",FaSy)) # subset to fagus 
coords.FS.no0<-coords.FS[which(coords.FS[,3]==1),1:2] # fagus yes's only

coords.PiAb<-subset(trees,select=c("X","Y",PiAb)) #subset data
coords.PiAb.no0<-coords.PiAb[which(coords.PiAb[,3]==1),1:2]

# extract climate for each day within the range of the species
# (this can take a while)

extracted.clim.spsi <- extract(tavg,coords.spsi.no0)



library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
#points(coords.spsi.no0$X, coords.spsi.no0$Y, col = "green",pch=20, cex=0.6)
points(coords.FS.no0$X, coords.FS.no0$Y, col = "red",pch=20,cex=0.6)
points(coords.PiAb.no0$X, coords.PiAb.no0$Y, col = "blue",pch=20,cex=0.6)

##ask nacho, is it better to work with ranges as a shapefile, polygon etc?


## to be completed if we only want a given set of dates...














