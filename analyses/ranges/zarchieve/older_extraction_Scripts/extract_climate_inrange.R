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
QuRo<-"QurRo1"#Quercus robur

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
  
  
  
  
  
  
  







