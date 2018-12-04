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
## https://eca.knmi.nl/download/ensembles/data/Grid_0.25deg_reg/

tavg<-brick("~/tg_0.25deg_reg_v17.0.nc", varname="tg", sep="")


## read in distribution data
trees<-read.csv("data/distributiondata/EU trees/Trees_EU_116sps.csv")


## example to extract climate for one species "Betula Pendula" (can be
## replicated or looped to include any other species)

spsi<-"BetPen" # select species

coords.spsi<-subset(trees,select=c("X","Y",spsi)) #subset data
coords.spsi.no0<-coords.spsi[which(coords.spsi[,3]==1),1:2] #remove zeros

# extract climate for each day within the range of the species
# (this can take a while)
extracted.clim.spsi <- extract(tavg,coords.spsi.no0)


## to be completed if we only want a given set of dates...
  
  
  
  
  
  
  







