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

#tavg<-brick("~/Data_Harvard/EU trees/tg_0.25deg_reg_v17.0.nc", varname="tg", sep="")
#tavg<-brick("~/tg_0.25deg_reg_v17.0.nc", varname="tg", sep="")


## read in distribution data
trees<-read.csv("data/distributiondata/EU trees/Trees_EU_116sps.csv")

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
  chillforcespsyears<-array(NA,dim=c(nyears,4,nsps))
  row.names(chillforcespsyears)<-period
  colnames(chillforcespsyears)<-c("Mean.Chill","SDev.Chill","Mean.GDD",
                                  "SDev.GDD.sites")
  #dimnames(chillforcespsyears)<-spslist
  
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
  for (i in 1:nsps){#i=2
    print(i)
    spsi<-spslist[i]
    fullnamei<-fullnames[i]
    
    ## load shape
    
    
    if(spsi=="FagSyl"){
    spsshape<-shapefile(paste("data/distributiondata/",
                              fullnamei,"/","shapefiles/",
                              paste(gsub(" ","_",fullnamei),"_sylvatica_plg.shp",sep="")
                              ,sep=""))
    
    } else {
      direct<-dir(paste("data/distributiondata/",
                        fullnamei,"/","shapefiles/",sep=""))
      #directname<-direct[which(grepl(direct,fullnamei))]
      spsshape<-shapefile(paste("data/distributiondata/",
                                fullnamei,"/","shapefiles/",
                                paste(gsub(" ","_",fullnamei),"_plg.shp",sep="")
                                ,sep=""))  
    }
    
    ## need to re-project shape from lamber equal area to geographic
    ## 
    spsshapeproj<-spTransform(spsshape,proj4string(chillsub[[1]]))
    
    ## loop across years to extract each years averages and stddevs
    # save first an array to store results
    yearlyresults<-array(NA,dim=c(length(period),4))
      
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
      gddseachcelleachday<-apply(ff,2,function(x){
        Tb<-10
        gdd<-ifelse((x-Tb)<0,0,x-Tb)
        return(gdd)})
      gddssum<-rowSums(gddseachcelleachday)
      #hist(gddssum)
      
      chillunitseachcelleachday<-apply(ch,2,function(x){
        tlow=-5 ## not sure about which parameteres we are using
        thigh=5
        minns<-ifelse((thigh-x)>(thigh-tlow),thigh-tlow,thigh-x)
        utah<-ifelse(minns>0,minns,0)
        return(utah)})
      utahssum<-rowSums(chillunitseachcelleachday)
      #hist(utahssum)
      
      
      yearlyresults[which(period==j),1]<-mean(gddssum,na.rm=T)
      yearlyresults[which(period==j),2]<-sd(gddssum,na.rm=T)
      yearlyresults[which(period==j),3]<-mean(utahssum,na.rm=T)
      yearlyresults[which(period==j),4]<-sd(utahssum,na.rm=T)
      
      }
    
    chillforcespsyears[,,i]<-yearlyresults
    
    }  
  
  return(chillforcespsyears)

}


## apply function
Climate.in.range<-extractchillforce(spslist,fullnames,trees,tavg,period)

write.csv(Climate.in.range,file = "analyses/ranges/climate.in.range1980-20176sps.csv")

#plots
par(mfrow=c(3,2))
for(i in 1:6){
plot(1980:2017,Climate.in.range[,1,i],"l",
     main=spslist[i],ylab="mean GDD within range")
}

## the above function extracts forcing and chilling 
## it takes a bit of time per species
## the parameters to compute chilling and gdds should be reviewed:
## for GDD I used Tb=10
## for Utah units I used Thigh=5 ; Tlow=-5
## Still lacking code to extract centroids in environmental space
## The output table yields yearly mean and sd values of chilling and 
## forcing across the range computed out of daily values within
## the Oct-Feb period (chilling) and March-May period (forcing)
## Do we want further results?














