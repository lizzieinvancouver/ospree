## Script to extract geographic distribution for European Tree species out of
## published shapefiles  
## 
# Started by Nacho
# Date: 23th Oct 2019


## to start
rm(list=ls())
options(stringsAsFactors=FALSE)



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


## load species list (beware this list has both species and complexes)
species.list <- read.csv("../phylogeny/input/spslist.csv")
species.list <- sort(species.list$Species_binomial)


## read in list of species with distribution shapefiles
dir("../../data/distributiondata/")

ff <- unzip("../../data/distributiondata/chorological_maps_dataset_20170220.zip")

aa <- ff[which(grepl(".shp",ff))][1]

sps.i <- unzip("../../data/distributiondata/chorological_maps_dataset_20170220.zip",
               files = aa)


## create a vector file names we want to extract from
files.temp <- c("EF2008C.zip","EF2006C.zip","EF2004C.zip","EF2002C.zip","EF2000C.zip","EF98_C.zip","EF1988_C.zip")

## create a loop to extract the files to the directory set above
## will build the file name of the extracted file
unzip("EF2008c.zip")
for (i in files.temp)
  unzip(i)

## a quicker way that doesnt require that you know which files - just does all
## \ allows you to use the . in .zip, the . is a special character
## $ is tells the pattern to search is the end? not sure about this one
for (i in dir(pattern="\.zip$"))
  unzip(i)


## clean up the memory and close
rm(list=ls())
q()
n



## example to extract climate for one species "Betula Pendula" (can be
## replicated or looped to include any other species)

BePu<-"BetPub" # select Betula pubescnes
FaSy<-"FagSyl" #select Fagus
PiAb<-"PicAb1" #speciee picea
BePe<-"BetPen" #Betula pendula
CoAv<-"CorAve" #Corylus avenula
QuRo<-"QurRo1" #Quercus robur

## create a species list to apply the function to
spslist<-c(#"BetPub" # select Betula pubescnes
           #,"FagSyl" #select Fagus
           "PicAb1" #speciee picea
           ,"BetPen" #Betula pendula
           ,"CorAve" #Corylus avenula
           ,"QurRo1")

fullnames<-c(#"Betula pubescens","Fagus sylvatica",
             "Picea abies", "Betula pendula",
             "Corylus avellana", "Quercus robur")

#shorter version with only BetPub and FagSyl
spslist<-c("BetPub" # select Betula pubescnes
           ,"FagSyl") #select Fagus

fullnames<-c("Betula pubescens","Fagus sylvatica")


# define period
period<-1980:2017
#period<-2009:2014

## set function
extractchillforce<-function(spslist,fullnames,trees,tmin,tmax,period){
  
  ## define array to store results
  nsps<-length(spslist)
  nyears<-length(period)
  chillforcespsyears<-array(NA,dim=c(nyears,6,nsps))
  row.names(chillforcespsyears)<-period
  colnames(chillforcespsyears)<-c("Mean.GDD",
                                  "SDev.GDD.sites","Mean.Chill.Utah","SDev.Chill.Utah",
                                  "Mean.Chill.Portions","SDev.Chill.Portions")
  #dimnames(chillforcespsyears)<-spslist
  
  ## subset climate years
  yearsinclim<-as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
  yearsinperiod<-which(yearsinclim%in%period)
  climsub.min<-subset(tmin,yearsinperiod)
  climsub.max<-subset(tmax,yearsinperiod)
  
  ## subset climate days
  monthsinclim<-as.numeric(format(as.Date(names(climsub.min),format="X%Y.%m.%d"),"%m"))
  chillmonths<-c(10:12,1:2)
  forcingmonths<-c(3:5)
  monthsinchill<-which(monthsinclim%in%chillmonths)
  monthsinforce<-which(monthsinclim%in%forcingmonths)
  chillsub1<-subset(climsub.min,monthsinchill)
  chillsub2<-subset(climsub.max,monthsinchill)
  
  forcesub<-subset(climsub.max,monthsinforce)
  
  ## commence loop  
  for (i in 1:nsps){#i=1
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
    spsshapeproj<-spTransform(spsshape,proj4string(chillsub1[[1]]))
    
    ## loop across years to extract each years averages and stddevs
    # save first an array to store results
    yearlyresults<-array(NA,dim=c(length(period),6))
    
    for(j in period){#j=1981
      print(paste(i,j))
      
      # select year's layer
      chillyears<-which(as.numeric(format(as.Date(
        names(chillsub1),format="X%Y.%m.%d"),"%Y"))==j)
      forceyears<-which(as.numeric(format(as.Date(
        names(forcesub),format="X%Y.%m.%d"),"%Y"))==j)
      
      yearschillmin<-subset(chillsub1,chillyears)
      yearschillmax<-subset(chillsub2,chillyears)
      
      yearsforce<-subset(forcesub,forceyears)
      
      # extract values and format to compute means and sdevs
      tempschillsmin<-extract(yearschillmin,spsshapeproj,cellnumbers=T)
      tempschillsmax<-extract(yearschillmax,spsshapeproj,cellnumbers=T)
      
      tempsforces<-extract(yearsforce,spsshapeproj,cellnumbers=T)
      
      #turn into data frame and remove NAs
      ch<-do.call("rbind",tempschillsmin)
      ch<-subset(ch,!is.na(rowSums(ch)))
      
      ch2<-do.call("rbind",tempschillsmax)
      ch2<-subset(ch2,!is.na(rowSums(ch2)))
      
      ff<-do.call("rbind",tempsforces)
      ff<-subset(ff,!is.na(rowSums(ff)))
      
      # get coordinates and names
      chcoord<-coordinates(yearschillmin[[1]])[ch[,1],]
      chcoord2<-coordinates(yearschillmin[[1]])[ch2[,1],]
      ch<-cbind(chcoord,ch[,2:ncol(ch)])
      ch2<-cbind(chcoord2,ch2[,2:ncol(ch2)])
      
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
      
      datesch<-as.Date(colnames(ch),format="X%Y.%m.%d")[3:ncol(ch)]
      
      ffcoord<-coordinates(yearschillmax[[1]])[ff[,1],]
      ff<-cbind(ffcoord,ff[,2:ncol(ff)])
      
      
      ## calculate chilling (Utah) and GDD across the period
      gddseachcelleachday<-apply(ff[,3:ncol(ff)],2,function(x){
        Tb<-10
        gdd<-ifelse((x-Tb)<0,0,x-Tb)
        return(gdd)})
      gddssum<-rowSums(gddseachcelleachday)
      #hist(gddssum)
      
      
      #library(abind)
      minmaxtemp<-abind(ch,ch2, along = 3)
      
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
      
      
      
      yearlyresults[which(period==j),1]<-mean(gddssum,na.rm=T)
      yearlyresults[which(period==j),2]<-sd(gddssum,na.rm=T)
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
Climate.in.range<-extractchillforce(spslist,fullnames,trees,tmin,tmax,period)

## saving outputs
#yearlyresultsBettil2014<-yearlyresults
write.csv(Climate.in.range[,,1],file = "analyses/ranges/climate.in.range1980-2017BetPub.csv")
write.csv(Climate.in.range[,,2],file = "analyses/ranges/climate.in.range1980-2017FagSyl.csv")
write.csv(Climate.in.range[,,1],file = "analyses/ranges/climate.in.range1980-2017PicAb1.csv")
write.csv(Climate.in.range[,,2],file = "analyses/ranges/climate.in.range1980-2017BetPen.csv")
write.csv(Climate.in.range[,,3],file = "analyses/ranges/climate.in.range1980-2017CorAve.csv")
write.csv(Climate.in.range[,,4],file = "analyses/ranges/climate.in.range1980-2017QurRo1.csv")




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
## for Utah units I used default parameters in package chillR
## Still lacking code to extract centroids in environmental space
## The output table yields yearly mean and sd values of chilling and 
## forcing across the range computed out of daily values within
## the Oct-Feb period (chilling) and March-May period (forcing)
## Do we want further results?














