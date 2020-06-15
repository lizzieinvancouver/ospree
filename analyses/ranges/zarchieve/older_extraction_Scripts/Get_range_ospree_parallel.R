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
library('foreach')
library('doParallel')



## load climate data rasters (these data are not currently in the ospree folder 
## as they are heavy - 2.2Gb) - E-OBS at http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.50regular/catalog.html
tmin<-brick("~/Data_Harvard/EU trees/tn_0.50deg_reg_v17.0.nc", varname="tn", sep="")
tmax<-brick("~/Data_Harvard/EU trees/tx_0.50deg_reg_v17.0.nc", varname="tx", sep="")
tmin<-brick("~/Data_Harvard/EU trees/tn_ens_mean_0.25deg_reg_v20.0e.nc", varname="tn", sep="")
tmax<-brick("~/Data_Harvard/EU trees/tx_ens_mean_0.25deg_reg_v20.0e.nc", varname="tx", sep="")



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
ospreespslist <- species.list[which(species.list %in% species.list.maps)]


# define period
period<-1980:2017
#period<-2009:2010


## set function
extractchillforce<-function(spslist,tmin,tmax,period){
  
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
    #fullnamei<-fullnames[i]
    
    ## load shape
    
    path.source.i <- "../../data/distributiondata/chorological_maps_dataset_20170220.zip"
    
    # get the file address for target file
    zipped_name.i <- grep(paste(spsi,'_plg',sep=""), 
                          unzip(path.source.i,
                                list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
    
    # extract target file
    unzip(path.source.i, files=zipped_name.i)
    
    # load shapefile
    spsshape <- shapefile(zipped_name.i[3])
    
    
    ## need to re-project shape from lamber equal area to geographic
    ## 
    spsshapeproj<-spTransform(spsshape,proj4string(chillsub1[[1]]))
    
    ## loop across years to extract each years averages and stddevs
    # save first an array to store results
    yearlyresults<-array(NA,dim=c(length(period),12))
    
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
      
      yearlyresults[which(period==j),7]<-mean(gddssum,na.rm=T)
      yearlyresults[which(period==j),8]<-sd(gddssum,na.rm=T)
      yearlyresults[which(period==j),9]<-mean(chillunitseachcelleachday$Utah_Model,na.rm=T)
      yearlyresults[which(period==j),10]<-sd(chillunitseachcelleachday$Utah_Model,na.rm=T)
      yearlyresults[which(period==j),11]<-mean(chillunitseachcelleachday$Chill_portions,na.rm=T)
      yearlyresults[which(period==j),12]<-sd(chillunitseachcelleachday$Chill_portions,na.rm=T)
      
    }
    
    chillforcespsyears[,,i]<-yearlyresults
    
  }  
  
  return(chillforcespsyears)
  
}


## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
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
  
