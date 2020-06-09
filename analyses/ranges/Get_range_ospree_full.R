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
  #chillforcespsyears <- array(NA,dim=c(nyears,6,nsps))
  #row.names(chillforcespsyears)<-period
  #colnames(chillforcespsyears)<-c("Mean.GDD",
   #                               "SDev.GDD.sites","Mean.Chill.Utah","SDev.Chill.Utah",
    #                              "Mean.Chill.Portions","SDev.Chill.Portions")
  minmaxtemps.eachsps <- list()
  
  #dimnames(chillforcespsyears)<-spslist
  
  ## subset climate years
  yearsinclim<-as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
  
  ras.numpixels<-chillsub1[[1]]
  values(ras.numpixels)<-1:ncell(ras.numpixels)
  
  ## commence loop  
  for (i in 1:nsps){#i=2
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
    ## 
    spsshapeproj<-spTransform(spsshape,proj4string(chillsub1[[1]]))
    #lines(spsshapeproj)
    #plot(chillsub1[[1]])
    pixels.sps.i<-unique(sort(unlist(extract(ras.numpixels,spsshapeproj))))
    npix<-length(pixels.sps.i)
    ## loop across years to extract each years averages and stddevs
    # save first an array to store results
    #period=2005:2007
    yearlyresults<-array(NA,dim=c(npix,9,length(period)))
    colnames(yearlyresults)<-c("x","y",
                               "GDD","GDD.lastfrost",
                               "DayLastFrost","MeanTmins","SDev.Tmins",
                               "Mean.Chill.Utah","Mean.Chill.Portions")
    
    
    for(j in period){#j=1980
      print(paste(i,j))
      
      
      yearsinperiod<-which(yearsinclim%in%c(j,j+1))
      climsub.min<-subset(tmin,yearsinperiod)
      climsub.max<-subset(tmax,yearsinperiod)
      
      ## subset climate by months & days
      chillsub1<-subset(climsub.min,274:424)
      chillsub2<-subset(climsub.max,274:424)
      forcesub1<-subset(climsub.min,1:151)
      forcesub2<-subset(climsub.max,1:151)
      
      nas<-which(is.na(values(forcesub1)[pixels.sps.i]))
      
      # extract values and format to compute means and sdevs
      #tempschillsmin<-extract(yearschillmin,spsshapeproj,cellnumbers=T)
      ch<-chillsub1[pixels.sps.i][-nas,]
      
      #tempschillsmax<-extract(yearschillmax,spsshapeproj,cellnumbers=T)
      ch2<-chillsub2[pixels.sps.i][-nas,]
      
      #tempsforces<-extract(yearsforce,spsshapeproj,cellnumbers=T)
      ff<-forcesub1[pixels.sps.i][-nas,]
      ff2<-forcesub2[pixels.sps.i][-nas,]
      
    
      
      # add coordinates and names
      if(length(nas)>0){
      chcoord<-coordinates(yearschillmin[[1]])[pixels.sps.i[-nas],]
      yearlyresults[-nas,1:2,]<-chcoord
      } else {
      chcoord<-coordinates(yearschillmin[[1]])[pixels.sps.i,]
      yearlyresults[,1:2,]<-chcoord
      }
      ch<-cbind(chcoord,ch[,1:ncol(ch)])
      ch2<-cbind(chcoord,ch2[,1:ncol(ch2)])
      ff<-cbind(chcoord,ff[,1:ncol(ff)])
      ff2<-cbind(chcoord,ff2[,1:ncol(ff2)])
      
      
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
        return(ifelse(length(a)>0,max(a),NA)) 
      }) 
      
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


## apply function (beware this function takes ~7mins per year, consider 
## parallelizing)
Climate.in.range.list<-list()
for(i in 1:length(spslist)){
  Climate.in.range.list[[i]]<-extractchillforce(spslist[i],tmin,tmax,period)
}
Climate.in.range.list[[1]][[1]][,,20]

## saving outputs
save(Climate.in.range, file = paste("output/climate.in.range",
                                    period[1],max(period),"RData",sep="."))

## remove aux unnecessary files
unlink("chorological_maps_dataset/*", recursive = T)




