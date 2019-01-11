extractchillpost<-function(tavg,period){
  
  ## define array to store results
  nyears<-length(period)
  chillingyears<-array(NA,dim=c(nyears, 3, nsites))
  row.names(chillingyears)<-period
  colnames(chillingyears)<-c("Mean.Chill","SDev.Chill", "Site Num.")
  #dimnames(chillforcespsyears)<-spslist
  
  ## subset climate years
  yearsinclim<-as.numeric(format(as.Date(names(tavg),format="X%Y.%m.%d"),"%Y"))
  yearsinperiod<-which(yearsinclim%in%period)
  climsub<-subset(tavg,yearsinperiod)
  
  ## subset climate days
  monthsinclim<-as.numeric(format(as.Date(names(climsub),format="X%Y.%m.%d"),"%m"))
  chillmonths<-c(9:12,1:3)
  monthsinchill<-which(monthsinclim%in%chillmonths)
  chillsub<-subset(climsub,monthsinchill)
  
  ## commence loop  
  for (i in 1:nsites){#i=2
    print(i)
    sitesi<-sites$siteslist[i]
    
    
    #values <- raster::extract(r,points)
    
    
    ## load shape
    if(sitesi==sites$siteslist[i])
      Coords<-data.frame(sites$x, sites$y)
    points <- SpatialPoints(Coords, proj4string = tavg@crs)
    #spsshapeproj<-spTransform(points,proj4string(chillsub[[1]]))
    
    
    ## loop across years to extract each years averages and stddevs
    # save first an array to store results
    yearlyresults<-array(NA,dim=c(length(period),3))
    
    for(j in period){#j=1980
      print(paste(i,j))
      
      # select year's layer
      chillyears<-which(as.numeric(format(as.Date(
        names(chillsub),format="X%Y.%m.%d"),"%Y"))==j)
      
      yearschill<-subset(chillsub,chillyears)
      
      # extract values and format to compute means and sdevs
      tempschills<-raster::extract(yearschill,points)
      
      #turn into data frame and remove NAs
      ch<-as.data.frame(tempschills)
      ch<-subset(ch,!is.na(rowSums(ch)))
      
      
      ## calculate chilling (Utah)
      chillunitseachcelleachday<-apply(ch,2,function(x){
        tlow=-5 ## not sure about which parameteres we are using
        thigh=5
        minns<-ifelse((thigh-x)>(thigh-tlow),thigh-tlow,thigh-x)
        utah<-ifelse(minns>0,minns,0)
        return(utah)})
      utahssum<-rowSums(chillunitseachcelleachday)
      #hist(utahssum)
      
      yearlyresults[which(period==j),1]<-mean(utahssum,na.rm=T)
      yearlyresults[which(period==j),2]<-sd(utahssum,na.rm=T)
      yearlyresults[which(period==j),3]<-sites$siteslist[i]
      
    }
    
    chillingyears[,,i]<-yearlyresults
    
  } 
  
  return(chillingyears)
  
}
