# Read in netCDF files to pull climate data for North America
# This requires you to work off of external hard drive

nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]
#loop through each lat/long for which we want to calculate chilling and pull the climate data for that lat/long
#the climate data that we are pulling is daily min and max temperature

tempval <- list() 
for(i in 1:nrow(nam)){ # i = 1
  # find this location
  lo <- nam[i,"chill.long"]
  la <- nam[i,"chill.lat"]
  
  # make sure longitudes are negative, need to be for North America this step is now done in "cleaning/clean_latlong" so it is no longer necessary
  #if(lo > 0) { lo = lo*-1 }
  yr<-as.numeric(substr(nam[i,"fieldsample.date2"],1,4))
  # start and end days of the climate data we need to calculate chilling, for the focal lat/long. 
  #This is in days since baseline date (sept 1) Set to GMT to avoid daylight savings insanity
  # using d$fieldsample.date2 (this is the same as fieldsampledate, but formatted as  "%Y-%m-%d")
  
  #do everything in reference to field sample year becuase the year column is too variable
  if(nam[i,"fieldsample.date2"]!=""){endday <- strptime(nam[i,"fieldsample.date2"],"%Y-%m-%d", tz = "GMT")}
  if(nam[i,"fieldsample.date2"]==""){endday <- strptime(paste(yr, "04-30", sep="-"),"%Y-%m-%d", tz = "GMT")}#I think we have field sample dates for everything, but just in case...
  
  if(nam[i,"fieldsample.date2"]!="" & as.numeric(substr(nam[i,"fieldsample.date2"],6,7))>=9){
    stday <- strptime(paste(yr, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    chillmo<-paste(yr, formatC(9:substr(endday,6,7), width=2, flag="0"), sep="")
    }#If field sample date is after september 1, then we use the chilling from the current year, since sept 1
  
  if(nam[i,"fieldsample.date2"]!="" & as.numeric(substr(nam[i,"fieldsample.date2"],6,7))<9){
    stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")#
    prevmo <- paste(yr-1, formatC(9:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endmo<-substr(endday,6,7);#month of sampling date
    thismo <- paste(yr, formatC(1:endmo, width=2, flag="0"), sep="")#months from current year of chilling, through sampling date (Jan-whenever sampled)
    chillmo<-c(prevmo, thismo)
  }#If field sample date is before september 1, then we use the chilling from the previous year.
  
# now loop over these year-month combo files and get temperature values for this date range.

  mins <- maxs <- vector()
  
  for(j in yr) { # j = 2015
    
    tmax <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",j), full.names = TRUE)
    tmaxprev <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",j-1), full.names = TRUE)
    tmin <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",j), full.names = TRUE)
    tminprev <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",j-1), full.names = TRUE)
    jx <- nc_open(tmax)
    jxprev <- nc_open(tmaxprev)
    jn <- nc_open(tmin)
    jnprev <- nc_open(tminprev)
    
    jx$dim$time$vals<-seq(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-12-31")), by="day")
    jxprev$dim$time$vals<-seq(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-12-31")), by="day")
    jx <- cbind( ### doesn't work
      ncvar_get(jx, jx$var$tmax),
      ncvar_get(jxprev, jxprev$var$tmax)
    )
    
    
    jn$dim$time$vals<-seq(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-12-31")), by="day")
    jnprev$dim$time$vals<-seq(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-12-31")), by="day")
    
    diff.long.cell <- abs(jx$dim$lon$vals-as.numeric(lo))#differences between all longitudes & latitudes in the focal month's dataset and longitude[i]
    diff.lat.cell <- abs(jx$dim$lat$vals-as.numeric(la))
    long.cell <- which(diff.long.cell==min(diff.long.cell))[1] #select the closest longitude & latitude with climate data to longitude[i]
    lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
    long.cell <- which.min(abs(jx$dim$lon$vals-as.numeric(lo)))
    lat.cell <- which.min(abs(jx$dim$lat$vals-as.numeric(la)))
    maxtest<-(ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1)))-273.15#checl that the lat/long combinations has temperature data. 
    #if no temperature data for the focal lat/long, choose the next closest one. 
    #the below code goes up to 0.1 degrees (~10km) away from the closest lat/long)
    if(is.na(unique(maxtest))){#if there are no temp data for the selected lat/long, choose a different one
     diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
      diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
      long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
      lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
      mintest<-(ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1)))-273.15
      if(is.na(unique(maxtest))){
        diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
        diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
        long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
        lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        maxtest<-(ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1))) -273.15
        if(is.na(unique(maxtest))){
          diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
          diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
          long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
          lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        }}}
    
    mins <- c(mins, ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1))-273.15)#minimum temperatures for selected lat/long
    maxs <- c(maxs, ncvar_get(jn,start=c(long.cell,lat.cell,1),count=c(1,1,-1))-273.15)#maximum temperatures for selected lat/long
    nc_close(jx)
    nc_close(jn)
    }

  tempval[[as.character(nam[i,"ID_fieldsample.date2"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),  
                                                        Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])
}
# If you want to (as Lizzie does) you can write out tempval, which is all the climate pulled in a list form
save(tempval, file="output/fieldclimate.RData")
#tempval_all <- do.call("rbind", tempval)
#tempval_all<-cbind(row.names(tempval_all),tempval_all)
#row.names(tempval_all)<-NULL
#colnames(tempval_all)[1]<-"ID_fieldsample.date2"

#write.csv(tempval_all, "output/tempval_all.csv", row.names=FALSE, eol="\r\n")

stop("Not an error, 
     just stopping here to say we're now done pulling daily climate data 
     for North America! Next step is to interpolate these to hourly data 
     and estimate chilling from the hourly data. You can ignore the warning that comes at the end of this code- it's ok!")

