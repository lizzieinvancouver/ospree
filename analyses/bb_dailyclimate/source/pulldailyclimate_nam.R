# Read in netCDF files to pull climate data for North America
# This requires you to work off of external hard drive
# This code pulls data for the calendar yar before and after the field sample date
# because PNP requires full year of data to run
# Ailene got a start on this: June 13, 2017
#If  just looking at nam climate, do this:
#tempval <- list() 

nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]
#loop through each lat/long for which we want to calculate chilling and pull the climate data for that lat/long
#the climate data that we are pulling is daily min and max temperature

for(i in 1:nrow(nam)){ # i = 1
  
  # find this location
  lo <- nam[i,"chill.long"] + 360
  la <- nam[i,"chill.lat"]
  
  # make sure longitudes are negative, need to be for North America
  if(lo > 0) { lo = lo*-1 }
  
  ##REMOVE##yr <- as.numeric(nam[i,"year"])#i think we need to use this year for nam because it is referenced in the code below.
  #yr <-as.numeric(substr(nam[i,"fieldsample.date2"],1,4))#year for climate data
  
  yr<-as.numeric(substr(nam[i,"fieldsample.date2"],1,4))
  # start and end days of the climate data we need to calculate chilling, for the focal lat/long. 
  #This is in days since baseline date (sept 1) Set to GMT to avoid daylight savings insanity
  # using d$fieldsample.date2 (this is the same as fieldsampledate, but formatted as  "%Y-%m-%d")
  
  #for pmp, we always need climate data to go until 12-31
  fsday <- strptime(nam[i,"fieldsample.date2"],"%Y-%m-%d", tz = "GMT")
  endday <- strptime(paste(yr, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")
  
  if(nam[i,"fieldsample.date2"]!="" & as.numeric(substr(nam[i,"fieldsample.date2"],6,7))>=9){
    stday <- strptime(paste(yr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
    firstyr <- yr;#
    endyr<-yr+1;#month of last date of climate year
    endday <- strptime(paste(yr+1, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")
    pmpclim<-c(firstyr, endyr)
    }#If field sample date is after september 1, then we use the chilling from the current year, since sept 1
  
  if(nam[i,"fieldsample.date2"]!="" & as.numeric(substr(nam[i,"fieldsample.date2"],6,7))<9){
    stday <- strptime(paste(yr-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")#always start getting date jan 1
    firstyr <- yr-1;# use previous year's fall months of chilling (Sept-Dec)
    endyr<-yr;#month of last date of climate year
    endday <- strptime(paste(yr, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")
    pmpclim<-c(firstyr, endyr)
  }#If field sample date is before september 1, then we use the chilling from the previous year.
  
  if(la==38.988){# 
    #For this one study (swartz81) we need two extra years of climate data (e.g. because of long chilling treatments) to correspond to the budburst dates and calculate accurate forcing.
    #we will use the latitude of this study to select it out and extend the end yr for climate data to pull
    #unique(nam$datasetID[nam$chill.lat== 38.988])
      stday <- strptime(paste(yr-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")#always start getting date jan 1
      firstyr <- yr-1;# use previous year's fall months of chilling (Sept-Dec)
      secondyr<-yr;# 
      thirdyr<-yr+1;# 
      endyr<-yr+2;#month of last date of climate year
      endday <- strptime(paste(yr+2, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")
      pmpclim<-c(firstyr,secondyr,thirdyr,endyr)
  }
  # now loop over these year-month combo files and get temperature values for this date range.
  mins <- maxs <- vector()
  
  for(j in c(pmpclim)){ # j = 1956
    filemax <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",j), full.names = TRUE)
    filemin <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",j), full.names = TRUE)
    if(length(nchar(filemax))==0){next}
    if(length(nchar(filemin))==0){next}
    jx <- nc_open(filemax)
    jn <- nc_open(filemin)
    
    diff.long.cell <- abs(jx$dim$lon$vals-as.numeric(lo))
    diff.lat.cell <- abs(jx$dim$lat$vals-as.numeric(la))
    long.cell <- which(diff.long.cell==min(diff.long.cell))[1] 
    lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
    mintest<-ncvar_get(jn,'tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))#check that the lat/long combinations has temperature data. 
    #print(mintest);print(j)
    #if no temperature data for the focal lat/long, choose the next closest one. 
    #the below code goes up to 0.1 degrees (~10km) away from the closest lat/long)
    if(is.na(unique(mintest))){#if there are no temp data for the selected lat/long, choose a different one
      diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
      diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
      long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
      lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
      mintest<-ncvar_get(jn,'tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))
      
      if(is.na(unique(mintest))){
        diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
        diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
        long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
        lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        mintest<-ncvar_get(jn,'tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))
        if(is.na(unique(mintest))){
          diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
          diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
          long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
          lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
          }}}
    mins <- c(mins, (ncvar_get(jn,'tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))-273.15)
    maxs <- c(maxs, (ncvar_get(jx,'tmax',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))-273.15)
    nc_close(jx)
    nc_close(jn)
    }
  #print(i);print(stday);print(endday)
  tempval[[as.character(nam[i,"ID_fieldsample.date2"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),
                                                                       Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])#
}

# If you want to (as Lizzie does) you can write out tempval, which is all the climate pulled in a list form
save(tempval, file="output/dailyclim/fieldclimate_daily.RData")
#(If you want to avoid connecting to the external hard drive, then start here)
#load this .RData workspace)
#load("output/dailyclim/fieldclimate_daily.RData")
#dailytemp <- do.call("rbind", tempval)
#dailytemp<-as.data.frame(cbind(row.names(dailytemp),dailytemp))
#colnames(dailytemp)[1]<-"ID_fieldsample.date2"
#dailytemp2<-separate(data = dailytemp, col = ID_fieldsample.date2, into = c("datasetID", "lat","long","fieldsample.date2"), sep = "\\_")
#row.names(dailytemp2)<-NULL
#dailytemp3<-subset(dailytemp2,select=c(datasetID,lat,long,fieldsample.date2,Date,Tmin,Tmax))
#note: no climate data for boyer 1983-1984

stop("Not an error, just stopping here to say we're now done pulling daily climate data for North America!")

