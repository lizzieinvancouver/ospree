# Read in netCDF files to pull climate data for North America
# This requires you to work off of external hard drive
# This code pulls data for the calendar yar before and after the field sample date
# because PNP requires full year of data to run
# Ailene got a start on this: June 13, 2017
#If  just looking at nam climate, do this:
#tempval <- list() 

nafiles <- dir(climatedrive)[grep("livneh", dir(climatedrive))]
#loop through each lat/long for which we want to calculate chilling and pull the climate data for that lat/long
#the climate data that we are pulling is daily min and max temperature

for(i in 1:nrow(nam)){ # i = 1
  
  # find this location
  lo <- nam[i,"chill.long"]
  la <- nam[i,"chill.lat"]
  
  # make sure longitudes are negative, need to be for North America
  if(lo > 0) { lo = lo*-1 }
  
  yr <- as.numeric(nam[i,"year"])#i think we need to use this year for nam because it is referenced in the code below.
  #yr <-as.numeric(substr(nam[i,"fieldsample.date2"],1,4))#year for climate data
  
  # start and end days of the climate data we need to calculate chilling, for the focal lat/long. This is in days since baseline date (sept 1) Set to GMT to avoid daylight savings insanity
  # using d$fieldsample.date2 (this is the same as fieldsampledate, but formatted as  "%Y-%m-%d")
  #if(nam[i,"fieldsample.date2"]!=""){endday <- strptime(nam[i,"fieldsample.date2"],"%Y-%m-%d", tz = "GMT")}
  #if(nam[i,"fieldsample.date2"]==""){endday <- strptime(paste(yr, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")} #if no sampling date given, use december 31 of same year
  #for pmp, we always need climate data to go until 12-31
  fsday <- strptime(nam[i,"fieldsample.date2"],"%Y-%m-%d", tz = "GMT")
  endday <- strptime(paste(yr, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")
  
  if(substr(fsday,1,4)==yr & as.numeric(substr(fsday,6,7))<=9){#when sampling occurred in same year as study and when collection occurred before that year's sept chilling,
    stday <- strptime(paste(yr-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
    firstyr <- paste(yr-1, formatC(1:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endyr<-paste(yr, formatC(1:12, width=2, flag="0"), sep="");#month of last date of climate year
     pmpclim<-c(firstyr, endyr)
  } else if(substr(fsday,1,4)==yr-1 & as.numeric(substr(fsday,6,7))<=12  & as.numeric(substr(fsday,6,7))>=9){#when sampling occurred in previous year as study only
    stday <- strptime(paste(yr-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
  # prevmo <- paste(yr-1, formatC(1:12, width=2, flag="0"), sep="");# use previous year of chilling (always have to start in january for pmp-whenever collection occured)}
    firstyr <- paste(yr-1, formatC(1:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endyr<-paste(yr, formatC(1:12, width=2, flag="0"), sep="");#month of last date of climate year
    pmpclim<-c(firstyr, endyr)
    }else if(substr(fsday,1,4)==yr & as.numeric(substr(fsday,6,7))>=9){#when sampling occurred in same year as study and after chilling started that year
    stday <- strptime(paste(yr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")#always start getting climate data jan 1 for pmp
    endday <- strptime(paste(yr+1, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")
    firstyr <- paste(yr, formatC(1:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endyr<-paste(yr+1, formatC(1:12, width=2, flag="0"), sep="");#month of last date of climate year
    pmpclim<-c(firstyr, endyr)
  } else if(substr(fsday,1,4)==yr-1 & as.numeric(substr(fsday,6,7))<=12  & as.numeric(substr(endday,6,7))>=9){#when sampling occurred in previous year as study between sept and dec
    stday <- strptime(paste(yr-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
    #prevmo <- paste(yr-1, formatC(1:12, width=2, flag="0"), sep="");# always start jan 1 for pmp
    firstyr <- paste(yr-1, formatC(1:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endyr<-paste(yr, formatC(1:12, width=2, flag="0"), sep="");#month of last date of climate year
    pmpclim<-c(firstyr, endyr)
  } else if(substr(fsday,1,4)==yr-1 & as.numeric(substr(fsday,6,7))<=12  & as.numeric(substr(endday,6,7))<9){#when sampling occurred in previous year as study, NOT during the fall
    stday <- strptime(paste(as.numeric(substr(fsday,1,4))-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")#always start getting date jan 1
    #prevmo <- paste(as.numeric(substr(endday,1,4))-1, formatC(1:12, width=2, flag="0"), sep="");# use previous year's climate data 
    #endmo<-"12";#month of last year (should always be 12 for pmp)
    #thismo <- paste(as.numeric(substr(endday,1,4)), formatC(1:endmo, width=2, flag="0"), sep="")#months from current year of chilling, through sampling date (Jan-Dec for pmp)
    firstyr <- paste(yr-1, formatC(1:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endyr<-paste(yr, formatC(1:12, width=2, flag="0"), sep="");#month of last date of climate year
    pmpclim<-c(firstyr, endyr)
  } 
  if(la==38.988){# 
    #For this one study (swartz81) we need two extra years of climate data (e.g. because of long chilling treatments) to correspond to the budburst dates and calculate accurate forcing.
    #we will use the latitude of this study to select it out and extend the end yr for climate data to pull
    #unique(nam$datasetID[nam$chill.lat== 38.988])
      stday <- strptime(paste(as.numeric(substr(fsday,1,4))-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")#always start getting date jan 1
      firstyr <- paste(yr-1, formatC(1:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
      secondyr<-paste(yr, formatC(1:12, width=2, flag="0"), sep="");# 
      thirdyr<-paste(yr+1, formatC(1:12, width=2, flag="0"), sep="");# 
      endyr<-paste(yr+2, formatC(1:12, width=2, flag="0"), sep="");#month of last date of climate year
      endday <- strptime(paste(yr+2, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")
      pmpclim<-c(firstyr,secondyr,thirdyr,endyr)
  }
  # now loop over these year-month combo files and get temperature values for this date range.
  mins <- maxs <- vector()
  
  for(j in c(pmpclim)){ # j = "200009"
    file <- file.path(climatedrive,nafiles[grep(j, nafiles)])
    if(length(nchar(file))==0){next}
    jx <- nc_open(file)
    
    diff.long.cell <- abs(jx$dim$lon$vals-as.numeric(lo))
    diff.lat.cell <- abs(jx$dim$lat$vals-as.numeric(la))
    long.cell <- which(diff.long.cell==min(diff.long.cell))[1] 
    lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
    mintest<-ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))#check that the lat/long combinations has temperature data. 
    #if no temperature data for the focal lat/long, choose the next closest one. 
    #the below code cose up to 0.1 degrees (~10km) away from the closest lat/long)
    if(is.na(unique(mintest))){#if there are no temp data for the selected lat/long, chosee a different one
      diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
      diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
      long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
      lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
      mintest<-ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))
      if(is.na(unique(mintest))){
        diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
        diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
        long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
        lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        mintest<-ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))
        if(is.na(unique(mintest))){
          diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
          diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
          long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
          lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        }}}
    mins <- c(mins, ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))
    maxs <- c(maxs, ncvar_get(jx,'Tmax',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))
    nc_close(jx)
    }
  #print(i);print(stday);print(endday)
  tempval[[as.character(nam[i,"ID_fieldsample.date2"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),
                                                                       Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])#
}

# If you want to (as Lizzie does) you can write out tempval, which is all the climate pulled in a list form
save(tempval, file="output/fieldclimate_daily.RData")
#(If you want to avoid connecting to the external hard drive, then start here)
#load this .RData workspace)
#load("output/fieldclimate_daily.RData")
#dailytemp <- do.call("rbind", tempval)
#dailytemp<-as.data.frame(cbind(row.names(dailytemp),dailytemp))
#colnames(dailytemp)[1]<-"ID_fieldsample.date2"
#dailytemp2<-separate(data = dailytemp, col = ID_fieldsample.date2, into = c("datasetID", "lat","long","fieldsample.date2"), sep = "\\_")
#row.names(dailytemp2)<-NULL
#dailytemp3<-subset(dailytemp2,select=c(datasetID,lat,long,fieldsample.date2,Date,Tmin,Tmax))

stop("Not an error, just stopping here to say we're now done pulling daily climate data for North America!")

