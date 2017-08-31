# Read in netCDF files to pull climate data for North America
# This requires you to work off of external hard drive

nafiles <- dir(climatedrive)[grep("livneh", dir(climatedrive))]
#loop through each lat/long for which we want to calculate chilling and pull the climate data for that lat/long
#the climate data that we are pulling is daily min and max temperature

for(i in 1:nrow(nam)){ # i = 1
  
  # find this location
  lo <- nam[i,"chill.long"]
  la <- nam[i,"chill.lat"]
  
  # make sure longitudes are negative, need to be for North America this step is now done in "cleaning/clean_latlong" so it is no longer necessary
  #if(lo > 0) { lo = lo*-1 }
  
  yr <- as.numeric(nam[i,"year"])
  # start and end days of the climate data we need to calculate chilling, for the focal lat/long. This is in days since baseline date (sept 1) Set to GMT to avoid daylight savings insanity
  # using d$fieldsample.date2 (this is the same as fieldsampledate, but formatted as  "%Y-%m-%d")
  if(nam[i,"fieldsample.date2"]!=""){endday <- strptime(nam[i,"fieldsample.date2"],"%Y-%m-%d", tz = "GMT")}
  if(nam[i,"fieldsample.date2"]==""){endday <- strptime(paste(yr, "04-30", sep="-"),"%Y-%m-%d", tz = "GMT")} #if no sampling date given, use april 30
  
  if(substr(endday,1,4)==yr & as.numeric(substr(endday,6,7))<=9){#when sampling occurred in same year as study and when collection occurred before that year's sept chilling,
    stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(yr-1, formatC(9:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endmo<-substr(endday,6,7);#month of sampling date
    thismo <- paste(yr, formatC(1:endmo, width=2, flag="0"), sep="")#months from current year of chilling, through sampling date (Jan-whenever sampled)
    chillmo<-c(prevmo, thismo)
  }
  
  if(substr(endday,1,4)==yr-1 & as.numeric(substr(endday,6,7))<=12  & as.numeric(substr(endday,6,7))>=9){#when sampling occurred in previous year as study only
    stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(yr-1, formatC(9:substr(endday,6,7), width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-whenever collection occured)}
    chillmo<-prevmo
  }
  
  if(substr(endday,1,4)==yr & as.numeric(substr(endday,6,7))>=9){#when sampling occurred in same year as study and after chilling started that year
    stday <- strptime(paste(yr, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(yr, formatC(9:substr(endday,6,7), width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-whenever collection occured)}
    chillmo<-prevmo
  }
  
  if(substr(endday,1,4)==yr-1 & as.numeric(substr(endday,6,7))<=12  & as.numeric(substr(endday,6,7))>=9){#when sampling occurred in previous year as study between sept and dec
    stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(yr-1, formatC(9:substr(endday,6,7), width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-whenever collection occured)}
    chillmo<-prevmo
  }
  
  if(substr(endday,1,4)==yr-1 & as.numeric(substr(endday,6,7))<=12  & as.numeric(substr(endday,6,7))<9){#when sampling occurred in previous year as study, NOT during the fall
    stday <- strptime(paste(as.numeric(substr(endday,1,4))-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(as.numeric(substr(endday,1,4))-1, formatC(9:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endmo<-substr(endday,6,7);#month of sampling date
    thismo <- paste(as.numeric(substr(endday,1,4)), formatC(1:endmo, width=2, flag="0"), sep="")#months from current year of chilling, through sampling date (Jan-whenever sampled)
    chillmo<-c(prevmo, thismo)
  }
  # now loop over these year-month combo files and get temperature values for this date range.

  mins <- maxs <- vector()
  
  for(j in c(chillmo)){ # j = "200009"
    file <- file.path(climatedrive,nafiles[grep(j, nafiles)])
    jx <- nc_open(file)
    
    diff.long.cell <- abs(jx$dim$lon$vals-as.numeric(lo))
    diff.lat.cell <- abs(jx$dim$lat$vals-as.numeric(la))
    long.cell <- which(diff.long.cell==min(diff.long.cell))[1] 
    lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
    
    mins <- c(mins, ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))
    maxs <- c(maxs, ncvar_get(jx,'Tmax',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))
    nc_close(jx)
    }
  
  tempval[[as.character(nam[i,"ID_fieldsample.date2"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),
                                                                       Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])#
}

# If you want to (as Lizzie does) you can write out tempval, which is all the climate pulled in a list form
save(tempval, file="output/fieldclimate.RData")
#tempval_all <- do.call("rbind", tempval)
#tempval_all<-cbind(row.names(tempval_all),tempval_all)
#row.names(tempval_all)<-NULL
#colnames(tempval_all)[1]<-"ID_fieldsample.date2"

#write.csv(tempval_all, "output/tempval_all.csv", row.names=FALSE, eol="\r\n")

stop("Not an error, just stopping here to say we're now done pulling daily climate data for North America! Next step is to interpolate these to hourly data and estimate chilling from the hourly data.")

