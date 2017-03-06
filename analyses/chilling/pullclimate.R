
# Read in netCDF files to pull climate data. 
# This requires you to work off of external hard drive
# Europe first

eur.tempmn <- nc_open(file.path(climatedrive, "tn_0.25deg_reg_v12.0.nc"))
eur.tempmx <- nc_open(file.path(climatedrive, "tx_0.25deg_reg_v12.0.nc"))

tempval <- list() 
for(i in 1:nrow(eur)){ # i = 1
  
  # find this location
  lo <- eur[i,"chill.long"]
  la <- eur[i,"chill.lat"]
  
  ndiff.long.cell <- abs(eur.tempmn$dim$longitude$vals-as.numeric(lo))
  ndiff.lat.cell <- abs(eur.tempmn$dim$latitude$vals-as.numeric(la))
  nlong.cell <- which(ndiff.long.cell==min(ndiff.long.cell))[1] 
  nlat.cell <- which(ndiff.lat.cell==min(ndiff.lat.cell))[1]
  
  xdiff.long.cell <- abs(eur.tempmx$dim$longitude$vals-as.numeric(lo))
  xdiff.lat.cell <- abs(eur.tempmx$dim$latitude$vals-as.numeric(la))
  xlong.cell <- which(xdiff.long.cell==min(xdiff.long.cell))[1]
  xlat.cell <- which(xdiff.lat.cell==min(xdiff.lat.cell))[1]
  
  yr <- as.numeric(eur[i,"year"])#need to add month
  
  # start and end days, in days since baseline date (#sept 1) Set to GMT to avoid daylight savings insanity
  stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
  if(eur[i,"fieldsample.date"]!="" & as.numeric(substr(eur[i,"fieldsample.date"],6,7))>=9){stday <- strptime(paste(yr, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  if(eur[i,"fieldsample.date"]!="" & as.numeric(substr(eur[i,"fieldsample.date"],6,7))<9){stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  if(eur[i,"fieldsample.date"]==""){stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  
  # using d$fieldsample.date
  if(eur[i,"fieldsample.date"]!=""){endday <- strptime(eur[i,"fieldsample.date"],"%Y-%m-%d", tz = "GMT")}
  if(eur[i,"fieldsample.date"]==""){endday <- strptime(paste(yr, "04-30", sep="-"),"%Y-%m-%d", tz = "GMT")}
  
  st <- as.numeric(as.character(stday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  en <- as.numeric(as.character(endday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  if(en<st){en=st}
  if(endday<stday){endday=stday}
  
  # get temperature values for this range.
  # check the dim of the net cdf file, str(netcdf), and see what the order of the different dimensions are. In this case, it goes long, lat, time. So when we are moving through the file, we give it the long and lat and date of start, then move through the files by going 'up' the cube of data to the end date
  mins <- ncvar_get(eur.tempmn,'tn', 
                    start=c(nlong.cell,nlat.cell,st), 
                    count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
  ) 
  
  maxs <- ncvar_get(eur.tempmx,'tx',
                    start=c(xlong.cell,xlat.cell,st),
                    count=c(1,1,en-st+1)
  )
  
  tempval[[as.character(eur[i,"ID_fieldsample.date"])]] <- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                                                                      Tmin = mins, Tmax = maxs)#
}


nc_close(eur.tempmx)
nc_close(eur.tempmn)

stop("Not an error, just stopping here to say we're now done pulling daily climate data for Europe! Now working on North America...")

######################################################
# North America
######################################################

nafiles <- dir(climatedrive)[grep("livneh", dir(climatedrive))]

for(i in 1:nrow(nam)){ # i = 1
  
  # find this location
  lo <- nam[i,"chill.long"]
  la <- nam[i,"chill.lat"]
  
  # make sure longitudes are negative, need to be for North America
  if(lo > 0) { lo = lo*-1 }
  
  yr <- as.numeric(nam[i,"year"])
  
  # using d$fieldsample.date
  if(nam[i,"fieldsample.date"]!=""){endday <- strptime(nam[i,"fieldsample.date"],"%Y-%m-%d", tz = "GMT")}
  if(nam[i,"fieldsample.date"]==""){endday <- strptime(paste(yr, "04-30", sep="-"),"%Y-%m-%d", tz = "GMT")} #if no sampling date given, use april 30
  
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
  # now loop over these year-month combo files
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
  }
  
  tempval[[as.character(nam[i,"ID_fieldsample.date"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),
                                                                      Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])#
}


stop("Not an error, just stopping here to say we're now done pulling daily climate data for North America! Next step is to interpolate these to hourly data and estimate chilling from the hourly data.")

