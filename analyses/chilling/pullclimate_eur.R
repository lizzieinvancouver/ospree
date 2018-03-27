# Read in netCDF files to pull climate data from europe
# This requires you to work off of the external hard drive with climate data

eur.tempmn <- nc_open(file.path(climatedrive, "tn_0.25deg_reg_v16.0.nc"))
eur.tempmx <- nc_open(file.path(climatedrive, "tx_0.25deg_reg_v16.0.nc"))
#loop through each lat/long for which we want to calculate chilling and pull the climate data for that lat/long
#the climate data that we are pulling is daily min and max temperature
#missing climate data for i=18 (lat38.2 6666667, long15.988)
tempval <- list() 
for(i in 1:nrow(eur)){ # i = 1 
  print(i)
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
  
  #yr <- as.numeric(eur[i,"year"])#
  #do everything in reference to field sample year becuase the year column is too variable
  yr<-as.numeric(substr(eur[i,"fieldsample.date2"],1,4))
  # start and end days of the climate data we need for the lat/long. This is in days since baseline date (sept 1) Set to GMT to avoid daylight savings insanity
  stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")#start day for chilling is september 1
  if(eur[i,"fieldsample.date2"]!="" & as.numeric(substr(eur[i,"fieldsample.date2"],6,7))>=9){stday <- strptime(paste(yr, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}#If field sample date is after september 1, then we use the chilling from the current year.
  if(eur[i,"fieldsample.date2"]!="" & as.numeric(substr(eur[i,"fieldsample.date2"],6,7))<9){stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}#If field sample date is before september 1, then we use the chilling from the previous year.

  # using fieldsample.date2, which is the same as fieldsampledate, but formatted as  "%Y-%m-%d"
  #field sample date2 is the end day for chilling calculations

  if(eur[i,"fieldsample.date2"]!=""){endday <- strptime(eur[i,"fieldsample.date2"],"%Y-%m-%d", tz = "GMT")}
  if(eur[i,"fieldsample.date2"]==""){endday <- strptime(paste(yr, "04-30", sep="-"),"%Y-%m-%d", tz = "GMT")}
  
  st <- as.numeric(as.character(stday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  en <- as.numeric(as.character(endday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  if(en<st){en=st}
  if(endday<stday){endday=stday}
  
  # get temperature values for this date range.
  #if no temperature data for the focal lat/long, choose the next closest one. 
  #the below code cose up to 0.1 degrees (~10km) away from the closest lat/long)
  mintest<-ncvar_get(eur.tempmn,'tn', 
                     start=c(nlong.cell,nlat.cell,st), 
                     count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
  ) 
  if(is.na(unique(mintest))){#if there are no temp data for the selected lat/long, chosee a different one
    ndiff.long.cell[which(ndiff.long.cell==min(ndiff.long.cell, na.rm=TRUE))[1]]<-NA 
    ndiff.lat.cell[which(ndiff.lat.cell==min(ndiff.lat.cell, na.rm=TRUE))[1]]<-NA
    nlong.cell <- which(ndiff.long.cell==min(ndiff.long.cell, na.rm=TRUE))[1]
    nlat.cell <- which(ndiff.lat.cell==min(ndiff.lat.cell, na.rm=TRUE))[1]
    mintest<-ncvar_get(eur.tempmn,'tn', 
                       start=c(nlong.cell,nlat.cell,st), 
                       count=c(1,1,en-st+1) 
    )     
    if(is.na(unique(mintest))){
      ndiff.long.cell[which(ndiff.long.cell==min(ndiff.long.cell, na.rm=TRUE))[1]]<-NA 
      ndiff.lat.cell[which(ndiff.lat.cell==min(ndiff.lat.cell, na.rm=TRUE))[1]]<-NA
      nlong.cell <- which(ndiff.long.cell==min(ndiff.long.cell, na.rm=TRUE))[1]
      nlat.cell <- which(ndiff.lat.cell==min(ndiff.lat.cell, na.rm=TRUE))[1]
      mintest<-ncvar_get(eur.tempmn,'tn', 
                         start=c(nlong.cell,nlat.cell,st), 
                         count=c(1,1,en-st+1) # warnings ok
      ) 
      }}
  maxtest<-ncvar_get(eur.tempmx,'tx', 
                     start=c(xlong.cell,xlat.cell,st), 
                     count=c(1,1,en-st+1) # warnings ok
  ) 
  if(is.na(unique(maxtest))){#if there are no temp data for the selected lat/long, chosee a different one
    xdiff.long.cell[which(xdiff.long.cell==min(xdiff.long.cell, na.rm=TRUE))[1]]<-NA 
    xdiff.lat.cell[which(xdiff.lat.cell==min(xdiff.lat.cell, na.rm=TRUE))[1]]<-NA
    xlong.cell <- which(xdiff.long.cell==min(xdiff.long.cell, na.rm=TRUE))[1]
    xlat.cell <- which(xdiff.lat.cell==min(xdiff.lat.cell, na.rm=TRUE))[1]
    maxtest<-ncvar_get(eur.tempmx,'tx', 
                       start=c(xlong.cell,xlat.cell,st), 
                       count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
    )     
    if(is.na(unique(mintest))){
      xdiff.long.cell[which(xdiff.long.cell==min(xdiff.long.cell, na.rm=TRUE))[1]]<-NA 
      xdiff.lat.cell[which(xdiff.lat.cell==min(xdiff.lat.cell, na.rm=TRUE))[1]]<-NA
      xlong.cell <- which(xdiff.long.cell==min(xdiff.long.cell, na.rm=TRUE))[1]
      xlat.cell <- which(xdiff.lat.cell==min(xdiff.lat.cell, na.rm=TRUE))[1]
      maxtest<-ncvar_get(eur.tempmx,'tx', 
                         start=c(xlong.cell,xlat.cell,st), 
                         count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
      )
    }}
  
  # check the dim of the netcdf file, str(netcdf), and see what the order of the different dimensions are. In this case, it goes long, lat, time. So when we are moving through the file, we give it the long and lat and date of start, then move through the files by going 'up' the cube of data to the end date
  mins <- ncvar_get(eur.tempmn,'tn', 
                    start=c(nlong.cell,nlat.cell,st), 
                    count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
  ) 
  
  maxs <- ncvar_get(eur.tempmx,'tx',
                    start=c(xlong.cell,xlat.cell,st),
                    count=c(1,1,en-st+1)
  )
  
  tempval[[as.character(eur[i,"ID_fieldsample.date2"])]] <- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                                                                      Tmin = mins, Tmax = maxs)#
}


nc_close(eur.tempmx)
nc_close(eur.tempmn)

stop("Not an error, just stopping here to say we're now done 
     pulling daily climate data for Europe! Also, the warnings are ok to ignore!")
