# Read in netCDF files to pull climate data from europe
# This requires you to work off of external hard drive

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
  if(eur[i,"fieldsample.date2"]!="" & as.numeric(substr(eur[i,"fieldsample.date2"],6,7))>=9){stday <- strptime(paste(yr, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  if(eur[i,"fieldsample.date2"]!="" & as.numeric(substr(eur[i,"fieldsample.date2"],6,7))<9){stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  if(eur[i,"fieldsample.date2"]==""){stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  
  # using fieldsample.date2
  if(eur[i,"fieldsample.date2"]!=""){endday <- strptime(eur[i,"fieldsample.date2"],"%Y-%m-%d", tz = "GMT")}
  if(eur[i,"fieldsample.date2"]==""){endday <- strptime(paste(yr, "04-30", sep="-"),"%Y-%m-%d", tz = "GMT")}
  
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
  
  tempval[[as.character(eur[i,"ID_fieldsample.date2"])]] <- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                                                                      Tmin = mins, Tmax = maxs)#
}


nc_close(eur.tempmx)
nc_close(eur.tempmn)

stop("Not an error, just stopping here to say we're now done pulling daily climate data for Europe!")
