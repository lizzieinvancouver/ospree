# Read in netCDF files to pull climate data from europe
# This requires you to work off of the external hard drive with climate data
# This code pulls data for the calendar yar before and after the field sample date
# because PMP requires full year of data to run
# To use, run through steps 1-4b in chillmerge_all.R; 
# then use this file and pullclimate_nam_pnp.R
# Ailene got a start on this: June 13, 2017

eur.tempmn <- nc_open(file.path(climatedrive, "tn_0.25deg_reg_v15.0.nc"))
eur.tempmx <- nc_open(file.path(climatedrive, "tx_0.25deg_reg_v15.0.nc"))
#loop through each lat/long for which we want to calculate chilling and pull the climate data for that lat/long
#the climate data that we are pulling is daily min and max temperature
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
  
  #yr <- as.numeric(eur[i,"year"])#old code-realized this was wrong July 2017 and changed to reference year in field sample date instead
  yr <-as.numeric(substr(eur[i,"fieldsample.date2"],1,4))#year for climate data
            
  
  # start and end days of the climate data we need for the lat/long. 
  #This is in days since baseline date (Jan 1) Set to GMT to avoid daylight savings insanity
  lastyr<-2014#last year for which there are climate data in our current dataset
  stday <- strptime(paste(yr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")#start day for climate data in PNP must be Jan 1, i think and go for full year
  if(eur[i,"fieldsample.date2"]!="" & as.numeric(substr(eur[i,"fieldsample.date2"],6,7))>=9){stday <- strptime(paste(yr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")}#If field sample date is after september 1, then we use the chilling from the current year only.
  if(eur[i,"fieldsample.date2"]!="" & as.numeric(substr(eur[i,"fieldsample.date2"],6,7))<9){stday <- strptime(paste(yr-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")}#If field sample date is before september 1, then we pull the climate from the previous year, too.
  if(eur[i,"fieldsample.date2"]==""){stday <- strptime(paste(yr-1, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  #add a line of code here that makes 2014 the last year, if 2015 would be the year that 
  
  # using fieldsample.date2, which is the same as fieldsampledate, but formatted as  "%Y-%m-%d"
  # field sample date2 is the end day for chilling calculations, but for pnp we will pull climate throughout 
  # the end of the calendar year after field sample date

  endday <- strptime(paste(yr+1, "12-31", sep="-"),"%Y-%m-%d", tz="GMT")#end day for climate data should be dec 31 of following year, i think
  if(eur[i,"fieldsample.date2"]!="" & as.numeric(substr(eur[i,"fieldsample.date2"],1,4))>lastyr){endday <- strptime(paste(lastyr, "12-31", sep="-"),"%Y-%m-%d", tz="GMT")}#If year of field sample date is after the last year of climate data, then just pull data from the last year available
  
  st <- as.numeric(as.character(stday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  en <- as.numeric(as.character(endday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  if(en<st){en=st}
  if(endday<stday){endday=stday}
  
  # get temperature values for this date range.
  # check the dim of the netcdf file, str(netcdf), and see what the order of the different dimensions are. In this case, it goes long, lat, time. So when we are moving through the file, we give it the long and lat and date of start, then move through the files by going 'up' the cube of data to the end date
  mins <- ncvar_get(eur.tempmn,'tn', 
                    start=c(nlong.cell,nlat.cell,st), 
                    count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
  ) ###This is where lizzie gets the error, start plus count exceeds dimension bounds
  
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
