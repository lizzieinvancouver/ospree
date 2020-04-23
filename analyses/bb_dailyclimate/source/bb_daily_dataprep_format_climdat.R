#format the climate data, add photoperiod
cdater$Date <- substr(cdater$Date, 1, 10)
cdater$date <- as.Date(cdater$Date, format="%Y-%m-%d")
cdater$year <- as.numeric(format(cdater$date , "%Y"))
cdater$doy <- as.numeric(format(cdater$date , "%j"))

# makes Tmean while we'here ... should check this
cdater$Tmean <- rowMeans(cbind(cdater$Tmin, cdater$Tmax))
# Get ambient photoperiod 
cdater$daylength <- geosphere::daylength(lat=cdater$lat, doy=cdater$date)
cdat <- cdater
#not sure what the difference is between the "date" column and the "Date" column in cdat; using Date for now
daily_ambtemp<-dplyr::select(cdat, datasetID, lat,long,fieldsample.date2,Date,Tmin,Tmax,daylength)
#format the Date columns as dates (otherwise some of the  values get weird...) 
daily_ambtemp$Date<-as.Date(daily_ambtemp$Date)

#format lat and long 
daily_ambtemp$lat<-round(daily_ambtemp$lat, digits=5)
daily_ambtemp$long<-round(daily_ambtemp$long, digits=4)
