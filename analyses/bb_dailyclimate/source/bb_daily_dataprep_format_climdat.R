#format the cliamte data, add photoperiod
cdater$Date <- substr(cdater$Date, 1, 10)
cdater$date <- as.Date(cdater$Date, format="%Y-%m-%d")
cdater$year <- as.numeric(format(cdater$date , "%Y"))
cdater$doy <- as.numeric(format(cdater$date , "%j"))

# makes Tmean while we'here ... should check this
cdater$Tmean <- rowMeans(cbind(cdater$Tmin, cdater$Tmax))
# Get ambient photoperiod 
cdater$daylength <- daylength(lat=cdater$lat, doy=cdater$date)

#Climate data from some sites (boyer, ashby62, others) uses a different latitude (growing latitude instead of provenance lat)
#we need to change the latitude here back to the provenance latitude for it to line up
#read in conversion table for growing lat to provenance lat for these sites
#(generated with chilling code)
grotopro<-read.csv("output/dailyclim/chill.lat.conversion.csv", header=TRUE)

for (i in 1:dim(grotopro)[1]){
  cdater$lat[which(cdater$datasetID==grotopro$datasetID[i] & cdater$lat==grotopro$chill.lat[i])]<-grotopro$provenance.lat[i]
  cdater$long[which(cdater$datasetID==grotopro$datasetID[i] & cdater$long==grotopro$chill.long[i])]<-grotopro$provenance.long[i]
}
#cdat <- cdater[which(cdater$datasetID %in% unique(dat$datasetID)),]
cdat <- cdater
#not sure what the difference is between the "date" column and the "Date" column in cdat; using Date for now
daily_ambtemp<-dplyr::select(cdat, datasetID, lat,long,fieldsample.date2,Date,Tmin,Tmax,daylength)
#format the Date columns as dates (otherwise some of the  values get weird...) 
daily_ambtemp$Date<-as.Date(daily_ambtemp$Date)

#format lat and long 
daily_ambtemp$lat<-round(daily_ambtemp$lat, digits=5)
daily_ambtemp$long<-round(daily_ambtemp$long, digits=4)
