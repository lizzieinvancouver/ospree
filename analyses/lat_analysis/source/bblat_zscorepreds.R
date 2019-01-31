## z-score the predictors:
lat.stan$force.z <- (lat.stan$force-mean(lat.stan$force,na.rm=TRUE))/sd(lat.stan$force,na.rm=TRUE)
lat.stan$photo.z <- (lat.stan$photo-mean(lat.stan$photo,na.rm=TRUE))/sd(lat.stan$photo,na.rm=TRUE)
lat.stan$chill.z <- (lat.stan$chill-mean(lat.stan$chill,na.rm=TRUE))/sd(lat.stan$chill,na.rm=TRUE)
lat.stan$lat.z <- (lat.stan$provenance.lat-mean(lat.stan$provenance.lat,na.rm=TRUE))/sd(lat.stan$provenance.lat,na.rm=TRUE)
if(use.chillports){#if using chill portions instead of utah
  lat.stan$chill.ports.z <- (lat.stan$chill.ports-mean(lat.stan$chill.ports,na.rm=TRUE))/sd(lat.stan$chill.ports,na.rm=TRUE)}
