## z-score the predictors:
bb.stan$force.z <- (bb.stan$force-mean(bb.stan$force,na.rm=TRUE))/sd(bb.stan$force,na.rm=TRUE)
bb.stan$photo.z <- (bb.stan$photo-mean(bb.stan$photo,na.rm=TRUE))/sd(bb.stan$photo,na.rm=TRUE)
bb.stan$chill.z <- (bb.stan$chill-mean(bb.stan$chill,na.rm=TRUE))/sd(bb.stan$chill,na.rm=TRUE)
if(use.chillports){#if using chill portions instead of utah
  bb.stan$chill.ports.z <- (bb.stan$chill.ports-mean(bb.stan$chill.ports,na.rm=TRUE))/sd(bb.stan$chill.ports,na.rm=TRUE)}