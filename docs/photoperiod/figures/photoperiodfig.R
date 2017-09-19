#Figure of temporal and spatial effects on daylength for OSPREE photoperiod paper:
#Started 18 Sept 2017 by Ailene 
#examples from geosphere package
#https://www.rdocumentation.org/packages/geosphere/versions/1.5-5/topics/daylength

library(geosphere)#daylength(lat, doy)
#generate a vector of daylengths from January 1-June 30 for Washington DC
photo_dc<-daylength(39.0, 1:181)
#generate a vector of daylengths from January 1-June 30 for Montreal
photo_mont<-daylength(45.5, 1:181)

#In 100 years, with temporal shifts earlier 3 days per decade (30 days total) as has been observed (Parmesan 2006)- this is a low end
photo_dc_temp<-daylength(39, c(336:365,1:151))
photo_mont_temp<-daylength(45.5, c(336:365,1:151))

#In 100 years, with spatial shifts of ~6km ( or ~0.05 degrees) per decade (0.5 deg total) poleward as has been observed (Parmesan 2006)- this is a low end
photo_dc_spat<-daylength(39.5, 1:181)
photo_mont_spat<-daylength(46, 1:181)

#Plot
quartz(height=6,width=7)
x<-seq(1:181)
plot(x,photo_dc_temp-photo_dc, type="l", col="red", lwd=2,lty=1,xaxt='n',ylab="Change in daylength (h)",xlab="Month",ylim=c(-1.6,.2))
lines(x,photo_dc_spat-photo_dc, type="l", col="red", lwd=2,lty=2)
lines(x,photo_mont_temp-photo_mont, type="l", col="blue", lwd=2,lty=1)
lines(x,photo_mont_spat-photo_mont, type="l", col="blue", lwd=2,lty=2)
mtext("DC (39 deg)",side=1,line=-4.3, col="red")
mtext("Montreal (45.5 deg)",side=1,line=-0.8, col="blue")
abline(h=0, lwd=2)
axis(1, at=c(1,32,60,91,122,152,182),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul"))
#legend(150,-1.4,legend=c("Space","Time"), bty="n", lty = c(1,2), lwd=2)

#Try making bar plot by month to see if this shows it more dramatically
date<-strptime(c(seq(1:181)), format = "%j")
month<-substr(date,6,7)
day<-substr(date,9,10)
#photo.df<-cbind(month,day,photo_dc_spat,photo_mont_spat,photo_dc_temp,photo_mont_temp)
photo_dc_spat_month<-aggregate(photo_dc_spat-photo_dc,by=list(month),mean)
photo_dc_temp_month<-aggregate(photo_dc_temp-photo_dc,by=list(month),mean)
photo_mont_spat_month<-aggregate(photo_mont_spat-photo_mont,by=list(month),mean)
photo_mont_temp_month<-aggregate(photo_mont_temp-photo_mont,by=list(month),mean)
quartz(height=5,width=10)
par(mfrow=c(1,2))
barplot(rbind(photo_dc_temp_month$x,photo_dc_spat_month$x),beside=T,ylab="Change in daylength (h)",names.arg = c("Jan","Feb","Mar","Apr","May","Jun"), main="DC (39 deg)")
abline(h=0, lwd=1)
barplot(rbind(photo_mont_temp_month$x,photo_mont_spat_month$x),beside=T,ylab="Change in daylength (h)",names.arg = c("Jan","Feb","Mar","Apr","May","Jun"), main="Montreal (45.5 deg)")
abline(h=0, lwd=1)

#no real difference between dc and montreal in overall patterns. look at lats that are further apart
#choose any lats
lat<-c(15,30,45)
quartz(height=3,width=11)
par(mfrow=c(1,3))
for(i in 1:length(lat)){
  photos<-daylength(lat[i], 1:181)
  #In 100 years, with temporal shifts earlier 3 days per decade (30 days total) as has been observed (Parmesan 2006)- this is a low end
  photos_temp<-daylength(lat[i], c(336:365,1:151))
  #In 100 years, with spatial shifts of ~6km ( or ~0.05 degrees) per decade (0.5 deg total) poleward as has been observed (Parmesan 2006)- this is a low end
  photos_spat<-daylength(lat[i]+0.5, 1:181)
  date<-strptime(c(seq(1:181)), format = "%j")
  month<-substr(date,6,7)
  photos_spat_month<-aggregate(photos_spat-photos,by=list(month),mean)
  photos_temp_month<-aggregate(photos_temp-photos,by=list(month),mean)
  barplot(rbind(photos_temp_month$x,photos_spat_month$x),beside=T,ylab="Change in daylength (h)",names.arg = c("Jan","Feb","Mar","Apr","May","Jun"), ylim=c(-1.6,.4),main=c(paste("lat=",lat[i])))
  abline(h=0, lwd=1)
  if(i==1){legend("bottomleft",legend=c("Shifts in Space","Shifts in Time"),fill=c("white","darkgray"),bty="n")}
}

