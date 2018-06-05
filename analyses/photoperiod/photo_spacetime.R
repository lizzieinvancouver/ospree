#Make a figure showing how photoperiod changes during the year at different larititudes
#Started by Ailene Ettinger, aettinger@fas.harvard.edu
#May 2018
library(geosphere)
library(TeachingDemos)
lats<-c(22.5,45)# focl latitudes
doy<-c(1:365)
quartz(height=6,width=6)

plot(doy,daylength(0,doy), xlim=c(1,380), ylim=c(8,16), xlab="Day of year", ylab="Daylength (hours)", bty="l",type="l", lwd=2)

for(i in 1:length(lats)){
  lines(doy,daylength(lats[i],doy), lwd=2)
}
text.x<-rep(380, times=length(lats)+1)
text.y<-daylength(c(0,lats),365)
text.labs<-c("lat=0","lat=22.5","lat=45")
text(text.x,text.y,text.labs, adj=.5)
#Add points for March 21 and June 21 to lat=22.5 and lat=45
#March 31=doy 91; June 31=doy 182
doys<-c(91,182)
cols<-c("gray","white")
shape<-c(21,22)
for(i in 1:length(lats)){
  points(doys,daylength(lats[i],doys), pch=21,bg=cols[i], cex=1.1)
  
}

#add shifts
#In 100 years, with temporal shifts earlier 3 days per decade (30 days total) as has been observed (Parmesan 2006)- this is a low end
#
for(i in 1:length(lats)){
  arrows(doys,daylength(lats[i],doys), doys-30,daylength(lats[i],doys-30), col="salmon4", length=0.1, lwd=3)
  
}
#Add insets for spatial change- need to zoom in
#zoom in on days 181-183
doy2<-c(181,182,183)
subplot( 
  plot(doy2,daylength(lats[2],doy2),xlim=c(181,183),ylim=c(15.5,16.0), col=1, type="l", mgp=c(1,0.4,0),
       xlab='', ylab='', cex.axis=0.5), 
  x=grconvertX(c(0.5,0.75), from='npc'),
  y=grconvertY(c(0.8,1.05), from='npc'),
  type='fig', pars=list( mar=c(1.5,1.5,0,0)+0.1) )
subplot( 
  points(182,daylength(lats[2],182)-4.5, pch=21,bg=cols[2], cex=1.1), 
  x=grconvertX(c(0.5,0.75), from='npc'),
  y=grconvertY(c(0.8,1.05), from='npc'),
  type='fig' )
points(275,15.5, pch=21,bg=cols[2], col="white",cex=1.1)

#Add point to subplot

#Add spatial shift to inset
#In 100 years, with spatial shifts of ~6km ( or ~0.05 degrees) per decade (0.5 deg total) poleward as has been observed (Parmesan 2006)- this is a low end
photos_spat_2<-daylength(lats[2]+0.5, 182)
for(i in 1:length(lats)){
  arrows(doys,daylength(lats[i],doys), doys,daylength(lats[i]+0.5,doys), col="darkblue", length=0.1, lwd=2)
  
}

#In 100 years, with spatial shifts of ~6km ( or ~0.05 degrees) per decade (0.5 deg total) poleward as has been observed (Parmesan 2006)- this is a low end
photos_spat<-daylength(lat[i]+0.5, 1:181)
for(i in 1:length(lats)){
  arrows(doys,daylength(lats[i],doys), doys,daylength(lats[i]+0.5,doys), col="darkblue", length=0.1, lwd=2)
  
}





#add new points for shift in time
for(i in 1:length(lats)){
  points(doys-30,daylength(lats[i],doys-30), pch=c(21,24),bg=cols[i], cex=1.1)
  
}

#add new points for shift in space
for(i in 1:length(lats)){
  points(doys,daylength(lats[i]+0.5,doys), pch=c(21,24),bg=cols[i], cex=1.1)
  
}

