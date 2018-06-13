#Make a figure showing how photoperiod changes during the year at different larititudes
#Started by Ailene Ettinger, aettinger@fas.harvard.edu
#May 2018
library(geosphere)
library(TeachingDemos)
lats<-c(22.5,45)# focl latitudes
doy<-c(1:365)
quartz(height=6.5,width=6)
par(oma=c(0.1,0.1,0.1,0.1), mar=c(5,4,4,2) + 0.1)
plot(doy,daylength(0,doy), xlim=c(1,380), ylim=c(8,17.5), xlab="Day of year", ylab="Daylength (hours)", bty="l",type="l", lwd=2)

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
#add rectangles for zoom-in around each point
for(i in 1:length(lats)){
  rect(doys-10,daylength(lats[i],doys)+-.2,doys+11,daylength(lats[i],doys)--.2)
  
}
#add shifts
#In 100 years, with temporal shifts earlier 3 days per decade (30 days total) as has been observed (Parmesan 2006)- this is a low end
#
for(i in 1:length(lats)){
  arrows(doys,daylength(lats[i],doys), doys-30,daylength(lats[i],doys-30), col="salmon4", length=0.1, lwd=3)
  
}
#Add insets for spatial change- need to zoom in
#zoom in on days 181-183 for latitude 45
doy2<-c(181,182,183)
ins1<-subplot( 
  plot(doy2,daylength(lats[2],doy2),xlim=c(181,183),ylim=c(15.54,15.64), col=1, type="l",lwd=2, mgp=c(1,0.4,0),
       xlab='', ylab='', cex.axis=0.5, cex.lab=0.5), 
  x=grconvertX(c(0.5,0.75), from='npc'),
  y=grconvertY(c(0.72,0.97), from='npc'),
  type='fig', pars=list( mar=c(2,1.5,0,0)+0.1) )
par(new = T)
subplot(
  par(new = T),
  points(182,daylength(lats[2],182), pch=21,bg=cols[2], cex=1.1), 
  x=grconvertX(c(0.5,0.75), from='npc'),
  y=grconvertY(c(0.72,0.97), from='npc'),
  type='fig' )

#plot.new()
#Add point to subplot
points(255,16.4, pch=21,bg=cols[2],cex=2)
spatshift_2<-daylength(lats[2]+0.5, 182)-daylength(lats[2],182)
arrows(255,16.4, 255,17.3, col="darkblue", length=0.1, lwd=3)
#zoom in on days 181-183 for latitude 22.5
doy2<-c(181,182,183)
ins2<-subplot( 
  plot(doy2,daylength(lats[1],doy2),xlim=c(181,183),ylim=c(13.45,13.55), col=1, type="l",lwd=2, mgp=c(1,0.4,0),
       xlab='', ylab='', cex.axis=0.5, cex.lab=0.5), 
  x=grconvertX(c(0.65,0.9), from='npc'),
  y=grconvertY(c(0.45,0.70), from='npc'),
  type='fig', pars=list( mar=c(2,1.5,0,0)+0.1) )
par(new = T)
subplot(
  par(new = T),
  points(182,daylength(lats[2],182), pch=21,bg=cols[2], cex=1.1), 
  x=grconvertX(c(0.5,0.75), from='npc'),
  y=grconvertY(c(0.8,1.05), from='npc'),
  type='fig' )

#Add point to subplot
points(318,13.9, pch=21,bg=cols[1],cex=2)
spatshift_2<-daylength(lats[1]+0.5, 182)-daylength(lats[1],182)
arrows(318,13.9, 318,14.5, col="darkblue", length=0.1, lwd=3)

##Add insets for spatial change- need to zoom in
#zoom in on days 90-92 for latitude 45
doy2<-c(90,91,92)
ins3<-subplot( 
  plot(doy2,daylength(lats[2],doy2),xlim=c(90,92),ylim=c(12.7,12.8), col=1, type="l",lwd=2, mgp=c(1,0.4,0),
       xlab='', ylab='', cex.axis=0.5, cex.lab=0.5), 
  x=grconvertX(c(-.03,0.23), from='npc'),
  y=grconvertY(c(0.5,0.75), from='npc'),
  type='fig', pars=list( mar=c(2,1.5,0,0)+0.1) )
par(new = T)
subplot(
  par(new = T),
  points(91,daylength(lats[2],91), pch=21,bg=cols[2], cex=1.1), 
  x=grconvertX(c(-.03,0.23), from='npc'),
  y=grconvertY(c(0.5,0.75), from='npc'),
  type='fig' )

#plot.new()
#Add point to subplot
points(41,14, pch=21,bg=cols[2],cex=2)
spatshift_2<-daylength(lats[2]+0.5, 91)-daylength(lats[2],91)
arrows(41,14,41,14.3, col="darkblue", length=0.1, lwd=3)

#zoom in on days 181-183 for latitude 22.5
doy2<-c(90,91,92)
ins4<-subplot( 
  plot(doy2,daylength(lats[1],doy2),xlim=c(90,92),ylim=c(12.3,12.4), col=1, type="l",lwd=2, mgp=c(1,0.4,0),
       xlab='', ylab='', cex.axis=0.5, cex.lab=0.5), 
  x=grconvertX(c(.25,0.5), from='npc'),
  y=grconvertY(c(0.1,0.35), from='npc'),
  type='fig', pars=list( mar=c(2,1.5,0,0)+0.1) )
par(new = T)
subplot(
  par(new = T),
  points(91,daylength(lats[2],91), pch=21,bg=cols[2], cex=1.1), 
  x=grconvertX(c(.25,0.5), from='npc'),
  y=grconvertY(c(0.1,0.35), from='npc'),
  type='fig' )

#Add point to subplot
points(150,10.45, pch=21,bg=cols[1],cex=2)
spatshift_2<-daylength(lats[1]+0.5, 91)-daylength(lats[1],91)
arrows(150,10.45, 150,10.65, col="darkblue", length=0.1, lwd=3)
legend(0,17.5,legend=c("Temporal shift","Spatial shift"), lty=1,col=c("salmon4","darkblue"), lwd=3, bty="n")


#add lines connecting the boxes- coudn't figure out how to get x-y coords for inserted plots
#  rect(doys-10,daylength(lats[i],doys)+-.2,doys+11,daylength(lats[i],doys)--.2)

#for(i in 1:length(lats)){
#  arrows(doys-10,daylength(lats[i],doys)+-.2, doys-30,daylength(lats[i],doys-30), col="salmon4", length=0.1, lwd=3)
  
#}

#Try alternative figure without inset boxes:
lats<-c(22.5,45)# focl latitudes
doy<-c(1:365)
quartz(height=6.5,width=6)
par(oma=c(0.1,0.1,0.1,0.1), mar=c(5,4,4,2) + 0.1)
plot(doy,daylength(0,doy), xlim=c(1,380), ylim=c(8,17.5), xlab="Day of year", ylab="Daylength (hours)", bty="l",type="l", lwd=2)

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

for(i in 1:length(lats)){
  arrows(doys-40,daylength(lats[i],doys), doys-40,daylength(lats[i],doys-30), col="salmon4", length=0.1, lwd=2,code=3, angle=90)
  
}
#Add spatial shift to inset
#In 100 years, with spatial shifts of ~6km ( or ~0.05 degrees) per decade (0.5 deg total) poleward as has been observed (Parmesan 2006)- this is a low end
photos_spat_2<-daylength(lats[2]+0.5, 182)
for(i in 1:length(lats)){
  arrows(doys+10,daylength(lats[i],doys), doys+10,daylength(lats[i]+0.5,doys), col="darkblue", length=0.05, lwd=2, code=3, angle=90)
  text(doys+15,daylength(lats[i],doys),label="deltadlh space", )
}
