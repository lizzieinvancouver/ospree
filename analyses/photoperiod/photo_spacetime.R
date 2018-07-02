#Make a figure showing how photoperiod changes during the year at different latititudes
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
text.y<-daylength(lats,365)
text.labs<-c("lat=22.5","lat=45")
text(text.x,text.y,text.labs, adj=.5)
#Add points for March 21 and June 21 to lat=22.5 and lat=45
#March 31=doy 91; June 31=doy 182
doys<-c(91,182)
cols<-c("gray","white")
shape<-c(21,22)

#for(i in 1:length(lats)){
#for lat 45, do both doys
points(doys,daylength(45,doys), pch=21,bg=cols[2], cex=1.1)
  
#for lat 22.5, just  the second doy
points(doys[2],daylength(22.5,doys[2]), pch=21,bg=cols[2], cex=1.1)

#}
#add rectangles for zoom-in around each point
#for(i in 1:length(lats)){
  rect(doys-9,daylength(45,doys)+-.2,doys+9,daylength(45,doys)--.2)
  rect(doys[2]-9,daylength(22.5,doys[2])+-.2,doys[2]+9,daylength(22.5,doys[2])--.2)
  
#}
#add shifts
#In 100 years, with temporal shifts earlier 3 days per decade (30 days total) as has been observed (Parmesan 2006)- this is a low end
#in 100 years, spatial shifts observed =0.5 degrees poleward (Parmesan)
lat.shift<-0.5
temp.shift<-30
#for(i in 1:length(lats)){
  arrows(doys,daylength(45,doys), doys-temp.shift,daylength(45,doys-temp.shift), col="salmon4", length=0.1, lwd=3)
  arrows(doys[2],daylength(22.5,doys[2]), doys[2]-temp.shift,daylength(22.5,doys[2]-temp.shift), col="salmon4", length=0.1, lwd=3)
  
#}
#Add insets for spatial change- need to zoom in
#zoom in on days 181-183 for latitude 45
doy2<-c(181,182,183)
ins1<-subplot( 
  plot(doy2,daylength(lats[2],doy2),xlim=c(181,183),ylim=c(15.54,15.64), col=1, type="l",lwd=2, mgp=c(1,0.4,0),
       xlab='', ylab='', cex.axis=0.5, cex.lab=0.5,xaxt="n",tck=-0.02), 
  x=grconvertX(c(0.65,0.9), from='npc'),
  y=grconvertY(c(0.6,0.85), from='npc'),
  type='fig', pars=list( mar=c(2,1.5,0,0)+0.1) )
par(new = T)
subplot(
  par(new = T),
  points(182,daylength(lats[2],182), pch=21,bg=cols[2], cex=1.1), 
  x=grconvertX(c(0.65,0.9), from='npc'),
  y=grconvertY(c(0.6,0.85), from='npc'),
  type='fig' )

#plot.new()
#Add point to subplot
points(315,15.2, pch=21,bg=cols[2],cex=2)
spatshift_2<-daylength(lats[2]+lat.shift, 182)-daylength(lats[2],182)
arrows(315,15.2, 315,16, col="darkblue", length=0.1, lwd=3)
#zoom in on days 181-183 for latitude 22.5
doy2<-c(181,182,183)
ins2<-subplot( 
  plot(doy2,daylength(lats[1],doy2),xlim=c(181,183),ylim=c(13.45,13.55), col=1, type="l",lwd=2, mgp=c(1,0.4,0),
       xlab='', ylab='', cex.axis=0.5, cex.lab=0.5,tck=-0.02), 
  x=grconvertX(c(0.65,0.9), from='npc'),
  y=grconvertY(c(0.45,0.70), from='npc'),
  type='fig', pars=list( mar=c(2,1.5,0,0)+0.1) )
par(new = T)
subplot(
  par(new = T),
  points(182,daylength(lats[2],182), pch=21,bg=cols[2], cex=1.1), 
  x=grconvertX(c(0.65,0.9), from='npc'),
  y=grconvertY(c(0.8,1.05), from='npc'),
  type='fig' )

#Add point to subplot
points(315,13.9, pch=21,bg=cols[2],cex=2)
spatshift_2<-daylength(lats[1]+lat.shift, 182)-daylength(lats[1],182)
arrows(315,13.9, 315,14.5, col="darkblue", length=0.1, lwd=3)

##Add insets for spatial change- need to zoom in
#zoom in on days 90-92 for latitude 45
doy2<-c(90,91,92)
ins3<-subplot( 
  plot(doy2,daylength(lats[2],doy2),xlim=c(90,92),ylim=c(12.7,12.8), col=1, type="l",lwd=2, mgp=c(1,0.4,0),
       xlab='', ylab='', cex.axis=0.5, cex.lab=0.5,tck=-0.02), 
  x=grconvertX(c(.3,0.55), from='npc'),
  y=grconvertY(c(0.2,0.45), from='npc'),
  type='fig', pars=list( mar=c(2,1.5,0,0)+0.1) )
par(new = T)
subplot(
  par(new = T),
  points(91,daylength(lats[2],91), pch=21,bg=cols[2], cex=1.1), 
  x=grconvertX(c(.3,0.55), from='npc'),
  y=grconvertY(c(0.2,0.45), from='npc'),
  type='fig' )
#Add point to subplot
points(175,11.1, pch=21,bg=cols[2],cex=2)
spatshift_2<-daylength(lats[2]+lat.shift, 91)-daylength(lats[2],91)
arrows(175,11.1, 175,11.3, col="darkblue", length=0.1, lwd=3)
legend(0,17.5,legend=c("Temporal shift","Spatial shift"), lty=1,col=c("salmon4","darkblue"), lwd=3, bty="n")


#add lines connecting the boxes- coudn't figure out how to get x-y coords for inserted plots
#  rect(doys-10,daylength(lats[i],doys)+-.2,doys+11,daylength(lats[i],doys)--.2)

#for(i in 1:length(lats)){
#  arrows(doys-10,daylength(lats[i],doys)+-.2, doys-temp.shift,daylength(lats[i],doys-temp.shift), col="salmon4", length=0.1, lwd=3)
  
#}
