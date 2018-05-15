#Make a figure showing how photoperiod changes during the year at different larititudes
#Started by Ailene Ettinger, aettinger@fas.harvard.edu
#May 2018
lats<-c(22.5,45)# focl latitudes
doy<-c(1:365)
quartz()

plot(doy,daylength(0,doy), xlim=c(1,365), ylim=c(0,24), xlab="Day of year", ylab="Daylength (hours)", type="l", lwd=2)

for(i in 1:length(lats)){
  lines(doy,daylength(lats[i],doy), lwd=2)
}