#Make table summarizing climate and bb for betpen and fagsyl sites
# housekeeping
options(stringsAsFactors = FALSE)

lat1<-spests[spests$lat==46.8167 & spests$lon==12.8 & spests$X=="bb.wintemp.0",]
lat2<-spests[spests$lat==48.3167 & spests$lon==15.8167 & spests$X=="bb.wintemp.0",]
lat3<-spests[spests$lat==48.7833 & spests$lon==15.4 & spests$X=="bb.wintemp.0",]
lat4<-spests[spests$lat==46.7167 & spests$lon==15.7667 & spests$X=="bb.wintemp.0",]
alllats<-rbind(lat1,lat2,lat3,lat4)
alllats$chill.utah<-alllats$chill.forecast*240
alllats$species<-c("Betula pendula","","Fagus sylvatica","")
clim.tab<-subset(alllats,select=c(species,lat,lon,sprT.forecast,winT.forecast))
colnames(clim.tab)<-c("Species","Latitude","Longitude","Spring Temp","Winter Temp")
