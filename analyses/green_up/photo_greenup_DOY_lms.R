#Code to look at trends in green up doy and photoperiod day frmo 2009-2018
#By Ailene
#Nov 4, 2020

rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/GitHub/ospree/analyses/green_up/")

#put all the greenup doy info together
files<-list.files(path = "./greenup_dates", full.names = FALSE)
allyears<-c()
for(f in 1:length(files)){
  fname = paste("greenup_dates/",files[f],sep="")
  y<-read.csv(fname)
  y$year<-substr(fname, 22,25)
  allyears<-rbind(allyears,y)
}
dim(allyears)

#add columns for year and doy

allyears$doy<-as.integer(format(as.Date(allyears$date),format ="%j"))

allyears$loc<-paste(allyears$lat, allyears$long, sep = "")
locs<-unique(allyears$loc)
doymods<-matrix(data = NA,nrow = length(locs),ncol =6)
dlmods<-matrix(data = NA,nrow = length(locs),ncol =6)
for(i in 1:length(locs)){
  dat<-allyears[allyears$loc==locs[i],]
  if(dim(dat)[1]>3){
    doymod<-lm(dat$doy~as.numeric(dat$year))
    doymods[i,]<-round(c(summary(doymod)$coef[2,],confint(doymod)[2,]),digits=3)
    dlmod<-lm(dat$daylength~as.numeric(dat$year))
    dlmods[i,]<-round(c(summary(dlmod)$coef[2,],confint(dlmod)[2,]),digits=3)
  }
  else {next}
}
colnames(dlmods)<-colnames(doymods)<-c("yr.coef","yr.se","yr.t","yr.p","yr.2.5","yr.97.5") 
dlmods<-as.data.frame(dlmods)
doymods<-as.data.frame(doymods)

hist(dlmods$yr.coef)
range(dlmods$yr.p, na.rm = TRUE)
mean(dlmods$yr.p, na.rm = TRUE)

#plot the effect sizes of both so we can see what they look like
pdf("greenuptrendsplot.pdf", height = 6, width = 10)
par(mfrow = c(1,2))
plot(as.numeric(substr(locs,1,8)),doymods$yr.coef, 
     type = "p",col = alpha("black", 0.5), pch = 16, bty = "l",
     xlab = "Latitude", ylab = "2009-2018 trend", main = "DOY of Greenup")
abline(h=0,col ="red", lty = 2)
for (i in 1:length(locs)){
  arrows(as.numeric(substr(locs,1,8))[i],doymods$yr.2.5[i],as.numeric(substr(locs,1,8))[i],doymods$yr.97.5[i],
         code = 3, length = 0, col = alpha("black", 0.2))
  }
                
plot(as.numeric(substr(locs,1,8)),dlmods$yr.coef, 
     type = "p",col = alpha("black", 0.5), pch = 16, bty = "l",
     xlab = "Latitude", ylab = "2009-2018 trend", main = "Daylength at Greenup")
abline(h=0,col ="red", lty = 2)
for (i in 1:length(locs)){
  arrows(as.numeric(substr(locs,1,8))[i],dlmods$yr.2.5[i],as.numeric(substr(locs,1,8))[i],dlmods$yr.97.5[i],
         code = 3, length = 0, col = alpha("black", 0.2))
}
dev.off()
pdf("dldoytrendsplot.pdf", height = 6, width = 6)
plot(doymods$yr.coef, dlmods$yr.coef,
     type = "p",col = alpha("darkblue", 0.5), pch = 16, bty = "l",
     xlab = "DOY trend", ylab = "Daylength trend")
abline(h= 0, lwd = 2)
abline(v= 0, lwd = 2)

for (i in 1:length(doymods$yr.coef)){
  arrows(doymods$yr.coef[i],dlmods$yr.2.5[i],doymods$yr.coef[i],dlmods$yr.97.5[i],
         code = 3, length = 0, col = alpha("darkblue", 0.2))
}
for (i in 1:length(doymods$yr.coef)){
  arrows(doymods$yr.2.5[i],dlmods$yr.coef[i],doymods$yr.97.5[i],dlmods$yr.coef[i],
         code = 3, length = 0, col = alpha("darkblue", 0.2))
}

dev.off()

pdf("dldoycoefsp.pdf", height = 6, width = 6)
par(mfrow=c(2,2))
boxplot(doymods$yr.coef,main = "DOY of greenup trends")
boxplot(dlmods$yr.p, main = "DOY trend p-values")

boxplot(dlmods$yr.coef,main = "Daylength at greenup trends")
boxplot(dlmods$yr.p, main = "Daylength trend p-values")
dev.off()
  
