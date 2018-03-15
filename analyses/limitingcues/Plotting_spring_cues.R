

## to start
rm(list=ls())
options(stringsAsFactors=FALSE)


## load packages
packs.to.extract<-list('raster','ncdf4','maptools','sp','foreach',
                       'doParallel','abind','stringr','rgdal','foreign')
lapply(packs.to.extract,require, character.only=T)

r1<-brick("D:/Vitis_Data/European climate data/tn_0.25deg_reg_v16.0.nc")
r2<-brick("D:/Vitis_Data/European climate data/tx_0.25deg_reg_v16.0.nc")


fagus<-read.csv("~/GitHub/ospree/analyses/limitingcues/output/fagpred.wlatlong.csv")
betula<-read.csv("~/GitHub/ospree/analyses/limitingcues/output/betpred.wlatlong.csv")

hist(fagus$pred1983.pred)
hist(betula$pred1983.pred,20,col="black",add=T)

## function to extract data
get.climate.pep75<-function(dat,year,brick.mins,brick.maxs){

r1sub<-subset(brick.mins,which(grepl(paste("X",year,sep=""),names(brick.mins))))
r2sub<-subset(brick.maxs,which(grepl(paste("X",year,sep=""),names(brick.maxs))))

dat.days<-round(dat$pred1983.pred,0)
dates.matrix=t(apply(as.data.frame(dat.days),1,function(x){return(seq(x-60,x,1))}))
mindates<-min(dates.matrix[,1])
maxdates<-max(dates.matrix[,61])
dat.store<-array(NA,dim=c(nrow(dat),62,2))

for(i in mindates:maxdates){#i=120
  print(i)
  rows.i<-apply(dates.matrix,1,function(x){ifelse(i%in%x,1,0)})
  date.i<-dat.days[i]
  positions.i<-which(rows.i==1)
  coords.i<-subset(dat[,3:2],rows.i==1)
  
  if(length(positions.i)>1){
  dates.mat.sub<-apply(dates.matrix[positions.i,],1,function(x){which(x==i)})
  sorted.unique.matsubs<-sort(unique(dates.mat.sub))
  
  for (j in sorted.unique.matsubs){
    print(paste(i,j))
    pos.j<-which(dates.mat.sub==j)
    dat.store[positions.i[pos.j],j,1]<-extract(r1sub[[j]],coords.i[pos.j,])
    dat.store[positions.i[pos.j],j,2]<-extract(r2sub[[j]],coords.i[pos.j,])
    
  }
  }

  }
dat$chilling<-rowMeans(dat.store[,,1],na.rm=T)
dat$forcing<-rowMeans(dat.store[,,2],na.rm=T)

return(dat)
}


## applying function
fagus.pep75<-get.climate.pep75(fagus,1983,r1,r2)
betula.pep75<-get.climate.pep75(betula,1983,r1,r2)


## plotting results
library(RColorBrewer)
library(colorspace)


# setting vector of colors
vec<-fagus.pep75$chilling+abs(min(fagus.pep75$chilling,na.rm=T))
vec2<-fagus.pep75$forcing+abs(min(fagus.pep75$forcing,na.rm=T))
vec3<-betula.pep75$chilling+abs(min(betula.pep75$chilling,na.rm=T))
vec4<-betula.pep75$forcing+abs(min(betula.pep75$forcing,na.rm=T))

vec.chills<-c(vec,vec3)
vec.forces<-c(vec2,vec4)

cols1<-colorRampPalette(brewer.pal(9,"RdYlBu"))(30)[as.numeric(cut(vec.chills,
                                                                  breaks = 30))[1:length(vec)]]
cols2<-colorRampPalette(brewer.pal(9,"Reds"))(30)[as.numeric(cut(vec.forces,
                                                                 breaks = 30))[1:length(vec2)]]
cols3<-colorRampPalette(brewer.pal(9,"Blues"))(30)[as.numeric(cut(vec.chills,
                                                                  breaks = 30))[(length(vec)+1):(length(vec)+length(vec3))]]
cols4<-colorRampPalette(brewer.pal(9,"Reds"))(30)[as.numeric(cut(vec.forces,
                                                                 breaks = 30))[(length(vec2)+1):(length(vec2)+length(vec4))]]

## get colors & positions for legend
mincol<-which.min(vec.chills)
maxcol<-which.max(vec.chills)
meancol<-which.min(abs(vec.chills-mean(vec.chills,na.rm=T)))
cols.mins<-colorRampPalette(brewer.pal(9,"Blues"))(30)[as.numeric(cut(vec.chills,
            breaks = 30))[c(mincol,meancol,maxcol)]]
valsmins<-c(fagus.pep75$chilling,betula.pep75$chilling)
valsmins<-c(min(valsmins,na.rm=T),
mean(valsmins,na.rm=T),
max(valsmins,na.rm=T))

mincol<-which.min(vec.forces)
maxcol<-which.max(vec.forces)
meancol<-which.min(abs(vec.forces-mean(vec.forces,na.rm=T)))
cols.maxs<-colorRampPalette(brewer.pal(9,"Reds"))(30)[as.numeric(cut(vec.forces,
          breaks = 30))[c(mincol,meancol,maxcol)]]
valsmaxs<-c(fagus.pep75$forcing,betula.pep75$forcing)
valsmaxs<-c(min(valsmaxs,na.rm=T),
            mean(valsmaxs,na.rm=T),
            max(valsmaxs,na.rm=T))


## plots
par(mfrow=c(2,2),mar=c(1,2,3,1))
land<-readShapeSpatial("~/GitHub/regionalrisk/analyses/input/natural_earth_vector/50m_physical/ne_50m_land.shp") ## 
crsss<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
projection(land)<-crsss
plot(land,col="lightgrey",lty=0,
     main="Fagus minimum spring temperatures",
     xlim=c(-5,10),ylim=c(35,66))
points(fagus$lon,fagus$lat,col=adjustcolor(cols1,alpha=0.5),pch=19,cex=1.5)

#legend(ad hoc)
points(cbind(rep(-24,3),seq(46,52,3)),col=cols.mins,pch=19,cex=3)
text(rep(-18,3),seq(46,52,3),round(valsmins,2),cex=2)

plot(land,col="lightgrey",lty=0,
     main="Fagus maximum spring temperatures",
     xlim=c(-5,10),ylim=c(35,66))
points(fagus$lon,fagus$lat,col=adjustcolor(cols2,alpha=0.5),pch=19,cex=1.5)

#legend(ad hoc)
points(cbind(rep(-24,3),seq(46,52,3)),col=cols.maxs,pch=19,cex=3)
text(rep(-18,3),seq(46,52,3),round(valsmaxs,2),cex=2)


plot(land,col="lightgrey",lty=0,
     main="Betula minimum spring temperatures",
     xlim=c(-5,10),ylim=c(35,70))
points(betula$lon,betula$lat,col=adjustcolor(cols3,alpha=0.5),pch=19,cex=1.5)

plot(land,col="lightgrey",lty=0,
     main="Betula maximum spring temperatures",
     xlim=c(-5,10),ylim=c(35,70))
points(betula$lon,betula$lat,col=adjustcolor(cols4,alpha=0.5),pch=19,cex=1.5)

## climate data for 10 years
## clean model
## smooth phenofit model predictions spatilally


############
############ REPLICATE ANALYSIS FOR FUTURE TIMES
############

r1<-brick("D:/Vitis_Data/climatefuture/CorrectedGCMs/GCM.07.Tmin2081-2100.nc")
r2<-brick("D:/Vitis_Data/climatefuture/CorrectedGCMs/GCM.07.Tmax2081-2100.nc")
r1p<-brick("D:/Vitis_Data/climatefuture/CorrectedGCMs/GCM.07.Tmin1950-2005.nc")
r2p<-brick("D:/Vitis_Data/climatefuture/CorrectedGCMs/GCM.07.Tmax1950-2005.nc")

source("~/GitHub/vin/climatefuture/analyses/projections/input/Script_GCM_rename_corrected_bias.R")
r1.names<-rename.GCM(r1,2081:2100)
r2.names<-rename.GCM(r2,2081:2100)
names(r1)=r1.names
names(r2)=r2.names


##   



## function to extract data
get.futclimate.pep75<-function(dat,years,brick.mins,brick.maxs){
  
  list.futclimates=list()
  
  for (k in years){
  
  r1sub<-subset(brick.mins,which(grepl(paste("X.",k,sep=""),names(brick.mins))))
  r2sub<-subset(brick.maxs,which(grepl(paste("X.",k,sep=""),names(brick.maxs))))
  
  dat.days<-round(dat$pred1983.pred,0)
  dates.matrix=t(apply(as.data.frame(dat.days),1,function(x){return(seq(x-60,x,1))}))
  mindates<-min(dates.matrix[,1])
  maxdates<-max(dates.matrix[,61])
  dat.store<-array(NA,dim=c(nrow(dat),62,2))
  
  for(i in mindates:maxdates){#i=120
    print(paste(j,i))
    rows.i<-apply(dates.matrix,1,function(x){ifelse(i%in%x,1,0)})
    date.i<-dat.days[i]
    positions.i<-which(rows.i==1)
    coords.i<-subset(dat[,3:2],rows.i==1)
    
    if(length(positions.i)>1){
      dates.mat.sub<-apply(dates.matrix[positions.i,],1,function(x){which(x==i)})
      sorted.unique.matsubs<-sort(unique(dates.mat.sub))
      
      for (j in sorted.unique.matsubs){
        print(paste(i,j))
        pos.j<-which(dates.mat.sub==j)
        dat.store[positions.i[pos.j],j,1]<-extract(r1sub[[j]],coords.i[pos.j,])
        dat.store[positions.i[pos.j],j,2]<-extract(r2sub[[j]],coords.i[pos.j,])
        
      }
    }
    
  }
  dat$chilling<-rowMeans(dat.store[,,1],na.rm=T)
  dat$forcing<-rowMeans(dat.store[,,2],na.rm=T)
  list.futclimates[[which(years==k)]]<-dat
  }
  return(list.futclimates)
}


## applying function
fagus.pep75fut<-get.futclimate.pep75(fagus,2081:2090,r1,r2)
betula.pep75fut<-get.futclimate.pep75(betula,2081:2085,r1,r2)


for(i in 1:length(fagus.pep75fut)){
  
  fagus<-cbind(fagus,fagus.pep75fut[[i]][,8:9])
  
}

for(i in 1:length(betula.pep75fut)){
  
  betula<-cbind(betula,betula.pep75fut[[i]][,8:9])
  
}
head(betula)

fagussave<-fagus[,1:7]
fagussave<-cbind(fagussave,fagus[,seq(8,26,2)],fagus[,seq(9,27,2)])
write.csv(fagussave,"~/MEGA/Work_Harvard_postdoc/Ospree/limiting cues project - Lizzie/Fagus.pep75fut10years.chill.force.csv")

fagussave<-betula[,1:7]
fagussave<-cbind(fagussave,betula[,seq(8,16,2)],betula[,seq(9,17,2)])
write.csv(fagussave,"~/MEGA/Work_Harvard_postdoc/Ospree/limiting cues project - Lizzie/Betula.pep75fut10years.chill.force.csv")

## applying function
fagus.pep75past<-get.futclimate.pep75(fagus,1983,r1p,r2p)
betula.pep75past<-get.futclimate.pep75(betula,1983,r1p,r2p)

write.csv(fagus.pep75,"Fagus.pep75.chill.force.csv")
write.csv(betula.pep75,"Betula.pep75.chill.force.csv")
write.csv(fagus.pep75fut,"Fagus.pep75.chill.force_fut.csv")
write.csv(betula.pep75fut,"Betula.pep75.chill.force_fut.csv")



## plotting results
library(RColorBrewer)
library(colorspace)


# setting vector of colors
vec<-fagus.pep75$chilling+abs(min(fagus.pep75$chilling,na.rm=T))
vec2<-fagus.pep75$forcing+abs(min(fagus.pep75$forcing,na.rm=T))
vec3<-betula.pep75$chilling+abs(min(betula.pep75$chilling,na.rm=T))
vec4<-betula.pep75$forcing+abs(min(betula.pep75$forcing,na.rm=T))

vec.chills<-c(vec,vec3)
vec.forces<-c(vec2,vec4)

cols1<-colorRampPalette(brewer.pal(9,"Blues"))(30)[as.numeric(cut(vec.chills,
                                                                   breaks = 30))[1:length(vec)]]
cols2<-colorRampPalette(brewer.pal(9,"Reds"))(30)[as.numeric(cut(vec.forces,
                                                                 breaks = 30))[1:length(vec2)]]
cols3<-colorRampPalette(brewer.pal(9,"Blues"))(30)[as.numeric(cut(vec.chills,
                                                                  breaks = 30))[(length(vec)+1):(length(vec)+length(vec3))]]
cols4<-colorRampPalette(brewer.pal(9,"Reds"))(30)[as.numeric(cut(vec.forces,
                                                                 breaks = 30))[(length(vec2)+1):(length(vec2)+length(vec4))]]

## get colors & positions for legend
mincol<-which.min(vec.chills)
maxcol<-which.max(vec.chills)
meancol<-which.min(abs(vec.chills-mean(vec.chills,na.rm=T)))
cols.mins<-colorRampPalette(brewer.pal(9,"Blues"))(30)[as.numeric(cut(vec.chills,
                                                                      breaks = 30))[c(mincol,meancol,maxcol)]]
valsmins<-c(fagus.pep75$chilling,betula.pep75$chilling)
valsmins<-c(min(valsmins,na.rm=T),
            mean(valsmins,na.rm=T),
            max(valsmins,na.rm=T))

mincol<-which.min(vec.forces)
maxcol<-which.max(vec.forces)
meancol<-which.min(abs(vec.forces-mean(vec.forces,na.rm=T)))
cols.maxs<-colorRampPalette(brewer.pal(9,"Reds"))(30)[as.numeric(cut(vec.forces,
                                                                     breaks = 30))[c(mincol,meancol,maxcol)]]
valsmaxs<-c(fagus.pep75$forcing,betula.pep75$forcing)
valsmaxs<-c(min(valsmaxs,na.rm=T),
            mean(valsmaxs,na.rm=T),
            max(valsmaxs,na.rm=T))


## plots
par(mfrow=c(2,2),mar=c(1,2,3,1))
land<-readShapeSpatial("~/GitHub/regionalrisk/analyses/input/natural_earth_vector/50m_physical/ne_50m_land.shp") ## 
crsss<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
projection(land)<-crsss
plot(land,col="lightgrey",lty=0,
     main="Fagus minimum spring temperatures",
     xlim=c(-5,10),ylim=c(35,66))
points(fagus$lon,fagus$lat,col=adjustcolor(cols1,alpha=0.5),pch=19,cex=1.5)

#legend(ad hoc)
points(cbind(rep(-24,3),seq(46,52,3)),col=cols.mins,pch=19,cex=3)
text(rep(-18,3),seq(46,52,3),round(valsmins,2),cex=2)

plot(land,col="lightgrey",lty=0,
     main="Fagus maximum spring temperatures",
     xlim=c(-5,10),ylim=c(35,66))
points(fagus$lon,fagus$lat,col=adjustcolor(cols2,alpha=0.5),pch=19,cex=1.5)

#legend(ad hoc)
points(cbind(rep(-24,3),seq(46,52,3)),col=cols.maxs,pch=19,cex=3)
text(rep(-18,3),seq(46,52,3),round(valsmaxs,2),cex=2)


plot(land,col="lightgrey",lty=0,
     main="Betula minimum spring temperatures",
     xlim=c(-5,10),ylim=c(35,70))
points(betula$lon,betula$lat,col=adjustcolor(cols3,alpha=0.5),pch=19,cex=1.5)

plot(land,col="lightgrey",lty=0,
     main="Betula maximum spring temperatures",
     xlim=c(-5,10),ylim=c(35,70))
points(betula$lon,betula$lat,col=adjustcolor(cols4,alpha=0.5),pch=19,cex=1.5)



