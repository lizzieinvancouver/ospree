###Map changes in chilling and budburst, based on forecasting with our OSPREE model
###based on Cat's "Mapping_fsmatspace.R code" (Thanks Cat!)
###Started on 5 April 2019
###By Ailene

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#load libraries
library(ggplot2)
library(dplyr)
library(egg)
library(RColorBrewer)
library(maptools)
library(rworldmap)
library(maps)
#library(autoimage)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

#read in file with species, lat/long, and bbdoy forecast (from pep data)
spests<-read.csv("..//output/betpen_for3dplot/betpen.forecast.forheatmap.csv", header=TRUE)
#sort dataset based on lat
spests <- spests[order(spests$lat, spests$warming),]
#calculate difference in bb between no warming and warmed for each row
spestsnowarm<-spests[spests$warming_C==0,]
spestsnowarm<-subset(spestsnowarm,select=c(lat,lon,warming_C,sprT.forecast,winT.forecast,chill.forecast,bb.sprtemp.0))
spests1warm<-spests[spests$warming_C==1,]
spests1warm<-subset(spests1warm,select=c(lat,lon,warming_C,sprT.forecast,winT.forecast,chill.forecast,bb.sprtemp.1))
spests1warm$bbchange<-spests1warm$bb.sprtemp.1-spestsnowarm$bb.sprtemp.0
spests1warm$chillchange<-spests1warm$chill.forecast-spestsnowarm$chill.forecast

spests2warm<-spests[spests$warming_C==2,]
spests2warm<-subset(spests2warm,select=c(lat,lon,warming_C,sprT.forecast,winT.forecast,chill.forecast,bb.sprtemp.2))
spests2warm$bbchange<-spests2warm$bb.sprtemp.2-spestsnowarm$bb.sprtemp.0
spests2warm$chillchange<-spests2warm$chill.forecast-spestsnowarm$chill.forecast

spests3warm<-spests[spests$warming_C==3,]
spests3warm<-subset(spests3warm,select=c(lat,lon,warming_C,sprT.forecast,winT.forecast,chill.forecast,bb.sprtemp.3))
spests3warm$bbchange<-spests3warm$bb.sprtemp.3-spestsnowarm$bb.sprtemp.0
spests3warm$chillchange<-spests3warm$chill.forecast-spestsnowarm$chill.forecast

spests4warm<-spests[spests$warming_C==4,]
spests4warm<-subset(spests4warm,select=c(lat,lon,warming_C,sprT.forecast,winT.forecast,chill.forecast,bb.sprtemp.4))
spests4warm$bbchange<-spests4warm$bb.sprtemp.4-spestsnowarm$bb.sprtemp.0
spests4warm$chillchange<-spests4warm$chill.forecast-spestsnowarm$chill.forecast


spests5warm<-spests[spests$warming_C==5,]
spests5warm<-subset(spests5warm,select=c(lat,lon,warming_C,sprT.forecast,winT.forecast,chill.forecast,bb.sprtemp.5))
spests5warm$bbchange<-spests5warm$bb.sprtemp.5-spestsnowarm$bb.sprtemp.0
spests5warm$chillchange<-spests5warm$chill.forecast-spestsnowarm$chill.forecast

spests6warm<-spests[spests$warming_C==6,]
spests6warm<-subset(spests6warm,select=c(lat,lon,warming_C,sprT.forecast,winT.forecast,chill.forecast,bb.sprtemp.6))
spests6warm$bbchange<-spests6warm$bb.sprtemp.6-spestsnowarm$bb.sprtemp.0
spests6warm$chillchange<-spests6warm$chill.forecast-spestsnowarm$chill.forecast

spests7warm<-spests[spests$warming_C==7,]
spests7warm<-subset(spests7warm,select=c(lat,lon,warming_C,sprT.forecast,winT.forecast,chill.forecast,bb.sprtemp.7))
spests7warm$bbchange<-spests7warm$bb.sprtemp.7-spestsnowarm$bb.sprtemp.0
spests7warm$chillchange<-spests7warm$chill.forecast-spestsnowarm$chill.forecast
colnames(spests1warm)[7]<-colnames(spests2warm)[7]<-colnames(spests3warm)[7]<-colnames(spests4warm)[7]<-colnames(spests5warm)[7]<-colnames(spests6warm)[7]<-colnames(spests7warm)[7]<-"bbdoy"
allests<-rbind(spests1warm,spests2warm,spests3warm,spests4warm,spests5warm,spests6warm,spests7warm)
allests$bbchange<-as.integer(allests$bbchange)
allests$chillchange<-round(allests$chillchange, digits=2)

spests$bbnowarm<-rep(spests$bothwarm[spests$warming==0], each=8)
warming<-unique(spests$warming)[2:8]
#Make a map with change in budburst for different levels of warming
chillchange<-unique(allests$chillchange)
myPalette <- colorRampPalette(brewer.pal(9, "Blues")) #### Gives us a heat map look
cols = myPalette(length(chillchange))
mycols <- data.frame(cbind(sort(chillchange),cols))
colnames(mycols)[1]<-"changechill"

ylim=range(allests$lat)
xlim=range(allests$lon)
quartz(height=6,width=10)
par(mfrow=c(2,7),omi=c(0,0,0,0),mai=c(0,0,0,0))
for(i in 1:length(warming)){
  warmdat<-allests[allests$warming_C==warming[i],]
  #mapDevice() #create world map shaped window
  map("world", fill=TRUE
      ,col="grey65"
      ,boundary=FALSE,interior=FALSE
      ,ylim=ylim, xlim=xlim,mar=c(0,0,0,0)
      
      ,projection='albers',par=c(1,1),wrap=T
      ,resolution=1,border="lightgrey",myborder=0)
  mtext(paste(warming[i],"deg C"),side=3, line=-7,adj=.5, cex=.8)
  points(c(warmdat$lon), c(warmdat$lat), pch=16, col=mycols$cols[match(warmdat$chillchange, mycols$changechill)], cex=1.1)
  
}
dim(mycols)
rowstokeep<-c(1,as.integer(dim(mycols)[1]/4),as.integer(dim(mycols)[1]/2),as.integer((dim(mycols)[1]/4)+(dim(mycols)[1]/2)),dim(mycols)[1])
mycolslegend<-mycols[rowstokeep,]
mycolslegend$changechill<-round(as.numeric(mycolslegend$changechill)*240,digits=0)
legend("bottom", # position
       legend = mycolslegend$changechill, 
       title = "Change in Chilling (Utah units)",
       pch = 16,
       col=mycolslegend$cols,
       cex = .9,
       bty = "n") # border

bbchange<-unique(allests$bbchange)
myPalette <- colorRampPalette(brewer.pal(9, "YlOrRd")) #### Gives us a heat map look
cols = rev(myPalette(length(bbchange)))
mycols <- data.frame(cbind(sort(bbchange),cols))
colnames(mycols)[1]<-"changebb"

for(i in 1:length(warming)){
  warmdat<-allests[allests$warming_C==warming[i],]
#mapDevice() #create world map shaped window
map("world", fill=TRUE
    ,col="grey65"
    ,boundary=FALSE,interior=FALSE
    ,ylim=ylim, xlim=xlim,mar=c(0,0,0,0)
    
    ,projection='albers',par=c(1,1),wrap=T
    ,resolution=1,border="lightgrey",myborder=0)
mtext(paste(warming[i],"deg C"),side=3, line=-7,adj=.5, cex=.8)
points(c(warmdat$lon), c(warmdat$lat), pch=16, col=mycols$cols[match(warmdat$bbchange, mycols$changebb)], cex=1.1)

}
rowstokeep<-c(1,as.integer(dim(mycols)[1]/4),as.integer(dim(mycols)[1]/2),as.integer((dim(mycols)[1]/4)+(dim(mycols)[1]/2)),dim(mycols)[1])
rowstokeep<-c(1,3,5,7,10)
mycolslegend<-mycols[rowstokeep,]

legend("bottom", # position
       legend = mycolslegend$changebb, 
       title = "Change in budburst",
       pch = 16,
       col=mycolslegend$cols,
       cex = .9,
       bty = "n") # border



#the below doesn't work!
legend.scale(range(mycols$changechill), col = cols, 
             horizontal = FALSE,
             axis.args = list(at = c(-16,1)))
