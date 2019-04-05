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

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

#read in file with species, lat/long, and bbdoy forecast (from pep data)
spests<-read.csv("betpen.forecast.csv", header=TRUE)
#sort dataset based on lat
spests <- spests[order(spests$lat, spests$warming),]
#calculate difference in bb between no warming and warmed for each row
spests$sprwarm<-as.integer(spests$sprwarm)
spests$winwarm<-as.integer(spests$winwarm)
spests$bothwarm<-as.integer(spests$bothwarm)
spests$bbnowarm<-rep(spests$bothwarm[spests$warming==0], each=8)
warming<-unique(spests$warming)[2:8]
#Make a map with change in budburst for different levels of warming
spests$bbchange<-spests$bothwarm-spests$bbnowarm
bbch<-unique(spests$bbchange)
range(spests$lat)
range(spests$lon)
ylim=c(46.95, 47.1)
xlim=c(13.5,14)
myPalette <- colorRampPalette(brewer.pal(9, "YlOrRd")) #### Gives us a heat map look
cols = rev(myPalette(length(bbchange)))
mycols <- data.frame(cbind(sort(bbch),cols))
colnames(mycols)[1]<-"changebb"
quartz(height=4,width=10)
par(mfrow=c(1,7),omi=c(0,0,0,0),mai=c(0,0,0,0))
for(i in 1:length(warming)){
  warmdat<-spests[spests$warming==warming[i],]
#mapDevice() #create world map shaped window
map("world", fill=TRUE
    ,col="grey65"
    ,boundary=FALSE,interior=FALSE
    ,ylim=ylim, xlim=xlim,mar=c(0,0,0,0)
    
    ,projection='albers',par=c(1,1),wrap=T
    ,resolution=1,border="lightgrey",myborder=0)
mtext(paste(warming[i],"deg C"),side=3, line=-5,adj=.5)
points(c(warmdat$lon), c(warmdat$lat), pch=16, col=mycols$cols[match(warmdat$bbchange, mycols$changebb)], cex=1.1)

}
legend("bottom", # position
       legend = mycols$changebb, 
       title = "Change in budburst",
       pch = 16,
       col=mycols$cols,
       cex = .9,
       bty = "n") # border
