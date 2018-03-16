#Make a map showing shifts in space and time required to experience photoperiod shifts in OSPREE treatments
options(stringsAsFactors = FALSE)
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/photoperiod") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses/photoperiod") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/photoperiod")
} else 
  setwd("~/Documents/git/ospree/analyses/photoperiod")

ospree<-read.csv("../../analyses/output/ospree_clean.csv",header=T)#which version should I use?
source("shifts_table.R")
#Now try to make this table into a map figure
library(maptools)
library(mapdata)
library(geosphere)
library(mapproj)
library(igraph)
library(ggmap)
library(ggplot2)
library(rworldmap)
library(maps)
library(mapdata)
library(marmap)

photop_all$space2<-as.numeric(photop_all$space)
photop_all$time2<-as.numeric(photop_all$time)
spacerER<-photop_all[photop_all$space=="ER",]
spacerER$space2[spacerER$space=="ER"]<-90-spacerER$lat
timeER<-photop_all[photop_all$time=="ER"|photop_all$time=="min NA (9)"|photop_all$time=="min NA (6)",]

#nacho says that we can use a different background map that looks a little better 
#(i.e. without having entire countries cut off)
mapDevice() #create world map shaped window
map("world", fill=TRUE
    ,col="grey65"
    ,boundary=F,interior=F
    ,ylim=c(35, 80), xlim=c(-130,25),mar=c(0,0,0,0)
    ,projection='albers',par=c(0,0),wrap=T
    ,resolution=1,border="lightgrey",myborder=0)

points(c(photop_all$long), c(photop_all$lat), pch=21, bg="salmon4", cex=abs(.025*photop_all$time2))

points(c(timeER$long), c(timeER$lat), pch=8,cex=1.6)

for (i in c(1:nrow(photop_all))){
  inter2 <- gcIntermediate(c(photop_all$long[i], photop_all$lat[i]),
                           c(photop_all$long[i], photop_all$lat[i]+photop_all$space2[i]), n=50, addStartEnd=TRUE)
  lines(inter2, col="black",lwd=1.5)
}
#Make squares for points that  are "ER"
for (i in c(1:nrow(spacerER))){
  inter3 <- gcIntermediate(c(spacerER$long[i], spacerER$lat[i]),
                           c(spacerER$long[i], spacerER$lat[i]+spacerER$space2[i]), n=50, addStartEnd=TRUE)
  arrows(inter3[1,1],min(inter3[,2]),inter3[1,1],max(inter3[,2]),length=.10,code=2, angle=45,col="black",lwd=1.5)
}
#how do i add a legend to the map?
