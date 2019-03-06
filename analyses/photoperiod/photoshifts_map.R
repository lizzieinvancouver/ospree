#Make a map showing shifts in space and time required to experience photoperiod shifts in OSPREE treatments
options(stringsAsFactors = FALSE)
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/photoperiod") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses/photoperiod") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/photoperiod")
} else 
  setwd("~/Documents/git/ospree/analyses/photoperiod")
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
library(dplyr)
library(plyr)
ospree<-read.csv("../../analyses/output/ospree_clean.csv",header=T)#which version should I use?
#Now try to make this table into a map figure
source("shifts_table.R")

photop_all$space2<-as.numeric(photop_all$space)
photop_all$time2<-as.numeric(photop_all$time)
spacerER<-photop_all[photop_all$space=="ER",]
spacerER$space2[spacerER$space=="ER"]<-90-spacerER$lat
timeER<-photop_all[photop_all$time=="ER"|photop_all$time=="min NA (9)"|photop_all$time=="max NA (18.7)",]

#nacho says that we can use a different background map that looks a little better 
#(i.e. without having entire countries cut off)
mapDevice() #create world map shaped window
map("world", fill=TRUE
    ,col="grey65"
    ,boundary=F,interior=F
    ,ylim=c(38, 80), xlim=c(-135,25),mar=c(0,0,0,0)
    ,projection='albers',par=c(0,0),wrap=T
    ,resolution=1,border="lightgrey",myborder=0)

points(c(photop_all$long), c(photop_all$lat), pch=21, bg="salmon4", cex=abs(.03*photop_all$time2))

points(c(timeER$long), c(timeER$lat), pch=8,cex=1.8, col="salmon4", lwd=2.5)

for (i in c(1:nrow(photop_all))){
  inter2 <- gcIntermediate(c(photop_all$long[i], photop_all$lat[i]),
                           c(photop_all$long[i], photop_all$lat[i]+photop_all$space2[i]), n=50, addStartEnd=TRUE)
  lines(inter2, col="darkblue",lwd=2)
}
#Make stars for points that  are "ER"
for (i in c(1:nrow(spacerER))){
  inter3 <- gcIntermediate(c(spacerER$long[i], spacerER$lat[i]),
                           c(spacerER$long[i], spacerER$lat[i]+spacerER$space2[i]), n=50, addStartEnd=TRUE)
  arrows(inter3[1,1],min(inter3[,2]),inter3[1,1],max(inter3[,2]),length=.10,code=2, angle=45,col="darkblue",lwd=2)
}
#how do i add a legend to the map?
legend(-140,15, # position
       legend = c("5 days earlier","55 days earlier","105 days earlier","Exceeds range possible with temporal shift","Equivalent spatial shift"), 
       title = "Equivalent shift with climate change",
       pch = c(21,21,21,8),
       pt.cex=c(abs(.03*c(-5,-55,-105)),1.1,NA),
       pt.bg="salmon4",
       col=c("black","black","black","salmon4","darkblue"),
       lty=c(NA,NA,NA,NA,1),
       lwd=c(1,1,1,2.5,2),
       cex = .9,
       bty = "n") # border
mtext("A)", side=3, adj=0)

