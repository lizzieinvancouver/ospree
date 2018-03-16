#Try making a 3d plot of chilling,forcing and photoperiod
options(stringsAsFactors = FALSE)
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/docs/photoperiod") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/docs/photoperiod") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/docs/photoperiod")
} else 
  setwd("~/Documents/git/ospree/docs/photoperiod")

library(plot3D)
osp<-read.csv("../../analyses/output/ospree_clean_withchill.csv",header=T)
# x, y and z coordinates
x <- as.numeric(osp$Total_Utah_Model)
y<- as.numeric(osp$forcetemp)
z <- as.numeric(osp$photoperiod_day)
scatter3D(x, y, z, colvar = NULL, col = "blue", xlab="chilling", ylab="forcing",
          zlab="daylength",ticktype = "detailed",pch = 19, cex = 0.5, cex.lab=.8, cex.axis=.8)
unique(osp$forcetemp)
