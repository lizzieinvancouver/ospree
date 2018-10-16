#checking photoperiod
#October 15, 2018
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/git/ospree/analyses")
osp<-read.csv("input/ospree.csv", header=T)
ospbb<-read.csv("output/ospree_clean_withchill_BB.csv", header=T)
ospcl<-read.csv("output/ospree_clean.csv", header=T)
ospch<-read.csv("output/ospree_clean_withchill.csv", header=T)

#compare for devries82
osp$chilltemp[osp$datasetID=="devries82"]
ospbb$chilltemp[ospbb$datasetID=="devries82"]#doesn't exist
ospcl$chilltemp[ospcl$datasetID=="devries82"]#doesn't exist
cbind(osp$forcetemp[osp$datasetID=="devries82"],ospcl$forcetemp[ospcl$datasetID=="devries82"])
#mistakes in forcetemp for figures 2 and 3 (irradiance and forcetemp are switched)
cbind(osp$photoperiod_day[osp$datasetID=="devries82"],ospcl$photoperiod_day[ospcl$datasetID=="devries82"])
cbind(osp$figure.table..if.applicable.[osp$datasetID=="devries82"],ospcl$figure.table..if.applicable.[ospcl$datasetID=="devries82"])


#compare for hawerroth13
osp$chilltemp[osp$datasetID=="hawerroth13"]
ospbb$chilltemp[ospbb$datasetID=="hawerroth13"]#doesn't exist
ospcl$chilltemp[ospcl$datasetID=="hawerroth13"]#doesn't exist
cbind(osp$forcetemp[osp$datasetID=="hawerroth13"],ospcl$forcetemp[ospcl$datasetID=="hawerroth13"])
cbind(osp$photoperiod_day[osp$datasetID=="hawerroth13"],ospcl$photoperiod_day[ospcl$datasetID=="hawerroth13"])
cbind(osp$figure.table..if.applicable.[osp$datasetID=="hawerroth13"],ospcl$figure.table..if.applicable.[ospcl$datasetID=="hawerroth13"])
#no table/figure listed. Add?
cbind(osp$fieldchill[osp$datasetID=="hawerroth13"],ospch$fieldchill[ospch$datasetID=="hawerroth13"])
ospch$Field_Chilling_Hours[ospch$datasetID=="hawerroth13"]
