
##############################################################################################################
# Script and functions to:
#' * Extract GDD from last day of chilling until day of BB - potential response variable  
#'
#'  Ospree
#'  started 21 July 2017
#'  
#'  The file is unfinished!! Still needs to extract correctly the BB date and communicate 
#'  chill data with pmp data
##############################################################################################################


## to start
rm(list=ls())
options(stringsAsFactors=FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")


## read in last day of chilling
chill.day <- read.csv("output/daily_expchill.csv")
chill.unique.exptreat<-unique(chill.day$ID_exptreat2)
#head(chill.day)

## read in data for pmp containing climate each day each site
load("output/fieldclimate_pmp.RData")
pmp.data <- tempval
rm(tempval)
studiesnames<-names(pmp.data)

GDD.each.study<-rep(NA,length(chill.unique.exptreat))
for(i in 1:length(chill.unique.exptreat)){#i=1
  
  ## define target study
  exp.treat.i<-chill.unique.exptreat[i]
  study.i<-unique(chill.day$datasetID[which(chill.day$ID_exptreat2==exp.treat.i)])
  last.chill.study.i<-unique(chill.day[chill.day$datasetID==study.i,"lastchilldate"])
  
  ## NOTE: the number of unique treatments and unique last chill days differ -- check if makes sense
  
  ## define which elements in pmp.data correspond to target study
  pmp.elements<-which(grepl(study.i,studiesnames))
  treatment.elements<-unique(subset(chill.day,datasetID==study.i)$ID_exptreat2)
  
  if(length(pmp.elemnts)>0)
  
  ## loop across elements and compute GDDs between end-chilling and BB
    gdds.each<-list()
    for(j in 1:length(pmp.elements)){#j=2
      element.j<-pmp.data[[pmp.elements[j]]]
      start.date<-as.Date(last.chill.study.i[j],format="%Y-%m-%d")
      
      ##NOTE: need to get bb date from dataset below, just an example to work the code
      end.date<-as.Date("1983-11-25",format="%Y-%m-%d")
      period<-seq(start.date,end.date,1)
      Temps.avg<-rowMeans(element.j[which(as.Date(element.j$Date,format="%Y-%m-%d")%in%(period-100)),4:5])
      gdds.each[[j]]<-sum(Temps.avg>10)
    }
  
  gdds<-unlist(gdds.each)
  GDD.each.study[[i]]<-gdds[which(!is.na(gdds))]
  
}


## this object is a list that should contain the gdd for each study. Do we append to d?
GDD.each.study



