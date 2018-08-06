# Look for non-linearity in forcing or chilling responses (Issue #190)
# Started by Ailene ETtinger
# 2018 August
# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

#ospree <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE, na.strings = c("", "NA"))
#xx<-ospree
source("source/bbstanleadin.R")
xx<-d
xx[is.na(xx)] <- 0
xx <- within(xx, { force <- ifelse(forcetemp!=0, ave(forcetemp, datasetID, species, 
                                                     study, FUN=function(x) length(unique(x))), 0)}) # mult forcetemp
xx <- within(xx, { chill <- ifelse(chilltemp!=0, ave(chilltemp, datasetID, species, 
                                                     study, FUN=function(x) length(unique(x))), 0)}) # mult studychill
#xx <- within(xx, { photo <- ifelse(photoperiod_day!=0, ave(photoperiod_day, datasetID, species, 
#                                                           study, FUN=function(x) length(unique(x))), 0)}) # mult photoperiod_day

d.sub<-xx%>%dplyr::select(datasetID, species, study,force, chill,Total_Chill_portions,forcetemp,photoperiod_day,respvar.simple,response.time)
d.sub<-d.sub[d.sub$respvar.simple=="daystobudburst",]
d.sub.chill<-subset(d.sub, chill>2)#3 or more chilling treatments
d.sub.force<-subset(d.sub, force>2)#3 or more forcing treatments
dim(d.sub.chill)#323  10
unique(d.sub.chill$datasetID)#3 studies with >2 chilling treatments
dim(d.sub.force)#666   10
unique(d.sub.force$datasetID)##12 studies with >2 forcing treatments

study<-unique(d.sub.force$datasetID)#12 studies with >2 forcing treatments.
#now, plot days versus forcing for each study...
d.sub.force<-d.sub.force[d.sub.force$response.time<900,]#remove some extreme values
for(i in 1:length(study)){
  st<-d.sub.force[d.sub.force==study[i],]
  numch<-length(unique(st$chill))
  ch<-unique(st$chill)
  ph<-unique(st$photoperiod_day)
  #open plotting window with dimensions specificed by number of chilling and photoperiod treatments
  quartz()
  par(mfrow=c(length(ch),length(ph)))
  for(j in 1:length(ch)){
    st1<-st[st$chill==ch[j],]
    for(k in 1:length(ph)){
      st2<-st1[st1$photoperiod_day==ph[k],]
      boxplot(as.numeric(st2$response.time)~as.factor(st2$force), xlab="Forcing temp",ylab="Days to bb",main=paste("Chill=",ch[j],"DL=",ph[k]))
    }
  }
  mtext(paste(study[i]), side=3, adj=-1,line=3)
}
