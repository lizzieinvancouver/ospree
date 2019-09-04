## Started June 2019 
## By Ailene, 
## Table of all species and which studies contain them for supplement

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyr)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

## set up the flags to get the most species and studies included
use.chillports = FALSE
use.zscore = TRUE
use.allspp = TRUE
use.multcuespp = FALSE
use.cropspp = TRUE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE


## name your figures paths (based on flags above) ... this needs work
##Ailene updated to match models_stan file
# chill ports centered, with only expramptypes, with crops, not all sp
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE  & 
    use.chillports==FALSE){
    tablepath <- "allsppwcrops_exprampfp"
}

## functions for plotting 
source("source/bbstanleadin.R")
#get sp names
bb.stan$spname<-paste(bb.stan$genus,bb.stan$species,sep=".")
spnames<-sort(unique(bb.stan$spname))


#get studies that go with the names
bb.stan$ID_sp<-paste(bb.stan$datasetID,bb.stan$spname)
sp.st <- bb.stan %>% # start with the data frame
  distinct(ID_sp, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(datasetID, spname)
sp.st$study<-"study"
studynum<-rowSums(table(sp.st$spname,sp.st$datasetID))
spstudynum<-c()  
for (i in 1:length(spnames)){
    numstud<-studynum[i]
    listnumstud<-seq(1,numstud,by=1)
    spstudynum<-c(spstudynum,listnumstud)
  }
 
sp.st$numstud<-spstudynum
sp.st$newcols<-paste(sp.st$study,sp.st$numstud,sep=".")
sp.st2<-subset(sp.st,select=c(spname,datasetID,newcols))
sp.st.wide <- spread(sp.st2,newcols,datasetID, fill="")
sp.st.wide$stnum<-studynum
sp.st.wide$studies<-paste(sp.st.wide$study.1,sp.st.wide$study.2,sp.st.wide$study.3,sp.st.wide$study.4,sp.st.wide$study.5,sp.st.wide$study.6,sp.st.wide$study.7,sp.st.wide$study.8,sp.st.wide$study.8,sp.st.wide$study.10,sep=", ")

sp.st.wide$studies<-gsub(", , , , , , , , , ","", sp.st.wide$studies)
sp.st.wide$studies<-gsub(", , , , , , , , ","", sp.st.wide$studies)
sp.st.wide$studies<-gsub(", , , , , , , ","", sp.st.wide$studies)
sp.st.wide$studies<-gsub(", , , , , , ","", sp.st.wide$studies)
sp.st.wide$studies<-gsub(", , , , , ","", sp.st.wide$studies)
sp.st.wide$studies<-gsub(", , , , ","", sp.st.wide$studies)
#Fix some that have studies listed twice
sp.st.wide$studies[which(sp.st.wide$spname=="Picea.abies")]<-"basler12, basler14, gomory15, laube14a, laube14b, partanen01, partanen98, worrall67"
#strsplit(sp.st.wide$spname,".", fixed=TRUE))
sp.st.wide$studies[which(sp.st.wide$spname=="Fagus.sylvatica")]<-"falusi03, falusi90, falusi96, falusi97, basler12, basler14, caffarra11a, heide93a, zohner16"
sp.st.wide$studies[which(sp.st.wide$spname=="Betula.pendula")]<-"heide93, li05, rinne97, basler12, laube14a, laube14b, linkosalo06, myking95, skuterud94"

sp.st.table<-subset(sp.st.wide,select=c(spname,stnum,studies))
write.csv(sp.st.table,"../../analyses/output/supptables/speciestable.csv", row.names = FALSE)
