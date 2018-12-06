## Script to format Flynn's data and merge into Ospree
# Started by Nacho
# Date: 6th Dec 2018


## to start
rm(list=ls())
options(stringsAsFactors=FALSE)



## setwd
setwd("~/GitHub/ospree/")

## load data
bdaymean.hf<-read.csv("analyses/phylogeny/input/flynn/bdaymean.hf.csv")
bdaymean.sh<-read.csv("analyses/phylogeny/input/flynn/bdaymean.sh.csv")
spsnames<-read.csv("analyses/phylogeny/input/flynn/spcode.csv")
## info from README
#C: force - 15 C day/5 C night
#W: force - 20 C day/10 C night
#S: photo - 8 hrs
#L: photo - 12 hrs
#0: chill - field chill only -- 814.50 for HF; 599.50 for SH
#1: chill - field chill + weeks at 4 C -- 2062.50 for HF; 1847.50 for SH 
#2: chill - field chill + weeks at 1.5 C -- 1702.50 for HF; 1487.50 SH 

## set treatments
treats<-data.frame(forcetemp=c(rep(15,6),rep(20,6)),
                   forcetemp_night=c(rep(5,6),rep(10,6)),
                   photoperiod_day=c(rep(12,3),rep(8,3),rep(12,3),rep(8,3)),
                   chill=c(rep(c(814.5,2062.5,1702.5),4)))

## adding data to bb.all (output from bbstanleadin.phyla)
## first source ospree data to attach
bb.flynn<-bb.all
bb.flynn[,]<-NA
str(bb.flynn)

## fill data.frame
uniquesps<-unique(bdaymean.hf$X)
namesflynn<-expand.grid(1:12,spsnames$species,
                        c("Flynn.hf","Flynn.sh"),
                        stringsAsFactors = F)
bb.flynn=bb.flynn[1:nrow(namesflynn),]
bb.flynn$datasetID=namesflynn$Var3
bb.flynn$genus=unlist(lapply(strsplit(namesflynn$Var2," ")
                             ,function(x)x[1]))
bb.flynn$species=unlist(lapply(strsplit(namesflynn$Var2," ")
                               ,function(x)x[2]))
bb.flynn$woody="yes"

for(i in 1:28){#i=1
  print(i)
  binom.i<-spsnames$species[i]
  genus.i<-strsplit(binom.i," ")[[1]][1]
  sps.i<-strsplit(binom.i," ")[[1]][2]
  
  ## hf
  bb.flynn[which(bb.flynn$datasetID=="Flynn.hf" &
                 bb.flynn$genus==genus.i & 
                 bb.flynn$species==sps.i),
           "response"]=t(bdaymean.hf[i,2:13])
  
  bb.flynn[which(bb.flynn$datasetID=="Flynn.hf" &
                   bb.flynn$genus==genus.i & 
                   bb.flynn$species==sps.i),
                   c("forcetemp","forcetemp_night","photoperiod_day","chill")]=treats
  
  ## sh
  bb.flynn[which(bb.flynn$datasetID=="Flynn.sh" &
                   bb.flynn$genus==genus.i & 
                   bb.flynn$species==sps.i),
           "response"]=t(bdaymean.hf[i,2:13])
  
  bb.flynn[which(bb.flynn$datasetID=="Flynn.sh" &
                   bb.flynn$genus==genus.i & 
                   bb.flynn$species==sps.i),
           c("forcetemp","forcetemp_night","photoperiod_day","chill")]=treats
  
  
}

bb.flynn$force=bb.flynn$forcetemp
bb.flynn$photo=bb.flynn$photoperiod_day










