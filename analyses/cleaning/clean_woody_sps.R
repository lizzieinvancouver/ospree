###Script.checking.woody.sps.R
##'
##'1/22/2017
##'Ignacio Morales-Castilla
##'


## to start
rm(list=ls())
options(stringsAsFactors = FALSE)


## read in file
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/input")
setwd("~ospree/analyses/input/")
dat<-read.csv("ospree.csv")
head(dat)

## set output folder
out.folder<-"~ospree/analyses/output/"

## fill blanks with NAs
#dat[dat==""]<-NA

## look at the structure of variables within
#str(dat)

## summarize unique species
sps<-sort(unique(paste(dat$genus,dat$species,sep="_")))

## summarize unique genus
genus<-sort(unique(dat$genus))

## genus is non-woody (in ospree)
genus<-cbind(genus,non.woody=rep("no",length(genus)))
genus[c(10,13,22,23,40,41,42,43,46),2]<-"yes"
is.non.woody<-genus[which(genus[,2]=="yes"),1]

## remove non-woody species and save
dat.woody<-dat[-which(dat$genus%in%is.non.woody),]
write.csv(dat.woody,paste(out.folder,"ospree.woody.only.csv",sep=""))

## alternatively, correct the "woody" column within ospree
#dat$woody[which(dat$genus%in%is.non.woody)]<-"no"
#write.csv(dat,paste(out.folder,"ospree.woody.corrected.csv",sep=""))



