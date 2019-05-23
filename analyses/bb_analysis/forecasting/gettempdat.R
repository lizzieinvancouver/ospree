#read in climate files and generate a new file that has, for each lat/long:
#annual values for spring temperature and winter temp
#For now just read in "temp_forforecast files"
setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")

numsites<-length(list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="temp_forforecast__"))
tempfiles<-list.files(path="../output/dailyclim/betpen",pattern="temp_forforecast__")
chillfiles<-list.files(path=paste("../output/dailyclim/",sp[s],sep=""),pattern="chill_observed_")
dir<-"../output/dailyclim/betpen"
alltemps<-c()
for(i in 1:length(tempfiles)){
tempall<-read.csv(paste(dir,"/",tempfiles[i],sep=""), header=TRUE)
mnsprtemp <- aggregate(tempall$Tmean[tempall$Month>2 & tempall$Month<5], by=list(tempall$Year[tempall$Month>2 & tempall$Month<5]), mean)
misprtemp <- aggregate(tempall$Tmean[tempall$Month>2 & tempall$Month<5], by=list(tempall$Year[tempall$Month>2 & tempall$Month<5]), min)
mxsprtemp <- aggregate(tempall$Tmean[tempall$Month>2 & tempall$Month<5], by=list(tempall$Year[tempall$Month>2 & tempall$Month<5]), max)
mnwintemp <- aggregate(tempall$Tmean[tempall$Month>9 | tempall$Month<3], by=list(tempall$ChillYear[tempall$Month>9 | tempall$Month<3]), mean)
miwintemp <- aggregate(tempall$Tmean[tempall$Month>9 | tempall$Month<3], by=list(tempall$ChillYear[tempall$Month>9 | tempall$Month<3]), min)
mxwintemp <- aggregate(tempall$Tmean[tempall$Month>9 | tempall$Month<3], by=list(tempall$ChillYear[tempall$Month>9 | tempall$Month<3]), max)
#extract the lat/long from the file name
lat<-as.numeric(strsplit(substr(tempfiles[i],19,nchar(tempfiles[i])-14),"_")[[1]][1])
long<-as.numeric(strsplit(substr(tempfiles[i],19,nchar(tempfiles[i])-14),"_")[[1]][2])
temps<-cbind(mnsprtemp,misprtemp$x,mxsprtemp$x,mnwintemp$x[-1],miwintemp$x[-1],mxwintemp$x[-1])
temps$lat<-lat
temps$long<-long
alltemps<-rbind(alltemps,temps)
}
colnames(alltemps)[1:7]<-c("year","mnsprt","misprt","mxsprt","mnwint","miwint","mxwint")
write.csv(alltemps,"../output/tempsumsforplotting.csv")
