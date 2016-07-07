###Exploratory plots of studies and species to decide how we want to include 
##study and species in our stan model for percent budburst
rm(list=ls()) 
options(stringsAsFactors=FALSE)
setwd("~/GitHub/ospree")
d <- read.csv("ospree_clean.csv")
head(d)
d$genus.species<-paste(d$genus,d$species,sep=".")
d_percbb<-d[d$respvar=="percentbudburst"|d$respvar=="percentstage01"|d$respvar=="percentstage02"|d$respvar=="percentstage03"|d$respvar=="percentstage04",]#4923 rows 
d_percbb[which(d_percbb$response==max(d_percbb$response)),]
d_percbb2<-subset(d_percbb,as.numeric(d_percbb$response)<800)#remove the weird value for perc budburst
boxplot(as.numeric(d_percbb2$response)~d_percbb2$genus.species)
tapply(d_percbb2$genus.species,d_percbb2$datasetID,length)#42 studies included
sp.study.tab<-tapply(as.numeric(d_percbb2$response),list(d_percbb2$genus.species,d_percbb2$datasetID),length)#43 studies
sp.study.tab[is.na(sp.study.tab)] <- 0
sp.study.tab[which(sp.study.tab>0)] <- 1
numstudy<-rowSums(sp.study.tab)
length(numstudy)
numstudy[numstudy>1]
dim(d_percbb2)
