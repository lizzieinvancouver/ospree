#For experiments when multiple different percentages used yield single respvar.simple
#Goal is to select out the highest percentage of budburst only
#just days to budburst or percbb are cases when there are multiple percentages of budburst.
#This code that goes into budburst cleaning. 
#This should go before nacho's code
#Started by Ailene Ettinger
#2 Feb 2017
#start with output/ospree_clean_withchill.csv file
d<-read.csv("output/ospree_clean_withchill.csv", header=T )

multdatsets_days<-unique(d$datasetIDstudy[d$multiresp==TRUE & d$respvar.simple=="daystobudburst"])#only 3 studies for which there are multiple response variables and days to budburst
multdatsets_percbb<-unique(d$datasetIDstudy[d$multiresp==TRUE & d$respvar.simple=="percentbudburst"])#only 1 study for which there are multiple response variables and days to budburst

#Trying to figure out where the data for percbb are coming from and what they are
XX<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_percbb],d$respvar[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_percbb],d$respvar.simple[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_percbb],d$response[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_percbb],d$chilltemp[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_percbb]))
colnames(XX)<-c("fig","respvar","respvar.simple","response", "chilltemp")
XX[order(XX$fig),] 
#look at days
#Trying to figure out where the data for percbb are coming from and what they are
XX<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_days],d$figure.table..if.applicable.[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_days],d$respvar[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_days],d$respvar.simple[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_days],d$response[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_days],d$chilltemp[d$multiresp==TRUE & d$datasetIDstudy==multdatsets_days]))
colnames(XX)<-c("fig","respvar","respvar.simple","response", "chilltemp")
XX[order(XX$fig),] 
