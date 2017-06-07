#For experiments when multiple different percentages used yield single respvar.simple
#Goal is to select out the highest percentage of budburst only, and remove sites that contain duplicate data in two forms
#(i.e. days to budburst and percbb). these are cases when there are multiple percentages of budburst.
#Started by Ailene Ettinger
#2 Feb 2017 ## 
## continued by Nacho on 21st April, celebrating Earth's Day! (Go Earth! Don't let us humans keep you down.)
# Following Lizzie's recs @ issue 81 in Git
#(1) select daystobudburst if given (aka if daystobudburst is an option for the study, select that one)
#(2) select highest percent if no daystobudburst
#(3) check that n across different response types is not grossly different (aka >25 off)

#start with output/ospree_clean_withchill.csv file
#d<-read.csv("output/ospree_clean_withchill.csv", header=TRUE)

multdatsets_days<-unique(d$datasetIDstudy[d$multiresp==TRUE & d$respvar.simple=="daystobudburst"])#only 3 studies for which there are multiple response variables and days to budburst
multdatsets_percbb<-unique(d$datasetIDstudy[d$multiresp==TRUE & d$respvar.simple=="percentbudburst"])#only 1 study for which there are multiple response variables and percent to budburst
#multdatsets_tt<-unique(d$datasetIDstudy[d$multiresp==TRUE & d$respvar.simple=="thermaltime"])#should add studies with thermal time too?

#Trying to figure out where the data for percbb are coming from and what they are
# we generate a subset XX of data for studies with multiple response variables and percent to budburst

XX<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$respvar[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$respvar.simple[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$response[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$chilltemp[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$datasetIDstudy[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        seq(1,nrow(d),1)[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))
colnames(XX)<-c("fig","respvar","respvar.simple","response", "chilltemp","study","row.ID")
XX[order(XX$fig),] 

## select highest percent if no daystobudburst
resps<-unique(XX$respvar)
n<-length(resps)
higher.response<-list()
for(i in 1:n){
  higher.response[[i]]<-max(as.numeric(subset(XX,respvar==resps[i])$response)) ## something is off here, maximum is 100.22%, is that even possible? 
  names(higher.response[[i]])<-paste(resps[i],"maximum")
}

## this would identify which values to remove (not within 25 days of maximum)
off.vals.to.remove<-list()
for(i in 1:n){
  response.subset<-subset(XX,respvar==resps[i])
  off.vals.to.remove[[i]]<-as.numeric(response.subset$row.ID[which(abs(as.numeric(response.subset$response)-higher.response[[i]])>25)])
}

## this would remove identified rows from global dataset: Check before activating!!

#to.remove<-c(unique(unlist(off.vals.to.remove)))
#d<-d[-to.remove,]


#look at days and repeat the above procedure
#Trying to figure out where the data for percbb are coming from and what they are
YY<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$respvar[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$respvar.simple[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$response[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$chilltemp[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$datasetIDstudy[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        seq(1,nrow(d),1)[d$multiresp==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))
colnames(YY)<-c("fig","respvar","respvar.simple","response", "chilltemp","study","row.ID")
YY[order(YY$fig),] 


## select daystobudburst if present (looped across each of the studies)
study.i<-unique(YY$study)
for(j in study.i){ #j=study.i[2]
YY.j<-subset(YY,study==j)
no.daysBB<-as.numeric(YY.j[which(YY.j$respvar.simple!="daystobudburst"),"row.ID"]) 
YYsubset<-subset(YY.j,respvar.simple=="daystobudburst")
YYsubset<-YYsubset[which(YYsubset$response!=""),] ## ignore rows with empty values
resps<-unique(YYsubset$respvar)
n<-length(resps)
higher.response<-list()
for(i in 1:n){
  higher.response[[i]]<-max(as.numeric(subset(YYsubset,respvar==resps[i])$response),na.rm=T) ## something is off here, maximum is 100.22%, is that even possible? 
  names(higher.response[[i]])<-paste(resps[i],"maximum")
}

## this would identify which values to remove (not within 0.25 of maximum days to budburst)
off.vals.to.remove<-list()
for(i in 1:n){
  if(!is.na(higher.response[[i]])){
    response.subset<-subset(YYsubset,respvar==resps[i])
    off.vals.to.remove[[i]]<-as.numeric(response.subset$row.ID[which(abs(as.numeric(response.subset$response)-higher.response[[i]])>0.25)])
  } else {
    off.vals.to.remove[[i]]<-YYsubset[which(YYsubset$respvar==resps[i]),"row.ID"]
  }
}

## this would remove identified rows from global dataset: Check before activating!!

#to.remove<-c(unique(unlist(off.vals.to.remove),no.daysBB))
#d<-d[-to.remove,]

}

