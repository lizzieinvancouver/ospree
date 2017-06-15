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
#dim(d)

target.multiresp<-c("multi_respvar","multi_respvar.simple")
i=1
target.multiresp<-target.multiresp[i]


# just for your records: if we include both multiresp and multibothresp, there are 0 studies meeting that condition so we do not need this
#if(target.multiresp=="all"){
# both multiresp and multibothresp
#multdatsets_days<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$multi_respvar.simple==TRUE & d$respvar.simple=="daystobudburst"]) # flags 0 studies
#multdatsets_percbb<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$multi_respvar.simple==TRUE & d$respvar.simple=="percentbudburst"]) # flags 0 studies
#multdatsets_tt<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$multi_respvar.simple==TRUE & d$respvar.simple=="thermaltime"]) # flags 0 studies
#}

if(target.multiresp=="multi_respvar"){
# only multiresp
multdatsets_days<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$respvar.simple=="daystobudburst"])# 26 studies for which there are multiple response variables and days to budburst
multdatsets_percbb<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$respvar.simple=="percentbudburst"])# 28 study for which there are multiple response variables and percent to budburst
multdatsets_tt<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$respvar.simple=="thermaltime"])# 6 studies should add studies with thermal time too?
}

if(target.multiresp=="multi_respvar.simple"){
# only multibothresp
multdatsets_days<-unique(d$datasetIDstudy[d$multi_respvar.simple==TRUE & d$respvar.simple=="daystobudburst"])# 26 studies for which there are multiple response variables and days to budburst
multdatsets_percbb<-unique(d$datasetIDstudy[d$multi_respvar.simple==TRUE & d$respvar.simple=="percentbudburst"])# 28 study for which there are multiple response variables and percent to budburst
multdatsets_tt<-unique(d$datasetIDstudy[d$multi_respvar.simple==TRUE & d$respvar.simple=="thermaltime"])#6 studies flagged; should add studies with thermal time too?
}

# replacing factors for character (not needed but just in case)
multdatsets_days<-as.character(multdatsets_days)
multdatsets_percbb<-as.character(multdatsets_percbb)
multdatsets_tt<-as.character(multdatsets_tt)


#Trying to figure out where the data for percbb are coming from and what they are
# we generate a subset XX of data for studies with multiple response variables and percent to budburst

if(target.multiresp=="multi_respvar"){
XX<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$respvar[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$respvar.simple[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$response[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$chilltemp[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$datasetIDstudy[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        seq(1,nrow(d),1)[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))
}

if(target.multiresp=="multi_respvar.simple"){
XX<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$respvar[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$respvar.simple[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$response[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$chilltemp[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        d$datasetIDstudy[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                        seq(1,nrow(d),1)[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))
}

colnames(XX)<-c("fig","respvar","respvar.simple","response", "chilltemp","study","row.ID")
#XX[order(XX$fig),] 

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
to.remove<-c(unique(unlist(off.vals.to.remove)))
if(length(to.remove)>0){
d<-d[-to.remove,]
#dim(d)
}

#look at days and repeat the above procedure
#Trying to figure out where the data for percbb are coming from and what they are
if(target.multiresp=="multi_respvar"){
YY<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$respvar[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$respvar.simple[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$response[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$chilltemp[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        d$datasetIDstudy[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                        seq(1,nrow(d),1)[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))
}

if(target.multiresp=="multi_respvar.simple"){
  YY<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$respvar[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$respvar.simple[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$response[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$chilltemp[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$datasetIDstudy[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          seq(1,nrow(d),1)[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))
}

colnames(YY)<-c("fig","respvar","respvar.simple","response", "chilltemp","study","row.ID")
#YY[order(YY$fig),] 


## select daystobudburst if present (looped across each of the studies)
study.i<-unique(YY$study)
for(j in study.i){ #j=study.i[1]
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
  if(!is.na(higher.response[[i]]) & is.finite(higher.response[[i]])){
    response.subset<-subset(YYsubset,respvar==resps[i])
    off.vals.to.remove[[i]]<-as.numeric(response.subset$row.ID[which(abs(as.numeric(response.subset$response)-higher.response[[i]])>0.25)])
  } else {
    off.vals.to.remove[[i]]<-as.numeric(YYsubset[which(YYsubset$respvar==resps[i]),"row.ID"])
  }
}

## this would remove identified rows from global dataset: Check before activating!!

to.remove<-c(unique(unlist(off.vals.to.remove),no.daysBB))
if(length(to.remove)>0){
  d<-d[-to.remove,]
}

}

#dim(d)


# repeat the above procedure for thermal time
#Trying to figure out where the data for percbb are coming from and what they are
if(target.multiresp=="multi_respvar"){
  ZZ<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$respvar[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$respvar.simple[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$response[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$chilltemp[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$datasetIDstudy[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          seq(1,nrow(d),1)[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))
}

if(target.multiresp=="multi_respvar.simple"){
  ZZ<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$respvar[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$respvar.simple[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$response[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$chilltemp[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          d$datasetIDstudy[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_tt],
                          seq(1,nrow(d),1)[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))
}

colnames(ZZ)<-c("fig","respvar","respvar.simple","response", "chilltemp","study","row.ID")
#ZZ[order(ZZ$fig),] 


## select daystobudburst if present (looped across each of the studies)
study.i<-unique(ZZ$study)
for(j in study.i){ #j=study.i[1]
  print(j)
  ZZ.j<-subset(ZZ,study==j)
  no.daysBB<-as.numeric(ZZ.j[which(ZZ.j$respvar.simple!="daystobudburst"),"row.ID"]) 
  ZZsubset<-subset(ZZ.j,respvar.simple=="daystobudburst")
  ZZsubset<-ZZsubset[which(ZZsubset$response!=""),] ## ignore rows with empty values
  resps<-unique(ZZsubset$respvar)
  n<-length(resps)
  
  if(n>0 & nrow(ZZsubset)>0){
  higher.response<-list()
  for(i in 1:n){
    higher.response[[i]]<-max(as.numeric(subset(ZZsubset,respvar==resps[i])$response),na.rm=T) ## something is off here, maximum is 100.22%, is that even possible? 
    names(higher.response[[i]])<-paste(resps[i],"maximum")
  }
  
  
  ## this would identify which values to remove (not within 0.25 of maximum days to budburst)
  off.vals.to.remove<-list()
  for(i in 1:n){
    if(!is.na(higher.response[[i]]) & is.finite(higher.response[[i]])){
      response.subset<-subset(ZZsubset,respvar==resps[i])
      off.vals.to.remove[[i]]<-as.numeric(response.subset$row.ID[which(abs(as.numeric(response.subset$response)-higher.response[[i]])>0.25)])
    } else {
      off.vals.to.remove[[i]]<-as.numeric(ZZsubset[which(ZZsubset$respvar==resps[i]),"row.ID"])
    }
  }
  
  ## this would remove identified rows from global dataset: Check before activating!!
  
  to.remove<-c(unique(unlist(off.vals.to.remove),no.daysBB))
  if(length(to.remove)>0){
    d<-d[-to.remove,]
  }
  }
}
stop("Not an error, just stopping here to say we have successfully applied multiresp!")

#dim(d)

