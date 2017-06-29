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

## continued by Nacho on 29th Jun 
# Following Lizzie's recs @ issue 95 in Git
#(Aim 1) We need to deal with any data where multiresp=TRUE and make sure we are not using duplicate data
#(Aim 2) We need to deal with data where we have multiple forms of daystobudburst. Here is the order of what we want:
# (1) Most want: daystobudburst (if we have this for a certain datasetID-study, we ignore the other response variables)
# (2) percentbudburst with response.time filled in
# (3) thermaltime: I think we do not want this because we would have converted it if we could .... and thus it would be in daystobudburst


## to start
#rm(list=ls())
options(stringsAsFactors=FALSE)



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

# remove junttila from target studies 
if(sum(grepl("junttila",multdatsets_days))>0){
  juntt<-which(grepl("junttila",multdatsets_days))
  multdatsets_days<-multdatsets_days[-juntt]
}
if(sum(grepl("junttila",multdatsets_percbb))>0){
  juntt<-which(grepl("junttila",multdatsets_percbb))
  multdatsets_percbb<-multdatsets_percbb[-juntt]
}
if(sum(grepl("junttila",multdatsets_tt))>0){
  juntt<-which(grepl("junttila",multdatsets_tt))
  multdatsets_tt<-multdatsets_tt[-juntt]
}


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
#XX[order(XX$fig),] head(XX)


## subset each experiment from each paper and remove unwanted rows (either percbb, or ttime)
rows.to.remove.1<-list()
rows.to.remove.2<-list()
for(i in 1:length(multdatsets_percbb)){#i=2
  XX.i.ref<-subset(XX,study==multdatsets_percbb[i])
  
  if("daystobudburst" %in% unique(XX.i.ref$respvar.simple)){
    XX.i<-subset(XX.i.ref,respvar.simple=="daystobudburst")
    to.remove.1<-XX.i.ref$row.ID[which(!XX.i.ref$row.ID%in%XX.i$row.ID)]
    rows.to.remove.1[[i]]<-to.remove.1
  } else {
    XX.i<-XX.i.ref
  }
  
  if("percentbudburst" %in% unique(XX.i$respvar.simple)){
    XX.i<-subset(XX.i,respvar.simple=="percentbudburst")
    to.remove.2<-XX.i.ref$row.ID[which(!XX.i.ref$row.ID%in%XX.i$row.ID)]
    rows.to.remove.2[[i]]<-to.remove.2
  }

}

list.rows.2remove<-as.numeric(c(unique(unlist(rows.to.remove.1)),unique(unlist(rows.to.remove.2))))

## this would remove identified rows from global dataset: Check before activating!!
if(length(list.rows.2remove)>0){
  d<-d[-list.rows.2remove,]
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
                          seq(1,nrow(d),1)[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days]))
}

if(target.multiresp=="multi_respvar.simple"){
  YY<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$respvar[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$respvar.simple[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$response[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$chilltemp[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$datasetIDstudy[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          seq(1,nrow(d),1)[d$multi_respvar.simple==TRUE & d$datasetIDstudy%in%multdatsets_days]))
}

colnames(YY)<-c("fig","respvar","respvar.simple","response", "chilltemp","study","row.ID")
#YY[order(YY$fig),] 

## subset each experiment from each paper and remove unwanted rows (either percbb, or ttime)
rows.to.remove.1<-list()
rows.to.remove.2<-list()
for(i in 1:length(multdatsets_percbb)){#i=2
  YY.i.ref<-subset(YY,study==multdatsets_percbb[i])
  
  if("daystobudburst" %in% unique(YY.i.ref$respvar.simple)){
    YY.i<-subset(YY.i.ref,respvar.simple=="daystobudburst")
    to.remove.1<-YY.i.ref$row.ID[which(!YY.i.ref$row.ID%in%YY.i$row.ID)]
    rows.to.remove.1[[i]]<-to.remove.1
  } else {
    YY.i<-YY.i.ref
  }
  
  if("percentbudburst" %in% unique(YY.i$respvar.simple)){
    YY.i<-subset(YY.i,respvar.simple=="percentbudburst")
    to.remove.2<-YY.i.ref$row.ID[which(!YY.i.ref$row.ID%in%YY.i$row.ID)]
    rows.to.remove.2[[i]]<-to.remove.2
  }
  
}

list.rows.2remove.Y<-as.numeric(c(unique(unlist(rows.to.remove.1)),unique(unlist(rows.to.remove.2))))

## this would remove identified rows from global dataset: Check before activating!!
if(length(list.rows.2remove.Y)>0){
  d<-d[-list.rows.2remove.Y,]
  #dim(d)
}




stop("Not an error, just stopping here to say we have successfully applied multiresp!")

#dim(d)

