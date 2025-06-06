# For experiments when multiple different response variables that:
# (Aim 1) Were multiple response variables in respvar column, that collapsed into one response variable in respvar.simple column, that is multiresp=TRUE (see clean_respvar.R) ... but only dealing with budburst related stuff here!
# (Aim 2) Papers contained the same data in more than one response variable (e.g., a table with daystobudburst and a figure with percentbudburst versus daystobudburst for same exact study).

## Started by Ailene Ettinger ##
# 2 Feb 2017 ## 
## Continued by Nacho on 21st April, celebrating Earth's Day! (Go Earth! Don't let us humans keep you down.)
# Following Lizzie's recs @ issue 81 in Git:
# (1) select daystobudburst if given (aka if daystobudburst is an option for the study, select that one)
# (2) select highest percent if no daystobudburst (e.g., two response variables and one is daysto10perbudburst and other is daysto50perbudburst) 
# (3) check that n across different response types is not grossly different (aka >25 off)

## Continued by Nacho on 29 Jun 2017 ##
# Following Lizzie's recs @ issue 95 in Git (noted above)

## Note from Lizzie! 7 July 2017 ##
# For Aim 1 we do currently delete a little unique data, but it is for 10% budburst, which seems lower than we want for a days to budburst analysis. But note that PERCENT budburst studies would want to go get these data, perhaps. See notes for spiers74 below. #

# SOURCED in bb_cleanmergeall.R 
#start with output/ospree_clean_withchill.csv file
#d<-read.csv("output/ospree_clean_withchill.csv", header=TRUE)
#dim(d)

# Most of this deals with Aim 2 ..

target.multiresp<-"multi_respvar"


  # only multiresp
  multdatsets_days<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$respvar.simple=="daystobudburst"])# 26 studies for which there are multiple response variables and days to budburst
  multdatsets_percbb<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$respvar.simple=="percentbudburst"])# 28 study for which there are multiple response variables and percent to budburst
  multdatsets_tt<-unique(d$datasetIDstudy[d$multi_respvar==TRUE & d$respvar.simple=="thermaltime"])# 6 studies should add studies with thermal time too?

# replacing factors for character (not needed but just in case)
multdatsets_days <- as.character(multdatsets_days)
multdatsets_percbb <- as.character(multdatsets_percbb)
multdatsets_tt <- as.character(multdatsets_tt)

# remove junttila from target studies
# Juntilla has two names for the same variable, but it is okay because they are from two different experiments (this is handled at the bottom)
# So, to repeat, no changes need to juntilla
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

# Trying to figure out where the data for percbb are coming from and what they are
# we generate a subset XX of data for studies with multiple response variables and percent to budburst

  XX<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                          d$respvar[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                          d$respvar.simple[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                          d$response[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                          d$chilltemp[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                          d$datasetIDstudy[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb],
                          seq(1,nrow(d),1)[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_percbb]))

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

# Lizzie's checking code:
# goo <- d
# goorem <- d[list.rows.2remove,]

## this would remove identified rows from global dataset: Lizzie checked before activating... 
if(length(list.rows.2remove)>0){
  d<-d[-list.rows.2remove,]
  #dim(d)
}

# Sample of Lizzie's checking code:
# d$respvar[d$datasetID=="ghelardini10"] 
# goo$respvar[goo$datasetID=="ghelardini10"]

#look at days and repeat the above procedure
#Trying to figure out where the data for percbb are coming from and what they are
  YY<-as.data.frame(cbind(d$figure.table..if.applicable.[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$respvar[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$respvar.simple[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$response[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$chilltemp[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          d$datasetIDstudy[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days],
                          seq(1,nrow(d),1)[d$multi_respvar==TRUE & d$datasetIDstudy%in%multdatsets_days]))

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

# as of 7 July 2017, above does nothing (which is okay by Lizzie!)
list.rows.2remove.Y

## this would remove identified rows from global dataset: Check before activating!!
if(length(list.rows.2remove.Y)>0){
  d<-d[-list.rows.2remove.Y,]
  #dim(d)
}


# Now deal with the Aim 1 issues:
problemchildren1 <- subset(d, multiresp==TRUE & respvar.simple=="percentbudburst")
unique(problemchildren1$datasetID)
d$respvar[which(d$datasetID=="junttila12" & d$respvar=="percentbudburst_dormancy")] <- "percentbudburst"

problemchildren2 <- subset(d, multiresp==TRUE & respvar.simple=="daystobudburst")
unique(problemchildren2$datasetID)
d <- d[-which(d$datasetID=="spiers74" & d$respvar=="daysto10percentbudburst"),]
# Note that above (at least from looking at the original paper) we do lose some data here -- more of the treatments made it to 10% budburst than to 50%, but 10% is really low, so it seems okay to me to exclude these for the analysis at hand. 

stop("Not an error, just stopping here to say we have successfully applied multiresp!")

#dim(d)

