## See countintxns folder and related code ##
## You can even see this f(x) built in countintxns_lizziemethod.R ##

get.treatdists <- function(df, treatcol, othertreatcol){
    dfbuild <- data.frame(datasetID=character(), study=character(), intxn=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp.allcols <- subset(subbydid, study==studyid)
          subbydidexp <- subbydidexp.allcols[,c(treatcol, othertreatcol)]
             for (treathere in unique(subbydidexp[[treatcol]])){
                 # Finally, subset to one treatment (of treatcol) in each datasetID and study combo ...
                 sametreat <- subbydidexp[which(subbydidexp[[treatcol]]==treathere),]
                 uniquehere <- sametreat[!duplicated(sametreat),]
                 othertreatdist <- dist(uniquehere[[othertreatcol]])
                 # If there is not more than one other treatment, intxn=0
                 if (length(othertreatdist)==0) dfbuildadd <- data.frame(datasetID=did,
                     study=studyid, intxn=0) else {
                     subby.othertreat <- subbydidexp[which(subbydidexp[[othertreatcol]] %in% uniquehere[[othertreatcol]]),]
                     subby.diff <- subby.othertreat[which(!subby.othertreat[[treatcol]] %in% treathere),]
                       # If there are not any other treatments with a different treats and treathere, intxn=0
                       if (nrow(subby.diff) <= 1) dfbuildadd <- data.frame(datasetID=did,
                       study=studyid, intxn=0) else{
                           uniquehereagain <- subby.diff[!duplicated(subby.diff),]
                           othertreatdistagain <- dist(uniquehereagain[[othertreatcol]])
                             if (length(othertreatdistagain)==0) dfbuildadd <- data.frame(datasetID=did,
                                study=studyid, intxn=0) else{
                                dfbuildadd <- data.frame(datasetID=did, study=studyid, intxn=1)
                                }
                           }
                     }
                 dfbuild <- rbind(dfbuild, dfbuildadd)
                 dfbuildsum <- aggregate(dfbuild["intxn"], dfbuild[c("datasetID", "study")], FUN=sum)
                 }
          }
    }
  return(dfbuildsum)
}

# One more function we need
# f(x) to look at unique treats
get.uniquetreats <- function(df, treatcol, othertreatcol){
    dfbuild <- data.frame(datasetID=character(), study=character(), treat=numeric(), othertreat=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp <- subset(subbydid, study==studyid)
             for (treathere in unique(subbydidexp[[treatcol]])){
                 # Finally, subset to one treatment (of treatcol) in each datasetID and study combo ...
                 sametreat <- subbydidexp[which(subbydidexp[[treatcol]]==treathere),]
                 uniquehere <- sametreat[!duplicated(sametreat),]
                 dfadd <- data.frame(datasetID=rep(did, nrow(uniquehere)), study=rep(studyid, nrow(uniquehere)),
                     treat=uniquehere[[treatcol]], othertreat=uniquehere[[othertreatcol]])
                 dfbuild <- rbind(dfbuild, dfadd)
                 }
          }
     }
  return(dfbuild)
}
