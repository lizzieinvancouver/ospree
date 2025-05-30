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

############################################
## Below added for Isabelle Chuine's work ##
############################################

### f(x) to get just number of treatments
### I am sure we have done this for OSPREE other places also ... 

get.treatdists.singletreatment <- function(df, treatcol){
    dfbuild <- data.frame(datasetID=character(), study=character(), ntreats=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp.allcols <- subset(subbydid, study==studyid)
          subbydidexp <- subbydidexp.allcols[,c(treatcol)]
          dfbuildadd <- data.frame(datasetID=did, study=studyid, ntreats=length(unique(subbydidexp)))
          dfbuild <- rbind(dfbuild, dfbuildadd)
          dfbuild.multi <- subset(dfbuild, ntreats>1)
          }
     }
  return(dfbuild.multi)
}
          
### f(x) to get experiments with varying day-night temperatures

# this is a better f(x), it should be more accurate as I worked a little harder on it
# it writes out info on the forcing in general and then counts up number of treatments with constant day/night temps
# and number of treatments with varying day/night temps
get.treatdists.daynight <- function(df, treatcol, othertreatcol){
    dfbuild <- data.frame(datasetID=character(), study=character(), treatinfo=character(), numconstantforce=numeric(),
       numdiffforce=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp.allcols <- subset(subbydid, study==studyid)
          subbydidexp <- subbydidexp.allcols[,c(treatcol, othertreatcol)]
          ndaytemps <- length(unique(subbydidexp.allcols[,c(treatcol)]))
          nnighttemps <- length(unique(subbydidexp.allcols[,c(othertreatcol)]))
          if (ndaytemps==1 & nnighttemps==1) dfbuildadd <- data.frame(datasetID=did,
             study=studyid, treatinfo="forcing does not vary", numconstantforce=NA, numdiffforce=NA) else {
             subbydidexp.nodup <- subbydidexp[!duplicated(subbydidexp),]
             subbydidexp.nodup.wcount <- transform(subbydidexp.nodup, same = apply(subbydidexp.nodup, 1,
                function(x) length(unique(x)) == 1))
             numconstantforce <- nrow(subset(subbydidexp.nodup.wcount, same==TRUE))
             numdiffforce <- nrow(subset(subbydidexp.nodup.wcount, same==FALSE))
             dfbuildadd <- data.frame(datasetID=did, study=studyid, treatinfo="some diff daynight",
                 numconstantforce=numconstantforce, numdiffforce=numdiffforce)
            }
      dfbuild <- rbind(dfbuild, dfbuildadd)
                 }
          }
  return(dfbuild)
}


# this just tries to count day and night temps, it is not perfect as I wrote it quickly (use at your own risk!)
get.treatdists.daynight.alt <- function(df, treatcol, othertreatcol){
    dfbuild <- data.frame(datasetID=character(), study=character(), ndaytemps=numeric(), nnighttemps=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp.allcols <- subset(subbydid, study==studyid)
          ndaytemps <- length(unique(subbydidexp.allcols[,c(treatcol)]))
          nnighttemps <- length(unique(subbydidexp.allcols[,c(othertreatcol)]))
          dfbuildadd <- data.frame(datasetID=did, study=studyid, ndaytemps=ndaytemps, nnighttemps=nnighttemps)
          dfbuild <- rbind(dfbuild, dfbuildadd)
          dfbuild.multi <- subset(dfbuild, ndaytemps>1 | nnighttemps>1)
                 }
          }
  return(dfbuild.multi)
}
