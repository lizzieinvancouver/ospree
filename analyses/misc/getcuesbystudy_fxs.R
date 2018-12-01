## f(x)s to calculate differences between cues within studies ##

## Count up cues and get MAX differences between them ...
## Requires: (1) cue column must be NUMERIC, (2) datasetIDstudy column

getcueinfo <- function(df, cue.colname, datasetIDstudy){
    fillmein <- data.frame(datasetIDstudy=NA, maxdiff.treat=NA, n.treats=NA)
    for(i in c(1:length(unique(df[[datasetIDstudy]])))){
        subby <- df[which(df[[datasetIDstudy]]==unique(df[[datasetIDstudy]])[i]),]
        subbytreat <- subset(subby, select=c())
        subbytreat <- subby[, c(datasetIDstudy, cue.colname)]
        subbytreat.nodups <- subbytreat[!duplicated(subbytreat), ]
        # scenario #1: There's only one treatment (and or no treatment) 
        if(nrow(subbytreat.nodups)==1) {
            fillmein.add <-
                data.frame(datasetIDstudy=unique(df[[datasetIDstudy]])[i],
                maxdiff.treat=NA, n.treats=nrow(subbytreat.nodups))} else {
        # scenario # 2: There is more than one treatment
        fillmein.add <- data.frame(datasetIDstudy=unique(df[[datasetIDstudy]])[i],
            maxdiff.treat=(max(subbytreat.nodups[[cue.colname]])-
                           min(subbytreat.nodups[[cue.colname]])),
            n.treats=nrow(subbytreat.nodups))
        }
    fillmein <- rbind(fillmein, fillmein.add)
    }
   return(fillmein[-1,]) # delete my row of empty data
}

## Count up cues and get consecutive differences between them  ...
# For example: 6, 9, 21 forcetemp gives diffs of 3 and 12 (and misses 15)
## Requires: (1) cue column must be NUMERIC, (2) datasetIDstudy column

getcueinfo.consdiffs <- function(df, cue.colname, datasetIDstudy){
    fillmein <- data.frame(datasetIDstudy=NA, diff.treat=NA)
    for(i in c(1:length(unique(df[[datasetIDstudy]])))){
        subby <- df[which(df[[datasetIDstudy]]==unique(df[[datasetIDstudy]])[i]),]
        subbytreat <- subset(subby, select=c())
        subbytreat <- subby[, c(datasetIDstudy, cue.colname)]
        subbytreat.nodups <- subbytreat[!duplicated(subbytreat), ]
        # scenario #1: There's only one treatment (and or no treatment) so there is ONE ROW
        # in this case, we say there is NA for a diff
        if(nrow(subbytreat.nodups)==1) {
            fillmein.add <-
                data.frame(datasetIDstudy=unique(df[[datasetIDstudy]])[i],
                diff.treat=NA)} else {
        # scenario # 2: There is more than one treatment
        # order by ascending
        subbytreat.nodups <- subbytreat.nodups[order(subbytreat.nodups[[cue.colname]]),]
        fillmein.add <- data.frame(datasetIDstudy=rep(unique(df[[datasetIDstudy]])[i], nrow(subbytreat.nodups)-1),
           diff.treat=diff(subbytreat.nodups[[cue.colname]], lag=1 ))
        }
    fillmein <- rbind(fillmein, fillmein.add)
    }
   return(fillmein[-1,]) # delete my row of empty data
}


## Count up cues and get ALL differences between them  ...
# For example: 6, 9, 21 forcetemp gives diffs of 3, 12, 15 
## Requires: (1) cue column must be NUMERIC, (2) datasetIDstudy column
getcueinfo.alldiffs <- function(df, cue.colname, datasetIDstudy){
    fillmein <- data.frame(datasetIDstudy=NA, diff.treat=NA)
    for(i in c(1:length(unique(df[[datasetIDstudy]])))){
        subby <- df[which(df[[datasetIDstudy]]==unique(df[[datasetIDstudy]])[i]),]
        subbytreat <- subset(subby, select=c())
        subbytreat <- subby[, c(datasetIDstudy, cue.colname)]
        subbytreat.nodups <- subbytreat[!duplicated(subbytreat), ]
        # scenario #1: There's only one treatment (and or no treatment) so there is ONE ROW
        # in this case, we say there is NA for a diff
        if(nrow(subbytreat.nodups)==1) {
            fillmein.add <-
                data.frame(datasetIDstudy=unique(df[[datasetIDstudy]])[i],
                diff.treat=NA)} else {
        # scenario # 2: There is more than one treatment
        # here we need to get all diffs
        mathere <- as.matrix(dist(subbytreat.nodups[[cue.colname]]))
        mathere[upper.tri(mathere)] <- 0
        unique(c(mathere))
        alldiffs <- unique(c(mathere))
        uniquetreats <- alldiffs[alldiffs>0]
        fillmein.add <- data.frame(datasetIDstudy=rep(unique(df[[datasetIDstudy]])[i], length(uniquetreats)),
           diff.treat=uniquetreats)
        }
    fillmein <- rbind(fillmein, fillmein.add)
    }
   return(fillmein[-1,]) # delete my row of empty data
}
