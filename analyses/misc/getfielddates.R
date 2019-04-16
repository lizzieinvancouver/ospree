## How to extract only the field sample dates that are at least 2 weeks apart ##
## By Lizzie (June 2018)

## See also studydesignplots_more.R where I use the f(x) ##

if(FALSE){
## Here's the simple example I made up to test ##
fieldats <- c("01-Jan-2017", "05-Jan-2017", "16-Jan-2017", "16-Mar-2017",
    "17-Mar-2017", "16-Apr-2017", "16-Apr-2018")
goober <- as.Date(fieldats, format="%d-%b-%Y")
d <- as.matrix(dist(goober))
d[d<14] <- NA
d[d==0] <- NA

d[upper.tri(d)] <- NA

fieldat <- data.frame(date=fieldats, keep=NA)

# Take the first column, and find the row with minimum value:
datx <- which(d[,1]==min(d[,1], na.rm=TRUE))
keepwhich <- datx
# Now, while the row selected in datx is not the last (final) row in d...
# keep adding those rows to the keepwhich file
while (datx < nrow(d)){
    datw <- which(d[,datx]==min(d[,datx], na.rm=TRUE))
    keepwhich <- c(keepwhich, datw)
    datx <- datw
    }
fieldats[c(1,keepwhich)]
    }

## And here's a way to do it with OSPREE ##
countfieldsample <- function(dat, daysapart){
    fieldsamplecount <- data.frame(datasetIDstudy=unique(dat[["datasetIDstudy"]]), count=NA)
    for (uniquestudy in c(1:length(unique(dat[["datasetIDstudy"]])))){
        subby <- subset(dat, datasetIDstudy==unique(dat[["datasetIDstudy"]])[uniquestudy])
        fieldats <- unique(subby$fieldsample.date)
        if(all(is.na(fieldats))) {
            fieldsamplecount[uniquestudy,2] <- NA
            } else{
          if(length(unique(subby$fieldsample.date))==1) {
            fieldsamplecount[uniquestudy,2] <- 1
            } else{
              subbydates <- unique(subby$fieldsample.date)
              datemat <- as.matrix(dist(subbydates))
              datemat[datemat<daysapart] <- NA
              datemat[datemat==0] <- NA
              datemat[upper.tri(datemat)] <- NA 
            if(all(is.na(datemat))) {
              fieldsamplecount[uniquestudy,2] <- 1
              } else{
                datx <- which(datemat[,1]==min(datemat[,1], na.rm=TRUE))
                  if (length(datx)>1) {
                    datx <- min(datx)
                    }
                    else{ 
                    datx <- datx}
                keepwhich <- datx
                while (datx < nrow(datemat)){
                  if(all(is.na(datemat[,datx]))==TRUE) {
                    datx <- datx+1
                    } else{
                     datw <- which(datemat[,datx]==min(datemat[,datx], na.rm=TRUE))
                     keepwhich <- c(keepwhich, datw)
                     datx <- datw
            }
          }
            fieldsamplecount[uniquestudy,2] <- length(keepwhich)+1
        }
        }
        }
}
    return(fieldsamplecount)
}


fieldsample.getuniquedates <- function(dat, daysapart){
    fieldsamplediffdates <- data.frame(datasetIDstudy=NA, date=as.Date("1900-01-01"))
    for (uniquestudy in c(1:length(unique(dat[["datasetIDstudy"]])))){
        subby <- subset(dat, datasetIDstudy==unique(dat[["datasetIDstudy"]])[uniquestudy])
        fieldats <- unique(subby$fieldsample.date)
        if(all(is.na(fieldats))) {
            fieldsamplediffdates.add <- data.frame(datasetIDstudy=unique(dat[["datasetIDstudy"]])[uniquestudy],
                date=NA)            } else{
          if(length(unique(subby$fieldsample.date))==1) {
            fieldsamplediffdates.add <- data.frame(datasetIDstudy=unique(dat[["datasetIDstudy"]])[uniquestudy],
                date=unique(subby$fieldsample.date)) 
            } else{
              subbydates <- unique(subby$fieldsample.date)
              datemat <- as.matrix(dist(subbydates))
              datemat[datemat<daysapart] <- NA
              datemat[datemat==0] <- NA
              datemat[upper.tri(datemat)] <- NA 
            if(all(is.na(datemat))) {
              fieldsamplediffdates.add <- data.frame(datasetIDstudy=unique(dat[["datasetIDstudy"]])[uniquestudy],
                date=unique(subby$fieldsample.date)[1]) 
              } else{
                datx <- which(datemat[,1]==min(datemat[,1], na.rm=TRUE))
                  if (length(datx)>1) {
                    datx <- min(datx)
                    }
                    else{ 
                    datx <- datx}
                keepwhich <- datx
                while (datx < nrow(datemat)){
                  if(all(is.na(datemat[,datx]))==TRUE) {
                    datx <- datx+1
                    } else{
                     datw <- which(datemat[,datx]==min(datemat[,datx], na.rm=TRUE))
                     keepwhich <- c(keepwhich, datw)
                     datx <- datw
            }
          }
            fieldsamplediffdates.add <- data.frame(datasetIDstudy=rep(unique(dat[["datasetIDstudy"]])[uniquestudy],
                length(fieldats[c(1,keepwhich)])), date=fieldats[c(1,keepwhich)])
        }
        }
        }
        fieldsamplediffdates <- rbind(fieldsamplediffdates, fieldsamplediffdates.add)
}
    return(fieldsamplediffdates[-1,])
}


