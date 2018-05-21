#Format budburst data
# and format the dates ...
dater$date <- as.Date(dater$fieldsample.date2, format="%Y-%m-%d")
dater$sample.year <- as.numeric(format(dater$date , "%Y"))
dater$month <- as.numeric(format(dater$date , "%m"))

# okay, we only want to work with phenology data for which we have climate data ...
dat <- dater[which(dater$datasetID %in% unique(cdater$datasetID)),]
# Back to the phenology data ... 
# We need exp ID and field sample date.
# need to fix year ...
# rule for now: if field sample date < August, then use year + 1, otherwise use year ...
#subset(dat, is.na(month)==TRUE) # WTF
dat <- subset(dat, is.na(month)==FALSE)
dat$year <- dat$sample.year
dat$year[dat$month>8] <- dat$sample.year[dat$month>8]+1
dat$year[dat$month<8] <- dat$sample.year[dat$month<8]
dat$uniqueID <- paste(dat$datasetID, dat$fieldsample.date2, dat$forcetemp, dat$chilltemp, dat$chilldays,dat$chillphotoperiod,dat$photoperiod_day)
#Climate data from some sites (boyer, ashby62, others) uses a different latitude (growing latitude instead of provenance lat)
#we need to change the latitude here back to the provenance latitude for it to line up
#read in conversion table for growing lat to provenance lat for these sites
#(generated with chilling code)
grotopro<-read.csv("output/dailyclim/chill.lat.conversion.csv", header=TRUE)
#add a flag to cdater that determines whether growing lat long or provenance lat long is used.
dat$usegrolatlong<-0
dat$usegrolatlong[which(dat$datasetID %in% unique(grotopro$datasetID))]<-1

