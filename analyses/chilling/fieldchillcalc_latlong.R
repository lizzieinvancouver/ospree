#Calculating chilling units in field and from experimental work
#This code calculates the field chilling column from any chilling that occurred in the field (before material was brought into an experiment)
#It will be added to experimental chilling to make a 'totalchill' variable. 
#Based on code from EJ Forrestel 20 May 2016: Pieces for code for reading in and pulling data from netCDF files for Dan
#Ailene Ettinger added code for experimental chilling calculations on 5 July 2016
#Ailene and Cat modified January 2017 to include lat/longs collected from different subpopulations
#(Provenance lat/long)
#This code requires global climate data to be pulled from an external hard drive. 
#the climate data is used to calculate chilling units, which are then written to a csv file.

d$fieldsample.date2<-strptime(strptime(d$fieldsample.date, format = "%d-%b-%Y"),format = "%Y-%m-%d")
# make two data frames. North America and Europe, the lat longs and years.
d$continent <- tolower(d$continent)
d$datasetID <- as.character(d$datasetID)
#add new columns for lat and long to use to calculate chilling.
#if growing lat/long is present, use those. if not, use provenance lat/long
d$chill.lat<-as.numeric(as.character(d$growing.lat))
#hmm===how do we decide whether to use growing latitude or provenance latitude. 
#should we use provenance lat for cuttings?
d$chill.long<-as.numeric(as.character(d$growing.long))
d[which(is.na(d$chill.lat)),]$chill.lat<-d[which(is.na(d$chill.lat)),]$provenance.lat
d[which(is.na(d$chill.long)),]$chill.long<-d[which(is.na(d$chill.long)),]$provenance.long
d$year <- as.numeric(as.character(d$year))
#instead of this, pull the year from fieldsample.date2? This will solve the problem of studies that have year blank but do have a field sample data
d$fieldsample.date2<-as.character(as.Date(d$fieldsample.date2,"%m/%d/%y")) #needed for new version
#add column for when experimental chilling is added to field chilling (this gets done in the interpolclimate.R file and we need to be able to merge it back in- only 2 studies do this so I am adding it by hand)
d$addexpwarm<-0
d[which(d$chilltemp=="ambient + 4"),]$addexpwarm<-"ambplus4" ## From original dataset
d[which(d$chilltemp=="ambient + 0.76"),]$addexpwarm<-"ambplus0.76" ## From original dataset

## Now adding in from new dataset ambient +'s 
d[which(d$chilltemp=="ambient + 1"),]$addexpwarm<-"ambplus1"
d[which(d$chilltemp=="ambient + 2"),]$addexpwarm<-"ambplus2"
d[which(d$chilltemp=="ambient + 3"),]$addexpwarm<-"ambplus3"
d[which(d$chilltemp=="ambient + 5"),]$addexpwarm<-"ambplus5"
d[which(d$chilltemp=="ambient - 1"),]$addexpwarm<-"ambminus1"
d[which(d$chilltemp=="ambient + 2.25"),]$addexpwarm<-"ambplus2.25"
d[which(d$chilltemp=="ambient + 4.5"),]$addexpwarm<-"ambplus4.5"
d[which(d$chilltemp=="ambient + 6.75"),]$addexpwarm<-"ambplus6.75"
d[which(d$chilltemp=="ambient + 9"),]$addexpwarm<-"ambplus9"

d2 <- as.data.frame(d)

##add new column that combines datasetID,chill.lat, chill.long, and field sample.date for later indexing

d2$ID_fieldsample.date2<-paste(d2$datasetID,d2$chill.lat,d2$chill.long,d2$fieldsample.date2,d2$addexpwarm, sep="_")

# European studies
#want table with lat, long, year, field sample date for each study. there could  be multiple field sample dates for each study
#selecting out european studies using the dplyr package
d3<-d2[d2$continent=="europe" & d2$year >= 1950,]
eur <- d3 %>% # start with the data frame
  distinct(ID_fieldsample.date2, .keep_all = TRUE) %>% # establishing grouping variables
  #filter(continent == 'europe' & year >= 1950) %>%#select out europe#this removes all rows with  field sample date=2013-10-22, even though some are in europe
  dplyr::select(datasetID, chill.lat, chill.long, year,fieldsample.date2, ID_fieldsample.date2)
eur <- eur[apply(eur, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

# North America studies
nam <- d2 %>% # start with the data frame
  distinct(ID_fieldsample.date2, .keep_all = TRUE) %>% # establishing grouping variables
  filter(continent == 'north america'& year >= 1950) %>%
  dplyr::select(datasetID, chill.lat, chill.long, year,fieldsample.date2, ID_fieldsample.date2)

nam <- nam[apply(nam, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#Create and save a list of the sites that use growing.lat instead of provenance lat for chilling.
#we need this later for getting correct climate data...
chill.lats<-d2[d2$chill.lat==d2$growing.lat,]
clats <- chill.lats %>% # start with the data frame
  distinct(ID_fieldsample.date2, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(datasetID, chill.lat, chill.long, growing.lat,growing.long,provenance.lat,provenance.long)

clats <- clats[!duplicated(clats), ]
## Above now adds in thielges75 which we don't want so I will remove this and fix fu18
clats[which(clats$datasetID=="fu18"),]$provenance.lat <- clats[which(clats$datasetID=="fu18"),]$growing.lat
clats[which(clats$datasetID=="fu18"),]$provenance.long <- clats[which(clats$datasetID=="fu18"),]$growing.long
clats <- clats[apply(clats, 1, function(x) all(!is.na(x))),] # only keep rows of all not na; when activated this should only remove thielges75 now

write.csv(clats,"output/dailyclim/chill.lat.conversion.csv", row.names=FALSE, eol="\r\n")
# these may need manual cleaning. for now use as is
#no duplicates here: checked
#duplicated(nam$ID_fieldsample.date2); duplicated(eur$ID_fieldsample.date2)
stop("Not an error, just stopping here to say we're now done identifying the lat/longs for which chilling should be estimated. Make sure you have the external hard drive with climate data attached and proceed to pull the daily climate data.")
