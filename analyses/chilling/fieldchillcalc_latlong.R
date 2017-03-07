#Calculating chilling units in field and from experimental work
#This code calcluates the field chilling column from any chilling that occurred in the field (before material was brought into an experiment)
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
d$chill.long<-as.numeric(as.character(d$growing.long))
d[which(is.na(d$chill.lat)),]$chill.lat<-d[which(is.na(d$chill.lat)),]$provenance.lat
d[which(is.na(d$chill.long)),]$chill.long<-d[which(is.na(d$chill.long)),]$provenance.long
d$year <- as.numeric(as.character(d$year))

d$fieldsample.date2<-as.character(as.Date(d$fieldsample.date2,"%m/%d/%y")) #needed for new version
d2 <- as_data_frame(d)

##add new column that combines datasetID,chill.lat, chill.long, and field sample.date for later indexing

d2$ID_fieldsample.date2<-paste(d2$datasetID,d2$chill.lat,d2$chill.long,d2$fieldsample.date2, sep="_")

# European studies
#want table with lat, long, year, field sample date for each study. there could  be multiple field sample dates for each study
#selecting out european studies using the dplyr package
eur <- d2 %>% # start with the data frame
  distinct(ID_fieldsample.date2, .keep_all = TRUE) %>% # establishing grouping variables
  filter(continent == 'europe' & year >= 1950) %>%#select out europe
  dplyr::select(datasetID, chill.lat, chill.long, year,fieldsample.date2, ID_fieldsample.date2)
eur <- eur[apply(eur, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

# North America studies
nam <- d2 %>% # start with the data frame
  distinct(ID_fieldsample.date2, .keep_all = TRUE) %>% # establishing grouping variables
  filter(continent == 'north america'& year >= 1950) %>%
  dplyr::select(datasetID, chill.lat, chill.long, year,fieldsample.date2, ID_fieldsample.date2)

nam <- nam[apply(nam, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
# these may need manual cleaning. for now use as is
#no duplicates here: checked
#duplicated(nam$ID_fieldsample.date); duplicated(eur$ID_fieldsample.date)
stop("Not an error, just stopping here to say we're now done identifying the lat/longs for which chilling should be estimated. Make sure you have the external hard drive with climate data attached and proceed to pull the daily climate data.")
