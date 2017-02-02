#Calculating chilling units in field and from experimental work
#This code calcluates the field chilling column from any chilling that occurred in the field (before material was brought into an experiment)
#It will be added to experimental chilling to make a 'totalchill' variable. 
#Based on code from EJ Forrestel 20 May 2016: Pieces for code for reading in and pulling data from netCDF files for Dan
#Ailene Ettinger added code for experimental chilling calculations on 5 July 2016
#Ailene and Cat modified January 2017 to include lat/longs collected from different subpopulations
#(Provenance lat/long
#This code requires global climate data to be pulled from an external hard drive. 
#the climate data is used to calculate chilling units, which are then written to a csv file.
#somewhere in this code, dupilcate rows are created for some studies and Ailene needs to figure out why!
#rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(ncdf4)
library(dplyr)
library(Interpol.T)
library(chillR)

setwd("~/git/ospree")
d3 <- read.csv("analyses/output/ospree_clean.csv")#this file should use the datafile created from the "cleaning_chilltemp.R" code. For now, use cleaned data file created from Lizzie's "cleanmerge_all.R" code
colnames(d3)[17]<-"fsdate_tofix"#the date format in this new file needs to be changed, for this code to work
d3$fieldsample.date<-strptime(strptime(d3$fsdate_tofix, format = "%d-%b-%Y"),format = "%Y-%m-%d")

d <- subset(d3, woody=="yes")

# make two data frames. North America and Europe, the lat longs and years.

d$continent <- tolower(d$continent)
d$datasetID <- as.character(d$datasetID)
d$provenance.lat <- as.numeric(as.character(d$provenance.lat))
d$provenance.long <- as.numeric(as.character(d$provenance.long))
d$year <- as.numeric(as.character(d$year))

d$fieldsample.date<-as.character(as.Date(d$fieldsample.date,"%m/%d/%y")) #needed for new version
d <- as_data_frame(d)

##add new column that combines datasetID, provenance.lat, provenance.long, and field sample.date for later indexing

d$ID_fieldsample.date<-paste(d$datasetID,d$provenance.lat,d$provenance.long,d$fieldsample.date, sep="_")

# European studies
#want table with lat, long, year, field sample date for each study. there could  be multiple field sample dates for each study
#selecting out european studies using the dplyr package
eur <- d %>% # start with the data frame
  distinct(ID_fieldsample.date, .keep_all = TRUE) %>% # establishing grouping variables
  filter(continent == 'europe' & year >= 1950) %>%#select out europe
  dplyr::select(datasetID, provenance.lat, provenance.long, year,fieldsample.date, ID_fieldsample.date)
eur <- eur[apply(eur, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

# North America studies
nam <- d %>% # start with the data frame
  distinct(ID_fieldsample.date, .keep_all = TRUE) %>% # establishing grouping variables
  filter(continent == 'north america'& year >= 1950) %>%
  dplyr::select(datasetID, provenance.lat, provenance.long, year,fieldsample.date, ID_fieldsample.date)

nam <- nam[apply(nam, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
# these will need manual cleaning. for now use as is
#no duplicates here: checked
#duplicated(nam$ID_fieldsample.date); duplicated(eur$ID_fieldsample.date)
# Which days do we want? For year-1, start at sept 1

## reading in netCDF files. Working off of external hard drive or from downloaded climate data
# Europe first
climatedrive = "/Volumes/Ospree Climate" # "/Volumes/Ospree Climate (Ospree Climate is name of new external drive, change with new device)

eur.tempmn <- nc_open(file.path(climatedrive, "tn_0.25deg_reg_v12.0.nc"))
eur.tempmx <- nc_open(file.path(climatedrive, "tx_0.25deg_reg_v12.0.nc"))

tempval <- list() 
for(i in 1:nrow(eur)){ # i = 1
  
  # find this location
  lo <- eur[i,"provenance.long"]
  la <- eur[i,"provenance.lat"]
  
  ndiff.long.cell <- abs(eur.tempmn$dim$longitude$vals-as.numeric(lo))
  ndiff.lat.cell <- abs(eur.tempmn$dim$latitude$vals-as.numeric(la))
  nlong.cell <- which(ndiff.long.cell==min(ndiff.long.cell))[1] 
  nlat.cell <- which(ndiff.lat.cell==min(ndiff.lat.cell))[1]

  xdiff.long.cell <- abs(eur.tempmx$dim$longitude$vals-as.numeric(lo))
  xdiff.lat.cell <- abs(eur.tempmx$dim$latitude$vals-as.numeric(la))
  xlong.cell <- which(xdiff.long.cell==min(xdiff.long.cell))[1]
  xlat.cell <- which(xdiff.lat.cell==min(xdiff.lat.cell))[1]
  
  yr <- as.numeric(eur[i,"year"])#need to add month
  
  # start and end days, in days since baseline date. Set to GMT to avoid daylight savings insanity
  stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
  if(eur[i,"fieldsample.date"]!="" & as.numeric(substr(eur[i,"fieldsample.date"],6,7))>=9){stday <- strptime(paste(yr, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  if(eur[i,"fieldsample.date"]!="" & as.numeric(substr(eur[i,"fieldsample.date"],6,7))<9){stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  if(eur[i,"fieldsample.date"]==""){stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")}
  
  # using d$fieldsample.date
  if(eur[i,"fieldsample.date"]!=""){endday <- strptime(eur[i,"fieldsample.date"],"%Y-%m-%d", tz = "GMT")}
  if(eur[i,"fieldsample.date"]==""){endday <- strptime(paste(yr, "04-30", sep="-"),"%Y-%m-%d", tz = "GMT")}
  
  st <- as.numeric(as.character(stday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  en <- as.numeric(as.character(endday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
  if(en<st){en=st}
  if(endday<stday){endday=stday}
  
  # get temperature values for this range.
  # check the dim of the net cdf file, str(netcdf), and see what the order of the different dimensions are. In this case, it goes long, lat, time. So when we are moving through the file, we give it the long and lat and date of start, then move through the files by going 'up' the cube of data to the end date
  mins <- ncvar_get(eur.tempmn,'tn', 
                    start=c(nlong.cell,nlat.cell,st), 
                    count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
                    ) 
  
  maxs <- ncvar_get(eur.tempmx,'tx',
                    start=c(xlong.cell,xlat.cell,st),
                    count=c(1,1,en-st+1)
                    )
  
  tempval[[as.character(eur[i,"ID_fieldsample.date"])]] <- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                                                  Tmin = mins, Tmax = maxs)#
}


nc_close(eur.tempmx)
nc_close(eur.tempmn)
######################################################
# North America
######################################################

nafiles <- dir(climatedrive)[grep("livneh", dir(climatedrive))]

for(i in 1:nrow(nam)){ # i = 1
  
  # find this location
  lo <- nam[i,"provenance.long"]
  la <- nam[i,"provenance.lat"]
  
  # make sure longitudes are negative, need to be for North America
  if(lo > 0) { lo = lo*-1 }
  
  yr <- as.numeric(nam[i,"year"])
  
  # using d$fieldsample.date
  if(nam[i,"fieldsample.date"]!=""){endday <- strptime(nam[i,"fieldsample.date"],"%Y-%m-%d", tz = "GMT")}
  if(nam[i,"fieldsample.date"]==""){endday <- strptime(paste(yr, "04-30", sep="-"),"%Y-%m-%d", tz = "GMT")} #if no sampling date given, use april 30
  
  if(substr(endday,1,4)==yr & as.numeric(substr(endday,6,7))<=9){#when sampling occurred in same year as study and when collection occurred before that year's sept chilling,
    stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(yr-1, formatC(9:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endmo<-substr(endday,6,7);#month of sampling date
    thismo <- paste(yr, formatC(1:endmo, width=2, flag="0"), sep="")#months from current year of chilling, through sampling date (Jan-whenever sampled)
    chillmo<-c(prevmo, thismo)
    }
  
  if(substr(endday,1,4)==yr-1 & as.numeric(substr(endday,6,7))<=12  & as.numeric(substr(endday,6,7))>=9){#when sampling occurred in previous year as study only
    stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(yr-1, formatC(9:substr(endday,6,7), width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-whenever collection occured)}
    chillmo<-prevmo
     }

    if(substr(endday,1,4)==yr & as.numeric(substr(endday,6,7))>=9){#when sampling occurred in same year as study and after chilling started that year
    stday <- strptime(paste(yr, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(yr, formatC(9:substr(endday,6,7), width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-whenever collection occured)}
    chillmo<-prevmo
  }
  
  if(substr(endday,1,4)==yr-1 & as.numeric(substr(endday,6,7))<=12  & as.numeric(substr(endday,6,7))>=9){#when sampling occurred in previous year as study between sept and dec
    stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(yr-1, formatC(9:substr(endday,6,7), width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-whenever collection occured)}
    chillmo<-prevmo
  }
  
  if(substr(endday,1,4)==yr-1 & as.numeric(substr(endday,6,7))<=12  & as.numeric(substr(endday,6,7))<9){#when sampling occurred in previous year as study, NOT during the fall
    stday <- strptime(paste(as.numeric(substr(endday,1,4))-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
    prevmo <- paste(as.numeric(substr(endday,1,4))-1, formatC(9:12, width=2, flag="0"), sep="");# use previous year's fall months of chilling (Sept-Dec)
    endmo<-substr(endday,6,7);#month of sampling date
    thismo <- paste(as.numeric(substr(endday,1,4)), formatC(1:endmo, width=2, flag="0"), sep="")#months from current year of chilling, through sampling date (Jan-whenever sampled)
    chillmo<-c(prevmo, thismo)
  }
    # now loop over these year-month combo files
  mins <- maxs <- vector()
  
  for(j in c(chillmo)){ # j = "200009"
    file <- file.path(climatedrive,nafiles[grep(j, nafiles)])
    jx <- nc_open(file)
    
    diff.long.cell <- abs(jx$dim$lon$vals-as.numeric(lo))
    diff.lat.cell <- abs(jx$dim$lat$vals-as.numeric(la))
    long.cell <- which(diff.long.cell==min(diff.long.cell))[1] 
    lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
  
    mins <- c(mins, ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))
    maxs <- c(maxs, ncvar_get(jx,'Tmax',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))
    }
  
  tempval[[as.character(nam[i,"ID_fieldsample.date"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),
                          Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])#
}
######################################################
# Interpolation
######################################################

# interporlate to hourly, based on max min 
# Build a calibration table, here we don't actually have hourly data, use best guess, just the average temperatures within this study, with a minimum at 5am, max at 2pm,  

calibration_l = list(
  Average = data.frame(time_min = rep(5, 12),
                       time_max = rep(14, 12),
                       time_suns = rep(17, 12),
                       C_m = rep(0.35, 12))
)


chillcalcs <- vector()

for(i in names(tempval)){ 
  
  xx <- tempval[[i]]
  xx$Date<-strptime(xx$Date,"%Y-%m-%d", tz="GMT")
  year = as.numeric(format(xx$Date, "%Y"))
  month = as.numeric(format(xx$Date, "%m"))
  day = as.numeric(format(xx$Date, "%d"))
  
  Tmin = data.frame(year, month, day, T = xx$Tmin)
  Tmax = data.frame(year, month, day, T = xx$Tmax)
  
  hrly = vector()
  
  for(j in 1:nrow(xx)){
    
    xy <- Th_interp(Tmin, Tmax, 
                    day = j,
                    tab_calibr = calibration_l$Average)
    
    hrly = rbind(hrly,
                 data.frame(
                   date = xx[j,'Date'],
                   Temp = xy$Th,
                   Year = Tmin$year[j], 
                   JDay = as.numeric(format(xx[j,'Date'], "%j")),
                   month = Tmin$month[j],
                   day = Tmin$day[j],
                   Hour = 1:24
                 )
    )
    
  }
  
  # Skip if NA for temperature data
  if(apply(hrly, 2, function(x) all(!is.na(x)))["Temp"]) {
  
    chillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) # 
  } else { chillcalc <- data.frame("Season"=NA,"End_year"=NA,"Chilling_Hours"=NA, "Utah_Model"=NA, "Chill_portions"=NA) }
    chillcalcs <- rbind(chillcalcs, data.frame(datasetIDlatlong = i,chillcalc[c("Season","End_year","Chilling_Hours","Utah_Model","Chill_portions")]))
}


#next will need to parse out 
#save(file="input/ChillCalcs.RData", 
write.csv(chillcalcs,"analyses/output/fieldchillcalcslatlong.csv",row.names=FALSE, eol="\r\n")
