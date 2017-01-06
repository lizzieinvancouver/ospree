# Calculating chilling units in field and from experimental work, for summing to a 'totalchill' variable. 
# Based on code from EJ Forrestel 20 May 2016: Pieces for code for reading in and pulling data from netCDF files for Dan
# Ailene Ettinger added code for experimental chilling calculations on 5 July 2016
rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(ncdf4)
library(dplyr)
library(Interpol.T)
library(chillR)

setwd("~/Documents/git/ospree")

d <- read.csv("analyses/output/ospree_clean.csv")
#d2<-d#only pull climate data for sites that are woody species?
#d <- subset(d2, woody=="yes")#should we add this?

# make two data frames. North America and Europe, the lat longs and years.

d$continent <- tolower(d$continent)
d$datasetID <- as.character(d$datasetID)
d$provenance.lat <- as.numeric(as.character(d$provenance.lat))
d$provenance.long <- as.numeric(as.character(d$provenance.long))
d$year <- as.numeric(as.character(d$year))

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

# Which days do we want? For year-1, start at sept 1

## reading in netCDF files. Working off of external hard drive or from downloaded climate data
# Europe first
climatedrive = "/Volumes/Dan3/Climate" # "/Volumes/Dan3/Climate/" (Dan3 is name of device, change with new device)

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
  
  yr <- as.numeric(eur[i,"year"])
  
  # start and end days, in days since baseline date. Set to GMT to avoid daylight savings insanity
  stday <- strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT")
  
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
                                                  Tmin = mins, Tmax = maxs)#Ailene tried adding lat/long to this output table on 5 jan 2017 but i'm not sure if it will work
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
                          Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])#Ailene tried adding lat/long to this output table on 5 jan 2017 but i'm not sure if it will work
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

for(i in names(tempval)){ #this used to be i in names(tempval)- Ailene changed this to accomodate multiple prov lat/longs per dataset id not sure it will work!
  
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
  
    chillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) # 39 chill portions by Jan 20 last year.
  } else { chillcalc <- data.frame("Chilling_Hours"=NA, "Utah_Model"=NA, "Chill_portions"=NA) }
    chillcalcs <- rbind(chillcalcs, data.frame(datasetIDlatlong = i,chillcalc[c("Chilling_Hours","Utah_Model","Chill_portions")]))
    #i think this will work: lat and long are part of dataset id column chillcalcs output table
}

#next will need to parse out 
#save(file="input/ChillCalcs.RData", 
 #    list = c('chillcalcs', 'tempval'))
write.csv(chillcalcs,"analyses/output/fieldchillcalcslatlong.csv",row.names=FALSE, eol="\r\n")

############################################################################################
# Start here if field chill calcs from the climate data have already been done
# Merge field and experimental chilling calculations with the rest of the data
############################################################################################

dat <- read.csv("analyses/output/ospree_clean.csv")
#use only woody species
dat2 <- subset(dat, woody=="yes")
#Make a column that indexes the study, provenance latitude,provenance longitude, and field sample date, in order to calculate field chilling
dat2$ID_fieldsample.date<-paste(dat2$datasetID,dat2$provenance.lat,dat2$provenance.long,dat2$fieldsample.date, sep="_")

#Make a column that indexes the experimental chilling treatment (including chilltemp, chillphotoperiod & chilldays), in order to calculate field chilling
dat2$ID_chilltreat<-paste(dat2$datasetID,dat2$chilltemp,dat2$chilldays,sep=".")

##### Calculate experimental chilling, using chillday and chilltemp.
###there are many non-numeric values in the chilltemp and chilldays columns- these are unusable currently so remove:
#want table with datasetID chilling days, chilling temperature,  treat for each study. 
chilldat <- dat2 %>% # start with the data frame
  distinct(ID_chilltreat,.keep_all = TRUE) %>% # establishing grouping variables
  select(datasetID, chilltemp, chilldays, year,ID_chilltreat)

chilldat$chilltemp<-as.numeric(chilldat$chilltemp)
chilldat$chilldays<-as.numeric(chilldat$chilldays)
chilldat<- chilldat[apply(chilldat, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

expchillcalcs <- vector()

###First, need file with hrly temperature data for each row in dataframe

for(i in 1:nrow(chilldat)) {
  # Skip if NA for chilltemp or chilldays data
  if(!is.na(chilldat$chilltemp[i]) & chilldat$chilldays[i] !=0 & !is.na(chilldat$chilldays[i])) {
    yr <- as.numeric(chilldat$year[i]) 
    if(is.na(yr)){ yr <- 2016} #temporary fix for when years are not currently listed!
  hrly =
      data.frame(
        Temp = rep(as.numeric(chilldat$chilltemp[i]), 
                   times = 24 * round(as.numeric(chilldat$chilldays[i], digits=0))),
        Year = rep(yr, 
                   times = 24 * round(as.numeric(chilldat$chilldays[i],digits=0))),
        JDay = sort(rep(as.numeric(seq(1:round(as.numeric(chilldat$chilldays[i], digits=0)))), times = 24))
      )
  
     expchillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) 
     
  } else { expchillcalc <- data.frame("Chilling_Hours"=NA, "Utah_Model"=NA, "Chill_portions"=NA) }
  
  expchillcalcs <- rbind(expchillcalcs, 
                         data.frame(datasetID = chilldat$datasetID[i], 
                                    ID_chilltreat = chilldat$ID_chilltreat[i],
                                    expchillcalc[c("Chilling_Hours","Utah_Model","Chill_portions")]))
  
  }

colnames(expchillcalcs)[3:5] <- c("Exp_Chilling_Hours","Exp_Utah_Model","Exp_Chill_portions")


###Merge field and experimental chilling data with the rest of the data
# Add experimental chilling. Right number of rows = 11984
dat3 <- merge(dat2, expchillcalcs, 
              by.x = c("datasetID","ID_chilltreat"),
              by.y=c("datasetID","ID_chilltreat"),
              all.x=T)

#Add field chilling calculations to datafile, 
###First, read in chillcalc file, so that you don't have to run the above code with the external hard drive of climate data
chillcalcs <- read.csv("analyses/input/fieldchillcalcslatlong.csv", header=T)
chillcalcs <- chillcalcs[apply(chillcalcs, 1, function(x) all(!is.na(x))),] # only keep rows of all not NA. 354 rows now.

colnames(chillcalcs) <- c("ID_fieldsample.date","Field_Chilling_Hours","Field_Utah_Model","Field_Chill_portions")

(todrop <- chillcalcs$ID_fieldsample.date[!chillcalcs$ID_fieldsample.date %in% dat3$ID_fieldsample.date])#we lose 50 from heide07, heide07, heide77, kinet93,kronenberg76,lieten97,smeets80, smeets82,sonsteby06, sonsteby09a,sonsteby09b,verheul07,voipio01,bradford10,durner84

# Some will be missing because they are not North America or Europe (eg biasi12, cook00, gansert02, nishimoto95). Others should have it: viheraaarni06 for example. Those without dates do not have field chilling, because do not have a field sample date.
(nochillcalcs <- unique(dat3$ID_fieldsample.date[!dat3$ID_fieldsample.date %in% chillcalcs$ID_fieldsample.date]))

chillcalcs <- chillcalcs[chillcalcs$ID_fieldsample.date %in% dat3$ID_fieldsample.date,]

# now 304 rows

#When doing either of the merges below, we get 549 extra rows. still not sure why!
#dat4 <- merge(dat3, chillcalcs, 
#               by = "ID_fieldsample.date",
#               all.x = TRUE
#               ) 
dat4<-join(dat3, chillcalcs,by="ID_fieldsample.date",type="full",match="all")
dat4<-full_join(dat3, chillcalcs, by="ID_fieldsample.date", match="all") #Added by Cat

# Merge manually

dat4a <- data.frame(dat3, chillcalcs[match(dat3$ID_fieldsample.date, chillcalcs$ID_fieldsample.date),])

### Now add column for total chilling (field plus experimental)
### First, total chilling = exp and field
dat4$Total_Chilling_Hours <- dat4$Exp_Chilling_Hours+dat4$Field_Chilling_Hours
dat4$Total_Utah_Model <- dat4$Exp_Utah_Model+dat4$Field_Utah_Model
dat4$Total_Chill_portions <- dat4$Exp_Chill_portions+dat4$Field_Chill_portions

#For sites with no experimental chilling, just use field chilling:
dat4[which(is.na(dat4$Exp_Chilling_Hours)),]$Total_Chilling_Hours<-dat4[which(is.na(dat4$Exp_Chilling_Hours)),]$Field_Chilling_Hours
dat4[which(is.na(dat4$Exp_Utah_Model)),]$Total_Utah_Model<-dat4[which(is.na(dat4$Exp_Utah_Model)),]$Field_Utah_Model
dat4[which(is.na(dat4$Exp_Chill_portions)),]$Total_Chill_portions<-dat4[which(is.na(dat4$Exp_Chill_portions)),]$Field_Chill_portions
#For sites with no field chilling, should we use only experimental chilling? not sure if this is ok...
dat4[which(is.na(dat4$Field_Chilling_Hours)),]$Total_Chilling_Hours<-dat4[which(is.na(dat4$Field_Chilling_Hours)),]$Exp_Chilling_Hours
dat4[which(is.na(dat4$Field_Utah_Model)),]$Total_Utah_Model<-dat4[which(is.na(dat4$Field_Utah_Model)),]$Exp_Utah_Model
dat4[which(is.na(dat4$Field_Chill_portions)),]$Total_Chill_portions<-dat4[which(is.na(dat4$Field_Chill_portions)),]$Exp_Chill_portions

write.csv(dat4,"~/Documents/git/ospree/analyses/output/ospree_clean_final.csv",row.names=FALSE, eol="\r\n") #Added by Cat

write.csv(dat4,"output/ospree_clean_withchill.csv",row.names=FALSE, eol="\r\n")




# scratch
days <- ncvar_get(eur.tempmn,"time") # since jan 1 1950
daysd <- strptime("1950-01-01", "%Y-%m-%d") + days*60*60*24 # convert to actual days

nafiles <- dir()[grep("livneh", dir())]

for(i in nafiles){ # i = "livneh_NAmerExt_15Oct2014.195906.nc"
  
  xx <- nc_open(i)
  tx <- ncvar_get(xx, 'Tmax')
  tn <- ncvar_get(xx, 'Tmin')
  
  }


###### Below is from Beth

###reading in netCDF as raster files
##reading in raster files of relevant layers
landfrac.r <- raster("landfrac/b.e11.BRCP85C5CNBDRD.f09_g16.017.clm2.h0.QRUNOFF.208101-210012.nc",varname="landfrac")
##reading in land fraction as a raster and rotating to make it match BEST data orientation; from (0,360) to (-180,180)
landfrac.r <- rotate(landfrac.r)
#image(landfrac.r)
