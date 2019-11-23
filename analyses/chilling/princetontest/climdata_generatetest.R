## 4 November 2019 - by Cat
# Let's double check the new climate data
## Build fake data with list of more coordinates and years to better compare climate datasets 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")

# Load libraries
library(dplyr)
library(tidyr)
library(ncdf4)
library(Interpol.T)
library(chillR)
library(lubridate)
library(geosphere)
library(raster)

set.seed(1234)
coords <- as.data.frame(randomCoordinates(100000))
coords <- coords[(coords$lon>=-125 & coords$lon<=-60 &
                    coords$lat>=30 & coords$lat<=60),]


nam <- data.frame(lon=as.numeric(coords$lon),
                  lat=as.numeric(coords$lat), 
                  year=as.numeric(sample(1955:2013, 3317, replace=TRUE)))

noyrs <- c(1960, 1964:1970, 1975:1978, 1986:1990, 1999, 2002, 2005)

nam <- nam[!(nam$year%in%noyrs),]
nam <- nam[sample(nrow(nam), 100), ]

nam$id <- paste(nam$lat, nam$lon, nam$year)



if(FALSE){ ## Double checking data and code using 5 sites, change to `TRUE' if you want to take a closer look
#### Let's do a quick check on code.. 
save<-nam
c("55.7212644825263 -93.7611439358443 2008", "37.2717079835263 -102.993962494656 2011
", "57.0535457243981 -65.1322316285223 1972", "43.0070297862561 -121.330744698644 1980",
  "33.4588942389149 -83.5603736434132 1962")

nam <- save
nam <- nam[(nam$id=="33.4588942389149 -83.5603736434132 1962"),]
}

climatedrive = "/Volumes/climdata" # Cat's climate drive
nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]
#loop through each lat/long for which we want to calculate chilling and pull the climate data for that lat/long
#the climate data that we are pulling is daily min and max temperature

tempval_prince <- list() 
for(i in 1:nrow(nam)){ # i = 1
  # find this location
  lo <- nam[i,"lon"] + 360
  la <- nam[i,"lat"]
  
  # make sure longitudes are negative, need to be for North America this step is now done in "cleaning/clean_latlong" so it is no longer necessary
  #if(lo > 0) { lo = lo*-1 }
  yr<-as.numeric(nam[i,"year"])
  
  endday <- strptime(paste(yr, "08-30", sep="-"),"%Y-%m-%d", tz = "GMT")
  doyend <- yday(endday)
  stday <- strptime(paste(yr, "06-01", sep="-"),"%Y-%m-%d", tz="GMT")
  doyst <- yday(stday)
  
  mins <- maxs <- vector()
  
  ## for now exclude prevey18
  
  for(j in c(yr)) { # j = 1983
    print(c(i, j))
    
    tmax <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",yr), full.names = TRUE)
    tmin <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",yr), full.names = TRUE)
    jx <- nc_open(tmax)
    jn <- nc_open(tmin)
    
    leaps<-c(1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988,
             1992, 1996, 2000, 2004, 2008, 2012, 2016)
    
    if(j%in%leaps){
      yrend <- 366
    } else{
      yrend <- 365
    }
    
    gddstart <- vector()
    
    jx$dim$time$vals<-seq(1, yrend, by=1)
    thisyr <- which(jx$dim$time$vals<=doyend)
    thisyr <- which(thisyr>=doyst)
    
    diff.long.cell <- abs(jx$dim$lon$vals-as.numeric(lo))#differences between all longitudes & latitudes in the focal month's dataset and longitude[i]
    diff.lat.cell <- abs(jx$dim$lat$vals-as.numeric(la))
    long.cell <- which(diff.long.cell==min(diff.long.cell))[1] #select the closest longitude & latitude with climate data to longitude[i]
    lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
    long.cell <- which.min(abs(jx$dim$lon$vals-as.numeric(lo)))
    lat.cell <- which.min(abs(jx$dim$lat$vals-as.numeric(la)))
    maxtest<-(ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1)))[thisyr]-273.15#check that the lat/long combinations has temperature data. 
    #if no temperature data for the focal lat/long, choose the next closest one. 
    #the below code goes up to 0.1 degrees (~10km) away from the closest lat/long)
    if(is.na(unique(maxtest))){#if there are no temp data for the selected lat/long, choose a different one
      diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
      diff.lat.cell[which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]]<-NA
      long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
      lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
      maxtest<-(ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1)))[thisyr]-273.15#check that the lat/long combinations has temperature data.
      if(is.na(unique(maxtest))){
        diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
        diff.lat.cell[which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]]<-NA
        long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
        lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        maxtest<-(ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1)))[thisyr]-273.15#check that the lat/long combinations has temperature data. 
        if(is.na(unique(maxtest))){
          diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
          diff.lat.cell[which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]]<-NA
          long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
          lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        }}}
    
    maxs<-(ncvar_get(jx,start=c(long.cell,lat.cell,1),count=c(1,1,-1)))[thisyr]-273.15
    mins<-(ncvar_get(jn,start=c(long.cell,lat.cell,1),count=c(1,1,-1)))[thisyr]-273.15
    nc_close(jx)
    nc_close(jn)
  }
  
  tempval_prince[[as.character(nam[i,"id"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),  
                                                            Tmin = mins[1:length(seq(stday, endday, by = "day"))], 
                                                            Tmax =maxs[1:length(seq(stday, endday, by = "day"))])
}

#save(tempval_prince, file="output/fieldclimate_princetest.RData")

calibration_l = list(
  Average = data.frame(time_min = rep(5, 12),
                       time_max = rep(14, 12),
                       time_suns = rep(17, 12),
                       C_m = rep(0.35, 12))
)


testcalcs_prince <- vector()
for(i in names(tempval_prince)){ #i=2
  
  xx <- tempval_prince[[i]]
  xx$Date<-strptime(xx$Date,"%Y-%m-%d", tz="GMT")
  
  year = as.numeric(format(xx$Date, "%Y"))
  month = as.numeric(format(xx$Date, "%m"))
  day = as.numeric(format(xx$Date, "%d"))
  
  Tmin = data.frame(year, month, day, T = xx$Tmin)
  Tmax = data.frame(year, month, day, T = xx$Tmax)
  
  hrly = vector()
  
  for(j in 1:nrow(xx)){ #j=2
    xy <- Th_interp(Tmin, Tmax, #function that creates 24 values of hourly temperature from minimum and maximum daily values.
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
  # Skip interpolation if NA for temperature data
  if(apply(hrly, 2, function(x) all(!is.na(x)))["Temp"]) {
    
    testcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) # 
  } else { testcalc <- data.frame("Season"=NA,"End_year"=NA,"GDH"=NA) }
  
  testcalcs_prince <- rbind(testcalcs_prince, data.frame(datasetIDlatlong = i,
                                                         testcalc[c("Season","End_year","GDH")],
                                                         tmin=xx$Tmin, tmax=xx$Tmax))
}

############################################################
#################### Livneh ################################
############################################################

nafiles <- dir(climatedrive)[grep("livnehclimdata", dir(climatedrive))]

tempval_liv <- list() 
for(i in 1:nrow(nam)){ # i = 2
  # find this location
  lo <- nam[i,"lon"]
  la <- nam[i,"lat"]
  
  # make sure longitudes are negative, need to be for North America this step is now done in "cleaning/clean_latlong" so it is no longer necessary
  #if(lo > 0) { lo = lo*-1 }
  yr<-as.numeric(nam[i,"year"])
  
  endday <- strptime(paste(yr, "08-30", sep="-"),"%Y-%m-%d", tz = "GMT")
  doyend <- yday(endday)
  stday <- strptime(paste(yr, "06-01", sep="-"),"%Y-%m-%d", tz="GMT")
  
  chillmo<-paste(yr, formatC(6:substr(endday,6,7), width=2, flag="0"), sep="")
  
  mins <- maxs <- vector()
  
  for(j in c(chillmo)){ # j = "198306"
    print(c(i, j))
    
    file <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0(j), full.names = TRUE)
    jx <- nc_open(file)
    
    diff.long.cell <- abs(jx$dim$lon$vals-as.numeric(lo))#differences between all longitudes & latitudes in the focal month's dataset and longitude[i]
    diff.lat.cell <- abs(jx$dim$lat$vals-as.numeric(la))
    long.cell <- which(diff.long.cell==min(diff.long.cell))[1] #select the closest longitude & latitude with climate data to longitude[i]
    lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
    long.cell <- which.min(abs(jx$dim$lon$vals-as.numeric(lo)))
    lat.cell <- which.min(abs(jx$dim$lat$vals-as.numeric(la)))
    mintest<-ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))#checl that the lat/long combinations has temperature data. 
    #if no temperature data for the focal lat/long, choose the next closest one. 
    #the below code cose up to 0.1 degrees (~10km) away from the closest lat/long)
    if(is.na(unique(mintest))){#if there are no temp data for the selected lat/long, chosee a different one
      diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
      diff.lat.cell[which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]]<-NA
      long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
      lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
      mintest<-ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))
      if(is.na(unique(mintest))){
        diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
        diff.lat.cell[which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]]<-NA
        long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
        lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        mintest<-ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1))
        if(is.na(unique(mintest))){
          diff.long.cell[which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1]]<-NA
          diff.lat.cell[which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]]<-NA
          long.cell <- which(diff.long.cell==min(diff.long.cell,na.rm=TRUE))[1] #select the closest longitude & latitude with climate data to longitude[i]
          lat.cell <- which(diff.lat.cell==min(diff.lat.cell,na.rm=TRUE))[1]
        }}}
    
    mins <- c(mins, ncvar_get(jx,'Tmin',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))#minimum temperatures for selected lat/long
    maxs <- c(maxs, ncvar_get(jx,'Tmax',start=c(long.cell,lat.cell,1),count=c(1,1,-1)))#minimum temperatures for selected lat/long
    nc_close(jx)
  }
  
  tempval_liv[[as.character(nam[i,"id"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),  
                                                                           Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])
}
#save(tempval, file="output/fieldclimate.RData")

testcalcs_liv <- vector()

for(i in names(tempval_liv)){ # i = "35.8432389899629 -65.0118815805763 1983"
  
  xx <- tempval_liv[[i]]
  xx$Date<-strptime(xx$Date,"%Y-%m-%d", tz="GMT")
  
  year = as.numeric(format(xx$Date, "%Y"))
  month = as.numeric(format(xx$Date, "%m"))
  day = as.numeric(format(xx$Date, "%d"))
  
  Tmin = data.frame(year, month, day, T = xx$Tmin)
  Tmax = data.frame(year, month, day, T = xx$Tmax)
  
  hrly = vector()
  
  for(j in 1:nrow(xx)){
    xy <- Th_interp(Tmin, Tmax, #function that creates 24 values of hourly temperature from minimum and maximum daily values.
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
  # Skip interpolation if NA for temperature data
  if(apply(hrly, 2, function(x) all(!is.na(x)))["Temp"]) {
    
    testcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) # 
  } else { testcalc <- data.frame("Season"=NA,"End_year"=NA,"GDH"=NA) }
  
  testcalcs_liv <- rbind(testcalcs_liv, data.frame(datasetIDlatlong = i,
                                                   testcalc[c("Season","End_year","GDH")],
                                                   tmin=xx$Tmin, tmax=xx$Tmax))}

#write.csv(testcalcs_prince, "output/fieldchillcalcslatlong_princetest.csv", row.names=FALSE, eol="\r\n")

###### Now let's combine them all and check... 
#prince$gdd <- testcalcs_prince$GDH/24
#testcalcs_prince$climdata <- "prince"
prince <- testcalcs_prince
prince$gdd_prince <- prince$GDH/24
prince$tmin_prince <- prince$tmin
prince$tmax_prince <- prince$tmax

prince$tmin <- prince$tmax <- prince$GDH <- prince$Season <- NULL

#testcalcs_liv$gdd <- testcalcs_liv$GDH/24
#testcalcs_liv$climdata <- "livneh"
liv <- testcalcs_liv
liv$gdd_liv <- liv$GDH/24
liv$tmin_liv <- liv$tmin
liv$tmax_liv <- liv$tmax

liv$tmin <- liv$tmax <- liv$GDH <- liv$Season <- NULL

testall <- cbind(prince, liv$gdd_liv, liv$tmin_liv, liv$tmax_liv)
#testall <- dplyr::select(testall, -Season, -GDH)
names(testall) <- c("id", "year", "gdd_prince", "tmin_prince", "tmax_prince",
                    "gdd_liv", "tmin_liv", "tmax_liv")

testall$tmin_diff <- testall$tmin_prince - testall$tmin_liv
testall$tmax_diff <- testall$tmax_prince - testall$tmax_liv

library(ggplot2)
comparegdd <- ggplot(testall, aes(x=gdd_prince, y=gdd_liv)) + geom_point() +
  geom_abline(intercept=0, slope=1) + xlab("Princeton") + ylab("Livneh") + 
  theme_classic() + coord_cartesian(x=c(175, 1700), y=c(175, 1700)) + ggtitle("GDD")

comparetmin <- ggplot(testall, aes(x=tmin_prince, y=tmin_liv)) + geom_point() +
  geom_abline(intercept=0, slope=1) + xlab("Princeton") + ylab("Livneh") + 
  theme_classic() + coord_cartesian(x=c(-10, 30), y=c(-10, 30)) + 
  ggtitle("Tmin")

comparetmax <- ggplot(testall, aes(x=tmax_prince, y=tmax_liv)) + geom_point() +
  geom_abline(intercept=0, slope=1) + xlab("Princeton") + ylab("Livneh") + 
  theme_classic() + coord_cartesian(x=c(0, 50), y=c(0, 50)) + 
  ggtitle("Tmax")


quartz()
library(gridExtra)
compareall <- grid.arrange(comparegdd, comparetmin, comparetmax, ncol=3)

png("compare_gdd_tmin_tmax.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=8,
    height=5, units="in", res = 350 )
grid.arrange(compareall)
dev.off()

write.csv(testall, "chilling/princetontest/livnehvprinceton_fakecoords.csv", row.names=FALSE)


