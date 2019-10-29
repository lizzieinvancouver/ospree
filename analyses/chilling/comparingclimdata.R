## 29 October 2019 - by Cat
# Let's double check the new climate data
## Test to see if the sites have the same GDDs for summer to compare Livneh and Princeton datasets

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
# 1. Get the data
d <- read.csv("output/ospree_clean.csv")

source("chilling/cleaning_chilltemp.R")
source("chilling/fieldchillcalc_latlong.R")

climatedrive = "/Volumes/climdata" # Cat's climate drive

### But since Livneh only goes through 2013 need to remove some data first...
nam <- nam[(nam$year<=2013),]

nafiles <- dir(climatedrive)[grep("princetonclimdata", dir(climatedrive))]
#loop through each lat/long for which we want to calculate chilling and pull the climate data for that lat/long
#the climate data that we are pulling is daily min and max temperature

tempval_prince <- list() 
for(i in 1:nrow(nam)){ # i = 5
  # find this location
  lo <- nam[i,"chill.long"]
  la <- nam[i,"chill.lat"]
  
  # make sure longitudes are negative, need to be for North America this step is now done in "cleaning/clean_latlong" so it is no longer necessary
  #if(lo > 0) { lo = lo*-1 }
  yr<-as.numeric(substr(nam[i,"fieldsample.date2"],1,4))

  endday <- strptime(paste(yr, "08-30", sep="-"),"%Y-%m-%d", tz = "GMT")
  doyend <- yday(endday)
  stday <- strptime(paste(yr, "06-01", sep="-"),"%Y-%m-%d", tz="GMT")
  doyst <- yday(stday)
 
  mins <- maxs <- vector()
  
  ## for now exclude prevey18
  
  for(j in c(yr)) { # j = yr
    print(c(i, j))
    
    tmax <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmax",yr), full.names = TRUE)
    tmin <- list.files(path=paste(climatedrive,nafiles, sep="/"), pattern=paste0("tmin",yr), full.names = TRUE)
    jx <- nc_open(tmax)
    jn <- nc_open(tmin)
    
    yrend <- j
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
  
  tempval_prince[[as.character(nam[i,"ID_fieldsample.date2"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),  
                                                                       Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])
}
# If you want to (as Lizzie does) you can write out tempval, which is all the climate pulled in a list form
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
  #add interpolated climate data for studies with warming treatments (ambient plus 0.76, ambient plus 4 degrees)
  if(length(grep("ambplus0.76",i))==1){xx$Tmin<-xx$Tmin+0.76;xx$Tmax<-xx$Tmax+0.76}# pagter15
  if(length(grep("ambplus4",i))==1){xx$Tmin<-xx$Tmin+4;xx$Tmax<-xx$Tmax+4}#skre08
  if(length(grep("ambplus2.25",i))==1){xx$Tmin<-xx$Tmin+2.25;xx$Tmax<-xx$Tmax+2.25}
  if(length(grep("ambplus4.5",i))==1){xx$Tmin<-xx$Tmin+4.5;xx$Tmax<-xx$Tmax+4.5}
  if(length(grep("ambplus6.75",i))==1){xx$Tmin<-xx$Tmin+6.75;xx$Tmax<-xx$Tmax+6.75}
  if(length(grep("ambplus9",i))==1){xx$Tmin<-xx$Tmin+9;xx$Tmax<-xx$Tmax+9}
  
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
  
  testcalcs_prince <- rbind(testcalcs_prince, data.frame(datasetIDlatlong = i,testcalc[c("Season","End_year","GDH")]))
}

#write.csv(testcalcs_prince, "output/fieldchillcalcslatlong_princetest.csv", row.names=FALSE, eol="\r\n")


#####################################################
######## I think end of Princeton test... ###########
#####################################################
nafiles <- dir(climatedrive)[grep("livnehclimdata", dir(climatedrive))]

tempval_liv <- list() 
for(i in 1:nrow(nam)){ # i = 102
  # find this location
  lo <- nam[i,"chill.long"]
  la <- nam[i,"chill.lat"]
  
  # make sure longitudes are negative, need to be for North America this step is now done in "cleaning/clean_latlong" so it is no longer necessary
  #if(lo > 0) { lo = lo*-1 }
  yr<-as.numeric(substr(nam[i,"fieldsample.date2"],1,4))
  
  endday <- strptime(paste(yr, "08-30", sep="-"),"%Y-%m-%d", tz = "GMT")
  doyend <- yday(endday)
  stday <- strptime(paste(yr, "06-01", sep="-"),"%Y-%m-%d", tz="GMT")

  chillmo<-paste(yr, formatC(6:substr(endday,6,7), width=2, flag="0"), sep="")
 
  mins <- maxs <- vector()
  
  for(j in c(chillmo)){ # j = "198306"
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
  
  tempval_liv[[as.character(nam[i,"ID_fieldsample.date2"])]] <- data.frame(Lat = la,Long = lo,Date = as.character(seq(stday, endday, by = "day")),  
                                                                       Tmin = mins[1:length(seq(stday, endday, by = "day"))], Tmax =maxs[1:length(seq(stday, endday, by = "day"))])
}
# If you want to (as Lizzie does) you can write out tempval, which is all the climate pulled in a list form
#save(tempval, file="output/fieldclimate.RData")

testcalcs_liv <- vector()

for(i in names(tempval_liv)){ 
  
  xx <- tempval_liv[[i]]
  xx$Date<-strptime(xx$Date,"%Y-%m-%d", tz="GMT")
  #add interpolated climate data for studies with warming treatments (ambient plus 0.76, ambient plus 4 degrees)
  if(length(grep("ambplus0.76",i))==1){xx$Tmin<-xx$Tmin+0.76;xx$Tmax<-xx$Tmax+0.76}# pagter15
  if(length(grep("ambplus4",i))==1){xx$Tmin<-xx$Tmin+4;xx$Tmax<-xx$Tmax+4}#skre08
  if(length(grep("ambplus2.25",i))==1){xx$Tmin<-xx$Tmin+2.25;xx$Tmax<-xx$Tmax+2.25}
  if(length(grep("ambplus4.5",i))==1){xx$Tmin<-xx$Tmin+4.5;xx$Tmax<-xx$Tmax+4.5}
  if(length(grep("ambplus6.75",i))==1){xx$Tmin<-xx$Tmin+6.75;xx$Tmax<-xx$Tmax+6.75}
  if(length(grep("ambplus9",i))==1){xx$Tmin<-xx$Tmin+9;xx$Tmax<-xx$Tmax+9}
  
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
  
  testcalcs_liv <- rbind(testcalcs_liv, data.frame(datasetIDlatlong = i,testcalc[c("Season","End_year","GDH")]))
}

#write.csv(testcalcs_prince, "output/fieldchillcalcslatlong_princetest.csv", row.names=FALSE, eol="\r\n")

###### Now let's combine them all and check... 
#prince$gdd <- testcalcs_prince$GDH/24
#testcalcs_prince$climdata <- "prince"
prince <- testcalcs_prince
prince$gdd_prince <- testcalcs_prince$GDH/24

#testcalcs_liv$gdd <- testcalcs_liv$GDH/24
#testcalcs_liv$climdata <- "livneh"
liv <- testcalcs_liv
liv$gdd_liv <- testcalcs_liv$GDH/24

testall <- cbind(prince, liv$gdd_liv)
testall <- dplyr::select(testall, -Season, -GDH)
names(testall) <- c("datasetIDlatlong", "year", "gdd_prince", "gdd_liv")

library(ggplot2)
compareplot <- ggplot(testall, aes(x=gdd_prince, y=gdd_liv)) + geom_point() +
  geom_abline(intercept=0, slope=1) + xlab("Princeton") + ylab("Livneh") + 
  theme_classic() + coord_cartesian(x=c(175, 1700), y=c(175, 1700))

png("chilling/princetontest/livneh_vs_princeton.png", 
    width=4,
    height=4, units="in", res = 350 )
plot(compareplot)
dev.off()

write.csv(testall, "chilling/princetontest/livneh_vs_princeton.csv", row.names=FALSE)

