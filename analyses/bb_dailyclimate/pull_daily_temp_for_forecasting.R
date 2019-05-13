# This code pulls daily temperature from EOBs (on climate drive) for PEP sites
# and calculates chilling and and forcing temperature during periods of interest
# It pulls climate data from every grid cell for which Leaf out data were pulled from PEPs for betula pendaul
# And writes new files of annual chilling based on historica data and with different amounts of warming
# e.g. 1-7 degrees of warming
# After this code was used to pull the climate data and write the original files, 
# we decided to use a different window for chilling (sept1 - mar 1, instead of may1, as we did in this code)
# the code with this different window is calculated in daily_temp_for_forecastin.R

# By Ailene Ettinger, ailene.ettinger@gmail.com
# modified 13 May 2019

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/Github/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")

library(ncdf4)
library(Interpol.T)
library(chillR)
library(plyr)
library(dplyr)
#1) #Set the location of the external hard drive, then pull daily climate data for Europe and North America
#climatedrive = "/Volumes/Ospree Climate" # (Ospree Climate is name of the external drive, change with new device)
# climatedrive =  "/Volumes/BackYouEvilFiend/ospreeclimate" # Lizzie's backup drive (at HUH currently)
# climatedrive = "/Volumes/My Book for Mac/ospreeclimate" # Lizzie's backup drive (at WeldHill currently)
#climatedrive = "//128.103.155.31/WeldShare/Wolkovich Lab/Budburst Review - Ospree/Climate Data" # Access to the data from Weld Share (From Nacho's computer)
climatedrive = "/Volumes/climate" #Ailene's climate data drive
#Select the lat/long(s) and years of climate data you'd like

#2) select years for which you want climate data
styr<-1951

endyr<-2014
stday <- strptime(paste(styr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
endday <- strptime(paste(endyr, "12-30", sep="-"),"%Y-%m-%d", tz = "GMT")

#3) For chilling choose september 1 as a start date, April 30 as an end date
chillstmon <- 9
chillendmon <- 4
minwarm<-1
maxwarm<-7
#4) select species, identify latlon of their ranges to pull climte data
sp<-c("betpen","fagsyl")#pep_betpen_all, pep_fagsyl_all.csv
#i=1
for(i in 1:length(sp)){
 spfilename<-paste("bb_analysis/PEP_climate/input/pep_",sp[i],"_all.csv", sep="")
 spdat<-read.csv(spfilename, header=T)
#head(spdat)
  spdat<-spdat[spdat$YEAR<1980,] 
  spdat$lat.lon<-paste(spdat$LAT,spdat$LON,sep=".")
  #dim(spdat)
  latlon <- spdat %>% # start with the data frame
    distinct(lat.lon, .keep_all = TRUE) %>% # establishing grouping variables
    dplyr::select(LAT,LON)
  latlon <- latlon[apply(latlon, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
  #choose 50 random rows from latlon and pull climate from these:
  #latlonsubs<-sample_n(latlon, 50)
  for(l in 1:dim(latlon)[1]){
    la<- latlon$LAT[l] 
    lo<- latlon$LON[l] 
    
    eur.tempmn <- nc_open(file.path(climatedrive, "tn_0.25deg_reg_v16.0.nc"))
    eur.tempmx <- nc_open(file.path(climatedrive, "tx_0.25deg_reg_v16.0.nc"))
    
    #code to get daily climate data for focal lat/long
    ndiff.long.cell <- abs(eur.tempmn$dim$longitude$vals-as.numeric(lo))
    ndiff.lat.cell <- abs(eur.tempmn$dim$latitude$vals-as.numeric(la))
    nlong.cell <- which(ndiff.long.cell==min(ndiff.long.cell))[1] 
    nlat.cell <- which(ndiff.lat.cell==min(ndiff.lat.cell))[1]
    
    xdiff.long.cell <- abs(eur.tempmx$dim$longitude$vals-as.numeric(lo))
    xdiff.lat.cell <- abs(eur.tempmx$dim$latitude$vals-as.numeric(la))
    xlong.cell <- which(xdiff.long.cell==min(xdiff.long.cell))[1]
    xlat.cell <- which(xdiff.lat.cell==min(xdiff.lat.cell))[1]
    
    #EOBS has dates in reference to 1950
    # start and end days of the climate data we need for the lat/long. This is in days since baseline date (sept 1) Set to GMT to avoid daylight savings insanity
    st <- as.numeric(as.character(stday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
    en <- as.numeric(as.character(endday - strptime("1950-01-01", "%Y-%m-%d", tz = "GMT")))
    if(en<st){en=st}
    if(endday<stday){endday=stday}
    
    
    min<-ncvar_get(eur.tempmn,'tn', 
                   start=c(nlong.cell,nlat.cell,st), 
                   count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
    ) 
    max<-ncvar_get(eur.tempmx,'tx', 
                   start=c(xlong.cell,xlat.cell,st), 
                   count=c(1,1,en-st+1) # warnings ok
    )
    temp<- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                      Tmin = min, Tmax = max)#
    temp$Tmean<-(temp$Tmin + temp$Tmax)/2
    temp$Date<-strptime(temp$Date,"%Y-%m-%d", tz="GMT")
    
    temp$Year<-as.numeric(format(temp$Date, "%Y"))
    temp$Month = as.numeric(format(temp$Date, "%m"))
    temp$ChillYear<-NA
    temp$ChillYear[temp$Month>chillstmon]<-temp$Year[temp$Month>chillstmon]+1
    temp$ChillYear[temp$Month<(chillendmon+1)]<-temp$Year[temp$Month<(chillendmon+1)]
    
    name<-paste("output/dailyclim/",sp[i],"/temp_forforecast","_","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
    #write out the daily temperature file, in case its useful later
    if(length(unique(temp$Tmean))==1){next}
    write.csv(temp,name, row.names = FALSE)
    # Read in netCDF files to pull climate data from europe
    # This requires you to work off of the external hard drive with climate data
    
    #If you want to read in the file to avoid connecting to climate drive
    #temp<-read.csv("output/dailyclim/temp_observed_48.16447_11.50293_1979_2014.csv", header=TRUE)
    #get mean annual temp
    mat<-aggregate(temp$Tmean,by=list(temp$Year),mean)
    colnames(mat)<-c("year","mat")
    years<-mat$year
    
    chilltemp<-temp[-which(is.na(temp$ChillYear)),]
    #chilltemp<-chilltemp[chilltemp$ChillYear>1979,]
    #chilltemp<-chilltemp[chilltemp$ChillYear<2015,]
    macht<-aggregate(chilltemp$Tmean,by=list(chilltemp$ChillYear),mean)
    colnames(macht)<-c("End_year","mntemp")
    
    ######################################################
    # Interpolation
    ######################################################
    # interpolate daily climate data to hourly, based on max and min 
    # Then use this to summarize chilling, using 3 metrics
    # Build a calibration table, here since we don't actually have hourly data, use best guess: with a minimum at 5am, max at 2pm,  
    
    calibration_l = list(
      Average = data.frame(time_min = rep(5, 12),
                           time_max = rep(14, 12),
                           time_suns = rep(17, 12),
                           C_m = rep(0.35, 12))
    )
    
    
    chillcalcs <- vector()
    
    for(t in years){ 
      xx <- chilltemp[chilltemp$Year==t,]
      xx$Date<-strptime(xx$Date,"%Y-%m-%d", tz="GMT")
      
      year = (format(xx$Date, "%Y"))
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
      #if(apply(hrly, 2, function(x) all(!is.na(x)))["Temp"]) {
      
      chillcalc <- chilling(hrly, as.numeric(hrly$JDay[1]), as.numeric(hrly$JDay[nrow(hrly)]))  
      #} else { chillcalc <- data.frame("Season"=NA,"End_year"=NA,"Chilling_Hours"=NA, "Utah_Model"=NA, "Chill_portions"=NA) }
      
      chillcalcs <- rbind(chillcalcs, data.frame(chillcalc[c("Season","End_year","Chilling_Hours","Utah_Model","Chill_portions")]))
    }
    
    #trim off the incomplete years:
    chillcalcs<-chillcalcs[-1,]
    chillcalcs<-chillcalcs[-dim(chillcalcs)[1],]
    chillall<-join(chillcalcs,macht)
    #Look at differences in temperature and chilling in a "Warm year" versus a "cool Year"
    #1985= a cool winter year and 1995= a warm year
    name_chillall<-paste("output/dailyclim/",sp[i],"/chill_observed_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
    #write out the daily temperature file, in case its useful later
    write.csv(chillall,name_chillall, row.names = FALSE)
    #Now add warming (1-7 degrees) to the dataset.
    #warmestyr<-chillall$End_year[chillall$mntemp==max(chillall$mntemp)]
    #warmesttemp<-chilltemp[chilltemp$ChillYear< warmestyr + 2 & chilltemp$ChillYear>warmestyr -2,]
    
    #add 1:7 degrees
    tempfor<-seq(from =minwarm, to=maxwarm, by=1)
    chillcalcforcs <- vector()
    for(t in 1:length(tempfor)){
      tempwarm<-chilltemp
      tempwarm$Tmin<-tempwarm$Tmin+tempfor[t]
      tempwarm$Tmax<-tempwarm$Tmax+tempfor[t]
      tempwarm$Tmean<-(tempwarm$Tmin + tempwarm$Tmax)/2
      
      #calculate mean temps
      machtwarm<-aggregate(tempwarm$Tmean,by=list(tempwarm$ChillYear),mean)
      colnames(machtwarm)<-c("End_year","mntemp")
      
      for(y in years){ 
        xx <- tempwarm[tempwarm$Year==y,]
        xx$Date<-strptime(xx$Date,"%Y-%m-%d", tz="GMT")
        
        year = (format(xx$Date, "%Y"))
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
        chillcalcforc <- chilling(hrly, as.numeric(hrly$JDay[1]), as.numeric(hrly$JDay[nrow(hrly)]))  
        chillcalcforcs <- rbind(chillcalcforcs, data.frame(chillcalcforc[c("Season","End_year","Chilling_Hours","Utah_Model","Chill_portions")]))
      } 
      chillcalcforcs<-chillcalcforcs[-1,]
      chillcalcforcs<-chillcalcforcs[-dim(chillcalcforcs)[1],]
      chillfut<-join(chillcalcforcs,machtwarm)
      
      print(mean(chillall$Chill_portions))
      print(paste(t,"degree warming", sep=""))
      print(mean(chillfut$Chill_portions))
      name_chillallfut<-paste("output/dailyclim/",sp[i],"/chillforecast",tempfor[t],"deg","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
      write.csv(chillfut, name_chillallfut, row.names = FALSE)
    }
  }
}



