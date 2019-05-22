# This code uses previously pulled daily temperature from EOBs (on climate drive) for PEP sites
# to calculate chilling and and forcing temperature during periods of interest
# It uses climate data from every grid cell for which Leaf out data were pulled from PEPs for betula pendaul
# And writes new files of annual chilling based on historica data and with different amounts of warming
# e.g. 1-7 degrees of warming
# The forecasts are then fed into our model to create figures in forecast_changebb.R
# 13 May 2019: This code created by modifying pull_daily_temp_for_forecasting.R 
# to better match Cat's selection of years  (1950-1960 for pre-warmed period)
# This uses 1951-1961 (climate data only goes back to 1951)

# By Ailene Ettinger, ailene.ettinger@gmail.com
# This code assumes that the climate data has already been pulled and summarized in temp_forforecast...
# This code calculates chilling with different amounts of warming

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/Github/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")

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

endyr<-1961
stday <- strptime(paste(styr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
endday <- strptime(paste(endyr, "12-30", sep="-"),"%Y-%m-%d", tz = "GMT")

#3) For chilling choose september 1 as a start date, April 30 as an end date
chillstmon <- 9
chillendmon <- 2#used 4 (April) before. Cat said she used Sept 1-Mar 1 for chilling
minwarm<-1#add this amt of warming to temp
maxwarm<-7
#4) select species, identify latlon of their ranges to pull climte data
sp<-c("betpen","fagsyl")#pep_betpen_all, pep_fagsyl_all.csv
#i=1
for(i in 1:length(sp)){
 spfilename<-paste("bb_analysis/PEP_climate/input/pep_",sp[i],"_all.csv", sep="")
 spdat<-read.csv(spfilename, header=T)
#head(spdat)
  spdat<-spdat[spdat$YEAR<1961,] 
  spdat$lat.lon<-paste(spdat$LAT,spdat$LON,sep=".")
  #dim(spdat)
  latlon <- spdat %>% # start with the data frame
    distinct(lat.lon, .keep_all = TRUE) %>% # establishing grouping variables
    dplyr::select(LAT,LON)
  latlon <- latlon[apply(latlon, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
  #formerly chose 50 random rows from latlon and pull climate from these:
  #latlonsubs<-sample_n(latlon, 50)
  for(l in  2965:dim(latlon)[1]){#left off at l=135. 
    la<- latlon$LAT[l] 
    lo<- latlon$LON[l] 
    name<-paste("../../../../../..",climatedrive,"/",sp[i],"/temp_forforecast","_","_",la,"_",lo,"_",styr,"_2014.csv",sep="")
    #write out the daily temperature file, in case its useful later
    temp<-read.csv(name, header=TRUE)
    if(length(unique(temp$Tmean))==1){next}
    #temp<-read.csv("output/dailyclim/temp_observed_48.16447_11.50293_1979_2014.csv", header=TRUE)
    #get mean annual temp
    temp<-temp[temp$Year<=(endyr+1),]
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
    
    #trim off the incomplete years (first year of data:
    chillcalcs<-chillcalcs[-1,]
    chillcalcs<-chillcalcs[-dim(chillcalcs)[1],]
    chillall<-join(chillcalcs,macht)
    
    name_chillall<-paste("../../../../../..",climatedrive,"/",sp[i],"/chill_observed_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
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
      name_chillallfut<-paste("../../../../../..",climatedrive,"/",sp[i],"/chillforecast",tempfor[t],"deg","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
      write.csv(chillfut, name_chillallfut, row.names = FALSE)
    }
  }
}



