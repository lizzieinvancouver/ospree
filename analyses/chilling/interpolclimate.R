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

for(i in names(tempval)){ #i="okie11_32.12_-83.12_2008-01-18_0"
  
  xx <- tempval[[i]]
  xx$Date<-strptime(xx$Date,"%Y-%m-%d", tz="GMT")
  
  ### Adding in code for random missing values from new North America climate data
  #xx <- xx[!is.na(xx$Tmin),]
  
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
    
    chillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) # 
  } else { chillcalc <- data.frame("Season"=NA,"End_year"=NA,"Chilling_Hours"=NA, "Utah_Model"=NA, "Chill_portions"=NA) }
  
  chillcalcs <- rbind(chillcalcs, data.frame(datasetIDlatlong = i,chillcalc[c("Season","End_year","Chilling_Hours","Utah_Model","Chill_portions")]))
}
#Check to see if any problems from "imput_chilling.R" are solved:
#head(chillcalcs)
#head(chillcalcs[chillcalcs$datasetIDlatlong=="heide93_59.5_10.767_1991-01-15_0",])
#head(chillcalcs[chillcalcs$datasetIDlatlong=="sonsteby14_60.701_10.872_2010-11-03_0",])
#head(chillcalcs[chillcalcs$datasetIDlatlong=="sonsteby14_60.701_10.872_2011-10-19_0",])
#head(chillcalcs[chillcalcs$datasetIDlatlong=="sonsteby14_60.701_10.872_2012-10-17_0",])
#head(chillcalcs[chillcalcs$datasetIDlatlong=="sonsteby14_60.701_10.872_2012-10-17_0",])
#head(chillcalcs[chillcalcs$datasetIDlatlong=="falusi96_42.05_14.02_1988-04-30_0",])

#sv

#save the field chilling calculations in a separate file
write.csv(chillcalcs, "output/fieldchillcalcslatlong.csv", row.names=FALSE, eol="\r\n")

stop("Not an error, just stopping here to say we're now done interpolating climate and estimating the field chilling. A new file called fieldchillcalcslatlong.csv has been written to the output folder. Yay!")
