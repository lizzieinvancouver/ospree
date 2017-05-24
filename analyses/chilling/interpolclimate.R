######################################################
# Interpolation
######################################################
# interpolate daily climate data to hourly, based on max min 
# Then use this to summarize chilling, using 3 metrics
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
  #Need to add chilling treatments to some experiments (i.e. when design is a warming experiment X degrees above ambient)
  #These include:skre08, pagter15 (in chilltemp column, says "ambient + XXC")
  if(length(grep("ambplus0.76",i))==1){xx$Tmin<-xx$Tmin+0.76;xx$Tmax<-xx$Tmax+0.76}# pagter15
  if(length(grep("ambplus4",i))==1){xx$Tmin<-xx$Tmin+4;xx$Tmax<-xx$Tmax+4}#skre08
        
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




write.csv(chillcalcs, "output/fieldchillcalcslatlong.csv", row.names=FALSE, eol="\r\n")

stop("Not an error, just stopping here to say we're now done interpolating climate and estimating the field chilling. A new file called fieldchillcalcslatlong.csv has been written to the output folder. Yay!")
