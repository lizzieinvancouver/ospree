#Replacing Lizzie's simple "forecasts" of future climate
# in models_stan_plotting_APC with more rigorous estimates of current and future chilling and forcing
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
#The original code says:
# Let's pretend that average spring temp is 5 and average chill portions is 140
# And, let's pretend 2 deg warming decreases chilling by 30 portions (I made this up)
#sprtemp.forplot <- 5
#chillport.forplot <- 140
#photo.forplot <- 14
#warmspring <- 2
#warmwinter <- -30

#instead, we will estimate chilling and forcing from a location in europe, via EOBS
#Set the location of the external hard drive, then pull daily climate data for Europe and North America
#Skip ahead to 4e if you do not have the climate data drive
#climatedrive = "/Volumes/Ospree Climate" # (Ospree Climate is name of the external drive, change with new device)
# climatedrive =  "/Volumes/BackYouEvilFiend/ospreeclimate" # Lizzie's backup drive (at HUH currently)
# climatedrive = "/Volumes/My Book for Mac/ospreeclimate" # Lizzie's backup drive (at WeldHill currently)
#climatedrive = "//128.103.155.31/WeldShare/Wolkovich Lab/Budburst Review - Ospree/Climate Data" # Access to the data from Weld Share (From Nacho's computer)
climatedrive = "/Volumes/climate" #Ailene's climate data drive
#Select the lat/long(s) and years of climate data you'd like
la<- 48.16447 #zohner16 lat chosen for now
lo<- 11.50293 #zohner16 long chosen for now
#select years for which you want climate data
styr<-1979
endyr<-2014
stday <- strptime(paste(styr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
endday <- strptime(paste(endyr, "12-30", sep="-"),"%Y-%m-%d", tz = "GMT")

#For chilling choose september 1 as a start date, April 30 as an end date
chillstmon <- 9
chillendmon <- 4


# Read in netCDF files to pull climate data from europe
# This requires you to work off of the external hard drive with climate data

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

name<-paste("output/dailyclim/temp_forforecast","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
#write out the daily temperature file, in case its useful later
write.csv(temp,name, row.names = FALSE)

#If you need to read in the file to avoid connecting to climate drive
#temp<-read.csv("../output/dailyclim/temp_forforecast_48.16447_11.50293_1979_2014.csv", header=TRUE)
#get mean annual temp
mat<-aggregate(temp$Tmean,by=list(temp$Year),mean)
colnames(mat)<-c("year","mat")
chilltemp<-temp[-which(is.na(temp$ChillYear)),]
#chilltemp<-chilltemp[chilltemp$ChillYear>1979,]
#chilltemp<-chilltemp[chilltemp$ChillYear<2015,]
macht<-aggregate(chilltemp$Tmean,by=list(chilltemp$ChillYear),mean)
colnames(macht)<-c("End_year","mntemp")

years<-mat$year
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

for(i in years){ 
  xx <- chilltemp[chilltemp$Year==i,]
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
name_chillall<-paste("output/dailyclim/chill_forforecast","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
#write out the daily temperature file, in case its useful later
write.csv(chillall,name_chillall, row.names = FALSE)
#Now add 2 and 4 degrees to the dataset.
#warmestyr<-chillall$End_year[chillall$mntemp==max(chillall$mntemp)]
#warmesttemp<-chilltemp[chilltemp$ChillYear< warmestyr + 2 & chilltemp$ChillYear>warmestyr -2,]

#add 2 degrees
temp2warm<-chilltemp
temp2warm$Tmin<-temp2warm$Tmin+2
temp2warm$Tmax<-temp2warm$Tmax+2
#add 4 degrees
temp4warm<-chilltemp
temp4warm$Tmin<-temp2warm$Tmin+4
temp4warm$Tmax<-temp2warm$Tmax+4
#calculate mean temps
macht2<-aggregate(temp2warm$Tmean,by=list(temp2warm$ChillYear),mean)
colnames(macht2)<-c("End_year","mntemp")
macht4<-aggregate(temp4warm$Tmean,by=list(temp4warm$ChillYear),mean)
colnames(macht4)<-c("End_year","mntemp")


chillcalcs2 <- vector()
for(i in years){ 
xx <- temp2warm[temp2warm$Year==i,]
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
  chillcalc2 <- chilling(hrly, as.numeric(hrly$JDay[1]), as.numeric(hrly$JDay[nrow(hrly)]))  
  chillcalcs2 <- rbind(chillcalcs2, data.frame(chillcalc2[c("Season","End_year","Chilling_Hours","Utah_Model","Chill_portions")]))
  }
#trim off the incomplete years:
chillcalcs2<-chillcalcs2[-1,]
chillcalcs2<-chillcalcs2[-dim(chillcalcs2)[1],]
chillfut2<-join(chillcalcs2,macht2)
#write out the future chilling file
name_chillall2<-paste("output/dailyclim/chill_forecast2deg","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
write.csv(chillfut2,name_chillall2, row.names = FALSE)

#now 4-degrees of warming
chillcalcs4 <- vector()
for(i in years){ 
  xx <- temp4warm[temp4warm$Year==i,]
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
  chillcalc4 <- chilling(hrly, as.numeric(hrly$JDay[1]), as.numeric(hrly$JDay[nrow(hrly)]))  
  chillcalcs4 <- rbind(chillcalcs4, data.frame(chillcalc4[c("Season","End_year","Chilling_Hours","Utah_Model","Chill_portions")]))
}
#trim off the incomplete years:
chillcalcs4<-chillcalcs4[-1,]
chillcalcs4<-chillcalcs4[-dim(chillcalcs4)[1],]
chillfut4<-join(chillcalcs4,macht4)
#write out the future chilling file
name_chillall4<-paste("output/dailyclim/chill_forecast4deg","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
write.csv(chillfut4,name_chillall4, row.names = FALSE)

mean(chillall$Chill_portions)
mean(chillfut2$Chill_portions)#for this site, 2 degrees of wamring increases chilling slightly, on average
mean(chillfut4$Chill_portions)#4 degrees of warming yields reduced chilling

mean(chillall$Utah_Model)
mean(chillfut2$Utah_Model)#for this site, 2 degrees of wamring increases chilling, on average
mean(chillfut4$Utah_Model)#4 degrees of warming yields reduced chilling

range(chillall$Utah_Model)
range(chillfut2$Utah_Model)#for this site, 2 degrees of wamring increases chilling, on average
range(chillfut4$Utah_Model)
range(chillall$Chill_portions)
range(chillfut2$Chill_portions)#for this site, 2 degrees of wamring increases chilling slightly, on average
range(chillfut4$Chill_portions)#4 degrees of warming still yields higher chilling on average

