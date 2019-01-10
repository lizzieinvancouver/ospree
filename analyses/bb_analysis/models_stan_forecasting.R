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
chillstday <- "09-01"
chillendday <- "4-30"


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
name<-paste("output/dailyclim/temp_forforecast","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
write.csv(temp,name, row.names = FALSE)
