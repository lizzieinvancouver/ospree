#Code to calculate change in chilling using ChilR
#By Ailene
#12 December, 2018

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(chillR)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

#1. Create data frames of fake climate data
days<-31 #number of days of climate data
yr<-2018#year of climate data
JDay<-seq(1:days)#DOYs- in January in this case
Year<-rep(yr,length(JDay))
lat<-45.5#latitude

#2. Estimate chilling using Tmin and Tmax
#use min and max daily temp to create fake climate data frame for January 2018 
Tmin<- -5
Tmax<-10
minmaxdaily<-data.frame(Year,JDay,Tmin,Tmax)

#Convert daily tmin and tmax data to hourly data, using packaged functions in chillR
hrly<-stack_hourly_temps(latitude=lat,make_hourly_temps(lat, minmaxdaily, keep_sunrise_sunset = FALSE))$hourtemps

#Chilling calculations for lat and time period of interest (January 2018 in my example)
chillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) 

#3. Alternative: estimate chilling using one mean temperature

#use constant mean daily temp to create fake climate data frame for January 2018
Tmean<- 2.5
meandaily<-data.frame(JDay,Year,Tmean)
#convert mean daily temperature data to hourly data
hrly.temp =
    data.frame(
      Temp = c(rep(meandaily$Tmean, times = 24)),
      Year = c(rep(meandaily$Year, times = 24)),
      JDay = sort(c(rep(seq(1:length(JDay)), times = 24)))
      )

chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp1)]) 
