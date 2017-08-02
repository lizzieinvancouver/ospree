# Cleaning thermaltime studies, converting to daystobudburst
# Cat - 6 March 2017 
# Cat - reevaluated 22 March 2017
# Using ospree_clean.csv right now, should use new file from Nacho's clean merge all file for bb_analysis/cleaning


# Clear Workspace
#rm(list=ls()) 
#options(stringsAsFactors=FALSE)

# Load libraries
#library(dplyr)
#library(tidyr)

## read data
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
#if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
#} else 
#  setwd("~/Documents/git/ospree/analyses")
#d<-read.csv("output/ospree_clean_withchill.csv",as.is=TRUE)

if(is.data.frame(d)){

## Can change ghelardini10 & heide93 from thermaltime to daystobudburst
# Add note for page number for thermal time conversion equation
# d<-within(d, respvar.simple[datasetID=="ghelardini10" & respvar.simple=="thermaltime"]<-"daystobudburst")
# d<-within(d, respvar.simple[datasetID=="heide93"]<-"daystobudburst")

#dtt<-subset(d,respvar.simple=="thermaltime") #basler12, ghelardini10, heide93, karlsson03,
#laube14a, skuterud94
# Both thermal time ("degreedaystobudburst") and daystobudburst were entered for basler12: this should be fixed in multiresponse
# Both thermal time and percentbudburst were entered for ghelardini10- this should be fixed in multiresponse
# Cannot figure out laube14a (does not seem to give equation for 'median forcing requirements' at all, if we did get it would need crafty approach to deal with ramping of temperatures) or skuterud94

######## Changes made my Cat - 26 May 2017 #######################
# heide93 is already converted from percentbudburst so thermaltime does not to be converted or else would result in duplicated results!
## If thermaltime is needed to be used in the future...
# On pg 533, 2nd paragraph, equation can be found. Use 0 degC as base temp so to convert
# from degree days to days, simply divide the degree days by the forcetemp
# daystobudburst = response/forcetemp

## Can change karlsson03 from thermaltime to daystobudburst
# On pg 620, Fig 1 caption, equation can be found. Use 2 degC as base temp
# from degree days to days, simply divide the degree days by the forcetemp - 2degC
# daystobudburst = response.time/(forcetemp-2)
d$response.time[which(d$datasetID=="karlsson03")] <-
  as.numeric(d$response.time[which(d$datasetID=="karlsson03")])/(
    as.numeric(d$forcetemp[which(d$datasetID=="karlsson03")]) - 2)
d$response[which(d$datasetID=="karlsson03" & d$respvar.simple=="thermaltime")] <- "timeonly"
d<-within(d, respvar.simple[datasetID=="karlsson03"]<-"daystobudburst")

### Laube14a attempt...
temp<-0.5
laube14<-data.frame(matrix(0, ncol = 1, nrow = 42), temp=temp)
laube14<-dplyr::select(laube14, temp)
laube14$temp <- ave(
  laube14$temp,
  FUN=function(x) cumsum(c(0.5, head(x, -1)))
)
laube14$temp<-laube14$temp+6.5
laube14$day<-1:42
laube14$night<-laube14$temp
laube14$night<-ifelse(laube14$day>14 & laube14$day<=21, (laube14$temp-2), laube14$temp)
laube14$night<-ifelse(laube14$day>21 & laube14$day<=27, (laube14$temp-3), laube14$night)
laube14$night<-ifelse(laube14$day>27 & laube14$day<=34, (laube14$temp-4), laube14$night)
laube14$night<-ifelse(laube14$day>41 & laube14$day<=48, (laube14$temp-5), laube14$night)

laube14$hours<-(laube14$temp*8 + laube14$night*16)/24
laube14$hours.12<-(laube14$temp*12 + laube14$night*12)/24
laube14$hours.16<-(laube14$temp*16 + laube14$night*8)/24
laube14<-within(laube14, gdd <-cumsum(hours))
laube14<-within(laube14, gdd.12<-cumsum(hours.12))
laube14<-within(laube14, gdd.16<-cumsum(hours.16))

df<-subset(d, datasetID=="laube14a" & photoperiod_day==8)
df$response.time<-ifelse(df$response.time==laube14$gdd, laube14$day, df$response.time)
resps<-unique(df$response.time)
sort(resps)


} else {
  print("Error: d is not a data.frame")
}

stop("Not an error, thermal time is passed to days now")



