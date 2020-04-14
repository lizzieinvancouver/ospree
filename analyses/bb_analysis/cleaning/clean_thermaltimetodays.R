# Cleaning thermaltime studies, converting to daystobudburst
# Cat - 6 March 2017 
# Cat - reevaluated 22 March 2017
# Using ospree_clean.csv right now, should use new file from Nacho's clean merge all file for bb_analysis/cleaning
# Lizzie worked on updated data (27 Sep 2019) ... only new paper is vitra17
#Dan B added cleaning code for vitra17 on  11 Nov 2019 assuming base temperature in model is 0

if(is.data.frame(d)){
## vitra17
#We think vitra17 is a GDD model with base temp of 0. or maybe 5. Since we don't
  # know, we are going to split tge difference with 2.5 unless we hear otherwiseTherefore:
#GDD=(Temp-Tb)*Time so, daystobudburst = response.time/(forcetemp-2.5)
  d$response.time[which(d$datasetID=="vitra17")] <-
    as.numeric(d$response.time[which(d$datasetID=="vitra17")])/(
      as.numeric(d$forcetemp[which(d$datasetID=="vitra17")]) - 2.5)
  d$response[which(d$datasetID=="vitra17" & d$respvar.simple=="thermaltime")] <- "timeonly"
  d<-within(d, respvar.simple[datasetID=="vitra17"]<-"daystobudburst")

## Can change ghelardini10 & heide93 from thermaltime to daystobudburst
# You can figure out thermal time conversion equation from top of pg 267, left side
d.gher<-d
d.gher$response.time[which(d.gher$datasetID=="ghelardini10" & d.gher$respvar=="thermaltimetobudburst")] <-
    as.numeric(d.gher$response.time[which(d.gher$datasetID=="ghelardini10" & d.gher$respvar=="thermaltimetobudburst")])/
   (as.numeric(d.gher$forcetemp[which(d.gher$datasetID=="ghelardini10" & d.gher$respvar=="thermaltimetobudburst")]))
d$response.time[which(d.gher$datasetID=="ghelardini10" &
    d.gher$respvar=="thermaltimetobudburst")]<-d.gher$response.time[which(d.gher$datasetID=="ghelardini10" &
    d.gher$respvar=="thermaltimetobudburst")]
d<-within(d, respvar.simple[datasetID=="ghelardini10" & respvar.simple=="thermaltime"]<-"daystobudburst")
####check

# d<-within(d, respvar.simple[datasetID=="heide93"]<-"daystobudburst")

#dtt<-subset(d,respvar.simple=="thermaltime") #basler12, ghelardini10, heide93, karlsson03,
#laube14a, skuterud94
# Both thermal time ("degreedaystobudburst") and daystobudburst were entered for basler12: this should be fixed in multiresponse
# Both thermal time and percentbudburst were entered for ghelardini10- this should be fixed in multiresponse
# Cannot figure out skuterud94 - not enough information

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

### Adjust Laube14a based on slightly complex calculations (see paper and bbcleaning_README.txt for more details)...
temp<-0.5
laube14<-data.frame(matrix(0, ncol = 1, nrow = 42), temp=temp)
laube14<-dplyr::select(laube14, temp)
laube14$temp <- ave(
  laube14$temp,
  FUN=function(x) cumsum(c(0.5, head(x, -1)))
)
laube14$temp<-laube14$temp+6.5
laube14$day<-1:42
#laube14$night<-laube14$temp
#laube14$night<-ifelse(laube14$day>14 & laube14$day<=21, (laube14$temp-2), laube14$temp)
#laube14$night<-ifelse(laube14$day>21 & laube14$day<=27, (laube14$temp-3), laube14$night)
#laube14$night<-ifelse(laube14$day>27 & laube14$day<=34, (laube14$temp-4), laube14$night)
#laube14$night<-ifelse(laube14$day>41 & laube14$day<=48, (laube14$temp-5), laube14$night)

#laube14$eight<-(laube14$temp*8 + laube14$night*16)/24
#laube14$twelve<-(laube14$temp*12 + laube14$night*12)/24
#laube14$sixteen<-(laube14$temp*16 + laube14$night*8)/24
laube14<-within(laube14, gdd <-cumsum(temp))
#laube14<-within(laube14, gdd.12<-cumsum(hours.12))
#laube14<-within(laube14, gdd.16<-cumsum(hours.16))

d.sub<-subset(d, datasetID=="laube14a")
d.sub$response.time[which(d.sub$response.time==-23.76)]<-"no response"
d.sub$response.time<-ifelse(d.sub$response.time=="no response", 0, d.sub$response.time)


# Okay, we'll use a loop to match the extracted GDD values to days
# The way we do it, we'll need the upper and lower GDD range for each day, so build that here
laube14$upper <- NA
laube14$lower <- NA
laube14$lower[1] <- 1
laube14$upper[1] <- laube14$gdd[1]
laube14$lower[2:42] <- laube14$gdd[1:41]+0.001
laube14$upper[2:42] <- laube14$gdd[2:42]

    
# And here's the loop to assign a day to GDD 
d.sub$response.time<- as.numeric(d.sub$response.time)
for(i in c(1:nrow(d.sub))) {
  for(j in c(1:nrow(laube14)))
    if(d.sub$response.time[i] >= laube14$lower[j] & d.sub$response.time[i] <= laube14$upper[j])
      d.sub$response.time[i]<-laube14$day[j]
}


d.sub$response.time<-ifelse(d.sub$response.time==0, "no response", d.sub$response.time)
d$response.time[which(d$datasetID=="laube14a")]<-d.sub$response.time
d$respvar.simple[which(d$datasetID=="laube14a")]<- "daystobudburst"
d$respvar[which(d$datasetID=="laube14a")]<- "degreedaystobudburstconvertedtodays"

# Swartz81 - was converted from growing degree hours to growing degree days, now we need to 
# convert from growing degree days to days to budburst
d$response.time[which(d$datasetID=="swartz81")] <-
  as.numeric(d$response.time[which(d$datasetID=="swartz81")])/(
    as.numeric(d$forcetemp[which(d$datasetID=="swartz81")]) - 4.5)
#d$response[which(d$datasetID=="karlsson03" & d$respvar.simple=="thermaltime")] <- "timeonly"
d<-within(d, respvar.simple[datasetID=="swartz81"]<-"daystobudburst")


######## New data edits by Cat: 14 April 2020 ####
### man17: weinberger study with field forcing and also GH forcing at 15degC day and 5degC night. Day is 14 hours and night is 10
# one day in GH is then 15*14 + 10*5 = 260 gdh 
# field forcing doesn't start to accumulate until March 10 according to Figure 1
# scraped from figure 1: March 10 = 176; March 30 = 427; April 20 = 1685; May 11 = 3596; May 31 = 6663; Jun 20 = 8952 
## not sure where the extra dates are coming from...
noforcefieldsamps <- c("2014-10-01", "2014-10-11", "2014-10-21", "2014-10-31", "2014-11-10", "2014-11-20", "2014-11-30", 
                       "2015-01-09", "2015-01-29", "2015-02-18")

d$response.time[which(d$datasetID=="man17" & d$fieldsample.date2%in%noforcefieldsamps)] <-
  as.numeric(d$response.time[which(d$datasetID=="man17" & d$fieldsample.date2%in%noforcefieldsamps)])/(260)

d$response.time[which(d$datasetID=="man17" & d$fieldsample.date2=="2015-03-10")] <-
  (as.numeric(d$response.time[which(d$datasetID=="man17" & d$fieldsample.date2%in%noforcefieldsamps)])-176)/(260) ### IS THIS CORRECT??? SHOULD I SUBTRACT THE FIELD FORCING THIS WAY??

d$response.time[which(d$datasetID=="man17" & d$fieldsample.date2=="2015-03-30")] <-
  (as.numeric(d$response.time[which(d$datasetID=="man17" & d$fieldsample.date2%in%noforcefieldsamps)])-427)/(260)

#d$response.time[which(d$datasetID=="man17" & d$fieldsample.date2=="2015-03-30")] <-
 # (as.numeric(d$response.time[which(d$datasetID=="man17" & d$fieldsample.date2%in%noforcefieldsamps)])-6663)/(260) ### I'm not sure how to deal with the outdoor treatment...?
    

d<-within(d, respvar.simple[datasetID=="man17" & d$fieldsample.date2!="2015-05-31"]<-"daystobudburst")

} else {
  print("Error: d is not a data.frame")
}

###Dan B writes. I think I fixed where gheraldini10 was broken due to a response vs. response.time cleaning moment
###should be okay now. here is how I checked it.
#checker<-subset(d,d$datasetID=="ghelardini10" & d$respvar.simple=="daystobudburst")
#checker2<-subset(d.gher,d.gher$datasetID=="ghelardini10" & d.gher$respvar.simple=="thermaltime")
#checker$response.time==checker2$response.time


stop("Not an error, thermal time is passed to days now")



