### Attempt to make a source file to compare different approaches to ramped studies in our models
## Cat - 6 Dec 2018
# This is old method: taking the photoperiod at day of budburst rather than initial photoperiod, as is in
# bb_cleanmergeall.R
## BROKEN! Can be fixed if necessary

d$resp<-as.numeric(d$response.time)
d$photo<-as.numeric(d$photoperiod_day)

d$photoperiod_day[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 8h 40min")] <- 
 (d$resp[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 8h 40min")]*0.1667) + 
d$photo[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 8h 40min")]


# photoperiod shortening from 12h
d$photoperiod_day[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod shortening from 12 h")] <- 
 (d$resp[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod shortening from 12 h")]*0.1667) + 
d$photo[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod shortening from 12 h")]


# photoperiod lengthening from 6h
d$photoperiod_day[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 6h")] <- 
 (d$resp[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 6h")]*0.1667) + 6


if(TRUE){
  # photoperiod lengthening from 6h
  d$photoperiod_day[which(d$datasetID=="partanen98" & d$photoperiod_day=="ambient" & d$response.time==97
                          & d$other.treatment=="Photoperiod lengthening from 6h")] <- 22
  d$photoperiod_night[which(d$datasetID=="partanen98" & d$photoperiod_day==22 & d$response.time==97
                            & d$other.treatment=="Photoperiod lengthening from 6h")] <- 2
  
  d$photoperiod_day[which(d$datasetID=="partanen98" & d$photoperiod_day=="ambient" & d$response.time>=69 &
                            d$response.time<=73 & d$other.treatment=="Photoperiod lengthening from 6h")] <- 17
  d$photoperiod_night[which(d$datasetID=="partanen98" & d$photoperiod_day==17 & d$response.time>=69 &
                              d$response.time<=73 & d$other.treatment=="Photoperiod lengthening from 6h")] <- 7
  
  d$photoperiod_day[which(d$datasetID=="partanen98" & d$photoperiod_day=="ambient" & d$response.time==87
                          & d$other.treatment=="Photoperiod lengthening from 6h")] <- 20
  d$photoperiod_night[which(d$datasetID=="partanen98" & d$photoperiod_day==20 & d$response.time==87 
                            & d$other.treatment=="Photoperiod lengthening from 6h")] <- 4
  
  d$photoperiod_day[which(d$datasetID=="partanen98")]<-ifelse(as.numeric(d$photoperiod_day[which(d$datasetID=="partanen98")])>24, 24, d$photoperiod_day[which(d$datasetID=="partanen98")])
  d$photoperiod_night[which(d$datasetID=="partanen98")]<- 24-as.numeric(d$photoperiod_day[which(d$datasetID=="partanen98")])
}

if(TRUE){
  # photoperiod shortening from 16h
  d$photoperiod_day[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod shortening from 16h")] <- 
    (d$resp[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod shortening from 16h")]*0.1667) + 
    d$photo[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod shortening from 16h")]
  
  # photoperiod lengthening from 6h
  d$photoperiod_day[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod lengthening from 6h")] <- 
    (d$resp[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod lengthening from 6h")]*0.1667) + 6
}

if(TRUE){
  bas<-d[(d$datasetID=="basler12" | d$datasetID== "basler14" & d$other.treatment=="ramped_photoperiod"),]
  bas$photoperiod_day<-as.numeric(bas$photoperiod_day)
  bas$photoperiod_day<- ifelse(!is.na(bas$response.time), ((bas$response.time*0.06) + bas$photoperiod_day), bas$photoperiod_day)
  bas$photoperiod_night<-as.numeric(bas$photoperiod_night)
  bas$photoperiod_night<-24-bas$photoperiod_day
  d$photoperiod_day[which(d$datasetID=="basler12" | d$datasetID== "basler14" & d$other.treatment=="ramped_photoperiod")]<-bas$photoperiod_day
  d$photoperiod_night[which(d$datasetID=="basler12" | d$datasetID== "basler14" & d$other.treatment=="ramped_photoperiod")]<-bas$photoperiod_night
}

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

# laube14a 
d.sub$response.time<- as.numeric(d.sub$response.time)
for(i in c(1:nrow(d.sub))) {
 for(j in c(1:nrow(laube14)))
  d.sub$forcetemp[i]<-ifelse(d.sub$response.time[i] == laube14$day[j], laube14$temp[j], d.sub$forcetemp[i])
}
d.sub$forcetemp<-ifelse(is.na(d.sub$response.time), 27.5, d.sub$forcetemp)

d.sub$response.time<-ifelse(is.na(d.sub$response.time), "no response", d.sub$response.time)
d$forcetemp[which(d$datasetID=="laube14a")]<-d.sub$forcetemp

# basler12 - "Temperature was set to cycle ±5 K around the daily mean temperature, which was increased by
# 0.5 K every five days"
## Fixed 13 Apr 2018 - Cat
d$response.time.num <-as.numeric(as.character(d$response.time))
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=0 & d$response.time.num<5, 5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=5 & d$response.time.num<10, 5.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=10 & d$response.time.num<15, 6, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=15 & d$response.time.num<20, 6.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=20 & d$response.time.num<25, 7, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=25 & d$response.time.num<30, 7.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=30 & d$response.time.num<35, 8, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=35 & d$response.time.num<40, 8.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=40 & d$response.time.num<45, 9, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=45 & d$response.time.num<50, 9.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=50 & d$response.time.num<55, 10, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=55 & d$response.time.num<60, 10.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=60 & d$response.time.num<65, 11, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$force_type=="ramped" & d$response.time.num>=65 & d$response.time.num<70, 11.5, d$forcetemp)



