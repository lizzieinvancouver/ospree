### Attempt to make a source file to compare different approaches to ramped studies in our models
## Cat - 6 Dec 2018
# This is old method: taking the photoperiod at day of budburst rather than initial photoperiod, as is in
# bb_cleanmergeall.R

## Starting with photoperiod
#bb.noNA$photoperiod_day <-as.numeric(as.character(bb.noNA$photoperiod_day))

bb.noNA$photoperiod_day[which(bb.noNA$datasetID=="partanen98" & bb.noNA$other.treatment=="Photoperiod lengthening from 8h 40min")] <- 
 (bb.noNA$resp[which(bb.noNA$datasetID=="partanen98" & bb.noNA$other.treatment=="Photoperiod lengthening from 8h 40min")]*0.1667) + 
bb.noNA$photo[which(bb.noNA$datasetID=="partanen98" & bb.noNA$other.treatment=="Photoperiod lengthening from 8h 40min")]

# photoperiod shortening from 12h
bb.noNA$photoperiod_day[which(bb.noNA$datasetID=="partanen98" & bb.noNA$other.treatment=="Photoperiod shortening from 12 h")] <- 
 (bb.noNA$resp[which(bb.noNA$datasetID=="partanen98" & bb.noNA$other.treatment=="Photoperiod shortening from 12 h")]*0.1667) + 
bb.noNA$photo[which(bb.noNA$datasetID=="partanen98" & bb.noNA$other.treatment=="Photoperiod shortening from 12 h")]

# photoperiod lengthening from 6h
bb.noNA$photoperiod_day[which(bb.noNA$datasetID=="partanen98" & bb.noNA$other.treatment=="Photoperiod lengthening from 6h")] <- 
 (bb.noNA$resp[which(bb.noNA$datasetID=="partanen98" & bb.noNA$other.treatment=="Photoperiod lengthening from 6h")]*0.1667) + 6

if(TRUE){
  # photoperiod shortening from 16h
  bb.noNA$photoperiod_day[which(bb.noNA$datasetID=="partanen01" & bb.noNA$other.treatment=="Photoperiod shortening from 16h")] <- 
    (bb.noNA$resp[which(bb.noNA$datasetID=="partanen01" & bb.noNA$other.treatment=="Photoperiod shortening from 16h")]*0.1667) + 
    bb.noNA$photo[which(bb.noNA$datasetID=="partanen01" & bb.noNA$other.treatment=="Photoperiod shortening from 16h")]
  
  # photoperiod lengthening from 6h
  bb.noNA$photoperiod_day[which(bb.noNA$datasetID=="partanen01" & bb.noNA$other.treatment=="Photoperiod lengthening from 6h")] <- 
    (bb.noNA$resp[which(bb.noNA$datasetID=="partanen01" & bb.noNA$other.treatment=="Photoperiod lengthening from 6h")]*0.1667) + 6
}

if(TRUE){
  bas<-bb.noNA[(bb.noNA$datasetID=="basler12" | bb.noNA$datasetID== "basler14" & bb.noNA$other.treatment=="ramped_photoperiod"),]
  bas$photoperiod_day<-as.numeric(bas$photoperiod_day)
  bas$photoperiod_day<- ifelse(!is.na(bas$response.time), ((bas$response.time*0.06) + bas$photoperiod_day), bas$photoperiod_day)
  bas$photoperiod_night<-as.numeric(bas$photoperiod_night)
  bas$photoperiod_night<-24-bas$photoperiod_day
  bb.noNA$photoperiod_day[which(bb.noNA$datasetID=="basler12" | bb.noNA$datasetID== "basler14" & bb.noNA$other.treatment=="ramped_photoperiod")]<-bas$photoperiod_day
  bb.noNA$photoperiod_night[which(bb.noNA$datasetID=="basler12" | bb.noNA$datasetID== "basler14" & bb.noNA$other.treatment=="ramped_photoperiod")]<-bas$photoperiod_night
}


## And now forcing

# laube14a 
### Adjust Laube14a based on slightly complex calculations (see paper and bb.noNAcleaning_README.txt for more details)...
temp<-0.5
laube14<-data.frame(matrix(0, ncol = 1, nrow = 42), temp=temp)
laube14<-dplyr::select(laube14, temp)
laube14$temp <- ave(
  laube14$temp,
  FUN=function(x) cumsum(c(0.5, head(x, -1)))
)
laube14$temp<-laube14$temp+6.5
laube14$day<-1:42

laube14<-within(laube14, gdd <-cumsum(temp))

d.sub<-subset(bb.noNA, bb.noNA$datasetID=="laube14a")
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
bb.noNA$response.time[which(bb.noNA$datasetID=="laube14a")]<-d.sub$response.time
#bb.noNA$respvar.simple[which(bb.noNA$datasetID=="laube14a")]<- "daystobudburst"
#bb.noNA$respvar[which(bb.noNA$datasetID=="laube14a")]<- "degreedaystobudburstconvertedtodays"

d.sub$response.time<- as.numeric(d.sub$response.time)
for(i in c(1:nrow(d.sub))) {
 for(j in c(1:nrow(laube14)))
  d.sub$forcetemp[i]<-ifelse(d.sub$response.time[i] == laube14$day[j], laube14$temp[j], d.sub$forcetemp[i])
}
d.sub$forcetemp<-ifelse(is.na(d.sub$response.time), 27.5, d.sub$forcetemp)

d.sub$response.time<-ifelse(is.na(d.sub$response.time), "no response", d.sub$response.time)
bb.noNA$forcetemp[which(bb.noNA$datasetID=="laube14a")]<-d.sub$forcetemp


# basler12 - "Temperature was set to cycle ±5 K around the daily mean temperature, which was increased by
# 0.5 K every five days"
## Fixed 13 Apr 2018 - Cat
bb.noNA$response.time.num <-as.numeric(as.character(bb.noNA$response.time))
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=0 & bb.noNA$response.time.num<5, 5, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=5 & bb.noNA$response.time.num<10, 5.5, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=10 & bb.noNA$response.time.num<15, 6, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=15 & bb.noNA$response.time.num<20, 6.5, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=20 & bb.noNA$response.time.num<25, 7, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=25 & bb.noNA$response.time.num<30, 7.5, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=30 & bb.noNA$response.time.num<35, 8, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=35 & bb.noNA$response.time.num<40, 8.5, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=40 & bb.noNA$response.time.num<45, 9, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=45 & bb.noNA$response.time.num<50, 9.5, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=50 & bb.noNA$response.time.num<55, 10, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=55 & bb.noNA$response.time.num<60, 10.5, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=60 & bb.noNA$response.time.num<65, 11, bb.noNA$forcetemp)
bb.noNA$forcetemp<-ifelse(bb.noNA$datasetID=="basler12" & bb.noNA$response.time.num>=65 & bb.noNA$response.time.num<70, 11.5, bb.noNA$forcetemp)

#bb.noNA$photoperiod_day<-bb.noNA$photoperiod_day
bb.noNA$response.time.num <- NULL
#bb.noNA$photo.num <- NULL

