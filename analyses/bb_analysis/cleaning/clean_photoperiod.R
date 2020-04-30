## 22 June 2017 - Cat
## Checking where lost photoperiod data is going
## 10 July 2017 checked by Dan

# Load from bb_cleanmergeall.R (including library(geosphere) which happens there)

#### Updated 12 Oct 2018 by Cat ###
# Adding column for photoperiod type: thought is that ambient photoperiod is confounding with forcing or chilling temps.
# We want to know if using ambient photoperiod treatments changes the model estimates due to collinearity issues rather than 
# due to increasing sample size
### We will have four types: exp (experimental), amb (ambient), ramped (ramped photoperiod growth chamber controlled), none ( no info)

if(is.data.frame(d)){

d$photo_type<-NA
d$photo_type<-ifelse(d$photoperiod_day=="ambient" | d$photoperiod_day=="ambient-2", "amb", d$photo_type)
d$photo_type<-ifelse(d$photoperiod_day==""| d$photoperiod_day==" ", "none", d$photo_type) # treated as ambient in below code for imputation

# And away we go
amb<-d[which(d$photo_type=="amb"),]
unique(amb$datasetID)

phot_amb <- subset(d , photoperiod_day=="ambient" | photoperiod_night=="ambient")
#### Ailene'squery on 14 July 2017 returned this list
#[1] "chavarria09" "falusi96"    "guak98"      "jones12"     "rinne97"    
#[6] "sanzperez10" "cannell83"   "charrier11"  "fu13"        "gansert02"  
#[11] "gomory15"    "gunderson12" "hawkins12"   "lamb37"      "linkosalo06"
#[16] "morin10"     "partanen98"  "ruesink98"   "schnabel87"  "sonsteby13" 
#[21] "sonsteby14"  "yazdaniha64"
blank<-d[which(d$photoperiod_day==''),]
unique(blank$datasetID)
# "gianfagna85" "nishimoto95" "falusi96"

## charrier11: Table 1, Exp 1 - under long day conditions at 25 degC forcing

d$photoperiod_day[which(d$datasetID=="charrier11" & d$figure.table..if.applicable.== "table 1")] <- 16
d$photoperiod_night[which(d$datasetID=="charrier11" & d$figure.table..if.applicable.== "table 1")] <- 8

# fu13: "The experimental climate-controlled chambers were sunlit, facing south with a 
# transparent polycarbonate plate (4 mm thick) at the top (light absorption = 15% (De Boeck et al., 2006)).
# "winter 2009 to spring 2010 and winter 2010 to spring 2011".
fu10<-d[which(d$datasetID=="fu13" & d$year==2010),]
mean(fu10$response.time)
fu11<-d[which(d$datasetID=="fu13" & d$year==2011),]
mean(fu11$response.time)
geosphere::daylength(51.317, "2010-04-07") # -> 13
geosphere::daylength(51.317, "2011-04-10") # -> 13
d$photoperiod_day[which(d$datasetID=="fu13")] <- 13
d$photoperiod_night[which(d$datasetID=="fu13")] <- 11


# gansert02:
geosphere::daylength(35.3583 , "1998-04-29")
d$photoperiod_day[which(d$datasetID=="gansert02" & d$photoperiod_day=="ambient")] <- 14
d$photoperiod_night[which(d$datasetID=="gansert02" & d$photoperiod_day==14)] <- 10

# gomory15: "In spring 2011, budburst phenology was scored on each plant at approx. 
# two-week intervals between March 1 and June 29"
# Taking average (as we do elsewhere)...
low.initial<-geosphere::daylength(48.44820, "2011-03-01") # 10.9715
low.start<-geosphere::daylength(48.44820, "2011-04-14") # 13.56398
low.end<-geosphere::daylength(48.44820, "2011-04-26") # 14.23436
gom<-c(10.9715, 13.56398, 14.23436)
gom.photo<-mean(gom) #  13 hr photo

high.initial<-geosphere::daylength(49.01791, "2011-03-01") # 10.9490
high.start<-geosphere::daylength(49.01791, "2011-05-06") # 14.8131
high.end<-geosphere::daylength(49.01791, "2011-05-21") # 15.5071
gomz<-c(10.9490, 14.8131, 15.5071)
gom.high<-mean(gomz) #14 hr photo

d$photoperiod_day[which(d$datasetID=="gomory15" & d$growing.long==18.36271)] <- 13
d$photoperiod_night[which(d$datasetID=="gomory15" & d$growing.long==18.36271)] <- 11

d$photoperiod_day[which(d$datasetID=="gomory15" & d$forcetemp==4.9)] <- 14
d$photoperiod_night[which(d$datasetID=="gomory15" & d$forcetemp==4.9)] <- 10

# gunderson12: clear panels on growth chamber kept outside, use coordinates and day of year from figure 2 to calculate
oak.start<-geosphere::daylength(35.931428, "2002-03-01") # 11.3705
oak.end<-geosphere::daylength(35.931428, "2002-05-05") # 13.7482
gund<-c(11.3705, 13.7482)
gund.photo<-mean(gund) # 13 hr photo

d$photoperiod_day[which(d$datasetID=="gunderson12")] <- 13
d$photoperiod_night[which(d$datasetID=="gunderson12")] <- 11

#hawkins12
south.hawk<-geosphere::daylength(48.43, "1998-04-07") # 13.1584
central.hawk<-geosphere::daylength(50.68, "1998-04-27") # 14.4765
north.hawk<-geosphere::daylength(53.90, "1998-05-03") # 15.1800

d$photoperiod_day[which(d$datasetID=="hawkins12" & d$growing.lat==48.43)] <- 13
d$photoperiod_night[which(d$datasetID=="hawkins12" & d$growing.lat==48.43)] <- 11
d$photoperiod_day[which(d$datasetID=="hawkins12" & d$growing.lat==50.68)] <- 15
d$photoperiod_night[which(d$datasetID=="hawkins12" & d$growing.lat==50.68)] <- 9
d$photoperiod_day[which(d$datasetID=="hawkins12" & d$growing.lat==53.90)] <- 15
d$photoperiod_night[which(d$datasetID=="hawkins12" & d$growing.lat==53.90)] <- 9

# partanen98: can determine from Figure 2!
# before extracting data, photoperiod ranges from 8-11 hrs
# ambient treatment
d$photoperiod_day[which(d$datasetID=="partanen98" & d$photoperiod_day=="ambient" & d$response.time>=77 &
                          d$response.time<=87 & d$other.treatment=="Ambient photoperiod")] <- 10
d$photoperiod_night[which(d$datasetID=="partanen98" & d$photoperiod_day==10 & d$response.time>=77 &
                          d$response.time<=87 & d$other.treatment=="Ambient photoperiod")] <- 14

d$photoperiod_day[which(d$datasetID=="partanen98" & d$photoperiod_day=="ambient" & d$response.time>=69 &
                          d$response.time<=73 & d$other.treatment=="Ambient photoperiod")] <- 9
d$photoperiod_night[which(d$datasetID=="partanen98" & d$photoperiod_day==9 & d$response.time>=69 &
                          d$response.time<=73 & d$other.treatment=="Ambient photoperiod")] <- 15

d$photoperiod_day[which(d$datasetID=="partanen98" & d$photoperiod_day=="ambient" & d$response.time==97
                        & d$other.treatment=="Ambient photoperiod")] <- 11
d$photoperiod_night[which(d$datasetID=="partanen98" & d$photoperiod_day==11 & d$response.time==97
                          & d$other.treatment=="Ambient photoperiod")] <- 13

d$photoperiod_day[which(d$datasetID=="partanen98" & d$photoperiod_day=="ambient" & d$response.time==90
                        & d$other.treatment=="Ambient photoperiod")] <- 10
d$photoperiod_night[which(d$datasetID=="partanen98" & d$photoperiod_day==10 & d$response.time==90
                          & d$other.treatment=="Ambient photoperiod")] <- 14

d$photoperiod_day[which(d$datasetID=="partanen98" & d$photoperiod_day=="ambient" & d$response.time==63
                        & d$other.treatment=="Ambient photoperiod")] <- 8
d$photoperiod_night[which(d$datasetID=="partanen98" & d$photoperiod_day==8 & d$response.time==63
                          & d$other.treatment=="Ambient photoperiod")] <- 16

# photoperiod lengthening from 8h and 40 min
d$photo_type<-ifelse(d$datasetID=="partanen98" & (d$other.treatment=="Photoperiod lengthening from 8h 40min" |
                                                    d$other.treatment=="Photoperiod shortening from 12 h" |
                                                    d$other.treatment=="Photoperiod lengthening from 6h"), "ramped", d$photo_type)


d$resp<-as.numeric(d$response.time)
d$photo<-as.numeric(d$photoperiod_day)
#d$photoperiod_day[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 8h 40min")] <- 
 # (d$resp[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 8h 40min")]*0.1667) + 
  #d$photo[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 8h 40min")]

d$photoperiod_day<-ifelse(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 8h 40min", 8.67, d$photoperiod_day)

# photoperiod shortening from 12h
#d$photoperiod_day[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod shortening from 12 h")] <- 
 # (d$resp[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod shortening from 12 h")]*0.1667) + 
  #d$photo[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod shortening from 12 h")]

d$photoperiod_day<-ifelse(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod shortening from 12 h", 12, d$photoperiod_day)

# photoperiod lengthening from 6h
#d$photoperiod_day[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 6h")] <- 
 # (d$resp[which(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 6h")]*0.1667) + 6

d$photoperiod_day<-ifelse(d$datasetID=="partanen98" & d$other.treatment=="Photoperiod lengthening from 6h", 6, d$photoperiod_day)

if(FALSE){
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

## Partanen01 Fixes
d$photo_type<-ifelse(d$datasetID=="partanen01" & (d$other.treatment=="Photoperiod shortening from 16h" |
                                                    d$other.treatment=="Photoperiod lengthening from 6h"), "ramped", d$photo_type)

d$photoperiod_day<-ifelse(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod lengthening from 6h", 6, d$photoperiod_day)
d$photoperiod_day<-ifelse(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod shortening from 16h", 16, d$photoperiod_day)

if(FALSE){
# photoperiod shortening from 16h
d$photoperiod_day[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod shortening from 16h")] <- 
  (d$resp[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod shortening from 16h")]*0.1667) + 
  d$photo[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod shortening from 16h")]

# photoperiod lengthening from 6h
d$photoperiod_day[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod lengthening from 6h")] <- 
  (d$resp[which(d$datasetID=="partanen01" & d$other.treatment=="Photoperiod lengthening from 6h")]*0.1667) + 6
}

d$photoperiod_day[which(d$datasetID=="partanen01")]<-ifelse(as.numeric(d$photoperiod_day[which(d$datasetID=="partanen01")])>24, 24, d$photoperiod_day[which(d$datasetID=="partanen01")])
d$photoperiod_night[which(d$datasetID=="partanen01")]<- 24-as.numeric(d$photoperiod_day[which(d$datasetID=="partanen01")])


d<-dplyr::select(d, -photo, -resp)

# schnabel87: ambient entries are in field comparisons to experiment - we will keep these!
sch.1<-geosphere::daylength(46.206, "1984-10-11") # 11.1698
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="11-Oct-1984")] <- 11
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="11-Oct-1984")] <- 13
sch.2<-geosphere::daylength(46.206, "1984-10-25") # 10.4364
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="25-Oct-1984")] <- 10
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="25-Oct-1984")] <- 14
sch.3<-geosphere::daylength(46.206, "1984-11-8") # 9.7562
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="8-Nov-1984")] <- 10
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="8-Nov-1984")] <- 14
sch.4<-geosphere::daylength(46.206, "1984-11-22") # 9.1797
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="22-Nov-1984")] <- 9
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="22-Nov-1984")] <- 15
sch.5<-geosphere::daylength(46.206, "1984-12-06") # 8.7750
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="6-Dec-1984")] <- 9
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="6-Dec-1984")] <- 15
sch.6<-geosphere::daylength(46.206, "1984-12-20") # 8.6099
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="20-Dec-1984")] <- 9
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="20-Dec-1984")] <- 15
sch.7<-geosphere::daylength(46.206, "1985-1-3") # 8.7066
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="3-Jan-1985")] <- 9
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="3-Jan-1985")] <- 15
sch.8<-geosphere::daylength(46.206, "1985-1-17") # 9.0567
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="17-Jan-1985")] <- 9
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="117-Jan-1985")] <- 15
sch.9<-geosphere::daylength(46.206, "1985-1-31") # 9.6008
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="31-Jan-1985")] <- 10
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="31-Jan-1985")] <- 14
sch.10<-geosphere::daylength(46.206, "1985-2-14") # 10.2680
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="14-Feb-1985")] <- 10
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="14-Feb-1985")] <- 14
sch.11<-geosphere::daylength(46.206, "1985-2-28") # 11.0016
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="28-Feb-1985")] <- 11
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="28-Feb-1985")] <- 13
sch.12<-geosphere::daylength(46.206, "1985-3-14") # 11.7643
d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="14-Mar-1985")] <- 12
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_night=="ambient" & d$fieldsample.date=="14-Mar-1985")] <- 12

d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="14-9.5")] <- 14
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="14-9.5")] <- 10
d$photo_type[which(d$datasetID=="schnabel87" & d$photoperiod_day=="14-9.5")] <- "ramped"

d$photoperiod_day[which(d$datasetID=="schnabel87" & d$photoperiod_day=="13-9.5")] <- 13
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="13-9.5")] <- 11
d$photo_type[which(d$datasetID=="schnabel87" & d$photoperiod_day=="13-9.5")] <- "ramped"

# sonsteby14: Figure 5 has ambient, should be changed to 24 hour photoperiod
d$photoperiod_day[which(d$photoperiod_day=="ambient" & d$figure.table..if.applicable.=="fig 5" & d$datasetID=="sonsteby14")] <- 24
d$photoperiod_night[which(d$photoperiod_day==24 & d$figure.table..if.applicable.=="fig 5" & d$datasetID=="sonsteby14")] <- 0

# cannell83: not enough information - can't even assume ambient
## Can't fix!! There's no date of budburst - 30 October 2017 Cat
## Looking at bud length


# guak98: Should be able to calculate
guak.start<-geosphere::daylength(44.5659, "1996-2-17") # 10.5147
guak.end<-geosphere::daylength(44.5659, "1996-4-8") # 13.1222
guak<-c(10.5147,13.1222)
guak.photo<-mean(guak) # 12 hr photo

d$photoperiod_day[which(d$datasetID=="guak98")] <- 12
d$photoperiod_night[which(d$datasetID=="guak98")] <- 12

# yazdaniha64: 60 days and 100 days, latitude is 41.143
geosphere::daylength(41.143, "1964-02-24")
d$photoperiod_day[which(d$response.time==60 & d$datasetID=="yazdaniha64")] <- 11
d$photoperiod_night[which(d$response.time==60 & d$datasetID=="yazdaniha64")] <- 13

geosphere::daylength(41.143, "1964-04-06")
d$photoperiod_day[which(d$response.time==100 & d$datasetID=="yazdaniha64")] <- 13
d$photoperiod_night[which(d$response.time==100 & d$datasetID=="yazdaniha64")] <- 11



################################################################################
##################### New additions 30 October 2017 ###########################
# sanzperez10: uses ambient light but also uses shade cloth to manipulate percentage of sunlight... not sure if we can calculate
# percentage of light is 100%, 20%, or 5%, for the purposes of this study we will convert only the 100% light treatments
geosphere::daylength(40.47, "2004-01-24")
geosphere::daylength(40.47, "2004-04-10")
geosphere::daylength(40.47, "2004-06-26")
sanz<-c(9.830631, 13.06944, 15.05556)
mean(sanz)
d$photoperiod_day[which(d$photoperiod_day=="ambient" & d$datasetID=="sanzperez10")] <- 13 ## Changed all!! May want to change back to just 100% irradiance
## Fixed 30 October 2017 by Cat
d$photoperiod_night[which(d$photoperiod_day==13 & d$datasetID=="sanzperez10")] <- 11

# jones12: fixed 30 October 2017 by Cat
jones<-subset(d, d$datasetID=="jones12")
jones$date<-as.Date(jones$response.time, origin = "2008-01-01") ## date found from paper
for(i in c(1:nrow(jones))){
  jones$photoperiod_day[i] <- geosphere::daylength(jones$provenance.lat[i], jones$date[i])  ## uses daylength function from geosphere library to calculate photoperiod for each observation
}
jones$photoperiod_day<-as.numeric(jones$photoperiod_day) ## preparing to change photoperiod_night column, won't change in main dataframe!
jones$photoperiod_day<- round(jones$photoperiod_day, digits=0) # still preparing
jones$photoperiod_night<-24-jones$photoperiod_day ## and... here it is
jones$photoperiod_day<-as.character(jones$photoperiod_day) ## changing back so doesn't mess up other entries in main dataframe
jones$photoperiod_night<-as.character(jones$photoperiod_night)
d$photoperiod_day[which(d$datasetID=="jones12")]<-jones$photoperiod_day
d$photoperiod_night[which(d$datasetID=="jones12")]<-jones$photoperiod_night

# rinne97: fixed 30 October 2017 by Cat
rin<-subset(d, d$datasetID=="rinne97")
rin$date<-as.Date(rin$response.time, origin = "1995-01-15")
for(i in c(1:nrow(rin))){
  rin$photoperiod_day[i] <- geosphere::daylength(rin$provenance.lat[i], rin$date[i])
}
rin$photoperiod_day<-as.numeric(rin$photoperiod_day)
rin$photoperiod_day<- round(rin$photoperiod_day, digits=0)
rin$photoperiod_night<-24-rin$photoperiod_day
rin$photoperiod_day<-as.character(rin$photoperiod_day)
rin$photoperiod_night<-as.character(rin$photoperiod_night)
d$photoperiod_day[which(d$datasetID=="rinne97")]<-rin$photoperiod_day
d$photoperiod_night[which(d$datasetID=="rinne97")]<-rin$photoperiod_night

# chavarria09: fixed 30 October 2017 by Cat
chav<-subset(d, d$datasetID=="chavarria09")
chav$date<-as.Date(chav$response.time, origin = "2005-02-01")
for(i in c(1:nrow(chav))){
  chav$photoperiod_day[i] <- geosphere::daylength(chav$provenance.lat[i], chav$date[i])
}
chav$photoperiod_day<-as.numeric(chav$photoperiod_day)
chav$photoperiod_day<- round(chav$photoperiod_day, digits=0)
chav$photoperiod_night<-24-chav$photoperiod_day
chav$photoperiod_day<-as.character(chav$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="chavarria09")]<-chav$photoperiod_day
d$photoperiod_night[which(d$datasetID=="chavarria09")]<-chav$photoperiod_night

# linkosalo06: fixed 30 October 2017 by Cat
link<-subset(d, d$datasetID=="linkosalo06")
link$date<-as.Date(link$response.time, origin = "2003-01-01")
for(i in c(1:nrow(link))){
  link$photoperiod_day[i] <- geosphere::daylength(link$provenance.lat[i], link$date[i])
}
link$photoperiod_day<-as.numeric(link$photoperiod_day)
link$photoperiod_day<- round(link$photoperiod_day, digits=0)
link$photoperiod_night<-24-link$photoperiod_day
link$photoperiod_day<-as.character(link$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="linkosalo06")]<-link$photoperiod_day
d$photoperiod_night[which(d$datasetID=="linkosalo06")]<-link$photoperiod_night

# morin10: fixed 30 October 2017 by Cat
mo<-subset(d, d$datasetID=="morin10")
mo$date<-as.Date(mo$response.time, origin = "2004-01-01")
for(i in c(1:nrow(mo))){
  mo$photoperiod_day[i] <- geosphere::daylength(mo$provenance.lat[i], mo$date[i])
}
mo$photoperiod_day<-as.numeric(mo$photoperiod_day)
mo$photoperiod_day<- round(mo$photoperiod_day, digits=0)
mo$photoperiod_night<-24-mo$photoperiod_day
mo$photoperiod_day<-as.character(mo$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="morin10")]<-mo$photoperiod_day
d$photoperiod_night[which(d$datasetID=="morin10")]<-mo$photoperiod_night

# falusi96: fixed 30 October 2017 - Cat
fals<-subset(d, d$datasetID=="falusi96")
fals$date<-as.Date(fals$response.time, origin = "1988-03-01")
for(i in c(1:nrow(fals))){
  fals$photoperiod_day[i] <- ifelse(fals$study[i]=="exp1" | fals$study[i]=="exp2",
                                    (geosphere::daylength(fals$provenance.lat[i], fals$date[i])), fals$photoperiod_day[i])
}
fals$photoperiod_day<-as.numeric(fals$photoperiod_day)
fals$photoperiod_day<- round(fals$photoperiod_day, digits=0)
fals$photoperiod_night<-24-fals$photoperiod_day
fals$photoperiod_day<-as.character(fals$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="falusi96")]<-fals$photoperiod_day
d$photoperiod_night[which(d$datasetID=="falusi96")]<-fals$photoperiod_night

# gianfagna85: fixed 30 October 2017 - Cat
gian<-subset(d, d$datasetID=="gianfagna85")
gian$date<-as.Date(gian$response.time, origin = "1984-03-01")
for(i in c(1:nrow(gian))){
  gian$photoperiod_day[i] <- geosphere::daylength(gian$provenance.lat[i], gian$date[i])
}
gian$photoperiod_day<-as.numeric(gian$photoperiod_day)
gian$photoperiod_day<- round(gian$photoperiod_day, digits=0)
gian$photoperiod_night<-24-gian$photoperiod_day
gian$photoperiod_day<-as.character(gian$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="gianfagna85")]<-gian$photoperiod_day
d$photoperiod_night[which(d$datasetID=="gianfagna85")]<-gian$photoperiod_night
## Remaining missing rows are due to missing response.time values

# nishimoto95: fixed 30 October 2017 - Cat
nish<-subset(d, d$datasetID=="nishimoto95")
nish$date<-as.Date(nish$response.time, origin = "1989-03-30")
for(i in c(1:nrow(nish))){
  nish$photoperiod_day[i] <-geosphere::daylength(nish$provenance.lat[i], nish$date[i])
}
nish$photoperiod_day<-as.numeric(nish$photoperiod_day)
nish$photoperiod_day<- round(nish$photoperiod_day, digits=0)
nish$photoperiod_night<-24-nish$photoperiod_day
nish$photoperiod_day<-as.character(nish$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="nishimoto95")]<-nish$photoperiod_day
d$photoperiod_night[which(d$datasetID=="nishimoto95")]<-nish$photoperiod_night


# lamb37: not enough information - can't even assume ambient
## 30 October 2017: update - still cannot fix, need to impute - safest bet is 12 hours
d$photoperiod_day[which(d$datasetID=="lamb37")] <- 12

############ FLOWER DATA BUT FIXING IF NEEDED IN FUTURE! ######################

# ruesink98: is flower buds not leaf buds...
## Can't fix because there is no response.time column

# sonsteby13: is also on flower buds not leaf buds...
sons<-subset(d, d$datasetID=="sonsteby13")
sons$date<-as.Date(sons$response.time, origin = "2011-06-13")
for(i in c(1:nrow(sons))){
  sons$photoperiod_day[i] <- geosphere::daylength(sons$provenance.lat[i], sons$date[i])
}
sons$photoperiod_day<-as.numeric(sons$photoperiod_day)
sons$photoperiod_day<- round(sons$photoperiod_day, digits=0)
sons$photoperiod_night<-24-sons$photoperiod_day
sons$photoperiod_day<-as.character(sons$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="sonsteby13")]<-sons$photoperiod_day
d$photoperiod_night[which(d$datasetID=="sonsteby13")]<-sons$photoperiod_night

################# Checking missing data #########################
if(FALSE){
d.photo<-d
d.photo$photoperiod_day<- as.numeric(d.photo$photoperiod_day)
missing<-d.photo[is.na(d.photo$photoperiod_day),]
unique(missing$datasetID)
}



############### Updated 12 Oct 2018 by Cat ####################
##### Photoperiod type #####

d$photo_type<-ifelse(d$other.treatment=="ramped_photoperiod", "ramped", d$photo_type)
d$photo_type<-ifelse(is.na(d$photo_type), "exp", d$photo_type)


#### basler studies - updated by Cat 26 Oct 2018
if(FALSE){
bas<-d[(d$datasetID=="basler12" | d$datasetID== "basler14" & d$other.treatment=="ramped_photoperiod"),]
bas$photoperiod_day<-as.numeric(bas$photoperiod_day)
bas$photoperiod_day<- ifelse(!is.na(bas$response.time), ((bas$response.time*0.06) + bas$photoperiod_day), bas$photoperiod_day)
bas$photoperiod_night<-as.numeric(bas$photoperiod_night)
bas$photoperiod_night<-24-bas$photoperiod_day
d$photoperiod_day[which(d$datasetID=="basler12" | d$datasetID== "basler14" & d$other.treatment=="ramped_photoperiod")]<-bas$photoperiod_day
d$photoperiod_night[which(d$datasetID=="basler12" | d$datasetID== "basler14" & d$other.treatment=="ramped_photoperiod")]<-bas$photoperiod_night
}
d$photo_type<-ifelse(d$datasetID=="basler12" | d$datasetID=="basler14", "ramped", d$photo_type)



############### New data update 20 September 2019 by Cat ##################
fu19<-subset(d, d$datasetID=="fu19")
fu19$date<-as.Date(fu19$response.time, origin = "2016-01-01")
for(i in c(1:nrow(fu19))){
  fu19$photoperiod_day[i] <- ifelse(fu19$photoperiod_day=="ambient", 
                                    geosphere::daylength(fu19$provenance.lat[i], fu19$date[i]),
                                    geosphere::daylength(fu19$provenance.lat[i], fu19$date[i]) - 2)
}
fu19$photoperiod_day<-as.numeric(fu19$photoperiod_day)
fu19$photoperiod_day<- round(fu19$photoperiod_day, digits=0)
fu19$photoperiod_night<-24-fu19$photoperiod_day
fu19$photoperiod_day<-as.character(fu19$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="fu19")]<-fu19$photoperiod_day
d$photoperiod_night[which(d$datasetID=="fu19")]<-fu19$photoperiod_night


fu18<-subset(d, d$datasetID=="fu18")
fu18$date<-as.Date(fu18$response.time, origin = "2016-01-01")
for(i in c(1:nrow(fu18))){
  fu18$photoperiod_day[i] <- ifelse(fu18$photoperiod_day=="", 
                                    geosphere::daylength(fu18$provenance.lat[i], fu18$date[i]),
                                    geosphere::daylength(fu18$provenance.lat[i], fu18$date[i]) - 2)
}
fu18$photoperiod_day<-as.numeric(fu18$photoperiod_day)
fu18$photoperiod_day<- round(fu18$photoperiod_day, digits=0)
fu18$photoperiod_night<-24-fu18$photoperiod_day
fu18$photoperiod_day<-as.character(fu18$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="fu18")]<-fu18$photoperiod_day
d$photoperiod_night[which(d$datasetID=="fu18")]<-fu18$photoperiod_night




prev<-subset(d, d$datasetID=="prevey18")
prev$date<-as.Date(prev$response.time, origin = "2017-01-31")
for(i in c(1:nrow(prev))){
  prev$photoperiod_day[i] <- geosphere::daylength(prev$provenance.lat[i], prev$date[i])
}
prev$photoperiod_day<-as.numeric(prev$photoperiod_day)
prev$photoperiod_day<- round(prev$photoperiod_day, digits=0)
prev$photoperiod_night<-24-prev$photoperiod_day
prev$photoperiod_day<-as.character(prev$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="prevey18")]<-prev$photoperiod_day
d$photoperiod_night[which(d$datasetID=="prevey18")]<-prev$photoperiod_night

rich<-subset(d, d$datasetID=="richardson18")
rich$date<-as.Date(rich$response.time, origin = "2016-01-01")
for(i in c(1:nrow(rich))){
  rich$photoperiod_day[i] <- geosphere::daylength(rich$provenance.lat[i], rich$date[i])
}
rich$photoperiod_day<-as.numeric(rich$photoperiod_day)
rich$photoperiod_day<- round(rich$photoperiod_day, digits=0)
rich$photoperiod_night<-24-rich$photoperiod_day
rich$photoperiod_day<-as.character(rich$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="richardson18")]<-rich$photoperiod_day
d$photoperiod_night[which(d$datasetID=="richardson18")]<-rich$photoperiod_night

#### Vitra17 
vitra<-subset(d, d$datasetID=="vitra17")
vitra$date<-as.Date(vitra$response.time, origin = "2014-01-01")
for(i in c(1:nrow(vitra))){
  vitra$photoperiod_day[i] <- geosphere::daylength(vitra$provenance.lat[i], vitra$date[i])
}
vitra$photoperiod_day<-as.numeric(vitra$photoperiod_day)
vitra$photoperiod_day<- round(vitra$photoperiod_day, digits=0)
vitra$photoperiod_night<-24-vitra$photoperiod_day
vitra$photoperiod_day<-as.character(vitra$photoperiod_day)
d$photoperiod_day[which(d$datasetID=="vitra17")]<-vitra$photoperiod_day
d$photoperiod_night[which(d$datasetID=="vitra17")]<-vitra$photoperiod_night


} else {
  print("Error: d not a data.frame")
}

stop("Not an error, photoperiod is clean. Also, you can ignore the warning message below -- code converts a column to character, but the column is created in a different dataframe that is used and deleted in this source code and should not (that I can imagine) have any other impact.")
