## By Cat to double check forcing columns:
## Find any columns with 'ambient' or non-numerics and fix based on paper if possible
## See also: clean_ambientforcingfromdailyclimate.R
# Additions by Cat: 29 Oct 2018
  ### This file now adds a column 'force_type': amb = ambient force temp calculated from climate data in clean_ambientforcingfromdailyclimate.R,
  ## none = no information about force temp; not enough info = unable to calculate based on information; exp = experimentally manipulated; and 
  ## ramped = forcetemp was ramped


if(is.data.frame(d)){
  
d$force_type<-NA
  
amb<-d[which(d$forcetemp=="ambient"),]
unique(amb$datasetID)
### "boyer"       "cannell83"   "falusi96"    "fu13"        "guak98"      "gunderson12" "lamb37"     
### "morin10"     "sanzperez10" "sonsteby13" 
blank<- d[which(d$forcetemp==""),]
nas<-d[which(is.na(d$forcetemp)),]
unique(nas$datasetID)
unique(blank$datasetID)

d$response.time.num <-as.numeric(as.character(d$response.time))
    
## "ashby62"  "gansert02"  "hawkins12"  "ruesink98"

# ashby62: not enough information - above 3 degC but not sure how much more

# gansert02: ambient, maybe we can use climate data to calculate - imputed to be 5degC based on Fig 5
## Fixed 6 Feb 2018 - Cat
#d$forcetemp[which(d$datasetID=="gansert02")] <- 5

# hawkins12: complicated thermal time equation - maybe use climate data instead?

# ruesink98: flower data... not sure how it got through

# boyer: not enough information - assumed ambient, maybe could use climate data to calculate

# cannell83: ambient - maybe could use climate data to calculate

# falusi96: not enough information - assumed ambient, maybe could use climate data to calculate
## Calculates GDDs based on 5degC base temp. I will change to 5degC for now. To discuss.
## Fixed 6 Feb 2018 - Cat
#d$forcetemp[which(d$datasetID=="falusi96" & d$study=="exp1")] <- 5

# fu13: uses ambient temperature and some add degrees C, maybe could use climate data to calculate

# guak98: uses ambient temperature and some add degrees C, maybe could use climate data to calculate
## rough estimate based on Fig1
## Fixed 6 Feb 2018 - Cat
#d$forcetemp[which(d$datasetID=="guak98" & d$forcetemp=="ambient")] <- 12
#d$forcetemp[which(d$datasetID=="guak98" & d$forcetemp=="ambient + 4")] <- 16

# gunderson12: uses ambient temperature and some add degrees C, maybe could use climate data to calculate

# lamb37: not enough information - assumes ambient, maybe could use climate data to calculate

# morin10: uses ambient temperature and some add degrees C, maybe could use climate data to calculate
## Used rough estimate based on Fig1 - ambient = 15 -> 16.5 and 18
## Fixed 6 Feb 2018 - Cat
#d$forcetemp[which(d$datasetID=="morin10" & d$forcetemp=="ambient")] <- 15
#d$forcetemp[which(d$datasetID=="morin10" & d$forcetemp=="ambient + 1.5")] <- 16.5
#d$forcetemp[which(d$datasetID=="morin10" & d$forcetemp=="ambient + 3")] <- 18

# sanperez10: uses ambient temperature, recorded mean temperature per month - can extract data from there
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=0 & d$response.time.num<=30
 #                 & d$irradiance==100)] <- 6.2
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=0 & d$response.time.num<=30
 #                 & d$irradiance==20)] <- 5.2
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=0 & d$response.time.num<=30
 #                 & d$irradiance==5)] <- 5.4
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=31 & d$response.time.num<=59
 #                 & d$irradiance==100)] <- 6.4
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=31 & d$response.time.num<=59
 #                 & d$irradiance==20)] <- 4.8
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=31 & d$response.time.num<=59
 #                 & d$irradiance==5)] <- 5.2
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=60 & d$response.time.num<=90
  #                & d$irradiance==100)] <- 8.9
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=60 & d$response.time.num<=90
 #                 & d$irradiance==20)] <- 7.1
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=60 & d$response.time.num<=90
 #                 & d$irradiance==5)] <- 7.5
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=91 & d$response.time.num<=120 
  #                 & d$irradiance==100)] <- 10.7
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=91 & d$response.time.num<=120
 #                 & d$irradiance==20)] <- 9.2
#d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=91 & d$response.time.num<=120
 #                 & d$irradiance==5)] <- 9.7

# sonsteby13: flower bud study... not sure why it made it through

# man10 - fix from 0 ramped up 3 degrees every 6 days
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 28 & response == 100] <- 22)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 18 & response == 0] <- 16)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 19 & response == 20.981] <- 16)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 20 & response == 45.231] <- 16)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 21 & response == 75.684] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 22 & response == 90.156] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 23 & response == 97.813] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 24 & response == 97.813] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 25 & response == 98.167] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 26 & response == 99.267] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time.num == 27 & response == 100] <- 19)
d$force_type<-ifelse(d$datasetID=="man10", "ramped", d$force_type)

# schnabel87 - fix 10 for 2 week, then... to 10
d <- within(d, forcetemp[datasetID == 'schnabel87' & n == 30 & response.time.num == 35.4] <- 10)

# campbell75 - fix from 18-27 (20 average) to 20
d <- within(d, forcetemp[datasetID == 'campbell75' & study== "exp1"] <- 20)

# howe95 - calculated mean, changed from 22-27 to 24.5
d <- within(d, forcetemp[datasetID == 'howe95'] <- 24.5)

# laube14a 
d.sub$response.time<- as.numeric(d.sub$response.time)
for(i in c(1:nrow(d.sub))) {
  for(j in c(1:nrow(laube14)))
    d.sub$forcetemp[i]<-ifelse(d.sub$response.time[i] == laube14$day[j], laube14$temp[j], d.sub$forcetemp[i])
}
d.sub$forcetemp<-ifelse(is.na(d.sub$response.time), 27.5, d.sub$forcetemp)

d.sub$response.time<-ifelse(is.na(d.sub$response.time), "no response", d.sub$response.time)
d$forcetemp[which(d$datasetID=="laube14a")]<-d.sub$forcetemp
d$force_type<-ifelse(d$datasetID=="laube14a", "ramped", d$force_type)


# skuterud94 now - is thermal time, does not explicitly say which forcing temp
# for each tx (mean 9, 12, 15)

# basler12 - "Temperature was set to cycle ±5 K around the daily mean temperature, which was increased by
# 0.5 K every five days"
## Fixed 13 Apr 2018 - Cat
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=0 & d$response.time.num<5, 5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=5 & d$response.time.num<10, 5.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=10 & d$response.time.num<15, 6, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=15 & d$response.time.num<20, 6.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=20 & d$response.time.num<25, 7, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=25 & d$response.time.num<30, 7.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=30 & d$response.time.num<35, 8, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=35 & d$response.time.num<40, 8.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=40 & d$response.time.num<45, 9, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=45 & d$response.time.num<50, 9.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=50 & d$response.time.num<55, 10, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=55 & d$response.time.num<60, 10.5, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=60 & d$response.time.num<65, 11, d$forcetemp)
d$forcetemp<-ifelse(d$datasetID=="basler12" & d$respvar.simple=="daystobudburst" & d$response.time.num>=65 & d$response.time.num<70, 11.5, d$forcetemp)

d$force_type<-ifelse(d$datasetID=="basler12", "ramped", d$force_type)

# guak98: does not specify the "ambient" temperature but increased by 4 
# in other experiments, didn't change anything

# gunderson12: treatments were in relation to "thermal provenance" so I left them at ambient +2, +4

# delete the new response.time column that we don't need!
    
d$response.time.num <- NULL

d$force_type<-ifelse(d$forcetemp=="", "none", d$force_type)
d$force_type<-ifelse(d$forcetemp=="ambient" | d$forcetemp=="ambient + .7" | d$forcetemp=="ambient + 1.5" |
                       d$forcetemp=="ambient + 3" | d$forcetemp=="ambient + 4" | d$forcetemp=="ambient + 4.9", "amb", d$force_type)

d$force_type<-ifelse(d$forcetemp=="mean of 9, 12, 15", "not enough info", d$force_type)
d$force_type<-ifelse(d$forcetemp=="meandaily" & d$respvar.simple=="thermaltime", "not enough info", d$force_type)

d$force_type<-ifelse(is.na(d$force_type), "exp", d$force_type)
    
} else {
  print("Error: d not a data.frame")
}

stop("Not an error, ambient forcing to days now... getting closer. Also, you can ignore the two warning messages below -- code converts a column to character (twice), but the column is created, used and deleted in this source code and should not (that I can imagine) have any other impact.")
#setwd("~Documents/git/ospree/analyses/output")
#write.csv(d,"ospree_clean_withforce.csv")
