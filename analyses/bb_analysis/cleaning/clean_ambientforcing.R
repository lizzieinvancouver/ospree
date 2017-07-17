###Cat to double check forcing columns
##To do:
## Find any columns with 'ambient' or non-numerics and fix
#rm(list=ls()) 
#options(stringsAsFactors=FALSE)

## read data
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
#setwd("~/Documents/git/ospree/analyses")
#d<-read.csv("output/ospree_clean.csv",as.is=T)
if(is.data.frame(d)){
  
amb<-d[which(d$forcetemp=="ambient"),]
unique(amb$datasetID)
### "boyer"       "cannell83"   "falusi96"    "fu13"        "guak98"      "gunderson12" "lamb37"     
### "morin10"     "sanzperez10" "sonsteby13" 
blank<- d[which(d$forcetemp==""),]
unique(blank$datasetID)
    
## "ashby62"  "gansert02"  "hawkins12"  "ruesink98"

# ashby62: not enough information

# gansert02: ambient, maybe we can use climate data to calculate

# hawkins12: complicated thermal time equation - maybe use climate data instead?

# ruesink98: flower data... not sure how it got through

# boyer: not enough information - assumed ambient, maybe could use climate data to calculate

# cannell83: ambient - maybe could use climate data to calculate

# falusi96: not enough information - assumed ambient, maybe could use climate data to calculate

# fu13: uses ambient temperature and some add degrees C, maybe could use climate data to calculate

# guak98: uses ambient temperature and some add degrees C, maybe could use climate data to calculate

# gunderson12: uses ambient temperature and some add degrees C, maybe could use climate data to calculate

# lamb37: not enough information - assumes ambient, maybe could use climate data to calculate

# morin10: uses ambient temperature and some add degrees C, maybe could use climate data to calculate

# sanperez10: uses ambient temperature, recorded mean temperature per month - can extract data from there
d$response.time.num <-as.numeric(as.character(d$response.time))
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=0 & d$response.time.num<=30
                  & d$irradiance==100)] <- 6.2
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=0 & d$response.time.num<=30
                  & d$irradiance==20)] <- 5.2
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=0 & d$response.time.num<=30
                  & d$irradiance==5)] <- 5.4
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=31 & d$response.time.num<=59
                  & d$irradiance==100)] <- 6.4
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=31 & d$response.time.num<=59
                  & d$irradiance==20)] <- 4.8
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=31 & d$response.time.num<=59
                  & d$irradiance==5)] <- 5.2
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=60 & d$response.time.num<=90
                  & d$irradiance==100)] <- 8.9
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=60 & d$response.time.num<=90
                  & d$irradiance==20)] <- 7.1
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=60 & d$response.time.num<=90
                  & d$irradiance==5)] <- 7.5
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=91 & d$response.time.num<=120 
                  & d$irradiance==100)] <- 10.7
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=91 & d$response.time.num<=120
                  & d$irradiance==20)] <- 9.2
d$forcetemp[which(d$datasetID=="sanzperez10" & d$response.time.num>=91 & d$response.time.num<=120
                  & d$irradiance==5)] <- 9.7

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

# schnabel87 - fix 10 for 2 week, then... to 10
d <- within(d, forcetemp[datasetID == 'schnabel87' & n == 30 & response.time.num == 35.4] <- 10)

# campbell75 - fix from 18-27 (20 average) to 20
d <- within(d, forcetemp[datasetID == 'campbell75' & study== "exp1"] <- 20)

# howe95 - calculated mean, changed from 22-27 to 24.5
d <- within(d, forcetemp[datasetID == 'howe95'] <- 24.5)

# laube14a - currently listed as 7-27.5, need to calculate thermaltime in order to extract 
# this information. Does not seem to give equation for 'median forcing requirements' at all
# if we did get it would need crafty approach to deal with ramping of temperatures

# skuterud94 now - is thermal time, does not explicitly say which forcing temp
# for each tx (mean 9, 12, 15)

# basler12 - is another thermal time study, currently "meandaily", cannot be fixed

# guak98: does not specify the "ambient" temperature but increased by 4 
# in other experiments, didn't change anything

# gunderson12: treatments were in relation to "thermal provenance" so I left them at ambient +2, +4

# delete the new response.time column that we don't need!
    
d$response.time.num <- NULL
    
} else {
  print("Error: d not a data.frame")
}

stop("Not an error, ambient forcing to days now... getting closer. Also, you can ignore the single warning message below -- code converts a column to character, but the column is created, used and deleted in this source code and should not (that I can imagine) have any other impact.")
#setwd("~Documents/git/ospree/analyses/output")
#write.csv(d,"ospree_clean_withforce.csv")
