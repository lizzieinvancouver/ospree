## Started 26 June 2017 ##
## Started by Cat, edits by Lizzie, Ailene, Nacho, Dan ... ##
## 14 March 2018: Includes updates to recode the non-leafouts as 999 where appropriate ##

# Load from bb_cleanmergeall.R

## Recoding non-budburst to 999
## See bbcleaningresp_README.txt
d$response.time[d$response.time=="" & d$datasetID=="caffarra11b"] <- 999
d$response.time[d$response.time=="x" & d$datasetID=="Heide03"] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="heide93a"] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="laube14a"] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="nienstaedt66" & 
    d$chilldays==0 & d$species=="pungens"] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="spiers74" &
    d$respvar.simple=="daystobudburst"] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="webb78"] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="worrall67" &
    d$chilldays==14] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="zohner16" &
    d$genus=="Amelanchier" & d$species=="laevis"] <- 999
d$response.time[d$response.time=="NL" & d$datasetID=="zohner16" &
    d$genus=="Stachyurus" & d$species=="sinensis"] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="zohner16" &
    d$genus=="Viburnum" & d$species=="buddleifolium" & d$photoperiod_day==8] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="zohner16" &
    d$genus=="Viburnum" & d$species=="plicatum"& d$photoperiod_day==8] <- 999
d$response.time[d$response.time=="no response" & d$datasetID=="gianfagna85"] <- 999

## cleaning some phenstages that are daystobudburst
# first, identify which ones to look at
phenstage <- d[which(d$respvar.simple=="phenstage"),]
unique(phenstage$datasetID) # cannell83, gansert02, gunderson12, pagter15, pettersen71, sonsteby13

### Let's check...
# cannell83: not a useful daystobudburst conversion

# gansert02: we could use budstages 2-3 
d[which(d$datasetID=="gansert02" & d$response>=2 & d$response<3),] 
d$respvar.simple[which(d$datasetID=="gansert02" & d$response>=2 & d$response<3)] <- "daystobudburst"
d$response[which(d$datasetID=="gansert02" & d$response>=2 & d$response<3)] <- "timeonly"

# gunderson12: budburst is defined as stage 4 which is plotted in Figure 2 and is already recorded in ospree dataset

# pagter15: Between budstage 1 and 2 is considered budburst - gives 5 observations
d$respvar.simple[which(d$datasetID=="pagter15" & d$response>=1 & d$response<2)] <-"daystobudburst"
d$response[which(d$datasetID=="pagter15" & d$response>=1 & d$response<2)] <- "timeonly"

# pettersen71: flowers - can't fix

# sonsteby13: flowers - can't fix

#######################################################
## Converting "dayofyeartobudburst" to "daystobudburst" ##
#######################################################

########### gomory15 issues ###########
## response.time is in day of year to budburst and must be changed to daystobudburst
startofexp<-as.Date("2011-03-01")
startofexp <- strftime(startofexp, format = "%j") ## 60
d$response.time[which(d$datasetID=="gomory15")]<-as.numeric(d$response.time[which(d$datasetID=="gomory15")])-60
d<-within(d, respvar.simple[datasetID=="gomory15"]<-"daystobudburst")

#######################################################
## Converting "percentbudburst_dayofyear" to "percentbudburst" ##
#######################################################
########### Sanz-Perez09 ########### (Ailene added 2017 September)
#start time of experiment is day of year 49
d$response.time[which(d$datasetID=="Sanz-Perez09")]<-as.numeric(d$response.time[which(d$datasetID=="Sanz-Perez09")])-49
d<-within(d, respvar.simple[datasetID=="Sanz-Perez09"]<-"percentbudburst")

########### sanzperez10 ########### (Ailene added 2017 September)
# start time of experiment is day of year 24
d$response.time[which(d$datasetID=="sanzperez10")]<-as.integer(as.numeric(d$response.time[which(d$datasetID=="sanzperez10")])-24) # rounding here to avoid losing negative numbers close to zero
d<-within(d, respvar.simple[datasetID=="sanzperez10"]<-"percentbudburst")

#########Skre08 #####(Dan added 26 Sept 2017) decided we cannot determine, leaving code here incase authors responses (Dan)
#startofexp<-as.Date("2001-01-25")
#startofexp <- strftime(startofexp, format = "%j")
#d$response.time[which(d$datasetID=="skre08" & d$year=="2001")]<-as.numeric(d$response.time[which(d$datasetID=="skre08" & d$year=="2001")])-25

startofexp<-as.Date("2001-11-22")
startofexp <- strftime(startofexp, format = "%j") ### day of year 326 but of 2001
365-326
d$response.time[which(d$datasetID=="skre08" & d$year=="2002")]<-as.numeric(d$response.time[which(d$datasetID=="skre08" & d$year=="2002")])+39
d<-within(d, respvar.simple[datasetID=="skre08"]<-"daystobudburst")

###########################################################################
### dayofyeartobudburst where we cannot determine when forcing started ####
###########################################################################
d<-d[!(d$datasetID=="fu13"),]

#no start date for forcing in gunderson12 (multiyear OTC field study)
d<-d[!(d$datasetID=="gunderson12"),]

#no start date for forcing in hawkins12 (year round field study)
d<-d[!(d$datasetID=="hawkins12"),]

d<-d[!(d$datasetID=="skre08"),] 

d<-d[!(d$datasetID=="pop2000"),] 


######### Partanen05 ####### (Cat added 28 September 2017)
#### All response.time entries are field sample dates and not days to budburst. Unable to fix
## So will be removed here.
d<-d[!(d$datasetID=="partanen05"),]

######### Ramos99 ####### (Cat added 28 September 2017)
#### All response.time entries for Figure 1A and 1B are field sample dates and not days to budburst. 
## Unable to fix so will be removed
d<-d[!(d$datasetID=="ramos99" & is.na(d$response.time)),]
