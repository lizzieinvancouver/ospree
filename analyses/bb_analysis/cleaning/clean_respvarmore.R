## 26 June 2017 - Cat
# Updated 31 August 2017 - now fixing all respvar issues

# Load from bb_cleanmergeall.R

phenstage <- d[which(d$respvar.simple=="phenstage"),]
unique(phenstage$datasetID)
datasets<-unique(phenstage$datasetID)
xx<-d[which(d$datasetID==datasets),] # cannell83, gansert02, gunderson12, pagter15, pettersen71, sonsteby13
unique(xx$respvar.simple)
daysto<-xx%>%filter(respvar.simple=="daystobudburst")

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
## Converting "percentbudburst_dayofyear" to "percentbudburst" suggested changes##
#######################################################
########### Sanz-Perez09 ########### (Ailene added 2017 September)
#start time of experiment is julian day 49
d$response.time[which(d$datasetID=="Sanz-Perez09")]<-as.numeric(d$response.time[which(d$datasetID=="Sanz-Perez09")])-49
d<-within(d, respvar.simple[datasetID=="Sanz-Perez09"]<-"percentbudburst")

########### sanzperez10 ########### (Ailene added 2017 September)
#start time of experiment is julian day 24
d$response.time[which(d$datasetID=="sanzperez10")]<-as.numeric(d$response.time[which(d$datasetID=="sanzperez10")])-24
d<-within(d, respvar.simple[datasetID=="sanzperez10"]<-"percentbudburst")

#########Skre08 #####(Dan added 26 Sept 2017)
startofexp<-as.Date("2001-01-25")
startofexp <- strftime(startofexp, format = "%j")
d$response.time[which(d$datasetID=="skre08" & d$year=="2001")]<-as.numeric(d$response.time[which(d$datasetID=="skre08" & d$year=="2001")])-25

startofexp<-as.Date("2001-11-22")
startofexp <- strftime(startofexp, format = "%j")###Julian day 326 but of 2001
365-326 #### 36 days before 
d$response.time[which(d$datasetID=="skre08" & d$year=="2002")]<-as.numeric(d$response.time[which(d$datasetID=="skre08" & d$year=="2002")])+39
d<-within(d, respvar.simple[datasetID=="skre08"]<-"daystobudburst")

###########  ########### (Ailene added 2017 September)
#sanzperez10 study had a start day of experiment is julian day 24
d$response.time[which(d$datasetID=="sanzperez10")]<-as.numeric(d$response.time[which(d$datasetID=="sanzperez10")])-24
d<-within(d, respvar.simple[datasetID=="sanzperez10"]<-"percentbudburst")

#gunderson12 study used temperature-controlled opentop chambers that were warmed throughout the year for 2003, 2004, 2005, so there's not really a start date
#in the study, they use March 1 as a date from which to count accumulated growing degree days, so we could use that date to adjust time?
#That is what the code below does,but I'm not sure if we want to do this... first for the budburst rows:
#startofcalc<-c(as.Date("2003-03-01"))#Figure 2a shows just 2003 data
#startofcalc <- as.numeric(strftime(startofcalc, format = "%j"))###Julian day March 1, 2003
#d$response.time[which(d$respvar == "dayofyeartobudburst" & d$datasetID=="gunderson12")]<-
#    as.numeric(d$response.time[which(d$respvar == "dayofyeartobudburst" & d$datasetID=="gunderson12")])-startofcalc
#d<-within(d, respvar.simple[respvar == "dayofyeartobudburst" & d$datasetID=="gunderson12"]<-"percentbudburst")

#now for the budstage rows, which are a mean of all three years, so use same julian day for march 1, 2003:
#d$response.time[which(d$respvar == "budstage_dayofyear" & d$datasetID=="gunderson12")]<-
#   as.numeric(d$response.time[which(d$respvar == "budstage_dayofyear" & d$datasetID=="gunderson12")])-startofcalc
#d<-within(d, respvar.simple[respvar == "budstage_dayofyear" & datasetID=="gunderson12"]<-"budstage")

###########################################################################
### dayofyeartobudburst where we cannot determine when forcing started ####
###########################################################################
d<-d[!(d$datasetID=="fu13"),]


##########################################################
############## Problem D Issues! #########################
##########################################################

######### Partanen05 ####### (Cat added 28 September 2017)
#### All response.time entries are field sample dates and not days to budburst. Unable to fix
## So will be removed here.
d<-d[!(d$datasetID=="partanen05"),]

######### Ramos99 ####### (Cat added 28 September 2017)
#### All response.time entries for Figure 1A and 1B are field sample dates and not days to budburst. 
## Unable to fix so will be removed
d<-d[!(d$datasetID=="ramos99" & is.na(d$response.time)),]
