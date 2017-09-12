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


