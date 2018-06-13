## Started 2 October 2017 ##
## By Lizzie (so far)##

## This code does some basic OSPREE cleaning that didn't fit anywhere else ##
## It is sourced in cleanmerge_all.R ##

# In July 2017 we realized we left year off the Zonher data, but we need it!
# So we’re adding it in here, it should be year of experiment, which is 2014. 
d$year[which(d$datasetID=="zohner16")] <- 2014

# Fix a table that was mis-referenced
d$figure.table..if.applicable.[which(d$datasetID=="campbell75" &
    d$figure.table..if.applicable=="table1")] <- "table2"

# Falusi97 is difficult, the data on X axis in Fig 1 is TRANSFER date, but
# you cannot tell how long they measured them for. Based on my reading I am 90% sure
# Fig 1 shows values at the end of 120 d (see main text below fig and note exp 2 was
# all 60 d) so correcting that here. I still would be cautious of using these data as
# text below fig 1 also refers to 'last chilling cutoff date (December 24)' but it is
# not possible to figure what chilling was (perhaps 'nursery' was outside?)
d$response.time[which(d$datasetID=="falusi97" & d$study=="exp1")] <- 120

#pop2000 (Dan) is wrong on many levels.
# 1) resp.var is actually percenttwigswithbudburst 
# 2) response.time is actually chilling time
# I'm making those changes here, and also deleting the study from anaylsis in clean resp_varmore.R just incase it slips through
d$response.time[which(d$datasetID=="pop2000")] <- NA
d$respvar[which(d$datasetID=="pop2000")] <- "percentoftwigswithbudburst"


# Additional Edits made by Cat - 31 Jan 2017
## Moved from clean_respvar.R in Oct 2017 by Lizzie ##
# ghelardini10 issues - removed 8 rows not affiliated with study
for(i in d){
  d <- d[!(d$datasetID == "ghelardini10" & d$material == "root cuttings") &
            !(d$datasetID == "ghelardini10" & d$Entered.By == "DF"),]
}

###Added by Dan to make ashby  better
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="4-Mar-1957"&d$photoperiod_day==8 &
             d$respvar=="daystobudburst" &d$response==3] <- "Central Wisconsin"
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="4-Mar-1957"&d$photoperiod_day==12 &
             d$respvar=="daystobudburst" &d$response==2.6]<- "Central Wisconsin"
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="4-Mar-1957"&d$photoperiod_day==16 &
             d$respvar=="daystobudburst" & d$response==2.6]<- "Central Wisconsin"

d$population[d$datasetID=="ashby62"&d$fieldsample.date=="5-Feb-1957"&d$photoperiod_day==8 &
             d$respvar=="daystobudburst" &d$response==4.2]<- "Central Wisconsin"  
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="5-Feb-1957"&d$photoperiod_day==12 &
             d$respvar=="daystobudburst" &d$response==4.2]<- "Central Wisconsin"
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="5-Feb-1957"&d$photoperiod_day==16
             & d$respvar=="daystobudburst" &d$response==4.2]<- "Central Wisconsin"

d$population[d$datasetID=="ashby62"&d$fieldsample.date=="8-Jan-1957"&d$photoperiod_day==8 &
             d$respvar=="daystobudburst" &d$response==7.2]<- "Central Wisconsin"  
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="8-Jan-1957"&d$photoperiod_day==12 &
             d$respvar=="daystobudburst" &d$response==5.8]<- "Central Wisconsin"  
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="8-Jan-1957"&d$photoperiod_day==16 &
             d$respvar=="daystobudburst" &d$response==5.8]<- "Central Wisconsin"  

d$population[d$datasetID=="ashby62"&d$fieldsample.date=="11-Dec-1956"&d$photoperiod_day==8 &
             d$respvar=="daystobudburst" &d$response==13]<- "Central Wisconsin" 
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="11-Dec-1956"&d$photoperiod_day==12 &
             d$respvar=="daystobudburst" &d$response==16.2]<- "Central Wisconsin"
d$population[d$datasetID=="ashby62"&d$fieldsample.date=="11-Dec-1956"&d$photoperiod_day==16 &
             d$respvar=="daystobudburst" &d$response==32.2]<- "Central Wisconsin"

## Cleaning gheraldini10
## Issue is that the x axis is often field sample date, not time to budburst
# First, move the response time to fieldsample date and delete the data in response.time
d$fieldsample.date[d$datasetID=="ghelardini10"] <- d$response.time[d$datasetID=="ghelardini10"] 
d$response.time[d$datasetID=="ghelardini10"] <- ""
# Next re-assign the values to their appropriate fieldsample date
d$fieldsample.date[d$datasetID=="ghelardini10" & d$fieldsample.date=="15"] <- "15-Oct-2002"
d$fieldsample.date[d$datasetID=="ghelardini10" & d$fieldsample.date=="45"] <- "15-Nov-2002"
d$fieldsample.date[d$datasetID=="ghelardini10" & d$fieldsample.date=="75"] <- "15-Dec-2002"
d$fieldsample.date[d$datasetID=="ghelardini10" & d$fieldsample.date=="105"] <- "15-Jan-2003"
d$fieldsample.date[d$datasetID=="ghelardini10" & d$fieldsample.date=="135"] <- "15-Feb-2003"
d$fieldsample.date[d$datasetID=="ghelardini10" & d$fieldsample.date=="165"] <- "15-Mar-2003"
d$fieldsample.date[d$datasetID=="ghelardini10" & d$fieldsample.date=="185"] <- "15-Mar-2003"
# Note: I (Lizzie) assume 165 or 185 was a typo, only 6 datapoints in either figure (and 165 is only in fig 2 and 185 only in fig 4)
# Next, we need to make sure response col is now 1 (signifying that the only response variable is days to budburst and there is not corresponding Y axis of data)
d$response.time[d$datasetID=="ghelardini10" & d$respvar=="percentbudburst"] <- d$response[d$datasetID=="ghelardini10" & d$respvar=="percentbudburst" ]
d$response.time[d$datasetID=="ghelardini10" & d$respvar=="thermaltimetobudburst"] <- d$response[d$datasetID=="ghelardini10" & d$respvar=="thermaltimetobudburst" ]
# I didn't have to do the two above as separate lines (they are the only respvar I see for ghelardini10) but I wanted to...
# ...point out that these two respvar COULD be combined I think so thermal time is the X axis and % budburst is the Y
d$response[d$datasetID=="ghelardini10"] <- 1

#In June 2018, we discovered errors in some of the freezingtemp columns

#d$freeze.treatment.temp_day[d$datasetID=="biasi12"]#this study actually did freeze treatments! but the columns are somehow shifted the values do not make sense for temperatures, mistake?
#d$freeze.treatment.temp_night[d$datasetID=="biasi12"]#the values do not make sense for temperaturesthis study actually did freeze treatments! so if we want to clean it then something else should go here...
######Dan B's assement of biasi12:#######
# I dont think this study actually did freeze treatments.
#The column d$freeze.treatment.temp_day is actually 'chilling hours accumulated" based on table 1. this is not reflected in Chill_Hours columns in ospree_clean_withchill_BB.csv
#Proposed solution: migrate this column to Field_Chilling_Hours somewhere in chilling code, but ask Ailene?

#the column d$free.treatment_temp_night: Is actually the field sample day
#Solution: remove values from this column?
d$freeze.treatment.temp_night[d$datasetID=="biasi12"]<-""

#The column d$response..pre.treatment is just the average days to budburst same as d$response.time
#solution remove values from this column
d$response..pre.treatment[d$datasetID=="biasi12"]<-""

#response.
d$response..post.treatment[d$datasetID=="basler12"]<-d$response..pre.treatment[d$datasetID=="basler12"]#these were originally located in the response.pretreatment column
d$response..pre.treatment[d$datasetID=="basler12"]<-d$freeze.treatment.temp_night[d$datasetID=="basler12"]#these were originally located in the response.pretreatment column
d$freeze.treatment.temp_night[d$datasetID=="basler12"]<-""

d$freeze.treatment.temp_day[d$datasetID=="ruesink98"]<-""#one was "1" which was an error
d$freeze.treatment.photoperiod_night[d$datasetID=="ruesink98"]<-""#one was "21" which was an error
d$freeze.treatment.photoperiod_day[d$datasetID=="spann04"]<-""#was "d)."- mistake!
d$freeze.treatment.time[d$datasetID=="spann04"]<-""#these are all my mistake! 

