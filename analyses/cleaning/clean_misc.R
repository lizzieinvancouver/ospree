## Started 2 October 2017 ##
## By Lizzie (so far)##

## This code does some OSPREE cleaning that didn't fit anywhere else ##
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

## Added by Dan B. to make ashby62 better
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


########
# In June 2018, we discovered errors in some of the freezingtemp columns
#######

#d$freeze.treatment.temp_day[d$datasetID=="biasi12"]#this study actually did freeze treatments! but the columns are somehow shifted the values do not make sense for temperatures, mistake?
#d$freeze.treatment.temp_night[d$datasetID=="biasi12"]#the values do not make sense for temperatures but this study actually did freeze treatments! so if we want to clean it then something else should go here...
#d$chilltemp[d$datasetID=="biasi12"]#blank

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

#response
d$response..post.treatment[d$datasetID=="basler12"]<-d$response..pre.treatment.[d$datasetID=="basler12"]#these were originally located in the response.pretreatment column
d$response..pre.treatment.[d$datasetID=="basler12"]<-d$freeze.treatment.temp_night[d$datasetID=="basler12"]#these were originally located in the freeze.treatment.night column
d$freeze.treatment.temp_night[d$datasetID=="basler12"]<-""

d$freeze.treatment.temp_day[d$datasetID=="ruesink98"]<-""#one was "1" which was an error
d$freeze.treatment.photoperiod_night[d$datasetID=="ruesink98"]<-""#one was "21" which was an error
d$freeze.treatment.photoperiod_day[d$datasetID=="spann04"]<-""#was "d)."- mistake!
d$freeze.treatment.time[d$datasetID=="spann04"]<-""#these are all my mistake! 

#cleaning field sample date in caffarra11b, which are currently all "1-Jun-2004"
d$fieldsample.date[d$datasetID=="caffarra11b" & d$figure.table..if.applicable.=="table 3" & d$chilldays==30]<-"30-Dec-2004"
d$fieldsample.date[d$datasetID=="caffarra11b" & d$figure.table..if.applicable.=="table 3" & d$chilldays==55]<-"24-Jan-2005"#
d$fieldsample.date[d$datasetID=="caffarra11b" & d$figure.table..if.applicable.=="table 3" & d$chilldays==95]<-"6-Mar-2005"

########
# Fall 2018 random check of papers turned up some cleaning
#######

## Both basler studies have ramped photoperiods:
# basler14: "photoperiods (initially 9.2 h (short day length, SD) versus 10.8 h (long day length, LD), increased daily by the natural daily increase of photoperiod at 46.5°N).... An additional set of cuttings was placed in a warm greenhouse (>21 °C) with long day length (16 h)"
# basler12: "The length of the pho- toperiod was extended daily using time switches, set to follow the natural (astronomical) daylength extension at the sampling lati- tude (∼47◦ N) of around 3–4 min per day."
d$other.treatment[which(d$datasetID=="basler14" & d$photoperiod_day=="9.2")] <- "ramped_photoperiod"
d$other.treatment[which(d$datasetID=="basler14" & d$photoperiod_day=="10.2")] <- "ramped_photoperiod"
d$other.treatment[which(d$datasetID=="basler12" & d$photoperiod_day=="shortday")] <- "ramped_photoperiod"
d$other.treatment[which(d$datasetID=="basler12" & d$photoperiod_day=="longday")] <- "ramped_photoperiod"

# Sanz-Perez09 has forcetmp_night as ambient, which is not correct (issue #209)
d$forcetemp_night[which(d$datasetID=="Sanz-Perez09" & d$forcetemp_night=="ambient")] <- ""

# morin10 has a field sample date, but I (Lizzie) think it is in situ field warming, so removing the dates
d$fieldsample.date[which(d$datasetID=="morin10")] <- ""

# viheraarnio06 has zero chilling, so we want to enter this to make the study more usable
#NOTE: this is THE ONLY study for which this was done.
#There may be other studies with zero chilling, for which the database currently has "" rather than 0
d$chilldays[which(d$datasetID=="viheraaarnio06")] <- "0"
# in addition, this study has a field sample date, but I (Ailene) think all plants are grown indoors so no field chilling...
d$fieldsample.date[which(d$datasetID=="viheraaarnio06")] <- ""

#hawerroth13 is missing the figure number.
d$figure.table..if.applicable.[which(d$datasetID=="hawerroth13")]<-"fig 2"

#heide12 study says "chilled at 2C in darkness for breaking of dormany After 10-12 w"
#For Table 1, chilling was 10 w, for Table 2 chilling was 12 w.
#Add this info
d$chilltemp[which(d$datasetID=="heide12" & d$figure.table..if.applicable.=="table 1")]<-"2"
d$chilldays[which(d$datasetID=="heide12" & d$figure.table..if.applicable.=="table 1")]<-"70"
d$chilltemp[which(d$datasetID=="heide12" & d$figure.table..if.applicable.=="table 2")]<-"2"
d$chilldays[which(d$datasetID=="heide12" & d$figure.table..if.applicable.=="table 1")]<-"84"

#devries82 has switched irradience treatments and forcing temperature treatments for fig 2 and fig3
d$forcetemp[which(d$datasetID=="devries82" & d$figure.table..if.applicable.=="fig2")]<-
    d$irradiance[which(d$datasetID=="devries82" & d$figure.table..if.applicable.=="fig2")]
d$irradiance[which(d$datasetID=="devries82" & d$figure.table..if.applicable.=="fig2")]<-c("8","8","8","16","16","16","24","24","24")
d$forcetemp[which(d$datasetID=="devries82" & d$figure.table..if.applicable.=="fig3")]<-
  d$irradiance[which(d$datasetID=="devries82" & d$figure.table..if.applicable.=="fig3")]
d$irradiance[which(d$datasetID=="devries82" & d$figure.table..if.applicable.=="fig3")]<-c("24","24","24","16","16","16","8","8","8")

#skuterud94 has incorrect daylength for some rows (all should be 8; currently some are 24). Also add note that night irradiance was 5micromol par
d$photoperiod_day[which(d$datasetID=="skuterud94")]<-"8"
d$photoperiod_night[which(d$datasetID=="skuterud94")]<-"16"
d$irradiance[which(d$datasetID=="skuterud94")]<-"natural daylight supplemented with 125, night= 5"
d$irradiance.units[which(d$datasetID=="skuterud94")]<-"micromol m_2 s_1"
#myking97 has incorrect daylength for some rows (all should be 12; currently some are 24)
d$photoperiod_day[which(d$datasetID=="myking97")]<-"12"
d$photoperiod_night[which(d$datasetID=="myking97")]<-"12"
d$irradiance[which(d$datasetID=="myking97")]<-"night=5"
d$irradiance.units[which(d$datasetID=="myking97")]<-"micromol m_2 s_1"

####Linkosalo06, The time to budbust is calculated from start of the year rather than start of the experiment cleaned by Dan B Nov 19 2018

realstart<-as.Date("2004/02/22")
start<-as.Date("2004/01/01")
realstart-start

###make a numeric response time column so you can subtract the realstart time for the result
d$response.time.num<-as.numeric(d$response.time)
d$response.time.num<-ifelse(d$datasetID=="linkosalo06",d$response.time.num-52,d$response.time.num)#correct the new values
d$response.time.num<-as.character(d$response.time.num) ##convert back to character so you can paste new into the real column
d$response.time<-ifelse(d$datasetID=="linkosalo06",d$response.time.num,d$response.time) ###add the 5 values for linkosalo
d$response.time.num<-NULL ## get ride of transitory numerica column
