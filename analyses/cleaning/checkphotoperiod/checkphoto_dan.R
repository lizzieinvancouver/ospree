###Dan's check photowork

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/git/ospree/analyses")
osp<-read.csv("input/ospree.csv", header=T)
ospbb<-read.csv("output/ospree_clean_withchill_BB.csv", header=T)
ospcl<-read.csv("output/ospree_clean.csv", header=T)
ospch<-read.csv("output/ospree_clean_withchill.csv", header=T)

# check okie11

osp$chilltemp[osp$datasetID=="okie11" & osp$study=="exp1"] 
ospch$chilltemp[ospch$datasetID=="okie11" & ospch$study=="exp1"]
ospcl$chilltemp[ospcl$datasetID=="okie11" & ospcl$study=="exp1"]
ospbb$chilltemp[ospbb$datasetID=="okie11" & ospbb$study=="exp1"]### not in use
# day temp
cbind(osp$forcetemp[osp$datasetID=="okie11" & osp$study=="exp1"],ospcl$forcetemp[ospcl$datasetID=="okie11" &ospcl$study=="exp1"],ospch$forcetemp[ospch$datasetID=="okie11"& ospch$study=="exp1"])
#photoperiods
cbind(osp$photoperiod_day[osp$datasetID=="okie11" & osp$study=="exp1"],ospcl$photoperiod_day[ospcl$datasetID=="okie11" & ospcl$study=="exp1"],ospch$photoperiod_day[ospch$datasetID=="okie11" & ospch$study=="exp1"])
###
#thielges75_exp1
osp$chilltemp[osp$datasetID=="thielges75" & osp$study=="exp1"] 
ospch$chilltemp[ospch$datasetID=="thielges75" & ospch$study=="exp1"]
ospcl$chilltemp[ospcl$datasetID=="thielges75" & ospcl$study=="exp1"]
ospbb$chilltemp[ospbb$datasetID=="thielges75" & ospbb$study=="exp1"]

cbind(osp$forcetemp[osp$datasetID=="thielges75" & osp$study=="exp1"],ospcl$forcetemp[ospcl$datasetID=="thielges75" &ospcl$study=="exp1"],ospch$forcetemp[ospch$datasetID=="thielges75"& ospch$study=="exp1"],ospcl$forcetemp[ospcl$datasetID=="thielges75" &ospcl$study=="exp1"],ospbb$forcetemp[ospbb$datasetID=="thielges75"& ospbb$study=="exp1"])
cbind(osp$photoperiod_day[osp$datasetID=="thielges75" & osp$study=="exp1"],ospcl$photoperiod_day[ospcl$datasetID=="thielges75" & ospcl$study=="exp1"],ospch$photoperiod_day[ospch$datasetID=="thielges75" & ospch$study=="exp1"],ospbb$photoperiod_day[ospbb$datasetID=="thielges75" & ospbb$study=="exp1"])


##ruesink98_exp2
osp$chilltemp[osp$datasetID=="ruesink98" & osp$study=="exp2"] 
ospch$chilltemp[ospch$datasetID=="ruesink98" & ospch$study=="exp2"]
ospcl$chilltemp[ospcl$datasetID=="ruesink98" & ospcl$study=="exp2"]
ospbb$chilltemp[ospbb$datasetID=="ruesink98" & ospbb$study=="exp2"]

cbind(osp$forcetemp[osp$datasetID=="ruesink98" & osp$study=="exp2"],ospcl$forcetemp[ospcl$datasetID=="ruesink98" &ospcl$study=="exp2"],ospch$forcetemp[ospch$datasetID=="ruesink98"& ospch$study=="exp2"],ospbb$forcetemp[ospbb$datasetID=="ruesink98"& ospbb$study=="exp2"])


cbind(osp$photoperiod_day[osp$datasetID=="ruesink98" & osp$study=="exp2"],
      ospcl$photoperiod_day[ospcl$datasetID=="ruesink98" &ospcl$study=="exp2"],
      ospch$photoperiod_day[ospch$datasetID=="ruesink98"& ospch$study=="exp2"],
      ospbb$photoperiod_day[ospbb$datasetID=="ruesink98"& ospbb$study=="exp2"],
      ospbb$response[ospbb$datasetID=="ruesink98"& ospbb$study=="exp2"],osp$response[osp$datasetID=="ruesink98"& osp$study=="exp2"]) ###bb changes alignment




ospbb$respvar[ospbb$datasetID=="ruesink98"& ospbb$study=="exp2"]
osp$respvar[osp$datasetID=="ruesink98"& osp$study=="exp2"]

osp.rue<-filter(osp, datasetID=="ruesink98" &study=="exp2")
ospbb.rue<-filter(ospbb, datasetID=="ruesink98" &study=="exp2")
ospbb.rue<-ospbb.rue[order(ospbb.rue$response),]
osp.rue<-osp.rue[order(osp.rue$response),]
cbind(ospbb.rue$photoperiod_day,osp.rue$photoperiod_day)### okay except for resonse variable

#basler 14:
osp$chilltemp[osp$datasetID=="basler14" & osp$study=="exp1"] 
ospch$chilltemp[ospch$datasetID=="basler14" & ospch$study=="exp1"]
ospcl$chilltemp[ospcl$datasetID=="basler14" & ospcl$study=="exp1"]
ospbb$chilltemp[ospbb$datasetID=="basler14" & ospbb$study=="exp1"] #nochilling

ospbb$photoperiod_day[ospbb$datasetID=="basler14" & ospbb$study=="exp1"]
osp$photoperiod_day[osp$datasetID=="basler14" & osp$study=="exp1"]
table(ospbb$photoperiod_day[ospbb$datasetID=="basler14" & ospbb$study=="exp1"])
table(osp$photoperiod_day[osp$datasetID=="basler14" & osp$study=="exp1"])

table(ospbb$forcetemp[ospbb$datasetID=="basler14" & ospbb$study=="exp1"])
table(osp$forcetemp[osp$datasetID=="basler14" & osp$study=="exp1"])

osp$figure.table..if.applicable.[osp$datasetID=="basler14" & osp$study=="exp1"] 


###falusi 96 exp 3
osp$chilltemp[osp$datasetID=="falusi96" & osp$study=="exp3"] 
ospch$chilltemp[ospch$datasetID=="falusi96" & ospch$study=="exp3"]
ospcl$chilltemp[ospcl$datasetID=="falusi96" & ospcl$study=="exp3"]
ospbb$chilltemp[ospbb$datasetID=="falusi96" & ospbb$study=="exp3"] #nochilling

table(ospbb$respvar[ospbb$datasetID=="falusi96" & ospbb$study=="exp3"])
table(osp$respvar[osp$datasetID=="falusi96" & osp$study=="exp3"] )

table(ospbb$photoperiod_day[ospbb$datasetID=="falusi96" & ospbb$study=="exp3"])
table(osp$photoperiod_day[osp$datasetID=="falusi96" & osp$study=="exp3"] )

table(ospbb$forcetemp[ospbb$datasetID=="falusi96" & ospbb$study=="exp3"])
table(osp$forcetemp[osp$datasetID=="falusi96" & osp$study=="exp3"] )
table(osp$figure.table..if.applicable.[osp$datasetID=="falusi96" & osp$study=="exp3"] )

range(osp$response[osp$datasetID=="falusi96" & osp$study=="exp3"])
range(ospbb$response[ospbb$datasetID=="falusi96" & ospbb$study=="exp3"])
range(ospbb$response.time[ospbb$datasetID=="falusi96" & ospbb$study=="exp3"])

###heide 05 exp 2
osp$chilltemp[osp$datasetID=="heide05" & osp$study=="exp2"] 
ospch$chilltemp[ospch$datasetID=="heide05" & ospch$study=="exp2"]
ospcl$chilltemp[ospcl$datasetID=="heide05" & ospcl$study=="exp2"]
ospbb$chilltemp[ospbb$datasetID=="heide05" & ospbb$study=="exp2"] #nochilling

table(ospbb$respvar[ospbb$datasetID=="heide05" & ospbb$study=="exp2"])
table(osp$respvar[osp$datasetID=="heide05" & osp$study=="exp2"] )

table(ospbb$photoperiod_day[ospbb$datasetID=="heide05" & ospbb$study=="exp2"])
table(osp$photoperiod_day[osp$datasetID=="heide05" & osp$study=="exp2"] )

table(ospbb$forcetemp[ospbb$datasetID=="heide05" & ospbb$study=="exp2"])
table(osp$forcetemp[osp$datasetID=="heide05" & osp$study=="exp2"] )
table(osp$figure.table..if.applicable.[osp$datasetID=="falusi96" & osp$study=="exp3"] )

range(osp$response[osp$datasetID=="heide05" & osp$study=="exp2"])
range(ospbb$response[ospbb$datasetID=="heide05" & ospbb$study=="exp2"])
range(ospbb$response.time[ospbb$datasetID=="heide05" & ospbb$study=="exp2"])
#### linkosalo06_exp1
osp$chilltemp[osp$datasetID=="linkosalo06" & osp$study=="exp1"] 
ospch$chilltemp[ospch$datasetID=="linkosalo06" & ospch$study=="exp1"]
ospcl$chilltemp[ospcl$datasetID=="linkosalo06" & ospcl$study=="exp1"]
ospbb$chilltemp[ospbb$datasetID=="linkosalo06" & ospbb$study=="exp1"] #nochilling

table(ospbb$respvar[ospbb$datasetID=="linkosalo06" & ospbb$study=="exp1"])
table(osp$respvar[osp$datasetID=="linkosalo06" & osp$study=="exp1"] )


table(ospbb$photoperiod_day[ospbb$datasetID=="linkosalo06" & ospbb$study=="exp1"])
table(osp$photoperiod_day[osp$datasetID=="linkosalo06" & osp$study=="exp1"] )

table(ospbb$forcetemp[ospbb$datasetID=="linkosalo06" & ospbb$study=="exp1"])
table(osp$forcetemp[osp$datasetID=="linkosalo06" & osp$study=="exp1"] )
table(osp$figure.table..if.applicable.[osp$datasetID=="linkosalo06" & osp$study=="exp1"] )

range(osp$response.time[osp$datasetID=="linkosalo06" & osp$study=="exp1"])
range(ospbb$response.time[ospbb$datasetID=="linkosalo06" & ospbb$study=="exp1"])
#### more work on linksalo
linky<-dplyr::filter(osp,datasetID=="linkosalo06")
linky.clean<-dplyr::filter(ospcl,datasetID=="linkosalo06")
linky.final<-dplyr::filter(ospch,datasetID=="linkosalo06")

#Check respvar

#chill.no$date<-as.Date(chill.no$date,format =  "%m/%d/%y")
library(lubridate)
resp<-yday(chill.no$date)
start<-yday("2004/03/08")
start+120.22
as.Date(120.22, origin = "2004-01-01")
as.Date(138, origin = "2004-01-01")


###heide 93
osp$chilltemp[osp$datasetID=="heide93" & osp$study=="exp1"] 
ospch$chilltemp[ospch$datasetID=="heide93" & ospch$study=="exp1"]
ospcl$chilltemp[ospcl$datasetID=="heide93" & ospcl$study=="exp1"]
ospbb$chilltemp[ospbb$datasetID=="heide93" & ospbb$study=="exp1"] #nochilling

table(ospbb$respvar[ospbb$datasetID=="heide93" & ospbb$study=="exp1"])
table(osp$respvar[osp$datasetID=="heide93" & osp$study=="exp1"] )

table(ospbb$photoperiod_day[ospbb$datasetID=="heide93" & ospbb$study=="exp1"])
table(osp$photoperiod_day[osp$datasetID=="heide93" & osp$study=="exp1"] )

table(ospbb$forcetemp[ospbb$datasetID=="heide93" & ospbb$study=="exp1"])
table(osp$forcetemp[osp$datasetID=="heide93" & osp$study=="exp1"] )
table(ospbb$figure.table..if.applicable.[ospbb$datasetID=="heide93" & ospbb$study=="exp1"] )

range(osp$response[osp$datasetID=="heide93" & osp$study=="exp1"])
range(ospbb$response[ospbb$datasetID=="heide93" & ospbb$study=="exp1"])
range(ospbb$response.time[ospbb$datasetID=="heide93" & ospbb$study=="exp1"])

####heide 93a exp1 and 3
osp$chilltemp[osp$datasetID=="heide93a" & osp$study=="exp3"] 
ospch$chilltemp[ospch$datasetID=="heide93a" & ospch$study=="exp3"]
ospcl$chilltemp[ospcl$datasetID=="heide93a" & ospcl$study=="exp3"]
ospbb$chilltemp[ospbb$datasetID=="heide93a" & ospbb$study=="exp3"] #nochilling

table(ospbb$respvar[ospbb$datasetID=="heide93a" & ospbb$study=="exp3"])
table(osp$respvar[osp$datasetID=="heide93a" & osp$study=="exp3"] )

table(ospbb$photoperiod_day[ospbb$datasetID=="heide93a" & ospbb$study=="exp3"])
table(osp$photoperiod_day[osp$datasetID=="heide93a" & osp$study=="exp3"] )

table(ospbb$forcetemp[ospbb$datasetID=="heide93a" & ospbb$study=="exp3"])
table(osp$forcetemp[osp$datasetID=="heide93a" & osp$study=="exp3"] )
table(ospbb$figure.table..if.applicable.[ospbb$datasetID=="heide93a" & ospbb$study=="exp3"] )

range(osp$response[osp$datasetID=="heide93" & osp$study=="exp3"])
range(ospbb$response[ospbb$datasetID=="heide93a" & ospbb$study=="exp3"])

range(ospbb$response.time[ospbb$datasetID=="heide93a" & ospbb$study=="exp3"])
