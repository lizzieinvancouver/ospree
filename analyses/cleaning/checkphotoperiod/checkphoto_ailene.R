#checking photoperiod
#October 15, 2018
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/ospree/analyses")
osp<-read.csv("input/ospree.csv", header=T)
ospbb<-read.csv("output/ospree_clean_withchill_BB.csv", header=T)
ospcl<-read.csv("output/ospree_clean.csv", header=T)
ospch<-read.csv("output/ospree_clean_withchill.csv", header=T)

#compare for devries82
osp$chilltemp[osp$datasetID=="devries82"]
ospbb$chilltemp[ospbb$datasetID=="devries82"]#doesn't exist
ospcl$chilltemp[ospcl$datasetID=="devries82"]#doesn't exist
cbind(osp$forcetemp[osp$datasetID=="devries82"],ospcl$forcetemp[ospcl$datasetID=="devries82"])
#mistakes in forcetemp for figures 2 and 3 (irradiance and forcetemp are switched)
cbind(ospcl$photoperiod_day[ospcl$datasetID=="devries82"],ospcl$photoperiod_day[ospcl$datasetID=="devries82"])

#hawerroth13
osp$chilltemp[osp$datasetID=="hawerroth13"]
ospbb$chilltemp[ospbb$datasetID=="hawerroth13"]#doesn't exist
ospcl$chilltemp[ospcl$datasetID=="hawerroth13"]
cbind(osp$forcetemp[osp$datasetID=="hawerroth13"],ospcl$forcetemp[ospcl$datasetID=="hawerroth13"])
cbind(osp$photoperiod_day[osp$datasetID=="hawerroth13"],ospcl$photoperiod_day[ospcl$datasetID=="hawerroth13"])
cbind(osp$figure.table..if.applicable.[osp$datasetID=="hawerroth13"],ospcl$figure.table..if.applicable.[ospcl$datasetID=="hawerroth13"])
#no table/figure listed. Add?
cbind(osp$fieldchill[osp$datasetID=="hawerroth13"],ospch$fieldchill[ospch$datasetID=="hawerroth13"])
ospch$Field_Chilling_Hours[ospch$datasetID=="hawerroth13"]

#heide12
osp$chilltemp[osp$datasetID=="heide12"]#no chill dat
ospbb$chilltemp[ospbb$datasetID=="heide12"]#no chill dat
ospcl$chilltemp[ospcl$datasetID=="heide12"]#no chill dat
cbind(osp$response[osp$datasetID=="heide12"],osp$forcetemp[osp$datasetID=="heide12"],osp$photoperiod_day[osp$datasetID=="heide12"],osp$figure.table..if.applicable.[osp$datasetID=="heide12"])
cbind(osp$forcetemp[osp$datasetID=="heide12"],ospbb$forcetemp[ospbb$datasetID=="heide12"])

cbind(ospbb$other.treatment[ospbb$datasetID=="heide12" & ospbb$figure.table..if.applicable.=="table 1"],ospbb$response[ospbb$datasetID=="heide12" & ospbb$figure.table..if.applicable.=="table 1"])
#response
cbind( osp$respvar[osp$datasetID=="heide12" & osp$figure.table..if.applicable.=="table 1"],
    osp$response[osp$datasetID=="heide12" & osp$figure.table..if.applicable.=="table 1"],
      osp$forcetemp[osp$datasetID=="heide12"& osp$figure.table..if.applicable.=="table 1"],
      osp$photoperiod_day[osp$datasetID=="heide12"& osp$figure.table..if.applicable.=="table 1"],
      osp$figure.table..if.applicable.[osp$datasetID=="heide12"& osp$figure.table..if.applicable.=="table 1"])
plot(as.numeric(ospcl$response[ospcl$datasetID=="heide12"]),as.numeric(ospch$response[ospch$datasetID=="heide12"]))
cbind( ospcl$respvar.simple[ospcl$datasetID=="heide12" & ospcl$figure.table..if.applicable.=="fig 2"],
       ospcl$response[ospcl$datasetID=="heide12" & ospcl$figure.table..if.applicable.=="fig 2"],
       ospcl$forcetemp[ospcl$datasetID=="heide12"& ospcl$figure.table..if.applicable.=="fig 2"],
       ospcl$photoperiod_day[ospcl$datasetID=="heide12"& ospcl$figure.table..if.applicable.=="fig 2"])

cbind(ospbb$respvar.simple[ospbb$datasetID=="heide12"],
      ospbb$response[ospbb$datasetID=="heide12"],
      ospbb$forcetemp[ospbb$datasetID=="heide12"],
      ospbb$photoperiod_day[ospbb$datasetID=="heide12"],
      ospbb$figure.table..if.applicable.[ospbb$datasetID=="heide12"])

cbind(osp$respvar.simple[osp$datasetID=="heide12"],
      osp$response[osp$datasetID=="heide12"],
      osp$forcetemp[osp$datasetID=="heide12"],
      osp$photoperiod_day[osp$datasetID=="heide12"],
      osp$figure.table..if.applicable.[osp$datasetID=="heide12"])

#howe95
osp$chilltemp[osp$datasetID=="howe95"]#no chill dat
ospbb$chilltemp[ospbb$datasetID=="howe95"]#no chill dat
ospcl$chilltemp[ospcl$datasetID=="howe95"]#no chill dat
cbind(osp$response[osp$datasetID=="howe95"],osp$forcetemp[osp$datasetID=="howe95"],osp$photoperiod_day[osp$datasetID=="howe95"],osp$figure.table..if.applicable.[osp$datasetID=="howe95"])
cbind(osp$forcetemp[osp$datasetID=="howe95"],ospbb$forcetemp[ospbb$datasetID=="howe95"])
cbind(osp$respvar[osp$datasetID=="howe95"],osp$response.time[osp$datasetID=="howe95"],osp$forcetemp[osp$datasetID=="howe95"],osp$photoperiod_day[osp$datasetID=="howe95"],osp$figure.table..if.applicable.[osp$datasetID=="howe95"])
cbind(ospbb$respvar[ospbb$datasetID=="howe95"],ospbb$response.time[ospbb$datasetID=="howe95"],ospbb$forcetemp[ospbb$datasetID=="howe95"],ospbb$photoperiod_day[ospbb$datasetID=="howe95"],ospbb$figure.table..if.applicable.[ospbb$datasetID=="howe95"])


#myking97
osp$chilltemp[osp$datasetID=="myking97"]#0 degrees
ospbb$chilltemp[ospbb$datasetID=="myking97"]#0 degrees
ospcl$chilltemp[ospcl$datasetID=="myking97"]#0 degrees
osp$chilldays[osp$datasetID=="myking97"]#120 days
ospbb$chilldays[ospbb$datasetID=="myking97"]#no chill dat
ospcl$chilldays[ospcl$datasetID=="myking97"]#no chill dat

cbind(osp$response[osp$datasetID=="myking97"],osp$forcetemp[osp$datasetID=="myking97"],osp$photoperiod_day[osp$datasetID=="myking97"],osp$figure.table..if.applicable.[osp$datasetID=="myking97"])
cbind(osp$forcetemp[osp$datasetID=="myking97"],ospbb$forcetemp[ospbb$datasetID=="myking97"])
cbind(osp$respvar[osp$datasetID=="myking97"],osp$response.time[osp$datasetID=="myking97"],osp$forcetemp[osp$datasetID=="myking97"],osp$photoperiod_day[osp$datasetID=="myking97"],osp$figure.table..if.applicable.[osp$datasetID=="myking97"])
cbind(ospbb$respvar[ospbb$datasetID=="myking97"],ospbb$response.time[ospbb$datasetID=="myking97"],ospbb$forcetemp[ospbb$datasetID=="myking97"],ospbb$photoperiod_day[ospbb$datasetID=="myking97"],ospbb$figure.table..if.applicable.[ospbb$datasetID=="myking97"])
cbind(ospbb$forcetemp_night[ospbb$datasetID=="myking97"],ospbb$forcetemp[ospbb$datasetID=="myking97"],ospbb$response.time[ospbb$datasetID=="myking97"],ospbb$photoperiod_day[ospbb$datasetID=="myking97"],ospbb$figure.table..if.applicable.[ospbb$datasetID=="myking97"])


#partanen98
osp$chilltemp[osp$datasetID=="partanen98"]#no chilltemp data
ospbb$chilltemp[ospbb$datasetID=="partanen98"]#no chilltemp data
ospcl$chilltemp[ospcl$datasetID=="partanen98"]#no chilltemp data
osp$chilldays[osp$datasetID=="partanen98"]#chilldays are there though..
ospbb$chilldays[ospbb$datasetID=="partanen98"]#no chill dat
ospcl$chilldays[ospcl$datasetID=="partanen98"]#no chill dat

cbind(osp$response.time[osp$datasetID=="partanen98"],osp$forcetemp[osp$datasetID=="partanen98"],osp$photoperiod_day[osp$datasetID=="partanen98"],osp$figure.table..if.applicable.[osp$datasetID=="partanen98"])
cbind(osp$respvar[osp$datasetID=="partanen98"],osp$response.time[osp$datasetID=="partanen98"],osp$forcetemp[osp$datasetID=="partanen98"],osp$photoperiod_day[osp$datasetID=="partanen98"],osp$figure.table..if.applicable.[osp$datasetID=="partanen98"])
cbind(ospbb$respvar[ospbb$datasetID=="partanen98"],ospbb$response.time[ospbb$datasetID=="partanen98"],ospbb$forcetemp[ospbb$datasetID=="partanen98"],ospbb$photoperiod_day[ospbb$datasetID=="partanen98"],ospbb$figure.table..if.applicable.[ospbb$datasetID=="partanen98"])
cbind(ospbb$forcetemp_night[ospbb$datasetID=="partanen98"],ospbb$forcetemp[ospbb$datasetID=="partanen98"],ospbb$response.time[ospbb$datasetID=="partanen98"],ospbb$photoperiod_day[ospbb$datasetID=="partanen98"],ospbb$figure.table..if.applicable.[ospbb$datasetID=="partanen98"])

osp$photoperiod_day[osp$datasetID=="partanen98"]
ospch$photoperiod_day[ospch$datasetID=="partanen98"]
ospbb$photoperiod_day[ospbb$datasetID=="partanen98"]


#skuterud94
osp$chilltemp[osp$datasetID=="skuterud94"]
ospbb$chilltemp[ospbb$datasetID=="skuterud94"]#
ospcl$chilltemp[ospcl$datasetID=="skuterud94"]#
osp$chilldays[osp$datasetID=="skuterud94"]#
ospbb$chilldays[ospbb$datasetID=="skuterud94"]
ospcl$chilldays[ospcl$datasetID=="skuterud94"]

cbind(osp$response.time[osp$datasetID=="skuterud94"],osp$forcetemp[osp$datasetID=="skuterud94"],osp$photoperiod_day[osp$datasetID=="skuterud94"],osp$figure.table..if.applicable.[osp$datasetID=="skuterud94"])
cbind(osp$respvar[osp$datasetID=="partanen98"],osp$response.time[osp$datasetID=="partanen98"],osp$forcetemp[osp$datasetID=="partanen98"],osp$photoperiod_day[osp$datasetID=="partanen98"],osp$figure.table..if.applicable.[osp$datasetID=="partanen98"])
cbind(ospbb$respvar[ospbb$datasetID=="partanen98"],ospbb$response.time[ospbb$datasetID=="partanen98"],ospbb$forcetemp[ospbb$datasetID=="partanen98"],ospbb$photoperiod_day[ospbb$datasetID=="partanen98"],ospbb$figure.table..if.applicable.[ospbb$datasetID=="partanen98"])
cbind(ospbb$forcetemp_night[ospbb$datasetID=="partanen98"],ospbb$forcetemp[ospbb$datasetID=="partanen98"],ospbb$response.time[ospbb$datasetID=="partanen98"],ospbb$photoperiod_day[ospbb$datasetID=="partanen98"],ospbb$figure.table..if.applicable.[ospbb$datasetID=="partanen98"])

osp$photoperiod_day[osp$datasetID=="partanen98"]
ospch$photoperiod_day[ospch$datasetID=="partanen98"]
ospbb$photoperiod_day[ospbb$datasetID=="partanen98"]

#viheraaarnio06
osp$chilltemp[osp$datasetID=="viheraaarnio06"]
ospbb$chilltemp[ospbb$datasetID=="viheraaarnio06"]#
ospcl$chilltemp[ospcl$datasetID=="viheraaarnio06"]#no chilltemp info
osp$chilldays[osp$datasetID=="viheraaarnio06"]#
ospbb$chilldays[ospbb$datasetID=="viheraaarnio06"]#no chill info
ospcl$chilldays[ospcl$datasetID=="viheraaarnio06"]# no chill info

cbind(osp$response[osp$datasetID=="viheraaarnio06"],osp$forcetemp[osp$datasetID=="viheraaarnio06"],osp$photoperiod_day[osp$datasetID=="viheraaarnio06"],osp$figure.table..if.applicable.[osp$datasetID=="viheraaarnio06"])

#linksaloo
osp$chilltemp[osp$datasetID=="linkosalo06"]
ospbb$chilltemp[ospbb$datasetID=="linkosalo06"]#
ospcl$chilltemp[ospcl$datasetID=="linkosalo06"]
osp$chilldays[osp$datasetID=="linkosalo06"]#
ospbb$chilldays[ospbb$datasetID=="linkosalo06"]#no chill info
ospcl$chilldays[ospcl$datasetID=="linkosalo06"]# no chill info

cbind(osp$chilldays[osp$datasetID=="linkosalo06"],osp$forcetemp[osp$datasetID=="linkosalo06"],osp$photoperiod_day[osp$datasetID=="linkosalo06"],osp$figure.table..if.applicable.[osp$datasetID=="linkosalo06"])
