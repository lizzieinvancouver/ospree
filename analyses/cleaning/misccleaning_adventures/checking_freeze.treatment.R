d <- read.csv("output/ospree_clean_withchill.csv")
#unique(d$freeze.treatment.photoperiod_day)
#other values are "12" and "ambient" which make more sense
#unique(d$freeze.treatment.time)
#d$year[d$datasetID=="falusi96"]

#unique(d$freeze.treatment.photoperiod_night)

#d$datasetID[d$freeze.treatment.photoperiod_night=="13"]#cook05
#d$datasetID[d$freeze.treatment.photoperiod_night=="21"]#ruesink98; this
#d$datasetID[d$freeze.treatment.photoperiod_night=="ambient"]#schnabel87
#unique(d$freeze.treatment.temp_day)
#unique(d$datasetID[d$freeze.treatment.temp_day!=""])#"biasi12"    "cook05"     "ruesink98"  "schnabel87"
#unique(d$datasetID[d$freeze.treatment.temp_night!=""])#"basler12" "biasi12" "falusi90"

#If we want to clean these:
d$freeze.treatment.temp_day[d$datasetID=="biasi12"]#this study actually did freeze treatments! but the columns are somehow shifted the values do not make sense for temperatures, mistake?
d$freeze.treatment.temp_night[d$datasetID=="biasi12"]#the values do not make sense for temperaturesthis study actually did freeze treatments! so if we want to clean it then something else should go here...
d$response..pre.treatment.[d$datasetID=="basler12"]<-d$response..post.treatment[d$datasetID=="basler12"]#these were originally located in the response.posttreatment column- got sfifted somehow
d$freeze.treatment.temp_night[d$datasetID=="basler12"]<-d$response..pre.treatment.[d$datasetID=="basler12"]#these were originally located in the response.pretreatment column
d$freeze.treatment.temp_night[d$datasetID=="basler12"]<-""

d$freeze.treatment.temp_day[d$datasetID=="ruesink98"]<-""#one was "1" which was an error
d$freeze.treatment.photoperiod_night[d$datasetID=="ruesink98"]<-""#one was "21" which was an error
d$freeze.treatment.photoperiod_day[d$datasetID=="spann04"]<-""#was "d)."- mistake!
d$freeze.treatment.time[d$datasetID=="spann04"]<-""#these are all my mistake! 

#i think biasil12 response (pre-treatment) and response (post-treatment) columns are mistakes...
