#code to prep full ospree new (2019) for knb
setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses")


d <- read.csv("output/ospree_clean_withchill.csv", header=TRUE)
dknb<-subset(d,select=c("datasetID","study","Entered.By",
        "genus","species","varetc","woody","population","population.detail","provenance.lat",
        "provenance.long", "population.altitude.m","continent" ,"year","material","fieldchill",         
        "fieldsample.date","chilltemp","chillphotoperiod","chilldays",                   
       "forcetemp","forcetemp_night","photoperiod_day" ,"photoperiod_night",           
      "respvar","response","response.time", "n", "error.type","resp_error",                  
        "figure.table..if.applicable.","growing.lat","growing.long","field.chill.units",           
        "cu.model"))
write.csv(dknb,"output/ospreebb2019update_forknb.csv", row.names = FALSE)
