#Code to calculate change in chilling with one degree C of warming
#By Ailene
#June 13, 2018
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")

library(chillR)
#Compare chilling in 2 temperatures that differe by 1 degree
#try temps from -5 to 5
temp1<-seq(from=-5, to=5, by=1)#degrees C
temp2<-temp1+1
days<-90#90 days for now
year<-2018#i don't think this matters?
doys<-seq(1:days)
chillcalc.temp1<-c()
chillcalc.temp2<-c()

for (i in 1:length(temp1)){
hrly.temp1 =
  data.frame(
    Temp = c(rep(temp1[i], times = 24 * as.numeric(days))),
    Year = rep(year, times = 24 * as.numeric(days)),
    JDay = seq(1:days) )
hrly.temp2 =
  data.frame(
    Temp = c(rep(temp2[i], times = 24 * as.numeric(days))),
    Year = rep(year, times = 24 * as.numeric(days)),
    JDay = seq(1:days) )
chillcalc.temp1<- rbind(chillcalc.temp1,chilling(hrly.temp1, hrly.temp1$JDay[1], hrly.temp1$JDay[nrow(hrly.temp1)])) 
chillcalc.temp2<- rbind(chillcalc.temp2,chilling(hrly.temp2, hrly.temp2$JDay[1], hrly.temp2$JDay[nrow(hrly.temp2)]))
}
#put all the things together
chillests<-as.data.frame(cbind(temp1,temp2,chillcalc.temp1$Chilling_Hours,chillcalc.temp2$Chilling_Hours,chillcalc.temp1$Utah_Model,chillcalc.temp2$Utah_Model,chillcalc.temp1$Chill_portions,chillcalc.temp2$Chill_portions))
colnames(chillests)[3:8]<-c("t1.chhr","t2.chhr","t1.utah","t2.utah","t1.chp","t2.chp")
chillests$t1.chp<-round(chillests$t1.chp, digits=2)
chillests$t2.chp<-round(chillests$t2.chp, digits=2)
chillests$t1.chhr<-round(chillests$t1.chhr, digits=2)
chillests$t2.chhr<-round(chillests$t2.chhr, digits=2)
chillests$t1.utah<-round(chillests$t1.utah, digits=2)
chillests$t2.utah<-round(chillests$t2.utah, digits=2)
chillests$tdif.chp<-chillests$t1.chp-chillests$t2.chp
chillests$tdif.utah<-chillests$t1.utah-chillests$t2.utah
chillests$tdif.chhr<-chillests$t1.chhr-chillests$t2.chhr
write.csv(chillests,"output/chillests.csv")

