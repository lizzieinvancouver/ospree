### goal: plots ot understand experimental condition choices by region
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()
##setwd
setwd("~/Documents/git/ospree/analyses/output")
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


c<-read.csv("ospree_clean.csv",header = TRUE)
c$photoperiod_day<-as.numeric(c$photoperiod_day)

treatsum<-ddply(c,c("datasetID"), summarise,
mean=mean(photoperiod_day), max=max(photoperiod_day), min=min(photoperiod_day),meangrow=mean(growing.lat), meanprov=mean(provenance.lat))

treatsum<-filter(treatsum, meanprov>0)
##seperate which studies use same max and min photoperoioda
treatsum$yesphoto<-ifelse(treatsum$max==treatsum$min,"no-manipulation","manipulation")

##run 1m to see if there is a trend in photoperiod over space
Modmax<-lm(max~meanprov,data=treatsum)
summary(Modmax)
#plotthem
q<-ggplot(treatsum, aes(x=max, y=meanprov, color=yesphoto))
q+geom_point()+labs(title="Maximum experimental photoperiod", x="Max photoperiod", y="Latitude")
##for minimum photoperiod
Modmin<-lm(min~meanprov,data=treatsum)
summary(Modmin)
r<-ggplot(treatsum, aes(x=min, y=meanprov, color=yesphoto))
r+geom_point()+labs(title="Minimum experimental photoperiod", x="Min photoperiod", y="Latitude")
# next step could be to subset to include only studies where photoperiod was manipulated and run lms again
#########################################forcing temperature###############################
c$forcetemp<-as.numeric(c$forcetemp)
treatsum2<-ddply(c,c("datasetID"), summarise,
     mean=mean(forcetemp), max=max(forcetemp), min=min(forcetemp),meangrow=mean(growing.lat), meanprov=mean(provenance.lat))
treatsum2$yesforce<-ifelse(treatsum2$max==treatsum2$min,"no-manipulation","manipulation")
treatsum2<-filter(treatsum2, meanprov>0)
q<-ggplot(treatsum2, aes(x=max, y=meanprov, color=yesforce))
q+geom_point()+labs(title="Maximum experimental forcing", x="Max forcing", y="Latitude")
r<-ggplot(treatsum2, aes(x=min, y=meanprov, color=yesforce))
r+geom_point()+labs(title="Minimum experimental forcing", x="Min forcing", y="Latitude")
Modmax2<-lm(max~meanprov,data=treatsum2)
summary(Modmax2)
Modmin2<-lm(min~meanprov,data=treatsum2)
summary(Modmin2)


### let look just where thing were manipulated
treatsum<-filter(treatsum,yesphoto=="manipulation")
treatsum2<-filter(treatsum2,yesforce=="manipulation")
#lms with only manipulated value
Modmax<-lm(max~meanprov,data=treatsum)
summary(Modmax)
Modmin<-lm(min~meanprov,data=treatsum)
summary(Modmin)
Modmax2<-lm(max~meanprov,data=treatsum2)
summary(Modmax2)
Modmin2<-lm(min~meanprov,data=treatsum2)
summary(Modmin2)
##max min photo for manipulated only
q<-ggplot(treatsum, aes(x=max, y=meanprov, color=yesphoto))
q+geom_point()+labs(title="Maximum experimental photoperiod", x="Max photoperiod", y="Latitude")
r<-ggplot(treatsum, aes(x=min, y=meanprov, color=yesphoto))
r+geom_point()+labs(title="Minimum experimental photoperiod", x="Min photoperiod", y="Latitude")
##max and min forcing for manipulated only
q<-ggplot(treatsum2, aes(x=max, y=meanprov, color=yesforce))
q+geom_point()+labs(title="Maximum experimental forcing", x="Max forcing", y="Latitude")
r<-ggplot(treatsum2, aes(x=min, y=meanprov, color=yesforce))
r+geom_point()+labs(title="Minimum experimental forcing", x="Min forcing", y="Latitude")

#### LETS LOOK AT CHILLING
chill<-read.csv("ospree_clean_withchill.csv",header = TRUE)
chill$Field_Utah_Model<-as.numeric(chill$Field_Utah_Model)
chill$Exp_Utah_Model<-as.numeric(chill$Exp_Utah_Model)
chill$Exp_Chilling_Hours<-as.numeric(chill$Exp_Chilling_Hours)
chill$Field_Chilling_Hours<-as.numeric(chill$Field_Chilling_Hours)
chill$Exp_Chill_portions<-as.numeric(chill$Exp_Chill_portions)
chill$Field_Chill_portions<-as.numeric(chill$Field_Chill_portions)
###gather all of these so they can be compared on the same graph...this is incomplete
treat<-gather(chill,ex.calculation,ex.chillunits,Exp_Chilling_Hours:Exp_Chill_portions)
treat<-gather()
###not neccisarily using this part below
treat<-treatsum<-ddply(chill,c("datasetID"), summarise,
           maxFU=max(Field_Utah_Model), minFU=min(Field_Utah_Model),
         maxEU=max(Exp_Utah_Model), minEU=min(Exp_Utah_Model),
         maxECH=max(Exp_Chilling_Hours), minECH=min(Exp_Chilling_Hours),
         maxFCH=max(Field_Chilling_Hours),minFCH=min(Field_Chilling_Hours),
          maxECP=max(Exp_Chill_portions), minECP=min(Exp_Chill_portions),
         maxFCP=max(Field_Chill_portions),minFCP=min(Field_Chill_portions),
         meanprov=mean(provenance.lat))

