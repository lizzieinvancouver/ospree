### Started by Dan B in February 2017 ####
# Goal: Produce plots to evaluate correlation between latitude of study
# choices for high and low expiermental treatment (forcing temperature, chilling,and photo period)
# Graph outputs are saved in ospree/anayses/figures

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()
##setwd


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses") 
} else if (length(grep("danielbuonaiuto", getwd()))>0) {setwd("~/Documents/git/ospree/analyses/output")}

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

c<-read.csv("ospree_clean.csv",header = TRUE)
c$photoperiod_day<-as.numeric(c$photoperiod_day)

treatsum<-ddply(c,c("datasetID"), summarise,
mean=mean(photoperiod_day), max=max(photoperiod_day), min=min(photoperiod_day),meangrow=mean(growing.lat), meanprov=mean(provenance.lat))
treatsum<-filter(treatsum, meanprov>0)
##seperate which studies use same max and min photoperoioda
treatsum$yesphoto<-ifelse(treatsum$max==treatsum$min,"no-manipulation","manipulation")

###plot for photoperiod
plottingtreatsum<-gather(treatsum, maxmin,value, max:min)
q<-ggplot(plottingtreatsum, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Photoperiod", x=" Latititude", y="photoperiod")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

##linear models for photoperiod
Modmax<-lm(max~meanprov,data=treatsum)
summary(Modmax)
Modmin<-lm(min~meanprov,data=treatsum)
summary(Modmin)

###Forcing

c$forcetemp<-as.numeric(c$forcetemp)
treatsum2<-ddply(c,c("datasetID"), summarise,
     mean=mean(forcetemp), max=max(forcetemp), min=min(forcetemp),meangrow=mean(growing.lat), meanprov=mean(provenance.lat))
treatsum2$yesforce<-ifelse(treatsum2$max==treatsum2$min,"no-manipulation","manipulation")
treatsum2<-filter(treatsum2, meanprov>0)

##Plot for forcing
plottingtreatsum2<-gather(treatsum2, maxmin,value, max:min)
q<-ggplot(plottingtreatsum2, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Forcing", x=" Latititude", y="Forcing level")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

###Linear model
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

##plots for manipulated only
plottingtreatsum<-gather(treatsum, maxmin,value, max:min)
q<-ggplot(plottingtreatsum, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Manipulated Photoperiod", x=" Latititude", y="photoperiod")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)
plottingtreatsum2<-gather(treatsum2, maxmin,value, max:min)
q<-ggplot(plottingtreatsum2, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Manipulated Forcing", x=" Latititude", y="Forcing level")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

#### LETS LOOK AT CHILLING
##make all numeric
chill<-read.csv("ospree_clean_withchill.csv",header = TRUE)
chill$Field_Utah_Model<-as.numeric(chill$Field_Utah_Model)
chill$Exp_Utah_Model<-as.numeric(chill$Exp_Utah_Model)
chill$Exp_Chilling_Hours<-as.numeric(chill$Exp_Chilling_Hours)
chill$Field_Chilling_Hours<-as.numeric(chill$Field_Chilling_Hours)
chill$Exp_Chill_portions<-as.numeric(chill$Exp_Chill_portions)
chill$Field_Chill_portions<-as.numeric(chill$Field_Chill_portions)

treat<-ddply(chill,c("datasetID"), summarise,
           maxFU=max(Field_Utah_Model), minFU=min(Field_Utah_Model),
           maxEU=max(Exp_Utah_Model), minEU=min(Exp_Utah_Model),
           maxTU=max(Total_Utah_Model), minTU=min(Total_Utah_Model),
        maxECH=max(Exp_Chilling_Hours), minECH=min(Exp_Chilling_Hours),
        maxFCH=max(Field_Chilling_Hours),minFCH=min(Field_Chilling_Hours),
        maxTCH=max(Total_Chilling_Hours),minTCH=min(Total_Chilling_Hours), 
        maxECP=max(Exp_Chill_portions), minECP=min(Exp_Chill_portions),
        maxFCP=max(Field_Chill_portions),minFCP=min(Field_Chill_portions),
        maxTCP=max(Total_Chill_portions), minTCP=min(Total_Chill_portions),
        meanprov=mean(provenance.lat))
treat<-filter(treat, meanprov>0)
###########################################
####plotting total chilling
plottingUtah<-gather(treat, maxmin,value, maxTU:minTU)
q<-ggplot(plottingUtah, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Utah Chilling", x=" Latititude", y="Chilling")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

plottingCP<-gather(treat, maxmin,value, maxTCP:minTCP)
q<-ggplot(plottingCP, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Chilling Portions Chilling", x=" Latititude", y="Chilling")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

plottingCH<-gather(treat, maxmin,value, maxTCH:minTCH)
q<-ggplot(plottingCH, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Chilling Hours Chilling", x=" Latititude", y="Chilling")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

#################just for experimental
plottingUtah<-gather(treat, maxmin,value, maxEU:minEU)
q<-ggplot(plottingUtah, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Utah Exp. Chilling", x=" Latititude", y="Chilling")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

plottingCP<-gather(treat, maxmin,value, maxECP:minECP)
q<-ggplot(plottingCP, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Chilling Portions Exp. Chilling", x=" Latititude", y="Chilling")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

plottingCH<-gather(treat, maxmin,value, maxECH:minECH)
q<-ggplot(plottingCH, aes(x=meanprov, y=value, color=maxmin)) 
q+geom_point()+labs(title="Chilling Hours Exp. Chilling", x=" Latititude", y="Chilling")+ stat_smooth(method = "lm", formula = y ~ x, se = FALSE)
###Next step lms for chilling

##Utah
Modmax3<-lm(maxTU~meanprov,data=treat)
summary(Modmax3)
Modmin3<-lm(minTU~meanprov,data=treat)
summary(Modmin3)
Modmax4<-lm(maxEU~meanprov,data=treat)
summary(Modmax4)
Modmin4<-lm(minEU~meanprov,data=treat)
summary(Modmin4)
### chill hours
Modmax5<-lm(maxTCH~meanprov,data=treat)
summary(Modmax5)
Modmin5<-lm(minTCH~meanprov,data=treat)
summary(Modmin5)
Modmax6<-lm(maxECH~meanprov,data=treat)
summary(Modmax6)
Modmin6<-lm(minECH~meanprov,data=treat)
summary(Modmin6)
###chill portion
Modmax7<-lm(maxTCP~meanprov,data=treat)
summary(Modmax7)
Modmin7<-lm(minTCP~meanprov,data=treat)
summary(Modmin7)
Modmax8<-lm(maxECP~meanprov,data=treat)
summary(Modmax8)
Modmin8<-lm(minECP~meanprov,data=treat)
summary(Modmin8)

###instead of removing southern hemisphere I could, make absolute value
