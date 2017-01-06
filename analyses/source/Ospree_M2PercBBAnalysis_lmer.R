## Started 7 July 2016 ##
## By Ailene, after modifying Dan and Lizzie's code ##
##Try fitting the model for percbb in lme4 since Stan is not working great for me right now
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(lme4)
library(car)

if(length(grep("danflynn", getwd())>0)) { # set to DF working directory if DF computer.
  setwd("~/Documents/git/ospree") 
  } else setwd("~/git/ospree")

ospree <- read.csv("input/ospree_clean_withchill.csv", header=TRUE)

ospree.percbb <- ospree[which(ospree$respvar.simple=="percentbudburst"),]

dim(ospree)
dim(ospree.percbb) # 3371 rows

ospree.percbb$spp <- paste(ospree.percbb$genus, ospree.percbb$species, sep=".")

ospree.percbb <- subset(ospree.percbb, is.na(spp)==FALSE)
ospree.percbb<-subset(ospree.percbb,as.numeric(ospree.percbb$response)<800)#remove the weird value for perc budburst
#response variable = percent budburst:
ospree.percbb$response <- as.numeric(ospree.percbb$response)#this is percbudburst
##random effects
ospree.percbb$datasetID <- as.factor(ospree.percbb$datasetID)
ospree.percbb$spp <- as.factor(ospree.percbb$spp)
#predictors:
ospree.percbb$responsedays <- as.numeric(ospree.percbb$response.time)
ospree.percbb$totalchill <- as.numeric(ospree.percbb$Total_Chilling_Hours)
ospree.percbb$forcetemp <- as.numeric(ospree.percbb$forcetemp)
ospree.percbb$photoperiod_day <- as.numeric(ospree.percbb$photoperiod_day)

###create centered predictors so that we can compare their slopes
ospree.percbb$responsedays_cent <- as.numeric(ospree.percbb$response.time)-
      mean(as.numeric(ospree.percbb$response.time),na.rm=TRUE)
ospree.percbb$totalchill_cent <- as.numeric(ospree.percbb$Total_Chilling_Hours)-
      mean(as.numeric(ospree.percbb$Total_Chilling_Hours),na.rm=TRUE)
ospree.percbb$forcetemp_cent <- as.numeric(ospree.percbb$forcetemp)-
      mean(as.numeric(ospree.percbb$forcetemp),na.rm=TRUE)
ospree.percbb$photoper_cent <- as.numeric(ospree.percbb$photoperiod_day)-
      mean(as.numeric(ospree.percbb$photoperiod_day),na.rm=TRUE)
###create zscore predictors 
ospree.percbb$responsedays_z <- (as.numeric(ospree.percbb$response.time)-
                                      mean(as.numeric(ospree.percbb$response.time),na.rm=TRUE))/sd(as.numeric(ospree.percbb$response.time),na.rm=TRUE)
ospree.percbb$totalchill_z <- (as.numeric(ospree.percbb$Total_Chilling_Hours)-
                                    mean(as.numeric(ospree.percbb$Total_Chilling_Hours),na.rm=TRUE))/sd(as.numeric(ospree.percbb$Total_Chilling_Hours),na.rm=TRUE)
ospree.percbb$forcetemp_z <- (as.numeric(ospree.percbb$forcetemp)-
                                   mean(as.numeric(ospree.percbb$forcetemp),na.rm=TRUE))/sd(as.numeric(ospree.percbb$forcetemp),na.rm=TRUE)
ospree.percbb$photoper_z <- (as.numeric(ospree.percbb$photoperiod_day)-
                                  mean(as.numeric(ospree.percbb$photoperiod_day),na.rm=TRUE))/sd(as.numeric(ospree.percbb$photoperiod_day),na.rm=TRUE)

##look at how centering changes the distribution:
#quartz()
par(mfrow=c(1,2))
hist(ospree.percbb$responsedays);hist(ospree.percbb$responsedays_cent)  # some response days over 300??

#quartz()
par(mfrow=c(1,2))
hist(ospree.percbb$totalchill);hist(ospree.percbb$totalchill_cent)

#quartz()
par(mfrow=c(1,2))
hist(ospree.percbb$forcetemp);hist(ospree.percbb$forcetemp_cent)

###Try fitting model with lmer:
##First a simple model:

mod <- lmer(response~responsedays+(responsedays|datasetID/spp), data=ospree.percbb)

#this model does not converge...get a warning message
#difficult to put any random slopes in...

#try with just species as random effect and crossed random effect of dataset ID. ** Species 
mod2 <- lmer(response~responsedays+(responsedays|spp)+(1|datasetID),data=ospree.percbb)
summary(mod2)#as days to budburst increases, perc budburst increases! but 
ranef(mod2)
##Now add chilling, forcing, and photoperiod interactions with days: can't do random slopes- doesn't converge
mod.cfp<-lmer(response~responsedays+responsedays:totalchill+responsedays:forcetemp+responsedays:photoperiod_day+(1|spp)+(1|datasetID),data=ospree.percbb)
summary(mod.cfp)#as days to budburst increases, perc budburst increases, but 
#there's a warning about scaling of variables so...

##fit same model, but using centered predictors:
mod.cfp_cent<-lmer(response~responsedays_cent+responsedays_cent:totalchill_cent+responsedays_cent:forcetemp_cent+responsedays_cent:photoper_cent+(1|spp)+(1|datasetID),data=ospree.percbb)
summary(mod.cfp_cent)#as days to budburst increases, perc budburst increases
###So,according to this model, increasing the days to budburst leads to increases in percent budburst
#Interaction between days and chilling is negative, suggesting that chilling decreases the positive effect of days on %budburst
#interaction between days and forcing is negative, suggesting that forcing also decreases the positive effect of days on %budburst
#interaction between days and photo-period is positive, suggesting that longer days increase the positive effect of days on % budburst, but this effect is small
#if using z-scores instead, the sign of these effects is the same but the magnitude is different
#(chilling has the biggest effect size using zscored predictors)

########Now look at days to budburst model
ospree.days <- ospree[which(ospree$respvar.simple=="daystobudburst"),]

dim(ospree.days) # 2827 rows

ospree.days$spp <- paste(ospree.days$genus, ospree.days$species, sep=".")
# deal with response vs. responsetime (quick fix for now)
resp1 <- subset(ospree.days, response==1) # most of are data is like this
resp1.timeNA <- subset(resp1, is.na(response.time)==TRUE) # about 20 rows have this
ospree.days$responsedays <- ospree.days$response.time
ospree.days$responsedays[which(ospree.days$response>1 & ospree.days$response.time=="")] <- ospree.days$response

#response variable = days to budburst
ospree.days$responsedays <- as.numeric(ospree.days$responsedays)#
##random effects
ospree.days$datasetID <- as.factor(ospree.days$datasetID)
ospree.days$spp <- as.factor(ospree.days$spp)
#predictors:
ospree.days$totalchill <- as.numeric(ospree.days$Total_Chilling_Hours)
ospree.days$forcetemp <- as.numeric(ospree.days$forcetemp)
ospree.days$photoperiod_day <- as.numeric(ospree.days$photoperiod_day)
ospree.days$provenance.lat<-as.numeric(ospree.days$provenance.lat)

###create centered predictors so that we can compare their slopes in our models
ospree.days$responsedays_cent <- as.numeric(ospree.days$response.time)-
  mean(as.numeric(ospree.days$response.time),na.rm=TRUE)
ospree.days$totalchill_cent <- as.numeric(ospree.days$Total_Chilling_Hours)-
  mean(as.numeric(ospree.days$Total_Chilling_Hours),na.rm=TRUE)
ospree.days$forcetemp_cent <- as.numeric(ospree.days$forcetemp)-
  mean(as.numeric(ospree.days$forcetemp),na.rm=TRUE)
ospree.days$photoper_cent <- as.numeric(ospree.days$photoperiod_day)-
  mean(as.numeric(ospree.days$photoperiod_day),na.rm=TRUE)
ospree.days$lat_cent <- as.numeric(ospree.days$provenance.lat)-
  mean(as.numeric(ospree.days$provenance.lat),na.rm=TRUE)
###create zscore predictors 
ospree.days$responsedays_z <- (as.numeric(ospree.days$response.time)-
                                   mean(as.numeric(ospree.days$response.time),na.rm=TRUE))/sd(as.numeric(ospree.days$response.time),na.rm=TRUE)
ospree.days$totalchill_z <- (as.numeric(ospree.days$Total_Chilling_Hours)-
                                 mean(as.numeric(ospree.days$Total_Chilling_Hours),na.rm=TRUE))/sd(as.numeric(ospree.days$Total_Chilling_Hours),na.rm=TRUE)
ospree.days$forcetemp_z <- (as.numeric(ospree.days$forcetemp)-
                                mean(as.numeric(ospree.days$forcetemp),na.rm=TRUE))/sd(as.numeric(ospree.days$forcetemp),na.rm=TRUE)
ospree.days$photoper_z <- (as.numeric(ospree.days$photoperiod_day)-
                               mean(as.numeric(ospree.days$photoperiod_day),na.rm=TRUE))/sd(as.numeric(ospree.days$photoperiod_day),na.rm=TRUE)
ospree.days$lat_z <- (as.numeric(ospree.days$provenance.lat)-
  mean(as.numeric(ospree.days$provenance.lat),na.rm=TRUE))/sd(as.numeric(ospree.days$provenance.lat),na.rm=TRUE)
##now fit the model, using centered predictors:
daysmod_cent<-lmer(responsedays~totalchill_cent+forcetemp_cent+photoper_cent+lat_cent+
                     totalchill_cent:forcetemp_cent+totalchill_cent:photoper_cent+
                     totalchill_cent:lat_cent+lat_cent:forcetemp_cent+
                     lat_cent:photoper_cent+totalchill_cent:forcetemp_cent:photoper_cent+(1|spp)+(1|datasetID),data=ospree.days)
summary(daysmod_cent)
##chilling, forcing, daylength and latitude have neg effects on days to budburst; 
##all interactions positive except chilling and forcing, which is negative (& weak)
##using z scores yields similar results (same signs)

###So,according to this model, increasing the days to budburst leads to increases in percent budburst
#In


##############
##Some plots that only sort of show anything....
####try plotting to see effect of chilling?
ospree.percbb$chill.cat<-NA
ospree.percbb[which(ospree.percbb$totalchill<=1020),]$chill.cat<-"1low"
ospree.percbb[which(ospree.percbb$totalchill>1020),]$chill.cat<-"2high"
Anova(mod.cfp)
ranef(mod.cfp)
quartz()
plot.ospree<-subset(ospree.percbb, select=c("response","responsedays", "chill.cat"))
plot.ospree <- plot.ospree[complete.cases(plot.ospree),]
colors<-c("red","blue")[factor(plot.ospree$chill.cat)]
plot(as.numeric(plot.ospree$responsedays),as.numeric(plot.ospree$response),pch=16, col=colors)
