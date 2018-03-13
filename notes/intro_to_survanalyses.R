#Analyses suggested by reviewers for GCB Transplant Study
#Started by Ailene Ettinger on October 6, 2016
setwd("~/git/mora_transplant")
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(dplyr)
library(plyr)
library(survival)
library(car)
library(lme4)
library(RColorBrewer)
#Begin with transplant data: get the survival times, growth variables, etc
transdat<-read.csv("data/2013TransplantStatusHeight(October).csv", header=TRUE)#csv file has been sorted so that all dates appear together, all status columns are together, etc
dim(transdat)#3959 rows(=individuals),  38 columns
transdat$PlantedStand2<-as.numeric(transdat$PlantedStand)
transdat$PlantedStand<-as.factor(transdat$PlantedStand)
transdat$OriginStand<-as.factor(transdat$OriginStand)
transdat$Block<-as.factor(transdat$Block)
transdat$UniqueID<-as.factor(transdat$UniqueID)
transdat$Date1<-as.Date(transdat$Date1,format='%m/%d/%y')
transdat$Date2<-as.Date(transdat$Date2,format='%m/%d/%y')
transdat$Date3<-as.Date(transdat$Date3,format='%m/%d/%y')
transdat$Date4<-as.Date(transdat$Date4,format='%m/%d/%y')
transdat$Date5<-as.Date(transdat$Date5,format='%m/%d/%y')
transdat[which(transdat$Date3=="2025-06-14"),]$Date3<-"0012-06-14"#fix typo
#Add column for survival data  (0=alive, 1=dead)
transdat$Death=NA
transdat[which(transdat$StatusDate5==0),]$Death=1
transdat[which(transdat$StatusDate5==1),]$Death=0
#Add column for time to death in days
transdat$StartDate=as.Date("9/1/2010",format='%m/%d/%Y')
transdat$StartDateDate5=transdat$Date5-transdat$StartDate
transdat$StartDateDate4=transdat$Date4-transdat$StartDate
transdat$StartDateDate3=transdat$Date3-transdat$StartDate
transdat$StartDateDate2=transdat$Date2-transdat$StartDate
transdat$StartDateDate1=transdat$Date1-transdat$StartDate
DaysDeath<-c()
alltime1<-c()
alltime2<-c()
for (i in 1:dim(transdat)[1]){
  if(transdat$StatusDate5[i]==1)inddd<-transdat$StartDateDate5[i]
  if(transdat$StatusDate5[i]==0 & transdat$StatusDate4[i]==1) inddd<-transdat$StartDateDate5[i]
  if(transdat$StatusDate3[i]==1 & transdat$StatusDate4[i]==0) inddd<-transdat$StartDateDate4[i]
  if(transdat$StatusDate2[i]==1 & transdat$StatusDate3[i]==0) inddd<-transdat$StartDateDate3[i]
  if(transdat$StatusDate1[i]==1 & transdat$StatusDate2[i]==0) inddd<-transdat$StartDateDate2[i]
  if(transdat$StatusDate1[i]==0) inddd<-transdat$StartDateDate1[i]
  DaysDeath<-c(DaysDeath,inddd)
  #for interval censored data, need to specificy that we know it died between which intervals
  #If alive on final census, time2=NA, time1=#ofday on last census
  if(transdat$StatusDate5[i]==1)time1<-transdat$StartDateDate5[i]
  if(transdat$StatusDate5[i]==1)time2<-NA
  #If dead on final census, but alive on 4th census, time2=#ofdaydate5, time1=#ofdaydate4
  if(transdat$StatusDate5[i]==0 & transdat$StatusDate4[i]==1) time2<-transdat$StartDateDate5[i]
  if(transdat$StatusDate5[i]==0 & transdat$StatusDate4[i]==1) time1<-transdat$StartDateDate4[i]
  #If dead on 4th census, but alive on 3rd census, time2=#ofdaydate4, time1=#ofdaysondate3
  if(transdat$StatusDate3[i]==1 & transdat$StatusDate4[i]==0) time2<-transdat$StartDateDate4[i]
  if(transdat$StatusDate3[i]==1 & transdat$StatusDate4[i]==0) time1<-transdat$StartDateDate3[i]
  #If dead on 3rd census, but alive on 2nd census, time2=#ofdaydate3, time1=#ofdaysondate2
  if(transdat$StatusDate2[i]==1 & transdat$StatusDate3[i]==0) time2<-transdat$StartDateDate3[i]
  if(transdat$StatusDate2[i]==1 & transdat$StatusDate3[i]==0) time1<-transdat$StartDateDate2[i]
  #If dead on 2nd census, but alive on 1st census, time2=#ofdaydate2, time1=#ofdaysondate1
  if(transdat$StatusDate1[i]==1 & transdat$StatusDate2[i]==0) time2<-transdat$StartDateDate2[i]
  if(transdat$StatusDate1[i]==1 & transdat$StatusDate2[i]==0) time1<-transdat$StartDateDate1[i]
  #If dead on 1st census, time2=#ofdaydate1, time1=14 (all plants checked after 2 weeks and still alive)
  if(transdat$StatusDate1[i]==0) time2<-transdat$StartDateDate1[i]
  if(transdat$StatusDate1[i]==0) time1<-14
  alltime1<-c(alltime1,time1)
  alltime2<-c(alltime2,time2)
}
transdat$DaysDeath=DaysDeath
transdat$time1=as.numeric(alltime1)
transdat$time2=alltime2
tsmedat<-transdat[transdat$Species=="TSME",]
tsmedat$PlantedStand=factor(tsmedat$PlantedStand)
tsmedat$OriginStand=factor(tsmedat$OriginStand)
tsmedat$Block=factor(tsmedat$Block)

#Fit Kaplan-Meier estimate, a nonparametric maximum likelihood estimate (MLE) of the survival
#function, S(t). This estimate is a step function with jumps at observed event times.
surv.tsme<-survfit(Surv(time1,time2, type="interval2")~1, data=tsmedat)
summary(surv.tsme)

quartz()
plot(surv.tsme)

surv2.tsme<-survfit(Surv(time1,time2, type="interval2")~PlantedStand, data=tsmedat)
quartz()
plot(surv2.tsme, col=c("darkred","red","gray","blue","darkblue"))

#Fit accelerated failure-time model, which is a parametric model with covariates and failure times
#TSME
constmod.tsme<-survreg(Surv(time1,time2, type="interval2")~PlantedStand+OriginStand+Canopy+Understory+PlantedStand:OriginStand+PlantedStand:Canopy+PlantedStand:Understory+OriginStand:Canopy+OriginStand:Understory+Canopy:Understory+PlantedStand:Canopy:Understory, dist="lognormal", data=tsmedat)
summary(constmod.tsme)
coef(constmod.tsme)#coefficients are on a log scale
exp(coef(constmod.tsme))[1]#because the predictors are categorical, we can exponentiate to get estimated survival time (in days)
      #for reference group (=lowest elevation stand=1064)
exp(coef(constmod.tsme)[1]+coef(constmod.tsme)[2])#survival time at next highest elevation

Anova(constmod.tsme, test.statistic="LR", type="III")
plot(constmod.tsme)
