###Started by Dan on March 25 2020 
#to demonstrate why within group centering is/isnt good for bud burst models

###update 26 Mar 2020: I don't really see within group centering changing the models outputs so much
###maybe im fundamentally misinterpreting what is supposed to happen, someone else should look

rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(brms)
set.seed(613)

treatments<-seq(0,500, by=50)## chilling levels
baseinter<-200 #base day of budburst


baseeff<--.3 ##baseline effect size

sigma<-0.1

study<-as.factor(1:3) ### three differnt studies on the same species
rep<-1:50 ### each study has 50 replicated for each treatment level



### generate the data
df<-data.frame(treatments=numeric(),doy=numeric(),studyID=factor())

for (i in (1:length(study))){
for (k in (1:length(rep))){
y<-baseinter+baseeff*treatments
dfhere<-data.frame(treatments=treatments,doy=rnorm(length(y),y,sigma),studyID=study[i])
df<-rbind(df,dfhere)
}}

###In this world (the world of studyID) funding and effort is unlimited and all three research labs
##test the same species across all 11 chilling levels (0-500)

###but in the world of studyID2 resources are limited 
##lab 1 is far south, so their weinberger treatment allows for on 3 levels of chilling (0,50,100)

##lab 2 is at medium lattitudes but they get a late start int their collections 
#so some chilling has already occured before the start and their levels are(150,200,350,300,350)

##lab Is only really interested in how extreme chilling affects budburst so they apply 3 treatments (400,450, 500)
##this real word scenario is created below
df$studyID2<-NA
df$studyID2[which(df$treatments<=100)]<-1
df$studyID2[which(df$treatments>100)]<- 2
df$studyID2[which(df$treatments>=400)]<- 3

df$studyID2<-as.factor(df$studyID2)



###do the within group centering
meanies<-df %>% group_by(studyID2) %>% summarise(mean_treat=mean(treatments)) ##take the mean chilling value of each study
df<-left_join(df,meanies) ##merge that back to main data frame
df$var_treat<-df$treatments-df$mean_treat ### center

###run models
tru.mod<-lmer(doy~treatments+(treatments|studyID),data=df) ## ideal world
treat.mod<-lmer(doy~treatments+(treatments|studyID2),data=df) ## real world
wgc.treat.mod<-lmer(doy~mean_treat+var_treat+(treatments|studyID2),data=df) #centered real world

summary(tru.mod)
summary(treat.mod)
summary(wgc.treat.mod)
### it seems in this scenario all models give the same results

###but since we know weinberger isnt perfect chilling
##let's imaging that each study has a slightly different effect of chilling


df2<-data.frame(treatments=numeric(),doy=numeric(),studyID=factor())

for (i in (1:length(study))){
  steff<-baseeff + rnorm(1,0,.1)
  for (k in (1:length(rep))){
    y<-baseinter+steff*treatments
    dfhere2<-data.frame(treatments=treatments,doy=rnorm(length(y),y,sigma),studyID=study[i])
    df2<-rbind(df2,dfhere2)
  }}

df2$studyID2<-NA
df2$studyID2[which(df2$treatments<=100)]<-1
df2$studyID2[which(df2$treatments>100)]<- 2
df2$studyID2[which(df2$treatments>=400)]<- 3

df2$studyID2<-as.factor(df2$studyID2)

meanies2<-df2 %>% group_by(studyID2) %>% summarise(mean_treat=mean(treatments))

df2<-left_join(df2,meanies2)
df2$var_treat<-df2$treatments-df2$mean_treat

#### run models again
tru.mod2<-lmer(doy~treatments+(treatments|studyID),data=df2)
treat.mod2<-lmer(doy~treatments+(treatments|studyID2),data=df2)
wgc.treat.mod2<-lmer(doy~mean_treat+var_treat+(treatments|studyID2),data=df2)


summary(tru.mod2)
summary(treat.mod2)
summary(wgc.treat.mod2)

##again the results are pretty much all the same

###van de pol say one of the problems may occur with unbalanced data
###lets try un balancing it

df2<-arrange(df2,studyID2)### order datasheet by study ID

df2.unbal<-df2[c(1:300,601:1300),] ###now study 1 has only 275 rows, study 2 has 452, study 3 has 273

tru.mod2.unbal<-lmer(doy~treatments+(treatments|studyID),data=df2.unbal)
treat.mod2.unbal<-lmer(doy~treatments+(treatments|studyID2),data=df2.unbal)
wgc.treat.mod2.unbal<-lmer(doy~mean_treat+var_treat+(treatments|studyID2),data=df2.unbal)

summary(tru.mod2.unbal) #this one doesn't converge
summary(treat.mod2.unbal)
summary(wgc.treat.mod2.unbal)
### but all 3 are still giving the same answer

###I suspect the true problem is if there is actualyl no differnce between studies, 
##and the chilling response is nonlinear, than it will seem like there are major difference between
##studies when a linear model is fit
###yet i cant see how within group centering would fix this...but we'll see


#### try adding a slight nonliniarity
df3<-data.frame(treatments=numeric(),doy=numeric(),studyID=factor())

for (i in (1:length(study))){
  for (k in (1:length(rep))){
    y<-baseinter-(-baseeff*treatments)^1.2
    df3here<-data.frame(treatments=treatments,doy=rnorm(length(y),y,sigma),studyID=study[i])
    df3<-rbind(df3,df3here)
  }}

ggplot(df3,aes(treatments,doy))+geom_point()
df3$studyID2<-NA
df3$studyID2[which(df3$treatments<=100)]<-1
df3$studyID2[which(df3$treatments>100)]<- 2
df3$studyID2[which(df3$treatments>=400)]<- 3

df3$studyID2<-as.factor(df3$studyID2)



###do the within group centering
meanies<-df3 %>% group_by(studyID2) %>% summarise(mean_treat=mean(treatments))
df3<-left_join(df3,meanies)
df3$var_treat<-df3$treatments-df3$mean_treat

tru.mod3<-lmer(doy~treatments+(treatments|studyID),data=df3)
treat.mod3<-lmer(doy~treatments+(treatments|studyID2),data=df3)
wgc.treat.mod3<-lmer(doy~mean_treat+var_treat+(treatments|studyID2),data=df3)


summary(tru.mod3)
summary(treat.mod3)
summary(wgc.treat.mod3)
ranef(treat.mod3)
ranef(wgc.treat.mod3)
###all these models are mis estimating the intercept (which is to be expected) but wgc model doesn't help
