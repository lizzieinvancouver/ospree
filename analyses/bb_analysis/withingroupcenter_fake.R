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

treats<-seq(0,800, by=50)## chilling levels
baseinter<-200 #base day of budburst


baseeff<--.3 ##baseline effect size

sigma<-1

study<-as.factor(1:20) ### three differnt studies on the same species
rep<-1:100 ### each study has 50 replicated for each treatment level



### generate the data
df<-data.frame(treatments=numeric(),doy=numeric(),studyID=factor())

for (i in (1:length(study))){
  treatments<-sample(treats,sample(3:10))
for (k in (1:length(rep))){
y<-baseinter+baseeff*treatments
dfhere<-data.frame(treatments=treatments,doy=rnorm(length(y),y,sigma),studyID=study[i])
df<-rbind(df,dfhere)
}}

distinct(dplyr::select(df,treatments,studyID))



###do the within group centering
meanies<-df %>% dplyr::group_by(studyID) %>% dplyr::summarise(mean_treat=mean(treatments)) ##take the mean chilling value of each study
df<-left_join(df,meanies) ##merge that back to main data frame
df$var_treat<-df$treatments-df$mean_treat ### center

ggplot(df,aes(studyID,treatments))+stat_summary()
###run models
super.true<-lm(doy~treatments,data=df)
tru.mod<-lmer(doy~treatments+(1|studyID),data=df) ## ideal world
treat.mod<-lmer(doy~treatments+(1|studyID),data=df) ## real world
wgc.treat.mod<-lmer(doy~mean_treat+var_treat+(1|studyID),data=df) #centered real world

summary(tru.mod)
summary(treat.mod)
summary(wgc.treat.mod)
### it seems in this scenario all models give the same results

###but since we know weinberger isnt perfect chilling
##let's imaging that each study has a slightly different intercept of chilling


df2<-data.frame(treatments=numeric(),doy=numeric(),studyID=factor())


for (i in (1:length(study))){
  treatments2<-sample(treats,sample(3:10))
  baseinter<-rnorm(1,baseinter,5)
  for (k in (1:length(rep))){
    y<-baseinter+baseeff*treatments2
    dfhere2<-data.frame(treatments=treatments2,doy=rnorm(length(y),y,sigma),studyID=study[i])
    df2<-rbind(df2,dfhere2)
  }}



meanies2<-df2 %>% group_by(studyID) %>% dplyr::summarise(mean_treat=mean(treatments))

df2<-left_join(df2,meanies2)
df2$var_treat<-df2$treatments-df2$mean_treat

#### run models again
tru.mod2<-lm(doy~treatments,data=df2)
treat.mod2<-lmer(doy~treatments+(1|studyID),data=df2)
wgc.treat.mod2<-lmer(doy~mean_treat+var_treat+(1|studyID),data=df2)


summary(tru.mod2)
summary(treat.mod2)
summary(wgc.treat.mod2)

###I suspect the true problem is if there is actualyl no differnce between studies, 
##and the chilling response is nonlinear, than it will seem like there are major difference between
##studies when a linear model is fit
###yet i cant see how within group centering would fix this...but we'll see


#### try adding a slight nonliniarity
df3<-data.frame(treatments=numeric(),doy=numeric(),studyID=factor())


for (i in (1:length(study))){
  treatments3<-sample(treats,sample(3:10))
  baseinter<-rnorm(1,baseinter,5)
  for (k in (1:length(rep))){
    y<-baseinter+baseeff*treatments3+.002*(baseeff*treatments3)^2
    dfhere3<-data.frame(treatments=treatments3,doy=rnorm(length(y),y,sigma),studyID=study[i])
    df3<-rbind(df3,dfhere3)
  }}



meanies3<-df3 %>% group_by(studyID) %>% dplyr::summarise(mean_treat=mean(treatments))

df3<-left_join(df3,meanies3)
df3$var_treat<-df3$treatments-df3$mean_treat

#### run models again
tru.mod3<-lm(doy~treatments,data=df3)
treat.mod3<-lmer(doy~treatments+(1|studyID),data=df3)
wgc.treat.mod3<-lmer(doy~mean_treat+var_treat+(1|studyID),data=df3) ####slightly worse



summary(tru.mod3)
summary(treat.mod3)
summary(wgc.treat.mod3)
ranef(treat.mod3)
ranef(wgc.treat.mod3)
###all these models are mis estimating the intercept (which is to be expected) but wgc model doesn't help
