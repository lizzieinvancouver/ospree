rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(brms)
set.seed(613)

treatments<-seq(0,500, by=50)## chilling levels in the whole data base

nsp<-1:50 #number of species

baseinter<-200 #base day of budburst
spint <- baseinter +rnorm(nsp,0,3)

baseeff<--.3 ##baseline effect size
speff<- baseeff+rnorm(nsp,0,.5)
sigma<-0.01

rep<-1:50
###each species get different 5 treatment levels

##response is linear species have true differences in response
df<-data.frame(species=factor(),chilling=numeric(),doy=numeric())

for (i in (1:length(nsp))){
chilling<-rnorm(5,sample(treatments, 1),50)
for (k in (1:length(rep))){
y<-spint+speff*chilling
dfhere<-data.frame(species=nsp[i],chilling=chilling,doy=rnorm(50,y,sigma))
df<-rbind(df,dfhere)
}}

df2<-data.frame(species=factor(),chilling=numeric(),doy=numeric())

for (i in (1:length(nsp))){
  chilling<-rnorm(5,sample(treatments, 1),50)
  for (k in (1:length(rep))){
    y<-baseinter+baseeff*chilling
    dfhere2<-data.frame(species=nsp[i],chilling=chilling,doy=rnorm(50,y,sigma))
    df2<-rbind(df2,dfhere2)
  }}


##withing group centering should be the same


##I think the problem would be if a) chilling is non linear. b) there are actually small species differences

df3<-data.frame(factor=numeric(),chilling=numeric(),doy=numeric())

for (i in (1:length(nsp))){
  chilling<-rnorm(5,sample(treatments, 1),50)
  for (k in (1:length(rep))){
    y<-baseinter+baseeff*chilling-0.01*(baseeff*chilling)^2
    dfhere3<-data.frame(species=nsp[i],chilling=chilling,doy=rnorm(50,y,sigma))
    df3<-rbind(df3,dfhere3)
  }}


df4<-data.frame(species=factor(),chilling=numeric(),doy=numeric())

for (i in (1:length(nsp))){
  chilling<-rnorm(5,sample(treatments, 1),50)
  for (k in (1:length(rep))){
    y<-spint+speff*chilling-0.01*(baseeff*chilling)^2
    dfhere4<-data.frame(species=nsp[i],chilling=chilling,doy=rnorm(50,y,sigma))
    df4<-rbind(df4,dfhere4)
  }}



a<-ggplot(df,aes(chilling,doy))+geom_smooth(method="lm",se=FALSE,fullrange=TRUE,aes(color=as.factor(species)))+geom_point(aes(color=as.factor(species)),size=1,alpha=0.1)

b<-ggplot(df2,aes(chilling,doy))+geom_smooth(method="lm",se=FALSE,fullrange=TRUE,aes(color=as.factor(species)))+geom_point(aes(color=as.factor(species)),size=1,alpha=0.1)

c<-ggplot(df3,aes(chilling,doy))+geom_smooth(method="lm",se=FALSE,fullrange=TRUE,aes(color=as.factor(species)))+geom_point(aes(color=as.factor(species)),size=1,alpha=0.1)

d<-ggplot(df4,aes(chilling,doy))+geom_smooth(method="lm",se=FALSE,fullrange=TRUE,aes(color=as.factor(species)))+geom_point(aes(color=as.factor(species)),size=1,alpha=0.1)


###within group centering

mean.data<- df %>% group_by(species) %>% summarise(meanchill=mean(chilling))

df<-left_join(df,mean.data)
df$treatvar<-df$chilling-df$meanchill
mod1<-brm(doy~chilling+(chilling|species),data=df)

mod1.cent<-brm(doy~meanchill+treatvar+(meanchill+treatvar|species),data=df)


mean.data3<- df3 %>% group_by(species) %>% summarise(meanchill=mean(chilling))


df3<-left_join(df3,mean.data3)
df3$treatvar<-df3$chilling-df3$meanchill

mod3<-brm(doy~chilling+(chilling|species),data=df3)
mod3.cent<-brm(doy~meanchill+treatvar+(meanchill+treatvar|species),data=df3)





###compare this to a dataset where the treatments are even


treatments2<-seq(0,500,by=100)


df5<-data.frame(species=factor(),chilling=numeric(),doy=numeric())

for (i in (1:length(nsp))){
  chilling<-sample(treatments2, 5)
  for (k in (1:length(rep))){
    y<-spint+speff*chilling
    dfhere5<-data.frame(species=nsp[i],chilling=chilling,doy=rnorm(50,y,sigma))
    df5<-rbind(df5,dfhere5)
  }}

mean.data5<- df5 %>% group_by(species) %>% summarise(meanchill=mean(chilling))

df5<-left_join(df5,mean.data5)
df5$treatvar<-df5$chilling-df5$meanchill
mod5.cent<-lmer(doy~meanchill+treatvar+(meanchill+treatvar|species),data=df5)

mean.data3<- df3 %>% group_by(species) %>% summarise(meanchill=mean(chilling))


df3<-left_join(df3,mean.data3)
df3$treatvar<-df3$chilling-df3$meanchill

mod3<-lmer(doy~chilling+(chilling|species),data=df3)
mod3.cent<-lmer(doy~meanchill+treatvar+(meanchill+treatvar|species),data=df3)

ggpubr::ggarrange(a,b,c,d)