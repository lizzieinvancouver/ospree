## By Dan B.
## Simulation for requirements (e.g., GDD) vs .sensitivity (e.g., days per C in an experiment)
## Sent to lab December 2020

rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(brms)
library(dplyr)
library(broom)
library(ggplot2)
library(tibble)
library(ggstance)

set.seed(1000)

inds<-1:50
df<-data.frame(tree.id=numeric(),chill=numeric(),bb.hi=numeric(),bb.low=numeric()) ## make a data frame
##below simulates the f* for 2 differennt species
for(j in c(1:length(inds))){
  chill<-sample(c(0,1,2),1)#randomly select low or high chill
  bb.hi<-rnorm(1,600,10)-(rnorm(1,80,10)*chill) #notice all that is different from below is F*
  bb.low<-rnorm(1,200,10)-(rnorm(1,30,10)*chill)
  dfhere<-data.frame(tree.id=inds[j],chill=chill,bb.hi=bb.hi,bb.low=bb.low)
  
  df<-rbind(df,dfhere)}


df.sum<-df %>% dplyr::group_by(chill) %>%dplyr::summarise(mean.bb.low=mean(bb.low),mean.bb.high=mean(bb.hi))


df.sum[3,2]-df.sum[1,2] ## -66   (less than 75 diff) no chilling requirement

##BB.hi
df.sum[3,3]-df.sum[1,3] #-145 (greater than 75 diff)
df.sum[3,3]-df.sum[2,3] ### 71 

#side point, not true for forcing
weather<-data.frame(airt=rep(c(20,10),each=100),doe=rep(1:100,2)) ## set the temperature for each day of the experiment
weather$heatsum<-weather$airt-5 ##this calculate the GDD of each day
weather<-weather %>%group_by(airt) %>% mutate(GDD = cumsum(heatsum))

indsfoce<-1:60 ### now we put 600 cuttings in tyhe chambers
                                              
df<-data.frame(tree.id=numeric(),sp1=numeric(),sp2=numeric()) 
for(j in c(1:length(inds))){
  sp1<-rnorm(1,200,25) #notice all that is different from below is F*
  sp2<-rnorm(1,400,25)
  dfhere<-data.frame(tree.id=inds[j],sp1=sp1,sp2=sp2)
  df<-rbind(df,dfhere)
} 

df$airt<-sample(c(20,10),nrow(df),replace=TRUE)### now assign each a temperature treatment 
experiment<-left_join(weather,df)

sp1<-experiment%>% group_by(tree.id) %>% filter(GDD >=sp1) %>% slice(1) ## this pulls out the first time the GDD excede f&
sp2<-experiment%>% group_by(tree.id) %>% filter(GDD >=sp2) %>% slice(1)

lm(doe~airt,data=sp1)
lm(doe~airt,data=sp2)

## sp1 earlier, lower requirement, less sensitive

## sp2 later, higher heat requirement, more sensitive to forcing
