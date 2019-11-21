#started by Dan B on April 8 2019
# Goal to model some of the range dynamics of common ospress species 


rm(list=ls()) 
options(stringsAsFactors = FALSE)
# libraries
library(rstan)
library(dplyr)
library(tibble)
library(shinystan)
library(RColorBrewer)
library(tidyr)
library(gridExtra)
library("ggpubr")
library(ggstance)
library(brms)
library(reshape2)
library(tibble)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")



###run models_stan.R or load model output
#load("stan/output/m2lni_spcompalltypescp_z.Rda")

###choose species for lat model
unique(bb.stan$complex.wname)
rangesps<-c("Fagus_sylvatica","Betula_pendula","Betula_pubescens","Corylus_avellana",
"Picea_abies","Quercus_robur","Abies_alba","Acer_pseudoplatanus","Aesculus_hippocastanum")


concordance<-dplyr::select(bb.stan, complex,complex.wname)
concordance<-unique(concordance)
concordance<-concordance %>% filter(complex.wname %in% rangesps)

###extract posteriors
sample <- rstan::extract(m2l.ni)### takes a while

sample.force <- melt(sample$b_force)
sample.chill <- melt(sample$b_chill)
sample.photo <- melt(sample$b_photo)

names(sample.force) <- c("iter", "complex", "b_force")
names(sample.chill) <- c("iter", "complex", "b_chill")
names(sample.photo) <- c("iter", "complex", "b_photo")

output.df<- left_join(sample.force, sample.chill)
output.df<-left_join(output.df,sample.photo)
output.live.df <- subset(output.df, iter>1500)
output.live.df<-filter(output.live.df,complex %in% concordance$complex)

###read in range data for each species and give it a complex identifyer
fagrange<-read.csv("..//ranges/climate.in.range1980-2017FagSyl.csv")
fagrange$complex<-21
betpenrange<-read.csv("..//ranges/climate.in.range1980-2017BetPen.csv")
betpenrange$complex<-13
betpubrange<-read.csv("..//ranges/climate.in.range1980-2017BetPub.csv")
betpubrange$complex<-14
coryrange<-read.csv("..//ranges/climate.in.range1980-2017CorAve.csv")
coryrange$complex<-19
picearange<-read.csv("..//ranges/climate.in.range1980-2017PicAb1.csv")
picearange$complex<-28
querrange<-read.csv("..//ranges/climate.in.range1980-2017QurRo1.csv")
querrange$complex<-37
##updated ones
abiesrange<-read.csv("..//ranges/output/Climate.in.range.Abies_alba.1980.2017.csv")
abiesrange$complex<-1

acerrange<-read.csv("..//ranges/output/Climate.in.range.Acer_pseudoplatanus.1980.2017.csv")
acerrange$complex<-3

aesrange<-read.csv("..//ranges/output/Climate.in.range.Aesculus_hippocastanum.1980.2017.csv")
aesrange$complex<-6



range.dat<-rbind(fagrange,betpenrange,betpubrange,coryrange,picearange,querrange,abiesrange,acerrange,aesrange) ###bind all species ranges in one data sheet
range.dat.noacaes<-rbind(fagrange,betpenrange,betpubrange,coryrange,picearange,querrange,abiesrange)

range.mns<- range.dat %>% dplyr::group_by(complex) %>% dplyr::summarise(meanSDutah=mean(SDev.Chill.Utah),meanSDgdd=mean(SDev.GDD.sites))
range.mns.no<- range.dat.noacaes %>% dplyr::group_by(complex) %>% dplyr::summarise(meanSDutah=mean(SDev.Chill.Utah),meanSDgdd=mean(SDev.GDD.sites))

range.mns$zSDutah<-(range.mns$meanSDutah-mean(range.mns$meanSDutah))/sd(range.mns$meanSDutah)
range.mns$zSDgdd<-(range.mns$meanSDgdd-mean(range.mns$meanSDgdd))/sd(range.mns$meanSDgdd)


dater<-left_join(output.live.df,range.mns)
dater.no<-left_join(output.live.df,range.mns.no)

colnames(dater)
dater<-left_join(dater,concordance)
###plot raw data
?stat_summary()
ggplot(dater,aes(meanSDutah,b_chill))+stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max,
                                                   aes(color=as.factor(complex.wname)))
library("Hmisc")
dater %>%group_by(complex.wname)%>% summarise(mean=mean(b_chill),sd=sd(b_chill))

nlabels <- unique(dater$complex.wname)

#  To create the median labels, you can use by
means <- c(by(mtcars$mpg, mtcars$cyl, median))

uts<-ggplot(dater,aes(meanSDutah,b_chill,label=complex.wname))+stat_summary(fun.data = "mean_sdl",aes(color=complex.wname))+theme(legend.position = "none")+
geom_text(stat = 'summary', fun.y=mean, aes(label = complex.wname),size=3)+xlim(100,400)


gdd<-ggplot(dater,aes(meanSDgdd,b_chill,label=complex.wname))+stat_summary(fun.data = "mean_sdl",aes(color=complex.wname))+theme(legend.position = "none")+
  geom_text(stat = 'summary', fun.y=mean, aes(label = complex.wname),size=3)+xlim(90,200)


jpeg("..//ranges/firstfig.jpeg",width = 8.6, height = 4, units = 'in', res=200)
ggpubr::ggarrange(gdd,uts,nrow=2)
dev.off()

###make a single data sheet where range can predict cue effect sizes

mod1<-brm(b_chill~meanSDutah+meanSDgdd,data=dater,iter=3000,warmup=2000) 
summary(mod1)
pp_check(mod1,nsamples = 50)

mod1z<-brm(b_chill~zSDutah+zSDgdd,data=dater,iter=3000,warmup=2000) 
summary(mod1z)
extract_coefs<-function(x){rownames_to_column(as.data.frame(fixef(x, summary=TRUE,probs=c(0.025,0.1,0.9,0.975))),"trait")
}

ninesp<-extract_coefs(mod1)
ninesp.noi<-filter(ninesp,trait!="Intercept")
pd=position_dodgev(height=0.4)
nspsplot<-  ggplot(ninesp,aes(Estimate,trait))+geom_point(position=pd,size=3)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q10,xmax=Q90),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")



mod1a<-brm(b_chill~meanSDutah+meanSDgdd,data=dater.no,iter=3000,warmup=2000) 
summary(mod1a)
pp_check(mod1a,nsamples = 50)

sevensp<-extract_coefs(mod1a)
sevensp.noi<-filter(sevensp,trait!="Intercept")
pd=position_dodgev(height=0.1)
sspsplot<-ggplot(sevensp,aes(Estimate,trait))+geom_point(position=pd,size=3)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q10,xmax=Q90),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")+xlim(-3.5,1)

ggpubr::ggarrange(nspsplot,sspsplot)


nspsplot.noi<-  ggplot(ninesp.noi,aes(Estimate,trait))+geom_point(position=pd,size=3)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q10,xmax=Q90),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")

sspsplot.noi<-  ggplot(sevensp.noi,aes(Estimate,trait))+geom_point(position=pd,size=3)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q10,xmax=Q90),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")

ggpubr::ggarrange(nspsplot.noi,sspsplot.noi)

plotmodz<-extract_coefs(mod1z)
ggplot(plotmodz,aes(Estimate,trait))+geom_point(position=pd,size=3)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q10,xmax=Q90),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")


##Questions zscore sdev predictors