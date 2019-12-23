#started by Dan B on April 8 2019
# Goal to model some of the range dynamics of common ospress species 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")



#####################################
#STEP 1: RUN THE BB MODEL
######################################
# Flags to choose for bbstanleadin.R #
######################################

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- TRUE
use.flags.for.allsppmodel <- FALSE
use.yourown.flagdesign <- FALSE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
   use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
   & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp =FALSE # for the main model this is false
  use.multcuespp = FALSE
  use.cropspp = FALSE
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.allsppmodel){
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
  use.zscore = TRUE # change to false to use raw predictors
  
  # Default is species complex and no crops
  use.allspp = FALSE
  use.multcuespp = FALSE
  use.cropspp = FALSE
  
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  
  #Default is all chilling data
  use.expchillonly = FALSE # change to true for only experimental chilling 
  #note: with only exp chilling, there is only exp photo and force too.
  #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
}

source("source/bbstanleadin.R")




######################################
## Overview of the model run below ##
######################################
# Main model:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
m2l.ni = stan('stan/nointer_2level.stan', data = datalist.bb,
              iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))




###choose species for range model
unique(bb.stan$complex.wname)
rangesps<-c("Abies_alba","Acer_pseudoplatanus","Aesculus_hippocastanum", "Alnus_gluntinosa","Betula_pendula","Betula_pubescens",
         "Carpinus_betulus","Cornus_mas","Corylus_avellana","Fagus_sylvatica","Fraxinus_excelsior","Larix_decidua","Picea_abies",
       "Populus_tremula", "Prunus_avium","Prunus_padus","Quercus_petraea","Quercus_robur","Sorbus_aucuparia","Tilia_cordata")



rangyranger<-read.csv("..//ranges/range_extent.eusps.csv")
spsforphots<-rangyranger$complex

concordance<-dplyr::select(bb.stan, complex,complex.wname)
concordance<-unique(concordance)

concordance1<-concordance %>% filter(complex.wname %in% rangesps)
concordance2<-concordance %>% filter(complex.wname %in% spsforphots)
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
unique(output.df$iter)
output.live.df <- subset(output.df, iter>3500)### 500 posterior draws, should use 1000 for pub
unique(output.live.df$iter)



output.for.photomod<-dplyr::filter(output.live.df, complex %in% c(concordance2$complex))
output.live.df<-filter(output.live.df,complex %in% concordance1$complex)

###read in range data for each species and give it a complex identifyer
##updated ones
abal.range<-read.csv("..//ranges/output/Climate.in.range.Abies_alba.1980.2017.csv")
abal.range$complex<-1
acps.range<-read.csv("..//ranges/output/Climate.in.range.Acer_pseudoplatanus.1980.2017.csv")
acps.range$complex<-3
aehi.range<-read.csv("..//ranges/output/Climate.in.range.Aesculus_hippocastanum.1980.2017.csv")
aehi.range$complex<-6
algu.range<-read.csv("..//ranges/output/Climate.in.range.Alnus_glutinosa.1980.2017.csv")
algu.range$complex<-7
colnames(algu.range)<-colnames(aehi.range)
bepe.range<-read.csv("..//ranges/output/Climate.in.range.Betula_pendula.1980.2017.csv")
bepe.range$complex<-13
bepu.range<-read.csv("..//ranges/output/Climate.in.range.Betula_pubescens.1980.2017.csv")
bepu.range$complex<-14
cabe.range<-read.csv("..//ranges/output/Climate.in.range.Carpinus_betulus.1980.2017.csv")
cabe.range$complex<-15
coma.range<-read.csv("..//ranges/output/Climate.in.range.Cornus_mas.1980.2017.csv")
coma.range$complex<-18
coav.range<-read.csv("..//ranges/output/Climate.in.range.Corylus_avellana.1980.2017.csv")
coav.range$complex<-19
fasy.range<-read.csv("..//ranges/output/Climate.in.range.Fagus_sylvatica.1980.2017.csv")
fasy.range$complex<-21
frex.range<-read.csv("..//ranges/output/Climate.in.range.Fraxinus_excelsior.1980.2017.csv")
frex.range$complex<-23
lade.range<-read.csv("..//ranges/output/Climate.in.range.Larix_decidua.1980.2017.csv")
lade.range$complex<-26
piab.range<-read.csv("..//ranges/output/Climate.in.range.Picea_abies.1980.2017.csv")
piab.range$complex<-28
potr.range<-read.csv("..//ranges/output/Climate.in.range.Populus_tremula.1980.2017.csv")
potr.range$complex<-30
prav.range<-read.csv("..//ranges/output/Climate.in.range.Prunus_avium.1980.2017.csv")
prav.range$complex<-31
prpa.range<-read.csv("..//ranges/output/Climate.in.range.Prunus_padus.1980.2017.csv")
prpa.range$complex<-33
qupe.range<-read.csv("..//ranges/output/Climate.in.range.Quercus_petraea.1980.2017.csv")
qupe.range$complex<-36
quro.range<-read.csv("..//ranges/output/Climate.in.range.Quercus_robur.1980.2017.csv")
quro.range$complex<-37
soau.range<-read.csv("..//ranges/output/Climate.in.range.Sorbus_aucuparia.1980.2017.csv")
soau.range$complex<-44
tico.range<-read.csv("..//ranges/output/Climate.in.range.Tilia_cordata.1980.2017.csv")
tico.range$complex<-49




range.dat<-rbind(abal.range,acps.range,aehi.range,algu.range,bepe.range,bepu.range,cabe.range,coma.range,coav.range,fasy.range,frex.range,lade.range,piab.range,potr.range,prav.range,prpa.range,qupe.range,quro.range,soau.range,tico.range) ###bind all species ranges in one data sheet


range.mns<- range.dat %>% dplyr::group_by(complex) %>% dplyr::summarise(meanSDutah=mean(SDev.Chill.Utah),meanSDgdd=mean(SDev.GDD.sites))
#Not acerpsedu and aeshipo still seem to have same values

range.mns$zSDutah<-(range.mns$meanSDutah-mean(range.mns$meanSDutah))/sd(range.mns$meanSDutah)
range.mns$zSDgdd<-(range.mns$meanSDgdd-mean(range.mns$meanSDgdd))/sd(range.mns$meanSDgdd)


dater<-left_join(output.live.df,range.mns)


colnames(dater)
dater<-left_join(dater,concordance)
###plot raw data

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


mod1z<-brm(b_chill~zSDutah+zSDgdd,data=dater,iter=3000,warmup=2000) 
summary(mod1z)

extract_coefs<-function(x){rownames_to_column(as.data.frame(fixef(x, summary=TRUE,probs=c(0.025,0.25,0.75,0.975))),"trait")
}

Eur.sp<-extract_coefs(mod1)
Eur.sp.noi<-filter(Eur.sp,trait!="Intercept")
pd=position_dodgev(height=0.4)
nspsplot<-  ggplot(Eur.sp,aes(Estimate,trait))+geom_point(position=pd,size=1)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q25,xmax=Q75),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")

Eur.spz<-extract_coefs(mod1z)
Eur.sp.noi.z<-filter(Eur.spz,trait!="Intercept")
pd=position_dodgev(height=0.1)

jpeg("..//ranges/figures/mu_eusps_z.jpeg",width = 4, height = 4, units = 'in', res=200)
ggplot(Eur.spz,aes(Estimate,trait))+geom_point(position=pd,size=2)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q25,xmax=Q75),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")
dev.off()

mod1z.int<-brm(b_chill~zSDutah*zSDgdd,data=dater,iter=3000,warmup=2000) 
summary(mod1z.int)
Eur.spz.winters<-extract_coefs(mod1z.int)

jpeg("..//ranges/figures/mu_eusps_z_winter.jpeg",width = 4, height = 4, units = 'in', res=200)
ggplot(Eur.spz.winters,aes(Estimate,trait))+geom_point(position=pd,size=1)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q25,xmax=Q75),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")
dev.off()

####Photoperiods models
output.for.photomod<-left_join(output.for.photomod,concordance2)

colnames(rangyranger)[1]<-"complex.wname"


dater.phots<-left_join(output.for.photomod,rangyranger)
dater.phots$logdist<-log(dater.phots$distance)
dater.phots$dist100<-(dater.phots$distance)/100
dater.phots$logdist100<-log(dater.phots$dist100)

jpeg("..//ranges/photofig.jpeg",width = 8.6, height = 4, units = 'in', res=200)
ggplot(dater.phots,aes(distance,b_photo,label=complex.wname))+stat_summary(fun.data = "mean_sdl",aes(color=complex.wname))+theme(legend.position = "none")+
  geom_text(stat = 'summary', fun.y=mean, aes(label = complex.wname),size=3)
dev.off()

phot.mod<-brm(b_photo~logdist,data=dater.phots,iter=3000,warmup=2000,chains=4)



Eur.sps.photo<-extract_coefs(phot.mod)
a<-ggplot(Eur.sps.photo,aes(Estimate,trait))+geom_point(position=pd,size=2)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q25,xmax=Q75),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")
Eur.sps.photo<-filter(Eur.sps.photo,trait!="Intercept")
b<-ggplot(Eur.sps.photo,aes(Estimate,trait))+geom_point(position=pd,size=2)+
  geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q25,xmax=Q75),position=pd,width=0,linetype="solid")+
  theme_linedraw(base_size = 11)+geom_vline(aes(xintercept=0),color="black")

jpeg("..//ranges/figures/mu_eusps_photolog.jpeg",width = 7, height = 4, units = 'in', res=200)
ggpubr::ggarrange(a,b,widths=c(2,1))
dev.off()


summary(phot.mod)
##Questions zscore sdev predictors