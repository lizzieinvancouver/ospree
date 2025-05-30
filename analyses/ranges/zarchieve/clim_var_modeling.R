#started by Dan B on April 8 2019
# Goal to model some of the range dynamics of common ospress species 
#Updatef Jam 30
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstan)
library(shinystan)
library('raster')
library('ncdf4')
library('abind')
library('chillR')
library('foreach')
library(reshape2)
library(brms)
library(dplyr)
library(tibble)
library(ggstance)
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


use.flags.for.mainmodel="TRUE"
use.flags.for.spcomp.cp="FALSE"
use.flags.for.allspp.utah="FALSE"
use.flags.for.spcomp.utah.nonz="FALSE"
use.flags.for.spcomp.cp.nonz="FALSE"
use.flags.for.allspp.utah.nonz="FALSE"
use.yourown.flagdesign="FALSE"

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp =FALSE 
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

#source("source/flags.for.models.in.bbms.R")
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
EUrangesps.plus<-c("Abies_alba","Acer_pseudoplatanus","Aesculus_hippocastanum", "Alnus_gluntinosa","Betula_pendula","Betula_pubescens","Betula_lenta",
         "Carpinus_betulus","Cornus_mas","Corylus_avellana","Fagus_sylvatica","Fraxinus_excelsior","Larix_decidua","Picea_abies",
       "Populus_tremula", "Prunus_avium","Prunus_padus","Quercus_petraea","Quercus_robur","Sorbus_aucuparia","Tilia_cordata")



EUrange.extend<-read.csv("..//ranges/range_extent.eusps.csv")### note this is not the full list because Fagus is erroring in plot_range overland

EUrange.extend$complex

concordance<-dplyr::select(bb.stan, complex,complex.wname)
concordance<-unique(concordance)

concordance1<-concordance %>% filter(complex.wname %in% EUrangesps.plus)
concordance2<-concordance %>% filter(complex.wname %in% EUrange.extend)
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
output.live.df <- subset(output.df, iter>3000)### 1000 posterior estimates



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
bele1<-read.csv("..//ranges/output/Climate.in.range.betulent.1980.1998.csv")
bele2<-read.csv("..//ranges/output/Climate.in.range.betulent.1999.2016.csv")
bele.range<-rbind(bele1,bele2)
bele.range$complex<-11
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




range.dat.eu<-rbind(abal.range,acps.range,aehi.range,algu.range,bepe.range,bepu.range,cabe.range,coma.range,coav.range,fasy.range,frex.range,lade.range,piab.range,potr.range,prav.range,prpa.range,qupe.range,quro.range,soau.range,tico.range) ###bind all species ranges in one data sheet
colnames(range.dat.eu)
colnames(bele.range)<-colnames(range.dat.eu)
range.dat.full<-rbind(range.dat.eu,bele.range)

range.mns<- range.dat.full %>% dplyr::group_by(complex) %>% dplyr::summarise(meanSDutah=mean(SDev.Chill.Utah),meanSDgdd=mean(SDev.GDD.sites))
#Note acerpsedu and aeshipo still seem to have same values

range.mns$zSDutah<-(range.mns$meanSDutah-mean(range.mns$meanSDutah))/sd(range.mns$meanSDutah)
range.mns$zSDgdd<-(range.mns$meanSDgdd-mean(range.mns$meanSDgdd))/sd(range.mns$meanSDgdd)


dater<-left_join(output.live.df,range.mns)


colnames(dater)
dater<-left_join(dater,concordance)

###plot raw data

library("Hmisc")
dater %>%group_by(complex.wname)%>% summarise(mean=mean(b_chill),sd=sd(b_chill))

nlabels <- unique(dater$complex.wname)



uts<-ggplot(dater,aes(meanSDutah,b_chill,label=complex.wname))+stat_summary(fun.data = "mean_sdl",aes(color=complex.wname))+
geom_text(stat = 'summary', fun.y=mean, aes(label = complex.wname),size=3)+xlim(100,400)+theme_minimal()+theme(legend.position = "none")+
  geom_smooth(method=lm,color="black",linetype="dotted")


gdd<-ggplot(dater,aes(meanSDgdd,b_chill,label=complex.wname))+stat_summary(fun.data = "mean_sdl",aes(color=complex.wname))+
  geom_text(stat = 'summary', fun.y=mean, aes(label = complex.wname),size=3)+xlim(90,200)+theme_minimal()+theme(legend.position = "none")+
  geom_smooth(method=lm,color="black",linetype="dotted")


jpeg("..//ranges/figures/rawplots_GDD_CP.jpeg",width = 8.6, height = 4, units = 'in', res=200)
ggpubr::ggarrange(gdd,uts,nrow=2)
dev.off()

###model for climate variation and cue strength


mod.clim.var<-brm(b_chill~zSDutah+zSDgdd,data=dater,iter=3000,warmup=2000) 
summary(mod.clim.var)
extract_coefs<-function(x){rownames_to_column(as.data.frame(fixef(x, summary=TRUE,probs=c(0.05,0.25,0.75,0.95))),"climate_var")
}

clim.var.out<-extract_coefs(mod.clim.var)
clim.var.out$climate_var[which(clim.var.out$climate_var=="zSDgdd")]<-"forcing variability"
clim.var.out$climate_var[which(clim.var.out$climate_var=="zSDutah")]<-"chilling variability"


pd=position_dodgev(height=0.2)
#jpeg("..//ranges/figures/mu_eusps_z.jpeg",width = 4, height = 4, units = 'in', res=200)
a<-clim.var.out%>%
  arrange(Estimate) %>%
  mutate(climate_var = factor(climate_var, levels=c("chilling variability","forcing variability","Intercept"))) %>%
ggplot(aes(Estimate,climate_var))+geom_point(position=pd,size=1,shape=1)+
  geom_errorbarh(aes(xmin=Q5,xmax=Q95),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q25,xmax=Q75),position=pd,width=0,linetype="solid")+
  ggthemes::theme_base(base_size = 11)+geom_vline(aes(xintercept=0),color="black")+xlim(-4.5,3)

mod.clim.var.wint<-brm(b_chill~zSDutah*zSDgdd,data=dater,iter=3000,warmup=2000) 


clim.var.out.wint<-extract_coefs(mod.clim.var.wint)
clim.var.out.wint$climate_var[which(clim.var.out.wint$climate_var=="zSDgdd")]<-"forcing variability"
clim.var.out.wint$climate_var[which(clim.var.out.wint$climate_var=="zSDutah")]<-"chilling variability"
clim.var.out.wint$climate_var[which(clim.var.out.wint$climate_var=="zSDutah:zSDgdd")]<-"chilling:forcing"


b<-clim.var.out.wint%>%
  arrange(Estimate) %>%
  mutate(climate_var = factor(climate_var, levels=c("chilling:forcing","chilling variability","forcing variability","Intercept"))) %>%
  ggplot(aes(Estimate,climate_var))+geom_point(position=pd,size=1,shape=1)+
  geom_errorbarh(aes(xmin=Q5,xmax=Q95),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q25,xmax=Q75),position=pd,width=0,linetype="solid")+
  ggthemes::theme_base(base_size = 11)+geom_vline(aes(xintercept=0),color="black")+xlim(-4.5,3)




jpeg("..//ranges/figures/var_models_mus.jpeg",width = 6, height = 4, units = 'in', res=150)
ggpubr::ggarrange(a,b,nrow=2)
dev.off()


####Photoperiods models
output.for.photomod<-left_join(output.for.photomod,concordance2)

colnames(rangyranger)[1]<-"complex.wname"


dater.phots<-left_join(output.for.photomod,rangyranger)
dater.phots$logdist<-log(dater.phots$distance)
dater.phots$dist100<-(dater.phots$distance)/100
dater.phots$logdist100<-log(dater.phots$dist100)

