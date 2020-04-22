###photoperiod model scripts
#started by Dan B on Jan 30. check photo period predictons
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

EUrange.extend<-read.csv("..//ranges/range_extent.eusps.csv")### note this is not the full list because Fagus and others is erroring in plot_range overland
NArange.extend<-read.csv("..//ranges/range_extent.nasps.csv") ### oh crap, I think these are in difference projects

##conver them to same scale
#EU 52/43321000 NA =
NArange.extend$min<-NArange.extend$min*(4321000/52)
NArange.extend$max<-NArange.extend$max*(4321000/52)
NArange.extend$distance<-NArange.extend$max-NArange.extend$min
EUrange.extend$distance

range.extend<-rbind(NArange.extend,EUrange.extend)

extend.sps<-range.extend$complex


concordance<-dplyr::select(bb.stan, complex,complex.wname)
concordance<-unique(concordance)

sort(concordance$complex.wname)
extend.sps
concordance2<-concordance %>% filter(complex.wname %in% extend.sps)
###extract posteriors
sample <- rstan::extract(m2l.ni)### takes a while

#sample.force <- melt(sample$b_force)
#sample.chill <- melt(sample$b_chill)
sample.photo <- melt(sample$b_photo)

#names(sample.force) <- c("iter", "complex", "b_force")
#names(sample.chill) <- c("iter", "complex", "b_chill")
names(sample.photo) <- c("iter", "complex", "b_photo")


photo.df <- subset(sample.photo, iter>3000)### 1000 posterior estimates

output.for.photomod<-dplyr::filter(photo.df, complex %in% c(concordance2$complex))
output.for.photomod<-left_join(output.for.photomod,concordance2)



colnames(range.extend)[3]<-"complex.wname"
dater.phots<-left_join(output.for.photomod,range.extend)


dater.phots$logdist<-log(dater.phots$distance)
dater.phots$dist100<-(dater.phots$distance)/100
dater.phots$logdist100<-log(dater.phots$dist100)
unique(dater.phots$complex.wname) 
unique(dater.phots$distance) 

jpeg("..//ranges/figures/rawphotoperiod.jpeg",height = 6, width=14, units = "in",res=100)
ggplot(dater.phots,aes(distance,b_photo,label=complex.wname))+stat_summary(fun.data = "mean_sdl",aes(color=complex.wname))+
  geom_text(stat = 'summary', fun.y=mean, aes(label = complex.wname),size=3)+theme_minimal()+theme(legend.position = "none")
dev.off()

ggplot(dater.phots,aes(logdist,b_photo,label=complex.wname))+stat_summary(fun.data = "mean_sdl",aes(color=complex.wname))+
  geom_text(stat = 'summary', fun.y=mean, aes(label = complex.wname),size=3)+theme_minimal()+theme(legend.position = "none")

phot.mod<-brm(b_photo~logdist,data=dater.phots,iter=3000,warmup=2000,control = list(adapt_delta=0.95),chains=4)

summary(phot.mod)
extract_coefs<-function(x){rownames_to_column(as.data.frame(fixef(x, summary=TRUE,probs=c(0.05,0.25,0.75,0.95))),"climate_var")
}

photo.var.out<-extract_coefs(phot.mod)
pd=position_dodgev(height=0.2)

jpeg("..//ranges/figures/photo_model_mus.jpeg",width = 6, height = 4, units = 'in', res=150)
photo.var.out%>%
  arrange(Estimate) %>%
  mutate(climate_var = factor(climate_var, levels=c("logdist","Intercept"))) %>%
  ggplot(aes(Estimate,climate_var))+geom_point(position=pd,size=1,shape=1)+
  geom_errorbarh(aes(xmin=Q5,xmax=Q95),position=pd,width=0,linetype="dotted")+
  geom_errorbarh(aes(xmin=Q25,xmax=Q75),position=pd,width=0,linetype="solid")+
  ggthemes::theme_base(base_size = 11)+geom_vline(aes(xintercept=0),color="black")+xlim(-2.2,.5)
dev.off()
