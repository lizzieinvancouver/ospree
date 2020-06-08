###Dan runs some code to plot  range extent vs betas

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

unique(bb.stan$complex.wname)

##read in extent data, prepare for extracting posterirs
ranges<-read.csv("..//ranges/output/full_extent_data.csv")

concordance<-dplyr::select(bb.stan, complex,complex.wname)
concordance<-unique(concordance)

concordance<-concordance %>% filter(complex.wname %in% ranges$species)

######extract posteriors
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
output.df <- subset(output.df, iter>3000)#

output.df<-dplyr::filter(output.df, complex %in% c(concordance$complex))


###merge the two datasets (extent and posterior estimates)
output.df<-left_join(output.df,concordance)
colnames(ranges)
colnames(output.df)
colnames(ranges)[2]<-"complex.wname"
plotty.data<-left_join(output.df,ranges,by="complex.wname")

##continetal acheivements
pdf("..//ranges/figures/continental_cues.pdf")
ggpubr::ggarrange(ggplot(plotty.data,aes(continent,b_force))+geom_boxplot(),
ggplot(plotty.data,aes(continent,b_chill))+geom_boxplot(),
ggplot(plotty.data,aes(continent,b_photo))+geom_boxplot(),nrow=1)
dev.off()
####extent


pdf("..//ranges/figures/range_size_cues.pdf",width = 7)
ggpubr::ggarrange(ggplot(plotty.data,aes(cent.lat,b_force))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"), ggplot(plotty.data,aes(lat.extent,b_force))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ggplot(plotty.data,aes(lon.extent,b_force))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),##forceing
                  ggplot(plotty.data,aes(cent.lat,b_chill))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ggplot(plotty.data,aes(lat.extent,b_chill))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ggplot(plotty.data,aes(lon.extent,b_chill))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),##chilling
                  ggplot(plotty.data,aes(cent.lat,b_photo))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"), ##phto
                  ggplot(plotty.data,aes(lat.extent,b_photo))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ggplot(plotty.data,aes(lon.extent,b_photo))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ncol=3,nrow=3,common.legend = TRUE,legend="bottom")
dev.off()

###now just extents (range size)
pdf("..//ranges/figures/range_startend_cues.pdf",height= = 11)
ggpubr::ggarrange(ggplot(plotty.data,aes(cent.lat,b_force))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"), ggplot(plotty.data,aes(max.y,b_force))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ggplot(plotty.data,aes(min.y,b_force))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),##forceing
                  ggplot(plotty.data,aes(cent.lat,b_chill))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ggplot(plotty.data,aes(max.y,b_chill))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ggplot(plotty.data,aes(min.y,b_chill))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),##chilling
                  ggplot(plotty.data,aes(cent.lat,b_photo))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"), ##phto
                  ggplot(plotty.data,aes(max.y,b_photo))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ggplot(plotty.data,aes(min.y,b_photo))+stat_summary(aes(color=complex.wname))+
                    geom_smooth(method="lm"),ncol=3,nrow=3,common.legend = TRUE,legend="bottom")
dev.off()

