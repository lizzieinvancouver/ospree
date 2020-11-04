####### Started 8 June 2020 ##
## By Lizzie ##
#### First plots of the Europe species cliamte and range and cue relationship by Dan June 8 2020.
# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)


# libraries
library(shinystan)
library(reshape2)
library(dplyr)
library(ggplot2)
library(rstan)
library(brms)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

#load("cheap.mods.Rda")
posties<-read.csv("output/cue_posteriors.csv")
mysps<-filter(posties,latbi %in% c("Populus_grandidentata","Betula_lenta","Acer_pensylvanicum","Populus_tremuloides",
                                   "Acer_rubrum","Betula_papyrifera"))
mysps$type<-ifelse(mysps$latbi %in%c("Populus_grandidentata","Betula_lenta","Acer_pensylvanicum"),"small","big")
unique(mysps$latbi)
#coool
a<-ggpubr::ggboxplot(x='latbi',y='b_chill',data=mysps,color='type')
#now for other paramentere
rangiesNa<-read.csv("output/Synthesis_climate_Namsps_weighted.csv")

head(rangiesNa,14)
##clean North America names
rangiesNa$species[which(rangiesNa$species=="betulent")]<- "Betula_lenta"
rangiesNa$species[which(rangiesNa$species=="popugran")]<- "Populus_grandidentata"
rangiesNa$species[which(rangiesNa$species=="fagugran")]<- "Fagus_grandifolia"
rangiesNa$species[which(rangiesNa$species=="querrubr")]<- "Quercus_rubra"
rangiesNa$species[which(rangiesNa$species=="acerpens")]<- "Acer_pensylvanicum"
rangiesNa$species[which(rangiesNa$species=="betupapy")]<- "Betula_papyrifera"
rangiesNa$species[which(rangiesNa$species=="fraxnigr")]<- "Fraxinus_nigra"
rangiesNa$species[which(rangiesNa$species=="robipseu")]<- "Robinia_pseudoacacia"
rangiesNa$species[which(rangiesNa$species=="pseumenz")]<- "Pseudotsuga_menziesii"
rangiesNa$species[which(rangiesNa$species=="prunpens")]<- "Prunus_pensylvanicum"
rangiesNa$species[which(rangiesNa$species=="poputrem")]<- "Populus_tremuloides"
rangiesNa$species[which(rangiesNa$species=="betualle")]<- "Betula_alleghaniensis"
rangiesNa$species[which(rangiesNa$species=="acersacr")]<- "Acer_saccharum"
rangiesNa$species[which(rangiesNa$species=="acerrubr")]<- "Acer_rubrum"
rangiesNa$species[which(rangiesNa$species=="alnurugo")]<- "Alnus_incana"
rangiesNa$species[which(rangiesNa$species=="corycorn")]<- "Corylus_cornuta"
rangiesNa$species[which(rangiesNa$species=="piceglau")]<- "Picea_glauca"
rangiesNa$species[which(rangiesNa$species=="picemari")]<- "Picea_mariana"
###33
myclim<-filter(rangiesNa,species %in%c("Populus_grandidentata","Betula_lenta","Acer_pensylvanicum","Populus_tremuloides",
                                       "Acer_rubrum","Betula_papyrifera"))
unique(myclim$variable)
myclim.gg2lf<-filter(myclim,variable=="GDD.lastfrost")
myclim.gg2lf$type<-ifelse(myclim.gg2lf$species %in%c("Populus_grandidentata","Betula_lenta","Acer_pensylvanicum"),"small","big")

myclim.gg2lf$species<-as.factor(myclim.gg2lf$species)
b<-ggpubr::ggbarplot(x='species',y='Temp.SD',data=myclim.gg2lf,color='type',main="GDD2LF")
  
  myclim.stv<-filter(myclim,variable=="MeanTmins")
  myclim.stv$type<-ifelse(myclim.stv$species %in%c("Populus_grandidentata","Betula_lenta","Acer_pensylvanicum"),"small","big")
  myclim.stv$species<-as.factor(myclim.stv$species)
 
   c<-ggpubr::ggbarplot(x='species',y='Temp.SD',data=myclim.stv,color='type',main="stv")
png("./figures/biggysmalls_comps.png",height=12,width=8,units='in',res=200)
   ggpubr::ggarrange(a,b,c,nrow=3,common.legend = TRUE)  
dev.off()
