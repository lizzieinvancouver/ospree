###cheap modeling NA sps

####### Started 8 June 2020 ##
## By Lizzie ##
#### First plots of the Europe species cliamte and range and cue relationship by Dan June 8 2020.
# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)


# libraries
library(shinystan)
library(reshape2)

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

posties<-read.csv("output/cue_posteriors.csv") ##read in both data
rangies<-read.csv("output/Synthesis_climate_NAMsps.csv")
unique(rangies$species)

##fix species names
rangies$species[which(rangies$species=="betulent")]<- "Betula_lenta"
rangies$species[which(rangies$species=="popugran")]<- "Populus_grandidentata"
rangies$species[which(rangies$species=="querrubr")]<- "Quercus_rubra"
rangies$species[which(rangies$species=="acerpens")]<- "Acer_pensylvanicum"
rangies$species[which(rangies$species=="betupapy")]<- "Betula_papyrifera"
rangies$species[which(rangies$species=="fraxnigr")]<- "Fraxinus_nigra"
rangies$species[which(rangies$species=="alnurubr")]<- "Alnus_rubra"
rangies$species[which(rangies$species=="pseumenz")]<- "Pseudotsuga_menziesii"
rangies$species[which(rangies$species=="prunpens")]<- "Prunus_pensylvanicum"
rangies$species[which(rangies$species=="betualle")]<- "Betula_alleghaniensis"
rangies$species[which(rangies$species=="acersacr")]<- "Acer_saccharum"
rangies$species[which(rangies$species=="acerrubr")]<- "Acer_rubrum"
rangies$species[which(rangies$species=="corycorn")]<- "Cornus_cornuta"
rangies$species[which(rangies$species=="piceglau")]<- "Picea_glauca"
##clean it
#rangies$variable[which(rangies$variable=="GDD.")]<-"GDD"
#rangies$variable[which(rangies$variable=="GDD.lastfrost.")]<-"GDD.lastfrost"
#rangies$variable[which(rangies$variable=="DayLastFrost.")]<-"DayLastFrost"
#rangies$variable[which(rangies$variable=="MeanTmins.")]<-"MeanTmins"
#rangies$variable[which(rangies$variable=="SDev.Tmins.")]<-"SDevs.Tmins"
#rangies$variable[which(rangies$variable=="Mean.Chill.Utah.")]<-"Mean.Chill.Utah"
#rangies$variable[which(rangies$variable=="Mean.Utah.Chill")]<-"Mean.Chill.Utah"
#rangies$variable[which(rangies$variable=="Mean.Chill.Portions.")]<-"Mean.Chill.Portions"

#rangies$variable[which(rangies$variable=="GDD.1")]<-"GDD"
#rangies$variable[which(rangies$variable=="GDD.lastfrost.1")]<-"GDD.lastfrost"
#rangies$variable[which(rangies$variable=="DayLastFrost.1")]<-"DayLastFrost"
#rangies$variable[which(rangies$variable=="MeanTmins.1")]<-"MeanTmins"
#rangies$variable[which(rangies$variable=="SDev.Tmins.1")]<-"SDevs.Tmins"
#rangies$variable[which(rangies$variable=="Mean.Chill.Utah.1")]<-"Mean.Chill.Utah"
#rangies$variable[which(rangies$variable=="Mean.Chill.Portions.1")]<-"Mean.Chill.Portions"

## more formating

X<-split(rangies, with(rangies, rangies$variable), drop = TRUE)

Y <- lapply(seq_along(X), function(x) as.data.frame(X[[x]])[, 1:7]) 

#names(Y) <-(c("GDD","GDD.lastfrost","DayLastFrost","MeanTmins",          
# "SDev.Tmins","Mean.Chill.Utah","Mean.Chill.Portions"))

names(Y)<-(c("DayLastFrost","GDD","GDD.lastfrost"
             ,"Mean.Chill.Portions","Mean.Chill.Utah","MeanTmins","SDev.Tmins"))
colnames(posties)[6]<-"species" ##merge them

list2env(Y, envir = .GlobalEnv)
###make the data sheets

GDD.lastfrost<-left_join(posties,GDD.lastfrost)
GDD.lastfrost.NA<-filter(GDD.lastfrost,species %in% unique(rangies$species))

GDD<-left_join(posties,GDD)
GDD.NA<-filter(GDD,species %in% unique(rangies$species))

SDev.Tmins<-left_join(posties,SDev.Tmins)
SDev.Tmins.NA<-filter(SDev.Tmins,species %in% unique(rangies$species))

MeanTmins<-left_join(posties,MeanTmins)
MeanTmins.NA<-filter(MeanTmins,species %in% unique(rangies$species))

Mean.Chill.Utah<-left_join(posties,Mean.Chill.Utah)
Mean.Chill.Utah.NA<-filter(Mean.Chill.Utah,species %in% unique(rangies$species))

rangegeo<-read.csv("output/zolder_datagrabs/full_extent_data.csv") ##not sure this is the best extent data
rangegeo<-left_join(posties,rangegeo)
rangegeo.NA<-filter(rangegeo,species %in% unique(rangies$species))

#####plots

lastfrosta<-ggplot(GDD.lastfrost.NA,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)#+theme(legend.position = "none") #+geom_point(aes(color=species),size=0.3,alpha=0.6)
lastfrostb<-ggplot(GDD.lastfrost.NA,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none") #+geom_point(aes(color=species),size=0.3,alpha=0.6)

####STV 
stva<-ggplot(MeanTmins.NA,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
stvb<-ggplot(MeanTmins.NA,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none") #+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/chill_variation_NAsp.jpg",width = 9, height = 5,units = "in",res=300)
ggpubr::ggarrange(lastfrosta,lastfrostb,stva,stvb,ncol=2,nrow=2,labels = c("GDD to last frost","b","STV","b","Chill magnitude","b"),common.legend = TRUE)
dev.off()

maxychill<-ggplot(rangegeo.NA,aes(max.y,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)#+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minychill<-ggplot(rangegeo.NA,aes(min.y,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centchill<-ggplot(rangegeo.NA,aes(cent.lat,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

maxyforce<-ggplot(rangegeo.NA,aes(max.y,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minyforce<-ggplot(rangegeo.NA,aes(min.y,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centforce<-ggplot(rangegeo.NA,aes(cent.lat,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

maxyphoto<-ggplot(rangegeo.NA,aes(max.y,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minyphoto<-ggplot(rangegeo.NA,aes(min.y,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centphoto<-ggplot(rangegeo.NA,aes(cent.lat,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/geographic_influence_NA.SP.jpg",width = 7, height = 7,units = "in",res=300)
ggpubr::ggarrange(maxyforce,minyforce,centforce,maxyphoto,minyphoto,centphoto,maxychill,minychill,centchill,nrow=3,ncol=3,common.legend = TRUE)
dev.off()

magchilla<-ggplot(Mean.Chill.Utah.NA,aes(Geo.Mean,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
magchillb<-ggplot(Mean.Chill.Utah.NA,aes(Temp.Mean,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)


magfora<-ggplot(GDD.NA,aes(Geo.Mean,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
magforb<-ggplot(GDD.NA,aes(Temp.Mean,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)#+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/magnitude_influence_NAsp.jpg",width = 9, height = 5,units = "in",res=300)
ggpubr::ggarrange(magfora,magforb,magchilla,magchillb,nrow=2,ncol=2,common.legend = TRUE)
dev.off()


####3models

brm(b_chill)