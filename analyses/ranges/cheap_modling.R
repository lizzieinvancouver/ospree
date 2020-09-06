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
rangiesEu<-read.csv("output/Synthesis_climate_EUsps_corr.csv")
rangiesNa<-read.csv("output/Synthesis_climate_NAMsps.csv")

unique(rangiesNa$species)
##clean North America names
rangiesNa$species[which(rangiesNa$species=="betulent")]<- "Betula_lenta"
rangiesNa$species[which(rangiesNa$species=="popugran")]<- "Populus_grandidentata"
rangiesNa$species[which(rangiesNa$species=="querrubr")]<- "Quercus_rubra"
rangiesNa$species[which(rangiesNa$species=="acerpens")]<- "Acer_pensylvanicum"
rangiesNa$species[which(rangiesNa$species=="betupapy")]<- "Betula_papyrifera"
rangiesNa$species[which(rangiesNa$species=="fraxnigr")]<- "Fraxinus_nigra"
rangiesNa$species[which(rangiesNa$species=="alnurubr")]<- "Alnus_rubra"
rangiesNa$species[which(rangiesNa$species=="pseumenz")]<- "Pseudotsuga_menziesii"
rangiesNa$species[which(rangiesNa$species=="prunpens")]<- "Prunus_pensylvanicum"
rangiesNa$species[which(rangiesNa$species=="betualle")]<- "Betula_alleghaniensis"
rangiesNa$species[which(rangiesNa$species=="acersacr")]<- "Acer_saccharum"
rangiesNa$species[which(rangiesNa$species=="acerrubr")]<- "Acer_rubrum"
rangiesNa$species[which(rangiesNa$species=="corycorn")]<- "Corylus_cornuta"
rangiesNa$species[which(rangiesNa$species=="piceglau")]<- "Picea_glauca"

unique(rangiesNa$species)


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

####if activated this removes 2 outlyerspecies
posties<-filter(posties,!species %in% c("Quercus_ilex","Larix_decidua"))

GDD.lastfrost<-left_join(posties,GDD.lastfrost)
GDD.lastfrost.EU<-filter(GDD.lastfrost,species %in% unique(rangies$species))

GDD<-left_join(posties,GDD)
GDD.EU<-filter(GDD,species %in% unique(rangies$species))

SDev.Tmins<-left_join(posties,SDev.Tmins)
SDev.Tmins.EU<-filter(SDev.Tmins,species %in% unique(rangies$species))

MeanTmins<-left_join(posties,MeanTmins)
MeanTmins.EU<-filter(MeanTmins,species %in% unique(rangies$species))

Mean.Chill.Utah<-left_join(posties,Mean.Chill.Utah)
Mean.Chill.Utah.EU<-filter(Mean.Chill.Utah,species %in% unique(rangies$species))

rangegeo<-read.csv("output/zolder_datagrabs/full_extent_data.csv") ##not sure this is the best extent data
rangegeo<-left_join(posties,rangegeo)
rangegeo.EU<-filter(rangegeo,species %in% unique(rangies$species))

#####plots

lastfrosta<-ggplot(GDD.lastfrost.EU,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none") #+geom_point(aes(color=species),size=0.3,alpha=0.6)
lastfrostb<-ggplot(GDD.lastfrost.EU,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none") #+geom_point(aes(color=species),size=0.3,alpha=0.6)

#I think Sdev of the Sdev is not what we want
#ggplot(SDev.Tmins.EU,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
#ggplot(SDev.Tmins.EU,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
#or
####STV 
stva<-ggplot(MeanTmins.EU,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
stvb<-ggplot(MeanTmins.EU,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none") #+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/chill_variation_nool.jpg",width = 9, height = 5,units = "in",res=300)
ggpubr::ggarrange(lastfrosta,lastfrostb,stva,stvb,ncol=2,nrow=2,labels = c("GDD to last frost","b","STV","b","Chill magnitude","b"))
dev.off()


maxychill<-ggplot(rangegeo.EU,aes(max.y,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minychill<-ggplot(rangegeo.EU,aes(min.y,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centchill<-ggplot(rangegeo.EU,aes(cent.lat,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

maxyforce<-ggplot(rangegeo.EU,aes(max.y,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minyforce<-ggplot(rangegeo.EU,aes(min.y,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centforce<-ggplot(rangegeo.EU,aes(cent.lat,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

maxyphoto<-ggplot(rangegeo.EU,aes(max.y,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minyphoto<-ggplot(rangegeo.EU,aes(min.y,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centphoto<-ggplot(rangegeo.EU,aes(cent.lat,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/geographic_influence_nool.jpg",width = 7, height = 7,units = "in",res=300)
ggpubr::ggarrange(maxyforce,minyforce,centforce,maxyphoto,minyphoto,centphoto,maxychill,minychill,centchill,nrow=3,ncol=3)
dev.off()

#chilling




###photocue by chiling var
magchilla<-ggplot(Mean.Chill.Utah.EU,aes(Geo.Mean,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
magchillb<-ggplot(Mean.Chill.Utah.EU,aes(Temp.Mean,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)


magfora<-ggplot(GDD.EU,aes(Geo.Mean,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
magforb<-ggplot(GDD.EU,aes(Temp.Mean,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/magnitude_influence_nool.jpg",width = 9, height = 5,units = "in",res=300)
ggpubr::ggarrange(magfora,magforb,magchilla,magchillb,nrow=2,ncol=2)
dev.off()



