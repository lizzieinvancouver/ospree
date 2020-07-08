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
rangies<-read.csv("output/Synthesis_climate_EUsps.csv")





## more formating

X<-split(rangies, with(rangies, rangies$variable), drop = TRUE)

Y <- lapply(seq_along(X), function(x) as.data.frame(X[[x]])[, 1:7]) 

#names(Y) <-(c("GDD","GDD.lastfrost","DayLastFrost","MeanTmins",          
               "SDev.Tmins","Mean.Chill.Utah","Mean.Chill.Portions"))

names(Y)<-(c("DayLastFrost","GDD","GDD.lastfrost"
             ,"Mean.Chill.Portions","Mean.Chill.Utah","MeanTmins","SDev.Tmins"))
colnames(posties)[6]<-"species" ##merge them

list2env(Y, envir = .GlobalEnv)
###make the data sheets
GDD.lastfrost<-left_join(posties,GDD.lastfrost)
GDD.lastfrost.EU<-filter(GDD.lastfrost,species %in% unique(rangies$species))

SDev.Tmins<-left_join(posties,SDev.Tmins)
SDev.Tmins.EU<-filter(SDev.Tmins,species %in% unique(rangies$species))

MeanTmins<-left_join(posties,MeanTmins)
MeanTmins.EU<-filter(MeanTmins,species %in% unique(rangies$species))

Mean.Chill.Utah<-left_join(posties,Mean.Chill.Utah)
Mean.Chill.Utah.EU<-filter(Mean.Chill.Utah,species %in% unique(rangies$species))

#####plots
lastfrosta<-ggplot(GDD.lastfrost.EU,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
lastfrostb<-ggplot(GDD.lastfrost.EU,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)

#I think Sdev of the Sdev is not what we want
#ggplot(SDev.Tmins.EU,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
#ggplot(SDev.Tmins.EU,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
#or
####STV 
stva<-ggplot(MeanTmins.EU,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
stvb<-ggplot(MeanTmins,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)


###photocue by chiling var
photochilla<-ggplot(Mean.Chill.Utah.EU,aes(Geo.SD,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
photochillb<-ggplot(Mean.Chill.Utah.EU,aes(Temp.SD,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)


rangegeo<-read.csv("output/zolder_datagrabs/full_extent_data.csv") ##not sure this is the best extent data
rangegeo<-left_join(posties,rangegeo)
rangegeo.EU<-filter(rangegeo,species %in% unique(rangies$species))
ex1<-ggplot(rangegeo.EU,aes(lat.extent,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
ex2<-ggplot(rangegeo.EU,aes(cent.lat,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)




jpeg(file = "figures/cheap_approach/stv.jpg",width = 7, height = 5,units = "in",res=300)
ggpubr::ggarrange(stva,stvb,common.legend = TRUE,labels = "STV")
dev.off()

jpeg(file = "figures/cheap_approach/gddtofrost.jpg",width = 7, height = 5,units = "in",res=300)
ggpubr::ggarrange(lastfrosta,lastfrostb,common.legend = TRUE,labels = "GDDtolastfrost")
dev.off()


jpeg(file = "figures/cheap_approach/photo_chillvar.jpg",width = 7, height = 5,units = "in",res=300)
ggpubr::ggarrange(photochilla,photochillb,common.legend = TRUE,labels = "photo with chill var")
dev.off()

jpeg(file = "figures/cheap_approach/photo_extent.jpg",width = 7, height = 5,units = "in",res=300)
ggpubr::ggarrange(ex1,ex2,common.legend = TRUE,labels = "photo with extent")
dev.off()

range(GDD.lastfrost.EU$Temp.SD)
range(GDD.lastfrost.EU$Geo.SD)
FDD.lastfrost.EU$Temp.SD-mean(DD.lastfrost.EU$Temp.SD)


quickdirtay<-brm(b_chill~Geo.SD+Temp.SD,data=GDD.lastfrost.EU)
pp_check(quickdirtay)
quickdirtaySTV<-brm(b_chill~Geo.SD+Temp.SD,data=MeanTmins.EU)
pp_check(quickdirtaySTV)
summary(quickdirtaySTV)
