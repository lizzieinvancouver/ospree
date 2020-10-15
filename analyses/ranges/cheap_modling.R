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

unique(rangiesNa$species)
rangiesEu$continent<-"EU"
rangiesEu<-dplyr::select(rangiesEu,-X)
rangiesNa$continent<-"NA"

rangies<-rbind(rangiesEu,rangiesNa)
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
#posties<-filter(posties,!species %in% c("Quercus_ilex","Larix_decidua"))
colnames(rangies)
geos<-dplyr::select(rangies,species, continent)
geos<-geos[!duplicated(geos),]


cuecomps<-left_join(geos,posties)
a<-ggplot(cuecomps,aes(continent,b_force))+geom_boxplot()
b<-ggplot(cuecomps,aes(continent,b_chill))+geom_boxplot()
c<-ggplot(cuecomps,aes(continent,b_photo))+geom_boxplot()

pdf("figures/continental_cues.pdf")
ggpubr::ggarrange(a,b,c, nrow=1,ncol=3)
dev.off()


GDD.lastfrost<-left_join(posties,GDD.lastfrost)
GDD.lastfrost<-filter(GDD.lastfrost,species %in% unique(rangies$species))



GDD<-left_join(posties,GDD)
GDD<-filter(GDD,species %in% unique(rangies$species))

SDev.Tmins<-left_join(posties,SDev.Tmins)
SDev.Tmins<-filter(SDev.Tmins,species %in% unique(rangies$species))

MeanTmins<-left_join(posties,MeanTmins)
MeanTmins<-filter(MeanTmins,species %in% unique(rangies$species))

Mean.Chill.Utah<-left_join(posties,Mean.Chill.Utah)
Mean.Chill.Utah<-filter(Mean.Chill.Utah,species %in% unique(rangies$species))

rangegeo<-read.csv("output/zolder_datagrabs/full_extent_data.csv") ##not sure this is the best extent data
rangegeo<-left_join(posties,rangegeo)
rangegeo<-filter(rangegeo,species %in% unique(rangies$species))


#####plots
colnames(GDD.lastfrost)


lastfrost.geo<-ggplot(GDD.lastfrost,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes(),color="black")+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+xlab("Geographic variation in GDDs to last frost")
last.frost.geo2<-ggplot(GDD.lastfrost,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes(),color="black")+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+facet_wrap(~continent,scale="free_x")+theme(legend.position = "none")+xlab("Geographic variation in GDDs to last frost") #+geom_point(aes(color=species),size=0.3,alpha=0.6)

colnames(MeanTmins)[7]<-"Geo_Mean"
colnames(MeanTmins)[9]<-"Temp_Mean"
colnames(MeanTmins)[8]<-"Geo_SD"
colnames(MeanTmins)[10]<-"Temp_SD"
cheap.geo<-MeanTmins
datalist.cheap <- with(cheap.geo, 
                        list(y = b_chill,  
                             x = Geo_SD, 
                             N = nrow(cheap.geo)
                           
                        )
)


 modstv.geo = stan('stan/cheap_model.stan', data = cheap.geo,
              iter = 3000, warmup=2000, chains=4) ## my stan hardware seems off

library(rstanarm)
mod.stv<-brm(b_chill~Geo_SD+(1|species),data=MeanTmins)
summary(mod.stv)

new.data.stv<-data.frame(Geo_SD=MeanTmins$Geo_SD,continent=MeanTmins$continent)
stv.proj<-posterior_predict(mod.stv,newdata = new.data.stv)

stv.proj<-cbind(new.data.stv,stv.proj)
colnames(stv.proj)
ggplot(stv.proj,aes(Geo_SD,Estimate))+
  facet_wrap(~continent)+
  geom_point(data=MeanTmins,aes(Geo_SD,b_chill,color=species))+
  geom_smooth(aes(Geo_SD,Estimate),method="lm",se=TRUE)+geom_point()

?geom_smooth()
##### raw data plots below

jpeg(file = "figures/cheap_approach/gdd_2_lastfrost_geo.jpg",width = 9, height = 9,units = "in",res=200)
ggpubr::ggarrange(lastfrost.geo,last.frost.geo2,ncol=1,nrow=2)
dev.off()

lastfrost.temp<-ggplot(GDD.lastfrost,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes(),color="black")+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+xlab("Temporal variation in GDDs to last frost")

lastfrost.temp2<-ggplot(GDD.lastfrost,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes(),color="black")+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+facet_wrap(~continent,scale="free_x")+theme(legend.position = "none")+xlab("Temporal variation in GDDs to last frost") #+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/gdd_2_lastfrost_temporal.jpg",width = 9, height = 9,units = "in",res=200)
ggpubr::ggarrange(lastfrost.temp,lastfrost.temp2,ncol=1,nrow=2)
dev.off()
#I think Sdev of the Sdev is not what we want
#ggplot(SDev.Tmins.EU,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
#ggplot(SDev.Tmins.EU,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+geom_point(aes(color=species),size=0.3,alpha=0.6)
#or
####STV 
stv.geo<-ggplot(MeanTmins,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes(),color="black")+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+xlab("Geographic variation in STV")
stv.geo2<-ggplot(MeanTmins,aes(Geo.SD,b_chill))+geom_smooth(method="lm",aes(),color="black")+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+xlab("Geographic variation in STV")+
  facet_wrap(~continent, scales = "free_x")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/stv_geo.jpg",width = 9, height = 9,units = "in",res=200)
ggpubr::ggarrange(stv.geo,stv.geo2,ncol=1,nrow=2)
dev.off()

stv.temp<-ggplot(MeanTmins,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes(),color="black")+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+xlab("Temporal variation in STV")
stv.temp2<-ggplot(MeanTmins,aes(Temp.SD,b_chill))+geom_smooth(method="lm",aes(),color="black")+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+xlab("Temporal variation in STV")+
  facet_wrap(~continent, scales = "free_x")#+geom_point(aes(color=species),size=0.3,alpha=0.6)



maxychill<-ggplot(rangegeo,aes(max.y,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+facet_wrap(~continent)#+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minychill<-ggplot(rangegeo,aes(min.y,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+facet_wrap(~continent)#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centchill<-ggplot(rangegeo,aes(cent.lat,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+facet_wrap(~continent)#+geom_point(aes(color=species),size=0.3,alpha=0.6)

maxyforce<-ggplot(rangegeo,aes(max.y,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+facet_wrap(~continent)#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minyforce<-ggplot(rangegeo,aes(min.y,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+facet_wrap(~continent)#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centforce<-ggplot(rangegeo,aes(cent.lat,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+facet_wrap(~continent)#+geom_point(aes(color=species),size=0.3,alpha=0.6)

maxyphoto<-ggplot(rangegeo,aes(max.y,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+facet_wrap(~continent)#+geom_point(aes(color=species),size=0.3,alpha=0.6)
minyphoto<-ggplot(rangegeo,aes(min.y,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+facet_wrap(~continent)#+geom_point(aes(color=species),size=0.3,alpha=0.6)
centphoto<-ggplot(rangegeo,aes(cent.lat,b_photo))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species,shape=continent))+theme_bw(base_size = 11)+theme(legend.position = "none")+facet_wrap(~continent)#+geom_point(aes(color=species),size=0.3,alpha=0.6)

jpeg(file = "figures/cheap_approach/geographic_influence.jpg",width = 10, height = 9,units = "in",res=200)
ggpubr::ggarrange(maxyforce,minyforce,centforce,maxyphoto,minyphoto,centphoto,maxychill,minychill,centchill,nrow=3,ncol=3,common.legend = TRUE,legend="bottom")
dev.off()

#chilling




###photocue by chiling var ##currently not plotted
magchilla<-ggplot(Mean.Chill.Utah,aes(Geo.Mean,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
magchillb<-ggplot(Mean.Chill.Utah,aes(Temp.Mean,b_chill))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)


magfora<-ggplot(GDD,aes(Geo.Mean,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)
magforb<-ggplot(GDD,aes(Temp.Mean,b_force))+geom_smooth(method="lm",aes())+stat_summary(aes(color=species))+theme_bw(base_size = 11)+theme(legend.position = "none")#+geom_point(aes(color=species),size=0.3,alpha=0.6)

## dirtay models
#b_chill~var. gglast frost* Mean Chill Utah
meanposts<- posties %>% group_by(species) %>% summarise(mean_b_chill=mean(b_chill))
meanposts<-left_join(meanposts,rangies)
mod.dat<-dplyr::filter(meanposts, variable %in% c("Mean.Chill.Utah"))
mod.dat2<-dplyr::filter(meanposts, variable %in% c("GDD.lastfrost"))
mod.dat3<-dplyr::filter(meanposts, variable %in% c("MeanTmins"))

library(xtable)
xtable(summary(lm(mod.dat$mean_b_chill~mod.dat$Geo.Mean*mod.dat2$Geo.SD)),caption = "magnitude of  geographic chilling x geo variation in GDD to last frost")
xtable(summary(lm(mod.dat$mean_b_chill~mod.dat$Temp.Mean*mod.dat2$Temp.SD)),caption = "magnitude of temporal chilling x temporal variation in GDD to last frost")
xtable(summary(lm(mod.dat$mean_b_chill~mod.dat$Geo.Mean*mod.dat2$Temp.SD)),caption = "magnitude of  geographic chilling x temporal variation in GDD to last frost")


xtable(summary(lm(mod.dat$mean_b_chill~mod.dat$Geo.Mean*mod.dat3$Geo.SD)),caption = "magnitude of  geographic chilling x geo variation STV")
xtable(summary(lm(mod.dat$mean_b_chill~mod.dat$Temp.Mean*mod.dat3$Temp.SD)),caption = "magnitude of  temporal chilling x temporal variation STV")
xtable(summary(lm(mod.dat$mean_b_chill~mod.dat$Geo.Mean*mod.dat3$Temp.SD)),caption = "magnitude of  geographic chilling x temporal variation STV")

