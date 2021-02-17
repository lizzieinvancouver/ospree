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
posties<-read.csv("output/cue_posteriors.csv") ##read in both data
rangiesEu<-read.csv("output/Synthesis_climate_EUsps_corr.csv")
rangiesNa<-read.csv("output/Synthesis_climate_Namsps_weighted.csv")

area<-read.csv("output/rangeareas.csv")
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
cor(GDD.lastfrost$Temp.SD,GDD.lastfrost$Geo.SD)

cor(MeanTmins$Temp.SD,GDD.lastfrost$Geo.SD)
cor(MeanTmins$Temp.SD,GDD.lastfrost$Temp.SD)

area<-select(area,-continent)
area<-left_join(GDD.lastfrost,area)
cor(area$range_area,area$Temp.SD)
cor(area$range_area,area$Geo.SD)

area<-left_join(area,posties)
ggplot(area,aes(range_area,Geo.SD,color=continent))+geom_smooth(method="lm")

ggplot(area,aes(range_area,b_chill,color=continent))+geom_smooth(method="lm")
ggplot(area,aes(range_area,b_photo,color=continent))+geom_smooth(method="lm")
ggplot(area,aes(range_area,b_force,color=continent))+geom_smooth(method="lm")


plot(MeanTmins$Temp.SD~GDD.lastfrost$Temp.SD)
abline(lm(MeanTmins$Temp.SD~GDD.lastfrost$Temp.SD))
text(round(cor(MeanTmins$Temp.SD,GDD.lastfrost$Temp.SD),digits=4),x=30,y=4)
summary(lm(MeanTmins$Temp.SD~GDD.lastfrost$Temp.SD))

####if activated this removes 2 outlyerspecies
#posties<-filter(posties,!species %in% c("Quercus_ilex","Larix_decidua"))
colnames(rangies)
geos<-dplyr::select(rangies,species, continent)
geos<-geos[!duplicated(geos),]


cuecomps<-left_join(geos,posties)
a<-ggplot(cuecomps,aes(continent,b_force))+geom_violin(fill="pink")+geom_boxplot(outlier.shape = NA)+theme_bw()
b<-ggplot(cuecomps,aes(continent,b_chill))+geom_violin(fill="lightblue")+geom_boxplot(outlier.shape = NA)+theme_bw()
c<-ggplot(cuecomps,aes(continent,b_photo))+geom_violin(fill="yellow")+geom_boxplot(outlier.shape = NA)+theme_bw()

summary(aov(b_chill~continent,data=cuecomps))
summary(aov(b_photo~continent,data=cuecomps))
summary(aov(b_force~continent,data=cuecomps))

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


## hypothesis 2 last frost should im pact 

colnames(GDD.lastfrost)[7]<-"Geo_Mean"
colnames(GDD.lastfrost)[9]<-"Temp_Mean"
colnames(GDD.lastfrost)[8]<-"Geo_SD"
colnames(GDD.lastfrost)[10]<-"Temp_SD"


## Stv
colnames(MeanTmins)[7]<-"Geo_Mean"
colnames(MeanTmins)[9]<-"Temp_Mean"
colnames(MeanTmins)[8]<-"Geo_SD"
colnames(MeanTmins)[10]<-"Temp_SD"

cheap.stv<-MeanTmins
cheap.geo<-GDD.lastfrost

## model it
library(brms)

### try it with fewer iterations
range(cheap.geo$iter)
cheap.geo.small<-dplyr::filter(cheap.geo,iter>3500)
cheap.stv.small<-dplyr::filter(cheap.stv,iter>3500)

mod.ggdlf.geo<-brm(b_chill~Geo_SD*continent+(1|iter),data=cheap.geo.small)

mod.ggdlf.multivar.geo<-brm(mvbind(b_force,b_photo,b_chill)~Geo_SD*continent+(1|p|iter),data=cheap.geo.small)

#mod.ggdlf.geo.sp<-brm(b_chill~Geo_SD*continent+(1|species),data=cheap.geo.small) doesnt run


new.data.ggdlf.temp<-data.frame(Geo_SD=cheap.geo.small$Geo_SD,iter=cheap.geo.small$iter,continent=cheap.geo.small$continent)


ggdlf<-predict(mod.ggdlf.geo,newdata = new.data.ggdlf)
ggdlf<-as.data.frame(ggdlf)
#head(ggdlf)
new.data.ggdlf<-cbind(new.data.ggdlf,ggdlf)

library(ggplot2)
png("figures/cheap_approach/geo_sd_gdd2lf.png",width = 7,height = 6,units = "in",res=200)
gdd.geo.plot<-ggplot()+geom_point(data=cheap.geo,aes(Geo_SD,b_chill),size=0.3)+
  stat_summary(data=cheap.geo,aes(Geo_SD,b_chill),size=1,color="gray")+
 # geom_point(data=new.data.ggdlf,aes(Geo_SD,Estimate),color="skyblue1")+
#  geom_errorbar(data=new.data.ggdlf,aes(Geo_SD,ymin=Q2.5,ymax=Q97.5),color="red")+
geom_smooth(data=new.data.ggdlf,aes(Geo_SD,Estimate),method="lm",fullrange=TRUE)+
  geom_smooth(data=new.data.ggdlf,aes(Geo_SD,Q2.5),method="lm",color="red",linetype="dashed",fullrange=TRUE)+
  geom_smooth(data=new.data.ggdlf,aes(Geo_SD,Q97.5),method="lm",color="red",linetype="dashed",fullrange=TRUE)+
    facet_wrap(~continent)+theme_bw()+ylim(-50,50)
dev.off()
range(cheap.geo.small$Temp_SD)

mod.ggdlf.temp.nocont<-brm(b_chill~Temp_SD+(1|iter),data=cheap.geo.small)
mod.ggdlf.temp.nocon.nopool<-brm(b_chill~Temp_SD,data=cheap.geo.small)
mod.ggdlf.temp<-brm(b_chill~Temp_SD*continent+(1|iter),data=cheap.geo.small)
mod.ggdlf.temp.photo<-brm(b_photo~Temp_SD*continent+(1|iter),data=cheap.geo.small)
mod.ggdlf.temp.force<-brm(b_force~Temp_SD*continent+(1|iter),data=cheap.geo.small)
fixef(mod.ggdlf.temp)
fixef(mod.ggdlf.temp.photo)
fixef(mod.ggdlf.temp.force)

mod.ggdlf.geo.photo<-brm(b_photo~Geo_SD*continent+(1|iter),data=cheap.geo.small)
mod.ggdlf.geo.force<-brm(b_force~Geo_SD*continent+(1|iter),data=cheap.geo.small)
fixef(mod.ggdlf.temp)
new.data.ggdlf.temp<-data.frame(Temp_SD=cheap.geo.small$Temp_SD,iter=cheap.geo.small$iter,continent=cheap.geo.small$continent)

ggdlf.temp<-predict(mod.ggdlf.temp,newdata = new.data.ggdlf.temp)
ggdlf.temp<-as.data.frame(ggdlf.temp)
new.data.ggdlf.temp<-cbind(new.data.ggdlf.temp,ggdlf.temp)

png("figures/cheap_approach/temp_sd_gdd2lf.png",width = 7,height = 6,units = "in",res=200)
gdd.temp.plot<-ggplot()+geom_point(data=cheap.geo,aes(Temp_SD,b_chill),size=0.3)+
  stat_summary(data=cheap.geo,aes(Temp_SD,b_chill),size=1,color="gray")+
  # geom_point(data=new.data.ggdlf,aes(Geo_SD,Estimate),color="skyblue1")+
  #  geom_errorbar(data=new.data.ggdlf,aes(Geo_SD,ymin=Q2.5,ymax=Q97.5),color="red")+
  geom_smooth(data=new.data.ggdlf.temp,aes(Temp_SD,Estimate),method="lm",fullrange=TRUE)+
  geom_smooth(data=new.data.ggdlf.temp,aes(Temp_SD,Q2.5),method="lm",color="red",linetype="dashed",fullrange=TRUE)+
  geom_smooth(data=new.data.ggdlf.temp,aes(Temp_SD,Q97.5),method="lm",color="red",linetype="dashed",fullrange=TRUE)+
  facet_wrap(~continent)+theme_bw()+ylim(-100,50)
dev.off()

png("figures/cheap_approach/modeled_gdd2lf.png",width = 7,height = 7,units = "in",res=200)
ggpubr::ggarrange(gdd.temp.plot,gdd.geo.plot,nrow=2,ncol=1)
dev.off()

mod.stv.geo<-brm(b_chill~Geo_SD*continent+(1|iter),data=cheap.stv.small)

new.data.stv.geo<-data.frame(Geo_SD=cheap.stv.small$Geo_SD,iter=cheap.stv.small$iter,continent=cheap.stv.small$continent)

stv.geo<-predict(mod.stv.geo,newdata = new.data.stv.geo)
stv.geo<-as.data.frame(stv.geo)
new.data.stv.geo<-cbind(new.data.stv.geo,stv.geo)

png("figures/cheap_approach/geo_sd_stv.png",width = 7,height = 6,units = "in",res=200)
stv.geo.plot<-ggplot()+geom_point(data=cheap.stv,aes(Geo_SD,b_chill),size=0.3)+
  stat_summary(data=cheap.stv,aes(Geo_SD,b_chill),size=1,color="gray")+
  # geom_point(data=new.data.ggdlf,aes(Geo_SD,Estimate),color="skyblue1")+
  #  geom_errorbar(data=new.data.ggdlf,aes(Geo_SD,ymin=Q2.5,ymax=Q97.5),color="red")+
  geom_smooth(data=new.data.stv.geo,aes(Geo_SD,Estimate),method="lm",fullrange=TRUE)+
  geom_smooth(data=new.data.stv.geo,aes(Geo_SD,Q2.5),method="lm",color="red",linetype="dashed",fullrange=TRUE)+
  geom_smooth(data=new.data.stv.geo,aes(Geo_SD,Q97.5),method="lm",color="red",linetype="dashed",fullrange=TRUE)+
  facet_wrap(~continent)+theme_bw()+ylim(-100,50)
dev.off()

mod.stv.temp<-brm(b_chill~Temp_SD*continent+(1|iter),data=cheap.stv.small)

new.data.stv.temp<-data.frame(Temp_SD=cheap.stv.small$Temp_SD,iter=cheap.stv.small$iter,continent=cheap.stv.small$continent)

stv.temp<-predict(mod.stv.temp,newdata = new.data.stv.temp)
stv.temp<-as.data.frame(stv.temp)
new.data.stv.temp<-cbind(new.data.stv.temp,stv.temp)

png("figures/cheap_approach/temp_sd_stv.png",width = 7,height = 6,units = "in",res=200)
stv.temp.plot<-ggplot()+geom_point(data=cheap.stv,aes(Temp_SD,b_chill),size=0.3)+
  stat_summary(data=cheap.stv,aes(Temp_SD,b_chill),size=1,color="gray")+
  # geom_point(data=new.data.ggdlf,aes(Geo_SD,Estimate),color="skyblue1")+
  #  geom_errorbar(data=new.data.ggdlf,aes(Geo_SD,ymin=Q2.5,ymax=Q97.5),color="red")+
  geom_smooth(data=new.data.stv.temp,aes(Temp_SD,Estimate),method="lm",fullrange=TRUE)+
  geom_smooth(data=new.data.stv.temp,aes(Temp_SD,Q2.5),method="lm",color="red",linetype="dashed",fullrange=TRUE)+
  geom_smooth(data=new.data.stv.temp,aes(Temp_SD,Q97.5),method="lm",color="red",linetype="dashed",fullrange=TRUE)+
  facet_wrap(~continent)+theme_bw()+ylim(-100,50)
dev.off()

png("figures/cheap_approach/modeled_stv.png",width = 7,height = 7,units = "in",res=200)
ggpubr::ggarrange(stv.temp.plot,stv.geo.plot,nrow=2,ncol=1)
dev.off()
rangegeo<-filter(rangegeo,!is.na(continent))

maxy<-ggplot(rangegeo,aes(min.y,b_chill))+geom_point()+facet_wrap(~continent)+stat_smooth(method="lm")
miny<-ggplot(rangegeo,aes(max.y,b_chill))+geom_point()+facet_wrap(~continent)+stat_smooth(method="lm")

png("figures/cheap_approach/unmodeled_lat.png",width = 7,height = 7,units = "in",res=200)
ggpubr::ggarrange(maxy,miny,nrow=2,ncol=1)
dev.off()
save.image("cheap.mods.Rda")




##muplots
fixef(mod.ggdlf.geo)
pp_check(mod.ggdlf.temp)
pp_check(mod.stv.temp)

stop("below is scratch")

dev.off()
datalist.cheap <- with(cheap.geo, 
                        list(y = b_chill,  
                             x = Geo_SD, 
                             N = nrow(cheap.geo)
                           
                        )
)


 modstv.geo = stan('stan/cheap_model.stan', data = cheap.geo,
              iter = 3000, warmup=2000, chains=4) ## my stan hardware seems off

library(rstanarm)

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

