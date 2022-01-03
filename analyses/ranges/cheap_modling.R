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

load("cheap.mods.Rda")
posties<-read.csv("output/cue_posteriors.csv") ##read in both data
rangiesEu<-read.csv("output/Synthesis_climate_EUsps_corr.csv")
rangiesNa<-read.csv("output/Synthesis_climate_Namsps_weighted.csv")

#area<-read.csv("output/rangeareas.csv")
#head(rangiesNa,14)
##clean North America names
#rangiesNa$species[which(rangiesNa$species=="betulent")]<- "Betula_lenta"
#rangiesNa$species[which(rangiesNa$species=="popugran")]<- "Populus_grandidentata"
#rangiesNa$species[which(rangiesNa$species=="fagugran")]<- "Fagus_grandifolia"
#rangiesNa$species[which(rangiesNa$species=="querrubr")]<- "Quercus_rubra"
#rangiesNa$species[which(rangiesNa$species=="acerpens")]<- "Acer_pensylvanicum"
#rangiesNa$species[which(rangiesNa$species=="betupapy")]<- "Betula_papyrifera"
#rangiesNa$species[which(rangiesNa$species=="fraxnigr")]<- "Fraxinus_nigra"
#rangiesNa$species[which(rangiesNa$species=="robipseu")]<- "Robinia_pseudoacacia"
#rangiesNa$species[which(rangiesNa$species=="pseumenz")]<- "Pseudotsuga_menziesii"
#rangiesNa$species[which(rangiesNa$species=="prunpens")]<- "Prunus_pensylvanicum"
#rangiesNa$species[which(rangiesNa$species=="poputrem")]<- "Populus_tremuloides"
#rangiesNa$species[which(rangiesNa$species=="betualle")]<- "Betula_alleghaniensis"
#rangiesNa$species[which(rangiesNa$species=="acersacr")]<- "Acer_saccharum"
#rangiesNa$species[which(rangiesNa$species=="acerrubr")]<- "Acer_rubrum"
#rangiesNa$species[which(rangiesNa$species=="alnurugo")]<- "Alnus_incana"
#rangiesNa$species[which(rangiesNa$species=="corycorn")]<- "Corylus_cornuta"
#rangiesNa$species[which(rangiesNa$species=="piceglau")]<- "Picea_glauca"
#rangiesNa$species[which(rangiesNa$species=="picemari")]<- "Picea_mariana"
#unique(rangiesNa$species)
rangiesEu$continent<-"Europe"
rangiesEu<-dplyr::select(rangiesEu,-X)
rangiesNa$continent<-"N. America"

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

GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Geo.SD,Temp.SD))

a<-ggplot(GDD.lastfrost,aes(Geo.SD,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_few(base_size = 10)+ylab("Temporal variation \nin GDDs to last frost")+
  xlab("Geographic variation \nin GDDs to last frost")+annotate("text", x = 40, y = 150, 
label = "Correlation= 0.88
         EU= 0.37
         NA= 0.83")

colnames(MeanTmins)[4]<-"STV"
#MeanTmins<-dplyr::select(MeanTmins,STV,species)
#GDD.lastfrost<-left_join(GDD.lastfrost,MeanTmins)
###add few other climate paramenters
GDD.lastfrost$STV<-MeanTmins$STV
GDD.lastfrost$Geo.Mean.GDD<-GDD$Geo.Mean
GDD.lastfrost$Temp.Mean.GDD<-GDD$Temp.Mean
GDD.lastfrost$Geo.Mean.Chill<-Mean.Chill.Portions$Geo.Mean
GDD.lastfrost$Temp.Mean.Chill<-Mean.Chill.Portions$Temp.Mean

cor(GDD.lastfrost$Temp.SD,GDD.lastfrost$Geo.SD)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(STV,Temp.SD))

b<-ggplot(GDD.lastfrost,aes(STV,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_few(base_size = 10)+ylab("Temporal variation in \nGDDs to last frost")+
  xlab("Spring temperature variability \n(STV)")+annotate("text", x = 2.5, y = 150,
label = "Correlation= 0.789
         EU= -.39
         NA =.75")

cor(GDD.lastfrost$STV,GDD.lastfrost$Geo.SD)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(STV,Geo.SD))

c<-ggplot(GDD.lastfrost,aes(STV,Geo.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_few(base_size = 10)+ylab("Geographic variation in \nGDDs to last frost")+
  xlab("Spring temperature variability \n(STV)")+annotate("text", x = 2.5, y = 80, 
label = "Correlation= 0.639
          EU= -.18
          NA= 0.5")

ggpubr::ggarrange(a,b,c,ncol=1,nrow=3)

#cor(GDD.lastfrost$Geo.Mean.GDD,GDD.lastfrost$Geo.Mean.Chill)
#GDD.lastfrost %>%
 # dplyr::group_by(continent) %>%
  #dplyr::summarize(COR=cor(Temp.Mean.Chill,Temp.Mean.GDD))

#ggplot(GDD.lastfrost,aes(Temp.Mean.Chill,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  #scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)



cor(GDD.lastfrost$Temp.Mean.Chill,GDD.lastfrost$Temp.SD)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Temp.Mean.Chill,Temp.SD))
####################
cor(GDD.lastfrost$Temp.Mean,GDD.lastfrost$Temp.SD)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Temp.Mean,Temp.SD))

d<-ggplot(GDD.lastfrost,aes(Temp.Mean,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+
  ylab("Temporal variation in GDDs to last frost")+
  xlab("Temporal Mean GDD to last frost ")+annotate("text", x = 50, y =150, 
                                                                    label = "Correlation= 0.89
          EU= .96
          NA= .78")

ggpubr::ggarrange(a,b,c,d,nrow=2,ncol=2)


cor(GDD.lastfrost$Temp.Mean.GDD,GDD.lastfrost$Temp.SD)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Temp.Mean.GDD,Temp.SD))

e<-ggplot(GDD.lastfrost,aes(Temp.Mean.GDD,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+
  ylab("Temporal variation in GDDs to last frost")+
  xlab("Mean GDDs in range ")+annotate("text", x = 250, y =120, 
                                                    label = "Correlation= 0.36
          EU= .70
          NA= .81")

ggpubr::ggarrange(a,b,c,d,e,nrow=3,ncol=2)

cor(GDD.lastfrost$Temp.Mean.Chill,GDD.lastfrost$Temp.SD)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Temp.Mean.Chill,Temp.SD))
g<-ggplot(GDD.lastfrost,aes(Temp.Mean.Chill,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+
  ylab("Temporal variation in GDDs to last frost")+
  xlab("Mean Chill Portions in range ")+annotate("text", x = 35, y =120, 
                                       label = "Correlation= -0.31
          EU= .66
          NA= .80")
ggpubr::ggarrange(a,b,c,d,e,g,nrow=3,ncol=2)

cor(GDD.lastfrost$Temp.Mean.Chill,GDD.lastfrost$STV)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Temp.Mean.Chill,STV))

h<-ggplot(GDD.lastfrost,aes(Temp.Mean.Chill,STV))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+
  ylab("Interannual spring temperature variability (STV)")+
  xlab("Mean Chill Portions in range ")+annotate("text", x = 35, y =4,
                                                 label = "Correlation= -0.44
          EU= -.91.
          NA= .44")


cor(GDD.lastfrost$Temp.Mean.GDD,GDD.lastfrost$STV)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Temp.Mean.GDD,STV))

i<-ggplot(GDD.lastfrost,aes(Temp.Mean.GDD,STV))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+
  ylab("Interannual spring temperature variability (STV)")+
  xlab("Mean GDD's in range ")+annotate("text", x = 230, y =4,
                                                 label = "Correlation= 0.16
          EU= -.56
          
          NA= .54")

ggpubr::ggarrange(a,b,c,nrow=1,ncol=3,common.legend=TRUE)
ggpubr::ggarrange(e,h,g,i,nrow=2,ncol=2,common.legend=TRUE)

jpeg("figures/clim_params.jpeg",width=8,height=6,units = "in",res=200)
#ggpubr::ggarrange(a,b,c,nrow=1,ncol=3,common.legend=TRUE,labels=c("a)","b)","c)"))
a
dev.off()




cor(GDD.lastfrost$Temp.Mean.GDD,GDD.lastfrost$Temp.Mean.Chill)
GDD.lastfrost %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Geo.Mean.Chill,Geo.Mean.GDD))



options(scipen = 999)
area<-select(area,-continent)
area<-left_join(GDD.lastfrost,area)

area %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(STV,range_area))
d<-ggplot(area,aes(range_area,STV))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis(discrete=TRUE,begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+ylab("Interannual spring temperature variability (STV)")+
  xlab("Range area (km^2)")+annotate("text", x = 8000000, y = 4, 
label = "Correlation= -.07
         EU=.59
         NA =-.42")


area %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Temp.SD,range_area))

e<-ggplot(area,aes(range_area,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis(discrete=TRUE,begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+ylab("Temporal variation in GDDs to last frost")+
  xlab("Range area (km^2)")+annotate("text", x = 8000000, y = 100, 
label = "Correlation= -.39
                  EU= -.41
                  NA= -.73")

cor(area$range_area,area$Geo.SD)

area %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Geo.SD,range_area))

f<-ggplot(area,aes(range_area,Geo.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis(discrete=TRUE,begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+ylab("Geographic variation in GDDs to last frost")+
  xlab("Range area (km^2)")+annotate("text", x = 8000000, y = 75, 
  label = "Correlation= -.349
            EU= -.03
            NA= -.78")



### what about latitude extent
extent<-read.csv("output/zolder_datagrabs/full_extent_data.csv")
extent<-dplyr::select(extent,species,cent.lat,lat.extent)
area<-left_join(area,extent)

cor(area$lat.extent,area$cent.lat,use = "complete.obs")
cor(area$range_area,area$cent.lat,use = "complete.obs")

area %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(lat.extent,cent.lat,use = "complete.obs"))

g<-ggplot(area,aes(cent.lat,lat.extent))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis(discrete=TRUE,begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+ylab("Range extent (latititude)")+
  xlab("range centroid (latitude)")+annotate("text", x = 40, y = 40, 
  label = "Correlation= 0.51
            EU= 0.6
            NA= 0.47")

cor(area$range_area,area$cent.lat,use = "complete.obs")

area %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(range_area,cent.lat,use = "complete.obs"))

j<-ggplot(area,aes(range_area,cent.lat))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis(discrete=TRUE,begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+ylab("range centroid (latitude)")+
  xlab("Range area (km2)")+annotate("text", x = 10000000, y = 58, 
   label = "Correlation= 0.34
            EU= 0.22
            NA= 0.88")


cor(area$lat.extent,area$STV,use = "complete.obs")
area %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(lat.extent,STV,use = "complete.obs"))

h<-ggplot(area,aes(lat.extent,STV))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis(discrete=TRUE,begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+ylab("Interannual spring temperature variability (STV)")+
  xlab("Range extent (latititude)")+annotate("text", x = 15, y = 4, 
label = "Correlation= 0.15
          EU= .73
          NA= -.03")

cor(area$lat.extent,area$Temp.SD,use = "complete.obs")
area %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(lat.extent,Temp.SD,use = "complete.obs"))

i<-ggplot(area,aes(lat.extent,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis(discrete=TRUE,begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+ylab("Temporal variation in GDDs to last frost")+
  xlab("Range extent (latititude)")+annotate("text", x = 15, y = 120, 
      label = "Correlation= -0.18,
      EU= -.59
      NA= -.19")


cor(area$lat.extent,area$Geo.SD,use = "complete.obs")
area %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(lat.extent,Geo.SD,use = "complete.obs"))

k<-ggplot(area,aes(lat.extent,Geo.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",color="black")+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis(discrete=TRUE,begin = 0,end=.5)+ggthemes::theme_base(base_size = 10)+ylab("Geographic variation in GDDs to last frost")+
  xlab("Range extent (latititude)")+annotate("text", x = 10, y = 75,
    label = "Correlation= -0.17,
    EU= .08
    NA= -.31")

aa<-ggpubr::ggarrange(a,b,c,nrow=1)
bb<-ggpubr::ggarrange(g,j,nrow=1)
cc<-ggpubr::ggarrange(d,e,f,h,i,k,nrow=2,ncol=3)
jpeg("figures/clim_params.jpeg",width=14,height=10,units = "in",res=200)
ggpubr::ggarrange(a,b,c,g,j,d,e,f,h,i,nrow=2,ncol=5,common.legend = TRUE,
                  labels = c("a)","b)","c)","d)","e)","f)","g)","h)","i)","j)"))

dev.off()


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

jpeg("figures/continental_cues.jpeg",height= 5, width=8, units = "in",res=200)
ggpubr::ggarrange(a,b,c, nrow=1,ncol=3)
dev.off()

area2<-area
area<-left_join(posties,area)


rangegeo<-read.csv("output/zolder_datagrabs/full_extent_data.csv") ##not sure this is the best extent data
rangegeo<-left_join(posties,rangegeo)
rangegeo<-filter(rangegeo,species %in% unique(rangies$species))


#####plots

dev.off()




library(brms)

### try it with fewer iterations

area<-dplyr::filter(area,iter>3500)
area<-dplyr::filter(area,iter>3800)
table(area$continent)

goo<-posties %>% dplyr::group_by(species) %>%dplyr::summarise(meanchill=mean(b_chill),sdchill=sd(b_chill),
                                                              meanforce=mean(b_force),sdforce=sd(b_force),
                                                              meanphoto=mean(b_photo),sdphoto=sd(b_photo))
goo<-left_join(goo,area2)


mod.ggdlf.chill.nopool<-brm(b_chill~Temp.SD,data=area)

na.area<-filter(area,continent=="N. America")
mod.ggdlf.chill.nopool.a<-brm(b_chill~Temp.SD,data=na.area)
fixef(mod.ggdlf.chill.nopool.a)
fixef(mod.ggdlf.chill.nopool.cont)

mod.alt.c<-brm(meanchill|mi(sdchill)~Temp.SD,data=goo)
mod.alt.f<-brm(meanforce|mi(sdforce)~Temp.SD,data=goo)
mod.alt.p<-brm(meanphoto|mi(sdphoto)~Temp.SD,data=goo)

mod.area.c<-brm(meanchill|mi(sdchill)~lat.extent,data=goo)
mod.area.p<-brm(meanphoto|mi(sdphoto)~lat.extent,data=goo)
mod.area.f<-brm(meanforce|mi(sdforce)~lat.extent,data=goo)
fixef(mod.alt.p)

mod.area2.f<-brm(meanforce|mi(sdforce)~lat.extent*continent,data=goo)
mod.area2.c<-brm(meanchill|mi(sdchill)~lat.extent*continent,data=goo)
mod.area2.p<-brm(meanphoto|mi(sdphoto)~lat.extent*continent,data=goo)

mod.ggdlf.chill.nopool.cont<-brm(b_chill~Temp.SD*continent,data=area)
mod.ggdlf.chill.pool.cont<-brm(b_chill~Temp.SD*continent+(1|iter),data=area)

fixef(mod.ggdlf.chill.pool.cont)
fixef(mod.alt2.c)

pp_check(mod.ggdlf.chill.pool.cont,nsamples = 100)
pp_check(mod.alt2.c,nsamples = 100)

mod.alt2.c<-brm(meanchill|mi(sdchill)~Temp.SD*continent,data=goo)
mod.alt2.f<-brm(meanforce|mi(sdforce)~Temp.SD*continent,data=goo,iter=4000,warmup=3000)
#mod.alt2.p<-brm(meanphoto|mi(sdphoto)~Temp.SD*continent,data=goo,iter=8000,warmup=6000,control=list(adapt_delta=.999))
get_prior(meanphoto|mi(sdphoto)~Temp.SD*continent,data=goo)

fixef(mod.stv2.c)
fixef(mod.alt2.c)
fixef(mod.stvna)
fixef(mod.tempna)
pp_check(mod.tempna,nsamples = 100)
nadata<-filter(goo, continent=="N. America")


mod.stvna<-brm(meanchill|mi(sdchill)~STV,data=nadata)
mod.tempna<-brm(meanchill|mi(sdchill)~Temp.SD,data=nadata)


mod.stv2.c<-brm(meanchill|mi(sdchill)~STV*continent,data=goo)
mod.stv2.f<-brm(meanforce|mi(sdforce)~STV*continent,data=goo)
mod.stv2.p<-brm(meanphoto|mi(sdphoto)~STV*continent,data=goo)



pp_check(mod.alt,nsamples = 100)
pp_check(mod.ggdlf.chill.nopool.cont,nsamples = 100)

fixef(mod.ggdlf.chill.nopool.cont)
goober<-as.data.frame(fixef(mod.alt,probs = c(.025,.25,.75,.975)))
goober <- goober %>% filter(row.names(goober) %in% c("Temp.SD","Temp.SD:continentN.America"))
goober$continent<-row.names(goober)
ggplot(goober,aes(Estimate,continent))+geom_point(shape=1)+geom_errorbarh(aes(xmax=Q75,xmin=Q25),height=0)+
  geom_errorbarh(aes(xmax=Q97.5,xmin=Q2.5),height=0,linetype="dotted")

fixef(mod.alt2)
mod.ggdlf.photo.nopool<-brm(b_photo~Temp.SD,data=area)
mod.ggdlf.force.nopool<-brm(b_force~Temp.SD,data=goo)

#does this give the same resutl as a measurement erro model?


conditional_effects(mod.ggdlf.chill.nopool.cont,conditions ="Temp.SD:continent" )
?conditional_effects()
conditional_effects(mod.alt2.c)
conditional_effects(mod.stv2.c)


conditional_effects(mod.ggdlf.photo.nopool)
pp_check(mod.ggdlf.chill.nopool)## nor a good fit
chilly<-as.data.frame(fixef(mod.ggdlf.chill.nopool,probs = c(.025,.25,.75,.975)))
chilly$param<-"chilling"
photoy<-as.data.frame(fixef(mod.ggdlf.photo.nopool,probs = c(.025,.25,.75,.975)))
photoy$param<-"photoperiod"
forcy<-as.data.frame(fixef(mod.ggdlf.force.nopool,probs = c(.025,.25,.75,.975)))
forcy$param<-"forcing"
plotyggd2lf<-rbind(chilly,forcy,photoy)
plotyggd2lf <- plotyggd2lf %>% filter(row.names(plotyggd2lf) %in% c("Temp.SD","Temp.SD1","Temp.SD2"))
jpeg("figures/seq_mu_rough.jpeg",height=4, width=12, unit="in", res=200)
ggplot(plotyggd2lf,aes(Estimate,param))+geom_point(shape=1)+geom_errorbarh(aes(xmax=Q75,xmin=Q25),height=0)+
  geom_errorbarh(aes(xmax=Q97.5,xmin=Q2.5),height=0,linetype="dotted")+geom_vline(xintercept=0)+xlim(-0.03,0.03)+ggthemes::theme_base(base_size = 10)+
  xlab("\u0394 cue strength per \u0394 unit of variance of growing degree day to last frost")+ ylab("cue sensitivity")
dev.off()
ggplot(area,aes(iter,b_chill))+stat_summary()
ggplot(area,aes(species,b_chill))+stat_summary()

mod.ggdlf.chill.cont.sp<-brm(b_chill~Temp.SD+species+continent+Temp.SD:continent,data=area)
library(lme4)
summary(lmer(b_chill~Temp.SD+(1|species)+(1|iter),data=area))
pp_check(mod.ggdlf.chill.nopool.cont)
mod.ggdlf.photo.nopool.cont<-brm(b_photo~Temp.SD*continent,data=area)
mod.ggdlf.force.nopool.cont<-brm(b_force~Temp.SD*continent,data=area)

save.image("cheap.mods.Rda")
stop("below is scratch")


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

