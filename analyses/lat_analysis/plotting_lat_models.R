#### 13 March 2019 - by Cat
## Working on final figures for Latitude Analysis
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

library(RColorBrewer)
library(egg)
library(rstan)
library(brms) ## for posterior_samples function to make APC plot

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## load the model
load("../lat_analysis/stan/m2l.inter.lat.nonz.Rda")


# dostan = TRUE
use.chillports = TRUE# change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = TRUE # change to false to use raw predictors

# Default is species complex and no crops
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE

# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE

#Default is all chilling data
use.expchillonly = FALSE # change to true for only experimental chilling 
#note: with only exp chilling, there is only exp photo and force too.
#also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
source("source/bbstanleadin.R")

bb.wlat <- bb.stan
bb.wlat <- within(bb.wlat, { prov.lat <- ave(provenance.lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
bb.wlat <- subset(bb.wlat, bb.wlat$prov.lat>1)  
tt <- table(bb.wlat$complex.wname)### testing 
#Abies_alba           Acer_complex    Acer_pseudoplatanus         Acer_saccharum 
#16                     46                     36                     19 
#Aesculus_hippocastanum        Alnus_glutinosa           Alnus_incana         Betula_complex 
#19                     16                     22                     27 
#Betula_pendula       Betula_pubescens       Carpinus_betulus            Cornus_alba 
#269                    190                     19                     13 
#Corylus_avellana        Fagus_sylvatica       Fraxinus_complex     Fraxinus_excelsior 
#31                    157                     38                     17 
#Larix_decidua            Picea_abies        Populus_complex        Populus_tremula 
#59                    185                     14                     21 
#Prunus_avium         Prunus_complex           Prunus_padus  Pseudotsuga_menziesii 
#13                     33                     20                    114 
#Quercus_complex        Quercus_faginea           Quercus_ilex        Quercus_petraea 
#29                     22                     15                     29 
#Quercus_robur          Quercus_rubra          Salix_complex       Sorbus_aucuparia 
#20                     13                     21                     16 
#Syringa_vulgaris          Tilia_cordata          Ulmus_complex      Vaccinium_complex 
#16                     14                    180                     18 

#myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica", "Picea_abies", "Pseudotsuga_menziesii", "Ulmus_complex")
#bb.wlat.spp<-subset(bb.wlat, complex.wname%in%myspp)

#lat.stan<-bb.wlat.spp
lat.stan<-bb.wlat
#write.csv(lat.stan, "~/Documents/git/ospree/analyses/lat_analysis/lat_output/lat_arm.csv", row.names = FALSE)
#lat.stan<-subset(bb.wlat.spp, bb.wlat.spp$resp<600)
lat.stan<-subset(lat.stan, lat.stan$resp<600)

lat.stan$lat <- lat.stan$provenance.lat

lat.stan$complex<-as.numeric(as.factor(lat.stan$complex.wname))

lat.stan<-na.omit(lat.stan)

#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "../lat_analysis/figures"
if(use.allspp==FALSE & use.expramptypes.fp==TRUE){
  figpathmore <- "spcom_expramp_fp"
}
if(use.allspp==TRUE & use.expramptypes.fp==TRUE){
  figpathmore <- "allspp_expramp_fp"
}


source("../lat_analysis/lat_muplot.R")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

sumer.ni <- summary(m2l.inter)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]

sort(unique(lat.stan$complex)) # numbers are alphabetical
sort(unique(lat.stan$complex.wname))


modelhere <- m2l.inter
quartz()
muplotfx(modelhere, "", 7, 8, c(0,5), c(-20, 20) , 22, 5)
#muplotfx(modelhere, "Utah", 7, 8, c(0,5), c(-20, 15) , 17, 5)


########### Posterior Predictive Checks #############

if(FALSE){
  y_pred <- extract(m2l.inter, 'y_ppc')
  par(mfrow=c(1,2))
  hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
  hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}


lats <- rstanarm::posterior_predict(m2l.inter, "mu_b_lat_sp", draws = 500)

########### Now for the interaction plot #############

mod_sum <- posterior_samples(m2l.inter)

# To plot with photo on the x axis we need to set up a vector of distances to predict: 
newphoto <- seq(from=range(lat.stan$photo.z)[1], to=range(lat.stan$photo.z)[2], length.out=200)  
#newlat <- seq(from=range(lat.stan$photo)[1], to=range(lat.stan$photo)[2], length.out=200)  


### Repeat for two extreme species... and combine in one loop
osp.photo.lolat <- data.frame(photo=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric()) ### number 14 species
osp.photo.hilat <- data.frame(photo=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

for(i in 1:length(newphoto)){
  
  osp.photo.lolat.onephoto <- mod_sum$mu_a_sp + (mod_sum$mu_b_photo_sp)*newphoto[i] + 
    (mod_sum$mu_b_lat_sp)*sort(unique(lat.stan$lat.z))[1] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$lat.z))[1]*newphoto[i])
  osp.photo.hilat.onephoto <-(mod_sum$mu_a_sp) + (mod_sum$mu_b_photo_sp)*newphoto[i] + 
    (mod_sum$mu_b_lat_sp)*sort(unique(lat.stan$lat.z))[2] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$lat.z))[2]*newphoto[i])
  lolat.df.here <-  data.frame(photo=newphoto[i], resp=mean(osp.photo.lolat.onephoto),
                               fs.25=quantile(osp.photo.lolat.onephoto, 0.25), fs.75=quantile(osp.photo.lolat.onephoto, 0.75))
  hilat.df.here <-  data.frame(photo=newphoto[i], resp=mean(osp.photo.hilat.onephoto),
                               fs.25=quantile(osp.photo.hilat.onephoto, 0.25), fs.75=quantile(osp.photo.hilat.onephoto, 0.75))
  osp.photo.lolat <- rbind(osp.photo.lolat, lolat.df.here)
  osp.photo.hilat <- rbind(osp.photo.hilat, hilat.df.here)
}

hilat<-mean(mod_sum$mu_b_lat_sp)+1*sd(mod_sum$mu_b_lat_sp)
lolat<-mean(mod_sum$mu_b_lat_sp)-1*sd(mod_sum$mu_b_lat_sp)

osp.photo.hilat$lat <- hilat
osp.photo.lolat$lat <- lolat
osp.photo <- rbind(osp.photo.hilat, osp.photo.lolat)

osp.photo$photo_trans <- (osp.photo$photo)*sd(lat.stan$photo) + mean(lat.stan$photo)
osp.photo$lat_trans <- as.character((osp.photo$lat)*sd(lat.stan$lat) + mean(lat.stan$lat))

photoperiod.allspp <- ggplot(osp.photo, aes(x=photo_trans, y=resp)) + geom_line(aes(linetype=lat_trans, col=lat_trans)) +
  geom_ribbon(aes(ymin=fs.25, ymax=fs.75, fill=lat_trans), alpha=0.1) + theme_classic() +
  scale_linetype_manual(name="Latitude", values=c("dashed", "solid"),
                        labels=c("10.5078"="10.5078",
                                 "64.2669"="64.2669")) +
  scale_color_manual(name="Latitude", values=c("red", "blue"),
                     labels=c("10.5078"="10.5078",
                              "64.2669"="64.2669")) + xlab("Photoperiod (hrs)") +
  ylab("Day of Budburst") + guides(fill=FALSE) +
  theme(legend.text.align = 0, legend.position = c(0.75, 0.85), legend.box.background = element_rect(), panel.border = element_rect())

quartz()
photoperiod.allspp

grid.arrange()


### Repeat for two extreme species... and combine in one loop
fagsyl.photo.lolat <- data.frame(photo=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric()) ### number 14 species
fagsyl.photo.hilat <- data.frame(photo=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

picabi.photo.lolat <- data.frame(photo=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric()) ### number 18 species
picabi.photo.hilat <- data.frame(photo=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

for(i in 1:length(newphoto)){
  
  fagsyl.photo.lolat.onephoto <- (mod_sum$mu_a_sp + mod_sum$`a_sp[14]`) + (mod_sum$mu_b_photo_sp + mod_sum$`b_photo[14]`)*newphoto[i] + 
    (mod_sum$mu_b_lat_sp + mod_sum$`b_lat[14]`)*sort(unique(lat.stan$lat.z))[1] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$lat.z))[1]*newphoto[i])
  fagsyl.photo.hilat.onephoto <-(mod_sum$mu_a_sp + mod_sum$`a_sp[14]`) + (mod_sum$mu_b_photo_sp + mod_sum$`b_photo[14]`)*newphoto[i] + 
    (mod_sum$mu_b_lat_sp + mod_sum$`b_lat[14]`)*sort(unique(lat.stan$lat.z))[2] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$lat.z))[2]*newphoto[i])
  lolat.df.here <-  data.frame(photo=newphoto[i], resp=mean(fagsyl.photo.lolat.onephoto),
                               fs.25=quantile(fagsyl.photo.lolat.onephoto, 0.25), fs.75=quantile(fagsyl.photo.lolat.onephoto, 0.75))
  hilat.df.here <-  data.frame(photo=newphoto[i], resp=mean(fagsyl.photo.hilat.onephoto),
                               fs.25=quantile(fagsyl.photo.hilat.onephoto, 0.25), fs.75=quantile(fagsyl.photo.hilat.onephoto, 0.75))
  fagsyl.photo.lolat <- rbind(fagsyl.photo.lolat, lolat.df.here)
  fagsyl.photo.hilat <- rbind(fagsyl.photo.hilat, hilat.df.here)
  
  
  picabi.photo.lolat.onephoto <- (mod_sum$mu_a_sp + mod_sum$`a_sp[18]`) + (mod_sum$mu_b_photo_sp + mod_sum$`b_photo[18]`)*newphoto[i] + 
    (mod_sum$mu_b_lat_sp + mod_sum$`b_lat[18]`)*sort(unique(lat.stan$lat.z))[1] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$lat.z))[1]*newphoto[i])
  picabi.photo.hilat.onephoto <-(mod_sum$mu_a_sp + mod_sum$`a_sp[18]`) + (mod_sum$mu_b_photo_sp + mod_sum$`b_photo[18]`)*newphoto[i] + 
    (mod_sum$mu_b_lat_sp + mod_sum$`b_lat[18]`)*sort(unique(lat.stan$lat.z))[2] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$lat.z))[2]*newphoto[i])
  lolat.df.here <-  data.frame(photo=newphoto[i], resp=mean(picabi.photo.lolat.onephoto),
                               fs.25=quantile(picabi.photo.lolat.onephoto, 0.25), fs.75=quantile(picabi.photo.lolat.onephoto, 0.75))
  hilat.df.here <-  data.frame(photo=newphoto[i], resp=mean(picabi.photo.hilat.onephoto),
                               fs.25=quantile(picabi.photo.hilat.onephoto, 0.25), fs.75=quantile(picabi.photo.hilat.onephoto, 0.75))
  picabi.photo.lolat <- rbind(picabi.photo.lolat, lolat.df.here)
  picabi.photo.hilat <- rbind(picabi.photo.hilat, hilat.df.here)
  
  
}

hilat<-mean(mod_sum$mu_b_lat_sp)+1*sd(mod_sum$mu_b_lat_sp)
lolat<-mean(mod_sum$mu_b_lat_sp)-1*sd(mod_sum$mu_b_lat_sp)

fagsyl.photo.hilat$lat <- hilat
fagsyl.photo.lolat$lat <- lolat
fagsyl.photo <- rbind(fagsyl.photo.hilat, fagsyl.photo.lolat)
fagsyl.photo$species <- "FAGSYL"

picabi.photo.hilat$lat <- hilat
picabi.photo.lolat$lat <- lolat
picabi.photo <- rbind(picabi.photo.hilat, picabi.photo.lolat)
picabi.photo$species <- "PICABI"

photoxlat <- rbind(fagsyl.photo, picabi.photo)

photoxlat$photo_trans <- (photoxlat$photo)*sd(lat.stan$photo) + mean(lat.stan$photo)
photoxlat$lat_trans <- as.character((photoxlat$lat)*sd(lat.stan$lat) + mean(lat.stan$lat))

cols <- c("#1F78B4", "#E31A1C")
photoperiod <- ggplot(photoxlat, aes(x=photo_trans, y=resp)) + geom_line(aes(linetype=lat_trans, alpha=lat_trans, col=species)) +
  geom_ribbon(aes(ymin=fs.25, ymax=fs.75, fill=species), alpha=0.1) + theme_classic() +
  scale_linetype_manual(name="Latitude", values=c("dashed", "solid"),
                        labels=c("10.5078"="10.5078",
                                 "64.2669"="64.2669")) +
  scale_alpha_manual(name="Latitude", values=c(0.3, 1),
                     labels=c("10.5078"="10.5078",
                              "64.2669"="64.2669")) + xlab("Photoperiod (hrs)") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "PICABI"=expression(paste(italic("Picea abies"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "PICABI"=expression(paste(italic("Picea abies"))))) +
  ylab("Day of Budburst") + guides(fill=FALSE) +
  theme(legend.text.align = 0, legend.position = c(0.75, 0.85))

quartz()
photoperiod

