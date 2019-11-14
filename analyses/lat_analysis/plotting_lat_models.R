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
load("../lat_analysis/stan/m2l.inter.lat.z.Rda")


######################################
# Flags to choose for bbstanleadin.R #
######################################

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- FALSE
use.flags.for.allsppmodel <- FALSE
use.yourown.flagdesign <- TRUE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
   use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
   & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp =FALSE # for the main model this is false
  use.multcuespp = FALSE
  use.cropspp = FALSE
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.allsppmodel){
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = FALSE # change to false for using utah instead of chill portions (most models use chill portions z)
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
}

source("source/bbstanleadin.R")

if(use.flags.for.mainmodel){
  write.csv(bb.stan, "..//output/bbstan_mainmodel_utah_allsppwcrops_allfp_allchill.csv", row.names=FALSE) 
}

if(use.flags.for.allsppmodel){
  write.csv(bb.stan, "..//output/bbstan_allsppmodel_utahzscore_wcrops_allfp_allchill.csv", row.names=FALSE)
}

# write.csv(bb.stan, "..//output/bbstan_utahzscore_nocrops_exprampedfp_allchill.csv", row.names=FALSE)


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

lat.stan<-bb.wlat

lat.stan<-subset(lat.stan, lat.stan$resp<600)

lat.stan$lat <- lat.stan$provenance.lat

#### Responding to reviewers and re-evaluating definition of `provenance`
if(TRUE){
  lessgreatstudies <- c("caffarra11a", "caffarra11b", "Sanz-Perez09", "spann04", "spiers74", 
                        "webb78", "zohner16")
  lat.stan <- lat.stan[!(lat.stan$datasetID%in%lessgreatstudies),]
}

lat.stan$complex<-as.numeric(as.factor(lat.stan$complex.wname))

lat.stan<-na.omit(lat.stan)
source("../lat_analysis/source/bblat_zscorepreds.R")

#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "../lat_analysis/figures"
if(use.allspp==FALSE & use.expramptypes.fp==TRUE){
  figpathmore <- "latanalysis_spcom_expramp_fp_rmdatasets"
}
if(use.allspp==TRUE & use.expramptypes.fp==TRUE){
  figpathmore <- "allspp_expramp_fp"
}
#figpathmore <- "latanalysis_rmdatasets"

source("../lat_analysis/lat_muplot.R")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
my.pal <- my.pal[c(1:12, 14:29, 31:32, 34:48)]
# display.brewer.all()
my.pch <- rep(15:18, each=12)
my.pch <- my.pch[c(1:12, 14:29, 31:32, 34:48)]
alphahere = 0.4

sumer.ni <- summary(m2l.inter)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]

sort(unique(lat.stan$complex)) # numbers are alphabetical
sort(unique(lat.stan$complex.wname))


modelhere <- m2l.inter
quartz()
muplotfx(modelhere, "", 7, 8, c(0,5), c(-20, 20) , 22, 5)
#muplotfx(modelhere, "", 7, 8, c(0,5), c(-6, 4) , 4.3, 5)


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

# To plot with lat on the x axis we need to set up a vector of distances to predict: 
#newphoto <- seq(from=range(lat.stan$photo.z)[1], to=range(lat.stan$photo.z)[2], length.out=200)  
newlat <- seq(from=range(lat.stan$lat.z)[1], to=range(lat.stan$lat.z)[2], length.out=200)  


osp.lat.lophoto <- data.frame(lat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric()) ### number 14 species
osp.lat.hiphoto <- data.frame(lat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

for(i in 1:length(newlat)){
  
  osp.lat.lophoto.onelat <- mod_sum$mu_a_sp + (mod_sum$mu_b_lat_sp)*newlat[i] + 
    (mod_sum$mu_b_photo_sp)*sort(unique(lat.stan$photo.z))[1] +
    mod_sum$mu_b_pl_sp*(sort(unique(lat.stan$photo.z))[1]*newlat[i])
  osp.lat.hiphoto.onelat <-(mod_sum$mu_a_sp) + (mod_sum$mu_b_lat_sp)*newlat[i] + 
    (mod_sum$mu_b_photo_sp)*sort(unique(lat.stan$photo.z))[2] +
    mod_sum$mu_b_pl_sp*(sort(unique(lat.stan$photo.z))[2]*newlat[i])
  lophoto.df.here <-  data.frame(lat=newlat[i], resp=mean(osp.lat.lophoto.onelat),
                               fs.25=quantile(osp.lat.lophoto.onelat, 0.25), fs.75=quantile(osp.lat.lophoto.onelat, 0.75))
  hiphoto.df.here <-  data.frame(lat=newlat[i], resp=mean(osp.lat.hiphoto.onelat),
                               fs.25=quantile(osp.lat.hiphoto.onelat, 0.25), fs.75=quantile(osp.lat.hiphoto.onelat, 0.75))
  osp.lat.lophoto <- rbind(osp.lat.lophoto, lophoto.df.here)
  osp.lat.hiphoto <- rbind(osp.lat.hiphoto, hiphoto.df.here)
}


hiphoto<-mean(lat.stan$photo)+1*sd(lat.stan$photo)
lophoto<-mean(lat.stan$photo)-1*sd(lat.stan$photo)

osp.lat.hiphoto$photo <- hiphoto
osp.lat.lophoto$photo <- lophoto
osp.lat <- rbind(osp.lat.hiphoto, osp.lat.lophoto)

osp.lat$lat_trans <- (osp.lat$lat)*sd(lat.stan$lat) + mean(lat.stan$lat)
osp.lat$photo_trans <- as.character(osp.lat$photo)

latitude.allspp <- ggplot(osp.lat, aes(x=lat_trans, y=resp)) + geom_line(aes(linetype=photo_trans, col=photo_trans)) +
  geom_ribbon(aes(ymin=fs.25, ymax=fs.75, fill=photo_trans), alpha=0.1) + theme_classic() +
  scale_linetype_manual(name="Photoperiod", values=c("dashed", "solid"),
                        labels=c("7.86400247036863"="8 hours",
                                 "19.5324883417238"="20 hours")) +
  scale_color_manual(name="Photoperiod", values=c("red", "blue"),
                     labels=c("7.86400247036863"="8 hours",
                              "19.5324883417238"="20 hours")) + xlab("Latitude") +
  scale_fill_manual(name="Photoperiod", values=c("red", "blue"),
                     labels=c("7.86400247036863"="8 hours",
                              "19.5324883417238"="20 hours")) +
  ylab("Day of Budburst") + guides(fill=FALSE) +
  theme(legend.text.align = 0, legend.position = c(0.85, 0.85), legend.box.background = element_rect())

quartz()
latitude.allspp


############################################################################
################## Opposite plot now - photoperiod on x axis ###############
############################################################################
# To plot with lat on the x axis we need to set up a vector of distances to predict: 
newphoto <- seq(from=range(lat.stan$photo.z)[1], to=range(lat.stan$photo.z)[2], length.out=200)  


osp.photo.lolat <- data.frame(photo=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric()) ### number 14 species
osp.photo.hilat <- data.frame(photo=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

for(i in 1:length(newphoto)){
  
  osp.photo.lolat.onephoto <- mod_sum$mu_a_sp + (mod_sum$mu_b_photo_sp)*newphoto[i] + 
    (mod_sum$mu_b_lat_sp)*sort(unique(lat.stan$lat.z))[1] +
    mod_sum$mu_b_pl_sp*(sort(unique(lat.stan$lat.z))[1]*newphoto[i])
  osp.photo.hilat.onephoto <-(mod_sum$mu_a_sp) + (mod_sum$mu_b_photo_sp)*newphoto[i] + 
    (mod_sum$mu_b_lat_sp)*sort(unique(lat.stan$lat.z))[2] +
    mod_sum$mu_b_pl_sp*(sort(unique(lat.stan$lat.z))[2]*newphoto[i])
  lolat.df.here <-  data.frame(photo=newphoto[i], resp=mean(osp.photo.lolat.onephoto),
                                 fs.25=quantile(osp.photo.lolat.onephoto, 0.25), fs.75=quantile(osp.photo.lolat.onephoto, 0.75))
  hilat.df.here <-  data.frame(photo=newphoto[i], resp=mean(osp.photo.hilat.onephoto),
                                 fs.25=quantile(osp.photo.hilat.onephoto, 0.25), fs.75=quantile(osp.photo.hilat.onephoto, 0.75))
  osp.photo.lolat <- rbind(osp.photo.lolat, lolat.df.here)
  osp.photo.hilat <- rbind(osp.photo.hilat, hilat.df.here)
}


hilat<-mean(lat.stan$lat)+2*sd(lat.stan$lat)
lolat<-mean(lat.stan$lat)-2*sd(lat.stan$lat)

osp.photo.hilat$lat <- 80
osp.photo.lolat$lat <- 30
osp.photo <- rbind(osp.photo.hilat, osp.photo.lolat)

osp.photo$photo_trans <- (osp.photo$photo)*sd(lat.stan$photo) + mean(lat.stan$photo)
osp.photo$lat_trans <- as.character(osp.photo$lat)

photoperiod.allspp <- ggplot(osp.photo, aes(x=photo_trans, y=resp)) + geom_line(aes(linetype=lat_trans, col=lat_trans)) +
  geom_ribbon(aes(ymin=fs.25, ymax=fs.75, fill=lat_trans), alpha=0.1) + theme_classic() +
  scale_linetype_manual(name="Latitude", values=c("dashed", "solid"),
                        labels=c("7.97834632517043"="8 hours",
                                 "19.0553108948026"="19 hours")) +
  scale_color_manual(name="Latitude", values=c("red", "blue"),
                     labels=c("7.97834632517043"="8 hours",
                              "19.0553108948026"="19 hours")) + xlab("Photoperiod") +
  scale_fill_manual(name="Latitude", values=c("red", "blue"),
                    labels=c("7.97834632517043"="8 hours",
                             "19.0553108948026"="19 hours")) +
  ylab("Day of Budburst") + guides(fill=FALSE) +
  theme(legend.text.align = 0, legend.position = c(0.85, 0.85), legend.box.background = element_rect())

quartz()
photoperiod.allspp

############################################################################
######### Repeat for two extreme species... and combine in one loop ########
############################################################################

fagsyl.lat.lophoto <- data.frame(lat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric()) ### number 14 species
fagsyl.lat.hiphoto <- data.frame(lat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

picabi.lat.lophoto <- data.frame(lat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric()) ### number 18 species
picabi.lat.hiphoto <- data.frame(lat=numeric(), fs.mean=numeric(), fs.25=numeric(), fs.75=numeric())

for(i in 1:length(newlat)){
  
  fagsyl.lat.lophoto.onelat <- (mod_sum$`a_sp[14]`) + (mod_sum$`b_lat[14]`)*newlat[i] + 
    (mod_sum$`b_photo[14]`)*sort(unique(lat.stan$photo.z))[1] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$photo.z))[1]*newlat[i])
  fagsyl.lat.hiphoto.onelat <-(mod_sum$`a_sp[14]`) + (mod_sum$`b_lat[14]`)*newlat[i] + 
    (mod_sum$`b_photo[14]`)*sort(unique(lat.stan$photo.z))[2] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$photo.z))[2]*newlat[i])
  lophoto.df.here <-  data.frame(lat=newphoto[i], resp=mean(fagsyl.lat.lophoto.onelat),
                               fs.25=quantile(fagsyl.lat.lophoto.onelat, 0.25), fs.75=quantile(fagsyl.lat.lophoto.onelat, 0.75))
  hiphoto.df.here <-  data.frame(lat=newlat[i], resp=mean(fagsyl.lat.hiphoto.onelat),
                               fs.25=quantile(fagsyl.lat.hiphoto.onelat, 0.25), fs.75=quantile(fagsyl.lat.hiphoto.onelat, 0.75))
  fagsyl.lat.lophoto <- rbind(fagsyl.lat.lophoto, lophoto.df.here)
  fagsyl.lat.hiphoto <- rbind(fagsyl.lat.hiphoto, hiphoto.df.here)
  
  
  picabi.lat.lophoto.onelat <- (mod_sum$`a_sp[18]`) + (mod_sum$`b_lat[18]`)*newlat[i] + 
    (mod_sum$`b_photo[18]`)*sort(unique(lat.stan$photo.z))[1] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$photo.z))[1]*newlat[i])
  picabi.lat.hiphoto.onelat <-(mod_sum$`a_sp[18]`) + (mod_sum$`b_lat[18]`)*newlat[i] + 
    (mod_sum$`b_photo[18]`)*sort(unique(lat.stan$photo.z))[2] +
    mod_sum[["mu_b_pl_sp"]]*(sort(unique(lat.stan$photo.z))[2]*newlat[i])
  lophoto.df.here <-  data.frame(lat=newphoto[i], resp=mean(picabi.lat.lophoto.onelat),
                                 fs.25=quantile(picabi.lat.lophoto.onelat, 0.25), fs.75=quantile(picabi.lat.lophoto.onelat, 0.75))
  hiphoto.df.here <-  data.frame(lat=newlat[i], resp=mean(picabi.lat.hiphoto.onelat),
                                 fs.25=quantile(picabi.lat.hiphoto.onelat, 0.25), fs.75=quantile(picabi.lat.hiphoto.onelat, 0.75))
  picabi.lat.lophoto <- rbind(picabi.lat.lophoto, lophoto.df.here)
  picabi.lat.hiphoto <- rbind(picabi.lat.hiphoto, hiphoto.df.here)
  
  
}

hiphoto<-mean(lat.stan$photo)+1*sd(lat.stan$photo)
lophoto<-mean(lat.stan$photo)-1*sd(lat.stan$photo)

fagsyl.lat.hiphoto$photo <- hiphoto
fagsyl.lat.lophoto$photo <- lophoto
fagsyl.lat <- rbind(fagsyl.lat.hiphoto, fagsyl.lat.lophoto)
fagsyl.lat$species <- "FAGSYL"

picabi.lat.hiphoto$photo <- hiphoto
picabi.lat.lophoto$photo <- lophoto
picabi.lat <- rbind(picabi.lat.hiphoto, picabi.lat.lophoto)
picabi.lat$species <- "PICABI"

photoxlat <- rbind(fagsyl.lat, picabi.lat)

photoxlat$lat_trans <- (photoxlat$lat)*sd(lat.stan$lat) + mean(lat.stan$lat)
photoxlat$photo_trans <- as.character(photoxlat$photo)

cols <- c("#1F78B4", "#E31A1C")
photoperiod <- ggplot(photoxlat, aes(x=lat_trans, y=resp)) + geom_line(aes(linetype=photo_trans, alpha=photo_trans, col=species)) +
  geom_ribbon(aes(ymin=fs.25, ymax=fs.75, fill=species), alpha=0.1) + theme_classic() +
  scale_linetype_manual(name="Photoperiod", values=c("dashed", "solid"),
                        labels=c("7.97834632517043"="8 hours",
                                 "19.0553108948026"="19 hours")) +
  scale_alpha_manual(name="Photoperiod", values=c(0.3, 1),
                     labels=c("7.97834632517043"="8 hours",
                              "19.0553108948026"="19 hours")) + xlab("Latitude") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                               "PICABI"=expression(paste(italic("Picea abies"))))) +
  scale_fill_manual(name="Species", values=cols,
                    labels=c("FAGSYL"=expression(paste(italic("Fagus sylvatica"))),
                             "PICABI"=expression(paste(italic("Picea abies"))))) +
  ylab("Day of Budburst") + guides(fill=FALSE) +
  theme(legend.text.align = 0)

quartz()
photoperiod

