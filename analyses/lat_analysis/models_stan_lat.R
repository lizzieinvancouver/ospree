## Started 6 July 2017 ##
## By Cat and Dan and others ##
# Updated a tiny bit by Dan 19 June 2018
# Updated 8 Oct 2018
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

library(shinystan)
library(RColorBrewer)
library(egg)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# dostan = TRUE
use.chillports = TRUE# change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = FALSE # change to false to use raw predictors

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

#z-scored models
if(use.chillports == FALSE & use.zscore == TRUE){
  source("../lat_analysis/source/bblat_zscorepreds.R")
  datalist.lat <- with(lat.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         lat = lat.z,
                         sp = complex,
                         N = nrow(lat.stan),
                         n_sp = length(unique(lat.stan$complex))
                    )
)
}

if(use.chillports == TRUE & use.zscore == TRUE){
  source("../lat_analysis/source/bblat_zscorepreds.R")
  datalist.lat <- with(lat.stan, 
                       list(y = resp, 
                            chill = chill.ports.z, 
                            force = force.z, 
                            photo = photo.z,
                            lat = lat.z,
                            sp = complex,
                            N = nrow(lat.stan),
                            n_sp = length(unique(lat.stan$complex))
                       )
  )
}

if(use.zscore == TRUE){m2l.inter = stan('../lat_analysis/stan/winter_2level_lat.stan', data = datalist.lat,
              iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99))}

check_all_diagnostics(m2l.inter)
#pl<- plot(m2l.iter, pars="b_", ci.lvl=0.5) 
launch_shinystan(m2l.inter)

m2l.inter.sum <- summary(m2l.inter)$summary
m2l.inter.sum[grep("mu_", rownames(m2l.inter.sum)),]
m2l.inter.sum[grep("sigma_", rownames(m2l.inter.sum)),]

ys<-datalist.lat$y
# posterior predictive checks....
if(FALSE){
  y_pred <- extract(m2l.inter, 'y_ppc')
  
  par(mfrow=c(1,2))
  hist(lat.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
  hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}

# Code if you want to save your models (do NOT push output to git)
# Note that use.chillports is NOT generally included below ... expect when use.chillports==TRUE
if(use.chillports == TRUE & use.zscore == TRUE){
  save(m2l.inter, file="../lat_analysis/stan/m2l.inter.lat.chillport.z.Rda")
}
if(use.chillports == FALSE & use.zscore == TRUE){
  save(m2l.inter, file="../lat_analysis/stan/m2l.inter.lat.z.Rda")
}

##Non-z-scored models

if(use.chillports == FALSE & use.zscore == FALSE){
  datalist.lat.nonz <- with(lat.stan, 
                          list(y = resp, 
                               chill = chill, 
                               force = force, 
                               photo = photo,
                               lat = lat,
                               sp = complex,
                               N = nrow(lat.stan),
                               n_sp = length(unique(lat.stan$complex))
                          )
)
}
if(use.chillports == TRUE & use.zscore == FALSE){
  datalist.lat.nonz <- with(lat.stan, 
                            list(y = resp, 
                                 chill = chill.ports, 
                                 force = force, 
                                 photo = photo,
                                 lat = lat,
                                 sp = complex,
                                 N = nrow(lat.stan),
                                 n_sp = length(unique(lat.stan$complex))
                            )
  )
}
if(use.zscore == FALSE){
  m2l.inter = stan('../lat_analysis/stan/winter_2level_lat.stan', data = datalist.lat.nonz,iter = 2500, warmup=1500, control=list(max_treedepth = 12,adapt_delta = 0.99))
}

check_all_diagnostics(m2l.inter)
check_all_diagnostics(m2l.inter)
#pl<- plot(m2l.iter, pars="b_", ci.lvl=0.5) 
launch_shinystan(m2l.inter)

m2l.inter.sum <- summary(m2l.inter.)$summary
m2l.inter.sum[grep("mu_", rownames(m2l.inter.)),]
m2l.inter.sum[grep("sigma_", rownames(m2l.inter.)),]

ys<-datalist.lat$y
# posterior predictive checks....
if(FALSE){
  y_pred <- extract(m2l.inter, 'y_ppc')
  
  par(mfrow=c(1,2))
  hist(lat.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
  hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}

# Code if you want to save your models (do NOT push output to git)
# Note that use.chillports is NOT generally included below ... expect when use.chillports==TRUE
if(use.chillports == TRUE & use.zscore == FALSE){
  save(m2l.inter, file="../lat_analysis/stan/m2l.inter.lat.chillport.nonz.Rda")
}
if(use.chillports == FALSE & use.zscore == FALSE){
  save(m2l.inter, file="../lat_analysis/stan/m2l.inter.lat.nonz.Rda")
}




############# Interaction Plots #################
lats<-rstan::extract(m2l.inter, 'mu_b_lat_sp')
lats<-as.vector(lats$mu_b_lat_sp)
photos<-rstan::extract(m2l.inter, 'mu_b_photo_sp')
photos<-as.vector(photos$mu_b_photo_sp)
resps<-rstan::extract(m2l.inter, 'y_ppc')
resps<-as.vector(resps$y_ppc)
pl<-rstan::extract(m2l.inter, "mu_b_pl_sp")
pl<-as.vector(pl$mu_b_pl_sp)
alphas<-extract(m2l.inter, 'mu_a_sp')
alphas<-as.vector(alphas$mu_a_sp)

inter<-as.data.frame(cbind(lats, photos))
inter<-as.data.frame(cbind(inter, resps))
inter<-as.data.frame(cbind(inter, alphas))
inter<-as.data.frame(cbind(inter, pl))

hilat<-0+1*sd(inter$lats)
lolat<-0-1*sd(inter$lats)

hipho<-0+1*sd(inter$photos)
lopho<-0-1*sd(inter$photos)

y_hilat<-alphas + lats*2.59 + photos + pl*2.59
y_lolat<-alphas + lats*(-2.59) + photos + pl*(-2.59)

inter<-as.data.frame(cbind(inter, y_hilat))
inter<-as.data.frame(cbind(inter, y_lolat))

#foo<-inter[sample(nrow(inter), 4000), ]

quartz()
ggplot(inter, aes(x=photos, y=resps)) + geom_smooth(aes(x=photos, y=y_hilat, col="High"), stat="smooth", method="lm", size=1, se=FALSE) + 
  geom_line(aes(x=photos, y=y_lolat, col="Low"), stat="smooth", method="lm", size=1, se=FALSE) +
  ylab("Day of Budburst") + xlab("Photoperiod") + 
  scale_color_manual(name="Latitude", values=c(High='darkblue',Low='darkred')) + theme_classic()
  

#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
if(use.allspp==FALSE & use.expramptypes.fp==TRUE){
  figpathmore <- "spcom_expramp_fp"
}
if(use.allspp==TRUE & use.expramptypes.fp==TRUE){
  figpathmore <- "allspp_expramp_fp"
}


source("lat_muplot.R")
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
muplotfx(modelhere, "Chillports", 7, 8, c(0,5), c(-20, 20) , 22, 5)
muplotfx(modelhere, "Utah", 7, 8, c(0,5), c(-20, 15) , 17, 5)


########### Posterior Predictive Checks #############

if(FALSE){
  y_pred <- extract(m2l.inter, 'y_ppc')
  par(mfrow=c(1,2))
  hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
  hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}


lats <- rstanarm::posterior_predict(m2l.inter, "mu_b_lat_sp", draws = 500)

