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
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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
              iter = 2500, warmup=1500, control=list(max_treedepth = 15,adapt_delta = 0.99))}

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
  m2l.inter = stan('../lat_analysis/stan/winter_2level_lat.stan', data = datalist.lat.nonz,iter = 3000, warmup=2000, control=list(max_treedepth = 12,adapt_delta = 0.99))
}

check_all_diagnostics(m2l.inter)
#pl<- plot(m2l.iter, pars="b_", ci.lvl=0.5) 
#launch_shinystan(m2l.inter)

m2l.inter.sum <- summary(m2l.inter)$summary
m2l.inter.sum[grep("mu_", rownames(m2l.inter.sum)),]
m2l.inter.sum[grep("sigma_", rownames(m2l.inter.sum)),]

ys<-datalist.lat.nonz$y
# posterior predictive checks....
if(FALSE){
  y_pred <- extract(m2l.inter, 'y_ppc')
  
  par(mfrow=c(1,2))
  hist(lat.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
  hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
  
  library(bayesplot)
  y <- as.vector(lat.stan$resp)
  yrep <- extract(m2l.inter)
  yrep <- yrep$yhat
  ppc <- ppc_stat(y, yrep)
  ppc.max <- ppc_stat(y, yrep, stat = "max")
  ppc.min <- ppc_stat(y, yrep, stat = "min")
  ppc.sd <- ppc_stat(y, yrep, stat = "sd")
  
  library(egg)
  quartz()
  grid.arrange(ppc, ppc.sd, ppc.max, ppc.min, ncol=2, nrow=2)
  
  
}

# Code if you want to save your models (do NOT push output to git)
# Note that use.chillports is NOT generally included below ... expect when use.chillports==TRUE
if(use.chillports == TRUE & use.zscore == FALSE){
  save(m2l.inter, file="../lat_analysis/stan/m2l.inter.lat.chillport.nonz.Rda")
}
if(use.chillports == FALSE & use.zscore == FALSE){
  save(m2l.inter, file="../lat_analysis/stan/m2l.inter.lat.nonz.Rda")
}

if(use.chillports == TRUE & use.zscore == TRUE){
  save(m2l.inter, file="../lat_analysis/stan/m2l.inter.lat.chillportz.Rda")
}
if(use.chillports == FALSE & use.zscore == TRUE){
  save(m2l.inter, file="../lat_analysis/stan/m2l.inter.latz.Rda")
}



