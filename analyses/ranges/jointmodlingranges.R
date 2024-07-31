#####Dan has made a new, cleaned lead in for rangers, so that Deirdre can save this project###
###July 31, 2024 modfied and clean from rangeleadin_osp.R

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

# libraries
library(shinystan)
library(reshape2)
library(rstan)
library(ggplot2)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("dbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")


######################################
# Flags to choose for bbstanleadin.R #
######################################

# Our flags for ranges, for now ... (see issue #379)
use.chillports = FALSE
use.zscore = TRUE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE


setwd("..//bb_analysis")
source("source/bbstanleadin.R")


# Species complex for ranges, without crops and need species that do not only have field chilling, z-scored
use.rangespp = TRUE
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE & use.rangespp==TRUE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.alltypes.ranges
  
  source("source/bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")
bb.stan$latbinum <- as.numeric(as.factor(bb.stan$latbi))


setwd("..//ranges")

source("source/cleanranges.R")##clean up the ranges data file (now call fulldat)

goodsp<-intersect(unique(bb.stan$complex.wname),fulldat$complex.wname) ##get species in ospree and ranges

bb.stan<-dplyr::filter(bb.stan,complex.wname %in% goodsp)
fulldat<-dplyr::filter(fulldat,complex.wname %in% goodsp)

bb.stan<-left_join(bb.stan,fulldat)

##seperate out the continents
bb.stan.nam <- filter(bb.stan, continent=="N. America")
bb.stan.eu <- filter(bb.stan, continent!="N. America")

##numierate latbinum
bb.stan.nam$latbinum <- as.numeric(as.factor(bb.stan.nam$latbi))
bb.stan.eu$latbinum <- as.numeric(as.factor(bb.stan.eu$latbi))


###make datalists
bb.lf.nam <- with(bb.stan.nam, 
                     list(yPhenoi = resp, 
                          forcingi = force.z,
                          photoi = photo.z,
                          chillingi = chill.z,
                          species = latbinum,
                          N = nrow(bb.stan.nam),
                          n_spec = length(unique(bb.stan.nam$complex.wname)),
                          climvar=unique(bb.stan.nam$SD.lastfrost)
                     ))

bb.lf.eu <- with(bb.stan.eu, 
                    list(yPhenoi = resp, 
                         forcingi = force.z,
                         photoi = photo.z,
                         chillingi = chill.z,
                         species = latbinum,
                         N = nrow(bb.stan.eu),
                         n_spec = length(unique(bb.stan.eu$complex.wname)),
                         climvar=unique(bb.stan.eu$SD.lastfrost)
                         
                    ))


bb.stv.nam <- with(bb.stan.nam, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.nam),
                       n_spec = length(unique(bb.stan.nam$complex.wname)),
                       climvar=unique(bb.stan.nam$STV)
                  ))

bb.stv.eu <- with(bb.stan.eu, 
                 list(yPhenoi = resp, 
                      forcingi = force.z,
                      photoi = photo.z,
                      chillingi = chill.z,
                      species = latbinum,
                      N = nrow(bb.stan.eu),
                      n_spec = length(unique(bb.stan.eu$complex.wname)),
                      climvar=unique(bb.stan.eu$STV)
                      
                 ))


bb.GDD.nam <- with(bb.stan.nam, 
                   list(yPhenoi = resp, 
                        forcingi = force.z,
                        photoi = photo.z,
                        chillingi = chill.z,
                        species = latbinum,
                        N = nrow(bb.stan.nam),
                        n_spec = length(unique(bb.stan.nam$complex.wname)),
                        climvar=unique(bb.stan.nam$Temp.Mean.GDD)
                   ))

bb.GDD.eu <- with(bb.stan.eu, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.eu),
                       n_spec = length(unique(bb.stan.eu$complex.wname)),
                       climvar=unique(bb.stan.eu$Temp.Mean.GDD)
                       
                  ))


bb.CP.nam <- with(bb.stan.nam, 
                   list(yPhenoi = resp, 
                        forcingi = force.z,
                        photoi = photo.z,
                        chillingi = chill.z,
                        species = latbinum,
                        N = nrow(bb.stan.nam),
                        n_spec = length(unique(bb.stan.nam$complex.wname)),
                        climvar=unique(bb.stan.nam$Temp.Mean.CP)
                   ))

bb.CP.eu <- with(bb.stan.eu, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.eu),
                       n_spec = length(unique(bb.stan.eu$complex.wname)),
                       climvar=unique(bb.stan.eu$Temp.Mean.CP)
                       
                  ))


###models#####
##Lastfrost
lf_jnt.nam = stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data =bb.lf.nam,
                     iter = 4000, warmup=3000) # Purring away now!

lf_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.lf.eu,
                   iter = 5000, warmup=4000)


#STV
stv_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.stv.eu,
                iter = 5000, warmup=4000)

stv_jnt.nam= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.stv.nam,
                 iter = 5000, warmup=4000)

###mean GDD in range
GDD_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.GDD.eu,
                 iter = 5000, warmup=4000)

GDD_jnt.nam= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.GDD.nam,
                  iter = 5000, warmup=4000)

#mean CP in range
CP_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.CP.eu,
                 iter = 5000, warmup=4000)

CP_jnt.nam= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.CP.nam,
                  iter = 5000, warmup=4000)
check_all_diagnostics(CP_jnt.eu)

#quick checks
summy.eu <- summary(stv_jnt.eu)$summary
summy.nam <- summary(stv_jnt.nam)$summary

summy.eu[grep(c("betaTrait"), rownames(summy.eu)),]
summy.eu[grep(c("mu"), rownames(summy.eu)),]
summy.eu[grep(c("sigma"), rownames(summy.eu)),]

summy.nam[grep(c("betaTrait"), rownames(summy.nam)),]
summy.nam[grep(c("mu"), rownames(summy.nam)),]
summy.nam[grep(c("sigma"), rownames(summy.nam)),]

#save.image("jointmodels_ranges.Rda")
