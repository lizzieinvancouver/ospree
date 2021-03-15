####ranges joint model build off of models_stanforranges.R which has lots of stuff on it
##that Dan is have trouble running

## Started 8 June 2020 ##
## By Lizzie ##

## Where'd Lizzie go? She disappeared ... #
## but wait! It's 3 July 2020 and she's back. ##

## Take 1: Stole this code from bb_analysis/models_stan.R

## To do
# (a) subset down to relevant block/transplant treatments for gomory15??
# Impt: not dealing with provenance and material (which mean some treatments show up more than once but the partial pooling should handle this we think)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# libraries
library(shinystan)
library(reshape2)
library(rstan)
library(rstanarm)
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

setwd("..//ranges")
bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")

bb.stan$latbinum <- as.numeric(as.factor(bb.stan$latbi))


rangiesEu<-read.csv("output/Synthesis_climate_EUspsw.csv")
rangiesNa<-read.csv("output/Synthesis_climate_Namsps_weighted.csv")

rangiesEu$continent<-"Europe"
rangiesEu<-dplyr::select(rangiesEu,-X)
rangiesNa$continent<-"N. America"
rangies<-rbind(rangiesEu,rangiesNa)

###GDD2LF
ggdlf<-filter(rangies,variable=="GDD.lastfrost")
ggdlf<-dplyr::select(ggdlf,species,Temp.SD,continent)
colnames(ggdlf)[1]<-"complex.wname"

##STV
STV<-filter(rangies,variable=="MeanTmins")
STV<-dplyr::select(STV,species,Temp.SD,continent)
colnames(STV)[c(1,2)]<-c("complex.wname","STV")



ggdlf<-left_join(ggdlf,STV)

## remove duplicat4e for alnus incana
ggdlf<-dplyr::filter(ggdlf,(complex.wname!="Alnus_incana") | (continent!="Europe"))

#bb.stan<-filter(bb.stan,complex.wname!="Ulmus_minor")

goodsp<-intersect(unique(bb.stan$complex.wname),ggdlf$complex.wname)

intersect(ggdlf$complex.wname,unique(bb.stan$complex.wname))
bb.stan<-filter(bb.stan,complex.wname %in% goodsp)
ggdlf<-filter(ggdlf,complex.wname %in% goodsp)


bb.stan<-left_join(bb.stan,ggdlf)

##Range size
area<-read.csv("output/rangeareas.csv")
area<-select(area,-continent)
colnames(area)[2]<-"complex.wname"


bb.stan<-left_join(bb.stan,area)

bb.stan$Temp.SD.cent<-bb.stan$Temp.SD-mean(bb.stan$Temp.SD)
bb.stan$STV.cent<-bb.stan$STV-mean(bb.stan$STV)
bb.stan$Temp.SD.z<-(bb.stan$Temp.SD-mean(bb.stan$Temp.SD))/sd(bb.stan$Temp.SD)
bb.stan$STV.z<-(bb.stan$STV-mean(bb.stan$STV))/sd(bb.stan$STV)

bb.force.only <- with(bb.stan, 
                  list(yPhenoi = resp, 
                       forcingi = force.z, 
                       species = latbinum,
                       N = nrow(bb.stan),
                       n_spec = length(unique(bb.stan$complex.wname)),
                       climvar=bb.stan$Temp.SD.z
                  ))

forceonly = stan('stan/jointish_climvar_db.stan', data = bb.force.only,
              iter = 6000, warmup=4000)



bb.chill.only <- with(bb.stan, 
                      list(yPhenoi = resp, 
                           forcingi = chill.z, 
                           species = latbinum,
                           N = nrow(bb.stan),
                           n_spec = length(unique(bb.stan$complex.wname)),
                           climvar=bb.stan$Temp.SD.cent
                      ))

chillonly = stan('stan/jointish_climvar_db.stan', data = bb.chill.only,
                 iter = 6000, warmup=4000)
summary(chillonly)

goobsum<-summary(forceonly )$summary

goobsum[grep("muPhenoSp", rownames(goobsum)),]
goobsum[grep("muForceSp", rownames(goobsum)),] # why is the forcing effect so small? Is it coause theere is no chilling and photoperiod in the model?
goobsum[grep("betaTraitxPheno", rownames(goobsum)),]## 


bb.3param <- with(bb.stan, 
                      list(yPhenoi = resp, 
                           forcingi = force.z,
                           photoi = photo.z,
                           chillingi = chill.z,
                           species = latbinum,
                           N = nrow(bb.stan),
                           n_spec = length(unique(bb.stan$complex.wname)),
                           climvar=bb.stan$Temp.SD.cent
                      ))

threeparam_jnt = stan('stan/joint_climvar_3param.stan', data = bb.3param,
                 iter = 6000, warmup=4000)


goobsum<-summary(threeparam_jnt)$summary

goobsum[grep("mu", rownames(goobsum)),] # why is the forcing effect so small? Is it coause theere is no chilling and photoperiod in the model?
goobsum[grep("betaTrait", rownames(goobsum)),]## should be negative


##try NAM only

bb.stan.nam<-filter(bb.stan, continent=="N. America")

##runierate atbinum
bb.stan.nam$latbinum <- as.numeric(as.factor(bb.stan.nam$latbi))


bb.3param.nam <- with(bb.stan.nam, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.nam),
                       n_spec = length(unique(bb.stan.nam$complex.wname)),
                       climvar=bb.stan.nam$Temp.SD.cent
                  ))

threeparam_jnt.nam = stan('stan/joint_climvar_3param.stan', data = bb.3param.nam,
                      iter = 6000, warmup=4000)
