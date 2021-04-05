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
options(mc.cores = parallel::detectCores())

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
range(bb.stan$Temp.SD.z)
if(FALSE){ # Skip the force-only model for now...
bb.force.only <- with(bb.stan, 
                  list(yPhenoi = resp, 
                       forcingi = force.z, 
                       species = latbinum,
                       N = nrow(bb.stan),
                       n_spec = length(unique(bb.stan$complex.wname)),
                       climvar=bb.stan$Temp.SD.z
                  ))

forceonly = stan('popUP/stan/jointish_climvar_db.stan', data = bb.force.only,
              iter = 6000, warmup=4000)



goobsum<-summary(forceonly )$summary

goobsum[grep("muPhenoSp", rownames(goobsum)),]
goobsum[grep("muForceSp", rownames(goobsum)),] #
goobsum[grep("betaTraitxPheno", rownames(goobsum)),]## 
}

og.ospree <- with(bb.stan, 
                  list(y = resp, 
                       force = force.z,
                       photo = photo.z,
                       chill = chill.z,
                       sp = latbinum,
                       N = nrow(bb.stan),
                       n_sp = length(unique(bb.stan$complex.wname))
                  ))


###run original Ospree model for comparision
m2l.ni = stan('..//bb_analysis/stan/nointer_2level.stan', data = og.ospree,
              iter = 4000, warmup=2500) 

### run out range model
bb.3param <- with(bb.stan, 
                      list(yPhenoi = resp, 
                           forcingi = force.z,
                           photoi = photo.z,
                           chillingi = chill.z,
                           species = latbinum,
                           N = nrow(bb.stan),
                           n_spec = length(unique(bb.stan$complex.wname)),
                           climvar=unique(bb.stan$Temp.SD.z)
                      ))

threeparam_jnt = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
                 iter = 4000, warmup=2500)



### extract ospree
m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]
betasOSP<-as.data.frame(m2lni.sum[grep("b_", rownames(m2lni.sum)),])
betasOSP$cue<-NA
betasOSP$cue[grepl("b_force", rownames(betasOSP))]<-"force"
betasOSP$cue[grepl("b_photo", rownames(betasOSP))]<-"photo"
betasOSP$cue[grepl("b_chill", rownames(betasOSP))]<-"chill"

betasOSP$param<-NA
betasOSP$param[grepl("mu", rownames(betasOSP))]<-"mu"
betasOSP$param[grepl("sigma", rownames(betasOSP))]<-"sigma"
betasOSP$param[is.na(betasOSP$param)]<-"beta[sp]"

betasOSP$model<-"OSPREE"

#extract 3 range model
threeparam_jntsum <- summary(threeparam_jnt)$summary
threeparamys<-as.data.frame(threeparam_jntsum)

threeparamys$cue<-NA
threeparamys$cue[grepl("Forcing", rownames(threeparamys))]<-"force"
threeparamys$cue[grepl("Photo", rownames(threeparamys))]<-"photo"
threeparamys$cue[grepl("Chill", rownames(threeparamys))]<-"chill"

threeparamys$param<-NA
threeparamys$param[grepl("mu", rownames(threeparamys))]<-"mu"
threeparamys$param[grepl("sigma", rownames(threeparamys))]<-"sigma"
threeparamys$param[grepl("alpha", rownames(threeparamys))]<-"alpha[sp]"
threeparamys$param[grepl("beta", rownames(threeparamys))]<-"beta[sp]"
threeparamys$param[grepl("betaTraitx", rownames(threeparamys))]<-"beta[range]"

threeparamys$model<-"Ranges"

outy<-rbind(threeparamys,betasOSP)


write.csv(outy,"betasandmorefromPOPUP.csv",row.names = TRUE)
# On 26 March 2021 the code above runs!
# Who knows about the below ...

save.image("popupmods.Rda")


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
