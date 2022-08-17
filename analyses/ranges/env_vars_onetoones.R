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
library(dplyr)
library(ggplot2)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
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


#load("popupmods.Rda")
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

####read in data files
rangiesEu<-read.csv("output/Synthesis_climate_EUsps_STVfinalchill.csv") ### updated STV
rangiesNa<-read.csv("output/Synthesis_climate_NAMsps_STVfinal_nacho_chill.csv") ## updated stv

NAnames<-read.csv("output/Synthesis_climate_NAMsps_STVfinal_nacho.csv")

species<-rep(unique(NAnames$species),each=7)
rangiesNa$species<-species
colnames(rangiesNa)
colnames(rangiesEu)
rangiesNa<-rangiesNa[,c(1,2,3,4,5,7,6)]
rangiesEu$continent<-"Europe"
rangiesEu<-dplyr::select(rangiesEu,-X)
rangiesNa$continent<-"N. America"
rangiesNa<-dplyr::select(rangiesNa,-X)

rangies<-rbind(rangiesEu,rangiesNa)

###GDD2LF
ggdlf<-filter(rangies,variable=="GDD.lastfrost")

VarGDD2lf.plot<-ggplot(ggdlf,aes(Temp.SD,Geo.SD))+geom_point(aes(color=continent))+geom_abline(intercept=0,slope=1)+
  ggthemes::theme_few()


##STV
#STV<-filter(rangies,variable=="MeanTmins")


##MeanForcing
GDD<-filter(rangies,variable=="GDD")
GDD.plot<-ggplot(GDD,aes(Temp.Mean,Geo.Mean))+geom_point(aes(color=continent))+geom_abline(intercept=0,slope=1)+
  ggthemes::theme_few()



CP<-filter(rangies,variable=="Mean.Chill.Portions")
CP.plot<-ggplot(CP,aes(Temp.Mean,Geo.Mean))+geom_point(aes(color=continent))+geom_abline(intercept=0,slope=1)+
  ggthemes::theme_few()


jpeg("figures/onetonesAUG2022.jpeg")
ggpubr::ggarrange(VarGDD2lf.plot,GDD.plot,CP.plot,nrow=1, ncol=3,common.legend = TRUE)
dev.off()

ggdlf<-left_join(ggdlf,STV)
ggdlf<-left_join(ggdlf,GDD)
ggdlf<-dplyr::left_join(ggdlf,CP)

head(ggdlf)



#bb.stan<-filter(bb.stan,complex.wname!="Ulmus_minor")
###need to recode all North America species names to match format of bb.stan
ggdlf$complex.wname[which(ggdlf$complex.wname=="betulent")]<- "Betula_lenta"
ggdlf$complex.wname[which(ggdlf$complex.wname=="popugran")]<- "Populus_grandidentata"
ggdlf$complex.wname[which(ggdlf$complex.wname=="querrubr")]<- "Quercus_rubra"
ggdlf$complex.wname[which(ggdlf$complex.wname=="acerpens")]<- "Acer_pensylvanicum"
ggdlf$complex.wname[which(ggdlf$complex.wname=="betupapy")]<- "Betula_papyrifera"
ggdlf$complex.wname[which(ggdlf$complex.wname=="fraxnigr")]<- "Fraxinus_nigra"
ggdlf$complex.wname[which(ggdlf$complex.wname=="alnurubr")]<- "Alnus_rubra"
ggdlf$complex.wname[which(ggdlf$complex.wname=="pseumenz")]<- "Pseudotsuga_menziesii"
ggdlf$complex.wname[which(ggdlf$complex.wname=="prunpens")]<- "Prunus_pensylvanica"
ggdlf$complex.wname[which(ggdlf$complex.wname=="betualle")]<- "Betula_alleghaniensis"
ggdlf$complex.wname[which(ggdlf$complex.wname=="acersacr")]<- "Acer_saccharum"
ggdlf$complex.wname[which(ggdlf$complex.wname=="acerrubr")]<- "Acer_rubrum"
ggdlf$complex.wname[which(ggdlf$complex.wname=="corycorn")]<- "Corylus_cornuta"
ggdlf$complex.wname[which(ggdlf$complex.wname=="piceglau")]<- "Picea_glauca"
ggdlf$complex.wname[which(ggdlf$complex.wname=="fagugran")]<- "Fagus_grandifolia"
ggdlf$complex.wname[which(ggdlf$complex.wname=="robipseu")]<- "Robinia_pseudoacacia"
ggdlf$complex.wname[which(ggdlf$complex.wname=="poputrem")]<- "Populus_tremuloides"
ggdlf$complex.wname[which(ggdlf$complex.wname=="alnurugo")]<- "Alnus_incana"

ggdlf<-dplyr::filter(ggdlf,(complex.wname!="Alnus_incana") | (continent!="Europe"))

goodsp<-intersect(unique(bb.stan$complex.wname),ggdlf$complex.wname)

intersect(ggdlf$complex.wname,unique(bb.stan$complex.wname))
setdiff(ggdlf$complex.wname,unique(bb.stan$complex.wname))

bb.stan<-filter(bb.stan,complex.wname %in% goodsp)
ggdlf<-filter(ggdlf,complex.wname %in% goodsp)


