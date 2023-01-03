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
#ggdlf<-dplyr::select(ggdlf,species,Temp.SD,continent)
colnames(ggdlf)[5]<-"complex.wname"


##STV
#STV<-filter(rangies,variable=="MeanTmins")
#STV<-dplyr::select(STV,species,Temp.SD,continent)
#colnames(STV)[c(1,2)]<-c("complex.wname","STV")

##MeanForcing
GDD<-filter(rangies,variable=="GDD")
#GDD<-dplyr::select(GDD,species,Geo.Mean,continent)
colnames(GDD)[c(1,2,3,4)]<-c("Temp.Mean.GDD","Temp.SD.GDD","Geo.Mean.GDD","Geo.SD.GDD")
colnames(GDD)[5]<-"complex.wname"
#CP<-filter(rangies,variable=="Mean.Chill.Portions")
#CP<-dplyr::select(CP,species,Geo.Mean,continent)
#colnames(CP)[c(1,2)]<-c("complex.wname","ChP")

GDD<-select(GDD,-variable)

#ggdlf<-left_join(ggdlf,STV)
ggdlf<-left_join(ggdlf,GDD)
#ggdlf<-dplyr::left_join(ggdlf,CP)

#head(ggdlf)



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

bb.stan<-left_join(bb.stan,ggdlf)
##########correlations
ggdlf %>%
  dplyr::group_by(continent) %>%
  dplyr::summarize(COR=cor(Temp.SD,Geo.SD))

jpeg("figures/clim_params.jpeg",width=8,height=6,units = "in",res=300)
ggplot(ggdlf,aes(Geo.SD,Temp.SD))+geom_point(aes(color=continent))+stat_smooth(method="lm",aes(color=continent),se=FALSE,linetype="dashed",size=.4)+
  scale_color_viridis_d(begin = 0,end=.5)+ggthemes::theme_few(base_size = 10)+ylab("Temporal standard deviations \nof GDDs to last frost")+
  xlab("Geographic standard deviations \nof GDDs to last frost")+annotate("text", x = 40, y = 10, 
                                                                label = "Correlation:\nEurope= 0.62 \nN. America= 0.87")
dev.off()

a<-ggplot(ggdlf,aes(Temp.SD,Geo.SD))+geom_point(aes(color=continent))+geom_smooth(method="lm",aes(color=continent))
b<-ggplot(ggdlf,aes(Temp.SD,Temp.Mean))+geom_point(aes(color=continent))+geom_smooth(method="lm",aes(color=continent))
c<-ggplot(ggdlf,aes(Geo.SD,Geo.Mean))+geom_point(aes(color=continent))+geom_smooth(method="lm",aes(color=continent))
d<-ggplot(ggdlf,aes(Temp.Mean,Geo.Mean))+geom_point(aes(color=continent))+geom_smooth(method="lm",aes(color=continent))



h<-ggplot(ggdlf,aes(Temp.SD.GDD,Temp.Mean.GDD))+geom_point(aes(color=continent))+geom_smooth(method="lm",aes(color=continent))+facet_wrap(~continent,scales="free")
i<-ggplot(ggdlf,aes(Geo.SD.GDD,Geo.Mean.GDD))+geom_point(aes(color=continent))+geom_smooth(method="lm",aes(color=continent))+facet_wrap(~continent,scales="free")
ggpubr::ggarrange(a,b,c,d,common.legend = TRUE)


######zscore and center
bb.stan$Temp.SD.cent<-bb.stan$Temp.SD-mean(bb.stan$Temp.SD)
#bb.stan$STV.cent<-bb.stan$STV-mean(bb.stan$STV)
bb.stan$Temp.SD.z<-(bb.stan$Temp.SD-mean(bb.stan$Temp.SD))/sd(bb.stan$Temp.SD)
#bb.stan$STV.z<-(bb.stan$STV-mean(bb.stan$STV))/sd(bb.stan$STV)


#bb.stan$GDD.z<-(bb.stan$GDD-mean(bb.stan$GDD))/sd(bb.stan$GDD)
#bb.stan$CP.z<-(bb.stan$ChP-mean(bb.stan$ChP))/sd(bb.stan$ChP)



## NAM (North America) only

bb.stan.nam <- filter(bb.stan, continent=="N. America")
bb.stan.eu <- filter(bb.stan, continent!="N. America")


unique(bb.stan.eu$complex.wname)
unique(bb.stan.eu$range.z)

##runierate atbinum
bb.stan.nam$latbinum <- as.numeric(as.factor(bb.stan.nam$latbi))
bb.stan.eu$latbinum <- as.numeric(as.factor(bb.stan.eu$latbi))

###North America
bb.gddlf.nam <- with(bb.stan.nam, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.nam),
                       n_spec = length(unique(bb.stan.nam$complex.wname)),
                       climvar=unique(bb.stan.nam$Temp.SD)
                  ))



#bb.stan.eu$scaleSD<-bb.stan.eu$Temp.SD.z*4
bb.gddlf.eu <- with(bb.stan.eu, 
                      list(yPhenoi = resp, 
                           forcingi = force.z,
                           photoi = photo.z,
                           chillingi = chill.z,
                           species = latbinum,
                           N = nrow(bb.stan.eu),
                           n_spec = length(unique(bb.stan.eu$complex.wname)),
                           climvar=unique(bb.stan.eu$Temp.SD)
                           
                      ))



###models
gddlf_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.gddlf.eu,
                          iter = 5000, warmup=4000) #

#check_all_diagnostics(gddlf_jnt.eu)
#summary(gddlf_jnt.eu)
#launch_shinystan(gddlf_jnt.eu)
#stv_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.stv.eu,
 #                  iter = 5000, warmup=4000) #runs
#check_all_diagnostics(stv_jnt.eu)

#stv_area.eu= stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.area.eu,
 #                iter = 5000, warmup=4000)
#check_all_diagnostics(stv_area.eu)

if(FALSE){
gddlf_jnt.nam = stan('popUP/stan/joint_climvar_3param_osp.stan', data =bb.gddlf.nam,
                     iter = 4000, warmup=3000,control = list(adapt_delta=0.999)) #10 divergent transitions
}

gddlf_jnt.nam = stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data =bb.gddlf.nam,
                     iter = 4000, warmup=3000) # Purring away now!
# (NCP on photo got it down to 1 divergent trans, NCP on forcing took it home!)


summy.eu<- summary(gddlf_jnt.eu)$summary
summy.nam<- summary(gddlf_jnt.nam)$summary


summy.eu[grep(c("betaTrait"), rownames(summy.eu)),]
summy.eu[grep(c("mu"), rownames(summy.eu)),]
summy.eu[grep(c("sigma"), rownames(summy.eu)),]


summy.nam[grep(c("betaTrait"), rownames(summy.nam)),]
summy.nam[grep(c("mu"), rownames(summy.nam)),]
summy.nam[grep(c("sigma"), rownames(summy.nam)),]

save.image("popupmods.Rda")

if(FALSE){
## START Lizzie (Jan 2023: Playing around with figures) ###

## North America 
namforalpha <- summy.nam[grep(c("alphaForcingSp\\["), rownames(summy.nam)),]
namforbeta <- summy.nam[grep(c("betaForcingSp\\["), rownames(summy.nam)),]
namforbetatrait <- summy.nam[grep(c("betaTraitxForcing"), rownames(summy.nam)),]

# Sanity check -- rebuild the math... namforbeta = namforalpha + forbeta*range
testy <- namforalpha[,"mean"] + namforbetatrait[["mean"]]*bb.gddlf.nam$climvar
plot(namforbeta[,"mean"]~testy)
# Good news, math works!

## Europe
euforalpha <- summy.eu[grep(c("alphaForcingSp\\["), rownames(summy.eu)),]
euforbeta <- summy.eu[grep(c("betaForcingSp\\["), rownames(summy.eu)),]
euforbetatrait <- summy.eu[grep(c("betaTraitxForcing"), rownames(summy.eu)),]

# Sanity check -- rebuild the math... euforbeta = euforalpha + forbeta*range
testy <- euforalpha[,"mean"] + euforbetatrait[["mean"]]*bb.gddlf.eu$climvar
# plot(euforbeta[,"mean"]~testy)
# Good news, math works!

par(mfrow=c(2,2))
pchhere <- 16
plot(namforalpha[,"mean"]~bb.gddlf.nam$climvar, pch=pchhere, main="N. America", xlab="Range climate", ylab="unexplained by range")
plot(namforbeta[,"mean"]~bb.gddlf.nam$climvar, pch=pchhere, main="N. America", xlab="Range climate", ylab="full cue by range")
plot(euforalpha[,"mean"]~bb.gddlf.eu$climvar, pch=pchhere, main="Europe", xlab="Range climate", ylab="unexplained by range")
plot(euforbeta[,"mean"]~bb.gddlf.eu$climvar, pch=pchhere, main="Europe", xlab="Range climate", ylab="full cue by range")

# What about ratios?
quartz()
par(mfrow=c(1,2))
hist(namforalpha[,"mean"]/namforbeta[,"mean"])
hist(euforalpha[,"mean"]/euforbeta[,"mean"])

# Okay, get out of negative zone ...
valueadded <- 60
namforalphaadd <- namforalpha[,"mean"] + valueadded
namforbetaaadd <- namforbeta[,"mean"] + valueadded
euforalphaadd <- euforalpha[,"mean"] + valueadded
euforbetaaadd <- euforbeta[,"mean"] + valueadded

# Get ratios ...
par(mfrow=c(1,2))
hist((namforbetaaadd-namforalphaadd)/namforbetaaadd)
hist((euforbetaaadd-euforalphaadd)/euforbetaaadd)

# Take the mean of the absolute differences for the species-level
quartz()
par(mfrow=c(1,2))
hist(abs(namforbeta[,"mean"]-namforalpha[,"mean"]))
hist(abs(euforbeta[,"mean"]-euforalpha[,"mean"]))

# Variance instead?
var(namforalpha[,"mean"])/var(namforbeta[,"mean"])
var(euforalpha[,"mean"])/var(euforbeta[,"mean"])


## END Lizzie (Jan 2023: Playing around with figures) ###
}

#check_all_diagnostics(gddlf_jnt.nam)
#summary(gddlf_jnt.nam)
#launch_shinystan(gddlf_jnt.nam)


#stv_jnt.nam = stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data =bb.stv.nam,
#                     iter = 4000, warmup=3000) ## Dan ran this too.

#check_all_diagnostics(stv_jnt.nam)
#area_jnt.nam = stan('popUP/stan/joint_climvar_3param_osp.stan', data =bb.area.nam,
 #                  iter = 6000, warmup=5000,control = list(adapt_delta=0.99))

#check_all_diagnostics(area_jnt.nam)

# mean GDD predictor models, no longer using
if(FALSE){ 
gdd_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.gdd.eu,
                   iter = 5000, warmup=4000) #


#cp_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.cp.eu,
#                 iter = 5000, warmup=4000)

gdd_jnt.nam= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.gdd.nam,
                  iter = 5000, warmup=4000) #
}

#check_all_diagnostics(gddlf_jnt.eu)
#cp_jnt.nam= stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data = bb.cp.nam,
 #               iter = 5000, warmup=4000)

#check_all_diagnostics(cp_jnt.eu)


stop()

if (FALSE){
stvout.eu<-scrape(stv_jnt.eu)
stvout.nam<-scrape(stv_jnt.nam)

gddout.eu<-scrape(gddlf_jnt.eu)
gddout.nam<-scrape(gddlf_jnt.nam)

areaout.eu<-scrape(stv_area.eu)
areaout.nam<-scrape(area_jnt.nam)

###columns
stvout.eu$continent<-"Europe"
stvout.nam$continent<-"N. America"
stvcont<-rbind(stvout.eu,stvout.nam)

gddout.eu$continent<-"Europe"
gddout.nam$continent<-"N. America"
gddcont<-rbind(gddout.eu,gddout.nam)

#areaout.eu$continent<-"Europe"
#areaout.nam$continent<-"N. America"
#areacont<-rbind(areaout.eu,areaout.nam)

#areacont$climparam<-"area"
stvcont$climparam<-"stv"
gddcont$climparam<-"gdd2lf"

outycont<-rbind(stvcont,gddcont)#,areacont)

write.csv(dplyr::filter(outycont, grepl("Trait",rowname)),"seperate_ests.csv",row.names=FALSE)

write.csv(outycont,"betasandmorefromPOPUP_continent.csv",row.names = FALSE)




# Lizzie playing around with another model
# Can we include continent in our current model so we don't have to split them?
if(FALSE){
# add continent dummy variable to bb.stan ...
bb.stan$continentdummy <- 1
bb.stan$continentdummy[which(bb.stan$continent=="Europe")] <- 0
continentquick <- subset(bb.stan, select=c("complex.wname", "continentdummy"))
continentquick <- continentquick[!duplicated(continentquick), ]
                        
    
bb.3paramcont.gddlf <- with(bb.stan, 
                      list(yPhenoi = resp, 
                           forcingi = force.z,
                           photoi = photo.z,
                           chillingi = chill.z,
                           species = latbinum,
                           N = nrow(bb.stan),
                           n_spec = length(unique(bb.stan$complex.wname)),
                           climvar=unique(bb.stan$Temp.SD.z),
                           continent=continentquick$continentdummy
                      ))

goober = stan('popUP/stan/joint_climvar_3paramwCont.stan', data=bb.3paramcont.gddlf,
              iter = 4000, warmup=3000)

goobsumCont <- summary(goober)$summary
goobsumCont[grep("mu", rownames(goobsumCont)),]

goobsumCont[grep("betaTraitx", rownames(goobsumCont)),]
goobsumCont[grep("betaFS", rownames(goobsumCont)),]
goobsumCont[grep("betaPS", rownames(goobsumCont)),]
goobsumCont[grep("betaCS", rownames(goobsumCont)),]

## sequntial for just forcing
bb.seq.gddlf<- with(bb.stan, 
                            list(y = resp, 
                                 force = force.z,
                                 photo = photo.z,
                                 chill = chill.z,
                                 species = latbinum,
                                 N = nrow(bb.stan),
                                 n_sp = length(unique(bb.stan$complex.wname)),
                                 sp = latbinum,
                                 climvar=unique(bb.stan$Temp.SD.z),
                                 continent=continentquick$continentdummy
                            ))




goober2 = stan('popUP/stan/seq_climvar.stan', data=bb.seq.gddlf,
              iter = 4000, warmup=3000)

goobsum2Cont <- summary(goober2)$summary

    }


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

if(FALSE){ # Skip the original for now.
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
}
if (FALSE){
  ### run range model for all 3 parameters of interest, gdd2lf, stv, range area
  bb.3param.gddlf <- with(bb.stan, 
                          list(yPhenoi = resp, 
                               forcingi = force.z,
                               photoi = photo.z,
                               chillingi = chill.z,
                               species = latbinum,
                               N = nrow(bb.stan),
                               n_spec = length(unique(bb.stan$complex.wname)),
                               climvar=unique(bb.stan$Temp.SD.z)
                          ))
  
  
  bb.3param.stv <- with(bb.stan, 
                        list(yPhenoi = resp, 
                             forcingi = force.z,
                             photoi = photo.z,
                             chillingi = chill.z,
                             species = latbinum,
                             N = nrow(bb.stan),
                             n_spec = length(unique(bb.stan$complex.wname)),
                             climvar=unique(bb.stan$STV.z)
                        ))
  
  
  bb.3param.gdd <- with(bb.stan, 
                        list(yPhenoi = resp, 
                             forcingi = force.z,
                             photoi = photo.z,
                             chillingi = chill.z,
                             species = latbinum,
                             N = nrow(bb.stan),
                             n_spec = length(unique(bb.stan$complex.wname)),
                             climvar=unique(bb.stan$GDD.z)
                        ))
  
  bb.3param.cp <- with(bb.stan, 
                       list(yPhenoi = resp, 
                            forcingi = force.z,
                            photoi = photo.z,
                            chillingi = chill.z,
                            species = latbinum,
                            N = nrow(bb.stan),
                            n_spec = length(unique(bb.stan$complex.wname)),
                            climvar=unique(bb.stan$CP.z)
                       ))
  
  threeparam_jnt.gdd = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param.gddlf, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
                            iter = 4000, warmup=2500)
  
  threeparam_jnt.stv = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param.stv, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
                            iter = 4000, warmup=2500)
  
  threeparam_jnt.cp = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param.cp, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
                           iter = 4000, warmup=2500)
  
  threeparam_jnt.meangdd = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param.gdd, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
                                iter = 4000, warmup=2500)
  
  #summary(threeparam_jnt.cp)
  #threeparam_jnt.area = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param.area, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
  #                         iter = 4000, warmup=2500)
  
}

if(FALSE){ # Skip extracting original model unless you want to compare the parameters
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
}

###get excited to make some plots with Dan's clunky code below
summy<-summary(threeparam_jnt.cp)$summary

summy[grep("betaTrait", rownames(summy)),]






outy<-rbind(stvout,gddlfout)#,areaout)




write.csv(outy,"betasandmorefromPOPUP.csv",row.names = FALSE)

# On 26 March 2021 the code above runs!

###now plot it

# Who knows about the below ...}

if(FALSE){ #skip area models now
  bb.area.nam <- with(bb.stan.nam, 
                      list(yPhenoi = resp, 
                           forcingi = force.z,
                           photoi = photo.z,
                           chillingi = chill.z,
                           species = latbinum,
                           N = nrow(bb.stan.nam),
                           n_spec = length(unique(bb.stan.nam$complex.wname)),
                           climvar=unique(bb.stan.nam$range.z)
                      ))
  
  
  bb.area.eu <- with(bb.stan.eu, 
                     list(yPhenoi = resp, 
                          forcingi = force.z,
                          photoi = photo.z,
                          chillingi = chill.z,
                          species = latbinum,
                          N = nrow(bb.stan.eu),
                          n_spec = length(unique(bb.stan.eu$complex.wname)),
                          climvar=unique(bb.stan.eu$range.z)
                     ))
  
}

bb.stv.nam <- with(bb.stan.nam, 
                   list(yPhenoi = resp, 
                        forcingi = force.z,
                        photoi = photo.z,
                        chillingi = chill.z,
                        species = latbinum,
                        N = nrow(bb.stan.nam),
                        n_spec = length(unique(bb.stan.nam$complex.wname)),
                        climvar=unique(bb.stan.nam$STV.z)
  
                                         ))
}
##
bb.stv.eu <- with(bb.stan.eu, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.eu),
                       n_spec = length(unique(bb.stan.eu$complex.wname)),
                       climvar=unique(bb.stan.eu$STV.z)
                  ))


bb.gdd.eu <- with(bb.stan.eu, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.eu),
                       n_spec = length(unique(bb.stan.eu$complex.wname)),
                       climvar=unique(bb.stan.eu$GDD.z)
                  ))

bb.cp.eu <- with(bb.stan.eu, 
                 list(yPhenoi = resp, 
                      forcingi = force.z,
                      photoi = photo.z,
                      chillingi = chill.z,
                      species = latbinum,
                      N = nrow(bb.stan.eu),
                      n_spec = length(unique(bb.stan.eu$complex.wname)),
                      climvar=unique(bb.stan.eu$CP.z)
                 ))

bb.gdd.nam <- with(bb.stan.nam, 
                   list(yPhenoi = resp, 
                        forcingi = force.z,
                        photoi = photo.z,
                        chillingi = chill.z,
                        species = latbinum,
                        N = nrow(bb.stan.nam),
                        n_spec = length(unique(bb.stan.nam$complex.wname)),
                        climvar=unique(bb.stan.nam$GDD.z)
                   ))

bb.cp.nam <- with(bb.stan.nam, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.nam),
                       n_spec = length(unique(bb.stan.nam$complex.wname)),
                       climvar=unique(bb.stan.nam$CP.z)
                  ))


