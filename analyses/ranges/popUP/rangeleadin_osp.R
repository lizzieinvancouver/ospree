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

## remove duplicat4e for alnus incana only if you are running everything together
ggdlf<-dplyr::filter(ggdlf,(complex.wname!="Alnus_incana") | (continent!="Europe"))

#bb.stan<-filter(bb.stan,complex.wname!="Ulmus_minor")

goodsp<-intersect(unique(bb.stan$complex.wname),ggdlf$complex.wname)

intersect(ggdlf$complex.wname,unique(bb.stan$complex.wname))
bb.stan<-filter(bb.stan,complex.wname %in% goodsp)
ggdlf<-filter(ggdlf,complex.wname %in% goodsp)


bb.stan<-left_join(bb.stan,ggdlf)

##Range size
area<-read.csv("output/rangeareas.csv")
area$continent[area$continent=="europe"]<-"Europe"
area$continent[area$continent=="north america"]<-"N. America"
colnames(area)[2]<-"complex.wname"


bb.stan<-left_join(bb.stan,area)

bb.stan$Temp.SD.cent<-bb.stan$Temp.SD-mean(bb.stan$Temp.SD)
bb.stan$STV.cent<-bb.stan$STV-mean(bb.stan$STV)
bb.stan$range.cent<-bb.stan$range_area-mean(bb.stan$range_area)
bb.stan$Temp.SD.z<-(bb.stan$Temp.SD-mean(bb.stan$Temp.SD))/sd(bb.stan$Temp.SD)
bb.stan$STV.z<-(bb.stan$STV-mean(bb.stan$STV))/sd(bb.stan$STV)
bb.stan$range.z<-(bb.stan$range_area-mean(bb.stan$range_area))/sd(bb.stan$range_area)


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

if (FALSE){ #skip area model
bb.3param.area <- with(bb.stan, 
                        list(yPhenoi = resp, 
                             forcingi = force.z,
                             photoi = photo.z,
                             chillingi = chill.z,
                             species = latbinum,
                             N = nrow(bb.stan),
                             n_spec = length(unique(bb.stan$complex.wname)),
                             climvar=unique(bb.stan$range.z)
                        ))

}

threeparam_jnt.gdd = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param.gddlf, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
                 iter = 4000, warmup=2500)

threeparam_jnt.stv = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param.stv, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
                          iter = 4000, warmup=2500)

#threeparam_jnt.area = stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.3param.area, # this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu
 #                         iter = 4000, warmup=2500)



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
summy<-summary(threeparam_jnt.gdd)$summary

summy[grep("betaTrait", rownames(summy)),]



#extract paramenter 
scrape<-function(x){ # this one takes the species level beta values
goo <- summary(x)$summary
goo<-as.data.frame(goo)
goo<-tibble::rownames_to_column(goo, var = "rowname")
goo<-dplyr::filter(goo,grepl("betaForcing|betaPhoto|betaChill",rowname))

}

scrape2<-function(x){ # this one take the grand mu for each cue, (ie what the beta would be if clim var had no effect)
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("mu",rowname))
  
}


scrape3<-function(x){ ## this one takes the climvar slope estimate for each cue
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaTrait",rowname))
  
}



gddlfout<-scrape(threeparam_jnt.gdd) ### data for betas

gddlfcept<-scrape2(threeparam_jnt.gdd) #datafor intercept

##make them sortable
gddlfcept<-filter(gddlfcept,rowname!="muPhenoSp")
gddlfcept$cue[grepl("muForce", gddlfcept$rowname)]<-"force"
gddlfcept$cue[grepl("muPhoto", gddlfcept$rowname)]<-"photo"
gddlfcept$cue[grepl("muChill", gddlfcept$rowname)]<-"chill"


gddlfslope<-scrape3(threeparam_jnt.gdd) ## slope estimate 
gddlfslope$cue[grepl("betaTraitxForcing", gddlfslope$rowname)]<-"force"
gddlfslope$cue[grepl("betaTraitxPhoto", gddlfslope$rowname)]<-"photo"
gddlfslope$cue[grepl("betaTraitxChill", gddlfslope$rowname)]<-"chill"


gddlfcept<-select(gddlfcept,2,6,8,12) ## filter to columns of use
colnames(gddlfcept)[1:3]<-c("intercept","bottom","top") # name them in a way that works better with plotting

gdd2lfslope<-left_join(gddlfslope,gddlfcept) #mash em up so you have a slope and intercept for each trendline


 # areaout$climparam<-"area"

gddlfout$complex<-rep(1:38,3)### this is an index that will help later on

## same as above line 282

gddlfout$cue[grepl("betaForcing", gddlfout$rowname)]<-"force"
gddlfout$cue[grepl("betaPhoto", gddlfout$rowname)]<-"photo"
gddlfout$cue[grepl("betaChill", gddlfout$rowname)]<-"chill"

concordance<-unique(select(bb.stan,complex,complex.wname)) ## this translate our numeric species to their names

gddlfout<-left_join(gddlfout,concordance) ## merige


gddlfout<-left_join(gddlfout,ggdlf)## merge


colnames(gddlfout)[6:8]<-c("twofive","fiveoh","sevenfive")# again column names with % in them dont work so change them

gddlfout$Temp.SD.z<-(gddlfout$Temp.SD-mean(gddlfout$Temp.SD))/sd(gddlfout$Temp.SD) ## zscore the clim var for plotting cause that what the model used
range(bb.stan$Temp.SD.z)

colnames(gdd2lfslope)[6:8]<-c("twofive","fiveoh","sevenfive")# again column names with % in them dont work so change them
jpeg("popUP/popUpmods_try1.jpg")
ggplot()+
  geom_point(data=gddlfout,aes(Temp.SD.z,mean,shape=continent,color=cue))+geom_errorbar(data=gddlfout,aes(x=Temp.SD.z,ymin=twofive, ymax=sevenfive,color=cue))+
  geom_segment(data=gdd2lfslope,aes(x = -0.5881806,xend=2.606941,y=intercept,yend=intercept+(mean*2.606941)),size=1.5)+
facet_wrap(~cue)+ggthemes::theme_few()+ylab("Cue sensitivity")+xlab("Variation in Growing degrees to last frost")+scale_color_viridis_d()

dev.off()

#why do the trend line seem counter to the data? I think its the pooling thouch could also be a clerical error but I've check a few times
stop()
##are is geom_smooth the same as our model
gddlfout %>% 
  group_by(cue) %>% 
  do({
    mod = lm(mean ~ Temp.SD, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })

outy<-rbind(stvout,gddlfout)#,areaout)




write.csv(outy,"betasandmorefromPOPUP.csv",row.names = FALSE)

# On 26 March 2021 the code above runs!

###now plot it

# Who knows about the below ...
}


##
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
                       climvar=unique(bb.stan.nam$Temp.SD.z)
                  ))

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

bb.gddlf.eu <- with(bb.stan.eu, 
                      list(yPhenoi = resp, 
                           forcingi = force.z,
                           photoi = photo.z,
                           chillingi = chill.z,
                           species = latbinum,
                           N = nrow(bb.stan.eu),
                           n_spec = length(unique(bb.stan.eu$complex.wname)),
                           climvar=unique(bb.stan.eu$Temp.SD.z)
                           
                      ))


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



###models
gddlf_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.gddlf.eu,
                          iter = 5000, warmup=4000) #

check_all_diagnostics(gddlf_jnt.eu)
stv_jnt.eu= stan('popUP/stan/joint_climvar_3param_osp.stan', data = bb.stv.eu,
                   iter = 5000, warmup=4000) #runs
check_all_diagnostics(stv_jnt.eu)

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



check_all_diagnostics(gddlf_jnt.nam)
summary(gddlf_jnt.nam)
launch_shinystan(gddlf_jnt.nam)


stv_jnt.nam = stan('popUP/stan/joint_climvar_3param_osp_ncpPhotoForce.stan', data =bb.stv.nam,
                     iter = 4000, warmup=3000) ## Dan ran this too.

check_all_diagnostics(stv_jnt.nam)
#area_jnt.nam = stan('popUP/stan/joint_climvar_3param_osp.stan', data =bb.area.nam,
 #                  iter = 6000, warmup=5000,control = list(adapt_delta=0.99))

#check_all_diagnostics(area_jnt.nam)

save.image("popupmods.Rda")




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
