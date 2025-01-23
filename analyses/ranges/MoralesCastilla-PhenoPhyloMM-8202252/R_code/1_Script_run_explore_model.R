#'#############################################################
#' Phylogenetic estimates of species-level phenology improve ecological forecasting 
#' 
#' * Script #1 Phylogenetic Mixed Model fitting
#'
#'  
#'  by Morales-Castilla, I., et al. 
#'  feb 2024
#'  
#'  Adapted by Dan B, Jan 2025
#'#############################################################


## Runs (or reads) the phylogeny models, extracts some output
## Does some basic plotting

rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()
# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/ospree/analyses/ranges//MoralesCastilla-PhenoPhyloMM-8202252/")


# Loading packages
library(caper)
library(pez)
library(phytools)
library(rstan)
library(shinystan)
library(plyr)
library(dplyr)

options(mc.cores = parallel::detectCores())



#'######################################
#### load data and phylogeny ####
#'######################################


  d = read.csv("data/ospreebbphyloms_forknb.csv")
  phylo = read.tree("data/phyloforphyloms.tre")
  

  nspecies = length(phylo$tip.label)

  
  
  
  library(sp)
  library(rworldmap)
  
  # The single argument to this function, points, is a data.frame in which:
  #   - column 1 contains the longitude in degrees
  #   - column 2 contains the latitude in degrees
  
  coords2continent = function(points)
  {  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    #indices$continent   # returns the continent (6 continent model)
    indices$REGION   # returns the continent (7 continent model)
    #indices$ADMIN  #returns country name
    #indices$ISO3 # returns the ISO3 code 
  }  
  
d2<-dplyr::filter(d,!is.na(provenance.lat))

points<-data.frame(lon=d2$provenance.long,lat=d2$provenance.lat)  
points<-points[complete.cases(points),]
d2$continent<-coords2continent(points)

goo<-dplyr::select(d2,latbi,continent)

goo$continent[which(goo$latbi %in% c("Alnus_incana","Aronia_melanocarpa","Betula_lenta","Quercus_rubra","Robinia_pseudoacacia"))] <- "North America"
goo$continent[which(goo$latbi %in% c("Rhamnus_cathartica"))] <- "Europe"
goo<-distinct(goo)

colnames(goo)[2]<-"biogeography"


 
key<-dplyr::select(d2,latbi,sppnum)
key<-dplyr::distinct(key) 
setdiff(goo$latbi,key$latbi)

key<-left_join(key,goo)

#'###################################
# Run  the models      ####
#'###################################

## Fit model here and re run for longer
  fitlambest <- stan("stan_code/PhenoPhyloMM_PMM.stan",
              data=list(N=nrow(d),
                        n_sp=nspecies,
                        sp=d$sppnum,
                        x1=d$force.z,
                        x2 = d$chill.z,
                        x3=d$photo.z,
                        y=d$resp,
                        Vphy=vcv(phylo, corr = TRUE)),
              iter = 7000,
              warmup = 5000, # half the iter as warmp is default, but leaving in case we want to change
              control=list(adapt_delta=.99),
              chains = 4,
              seed = 1986 
  )
  
  ## Save fitted posterior
#  saveRDS(fitlambest, "fit_model_PMM.rds")


  
  library(reshape2)
  sample <- rstan::extract(fitlambest)   
 
   sample.force <- melt(sample$b_force)
  sample.chill <- melt(sample$b_chill)
  sample.photo <- melt(sample$b_photo)
  
  names(sample.force) <- c("iter", "sppnum", "cue_estimate")
  names(sample.chill) <- c("iter", "sppnum", "cue_estimate")
  names(sample.photo) <- c("iter", "sppnum", "cue_estimate")
  
sample.force$cue<-"forcing"
sample.chill$cue<-"chilling"
sample.photo$cue<-"photoperiod"
  posto<-rbind(sample.force,sample.photo,sample.chill)

sort(table(key$latbi))      
posto<-merge(posto,key,by="sppnum")  


library(ggplot2)
library(bayesplot)
library(tidybayes)
pd<-position_dodge(width = .2)
p1<-ggplot(posto,aes(cue_estimate,biogeography))+stat_interval(.width=c(.5,.75,.95))+
  stat_pointinterval(aes(x = cue_estimate), .width = c(0),size=12)+
  facet_wrap(~cue,scales = "free_x")+geom_vline(xintercept = 0,color="firebrick4",linetype="dashed")+
  ggthemes::theme_few()+scale_color_brewer()+xlab("estimated effect")+ylab("") 

jpeg("..//figures/NAvEuPMM.jpeg",width=6, height=1.25,unit='in',res=200)
p1
  dev.off()

setwd("~/Documents/git/ospree/analyses/ranges") 
rangiesEu<-read.csv("output/Synthesis_climate_EUsps_STVfinalchill.csv") ### updated STV
rangiesEu<-dplyr::filter(rangiesEu,species!="Alnus_incana")
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
colnames(ggdlf)[5]<-"complex.wname"

STV<-filter(rangies,variable=="MeanTmins")
STV<-dplyr::select(STV,species,Temp.SD)
colnames(STV)[c(1,2)]<-c("complex.wname","STV")
ggdlf<-dplyr::left_join(ggdlf,STV)


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


posto$complex.wname<-posto$latbi


postoW<-merge(ggdlf,posto)




library("brms")





if(FALSE){
p3<-ggplot(postoW,aes(Temp.SD,cue_estimate))+stat_pointinterval()+facet_grid(cue~biogeography)+geom_smooth(method="lm")+ggthemes::theme_few()

ggplot(postoW,aes(Temp.SD,cue_estimate))+stat_pointinterval()+facet_wrap(~cue)+geom_smooth(method="lm")+ggthemes::theme_few()

ggplot(postoW,aes(STV,cue_estimate))+stat_pointinterval()+facet_grid(cue~biogeography)+geom_smooth(method="lm")+ggthemes::theme_few()

ggplot(postoW,aes(STV,cue_estimate))+stat_pointinterval()+facet_wrap(~cue)+geom_smooth(method="lm")+ggthemes::theme_few()
}

ggplot(postoW,aes(cue_estimate,biogeography))+stat_interval(.width=c(.5,.75,.95))+
  stat_pointinterval(aes(x = cue_estimate), .width = c(0),size=12)+
  facet_wrap(~cue,scales = "free_x")+geom_vline(xintercept = 0,color="firebrick4",linetype="dashed")+
  ggthemes::theme_few()+scale_color_brewer()+xlab("estimated effect")+ylab("") 


sumz<-postoW %>% dplyr::group_by(complex.wname,cue,biogeography)%>% dplyr::summarise(mean_cue=mean(cue_estimate),sd=sd(cue_estimate))
sumz<-merge(sumz,ggdlf)
#write.csv(sumz,"mean_cues.csv")

chilldat<-filter(sumz, cue=="chilling")
forcedat<-filter(sumz, cue=="forcing")
photodat<-filter(sumz, cue=="photoperiod")

chill.na<-filter(sumz,biogeography=="North America" & cue=="chilling")
chill.eu<-filter(sumz,biogeography!="North America" & cue=="chilling")

force.na<-filter(sumz,biogeography=="North America" & cue=="forcing")
force.eu<-filter(sumz,biogeography!="North America" & cue=="forcing")

###measurement variable 
modChill<-brm(mean_cue|se(sd,sigma=TRUE)~Temp.SD,data=chilldat)
modForce<-brm(mean_cue|se(sd,sigma=TRUE)~Temp.SD,data=forcedat)
modPhoto<-brm(mean_cue|se(sd,sigma=TRUE)~Temp.SD,data=photodat)

modChillstv<-brm(mean_cue|se(sd,sigma=TRUE)~STV,data=chilldat)

newdater<-data.frame(Temp.SD=0:60,sd=rep(0,61))
ggdlfpred<-epred_draws(modChill,newdata = newdater,ndraws = 1000)
ggdlfpred$cue<-"chilling"
ggdlfpred2<-epred_draws(modForce,newdata = newdater,ndraws = 1000)
ggdlfpred2$cue<-"forcing"

ggdlfpre3<-epred_draws(modPhoto,newdata = newdater,ndraws = 1000)
ggdlfpre3$cue<-"photoperiod"


p2<-ggplot()+geom_line(data=ggdlfpred,aes(Temp.SD,.epred,group=.draw),size=.01)+
    geom_line(data=ggdlfpred2,aes(Temp.SD,.epred,group=.draw),size=.01)+
    geom_line(data=ggdlfpre3,aes(Temp.SD,.epred,group=.draw),size=.01)+
    stat_pointinterval(data=postoW,aes(Temp.SD,cue_estimate))+facet_wrap(~cue)+
    ggthemes::theme_few()+ylab("cue sensitivity")+xlab("Var(GDDs to last frost)")

jpeg("..//figures/NAvEuPMM.jpeg",width=8, height=8,unit='in',res=200)
ggpubr::ggarrange(p1,p2,p3,ncol=1,heights=c(1,1,2),labels = c("a)","b)","c)"))
dev.off()

modForce<-brm(mean_cue|se(sd,sigma=TRUE)~Temp.SD,data=forcedat)
modForcestv<-brm(mean_cue|se(sd,sigma=TRUE)~STV,data=forcedat)

modNAChill<-brm(mean_cue|se(sd,sigma=TRUE)~Temp.SD,data=chill.na)
modEuChill<-brm(mean_cue|se(sd,sigma=TRUE)~Temp.SD,data=chill.eu)
bayes_R2(modEuChill)
bayes_R2(modNAChill)

modNAChill2<-brm(mean_cue|se(sd,sigma=TRUE)~Geo.SD,data=chill.na)
modEuChill2<-brm(mean_cue|se(sd,sigma=TRUE)~Geo.SD,data=chill.eu)

modNAChillstv<-brm(mean_cue|se(sd,sigma=TRUE)~STV,data=chill.na)
modEuChillstv<-brm(mean_cue|se(sd,sigma=TRUE)~STV,data=chill.eu)


fixef(modChill,probs = c(.05,.25,.75,.95))
fixef(modChillstv,probs = c(.05,.25,.75,.95))

fixef(modForce,probs = c(.05,.25,.75,.95))
fixef(modForcestv,probs = c(.05,.25,.75,.95))

fixef(modNAChill2,probs = c(.05,.25,.75,.95))
fixef(modEuChill2,probs = c(.05,.25,.75,.95))

fixef(modNAChill,probs = c(.05,.25,.75,.95))
fixef(modEuChill,probs = c(.05,.25,.75,.95))

fixef(modNAChillstv,probs = c(.05,.25,.75,.95))
fixef(modEuChillstv,probs = c(.05,.25,.75,.95))

summary(lm(mean_cue~Temp.SD,data=chill.eu))
summary(lm(mean_cue~STV,data=chill.eu))

ggplot(ggdlf,aes(Temp.SD))+geom_histogram(aes(fill=continent),bins = 100)
ggplot(ggdlf,aes(Temp.SD))+geom_histogram(aes(fill=continent),bins = 100)


save.image("PMM4rangers.Rda") 


fitlamb0 <- stan("stan_code/PhenoPhyloMM_HMM.stan",
                   data=list(N=nrow(d),
                             n_sp=nspecies,
                             sp=d$sppnum,
                             x1=d$force.z,
                             x2 = d$chill.z,
                             x3=d$photo.z,
                             y=d$resp,
                             Vphy=vcv(phylo, corr = TRUE)),
                   iter = 4000,
                   warmup = 2000, 
                   chains = 4,
                   seed = 117 
  )
  saveRDS(fitlamb0, "fit_model_HMM.rds")
  

  sample <- rstan::extract(fitlamb0)   
  
  sample.force <- melt(sample$b_force)
  sample.chill <- melt(sample$b_chill)
  sample.photo <- melt(sample$b_photo)
  
  names(sample.force) <- c("iter", "sppnum", "cue_estimate")
  names(sample.chill) <- c("iter", "sppnum", "cue_estimate")
  names(sample.photo) <- c("iter", "sppnum", "cue_estimate")
  
  sample.force$cue<-"forcing"
  sample.chill$cue<-"chilling"
  sample.photo$cue<-"photoperiod"
  posto0<-rbind(sample.force,sample.photo,sample.chill)  
  posto0<-merge(posto0,key,by="sppnum")  
  
  ggplot(posto0,aes(cue,cue_estimate))+stat_eye(aes(fill=biogeography),position=pd)+coord_cartesian(ylim=c(-25,15))  
  
    

#'###################################
# Explore model fit            ####
#'###################################

## Summarize full fit
# summary(fit)$summary

## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
fitsum <- summary(fitlambest, pars = list("a_z", "sigma_interceptsa", 
                                   "b_zf", "sigma_interceptsbf", "lam_interceptsbf", 
                                   "b_zc", "sigma_interceptsbc", "lam_interceptsbc",
                                   "b_zp", "sigma_interceptsbp", "lam_interceptsbp","sigma_y"))$summary

fitsumdf <- as.data.frame(fitsum)

source("source/stan_utility.R")
check_all_diagnostics(fitlambest)





#'###############################################
#### comparing estimates lambda est vs 1 vs 0 ####
#'###############################################


## load models



## Summarize lambdas, b_zf, b_zc, , b_zp, intercept mean, and sigmas
tableresults.0 = summary(fitlam0, pars = list("a_z", "sigma_interceptsa", "b_zf", "sigma_interceptsbf", "b_zc", "sigma_interceptsbc", "b_zp", "sigma_interceptsbp", "sigma_y"))$summary
tableresults.est = summary(fitlambest, pars = list("a_z", "lam_interceptsa", "sigma_interceptsa", "b_zf", "lam_interceptsbf", "sigma_interceptsbf", "b_zc", "lam_interceptsbc", "sigma_interceptsbc", "b_zp", "lam_interceptsbp", "sigma_interceptsbp", "sigma_y"))$summary



## rename model to include species names
names(fitlambest)[grep(pattern = "^a\\[", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_force", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_chill", x = names(fitlambest))] <- phylo$tip.label
names(fitlambest)[grep(pattern = "^b_photo", x = names(fitlambest))] <- phylo$tip.label

names(fitlam0)[grep(pattern = "^a\\[", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_force", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_chill", x = names(fitlam0))] <- phylo$tip.label
names(fitlam0)[grep(pattern = "^b_photo", x = names(fitlam0))] <- phylo$tip.label




# get model estimates per species ----

## where species are

posspsindata.est <- list(10:200,202:392,394:584)
posspsindata.01 <- list(6:196,198:388,390:580)


## forcing
cueforce = summary(fitlambest)$summary[posspsindata.est[[1]],"mean"]
cueforcesdup = summary(fitlambest)$summary[posspsindata.est[[1]],"75%"]
cueforcesdlow = summary(fitlambest)$summary[posspsindata.est[[1]],"25%"]



cueforce0 = summary(fitlam0)$summary[posspsindata.01[[1]],"mean"]
cueforcesdup0 = summary(fitlam0)$summary[posspsindata.01[[1]],"75%"]
cueforcesdlow0 = summary(fitlam0)$summary[posspsindata.01[[1]],"25%"]


## chill
cuechill = summary(fitlambest)$summary[posspsindata.est[[2]],"mean"]
cuechillsdup = summary(fitlambest)$summary[posspsindata.est[[2]],"75%"]
cuechillsdlow = summary(fitlambest)$summary[posspsindata.est[[2]],"25%"]

cuechill0 = summary(fitlam0)$summary[posspsindata.01[[2]],"mean"]
cuechillsdup0 = summary(fitlam0)$summary[posspsindata.01[[2]],"75%"]
cuechillsdlow0 = summary(fitlam0)$summary[posspsindata.01[[2]],"25%"]


## photo
cuephoto = summary(fitlambest)$summary[posspsindata.est[[3]],"mean"]
cuephotosdup = summary(fitlambest)$summary[posspsindata.est[[3]],"75%"]
cuephotosdlow = summary(fitlambest)$summary[posspsindata.est[[3]],"25%"]

cuephoto0 = summary(fitlam0)$summary[posspsindata.01[[3]],"mean"]
cuephotosdup0 = summary(fitlam0)$summary[posspsindata.01[[3]],"75%"]
cuephotosdlow0 = summary(fitlam0)$summary[posspsindata.01[[3]],"25%"]





### plot correlations angio ----
plotting = F
lambdazero = F

if(plotting){
  
  dev.off()
  par(mfrow=c(1,3))
  
  virid <-  colorRampPalette(c("yellow","darkcyan","purple"))
  
  colschill <- virid(30)[as.numeric(cut(c(cuechill0, cuechill),breaks = 30))]
  colschillhmm <- colschill[1:length(cuechill0)]
  colschillpmm <- colschill[(length(cuechill0)+1):length(colschill)]
  
  
  plot(cuechill0, cuechill, 
       xlab="sensitivity to chilling HMM",
       ylab="sensitivity to chilling PMM", 
       pch=16, col=adjustcolor(colschillpmm,0.4),cex=1.2, cex.lab=1.5,
       xlim=c(-30,5),ylim=c(-30,5))
  abline(v=mean(cuechill0), col='grey', lty=2, lwd=2)  
  
  for(i in 1:length(cueforce0)){
    lines(c(cuechillsdlow0[i],cuechillsdup0[i]),
          rep(cuechill[i],2), col=adjustcolor(colschillpmm[i],0.2))
    lines(rep(cuechill0[i],2),
          c(cuechillsdlow[i],cuechillsdup[i]),
          col=adjustcolor(colschillhmm[i],0.2))
  }
  points(cuechill0, cuechill,pch=16, col=adjustcolor(colschillpmm,0.4),cex=1.2)
  
  abline(a=0,b=1, col='darkgrey', lty=2, lwd=1.5)  
  #abline(lm(cuechill~cuechill0), lwd=1.5)
  mtext("a", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  
  colsforce <- virid(30)[as.numeric(cut(c(cueforce0, cueforce),breaks = 30))]
  colsforcehmm <- colsforce[1:length(cueforce0)]
  colsforcepmm <- colsforce[(length(cueforce0)+1):length(colsforce)]
  
  plot(cueforce0, cueforce, 
       xlab="sensitivity to forcing HMM",
       ylab="sensitivity to forcing PMM", 
       pch=16, col=adjustcolor(colsforcepmm,0.4),cex=1.2, cex.lab=1.5,
       xlim=c(-20,5),ylim=c(-20,5))
  abline(v=mean(cueforce0), col='grey', lty=2, lwd=2)  
  
  for(i in 1:length(cueforce0)){
    lines(c(cueforcesdlow0[i],cueforcesdup0[i]),
          rep(cueforce[i],2), col=adjustcolor(colsforcepmm[i],0.2))
    lines(rep(cueforce0[i],2),
          c(cueforcesdlow[i],cueforcesdup[i]),
          col=adjustcolor(colsforcehmm[i],0.2))
    
  }
  points(cueforce0, cueforce,pch=16, col=adjustcolor(colsforcepmm,0.4),cex=1.2)
  
  abline(a=0,b=1, col='darkgrey', lty=2, lwd=1.5)  
  #abline(lm(cueforce~cueforce0), lwd=1.5)
  mtext("b", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  colsphoto <- virid(30)[as.numeric(cut(c(cuephoto0, cuephoto),breaks = 30))]
  colsphotohmm <- colsphoto[1:length(cuephoto0)]
  colsphotopmm <- colsphoto[(length(cuephoto0)+1):length(colsphoto)]
  
  plot(cuephoto0, cuephoto, 
       xlab="sensitivity to photoperiod HMM",
       ylab="sensitivity to photoperiod PMM", 
       pch=16, col=adjustcolor(colsphotohmm,0.4),cex=1.2, cex.lab=1.5,
       xlim=c(-10,3),ylim=c(-10,3))
  abline(v=mean(cuephoto0), col='grey', lty=2, lwd=2)  
  
  for(i in 1:length(cuephoto0)){
    lines(c(cuephotosdlow0[i],cuephotosdup0[i]),
          rep(cuephoto[i],2), col=adjustcolor(colsphotohmm[i],0.2))
    
    lines(rep(cuephoto0[i],2),
          c(cuephotosdlow[i],cuephotosdup[i]),
          col=adjustcolor(colsphotohmm[i],0.2))
  }
  points(cuephoto0, cuephoto,pch=16, col=adjustcolor(colsphotopmm,0.4),cex=1.2)
  
  abline(a=0,b=1, col='darkgrey', lty=2, lwd=1.5)  
  #abline(lm(cuephoto~cuephoto0), lwd=1.5)
  mtext("c", side = 3, adj = 0.05,line=-2,cex=1.5)
  
  
}



# end ----
