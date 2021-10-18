####ranges plotting based on pop up
###run the rangeleadin_osp.R or load save models on your own machine
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


load("popupmods.Rda")

concordance<-unique(select(bb.stan,complex,complex.wname)) ### for adding ordinal data
###z-score orginal variable for plotting because the model ran on zscored bariables
ggdlf$Temp.SD.z<-(ggdlf$Temp.SD-mean(ggdlf$Temp.SD))/sd(ggdlf$Temp.SD)
ggdlf$STV.z<-(ggdlf$STV-mean(ggdlf$STV))/sd(ggdlf$STV)

#extract paramenter 

######FUNS
scrapeForce<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaForcing",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"force"
  goo$complex<-rep(1:38)
  goo<-left_join(goo,concordance)
  }


scrapeForceslope<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaTraitxF",rowname))
}

scrapeForceintercept<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("muForce",rowname))
}



scrapeChill<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaChill",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"chill"
  goo$complex<-rep(1:38)
  goo<-left_join(goo,concordance)
}


scrapeChillslope<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaTraitxC",rowname))
}

scrapeChillintercept<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("muChill",rowname))
}





scrapePhoto<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaPhoto",rowname))
  goo$cue[grepl("betaPhoto", goo$rowname)]<-"photo"
  goo$complex<-rep(1:38)
  goo<-left_join(goo,concordance)
}


scrapePhotoslope<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaTraitxP",rowname))
}

scrapePhotointercept<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("muPhoto",rowname))
}


#########
#gdd2lf
Forcebeta.gdd<-scrapeForce(threeparam_jnt.gdd)
Forcebeta.gdd<-left_join(Forcebeta.gdd,ggdlf)
Forceslope.gdd<-scrapeForceslope(threeparam_jnt.gdd)
Forceintercept.gdd<-scrapeForceintercept(threeparam_jnt.gdd)


Chillbeta.gdd<-scrapeChill(threeparam_jnt.gdd)
Chillbeta.gdd<-left_join(Chillbeta.gdd,ggdlf)
Chillslope.gdd<-scrapeChillslope(threeparam_jnt.gdd)
Chillintercept.gdd<-scrapeChillintercept(threeparam_jnt.gdd)


Photobeta.gdd<-scrapePhoto(threeparam_jnt.gdd)
Photobeta.gdd<-left_join(Photobeta.gdd,ggdlf)
Photoslope.gdd<-scrapePhotoslope(threeparam_jnt.gdd)
Photointercept.gdd<-scrapePhotointercept(threeparam_jnt.gdd)

#STV
Forcebeta.stv<-scrapeForce(threeparam_jnt.stv)
Forcebeta.stv<-left_join(Forcebeta.stv,ggdlf)
Forceslope.stv<-scrapeForceslope(threeparam_jnt.stv)
Forceintercept.stv<-scrapeForceintercept(threeparam_jnt.stv)


Chillbeta.stv<-scrapeChill(threeparam_jnt.stv)
Chillbeta.stv<-left_join(Chillbeta.stv,ggdlf)
Chillslope.stv<-scrapeChillslope(threeparam_jnt.stv)
Chillintercept.stv<-scrapeChillintercept(threeparam_jnt.stv)


Photobeta.stv<-scrapePhoto(threeparam_jnt.stv)
Photobeta.stv<-left_join(Photobeta.stv,ggdlf)
Photoslope.stv<-scrapePhotoslope(threeparam_jnt.stv)
Photointercept.stv<-scrapePhotointercept(threeparam_jnt.stv)

###
##
force.gdd.plot<-ggplot()+
  geom_point(data=Forcebeta.gdd,aes(Temp.SD.z,mean,shape=continent))+geom_errorbar(data=Forcebeta.gdd,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`))+geom_point()+ggthemes::theme_few()+ylab("Cue sensitivity")+xlab("Variation in Growing degrees to last frost")

a<-ggplot()+
  geom_point(data=Forcebeta.gdd,aes(Temp.SD.z,mean),color="orange1")+
  geom_point(data=Forcebeta.stv,aes(STV.z,mean),color="darkgreen")+
  geom_errorbar(data=Forcebeta.gdd,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="orange1")+
 geom_errorbar(data=Forcebeta.stv,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="darkgreen")+ 
  ggthemes::theme_few()+ylab("Cue sensitivity")+xlab("climate variation")+
  geom_abline(slope = Forceslope.gdd$mean,intercept = Forceintercept.gdd$mean,color="orange1")+
  geom_abline(slope = Forceslope.stv$mean,intercept = Forceintercept.stv$mean,color="darkgreen")+
  ggtitle("Forcing")





b<-ggplot()+
  geom_point(data=Chillbeta.gdd,aes(Temp.SD.z,mean),color="darkgreen")+
  geom_point(data=Chillbeta.stv,aes(STV.z,mean),color="orange1")+
  geom_errorbar(data=Chillbeta.gdd,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="darkgreen")+
  geom_errorbar(data=Chillbeta.stv,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="orange1")+
  ggthemes::theme_few()+ylab("Cue sensitivity")+xlab("climate variation")+
  geom_abline(slope = Chillslope.gdd$mean,intercept = Chillintercept.gdd$mean,color="darkgreen")+
  geom_abline(slope = Chillslope.stv$mean,intercept = Chillintercept.stv$mean,color="orange1")+
  ggtitle("Chilling")


c<-ggplot()+
  geom_point(data=Photobeta.gdd,aes(Temp.SD.z,mean),color="darkgreen")+
  geom_point(data=Photobeta.stv,aes(STV.z,mean),color="orange1")+
  geom_errorbar(data=Photobeta.gdd,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="darkgreen")+
  geom_errorbar(data=Photobeta.stv,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="orange1")+
  ggthemes::theme_few()+ylab("Cue sensitivity")+xlab("climate variation")+
  geom_abline(slope = Photoslope.gdd$mean,intercept = Photointercept.gdd$mean,color="darkgreen")+
  geom_abline(slope = Photoslope.stv$mean,intercept = Photointercept.stv$mean,color="orange1")+
  ggtitle("Photoperiod")


### doit in in base

plot(Forcebeta.gdd$Temp.SD.z,Forcebeta.gdd$mean,col="darkdarkgreen",pch=15)
arrows(x0=x, y0=y-3, x1=x, y1=y+3, code=3, angle=90, length=0.5, col="blue", lwd=2)

ggpubr:::ggarrange(a,b,c,nrow=1,ncol=3,common.legend = TRUE)

unique(bb.stan.eu$latbinum)

####now continet models
#1 adapt functions
scrapeForce.na<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaForcing",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"force"
  goo$complex<-rep(1:17)
  goo<-left_join(goo,concordance)
}


scrapeChill.na<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaChill",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"chill"
  goo$complex<-rep(1:17)
  goo<-left_join(goo,concordance)
}

scrapePhoto.na<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaPhoto",rowname))
  goo$cue[grepl("betaPhoto", goo$rowname)]<-"photo"
  goo$complex<-rep(1:17)
  goo<-left_join(goo,concordance)
}

scrapeForce.eu<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaForcing",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"force"
  goo$complex<-rep(1:21)
  goo<-left_join(goo,concordance)
}


scrapeChill.eu<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaChill",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"chill"
  goo$complex<-rep(1:21)
  goo<-left_join(goo,concordance)
}

scrapePhoto.eu<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("betaPhoto",rowname))
  goo$cue[grepl("betaPhoto", goo$rowname)]<-"photo"
  goo$complex<-rep(1:21)
  goo<-left_join(goo,concordance)
}




###
Forcebeta.gdd.na<-scrapeForce.na(gddlf_jnt.nam)
Forcebeta.gdd.na<-left_join(Forcebeta.gdd.na,ggdlf)
Forceslope.gdd.na<-scrapeForceslope(gddlf_jnt.nam)
Forceintercept.gdd.na<-scrapeForceintercept(gddlf_jnt.nam)


Forcebeta.stv.na<-scrapeForce.na(stv_jnt.nam)
Forcebeta.stv.na<-left_join(Forcebeta.stv.na,ggdlf)
Forceslope.stv.na<-scrapeForceslope(stv_jnt.nam)
Forceintercept.stv.na<-scrapeForceintercept(stv_jnt.nam)


Forcebeta.gdd.eu<-scrapeForce.eu(gddlf_jnt.eu)
Forcebeta.gdd.eu<-left_join(Forcebeta.gdd.eu,ggdlf)
Forceslope.gdd.eu<-scrapeForceslope(gddlf_jnt.eu)
Forceintercept.gdd.eu<-scrapeForceintercept(gddlf_jnt.eu)


Forcebeta.stv.eu<-scrapeForce.eu(stv_jnt.eu)
Forcebeta.stv.eu<-left_join(Forcebeta.stv.eu,ggdlf)
Forceslope.stv.eu<-scrapeForceslope(stv_jnt.eu)
Forceintercept.stv.eu<-scrapeForceintercept(stv_jnt.eu)

###

Chillbeta.gdd.na<-scrapeChill.na(gddlf_jnt.nam)
Chillbeta.gdd.na<-left_join(Chillbeta.gdd.na,ggdlf)
Chillslope.gdd.na<-scrapeChillslope(gddlf_jnt.nam)
Chillintercept.gdd.na<-scrapeChillintercept(gddlf_jnt.nam)


Chillbeta.stv.na<-scrapeChill.na(stv_jnt.nam)
Chillbeta.stv.na<-left_join(Chillbeta.stv.na,ggdlf)
Chillslope.stv.na<-scrapeChillslope(stv_jnt.nam)
Chillintercept.stv.na<-scrapeChillintercept(stv_jnt.nam)


Chillbeta.gdd.eu<-scrapeChill.eu(gddlf_jnt.eu)
Chillbeta.gdd.eu<-left_join(Chillbeta.gdd.eu,ggdlf)
Chillslope.gdd.eu<-scrapeChillslope(gddlf_jnt.eu)
Chillintercept.gdd.eu<-scrapeChillintercept(gddlf_jnt.eu)


Chillbeta.stv.eu<-scrapeChill.eu(stv_jnt.eu)
Chillbeta.stv.eu<-left_join(Chillbeta.stv.eu,ggdlf)
Chillslope.stv.eu<-scrapeChillslope(stv_jnt.eu)
Chillintercept.stv.eu<-scrapeChillintercept(stv_jnt.eu)


#
Photobeta.gdd.na<-scrapePhoto.na(gddlf_jnt.nam)
Photobeta.gdd.na<-left_join(Photobeta.gdd.na,ggdlf)
Photoslope.gdd.na<-scrapePhotoslope(gddlf_jnt.nam)
Photointercept.gdd.na<-scrapePhotointercept(gddlf_jnt.nam)


Photobeta.stv.na<-scrapePhoto.na(stv_jnt.nam)
Photobeta.stv.na<-left_join(Photobeta.stv.na,ggdlf)
Photoslope.stv.na<-scrapePhotoslope(stv_jnt.nam)
Photointercept.stv.na<-scrapePhotointercept(stv_jnt.nam)


Photobeta.gdd.eu<-scrapePhoto.eu(gddlf_jnt.eu)
Photobeta.gdd.eu<-left_join(Photobeta.gdd.eu,ggdlf)
Photoslope.gdd.eu<-scrapePhotoslope(gddlf_jnt.eu)
Photointercept.gdd.eu<-scrapePhotointercept(gddlf_jnt.eu)


Photobeta.stv.eu<-scrapePhoto.eu(stv_jnt.eu)
Photobeta.stv.eu<-left_join(Photobeta.stv.eu,ggdlf)
Photoslope.stv.eu<-scrapePhotoslope(stv_jnt.eu)
Photointercept.stv.eu<-scrapePhotointercept(stv_jnt.eu)


aa<-ggplot()+
  geom_point(data=Forcebeta.gdd.na,aes(Temp.SD.z,mean),color="orange1",shape=15)+
  geom_point(data=Forcebeta.stv.na,aes(STV.z,mean),color="darkgreen",shape=15)+
  geom_point(data=Forcebeta.gdd.eu,aes(Temp.SD.z,mean),color="orange1",shape=17)+
  geom_point(data=Forcebeta.stv.eu,aes(STV.z,mean),color="darkgreen",shape=17)+
  geom_errorbar(data=Forcebeta.gdd.na,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="orange1")+
  geom_errorbar(data=Forcebeta.stv.na,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="darkgreen")+ 
  geom_errorbar(data=Forcebeta.gdd.eu,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="orange1")+
  geom_errorbar(data=Forcebeta.stv.eu,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="darkgreen") +
ggthemes::theme_few()+ylab("Cue sensitivity")+xlab("climate variation")+
  geom_abline(slope = Forceslope.gdd.na$mean,intercept = Forceintercept.gdd.na$mean,color="orange1",linetype="solid")+
  geom_abline(slope = Forceslope.stv.na$mean,intercept = Forceintercept.stv.na$mean,color="darkgreen",linetype="solid")+
  geom_abline(slope = Forceslope.gdd.eu$mean,intercept = Forceintercept.gdd.eu$mean,color="orange1",linetype="dashed")+
  geom_abline(slope = Forceslope.stv.eu$mean,intercept = Forceintercept.stv.eu$mean,color="darkgreen",linetype="dashed")+
  ggtitle("Forcing")


bb<-ggplot()+
  geom_point(data=Chillbeta.gdd.na,aes(Temp.SD.z,mean),color="orange1",shape=15)+
  geom_point(data=Chillbeta.stv.na,aes(STV.z,mean),color="darkgreen",shape=15)+
  geom_point(data=Chillbeta.gdd.eu,aes(Temp.SD.z,mean),color="orange1",shape=17)+
  geom_point(data=Chillbeta.stv.eu,aes(STV.z,mean),color="darkgreen",shape=17)+
  geom_errorbar(data=Chillbeta.gdd.na,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="orange1")+
  geom_errorbar(data=Chillbeta.stv.na,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="darkgreen")+ 
  geom_errorbar(data=Chillbeta.gdd.eu,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="orange1")+
  geom_errorbar(data=Chillbeta.stv.eu,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="darkgreen") +
  ggthemes::theme_few()+ylab("Cue sensitivity")+xlab("climate variation")+
  geom_abline(slope = Chillslope.gdd.na$mean,intercept = Chillintercept.gdd.na$mean,color="orange1",linetype="solid")+
  geom_abline(slope = Chillslope.stv.na$mean,intercept = Chillintercept.stv.na$mean,color="darkgreen",linetype="solid")+
  geom_abline(slope = Chillslope.gdd.eu$mean,intercept = Chillintercept.gdd.eu$mean,color="orange1",linetype="dashed")+
  geom_abline(slope = Chillslope.stv.eu$mean,intercept = Chillintercept.stv.eu$mean,color="darkgreen",linetype="dashed")+
  ggtitle("Chilling")


cc<-ggplot()+
  geom_point(data=Photobeta.gdd.na,aes(Temp.SD.z,mean),color="orange1",shape=15)+
  geom_point(data=Photobeta.stv.na,aes(STV.z,mean),color="darkgreen",shape=15)+
  geom_point(data=Photobeta.gdd.eu,aes(Temp.SD.z,mean),color="orange1",shape=17)+
  geom_point(data=Photobeta.stv.eu,aes(STV.z,mean),color="darkgreen",shape=17)+
  geom_errorbar(data=Photobeta.gdd.na,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="orange1")+
  geom_errorbar(data=Photobeta.stv.na,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="darkgreen")+ 
  geom_errorbar(data=Photobeta.gdd.eu,aes(x=Temp.SD.z,ymin=`25%`,ymax=`75%`),color="orange1")+
  geom_errorbar(data=Photobeta.stv.eu,aes(x=STV.z,ymin=`25%`,ymax=`75%`),color="darkgreen") +
  ggthemes::theme_few()+ylab("Cue sensitivity")+xlab("climate variation")+
  geom_abline(slope = Photoslope.gdd.na$mean,intercept = Photointercept.gdd.na$mean,color="orange1",linetype="solid")+
  geom_abline(slope = Photoslope.stv.na$mean,intercept = Photointercept.stv.na$mean,color="darkgreen",linetype="solid")+
  geom_abline(slope = Photoslope.gdd.eu$mean,intercept = Photointercept.gdd.eu$mean,color="orange1",linetype="dashed")+
  geom_abline(slope = Photoslope.stv.eu$mean,intercept = Photointercept.stv.eu$mean,color="darkgreen",linetype="dashed")+
  ggtitle("Photoperiod")


jpeg("./figures/mock1.jpeg",width = 10,height=8, units = "in",res = 300)
ggpubr::ggarrange(a,aa,b,bb,c,cc,nrow=3,ncol=2,labels = c("Full data","Continent subsets"),label.x = c(.2,.2))
dev.off()
