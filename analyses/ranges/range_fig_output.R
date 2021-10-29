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

concordance<-unique(dplyr::select(bb.stan,complex,complex.wname)) ### for adding ordinal data
###z-score orginal variable for plotting because the model ran on zscored bariables
ggdlf$Temp.SD.z<-(ggdlf$Temp.SD-mean(ggdlf$Temp.SD))/sd(ggdlf$Temp.SD)
ggdlf$STV.z<-(ggdlf$STV-mean(ggdlf$STV))/sd(ggdlf$STV)

#extract paramenter 
summary((threeparam_jnt.gdd))$summary
######FUNS
scrapebetas<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("beta",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"force"
  goo$cue[grepl("betaChill", goo$rowname)]<-"chill"
  goo$cue[grepl("betaPhoto", goo$rowname)]<-"photo"
  goo$complex<-c(99,99,99,rep(1:38,3))
  goo<-left_join(goo,concordance)
  }



scrapeslopes<-function(x){ 
sample <- rstan::extract(x)### takes a while

sample.force <- melt(sample$betaTraitxForcing)
sample.force2 <- melt(sample$muForceSp)

colnames(sample.force)[2]<-"trait_beta"
colnames(sample.force2)[2]<-"mu"
forcyb<-left_join(sample.force,sample.force2)
forcyb$cue<-"force"

sample.chill <- melt(sample$betaTraitxChill)
sample.chill2 <- melt(sample$muChillSp)

colnames(sample.chill)[2]<-"trait_beta"
colnames(sample.chill2)[2]<-"mu"
chillyb<-left_join(sample.chill,sample.chill2)
chillyb$cue<-"chill"



sample.photo <- melt(sample$betaTraitxPhoto)
sample.photo2 <- melt(sample$muPhotoSp)

colnames(sample.photo)[2]<-"trait_beta"
colnames(sample.photo2)[2]<-"mu"
photoyb<-left_join(sample.photo,sample.photo2)
photoyb$cue<-"photo"

cuey<-rbind(photoyb,chillyb,forcyb)
cuey<- cuey %>% dplyr::group_by(cue) %>% sample_n(100)}



# this one takes the species level beta values
scrapegrandies<-function(x){ 
  sample <- rstan::extract(x)### takes a while
  
  sample.force <- melt(sample$betaTraitxForcing)
  sample.force2 <- melt(sample$muForceSp)
  
  colnames(sample.force)[2]<-"trait_beta"
  colnames(sample.force2)[2]<-"mu"
  forcyb<-left_join(sample.force,sample.force2)
  forcyb$cue<-"force"
  
  sample.chill <- melt(sample$betaTraitxChill)
  sample.chill2 <- melt(sample$muChillSp)
  
  colnames(sample.chill)[2]<-"trait_beta"
  colnames(sample.chill2)[2]<-"mu"
  chillyb<-left_join(sample.chill,sample.chill2)
  chillyb$cue<-"chill"
  
  
  
  sample.photo <- melt(sample$betaTraitxPhoto)
  sample.photo2 <- melt(sample$muPhotoSp)
  
  colnames(sample.photo)[2]<-"trait_beta"
  colnames(sample.photo2)[2]<-"mu"
  photoyb<-left_join(sample.photo,sample.photo2)
  photoyb$cue<-"photo"
  
  cuey<-rbind(photoyb,chillyb,forcyb)
cuey %>% dplyr::group_by(cue) %>% dplyr::summarise(mu=mean(mu),trait_beta=mean(trait_beta))
}  

betas<-scrapebetas(threeparam_jnt.gdd)
betasggdf<-left_join(betas,ggdlf)
betasggdf<-filter(betasggdf,complex!=99)
cuebert<-scrapeslopes(threeparam_jnt.gdd)
betameans<-scrapegrandies(threeparam_jnt.gdd)


a<-ggplot(betasggdf,aes(Temp.SD.z,mean))+
  geom_point(aes(color=cue,shape=continent))+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(0,16))

gddfull<-a+geom_abline(data=cuebert,aes(intercept = mu,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameans,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+xlab("Interannual variation in GDD before last frost")+ylab("Cue sensitivity")
  

### next plot:
betasS<-scrapebetas(threeparam_jnt.stv)
betaSTV<-dplyr::left_join(betasS,ggdlf,by="complex.wname")
betaSTV<-filter(betasggdf,complex!=99)
cuebertSTV<-scrapeslopes(threeparam_jnt.stv)
betameansSTV<-scrapegrandies(threeparam_jnt.stv)



b<-ggplot(betaSTV,aes(STV.z,mean))+
  geom_point(aes(color=cue,shape=continent))+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(0,16))

stvfull<-b+geom_abline(data=cuebertSTV,aes(intercept = mu,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameansSTV,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("STV")+ylab("Cue sensitivity")

one<-ggpubr::ggarrange(gddfull,stvfull,common.legend = TRUE,labels = c("a)","b)"))


ggpubr::ggarrange(gddfull,stvfull,common.legend = TRUE)

####continent only



cuebertNA<-scrapeslopes(gddlf_jnt.nam)
betameansNA<-scrapegrandies(gddlf_jnt.nam)
cueberteu<-scrapeslopes(gddlf_jnt.eu)
betameanswu<-scrapegrandies(gddlf_jnt.eu)


cuebertNAstv<-scrapeslopes(stv_jnt.nam)
betameansNAstv<-scrapegrandies(stv_jnt.nam)
cueberteustv<-scrapeslopes(stv_jnt.eu)
betameanswustv<-scrapegrandies(stv_jnt.eu)


aa<-ggplot(betasggdf,aes(Temp.SD.z,mean))+
  geom_point(color="white")


bb<-ggplot(betaSTV,aes(STV.z,mean))+
  geom_point(color="white")
 


namplot<-aa+geom_abline(data=cuebertNA,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameansNA,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("North America")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none") 


europlot<-aa+ geom_abline(data=cueberteu,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswu,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+
  theme(legend.position = "none")


namplotstv<-bb+geom_abline(data=cuebertNAstv,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameansNAstv,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("North America")+ylab("Cue sensitivity")+theme(legend.position = "none") 


europlotstv<-bb+ geom_abline(data=cueberteustv,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswustv,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+ylab("Cue sensitivity")+theme(legend.position = "none") 



two<-ggpubr::ggarrange(namplot,europlot,namplotstv,europlotstv,nrow=1,ncol=4,labels=c("c)","d)","e)","f)"))

jpeg("./figures/mock1.jpeg",width = 10,height=8, units = "in",res = 300)
ggpubr::ggarrange(one,two,nrow=2,ncol=1,heights=c(6,5))
dev.off()
