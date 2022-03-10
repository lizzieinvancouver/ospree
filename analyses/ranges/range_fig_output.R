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

###


concordance<-unique(dplyr::select(bb.stan,complex,complex.wname)) ### for adding ordinal data
###z-score orginal variable for plotting because the model ran on zscored bariables
ggdlf$Temp.SD.z<-(ggdlf$Temp.SD-mean(ggdlf$Temp.SD))/sd(ggdlf$Temp.SD)
ggdlf$STV.z<-(ggdlf$STV-mean(ggdlf$STV))/sd(ggdlf$STV)
ggdlf$GDD.z<-(ggdlf$GDD-mean(ggdlf$GDD))/sd(ggdlf$GDD)
ggdlf$CP.z<-(ggdlf$ChP-mean(ggdlf$ChP))/sd(ggdlf$ChP)

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
cuey %>% dplyr::group_by(cue) %>% dplyr::summarise(mu=mean(mu),trait_beta_sd=sd(trait_beta),trait_beta=mean(trait_beta))
}  

betas<-scrapebetas(threeparam_jnt.gdd)
betasggdf<-left_join(betas,ggdlf)
betasggdf<-filter(betasggdf,complex!=99)
cuebert<-scrapeslopes(threeparam_jnt.gdd)
betameans<-scrapegrandies(threeparam_jnt.gdd)

range(betasggdf$Temp.SD.z)
a<-ggplot(betasggdf,aes(Temp.SD.z,mean))+
  geom_point(aes(color=cue,shape=continent),size=2)+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(4,16))+geom_rect(xmin=0,xmax=0.3,ymin=-70,ymax=25,color=
                                                                                                     "lightgray",alpha=0.001)

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
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(0,16))+geom_rect(xmin=0,xmax=0.3,ymin=-70,ymax=25,color=
                                                                                                     "lightgray",alpha=0.001)

stvfull<-b+geom_abline(data=cuebertSTV,aes(intercept = mu,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameansSTV,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("STV")+ylab("Cue sensitivity")

one<-ggpubr::ggarrange(gddfull,stvfull,common.legend = TRUE,labels = c("a)","b)"))


ggpubr::ggarrange(gddfull,stvfull,common.legend = TRUE)

####continent only


#### Growing degree to last frost
cuebertNA<-scrapeslopes(gddlf_jnt.nam)
betameansNA<-scrapegrandies(gddlf_jnt.nam)
cueberteu<-scrapeslopes(gddlf_jnt.eu)
betameanswu<-scrapegrandies(gddlf_jnt.eu)


betasggdf.na<-filter(betasggdf,continent=="N. America")
betasggdf.eu<-filter(betasggdf,continent=="Europe")

aa.na<-ggplot(betasggdf.na,aes(Temp.SD.z,mean))+geom_point(color="white")
aa.e<-ggplot(betasggdf.eu,aes(Temp.SD.z,mean))+geom_point(color="white")

namplot<-aa.na+geom_abline(data=cuebertNA,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameansNA,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("North America")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none")+ylab("")+ylim(-40,5)

europlot<-aa.e+ geom_abline(data=cueberteu,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswu,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+ylab("Cue sensitivity")+
  theme(legend.position = "none")+scale_x_continuous(breaks=c(-.725,-.72))+ylim(-40,5)

two<-ggpubr::ggarrange(europlot,namplot,nrow=1,ncol=2,widths = c(.2,.5),labels = c("b)","c)"))
#twob<-ggpubr::ggarrange(europlot2,namplot,nrow=1,ncol=2,widths = c(.5,.5),labels = c("b)","c)"))
#twoc<-ggpubr::ggarrange(europlot,europlot3,nrow=1,ncol=2,widths = c(.2,.4),labels = c("b)","c)"))

jpeg("./figures/mock1.jpeg",width = 10,height=8, units = "in",res = 300)
ggpubr::ggarrange(gddfull,two,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)"))
dev.off()


####STV
cuebertNAstv<-scrapeslopes(stv_jnt.nam)
betameansNAstv<-scrapegrandies(stv_jnt.nam)
cueberteustv<-scrapeslopes(stv_jnt.eu)
betameanswustv<-scrapegrandies(stv_jnt.eu)


betaSTV.na<-filter(betaSTV,continent=="N. America")
betaSTV.eu<-filter(betaSTV,continent=="Europe")


bb.na<-ggplot(betaSTV.na,aes(STV.z,mean))+geom_point(color="white")
bb.e<-ggplot(betaSTV.eu,aes(STV.z,mean))+geom_point(color="white")



namplotstv<-bb.na+geom_abline(data=cuebertNAstv,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameansNAstv,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("North America")+ylab("Cue sensitivity")+theme(legend.position = "none") 


europlotstv<-bb.e+ geom_abline(data=cueberteustv,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswustv,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+ylab("Cue sensitivity")+theme(legend.position = "none") 



twostv<-ggpubr::ggarrange(namplotstv,europlotstv,nrow=1,ncol=2,labels=c("b)","c)"))

jpeg("./figures/mockstv.jpeg",width = 10,height=8, units = "in",res = 300)
ggpubr::ggarrange(stvfull,twostv,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)"))
dev.off()

ggpubr::ggarrange(ggpubr::ggarrange(stvfull,twostv,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)")),ggpubr::ggarrange(gddfull,two,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)")))

#europlot2<-aa.na+geom_abline(data=cueberteu,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
 # geom_abline(data=betameanswu,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
#  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
 # xlab("Europe")+ylab("Cue sensitivity")+
  #theme(legend.position = "none")

#europlot3<-aa.na+geom_abline(data=cueberteu,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
 # geom_abline(data=betameanswu,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  #ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  #xlab("Europe")+ylab("")+
  #theme(legend.position = "none")
  #theme(axis.text.y=element_blank(),
   #     axis.ticks.y=element_blank() 
  )



ggpubr::ggarrange(gddfull,twob,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)"))
ggpubr::ggarrange(gddfull,twoc,namplot,nrow=3, ncol=1,common.legend = TRUE,labels=c("a)"))

library(patchwork)
europlot2+inset_element(europlot, 0, 0.05, 0.4, 0.5, align_to = 'full')







#jpeg("./figures/mock1.jpeg",width = 10,height=8, units = "in",res = 300)
ggpubr::ggarrange(one,two,nrow=2,ncol=1,heights=c(6,5))
dev.off()
###table of results

cp_summary <- summary(threeparam_jnt.cp, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
gdd_summary <- summary(threeparam_jnt.meangdd, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
stv_summary <- summary(threeparam_jnt.stv, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
gddlf_summary <- summary(threeparam_jnt.gdd, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary

cp_summary<-as.data.frame(cp_summary)
cp_summary$climate<-"Mean Chill Portions"

gdd_summary<-as.data.frame(gdd_summary)
gdd_summary$climate<- "Mean GDDs"

stv_summary<-as.data.frame(stv_summary)
stv_summary$climate<-"STV"

gddlf_summary<-as.data.frame(gddlf_summary)
gddlf_summary$climate<-"Var. GGD to last frost"

sums<-rbind(cp_summary,gdd_summary,stv_summary,gddlf_summary)
sums$paremeter<-rownames(cp_summary)

sums<-dplyr::select(sums,mean,`10%`,`90%`,paremeter,climate)
goot<-pivot_wider(data = sums, id_cols = climate, names_from =paremeter, values_from = c("mean","10%", "90%"))
colnames(goot)

###use this for table
goot2<- goot[, c(1, 2,5,8,3,6,9,4,7,10 )]

cp_summary_nam <- summary(cp_jnt.nam, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
gdd_summary_nam <- summary(gdd_jnt.nam, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
stv_summary_nam <- summary(stv_jnt.nam, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
gddlf_summary_nam <- summary(gddlf_jnt.nam, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary

cp_summary_nam<-as.data.frame(cp_summary_nam)
cp_summary_nam$climate<-"Mean Chill Portions"

gdd_summary_nam<-as.data.frame(gdd_summary_nam)
gdd_summary_nam$climate<- "Mean GDDs"

stv_summary_nam<-as.data.frame(stv_summary_nam)
stv_summary_nam$climate<-"STV"

gddlf_summary_nam<-as.data.frame(gddlf_summary_nam)
gddlf_summary_nam$climate<-"Var. GGD to last frost"

sums_nam<-rbind(cp_summary_nam,gdd_summary_nam,stv_summary_nam,gddlf_summary_nam)
sums_nam$paremeter<-rownames(cp_summary_nam)

sums_nam<-dplyr::select(sums_nam,mean,`10%`,`90%`,paremeter,climate)
goot_nam<-pivot_wider(data = sums_nam, id_cols = climate, names_from =paremeter, values_from = c("mean","10%", "90%"))


###use this for table
goot2_nam<- goot_nam[, c(1, 2,5,8,3,6,9,4,7,10 )]


cp_summary_eu <- summary(cp_jnt.eu, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
gdd_summary_eu <- summary(gdd_jnt.eu, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
stv_summary_eu <- summary(stv_jnt.eu, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary
gddlf_summary_eu <- summary(gddlf_jnt.eu, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.1, 0.9))$summary

cp_summary_eu<-as.data.frame(cp_summary_eu)
cp_summary_eu$climate<-"Mean Chill Portions"

gdd_summary_eu<-as.data.frame(gdd_summary_eu)
gdd_summary_eu$climate<- "Mean GDDs"

stv_summary_eu<-as.data.frame(stv_summary_eu)
stv_summary_eu$climate<-"STV"

gddlf_summary_eu<-as.data.frame(gddlf_summary_eu)
gddlf_summary_eu$climate<-"Var. GGD to last frost"

sums_eu<-rbind(cp_summary_eu,gdd_summary_eu,stv_summary_eu,gddlf_summary_eu)
sums_eu$paremeter<-rownames(cp_summary_eu)

sums_eu<-dplyr::select(sums_eu,mean,`10%`,`90%`,paremeter,climate)
goot_eu<-pivot_wider(data = sums_eu, id_cols = climate, names_from =paremeter, values_from = c("mean","10%", "90%"))


###use this for table
goot2_eu<- goot_eu[, c(1, 2,5,8,3,6,9,4,7,10 )]

goot2_eu$continent<-"Europe"
goot2_nam$continent<-"N. America"

contsums<-rbind(goot2_eu,goot2_nam)
contsums<-contsums[, c(11,1,2,3,4,5,6,7,8,9,10 )]
library(xtable)
xtable(goot2)
xtable(contsums)


beetas_summary_eu <- summary(gddlf_jnt.eu, pars = c("muChillSp","muPhotoSp","muForceSp"), probs = c(.1,0.25, 0.75,.9))$summary
beetas_summary_nam <- summary(gddlf_jnt.nam, pars = c("muChillSp","muPhotoSp","muForceSp"), probs = c(.1,0.25, 0.75,.9))$summary

beetas_summary_eu<-as.data.frame(beetas_summary_eu)
beetas_summary_nam<-as.data.frame(beetas_summary_nam)

beetas_summary_eu$continent<-"Eu"
beetas_summary_nam$continent<-"N.A"

beetas<-rbind(beetas_summary_eu,beetas_summary_nam)
beetas<-select(beetas, mean,`10%`,`25%`,`75%`,`90%`, continent)

xtable(beetas)

###### make of figure of the means just because #######
betasM<-scrapebetas(threeparam_jnt.meangdd)
betameanGDD<-dplyr::left_join(betasM,ggdlf,by="complex.wname")
betameanGDD<-filter(betameanGDD,complex!=99)
cuebertmeanGDD<-scrapeslopes(threeparam_jnt.meangdd)
betameanGDD2<-scrapegrandies(threeparam_jnt.meangdd)

gddplot<-ggplot(betameanGDD,aes(GDD.z,mean))+
  geom_point(aes(color=cue,shape=continent),size=2.5)+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(4,16))+
  geom_rect(xmin=-0.7,xmax=0,ymin=-70,ymax=25,color="lightgray",alpha=0.001)

gddplot<-gddplot+geom_abline(data=cuebertmeanGDD,aes(intercept = mu,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameanGDD2,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Mean GDDs")+ylab("Cue sensitivity")


betasC<-scrapebetas(threeparam_jnt.cp)
betameanCP<-dplyr::left_join(betasC,ggdlf,by="complex.wname")
betameanCP<-filter(betameanCP,complex!=99)
cuebertmeanCP<-scrapeslopes(threeparam_jnt.cp)
betameanCP2<-scrapegrandies(threeparam_jnt.cp)

cpplot<-ggplot(betameanCP,aes(CP.z,mean))+
  geom_point(aes(color=cue,shape=continent),size=2.5)+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(4,16))+
  geom_rect(xmin=-0.8,xmax=0.3,ymin=-70,ymax=25,color="lightgray",alpha=0.001)

cpplot<-cpplot+geom_abline(data=cuebertmeanCP,aes(intercept = mu,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameanCP2,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Mean Chill Portions")+ylab("Cue sensitivity")

ggpubr::ggarrange(cpplot,gddplot)



cuebertNAgdd<-scrapeslopes(gdd_jnt.nam)
betameansNAgdd<-scrapegrandies(gdd_jnt.nam)
cueberteugdd<-scrapeslopes(gdd_jnt.eu)
betameanswugdd<-scrapegrandies(gdd_jnt.eu)

cuebertNAcp<-scrapeslopes(cp_jnt.nam)
betameansNAcp<-scrapegrandies(cp_jnt.nam)
cueberteucp<-scrapeslopes(cp_jnt.eu)
betameanswucp<-scrapegrandies(cp_jnt.eu)

betameanGDD.na<-filter(betameanGDD,continent=="N. America")
betameanGDD.eu<-filter(betameanGDD,continent!="N. America")

aaa.eu<-ggplot(betameanGDD.eu,aes(GDD.z,mean))+
  geom_point(color="white")
aaa.na<-ggplot(betameanGDD.na,aes(GDD.z,mean))+
  geom_point(color="white")


betameanCP.na<-filter(betameanCP,continent=="N. America")
betameanCP.eu<-filter(betameanCP,continent!="N. America")

bbb.eu<-ggplot(betameanCP.eu,aes(CP.z,mean))+
  geom_point(color="white")

bbb.na<-ggplot(betameanCP.na,aes(CP.z,mean))+
  geom_point(color="white")


namplotgdd<-aaa.na+geom_abline(data=cuebertNAgdd,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameansNAgdd,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("North America")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none")+ylim(-40,5)


euplotgdd<-aaa.eu+geom_abline(data=cueberteugdd,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswugdd,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none") +ylim(-40,5)


namplotcp<-bbb.na+geom_abline(data=cuebertNAcp,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameansNAcp,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("North America")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none") +ylim(-40,5)


euplotcp<-bbb.eu+geom_abline(data=cueberteucp,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswucp,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none") +ylim(-40,5)


oneone<-ggpubr::ggarrange(gddplot,cpplot,common.legend = TRUE,labels = c("a)","b)"))
twotwo<-ggpubr::ggarrange(euplotgdd,namplotgdd,euplotcp,namplotcp,nrow=1,ncol=4,labels=c("c)","d)","e)","f)"),widths=c(.2,.3,.2,.3))
jpeg("./figures/mock2.jpeg",width = 10,height=8, units = "in",res = 300)
ggpubr::ggarrange(oneone,twotwo,nrow=2,ncol=1,heights=c(6,5))
dev.off()

library(tidybayes)


get_variables(gddlf_jnt.nam)
NAMcueests<-gddlf_jnt.nam%>%
  spread_draws(muChillSp,muPhotoSp,muForceSp)

NAMcueests<-tidyr::gather(NAMcueests,"cue","Estimate",4:6)
NAMcueests$continent<-"N. America"

ggplot(NAMcueests,aes(cue,Estimate))+geom_violin()+facet_wrap(~cue)

EUcuests<-gddlf_jnt.eu%>%
  spread_draws(muChillSp,muPhotoSp,muForceSp)
EUcuests<-tidyr::gather(EUcuests,"cue","Estimate",4:6)
EUcuests$continent<-"Europe"

contcomps<-rbind(NAMcueests,EUcuests)
contcomps$cues<-NA
contcomps$cues[which(contcomps$cue=="muChillSp" )]<- "Chilling"
contcomps$cues[which(contcomps$cue=="muForceSp" )]<- "Forcing"
contcomps$cues[which(contcomps$cue=="muPhotoSp" )]<- "Photoperiod"

jpeg("./figures/ontinental_cues.jpeg",width = 10,height=8, units = "in",res = 300)
ggplot(contcomps,aes(continent,Estimate))+geom_violin(aes(fill=cues,color=cues),alpha=0.2)+geom_boxplot(aes(fill=cues),alpha=0.8)+facet_wrap(~cues)+
  ggthemes::theme_few()+scale_fill_viridis_d(option="plasma")+scale_color_viridis_d(option="plasma")+
  theme(strip.background = element_blank(),
        strip.text = element_blank())
dev.off()
