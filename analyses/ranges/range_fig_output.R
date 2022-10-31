####ranges plotting based on pop up
###run the rangeleadin_osp.R or load save models on your own machine
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
graphics.off()
# libraries
library(shinystan)
library(reshape2)
library(rstan)
library(rstanarm)
library(dplyr)
library(ggplot2)
library(shinystan)
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

#launch_shinystan(gddlf_jnt.eu)
load("popupmods.Rda")
#tidybayes::get_variables(threeparam_jnt.cp)
###


concordance<-unique(dplyr::select(bb.stan,latbinum,complex.wname)) ### for adding ordinal data

concordance.na<-unique(dplyr::select(bb.stan.nam,latbinum,complex.wname))
concordance.eu<-unique(dplyr::select(bb.stan.eu,latbinum,complex.wname))
###z-score orginal variable for plotting because the model ran on zscored bariables
#ggdlf$Temp.SD.z<-(ggdlf$Temp.SD-mean(ggdlf$Temp.SD))/sd(ggdlf$Temp.SD)
#ggdlf$STV.z<-(ggdlf$STV-mean(ggdlf$STV))/sd(ggdlf$STV)
#ggdlf$GDD.z<-(ggdlf$GDD-mean(ggdlf$GDD))/sd(ggdlf$GDD)
#ggdlf$CP.z<-(ggdlf$ChP-mean(ggdlf$ChP))/sd(ggdlf$ChP)
#ggdlf$SDscale<-ggdlf$Temp.SD.z*4


head(ggdlf)
#extract paramenter 
#summary((threeparam_jnt.gdd))$summary
######FUNS
scrapebetas<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("beta",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"force"
  goo$cue[grepl("betaChill", goo$rowname)]<-"chill"
  goo$cue[grepl("betaPhoto", goo$rowname)]<-"photo"
  goo$latbinum<-c(99,99,99,rep(1:38,3))
  goo<-left_join(goo,concordance)
}


scrapealphas<-function(x){ # this one takes the species level alphas values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("alpha",rowname) &!grepl("Pheno",rowname))
  goo$cue[grepl("alphaForcing", goo$rowname)]<-"force"
  goo$cue[grepl("alphaChill", goo$rowname)]<-"chill"
  goo$cue[grepl("alphaPhoto", goo$rowname)]<-"photo"
  goo$latbinum<-c(rep(1:38,3))
  goo<-left_join(goo,concordance)
}

scrapebetas.na<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("beta",rowname)&!grepl("Trait",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"force"
  goo$cue[grepl("betaChill", goo$rowname)]<-"chill"
  goo$cue[grepl("betaPhoto", goo$rowname)]<-"photo"
  goo$latbinum<-c(rep(1:17,3))
  goo<-left_join(goo,concordance.na)
}


scrapealphas.na<-function(x){ # this one takes the species level alphas values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("alpha",rowname) &!grepl("Pheno",rowname))
  goo$cue[grepl("alphaForcing", goo$rowname)]<-"force"
  goo$cue[grepl("alphaChill", goo$rowname)]<-"chill"
  goo$cue[grepl("alphaPhoto", goo$rowname)]<-"photo"
  goo$latbinum<-c(rep(1:17,3))
  goo<-left_join(goo,concordance.na)
}

scrapealphas.eu<-function(x){ # this one takes the species level alphas values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname")
  goo<-dplyr::filter(goo,grepl("alpha",rowname) &!grepl("Pheno",rowname))
  goo$cue[grepl("alphaForcing", goo$rowname)]<-"force"
  goo$cue[grepl("alphaChill", goo$rowname)]<-"chill"
  goo$cue[grepl("alphaPhoto", goo$rowname)]<-"photo"
  goo$latbinum<-c(rep(1:21,3))
  goo<-left_join(goo,concordance.eu)
}
scrapebetas.eu<-function(x){ # this one takes the species level beta values
  goo <- summary(x)$summary
  goo<-as.data.frame(goo)
  goo<-tibble::rownames_to_column(goo, var = "rowname") 
  goo<-dplyr::filter(goo,grepl("beta",rowname)& !grepl("Trait",rowname))
  goo$cue[grepl("betaForcing", goo$rowname)]<-"force"
  goo$cue[grepl("betaChill", goo$rowname)]<-"chill"
  goo$cue[grepl("betaPhoto", goo$rowname)]<-"photo"
  goo$latbinum<-c(rep(1:21,3))
  goo<-left_join(goo,concordance.eu)
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
cuey<- cuey %>% dplyr::group_by(cue) %>% sample_n(1000)}



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
if(FALSE){

modz<-threeparam_jnt.cp
  
betas<-scrapebetas(modz)
betasggdf<-left_join(betas,ggdlf)
betasggdf<-dplyr::filter(betasggdf,latbinum!=99)
cuebert<-scrapeslopes(modz)
betameans<-scrapegrandies(modz)
alphas<-scrapealphas(modz)
alphasggdf<-left_join(alphas,ggdlf)

betasggdf$diff<-betasggdf$mean-alphasggdf$mean
betasggdf$diff.25<-betasggdf$`25%`-alphasggdf$`25%`
betasggdf$diff.75<-betasggdf$`75%`-alphasggdf$`75%`



a<-ggplot(betasggdf,aes(CP.z,mean))+
  geom_point(aes(color=cue,shape=continent),size=2)+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(4,16))+
  #geom_rect(xmin=-.85,xmax=-.65,ymin=-70,ymax=25,color= "lightgray",alpha=0.001)+
  geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+
  facet_wrap(~cue,scales="free_y")

gddfull<-a+geom_abline(data=cuebert,aes(intercept = mu,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameans,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+xlab("Envrioment")+ylab("Cue sensitivity")
  
aa<-ggplot(alphasggdf,aes(CP.z,mean))+
  geom_point(aes(color=cue,shape=continent),size=2)+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(4,16))+
  #geom_rect(xmin=-.85,xmax=-.65,ymin=-70,ymax=25,color= "lightgray",alpha=0.001)+
  geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+
  facet_wrap(~cue,scales="free_y")+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+xlab("Environment")+ylab("Alpha Cue")
  


aaa<-ggplot(betasggdf,aes(CP.z,diff))+
  geom_point(aes(color=cue,shape=continent),size=2)+facet_wrap(~cue)+
  geom_errorbar(aes(ymin=diff.25,ymax=diff.75,color=cue))+
  geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+
  facet_wrap(~cue,scales="free_y")+
  geom_abline(data=cuebert,aes(intercept = 0,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameans,aes(intercept = 0,slope= trait_beta,color=cue),size=1)+
  scale_shape_manual(values=c(4,16))+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+xlab("Environment")

jpeg("figures/trait_dcom_CPggplot.jpeg", width = 10,height=12,units = "in", res=300)
ggpubr::ggarrange(aaa,aa,gddfull,nrow=3,common.legend = TRUE) 
dev.off()

### next plot:
betasS<-scrapebetas(threeparam_jnt.stv)
betaSTV<-dplyr::left_join(betasS,ggdlf,by="complex.wname")
#betaSTV<-dplyr::filter(betasggdf,complex!=99)
cuebertSTV<-scrapeslopes(threeparam_jnt.stv)
betameansSTV<-scrapegrandies(threeparam_jnt.stv)



b<-ggplot(betaSTV,aes(STV.z,mean))+
  geom_point(aes(color=cue,shape=continent))+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(0,16))#+geom_rect(xmin=-1.1,xmax=-0.5,ymin=-70,ymax=25,color=
                                                                                                    # "lightgray",alpha=0.001)

stvfull<-b+geom_abline(data=cuebertSTV,aes(intercept = mu,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameansSTV,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("STV")+ylab("Cue sensitivity")+facet_wrap(~cue)

ggpubr::ggarrange(gddfull,stvfull,common.legend = TRUE,labels = c("a)","b)"))


ggpubr::ggarrange(gddfull,stvfull,common.legend = TRUE)

####continent only




}
#### Growing degree to last frost
modna<-gddlf_jnt.nam
modeu<-gddlf_jnt.eu


cuebertNA<-scrapeslopes(modna)
betameansNA<-scrapegrandies(modna)
cueberteu<-scrapeslopes(modeu)
betameanswu<-scrapegrandies(modeu)


betas.na<-scrapebetas.na(modna)
ggdlf.na<-dplyr::filter(ggdlf,continent=="N. America")
betasggdf.na<-left_join(betas.na,ggdlf.na)

alphas.na<-scrapealphas.na(modna)
ggdlf.na<-dplyr::filter(ggdlf,continent=="N. America")

alphasggdf.na<-left_join(alphas.na,ggdlf.na,by="complex.wname")


alphasggdf.na$trait<-"alphas"
betasggdf.na$trait<-"betas"

namcomp<-rbind(betasggdf.na,alphasggdf.na)


aa.na<-ggplot(betasggdf.na,aes(Temp.SD,mean))+geom_point(aes(color=cue))+
geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue,width=0))#+#+
 # geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+facet_wrap(~cue)

namplot<-aa.na+geom_abline(data=cuebertNA,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.01)+
  geom_abline(data=betameansNA,aes(intercept = mu,slope= trait_beta,color=cue),size=1.5)+
  ggthemes::theme_few(base_size = 8)+scale_color_viridis_d(option="plasma")+ylim(-50,20)+
  xlab("North America \nstandard deviation of \nGDDs to last frost")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none")+ylab("")



#ggplot(betasggdf.na,aes(Temp.SD.z,meanDiff))+
 # geom_point(aes(color=cue,shape=continent),size=2)+facet_wrap(~cue)
  #geom_errorbar(aes(ymin=diff.25,ymax=diff.75,color=cue))+
  #geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+
  #facet_wrap(~cue,scales="free_y")+
  #geom_abline(data=cuebertNA,aes(intercept = 0,slope= trait_beta,color=cue),alpha=0.05)+
  
#  geom_abline(data=betameansNA,aes(intercept = 0,slope= trait_beta,color=cue),size=1)+
  #scale_shape_manual(values=c(4,16))+
 # ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+xlab("Environment")

jpeg("figures/NorAmeritrait_dcom_gdd2pfggplot.jpeg", width = 10,height=12,units = "in", res=300)
ggpubr::ggarrange(aa.na.alpha,namplot,nrow=2,common.legend = TRUE)
dev.off()


betas.eu<-scrapebetas.eu(modeu)
ggdlf.eu<-dplyr::filter(ggdlf,continent=="Europe")
betasggdf.eu<-left_join(betas.eu,ggdlf.eu)

alphas.eu<-scrapealphas.eu(modeu)
ggdlf.eu<-dplyr::filter(ggdlf,continent=="Europe")
alphasggdf.eu<-left_join(alphas.eu,ggdlf.eu)

alphasggdf.eu$trait<-"alphas"
betasggdf.eu$trait<-"betas"

eurocomp<-rbind(betasggdf.eu,alphasggdf.eu)
plot.e<-ggplot(eurocomp,aes(Temp.SD,mean))+geom_point(aes(shape=trait))+facet_wrap(~cue)+geom_line(aes(group=complex.wname),)
plot.a<-ggplot(namcomp,aes(Temp.SD,mean))+geom_point(aes(shape=trait))+facet_wrap(~cue)+geom_line(aes(group=complex.wname),)
ggpubr::ggarrange(plot.e,plot.a,nrow=2)

data.frame(alphasggdf.eu$mean, alphasggdf.eu$cue,alphasggdf.eu$complex.wname)


aa.eu<-ggplot(betasggdf.eu,aes(Temp.SD,mean))+geom_point(aes(color=cue))+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue,width=0))#+
  #geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+facet_wrap(~cue)

euplot<-aa.eu+geom_abline(data=cueberteu,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.01)+
  geom_abline(data=betameanswu,aes(intercept = mu,slope= trait_beta,color=cue),size=1.5)+
  ggthemes::theme_few(base_size = 8)+scale_color_viridis_d(option="plasma")+ylim(-50,20)+
  xlab("Europe \nstandard deviation of \nGDDs to last frost")+
  theme(legend.position = "none")+ylab("Estimated cue sensitivity")+scale_x_continuous(breaks=c(0,1,2,3))

aa.eu.alpha<-ggplot(alphasggdf.eu,aes(Temp.SD,mean))+geom_point(aes(color=cue))+
  geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+facet_wrap(~cue,scales="free_y")+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma",begin=0.2)+
  xlab("Europe")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none")+ylab("")


aa.na.alpha<-ggplot(alphasggdf.na,aes(Temp.SD,mean))+geom_point(aes(color=cue))+
  geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+facet_wrap(~cue,scales="free_y")+
  ggthemes::theme_few(base_size = 10)+scale_color_viridis_d(option="plasma",begin = 0.2)+
  xlab("North America")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none")+ylab("")
#aaa.eu<-ggplot(betasggdf.eu,aes(Temp.SD.z,diff))+
 # geom_point(aes(color=cue,shape=continent),size=2)+facet_wrap(~cue)+
#  geom_errorbar(aes(ymin=diff.25,ymax=diff.75,color=cue))+
 # geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)+
#  facet_wrap(~cue,scales="free_y")+
#  geom_abline(data=cueberteu,aes(intercept = 0,slope= trait_beta,color=cue),alpha=0.05)+
  
#  geom_abline(data=betameanswu,aes(intercept = 0,slope= trait_beta,color=cue),size=1)+
  #scale_shape_manual(values=c(4,16))+
 # ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+xlab("Environment")


jpeg("figures/mock1.jpeg", width = 6,height=4,units = "in", res=250)

ggpubr::ggarrange(euplot,namplot,ncol=2,common.legend = TRUE,widths=c(.4,.7),labels = c("a)","b)"),label.y = 1.03,label.x = -0.01)
dev.off()



ggdlfmany<-dplyr::select(ggdlf,Temp.SD,complex.wname,continent)
ggdlfmanyF<-ggdlfmany
ggdlfmanyF$cue<-"force"

ggdlfmanyP<-ggdlfmany
ggdlfmanyP$cue<-"photo"

ggdlfmanyC<-ggdlfmany
ggdlfmanyC$cue<-"chill"

ggdlfmany<-rbind(ggdlfmanyF,ggdlfmanyP,ggdlfmanyC)
ggdlfmany<-distinct(ggdlfmany)


trait.eu<-as.data.frame(summy.eu[grep(c("betaTrait"), rownames(summy.eu)),])
trait.eu$cue<-c("force","photo","chill")
traitnam<-as.data.frame(summy.nam[grep(c("betaTrait"), rownames(summy.nam)),])
traitnam$cue<-c("force","photo","chill")


traitnam$continent<- "N. America"
trait.eu$continent<- "Europe"
testimates<-rbind(traitnam,trait.eu)


testimates<-dplyr::select(testimates,mean,cue,continent)
str(testimates)

alphas.eu2<-select(alphas.eu,mean,cue,complex.wname)
alphas.nam2<-select(alphas.na,mean,cue,complex.wname)

alphas.nam2$continent<- "N. America"
alphas.eu2$continent<- "Europe"
aestimates<-rbind(alphas.nam2,alphas.eu2)



colnames(aestimates)[1]<-"alpha"
goober<-merge(aestimates,testimates,by=c("continent","cue"))



betas.na2<-select(betas.na,mean,cue,complex.wname)
betas.eu2<-select(betas.eu,mean,cue,complex.wname)

betafor<-rbind(betas.na2,betas.eu2)

colnames(betafor)[1]<-"cue_beta"
goober<-left_join(goober,betafor)

checky<-merge(goober,ggdlfmany)
checky$check<-(checky$alpha+(checky$mean*checky$Temp.SD))
head(arrange(checky,cue_beta),10)

####


#betasggdf.na<-filter(betasggdf,continent=="N. America")
#betasggdf.eu<-filter(betasggdf,continent=="Europe")
betas.eu<-scrapebetas.eu(gddlf_jnt.eu)
ggdlf.eu<-filter(ggdlf,continent=="Europe")
betasggdf.eu<-left_join(betas.eu,ggdlf.eu)

range(betasggdf.eu$Temp.SD.z,na.rm=TRUE)

aa.na<-ggplot(betasggdf.na,aes(Temp.SD.z,mean))+geom_point(aes(color=cue))+geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)
aa.e<-ggplot(betasggdf.eu,aes(Temp.SD.z,mean))+geom_point(aes(color=cue),shape=4)+geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)

namplot<-aa.na+geom_abline(data=cuebertNA,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameansNA,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("North America")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none")+ylab("")

europlot<-aa.e+ geom_abline(data=cueberteu,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswu,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+ylab("Cue sensitivity")+
  theme(legend.position = "none")


two<-ggpubr::ggarrange(europlot,namplot,nrow=1,ncol=2,widths = c(.4,.5),labels = c("b)","c)"))
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
+ylim(-40,0) 


europlotstv<-bb.e+ geom_abline(data=cueberteustv,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswustv,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+ylab("Cue sensitivity")+theme(legend.position = "none")
+
  scale_x_continuous(breaks=c(-1.5,2))+ylim(-40,5)



twostv<-ggpubr::ggarrange(europlotstv,namplotstv,nrow=1,ncol=2,widths = c(.5,.6),labels=c("b)","c)"))

jpeg("./figures/mockstv.jpeg",width = 10,height=8, units = "in",res = 300)
ggpubr::ggarrange(stvfull,twostv,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)"))
dev.off()

ggpubr::ggarrange(ggpubr::ggarrange(stvfull,twostv,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)")),ggpubr::ggarrange(gddfull,two,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)")))

europlot2<-aa.na+geom_abline(data=cueberteu,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswu,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+ylab("Cue sensitivity")+
  theme(legend.position = "none")

europlot3<-aa.na+geom_abline(data=cueberteu,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameanswu,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Europe")+ylab("")+
  theme(legend.position = "none")
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() 
  )



#ggpubr::ggarrange(gddfull,twob,nrow=2, ncol=1,common.legend = TRUE,labels=c("a)"))
#ggpubr::ggarrange(gddfull,twoc,namplot,nrow=3, ncol=1,common.legend = TRUE,labels=c("a)"))

#library(patchwork)
#europlot2+inset_element(europlot, 0, 0.05, 0.4, 0.5, align_to = 'full')








###table of results
if(FALSE){
cp_summary <- summary(threeparam_jnt.cp, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.025,.25,.75, 0.975))$summary
cp_summary <- summary(threeparam_jnt.cp, probs = c(0.025,.25,.75, 0.975))$summary

gdd_summary <- summary(threeparam_jnt.meangdd, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.025,.25,.75, 0.975))$summary
stv_summary <- summary(threeparam_jnt.stv, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.025,.25,.75, 0.975))$summary
gddlf_summary <- summary(threeparam_jnt.gdd, pars = c("betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.025,.25,.75, 0.975))$summary

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

sums<-dplyr::select(sums,mean,`2.5%`,`25%`,`75%`, `97.5%`,paremeter,climate)
goot<-tidyr::pivot_wider(data = sums, id_cols = climate, names_from =paremeter, values_from = c("mean","10%", "90%"))
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
}
###### make of figure of the means just because #######
betasM<-scrapebetas(threeparam_jnt.meangdd)
betameanGDD<-dplyr::left_join(betasM,ggdlf,by="complex.wname")
betameanGDD<-filter(betameanGDD,latbinum!=99)
cuebertmeanGDD<-scrapeslopes(threeparam_jnt.meangdd)
betameanGDD2<-scrapegrandies(threeparam_jnt.meangdd)

gddplot<-ggplot(betameanGDD,aes(GDD.z,mean))+
  geom_point(aes(color=cue,shape=continent),size=2.5)+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(4,16))+
  geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)
  geom_rect(xmin=-0.7,xmax=0,ymin=-70,ymax=25,color="lightgray",alpha=0.001)

gddplot<-gddplot+geom_abline(data=cuebertmeanGDD,aes(intercept = mu,slope= trait_beta,color=cue),alpha=0.05)+
  
  geom_abline(data=betameanGDD2,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("Mean GDDs")+ylab("Cue sensitivity")


betasC<-scrapebetas(threeparam_jnt.cp)
betameanCP<-dplyr::left_join(betasC,ggdlf,by="complex.wname")
betameanCP<-filter(betameanCP,latbinum!=99)
cuebertmeanCP<-scrapeslopes(threeparam_jnt.cp)
betameanCP2<-scrapegrandies(threeparam_jnt.cp)

cpplot<-ggplot(betameanCP,aes(CP.z,mean))+
  geom_point(aes(color=cue,shape=continent),size=2.5)+
  geom_errorbar(aes(ymin=`25%`,ymax=`75%`,color=cue))+scale_shape_manual(values=c(4,16))+
  geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)
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

#betameanGDD.na<-filter(betameanGDD,continent=="N. America")
#betameanGDD.eu<-filter(betameanGDD,continent!="N. America")
betas.gdd.na<-scrapebetas.na(gdd_jnt.nam)
ggdlf.na<-filter(ggdlf,continent=="N. America")
betameanGDD.na<-left_join(betas.gdd.na,ggdlf.na)
betameanGDD.na<-filter(betameanGDD.na,latbinum!=99)

betas.gdd.eu<-scrapebetas.eu(gdd_jnt.eu)
ggdlf.eu<-filter(ggdlf,continent=="Europe")
betameanGDD.eu<-left_join(betas.gdd.eu,ggdlf.eu)
betameanGDD.eu<-filter(betameanGDD.eu,latbinum!=99)



aaa.eu<-ggplot(betameanGDD.eu,aes(GDD.z,mean))+
  geom_point(aes(color=cue),shape=4)+geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)
aaa.na<-ggplot(betameanGDD.na,aes(GDD.z,mean))+
  geom_point(aes(color=cue))+geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)


#betameanCP.na<-filter(betameanCP,continent=="N. America")
#betameanCP.eu<-filter(betameanCP,continent!="N. America")
betas.cp.na<-scrapebetas.na(cp_jnt.nam)
#ggdlf.na<-filter(ggdlf,continent=="N. America")
betameanCP.na<-left_join(betas.cp.na,ggdlf.na)
betameanCP.na<-filter(betameanCP.na,latbinum!=99)

betas.cp.eu<-scrapebetas.eu(cp_jnt.eu)
#ggdlf.eu<-filter(ggdlf,continent=="Europe")
betameanCP.eu<-left_join(betas.cp.eu,ggdlf.eu)
betameanCP.eu<-filter(betameanCP.eu,latbinum!=99)





bbb.eu<-ggplot(betameanCP.eu,aes(CP.z,mean))+
  geom_point(aes(color=cue),shape=4)+geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)

bbb.na<-ggplot(betameanCP.na,aes(CP.z,mean))+
  geom_point(aes(color=cue))+geom_text(aes(label=complex.wname),hjust=0, vjust=0,size=2)


namplotgdd<-aaa.na+geom_abline(data=cuebertNAgdd,aes(intercept=mu,slope=trait_beta,color=cue),alpha=0.05)+
  geom_abline(data=betameansNAgdd,aes(intercept = mu,slope= trait_beta,color=cue),size=1)+
  ggthemes::theme_few()+scale_color_viridis_d(option="plasma")+
  xlab("North America")+
  ylab("Cue sensitivity")+
  theme(legend.position = "none")


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
twotwo<-ggpubr::ggarrange(euplotgdd,namplotgdd,euplotcp,namplotcp,nrow=1,ncol=4,labels=c("c)","d)","e)","f)"),widths=c(.25,.3,.25,.3))
jpeg("./figures/mock2.jpeg",width = 10,height=8, units = "in",res = 300)
ggpubr::ggarrange(oneone,twotwo,nrow=2,ncol=1,heights=c(6,5))
dev.off()

jpeg("./figures/EUgdd.jpeg",width = 10,height=8, units = "in",res = 300)
euplotgdd
dev.off()

jpeg("./figures/EUcp.jpeg",width = 10,height=8, units = "in",res = 300)
euplotcp
dev.off()

jpeg("./figures/NAMgdd.jpeg",width = 10,height=8, units = "in",res = 300)
namplotgdd
dev.off()

jpeg("./figures/NAMcp.jpeg",width = 10,height=8, units = "in",res = 300)
namplotcp
dev.off()

ggpubr::ggarrange(euplotgdd,namplotgdd,euplotcp,namplotcp,nrow=2,ncol=2,labels=c("c)","d)","e)","f)"),widths=c(.3,.3,.3,.3))


#gddplotscont<-ggpubr::ggarrange(euplotgdd,namplotgdd,common.legend = TRUE,labels = c("a)","b)"),ncol=2,widths=c(.4,.6))
#jpeg("./figures/mock2.jpeg",width = 10,height=8, units = "in",res = 300)
#ggpubr::ggarrange(gddplot,gddplotscont,nrow=2,ncol=1,heights=c(6,5))
#dev.off()
#library(tidybayes)


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



####for understanding
gdd_summary.eu <- summary(gdd_jnt.eu,pars = c("muChillSp","muPhotoSp","muForceSp","betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.025,.25,.75, 0.975))$summary
gdd_summary.eu<-as.data.frame(gdd_summary.eu)
gdd_summary.eu<-dplyr::select(gdd_summary.eu, mean,`2.5%`,`25%`,`75%`,`97.5%`)


cp_summary.eu <- summary(cp_jnt.eu,pars = c("muChillSp","muPhotoSp","muForceSp","betaTraitxForcing","betaTraitxPhoto","betaTraitxChill"), probs = c(0.025,.25,.75, 0.975))$summary
cp_summary.eu<-as.data.frame(cp_summary.eu)
cp_summary.eu<-dplyr::select(cp_summary.eu, mean,`2.5%`,`25%`,`75%`,`97.5%`)
