###3 ploting range model

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
library(stringr)
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

outy<-read.csv(file = "betasandmorefromPOPUP.csv")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
outy$species<-substrRight(outy$X, 4)
outy$species<-str_remove(outy$species, "p")
outy$species<-str_remove(outy$species, "e")
outy$species<-str_remove(outy$species, "o")
outy$species<-str_remove(outy$species, "l")


beta<-filter(outy,param=="beta[sp]")



png("figures/betacomps.png",width = 10,height=10, units = "in",res=300)
pd=position_dodge(width=.5)
ggplot(beta, aes(cue,mean))+geom_point(aes(color=model),position=pd)+
  geom_errorbar(aes(ymin=X25.,ymax=X75.,color=model),position=pd,width=0)+
  facet_wrap(~species,scales = "free_y")+ggthemes::theme_clean()

dev.off()


ggplot(beta, aes(species,mean))+geom_point(aes(color=model),position=pd)+
  geom_errorbar(aes(ymin=X25.,ymax=X75.,color=model),position=pd,width=0)+
  facet_wrap(~cue,scales = "free_y")
dev.off()

range<-filter(outy,param %in% c("alpha[sp]"))
range2<-filter(range,!is.na(cue))

slope<-filter(outy,param %in% c("beta[range]"))

range2$slope<-NA
range2$slope[range2$cue=="chill"]<-slope$mean[3]
range2$slope[range2$cue=="force"]<-slope$mean[1]
range2$slope[range2$cue=="photo"]<-slope$mean[3]


png("figures/gdd2lfslopes.png",width = 10,height=10, units = "in",res=300)
ggplot(range2,aes(slope,mean))+
  geom_abline(aes(x = slope,y = mean,intercept = mean,slope=slope,color=species))+
  facet_wrap(~cue)+xlim(-0.5936388 , 2.6370043)+geom_hline(yintercept=0)+
  ggthemes::theme_clean()
##xlim are the range data
dev.off()
?geom_segment()

png("figures/proprotions.png",width = 10,height=10, units = "in",res=300)
ggplot()+
    geom_bar(data=beta,stat="identity",aes(species,mean),fill="darkgreen",color="darkgreen",alpha=0.4)+
  geom_bar(data=range2,stat="identity",aes(species,mean),fill="white",color="hotpink",alpha=0.4)+
  facet_wrap(~cue,nrow=3,scales="free_y")+ggthemes::theme_clean()

dev.off()
betamu<-filter(outy,param=="mu")
betamu$cue<-ifelse(betamu$X=="muForceSp","force",betamu$cue)
betamu<-filter(betamu,model=="Ranges")
betamu<-filter(betamu,!is.na(cue))

betamu<-dplyr::select(betamu,mean,se_mean, X25.,X75., cue)
colnames(betamu)<-c("intercepty","se_intercept","Incercept25","Intercept75","cue")

maineffs<-left_join(betamu,slope)

ggplot(maineffs,aes(y =intercepty,x=10))+geom_point()+
  geom_abline(aes(intercept =intercepty,slope=mean,color=cue))+
  geom_abline(aes(intercept =Incercept25,slope=X25.,color=cue),linetype="dashed")+
  geom_abline(aes(intercept =Intercept75,slope=X75.,color=cue),linetype="dashed")+
  xlim(-0.5936388 , 2.6370043)+xlab("gdd 2 lastfrost")+ylab("cue sensitivity")+ ylim(-15,1)+
  ggthemes::theme_clean()

