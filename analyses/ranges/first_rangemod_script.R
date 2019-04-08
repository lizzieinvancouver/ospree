#started by Dan B on April 8 2019
# Goal to model some of the range dynamics of common ospress species 


rm(list=ls()) 
options(stringsAsFactors = FALSE)
# libraries
library(rstan)
library(shinystan)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)
library("ggpubr")
library(ggstance)
library(brms)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

###run models_stan.R or load model output

### concordance between bb.stan$complex and bb.stan$complex.wnames
### Fagus sylvatica 15
##bet pubuscens  10
##pet pendula 9
#Corylus avenulla 14
#Picea abies 19
##Quercus robur 28

m21.ni.sum<-as.data.frame(summary(m2l.ni))
m21.ni.sum<-rownames_to_column(m21.ni.sum,"betas") ###sumarise the model and make the rownames a column

selex<-c("b_chill[15]","b_chill[10]","b_chill[9]","b_chill[14]","b_chill[19]","b_chill[28]",
  "b_force[15]","b_force[10]","b_force[9]","b_force[14]","b_force[19]","b_force[28]",
  "b_photo[15]","b_photo[10]","b_photo[9]","b_photo[14]","b_photo[19]","b_photo[28]") ## these are the predictors we want

test<-filter(m21.ni.sum, betas %in% selex) ## make a data frame with the mean estimates for each cue for each species.
test<-select(test,1:2) ##reduce data sets to just the means

test$complex<-NA ### give each beta a species                    
test$complex[grepl("15", test$betas)]<-"Fagus_sylvatica"
test$complex[grepl("10", test$betas)]<-"Betula_pubescens"
test$complex[grepl("9", test$betas)]<-"Betula_pendula"
test$complex[grepl("14", test$betas)]<-"Corylus_avellana"
test$complex[grepl("19", test$betas)]<-"Picea_abies"
test$complex[grepl("28", test$betas)]<-"Quercus_robur"

test$predictor<-NA ### make predictors
test$predictor[grepl("force", test$betas)]<-"force"
test$predictor[grepl("chill", test$betas)]<-"chill"
test$predictor[grepl("photo", test$betas)]<-"photo"
test<-select(test,-betas)

test<-spread(test,predictor,summary.mean) ### spread the means into predictor colummn
write.csv(test,file="..//ranges/betameans_for_range.sps.csv",row.names=FALSE)
?write.csv()
###read in range data for each species and give it a complex identifyer
fagrange<-read.csv("..//ranges/climate.in.range1980-2017FagSyl.csv")
fagrange$complex<-"Fagus_sylvatica"
betpenrange<-read.csv("..//ranges/climate.in.range1980-2017BetPen.csv")
betpenrange$complex<-"Betula_pendula"
betpubrange<-read.csv("..//ranges/climate.in.range1980-2017BetPub.csv")
betpubrange$complex<-"Betula_pubescens"
coryrange<-read.csv("..//ranges/climate.in.range1980-2017CorAve.csv")
coryrange$complex<-"Corylus_avellana"
picearange<-read.csv("..//ranges/climate.in.range1980-2017PicAb1.csv")
picearange$complex<-"Picea_abies"
querrange<-read.csv("..//ranges/climate.in.range1980-2017QurRo1.csv")
querrange$complex<-"Quercus_robur"


range.dat<-rbind(fagrange,betpenrange,betpubrange,coryrange,picearange,querrange) ###bind all species ranges in one data sheet
daty<-left_join(range.dat,test,by="complex") ###make a single data sheet where range can predict cue effect sizes


lm(chill~SDev.GDD.sites+SDev.Chill.Portions,data=daty)
brm(chill~SDev.GDD.sites+SDev.Chill.Portions,data=daty) ### I don't think this is the best way to modle this best way to model this, but what are alternatives

#                      Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
#Intercept              -9.93      0.44   -10.80    -9.08       5526 1.00
#SDev.GDD.sites          0.02      0.00     0.01     0.02       5223 1.00
#SDev.Chill.Portions     0.37      0.02     0.33     0.41       3669 1.00

##meaniing as chilling is more variable there is a weaker chilling effect??
## and very marginally as GDD varaibility increases chilling becomes weaker

