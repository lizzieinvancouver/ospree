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

# Setting working directory.
# Add in your own path in an if statement for your file structure
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

# f(x)s
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Get the data (meaning data and model output) 
outy <- read.csv(file = "betasandmorefromPOPUP.csv")
rangiesEu <- read.csv("output/Synthesis_climate_EUspsw.csv")
rangiesNa <- read.csv("output/Synthesis_climate_Namsps_weighted.csv")

# Range data ...
rangiesEu$continent<-"Europe"
rangiesEu<-dplyr::select(rangiesEu,-X)
rangiesNa$continent<-"N. America"
rangies<-rbind(rangiesEu,rangiesNa)
ggdlf<-filter(rangies,variable=="GDD.lastfrost")
ggdlf<-dplyr::select(ggdlf,species,Temp.SD,continent)
colnames(ggdlf)[1]<-"complex.wname"
STV<-filter(rangies,variable=="MeanTmins")
STV<-dplyr::select(STV,species,Temp.SD,continent)
colnames(STV)[c(1,2)]<-c("complex.wname","STV")
ggdlf<-left_join(ggdlf,STV)

# CHECK ME!!
# hmmm I think we don't want "Picea_mariana" or one of the  "Alnus_incana"
ggdlfuse <- subset(ggdlf, complex.wname!="Picea_mariana")
ggdlfuse <- ggdlfuse[-(which(ggdlfuse$continent=="Europe" & ggdlfuse$complex.wname=="Alnus_incana")),]

# CHECK ME!!
# blithley assuming this is the ordering of Stan model output!
ggdlfuse <- ggdlfuse[with(ggdlfuse, order(complex.wname)), ]


## lizzie trying to do the popUpmodels.jpg



par(mfrow=c(1,3))
par(mar=c(5,4,4,2))
 #plot 1
sloperow <- outy[which(outy$rowname=="betaTraitxForcing" & outy$climparam=="stv"),]
interceptrow <-outy[which(outy$rowname=="muForceSp" & outy$climparam=="stv"),]

forcefigY <- outy[grep("betaF", outy$rowname),]
forcefigY <- forcefigY[forcefigY$climparam=="stv",]

plot(forcefigY$mean~ggdlfuse$STV, xlab="STV", ylab="species-level estimated forcing cue")# is the model of STV? 
abline(a=interceptrow$mean, b=sloperow$mean)

sloperowC <- outy[which(outy$rowname=="betaTraitxChill" & outy$climparam=="stv"),]
interceptrowC <-outy[which(outy$rowname=="muChillSp"&  outy$climparam=="stv"),]

##stvplot
forcefigYc <- outy[grep("betaC", outy$rowname),]
forcefigYc <- forcefigYc[forcefigYc$climparam=="stv",]
plot(forcefigYc$mean~ggdlfuse$STV, xlab="STV", ylab="species-level estimated chilling cue") # is the model of STV? 
abline(a=interceptrowC$mean, b=sloperowC$mean)

sloperowP <- outy[which(outy$rowname=="betaTraitxPhoto" & outy$climparam=="stv"),]
interceptrowP <-outy[which(outy$rowname=="muPhotoSp" &  outy$climparam=="stv"),]

forcefigYp <- outy[grep("betaPho", outy$rowname),]
forcefigYp <- forcefigYp[forcefigYp$climparam=="stv",]
plot(forcefigYp$mean~ggdlfuse$STV, xlab="STV", ylab="species-level estimated photoperiod cue",xlim=c(0,max(ggdlfuse$STV))) # is the model of STV? 
abline(a=interceptrowC$mean, b=sloperowC$mean)


## end lizzie trying to do the popUpmodels.jpg


