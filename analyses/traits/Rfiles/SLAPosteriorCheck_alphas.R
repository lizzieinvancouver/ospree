#Running a posterior predictive check on the fit traitors model during teh November 2021 retreit
#The plot is to check the relationship bewteen model predicted day of year and the intercept day of year for each species (alphaPhenoSp)

rm(list = ls())

## Load libraries
library(rstan)
library(ggplot2)

if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
    } else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
    } else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
    } 

#Load SLA model fit
slaModel <- readRDS("../../../../mnt/UBC/ospree/traitorsModelFits/SLA_stanfit.RDS")
slaModelFit <- rstan::extract(slaModel)


#Load Trait Data
dat1 <- read.csv("input/try_bien_nodups_1.csv") 
dat2 <- read.csv("input/try_bien_nodups_2.csv") 
traitsData <- rbind(dat1, dat2)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)



#sensible cue values
#-------------------------------------
forcingValue <- 0.85 # 20 degrees C
chillinValue <- 1 #coudl go up to 2 or 3 
photoValue <- -0.25 # about 12 Or 0.5(about 16)

#Extracting  postreior values 
#----------------------------------

#meanInterceptValues
alphaPhenoSpdf <- data.frame(slaModelFit$alphaPhenoSp)
alphaPhenoSpMean <- colMeans(alphaPhenoSpdf)

#Forcing slope values 
betaForceSpdf <- data.frame(slaModelFit$betaForceSp)
betaForceSpMean <- colMeans(betaForceSpdf)

#Chilling slope values 
betaChillSpdf <- data.frame(slaModelFit$betaChillSp)
betaChillSpMean <- colMeans(betaChillSpdf)


#Photoperiod slope values 
betaPhotoSpdf <- data.frame(slaModelFit$betaPhotoSp)
betaPhotoSpMean <- colMeans(betaPhotoSpdf)

#Overall model 
sigmapheno_yMean <- mean(slaModelFit$sigmapheno_y)

#Predict DOY based on model estimated parameters, One DOY value per species
yPhenoi <- vector()

for(i in 1:length(betaPhotoSpMean)){
	yPhenoi[i] <- alphaPhenoSpMean[i] + betaForceSpMean[i] * forcingValue + betaPhotoSpMean[i] * photoValue + betaChillSpMean[i] * chillinValue
}

#Make a plot of intercept vs full pericted value
#---------------------------------------------------

plot(yPhenoi ~ alphaPhenoSpMean)


#Explore the relationship between slope and intercept 
#-------------------------------------------------------
plot(alphaPhenoSpMean ~ betaForceSpMean)
plot(alphaPhenoSpMean ~ betaChillSpMean)
plot(alphaPhenoSpMean ~ betaPhotoSpMean)

betaCombined <- betaPhotoSpMean+betaChillSpMean+betaForceSpMean
plot(alphaPhenoSpMean ~ betaCombined)



#Trait data 
#--------------

slaData <- traitsData[traitsData$traitname == "Specific_leaf_area",]


slaData$traitvalue


slaData$speciesname
