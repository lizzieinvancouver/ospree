#Running a posterior predictive check on the fit traitors model during teh November 2021 retreit
#The plot is to check the relationship bewteen model predicted day of year and the intercept day of year for each species (alphaPhenoSp)

rm(list = ls())

## Load libraries
library(rstan)
library(ggplot2)
library(reshape2)
library(viridis)
library(bayesplot)
library(tidybayes)

#set the traits we are assessing
#this code will only work if you have the model outputs saved locally 

traits <- c("SLA", "height", "LNC", "SeedMass", "SSD")
filePathData <- "../../../../mnt/UBC/ospree/traitorsModelFits"
traitModelNames <- grep(".RDS", list.files(filePathData), value = TRUE)


if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
    } else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
    } else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
    } 

#Load Trait Data
	dat1 <- read.csv("input/try_bien_nodups_1.csv") 
	dat2 <- read.csv("input/try_bien_nodups_2.csv") 
	traitsData <- rbind(dat1, dat2)

	traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
	traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

	traitsData$traitname <- as.character(traitsData$traitname)
	unique(traitsData$traitname)
	traitsData$traitname [traitsData$traitname == "Specific_leaf_area"] <- "SLA"
	traitsData$traitname [traitsData$traitname == "Stem_specific_density"] <- "SSD"
	traitsData$traitname [traitsData$traitname == "seed mass"] <- "SeedMass"
	traitsData$traitname [traitsData$traitname == "Specific_leaf_area"] <- "LNC"
	traitsData$traitname [traitsData$traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass"] <- "SLA"


for(traiti in 1:length(traitModelNames)){


	#traiti <- 2

	#Load SLA model fit
	slaModel <- readRDS(paste(filePathData,traitModelNames[traiti], sep = "/"))
	traitName <- gsub("_stanfit.RDS", "", traitModelNames[traiti])
	slaModelFit <- rstan::extract(slaModel)
	data.frame(summary(slaModel))



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
	
	png(paste("figures/interceptSlope", traitName, sep = "_"), width = 780, height = 380, units = "px")
	par(mfrow=c(1,4))  
	plot(alphaPhenoSpMean,betaForceSpMean, main = traitName)
	plot(alphaPhenoSpMean,betaChillSpMean, main = "Model Intercepts vs slopes")
	plot(alphaPhenoSpMean,betaPhotoSpMean)
	betaCombined <- betaPhotoSpMean+betaChillSpMean+betaForceSpMean
	plot(alphaPhenoSpMean ~ betaCombined)
	dev.off()
	par(mfrow=c(1,1))  


	betaCombined <- betaPhotoSpMean+betaChillSpMean+betaForceSpMean
	plot(alphaPhenoSpMean ~ betaCombined)



	#Trait data 
	#--------------

	traitSelection <- 
	slaData <- traitsData[traitsData$traitname == "traitName",]
	specieslist <- sort(unique(slaData$speciesname))

	meanRealTrait <- aggregate(slaData$traitvalue, by = list(slaData$speciesname), FUN = mean)
	names(meanRealTrait) <- c("species","meanTrait")

	#Trait predicted means
	#--------------------

	mu_grandDf <- data.frame(slaModelFit$mu_grand_sp)
	colnames(mu_grandDf) <- specieslist
	longMeans <- melt(mu_grandDf)
	colnames(longMeans) <- c("speciesname", "traitMean")

	mu_grand_mean <- colMeans(mu_grandDf)

	color_scheme_set("viridis")

	mcmc_intervals(mu_grandDf)+
	 theme_classic() + 
		theme(text = element_text(size=20))+
		geom_point(data = slaData, aes(y = speciesname, x = traitvalue), alpha = 0.5)


	traitFit <- ggplot(data = slaData, aes(y = speciesname, x = traitvalue))+
		geom_point( alpha = 0.5, colour = "red", size = 0.5)+
		theme_classic() +  
		theme(text = element_text(size=20))+
	  stat_eye(data = longMeans, aes(y = speciesname, x = traitMean))+
	  geom_point(data = meanRealTrait, aes(x = meanTrait,y = species), colour = "green", shape = 3)+
	  labs(title = traitName)

	 ggsave(paste(paste0("figures/traitFit_", traitName), "png", sep = "."), traitFit,   width = 10,
  height = 6,  units = "in")

}

