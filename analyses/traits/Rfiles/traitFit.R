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
library(gridExtra) # for arranging plots 
library(patchwork) # another way of arranging plots 
library(rethinking)

#set the traits we are assessing
#this code will only work if you have the model outputs saved locally 

if(length(grep("deirdreloughnan", getwd())>0)) {  
	setwd("~/Documents/github/ospree/analyses/traits")
    } else if (length(grep("faith", getwd())>0)) { 
    	setwd("/home/faith/Documents/github/ospree/analyses/traits") 
    } else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
    } 
traits <- c("SLA", "Height", "LNC", "SeedMass_log10")
	filePathData <- "output/"
traitModelNames <- grep("_37spp.RDS", list.files(filePathData), value = TRUE) 

#Make a dataframe for saving traiit estimates for results section
traitsDF <- data.frame(matrix(NA, 4,18))
names(traitsDF) <- c("Trait", "GrandMean", "GrandMean_upper", "GrandMean_lower", 
	"SpeciesSigma",  "SpeciesSigma_upper", "SpeciesSigma_lower", 
	"StudySigma",  "StudySigma_upper", "StudySigma_lower", 
	"MaxValue",  "MaxValue_upper", "MaxValue_lower", "MaxValueSp", 
	"MinValue", "MinValueSp", "MinValue_upper", "MinValue_lower")
traitsDF$Trait <- traits

#Load Trait Data
	dat1 <- read.csv("input/try_bien_nodups_1.csv") 
	dat2 <- read.csv("input/try_bien_nodups_2.csv") 
	traitsData <- rbind(dat1, dat2)

	traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")
	
	# traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

# Subset data to traitors species list
	traitsData <- subset(traitsData, traitsData$speciesname %in% traitors.sp)

	traitsData$traitname <- as.character(traitsData$traitname)
	unique(traitsData$traitname)
	traitsData$traitname [traitsData$traitname == "Specific_leaf_area"] <- "SLA"
	traitsData$traitname [traitsData$traitname == "Stem_specific_density"] <- "SSD"
	traitsData$traitname [traitsData$traitname == "seed mass"] <- "SeedMass_log10"
	traitsData$traitname [traitsData$traitname == "Plant_height_vegetative"] <- "height"
	traitsData$traitname [traitsData$traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass"] <- "LNC"
	traitsData$speciesname <- gsub("_", " ", traitsData$speciesname )

traitPlotList <- list() #Make a list to save trait plots so we can make 1 pannel with all 4 plots at the end 

for(traiti in 1:length(traitModelNames)){

 
# 	traiti <- 3

	#Load SLA model fit
	slaModel <- readRDS(paste(filePathData,traitModelNames[traiti], sep = "/"))
	traitName <- gsub("_stanfit_37spp.RDS", "", traitModelNames[traiti])
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

	for(ip in 1:length(betaPhotoSpMean)){
		yPhenoi[ip] <- alphaPhenoSpMean[ip] + betaForceSpMean[ip] * forcingValue + betaPhotoSpMean[ip] * photoValue + betaChillSpMean[ip]* chillinValue
	}

	#Make a plot of intercept vs full pericted value
	#---------------------------------------------------

	plot(yPhenoi ~ alphaPhenoSpMean)


	#Explore the relationship between slope and intercept 
	#-------------------------------------------------------
	
	#png(paste("figures/interceptSlope", traitName, sep = "_"), width = 780, height = 380, units = "px")
	# par(mfrow=c(1,4))  
	# plot(alphaPhenoSpMean,betaForceSpMean, main = traitName)
	# plot(alphaPhenoSpMean,betaChillSpMean, main = "Model Intercepts vs slopes")
	# plot(alphaPhenoSpMean,betaPhotoSpMean)
	# betaCombined <- betaPhotoSpMean+betaChillSpMean+betaForceSpMean
	# plot(alphaPhenoSpMean ~ betaCombined)
	# #dev.off()
	# par(mfrow=c(1,1))  
	# 

	betaCombined <- betaPhotoSpMean+betaChillSpMean+betaForceSpMean
	# plot(alphaPhenoSpMean ~ betaCombined)


	#Trait data 
	#--------------
#traiti <- "SeedMass_log10_stanfit.RDS"
	# traiti <- 1
	if(traitModelNames[traiti] == "SeedMass_log10_stanfit_37spp.RDS"){
		slaData <- traitsData[traitsData$traitname == "SeedMass_log10",]
		specieslist <- sort(unique(slaData$speciesname))
		slaData$traitvalue_log <- log10(slaData$traitvalue)
    
		meanRealTrait <- aggregate(slaData$traitvalue_log, by = list(slaData$speciesname), FUN = mean)
		names(meanRealTrait) <- c("species","meanTrait")
	} else {
		slaData <- traitsData[traitsData$traitname == traitName,]
		specieslist <- sort(unique(slaData$speciesname))
		
		meanRealTrait <- aggregate(slaData$traitvalue, by = list(slaData$speciesname), FUN = mean)
		names(meanRealTrait) <- c("species","meanTrait")
	}
	


	#Trait predicted means
	#--------------------

	mu_grandDf <- data.frame(slaModelFit$mu_grand_sp)
	colnames(mu_grandDf) <- specieslist
	longMeans <- melt(mu_grandDf)
	colnames(longMeans) <- c("speciesname", "traitMean")

	mu_grand_mean <- colMeans(mu_grandDf)

	color_scheme_set("viridis")

	if(traitName == "SeedMass_log10"){
	  mcmc_intervals(mu_grandDf)+
	    theme_classic() + 
	    theme(text = element_text(size=20))+
	    geom_point(data = slaData, aes(y = speciesname, x = traitvalue_log), alpha = 0.5)
	  
		traitFit <- ggplot(data = slaData, aes(y = speciesname, x = traitvalue_log, colour = "black"))+
			stat_eye(data = longMeans, aes(y = speciesname, x = traitMean))+
			geom_point( alpha = 0.5, size = 1.2, aes(colour = "red"))+
			theme_classic() +  
			theme(text = element_text(size=16))+
	 			 geom_point(data = meanRealTrait, aes(x = meanTrait,y = species, colour = "purple"), shape = 8, size = 3)+
	 			 labs(title = "Log Seed Mass", y = "Species", x =expression(Log[10]~Trait~Value))+ 
	 			 scale_color_identity(name = "Model fit",
                          breaks = c("black", "red", "purple"),
                          labels = c("Model Posterior", "Raw Data", "Data Mean"),
                          guide = guide_legend(override.aes = list(
                         	linetype = c(NA, NA, NA),
                         	shape = c(19, 20, 8)))) + 
	  			theme(legend.title = element_blank())
	} else {
	  mcmc_intervals(mu_grandDf)+
	    theme_classic() + 
	    theme(text = element_text(size=20))+
	    geom_point(data = slaData, aes(y = speciesname, x = traitvalue), alpha = 0.5)
	  
		traitFit <- ggplot(data = slaData, aes(y = speciesname, x = traitvalue, colour = "black"))+
			stat_eye(data = longMeans, aes(y = speciesname, x = traitMean))+
				geom_point( alpha = 0.5, size = 1.2, aes(colour = "red"))+
				theme_classic() +  
				theme(text = element_text(size=16))+
	  		geom_point(data = meanRealTrait, aes(x = meanTrait,y = species, colour = "purple"), shape = 8, size = 3)+
	  		labs(title = traitName, y = "Species", x ="Trait Value")+ 
	  		scale_color_identity(name = "Model fit",
                          breaks = c("black", "red", "purple"),
                          labels = c("Model Posterior", "Raw Data", "Data Mean"),
                          guide = guide_legend(override.aes = list(
                         	linetype = c(NA, NA, NA),
                         	shape = c(19, 20, 8)))) + 
	  		theme(legend.title = element_blank())
	}

	



	 #ggsave(paste(paste0("figures/traitFit_", traitName), "png", sep = "."), traitFit,   width = 10, height = 6,  units = "in")
	  traitPlotList[[traiti]] <-traitFit 

	  #saving model values in a table

	  traitOutput <- data.frame(summary(slaModel))
	  speciesMeans <-  aggregate(longMeans$traitMean, by = list(longMeans$speciesname), FUN = mean)
	  names(speciesMeans) <- c("speciesname", "traitMean")

	  #Get Trait Model output for the results section. 

		#Mean trait value
	  	traitsDF$GrandMean[traiti] <- mean(slaModelFit$mu_grand)
		#Uncertainty around mu grand
		traitsDF$GrandMean_upper[traiti] <- HPDI( as.vector(slaModelFit$mu_grand) , prob=0.90 )[2]
		traitsDF$GrandMean_lower[traiti] <- HPDI( as.vector(slaModelFit$mu_grand) , prob=0.90 )[1]

		#speciesSigma
		traitsDF$SpeciesSigma[traiti] <- mean(slaModelFit$sigma_sp)
		#Uncertainty around speciesSigma
		traitsDF$SpeciesSigma_upper[traiti] <- HPDI( as.vector(slaModelFit$sigma_sp) , prob=0.90 )[2]
		traitsDF$SpeciesSigma_lower[traiti] <- HPDI( as.vector(slaModelFit$sigma_sp) , prob=0.90 )[1]

		#studySigma
		traitsDF$StudySigma[traiti] <- mean(slaModelFit$sigma_study)
		#Uncertainty around studySigma
		traitsDF$StudySigma_upper[traiti] <- HPDI( as.vector(slaModelFit$sigma_study) , prob=0.90 )[2]
		traitsDF$StudySigma_lower[traiti] <- HPDI( as.vector(slaModelFit$sigma_study) , prob=0.90 )[1]



		#Max trait value
		traitsDF$MaxValue[traiti] <- max(speciesMeans$traitMean)
		#max trait value species id
        traitsDF$MaxValueSp[traiti] <- as.character(speciesMeans$speciesname[speciesMeans$traitMean == max(speciesMeans$traitMean)])
		#Uncertainty around max species
		traitsDF$MaxValue_upper[traiti] <- HPDI( as.vector(longMeans$traitMean[longMeans$speciesname == traitsDF$MaxValueSp[traiti]]) , prob=0.90 )[2]
		traitsDF$MaxValue_lower[traiti] <- HPDI( as.vector(longMeans$traitMean[longMeans$speciesname == traitsDF$MaxValueSp[traiti]]) , prob=0.90 )[1]



        #Min trait value
		traitsDF$MinValue[traiti] <- min(speciesMeans$traitMean)
		#min trait value species id
        traitsDF$MinValueSp[traiti] <- as.character(speciesMeans$speciesname[speciesMeans$traitMean == min(speciesMeans$traitMean)])
        #Uncertainty around max species
		traitsDF$MinValue_upper[traiti] <- HPDI( as.vector(longMeans$traitMean[longMeans$speciesname == traitsDF$MinValueSp[traiti]]) , prob=0.90 )[2]
		traitsDF$MinValue_lower[traiti] <- HPDI( as.vector(longMeans$traitMean[longMeans$speciesname == traitsDF$MinValueSp[traiti]]) , prob=0.90 )[1]


	
}



	#this plotting code needs the patchwork library 
	  png("figures/FourTraitFit_37spp.png", width = 14, height = 15, units = "in", res = 72)
	    combined <- traitPlotList[[1]] + traitPlotList[[2]] + traitPlotList[[3]] + traitPlotList[[4]] & theme(legend.position = "bottom") # combien plots and put legend at teh bottom
	    combined[[2]] <- combined[[2]] + theme(axis.title.y = element_blank() )#Remove y labels from plots 2 and 4
	    combined[[4]] <- combined[[4]] + theme(axis.title.y = element_blank() )
		combined + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")#add letter annotation to plots 
	  dev.off()
	  

