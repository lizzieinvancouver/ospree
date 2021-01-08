rm(list = ls())
## No one likes factors
options(stringsAsFactors = FALSE)


##setwd("C:\\Users\\Faith Jones\\Documents\\ubc\\OspreeTraits")
#setwd("/home/faith/Documents/UBC/ospree")

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX")
#}

## Load libraries
library(tidyr)
library(dplyr)
library(data.table)

### Start by cleaning species names ################################################### MG Started 4 Nov 2020
source("Rfiles/cleaning_try/TRY_cleaning_Nov2020FJ.R")
## UPDATE: Now using the new post Nov 2020 cleaned Try dataset
dataNoExpshort<-read.csv("input/TryDataCleanedNew_Nov2020.csv", fileEncoding="latin1")
namelist <- sort(unique(dataNoExpshort$SpeciesName))

#split into genus and species columns and a third column for authority/subsp/any other words
# break up name column into genus, species, extra stuff columns
breakname <- strsplit(as.character(dataNoExpshort$SpeciesName), " ", fixed=TRUE)
dataNoExpshort$genus <- unlist(lapply(breakname, function(x) x[1]))
dataNoExpshort$species <- unlist(lapply(breakname, function(x) x[2]))
dataNoExpshort$authority.subsp <- unlist(lapply(breakname, function(x) x[3]))

#acutal cleaning
dataNoExpshort$genus[which(dataNoExpshort$genus == "Facus")] <- "Fagus"
dataNoExpshort$genus[which(dataNoExpshort$genus == "Coryluss")] <- "Corylus"
dataNoExpshort$genus[which(dataNoExpshort$genus == "Beta")] <- "Betula"
dataNoExpshort$genus[which(dataNoExpshort$genus == "BETA")] <- "Betula"
dataNoExpshort$genus[which(dataNoExpshort$genus == "BETULA")] <- "Betula"


dataNoExpshort$species[which(dataNoExpshort$species == "pensilvanicum")] <- "pensylvanicum"
dataNoExpshort$species[which(dataNoExpshort$species == "pennsylvanicum")] <- "pensylvanicum"
dataNoExpshort$species[which(dataNoExpshort$species == "grandfolia")] <- "grandifolia"
dataNoExpshort$species[which(dataNoExpshort$species == "pennsylvanica")] <- "pensylvanica"
dataNoExpshort$species[which(dataNoExpshort$species == "catharticus")] <- "cathartica"
dataNoExpshort$species[which(dataNoExpshort$species =="myrtilLoides")] <- "myrtilloides"
dataNoExpshort$genus[which(dataNoExpshort$genus == "CORYLUS")] <- "Corylus"
dataNoExpshort$genus[which(dataNoExpshort$genus == "FRAXINUS")] <- "Fraxinus"
dataNoExpshort$genus[which(dataNoExpshort$genus == "KALMIA")] <- "Kalmia"

dataNoExpshort$genus[which(dataNoExpshort$genus == "SORBUS")] <- "Sorbus"
dataNoExpshort$species[which(dataNoExpshort$species == "ARIA")] <- "aria"
dataNoExpshort$species[which(dataNoExpshort$species == "TORMINALIS")] <- "torminalis"
dataNoExpshort$genus[which(dataNoExpshort$genus == "RHAMNUS")] <- "Rhamnus"
dataNoExpshort$species[which(dataNoExpshort$species == "CATHARTICA")] <- "cathartica"
dataNoExpshort$genus[which(dataNoExpshort$genus == "AESCULUS")] <- "Aesculus"
dataNoExpshort$species[which(dataNoExpshort$species == "HIPPOCASTANUM")] <- "hippocastanum"
dataNoExpshort$species[which(dataNoExpshort$species == "hippocastaneum")] <- "hippocastanum"
dataNoExpshort$species[which(dataNoExpshort$species == "seroti")] <- "serotina"
dataNoExpshort$species[which(dataNoExpshort$species == "ussurensis")] <- "ussuriensis"
dataNoExpshort$species[which(dataNoExpshort$species == "petrae")] <- "petraea"
dataNoExpshort$species[which(dataNoExpshort$species == "veluti")] <- "velutina"
dataNoExpshort$species[which(dataNoExpshort$species == "alpinus")] <- "alpina"
dataNoExpshort$species[which(dataNoExpshort$species == "parviflora")] <- "parvifolia"
dataNoExpshort$species[which(dataNoExpshort$species == "AVELLANA")] <- "avellana"
dataNoExpshort$species[which(dataNoExpshort$species == "EXCELSIOR")] <- "excelsior"
dataNoExpshort$species[which(dataNoExpshort$species == "ANGUSTIFOLIA")] <- "angustifolia"
dataNoExpshort$species[which(dataNoExpshort$species == "albo-sinensis")] <- "albosinensis"
dataNoExpshort$species[which(dataNoExpshort$species == "VULGARIS")] <- "vulgaris"
dataNoExpshort$species[which(dataNoExpshort$species == "cf_alba")] <- "alba"
dataNoExpshort$species[which(dataNoExpshort$species == "PUBESCENS")] <- "pubescens"
sort(unique(dataNoExpshort$genus))
sort(unique(dataNoExpshort$species))

head(dataNoExpshort)
# can recombine genus and species if needed but ospree lists them as separate
dataNoExpshort$new.SpeciesName <- paste(dataNoExpshort$genus, dataNoExpshort$species, sep = "_")
sort(unique(dataNoExpshort$new.SpeciesName))

dataNoExpshort$new.SpeciesName[which(dataNoExpshort$new.SpeciesName == "Betula_")] <- "Betula_ermannii"

######################################################################################################
# Next removing gymnosperm because their traits are different from decidous species
# Removing the gymnosperm from the try data 
gymno<-c("Abies","Pinus","Picea","Pseudotsuga")

dataNoExpshort <- dataNoExpshort[!dataNoExpshort$genus %in% gymno,]
sort(unique(dataNoExpshort$new.SpeciesName))
# now have 94 species
#################################################################################################
# 
# Removing duplicate datasets that are also in Bien: 
#1. "Chave, J., D. Coomes, S. Jansen, S. L. Lewis, N. G. Swenson, and A. E. Zanne. 2009. Towards a world wide wood economics spectrum. Ecology Letters 12:351-366."
#2."Paine CET, Amissah L, Auge H, Baraloto C, Baruffol M, Bourland N, Bruelheide H, Dainou K, de Gouvenain RC, Doucet J-L, Doust SJ, Fine PV a, Fortunel C, Haase J, Holl KD, Jactel H, Li X, Kitajima K, Koricheva J, Martinez-Garza C, Messier C, Paquette A, Philipson CD, Piotto D, Poorter L, Posada JM, Potvin C, Rainio K, Russo SE, Ruiz-Jaen M, Scherer-Lorenzen M, Webb CO, Zahawi RA & Hector A (2015) Globally, functional traits are weak predictors of juvenile tree growth, and we do not know why. Journal of Ecology, 103, 978\u0096989. DOI: 10.1111/1365-2745.12401" 
#3."Spasojevic, M. J., Turner, B. L., and Myers, J. A. (2016) When does intraspecific trait variation contribute to functional beta?diversity? J Ecol, 104: 487-496. doi:10.1111/1365-2745.12518" 

dupstudies <- c("Chave, J., D. Coomes, S. Jansen, S. L. Lewis, N. G. Swenson, and A. E. Zanne. 2009. Towards a world wide wood economics spectrum. Ecology Letters 12:351-366.","Paine CET, Amissah L, Auge H, Baraloto C, Baruffol M, Bourland N, Bruelheide H, Dainou K, de Gouvenain RC, Doucet J-L, Doust SJ, Fine PV a, Fortunel C, Haase J, Holl KD, Jactel H, Li X, Kitajima K, Koricheva J, Martinez-Garza C, Messier C, Paquette A, Philipson CD, Piotto D, Poorter L, Posada JM, Potvin C, Rainio K, Russo SE, Ruiz-Jaen M, Scherer-Lorenzen M, Webb CO, Zahawi RA & Hector A (2015) Globally, functional traits are weak predictors of juvenile tree growth, and we do not know why. Journal of Ecology, 103, 978\u0096989. DOI: 10.1111/1365-2745.12401","Spasojevic, M. J., Turner, B. L., and Myers, J. A. (2016) When does intraspecific trait variation contribute to functional beta?diversity? J Ecol, 104: 487-496. doi:10.1111/1365-2745.12518") 
dataNoExpshort_nodup <- dataNoExpshort[!dataNoExpshort$Reference %in% dupstudies,]

length(unique(dataNoExpshort$Reference))
length(unique(dataNoExpshort_nodup$Reference))
#Looks good, removed just 3 studies
#################################################################################################
#

dataNoExpshort_es<-subset(dataNoExpshort_nodup, TraitName == "Leaf_nitrogen_.N._content_per_leaf_dry_mass"| TraitName == "Leaf_nitrogen_.N._content_per_leaf_dry_mass" | TraitName == "Specific_leaf_area"| TraitName == "Plant_height_vegetative"| TraitName == "Leaf_dry_matter_content" | TraitName == "Stem_diameter" | TraitName == "Stem_specific_density" | TraitName == "Leaf_photosynthesis_rate_per_leaf_area"| TraitName == "Leaf_carbon_.C._content_per_leaf_dry_mass") 

#################################################################################################
#Deirdre's summary notes:
# Final dataset dataNoExpshort
length(unique(dataNoExpshort_es$DatasetID)) #43
length(unique(dataNoExpshort_es$new.SpeciesName)) # 94
length(unique(dataNoExpshort_es$TraitName)) # 8 traits
# Interesting, we seem to have lost our photosyn data

sort(unique(dataNoExpshort$new.SpeciesName))

dataNoExpshort_es$project_pi<-paste(dataNoExpshort_es$FirstName,dataNoExpshort_es$LastName, sep=" ")

write.csv(dataNoExpshort_es, "input/try_subsptraits.csv", row.names=FALSE)
