#Code writen by Faith after teh 2020 Ospree retreat, where the try dataset is cleaned and stripped down to only data we care about. 
#No experiments
#Got rid of most of teh non trait info, but kept things like location and reference info

#Date started Nov 5 2020 


rm(list = ls())
## No one likes factors
options(stringsAsFactors = FALSE)


setwd("/home/faith/Documents/mnt/UBC/ospree")

## Load libraries
library(tidyr)
library(dplyr)
library(data.table)

## Read the data (modify path as needed) 
#tryData <- fread("~/Downloads/TRYtraitdataNov2019.txt")
tryData2 <- fread("TryData.txt")
tryData2$counterID <- 1:nrow(tryData2)

#-----------------------------------------------------------------
#faith's Cleaning code
#--------------------------------------------------------------

#Remove rows with irrelevant information to make things easier to cope with 
precipNames <- grep( "precip", unique(tryData2$DataName),  value = TRUE)
evapotransNames <-  grep( "evapotranspiration", unique(tryData2$DataName),  value = TRUE)
radNames <-  grep( "radiation", unique(tryData2$DataName),  value = TRUE)
tempNames <-  grep( "temperature", unique(tryData2$DataName),  value = TRUE)
soilNames <-  grep( "oil", unique(tryData2$DataName),  value = TRUE)
herbNames <-  grep( "erbivory", unique(tryData2$DataName),  value = TRUE)
vpdNames <- grep("VPD", unique(tryData2$DataName), value = TRUE)
seedNames <- grep("Seed", unique(tryData2$DataName), value= TRUE)
dateNames <- grep("Measurement.date", unique(tryData2$DataName), value= TRUE)
ageNames <- grep("age",unique(tryData2$DataName), value= TRUE)
litterNames <- grep("litter", unique(tryData2$DataName), value = TRUE)

otherNames <- c("Soil.carbon.nitrogen..C.N..ratio", "Plant.developmental.status...plant.age...maturity...plant.life.stage",
	"Plant.environment" ,"Altitude.of.provenance.of.litter" , "Climate.zone.of.provenance.of.litter"      ,
	"Nitrogen.deposition.at.the.site", "Ecocraft.regression.ID", "Nitrogen.mineralisation.rate",
	"Plant.cover", "Plot.ID", "Atmospheric.CO2.concentration.during.measurement..Ca.",
	"Light.during.measurement" , "Phylogenetically.isolated...not.isolated.individuals",
	"Annual.moisture.balance" ,  "Location...Site.Name" , "Moisture.balance.code",
	"Moisture.balance.during.growth.season", "Radiation.classes.1", "Radiation.classes.2",
	"Temperature.during.respiration.measurements", "Altitude.comments", 
	"Plant.height.reference", "Altitude.comments" , "Plant.height.reference", "Stocking",
	"Slope.of.site", "Number.of.replicates", "Provenance.of.species", "Temperature.during.measurement", 
	"Leaf.area.index.of.the.site..LAI.", "Plant.longevity.reference",    "Method.by.which.dispersal.syndrom.was.acertained",
	"Reference.for.dispersal",  "O2.concentration.during.measurement", "Canopy.height.observed" ,
	 "Number.of.tree.rings.visible.in.core",  "Height.of.measurement..stem.diameter..tree.rings..bark.thickness" ,
	  "Age.of.the.stand" , "Analysis.ID.in.Kattge.Leaf.Physiology" , "Plant.growth.form.reference" ,  "Rooting.Volume..m3.", 
	  "Ecocraft.parameter.value.ID"  , "Sampling.date..year"  ,  "Site.burned.year"  , 
	        "Net.primary.productivity.of.the.site..NPP."  , "Vegetation.type...Biome" , "Species.phylogenic.group" ,
	        "Canopy.position..sun.vers..Shade.leaf.qualifier..light.exposure" ,  "Description.of.chamber",
	         "Vegetation.type...Biome...2." ,  "Pests.and.treatmens" )

valuesRemove <- c( dateNames, ageNames, seedNames, otherNames, 
	litterNames, vpdNames, evapotransNames, precipNames,radNames, tempNames, soilNames, herbNames)

head(tryDataShort)


tryDataShort <- tryData2[!tryData2$DataName %in% valuesRemove,]

#Remove experimental studies by removing observations with information pertaining to treatments or expositions
expNames <- grep("xposition",unique(tryData2$DataName), value= TRUE)
expNamesShort <- expNames[!expNames == "Exposition"] # keep "EXPOSITION" becasuse not all of these descriptions mean an experiment. SOme are in botanic gardens or are described as natural 
treatNames <- grep("Treatment.", unique(tryData2$DataName), value= TRUE)

experimentsObIDs <- tryDataShort$ObservationID[tryDataShort$DataName %in% c(expNames,treatNames)]
tryDataShort2 <- tryDataShort[!tryDataShort$ObservationID %in% experimentsObIDs,]

expositionOptions <- unique(tryData2$OrigValueStr[tryData2$DataName=="Exposition"])#Get a list of all exposition comment options
#I chose to keep studies where thety are described  as in a forest, a garden or a natural system
naturalNames  <- grep("atural", expositionOptions, value = TRUE)
forestNames <- grep("orest", expositionOptions, value = TRUE)
gardenNames <- grep("arden", expositionOptions, value = TRUE)

nonExperiments <- c(naturalNames, forestNames, gardenNames)
experiments2 <- expositionOptions[!expositionOptions %in% nonExperiments]

naturalObservations2 <- tryData2$ObservationID[!tryData2$OrigValueStr %in% experiments2] # Obseravtion ids for experimental systems 
tryDataShort3 <- tryDataShort2[tryDataShort2$ObservationID %in% naturalObservations2,]

head(tryDataShort3)


#Get the extra data out of the Traits column 
#------------------------------------------------



#Select only traits, not extra info
traitsOnly <- tryDataShort3[!is.na(tryDataShort3$TraitID),]

#Make a trail/observation ID column 
traitsOnly$Observation_TraitID <- paste(traitsOnly$ObservationID, traitsOnly$TraitID, sep = "_")



#Select only traits, not extra info
ExtraInfo <- tryDataShort3[is.na(tryDataShort3$TraitID),]

#Select only columns for exstra data
ExtraInfoColumns<- ExtraInfo[,c("DataName", "ObservationID", "OrigValueStr")]

#These are the information I think might be useful to retain 
dataCare <- c("Reference / source" ,"Latitude",  "Longitude" , "Comments, notes, methods", "Location Site ID", "Altitude"  ,  
	"Data type" , "Sample size" , "Location Name"  , "Reference 2", "Part of plant measured" , "Study ID; external Dataset ID ")

ExtraInfoColumns2 <- ExtraInfoColumns[ExtraInfoColumns$DataName %in% dataCare, ]

#Change these extra data to a wide format for each category in dataCare
wideExtraInfo <- spread(ExtraInfoColumns2, key = DataName, value = OrigValueStr)

#Merge in extra data to trait data based on observation ID
#---------------------------------------------------------
tryData <- merge(traitsOnly, wideExtraInfo, by = "ObservationID")


#---------------------------------------------------
#Geoff's cleaning code
#--------------------------------------------------


## Drop some columns to keep things simpler
tryData$V28 <- NULL
tryData$Comment <- NULL
tryData$ObsDataID <- NULL

## What traits do we have data on?
traitN <- unique(tryData$TraitName)
traitN
## Let's clean this up a bit
### Drop rows where TraitName is blank (these are location or reference data; these can be merged later)
tryData <- subset(tryData, TraitName != "")
### Skip plant biomass data (only 104 measurements)
tryData <- subset(tryData, TraitName != c("Plant biomass and allometry: Leaves per plant (emergent, mature, senescent)"))
### Lump together all SLA measurements
tryData$TraitName <- gsub(pattern = "Leaf area per leaf dry mass.*uded", replacement = "Specific_leaf_area", x = tryData$TraitName)
### Lump together all Leaf area measurements
tryData$TraitName <- gsub(pattern = "Leaf area .*)", replacement = "Leaf_area", x = tryData$TraitName)
### Rename Stem specific density
tryData$TraitName <- gsub(pattern = "Stem specific density.*)", replacement = "Stem_specific_density", x = tryData$TraitName)
### Rename Leaf dry mass per leaf
tryData$TraitName <- gsub(pattern = "Leaf dry mass per leaf.*)", replacement = "Leaf_dry_matter_content", x = tryData$TraitName)
### Rename Crown height
tryData$TraitName <- gsub(pattern = "Crown.*)", replacement = "Crown_height", x = tryData$TraitName)

#remove any other symbols that might caouse problems later 
tryData$TraitName  <- gsub( " ", "_", tryData$TraitName )
tryData$TraitName  <- gsub( "\\(", ".", tryData$TraitName )
tryData$TraitName  <- gsub( "\\)", ".", tryData$TraitName )
tryData$TraitName <- gsub( "\\:", ".", tryData$TraitName )
tryData$TraitName <- gsub( ",", ".", tryData$TraitName )


### Now how we do look?
traitN <- unique(tryData$TraitName)
traitN # much nicer

## What type of trait values do we have?
traitT <- unique(tryData$ValueKindName)
traitT
### How many measurements for each one?
aggregate(StdValue ~ ValueKindName, data = tryData, FUN = length)

### Keep only "Single"
#tryData <- subset(tryData, ValueKindName == c("Single"))

## Keep only data with standardized values (StdValue)
#tryData <- tryData[!is.na(tryData$StdValue), ]

## What kind of units are we dealing with?
aggregate(UnitName ~ TraitName, data = tryData, unique) # should only be 1 per TraitName
### Remove "g/m2/d" measurements for Leaf photosynthesis (only 81 measurements)
tryData <- subset(tryData, UnitName != c("g/m2/d"))
aggregate(UnitName ~ TraitName, data = tryData, unique) # much better

## Generate sample plots of key traits (hopefully not much variation)
testspecies <- subset(tryData, SpeciesName == c("Abies alba"))
trait.list <- c("Stem specific density", "Leaf dry matter content", "Crown height", "Specific leaf area")
par(mfrow = c(2, 2), mar = c(5, 5, 2, 1))
for(i in 1:length(trait.list)){
    temp <- subset(testspecies, TraitName == trait.list[i])
    plot(temp$StdValue, main = trait.list[i], xlab = "Observation", ylab = "Trait value")
}




#Save Data as a csv
#--------------------------------------------

write.csv(tryData, "TryDataCleanedNew.csv")

