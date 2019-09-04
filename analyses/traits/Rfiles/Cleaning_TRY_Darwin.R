# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/OSPREE/sep_data1/")

library(data.table)
library(tidyverse)
library(dplyr)
library(readr)
library(reshape2)
library(plyr)
library(janitor)

files <- as.character(list.files(path="~/Documents/Ph.D/OSPREE/sep_data1/"))
readLines(paste("~/Documents/Ph.D/OSPREE/sep_data1/",.Platform$file.sep,files[1],sep=""))

###Abisko_SheffieldDatabase 
####Now, using reshape2, convert it from long to wide formatted data
Abisko_SheffieldDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/Abisko_SheffieldDatabase.csv")
Abiskowide<- dcast(Abisko_SheffieldDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

###There are 36 variables, many of which are weather eg rel humidity for ea month, sum of precip for ea month
##Subsetting just the trait/site data
tmp1<-Abiskowide[,c(1:7,13, 15, 19, 24:25)]
str(tmp1)
names(tmp1)

###Writing CSV
write.csv(tmp1, file="Abisko_SheffieldDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
###AllometricCoefficientsofAbovegroundtreebiomass
####Now, using reshape2, convert it from long to wide formatted data
AllometricCoefficientsofAbovegroundTreeBiomass <- read_csv("Documents/Ph.D/OSPREE/sep_data/AllometricCoefficientsofAbovegroundTreeBiomass.csv")
Allometricwide <- dcast(AllometricCoefficientsofAbovegroundTreeBiomass, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp2<- Allometricwide[,c(1:6, 10:13, 17)]

###Writing CSV
write.csv(tmp2, file="AllometricCoefficientsofAbovegroundTreeBiomass.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
###AltitudinalVicariantsSpain
####Now, using reshape2, convert it from long to wide formatted data
AltitudinalVicariantsSpain <- read_csv("Documents/Ph.D/OSPREE/sep_data/AltitudinalVicariantsSpain.csv")
Altitudinalwide <- dcast(AltitudinalVicariantsSpain, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp3<- Altitudinalwide[,c(1:7, 10:11, 15:17, 19)]

###Writing CSV
write.csv(tmp3, file="AltitudinalVicariantsSpain.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
###Baad_abiomassandallometrydatabase
BAAD_abiomassandallometrydatabaseforwoodyplants <- read_csv("Documents/Ph.D/OSPREE/sep_data/BAAD_abiomassandallometrydatabaseforwoodyplants.csv")
BAADwide <- dcast(BAAD_abiomassandallometrydatabaseforwoodyplants, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp4<- BAADwide[,c(1:6, 52, 54, 60, 63, 64, 67:70)]

###Writing CSV
write.csv(tmp4, file="BAAD_abiomassandallometrydatabaseforwoodyplants.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
###Baccara_plantTraitsofEuropeanForests
Baccara_PlantTraitsofEuropeanForests <- read_csv("Documents/Ph.D/OSPREE/sep_data/Baccara-PlantTraitsofEuropeanForests.csv")
Baccarwide <- dcast(Baccara_PlantTraitsofEuropeanForests, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp5<- Baccarwide[,c(1:5, 7:11)]

###Writing CSV
write.csv(tmp5, file="Baccara_PlantTraitsofEuropeanForests.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
###BASECO_afloristicandecoloicaldatabaseofMediterraneanFrenchFlowers
BASECO_afloristicandecologicaldatabaseofMediterraneanFrenchflora <- read_csv("Documents/Ph.D/OSPREE/sep_data/BASECO_afloristicandecologicaldatabaseofMediterraneanFrenchflora.csv")
BASECO_wide <- dcast(BASECO_afloristicandecologicaldatabaseofMediterraneanFrenchflora, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp6<- BASECO_wide[,c(1:3, 5:7)]

###Writing CSV
write.csv(tmp6, file="BASECO_afloristicandecologicaldatabaseofMediterraneanFrenchflora.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
##no actual trait values
###BASECO_afloristicandecoloicaldatabaseofMediterraneanFrenchFlowers
BBB_AglobalBelowgroundBudBankdatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/BBB-AglobalBelowgroundBudBankdatabase.csv")
BBB_wide <- dcast(BBB_AglobalBelowgroundBudBankdatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp7<- BBB_wide[,c(1:2, 5, 10,12)]

###Writing CSV
write.csv(tmp7, file="BBB_AglobalBelowgroundBudBankdatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
Biomassallocationinbeechandspruceseedlings <- read_csv("Documents/Ph.D/OSPREE/sep_data/Biomassallocationinbeechandspruceseedlings.csv")
Biomass_wide <- dcast(Biomassallocationinbeechandspruceseedlings, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp8<- Biomass_wide[,c(1:5, 9, 11:12)]

###Writing CSV
write.csv(tmp8, file="Biomassallocationinbeechandspruceseedlings.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
BIOME_BGCParameterizationDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/BIOME-BGCParameterizationDatabase.csv")
Biome_wide <- dcast(BIOME_BGCParameterizationDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp9<- Biome_wide[,c(1:7)]

###Writing CSV
write.csv(tmp9, file="BIOME_BGCParameterizationDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
BIOTREETraitShadeExperiment <- read_csv("BIOTREETraitShadeExperiment.csv")

##Subsetting just the trait/site data
tmp10<- BIOTREETraitShadeExperiment[,c(2:6)]

###Writing CSV
write.csv(tmp10, file="BIOTREETraitShadeExperiment.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
BROTPlantTraitDatabase <- read_csv("~/Documents/Ph.D/OSPREE/sep_data/BROTPlantTraitDatabase.csv")
BROPlant_wide <- dcast(BROTPlantTraitDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp11<- BROPlant_wide[,c(1:5, 10:11)]

###Writing CSV
write.csv(tmp11, file="BROTPlantTraitDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
CanopyTraitsforTemperateTreeSpeciesUnderHighN_Deposition <- read_csv("Documents/Ph.D/OSPREE/sep_data/CanopyTraitsforTemperateTreeSpeciesUnderHighN-Deposition.csv")
Canopy_wide <- dcast(CanopyTraitsforTemperateTreeSpeciesUnderHighN_Deposition, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp12<- Canopy_wide[,c(1:5,8:10, 12, 14:16)]

###Writing CSV
write.csv(tmp12, file="CanopyTraitsforTemperateTreeSpeciesUnderHighN_Deposition.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
###No trait values in this dataset
CategoricalPlantTraitsDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/CategoricalPlantTraitsDatabase.csv")
Categorical_wide <- dcast(CategoricalPlantTraitsDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp13<- Categorical_wide[,c(1:8)]

###Writing CSV
write.csv(tmp13, file="CategoricalPlantTraitsDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
ChineseTraits <- read_csv("Documents/Ph.D/OSPREE/sep_data/ChineseTraits.csv")
Chinese_wide <- dcast(ChineseTraits, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp14<- Chinese_wide[,c(1:5, 8:17)]

###Writing CSV
write.csv(tmp14, file="ChineseTraits.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
ColdTolerance_SeedSizeandHeightofNorthAmericanForestTreeSpecies <- read_csv("Documents/Ph.D/OSPREE/sep_data/ColdTolerance,SeedSizeandHeightofNorthAmericanForestTreeSpecies.csv")
ColdTolerance_wide <- dcast(ColdTolerance_SeedSizeandHeightofNorthAmericanForestTreeSpecies, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp15<- ColdTolerance_wide[,c(1:7)]

###Writing CSV
write.csv(tmp15, file="ColdTolerance_SeedSizeandHeightofNorthAmericanForestTreeSpecies.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
### This dataset is the same as above
ColdTolerance <- read_csv("Documents/Ph.D/OSPREE/sep_data/ColdTolerance.csv")
Cold_wide <- dcast(ColdTolerance, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp16<- Cold_wide[,c(1:7)]

###Writing CSV
write.csv(tmp16, file="ColdTolerance.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
DayandNightGasExchangeofDeciduousTreeSeedlingsinResponsetoExperimentalWarmingandPreci <- read_csv("Documents/Ph.D/OSPREE/sep_data/DayandNightGasExchangeofDeciduousTreeSeedlingsinResponsetoExperimentalWarmingandPreci.csv")
DayandNight_wide <- dcast(DayandNightGasExchangeofDeciduousTreeSeedlingsinResponsetoExperimentalWarmingandPreci, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp17<- DayandNight_wide[,c(1:5, 17, 20, 24, 39, 46:49)]

###Writing CSV
write.csv(tmp17, file="DayandNightGasExchangeofDeciduousTreeSeedlingsinResponsetoExperimentalWarmingandPreci.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
ECOCRAFT <- read_csv("Documents/Ph.D/OSPREE/sep_data/ECOCRAFT.csv")
ECO_wide <- dcast(ECOCRAFT, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp18<- ECO_wide[,c(1:5, 7:8, 11, 18:21, 23, 38:40, 42:43)]

###Writing CSV
write.csv(tmp18, file="ECOCRAFT.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
EcologicalFloraoftheBritishIsles <- read_csv("Documents/Ph.D/OSPREE/sep_data/EcologicalFloraoftheBritishIsles.csv")
EcologicalFlora_wide <- dcast(EcologicalFloraoftheBritishIsles, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp19<- EcologicalFlora_wide[,c(1:12)]

###Writing CSV
write.csv(tmp19, file="EcologicalFloraoftheBritishIsles.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
FloridianLeafTraitsDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/FloridianLeafTraitsDatabase.csv")
Floridian_wide <- dcast(FloridianLeafTraitsDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp20<- Floridian_wide[,c(1:5, 9, 12, 14)]

###Writing CSV
write.csv(tmp20, file="FloridianLeafTraitsDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
FunctionalResilienceofTemperateForestsDataset <- read_csv("~/Documents/Ph.D/OSPREE/sep_data/FunctionalResilienceofTemperateForestsDataset.csv")
FunctionalR_wide <- dcast(FunctionalResilienceofTemperateForestsDataset, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp21<- FunctionalR_wide[,c(1:5, 7:8, 10)]

###Writing CSV
write.csv(tmp21, file="FunctionalResilienceofTemperateForestsDataset.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
Functionaltraitsexplainingvariationinplantlifehistorystrategies <- read_csv("~/Documents/Ph.D/OSPREE/sep_data/Functionaltraitsexplainingvariationinplantlifehistorystrategies.csv")
FunctionalT_wide <- dcast(Functionaltraitsexplainingvariationinplantlifehistorystrategies, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
FunctionalT_wide[,7] <- 5.5
tmp22<- FunctionalT_wide[,c(1:8)]

###Writing CSV
write.csv(tmp22, file="Functionaltraitsexplainingvariationinplantlifehistorystrategies.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
FunctionalTraitsofTrees <- read_csv("~/Documents/Ph.D/OSPREE/sep_data/FunctionalTraitsofTrees.csv")
FunctionalTr_wide <- dcast(FunctionalTraitsofTrees, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp23<- FunctionalTr_wide[,c(1:5, 13:14)]

###Writing CSV
write.csv(tmp23, file="FunctionalTraitsofTrees.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
GlobalLeafGasExchangeDatabase_I_ <- read_csv("Documents/Ph.D/OSPREE/sep_data/GlobalLeafGasExchangeDatabase(I).csv")
Global1_wide <-dcast(GlobalLeafGasExchangeDatabase_I_, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp24<- Global1_wide[,c(1:5, 16, 18:19, 23, 27, 28, 30, 34)]

###Writing CSV
write.csv(tmp24, file="GlobalLeafGasExchangeDatabase_I_.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
GlobalLeafGasExchangeDatabase_II_ <- read_csv("Documents/Ph.D/OSPREE/sep_data/GlobalLeafGasExchangeDatabase(II).csv")
Global2_wide <- dcast(GlobalLeafGasExchangeDatabase_II_, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp25<- Global2_wide[,c(1:5, 16, 19, 23, 25, 28)]

###Writing CSV
write.csv(tmp25, file="GlobalLeafGasExchangeDatabase_II_.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
GlobalRespirationDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/GlobalRespirationDatabase.csv")
Globalres_wide <-dcast(GlobalRespirationDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp26<- Globalres_wide[,c(1:5, 7, 12, 18, 24, 26, 27)]

###Writing CSV
write.csv(tmp26, file="GlobalRespirationDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
GlobalWoodDensityDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/GlobalWoodDensityDatabase.csv")
Globalwood_wide <-dcast(GlobalWoodDensityDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp27<- Globalwood_wide[,c(1:5, 7:8)]

###Writing CSV
write.csv(tmp27, file="GlobalWoodDensityDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
GLOPNET_GlobalPlantTraitNetworkDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/GLOPNET-GlobalPlantTraitNetworkDatabase.csv")
GLOPNET_wide <-dcast(GLOPNET_GlobalPlantTraitNetworkDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp28<- GLOPNET_wide[,c(1:5, 7, 12, 13, 16, 65, 66, 70, 72)]

###Writing CSV
write.csv(tmp28, file="GLOPNET_GlobalPlantTraitNetworkDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
GrowthandHerbivoryofJuvenilTrees <- read_csv("Documents/Ph.D/OSPREE/sep_data/GrowthandHerbivoryofJuvenilTrees.csv")
Growth_wide <-dcast(GrowthandHerbivoryofJuvenilTrees, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp29<- Growth_wide[,c(1:5, 7, 8:13, 16, 21)]

###Writing CSV
write.csv(tmp29, file="GrowthandHerbivoryofJuvenilTrees.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
HerbaceousplantsofRougeNationalUrbanPark <- read_csv("Documents/Ph.D/OSPREE/sep_data/HerbaceousplantsofRougeNationalUrbanPark.csv")
Herbaceousplants_wide <-dcast(HerbaceousplantsofRougeNationalUrbanPark, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)

##Subsetting just the trait/site data
Herbaceousplants_wide[,7] <- col.name
colnames(Herbaceousplants_wide)[which(names(Herbaceousplants_wide) == "258")] <- "Leaf dry mass per leaf fresh mass (leaf dry matter content)"
tmp30<- Herbaceousplants_wide[,c(1:5, 7, 9, 12)]

###Writing CSV
write.csv(tmp30, file="HerbaceousplantsofRougeNationalUrbanPark.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
ItalianAlpsPlantTraitsDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/ItalianAlpsPlantTraitsDatabase.csv")
Italian_wide <- dcast(ItalianAlpsPlantTraitsDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp31<- Italian_wide[,c(1:5, 8)]

###Writing CSV
write.csv(tmp31, file="ItalianAlpsPlantTraitsDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
JasperRidgeCalifornianWoodyPlantsDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/JasperRidgeCalifornianWoodyPlantsDatabase.csv")
JasperRidge_wide <- dcast(JasperRidgeCalifornianWoodyPlantsDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp32<- JasperRidge_wide[,c(1:5, 7:9, 11:12)]

###Writing CSV
write.csv(tmp32, file="JasperRidgeCalifornianWoodyPlantsDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
###error in cleaning code
LCE_Leafcarbonexchangedatasetfortropical <- read_csv("Documents/Ph.D/OSPREE/sep_data/LCE_Leafcarbonexchangedatasetfortropical.csv")
LCE_wide <-dcast(LCE_Leafcarbonexchangedatasetfortropical, LastName+FirstName+DatasetID+Dataset+SpeciesName~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp33<- LCE_wide[,c(1:13)]

###Writing CSV
write.csv(tmp33, file="LCE_Leafcarbonexchangedatasetfortropical.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
###issue with cleaning 
LeafandWholePlantTraitsDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/LeafandWholePlantTraitsDatabase.csv")
Leafand_wide <-dcast(LeafandWholePlantTraitsDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp34<- Leafand_wide[,c(1:2, 4:5, 9, 12:14, 16:23)]

###Writing CSV
write.csv(tmp34, file="LeafandWholePlantTraitsDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
LeafPhotosynthesisandNitrogenatOakRidgeDataset <- read_csv("Documents/Ph.D/OSPREE/sep_data/LeafPhotosynthesisandNitrogenatOakRidgeDataset.csv")
LeafPhotosynthesis_wide <-dcast(LeafPhotosynthesisandNitrogenatOakRidgeDataset, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp35<- LeafPhotosynthesis_wide[,c(1:5, 7:11, 13)]

###Writing CSV
write.csv(tmp35, file="LeafPhotosynthesisandNitrogenatOakRidgeDataset.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
LeafPhysiologyDatabase <- read_csv("Documents/Ph.D/OSPREE/sep_data/LeafPhysiologyDatabase.csv")
LeafPhysiology_wide <-dcast(LeafPhysiologyDatabase, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp36<- LeafPhysiology_wide[,c(1:5,8, 14, 18, 27, 29:30, 33)]

###Writing CSV
write.csv(tmp36, file="LeafPhysiologyDatabase.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
LeafStructure_VenationandEconomicSpectrum <- read_csv("Documents/Ph.D/OSPREE/sep_data/LeafStructure,VenationandEconomicSpectrum.csv")
LeafStructure_wide <-dcast(LeafStructure_VenationandEconomicSpectrum, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)

##Subsetting just the trait/site data
tmp37<- LeafStructure_wide[,c(1:5, 8:9)]

###Writing CSV
write.csv(tmp37, file="LeafStructure_VenationandEconomicSpectrum.csv", row.names = TRUE)

##############################
#loop for cleaning
##############################
df <- list.files(full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

##try this code
filenames <- list.files(path="~/Documents/Ph.D/OSPREE/sep_data1/", full.names=TRUE)
import.list <- llply(filenames, read.csv)

data <- Reduce(function(x, y) merge(x, y, all=T,
by=colnames(import.list[1])), import.list, accumulate=F)
#######

myfiles = list.files(path="~/Documents/Ph.D/OSPREE/sep_data1/", pattern="*.csv", full.names=TRUE)
myfiles
dat_clean = ldply(myfiles, read_csv)
dat_clean

Trydata <- dcast(dat_clean, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+unique(ObsDataID)~DataName, value.var = "OrigValueStr", na.rm=TRUE)
unique(Trydata$`Exposition light / irradiance`)

Trydata %>% 
  summarise_all((funs(sum(is.na(.)))))
