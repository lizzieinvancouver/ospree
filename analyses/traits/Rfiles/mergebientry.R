# The aim of this code is to clean up the try and bien data and combine it into one useable file! There are many steps that need to be completed for this to happen:

#1. Fix typos try species names
#2. Remove columns in try that we don't need right now, there is tons of weather data and factors regarding site conditions
#3. Remove data from experiments where they manipulated factors that might change trails: fert additions, temp manip, chamber studies
#4. Check that the BIEN data is standardized in its trait units, lat/long
#5. Remove any dup data
#6. Subset to only deciduous species
#7. Subset to spp. with sufficient data, focus on leaf economic traits and SSD (one of the only wood economic traits)

rm(list=ls()) 
options(stringsAsFactors = FALSE)

require(stringr)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Desktop/ospree_trait_analysis")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

#Since the try data is still being cleaned, we are moving forward with the cleaning and plotting/preliminary testing of our hypotheses with the BIEN data
biendat<-read.csv("input/newspp_BIEN_traitdata_Nov11.csv", header=TRUE)

head(biendat)
length(unique(biendat$scrubbed_species_binomial))
#94 species represented in some form

trydat<-read.csv("input/TryDataCleaned.csv", header=TRUE)
#trydat<-read.csv("input/TryDataCleaned22012020.csv", header=TRUE)

head(trydat)
length(unique(trydat$SpeciesName))
#122 species represented...but I think some of these were typos

## Fixing species names ############################################################
trydat$SpeciesName[which(trydat$SpeciesName == "Acer pensilvanicum")] <- "Acer pensylvanicum"
trydat$SpeciesName[which(trydat$SpeciesName == "Rhamnus catharticus")] <- "Rhamnus cathartica"
trydat$SpeciesName[which(trydat$SpeciesName == "Quercus velutinam.")] <- "Quercus velutina"
trydat$SpeciesName[which(trydat$SpeciesName == "Quercus rubra I")] <- "Quercus rubra"
trydat$SpeciesName[which(trydat$SpeciesName == "Quercus shumardii Buckley")] <- "Quercus shumardii"
trydat$SpeciesName[which(trydat$SpeciesName == "Quercus ilex ilex")] <- "Quercus ilex"
trydat$SpeciesName[which(trydat$SpeciesName == "Pseudotsuga menziesii (Mirb.) Franco")] <- "Pseudotsuga menziesii" 
trydat$SpeciesName[which(trydat$SpeciesName == "Pseudotsuga menziesii var. menziesii")] <- "Pseudotsuga menziesii" 
trydat$SpeciesName[which(trydat$SpeciesName == "Prunus pensylvanica f.")] <- "Prunus pensylvanica"
trydat$SpeciesName[which(trydat$SpeciesName == "Quercus bicolor Willd.")] <- "Quercus bicolor"
trydat$SpeciesName[which(trydat$SpeciesName == "Alnus glutinosa (L.) Gaertn.")] <- "Alnus glutinosa"
trydat$SpeciesName[which(trydat$SpeciesName == "Betula pendula Roth")] <- "Betula pendula"
trydat$SpeciesName[which(trydat$SpeciesName == "Alnus incana (L.) Moench")] <- "Alnus incana"
trydat$SpeciesName[which(trydat$SpeciesName == "Facus grandifolia")] <- "Fagus grandifolia"
trydat$SpeciesName[which(trydat$SpeciesName == "Fagus grandfolia")] <- "Fagus grandifolia"
trydat$SpeciesName[which(trydat$SpeciesName == "Photinia melanocarpa (Michx.) K.R. Robertson & Phipps")] <- "Photinia melanocarpa"
trydat$SpeciesName[which(trydat$SpeciesName == "Populus grandidentata Michx.")] <- "Populus grandidentata"
trydat$SpeciesName[which(trydat$SpeciesName == "Prunus pennsylvanica")] <- "Prunus pensylvanica"
trydat$SpeciesName[which(trydat$SpeciesName == "Picea abies (L.) Karst.")] <- "Picea abies"
trydat$SpeciesName[which(trydat$SpeciesName == "Picea abies/obovata")] <- "Picea abies"
trydat$SpeciesName[which(trydat$SpeciesName == "Picea abies.x.obovata")] <- "Picea abies"
trydat$SpeciesName[which(trydat$SpeciesName == "Picea glauca (Moench) Voss")] <- "Picea glauca"
trydat$SpeciesName[which(trydat$SpeciesName == "Pieris japonica (Thunb.) D. Don ex G. Don")] <- "Pieris japonica"
trydat$SpeciesName[which(trydat$SpeciesName == "Pinus nigra Arnold")] <- "Pinus nigra"
trydat$SpeciesName[which(trydat$SpeciesName == "Pinus banksianamb.")] <- "Pinus banksiana"
trydat$SpeciesName[which(trydat$SpeciesName == "Pinus banksianana")] <- "Pinus banksiana"
trydat$SpeciesName[which(trydat$SpeciesName == "Sorbus decora (Sarg.) C.K. Schneid.")] <- "Sorbus decora"
trydat$SpeciesName[which(trydat$SpeciesName == "Rhododendron canadense (L.) Torr.")] <- "Rhododendron canadense"
trydat$SpeciesName[which(trydat$SpeciesName == "Prunus pensylvanica f.")] <- "Prunus pensylvanica"
trydat$SpeciesName[which(trydat$SpeciesName == "Pinus contorta Douglas exuden")] <- "Pinus contorta"
trydat$SpeciesName[which(trydat$SpeciesName == "Pinus contorta Douglas exuden var. contorta")] <- "Pinus contorta"
trydat$SpeciesName[which(trydat$SpeciesName == "Quercus velutinam.")] <- "Quercus velutina"
trydat$SpeciesName[which(trydat$SpeciesName == "Vaccinium myrtilLoides")] <- "Vaccinium myrtilloides"

trydat$SpeciesName<-str_replace(trydat$SpeciesName," (L.)","") 
trydat$SpeciesName<-str_replace(trydat$SpeciesName," L.","") 
trydat$SpeciesName<-str_replace(trydat$SpeciesName," Jacq.","") 
trydat$SpeciesName<-str_replace(trydat$SpeciesName," Ehrh.","") 
trydat$SpeciesName<-str_replace(trydat$SpeciesName," Marsh.","") 
trydat$SpeciesName<-str_replace(trydat$SpeciesName," Hook.","") 
trydat$SpeciesName<-str_replace(trydat$SpeciesName," Maxim.","") 

length(unique(trydat$SpeciesName))
# Once species names are fixed, try dataset only has 75 species

sort(unique(trydat$SpeciesName))
##################################################################################
names(trydat) # so many of these columns are not useful right now


trydat<-trydat[,c("SpeciesName","Dataset","Reference","Latitude","Longitude","Traits","TraitValue", "OrigUnitStr.y","UnitName","std_Latitude","std_Longitude","Std_Traits","TraitValue_std", "Exposition","Exposition.temperature","Leaf.exposition" )]

# Excluding experiment data
unique(trydat$Exposition) # I think we should subset out all experiments
# "Open Top" "open-top chamber"  "open-sided growth chamber" "forest fertilization" "Climate Chamber"

unique(trydat$Exposition.temperature) # Several studies manipulate temp
unique(trydat$Treatment..exposition.to.measurement.temperature.before.measurement) # All NA
unique(trydat$Treatment..period.of.darkness.before.respiration.measurement) # All NA

unique(trydat$Exposition..position.of.plant.in.the.canopy) # Variable, some numbers (5,6,7), bottom, middle etc., coments about understory

unique(trydat$Treatment.water.supply) # All NA #Intermediate, high, low
unique(trydat$Treatment.ozon) # high, low
unique(trydat$Treatment.conditions) # descriptions of experiments, 17 different comments, but includes an n/a and "no treatment"
unique(trydat$Treatment.CO2) # Various values, as well as "ambient (about 360ppm)" "actual" 
unique(trydat$Leaf.exposition) # not clear what these numbers represent
unique(trydat$Treatment.relative.humidity..Relative.humidity..) # All NA
unique(trydat$Treatment.plant.growth.temperature.during.night) # All NA
unique(trydat$Treatment.daylength) # All NA
unique(trydat$Treatment.plant.growth.temperature.during.day) # All NA
unique(trydat$Treatment.nutrient.solution.per.week) # a range of comments, including "Natural conditions", "none","None", "natural", and details of how much and when fertilizers were applied
unique(trydat$Treatment.K..potassium..supply) # All NA
unique(trydat$Treatment.P..phosphorus..supply) # All NA
unique(trydat$Treatment.nutrient.supply) # intermediate, low, high
unique(trydat$Treatment.light) # 2,1,0 no indication of what this represents
unique(trydat$Treatment.growth.medium...substrat...soil) # All NA

##################################################################################
#Changing names in BIEN to better match those in try
colnames(biendat)[colnames(biendat)=="scrubbed_species_binomial"] <- "SpeciesName"
colnames(biendat)[colnames(biendat)=="trait_name"] <-"Traits"
colnames(biendat)[colnames(biendat)=="trait_value"] <-"TraitValue" # Are the bien trait values standardized, something to check!! 
colnames(biendat)[colnames(biendat)=="unit"] <- "UnitName"
colnames(biendat)[colnames(biendat)=="longitude"] <- "Longitdue"
colnames(biendat)[colnames(biendat)=="latitude"] <- "Latitude"

##################################################################################
# But is the BIEN data standardized, ie lat long and trait units?
unique(biendat$unit)

# There are some none-standard units: 
gcm3<-subset(biendat, unit == "g.cm-3") # this study has stem wood density in cm
temp1<-subset(trydat, Traits == "Stem_specific_density") # this study has stem wood density in cm
# There are no units for the SSD or SWD from try, but the values are similar

m2kg<-subset(biendat, unit == "m2.kg-1") # this study has leaf area per day mass in m and kg! ie SLA
temp2<-subset(trydat, Traits == "Specific_leaf_area") 

mgg<-subset(biendat, unit == "mg.g-1") # this study has leaf dry mass per fresh mass in m and mg! ie LDMC
temp3<-subset(trydat, Traits == "Leaf_dry_matter_content") #standardized is g per g

# What about lat/long
unique(biendat$latitude)
unique(biendat$longitude)

unique(trydat$std_Latitude) 
unique(trydat$std_Longitude) 

# Are there any duplicates in the dataset

# To get the ball rolling on the analysis, we can start working with a still curated subset of the try data that does not include experiments and subsets species to those that have several functional traits

# Start by subsetting out studies that are growth chamber studies or experiments
trysub<-subset(trydat, Exposition == "Botanical garden"| Exposition == "Natural Vegetation"| Exposition == "natural vegetation, but not top canopy"| Exposition == "natural environment"| Exposition == "forest stand"| Exposition == "natural"| is.na(Exposition)) 

#removing a few others....still to do!
#trysub1<-subset(trysub, is.na(Treatment.water.supply)) 

#Other factors that should be subset
# unique(trydat$Treatment.water.supply)
# unique(trydat$Exposition..position.of.plant.in.the.canopy) # Variable, some numbers (5,6,7), bottom, middle etc., coments about understory
# unique(trydat$Treatment.water.supply) #Intermediate, high, low
# unique(trydat$Treatment.ozon) # high, low
# unique(trydat$Treatment.conditions) # descriptions of experiments, 17 different comments, but includes an n/a and "no treatment"
# unique(trydat$Treatment.CO2) # Various values, as well as "ambient (about 360ppm)" "actual" 
# unique(trydat$Leaf.exposition) # not clear what these numbers represent
# unique(trydat$Treatment.nutrient.solution.per.week) # a range of comments, including "Natural conditions", "none","None", "natural", and details of how much and when fertilizers were applied
# unique(trydat$Treatment.nutrient.supply) # intermediate, low, high
# unique(trydat$Treatment.light) # 2,1,0 no indication of what this represents

###########################################################################
#6. Remove conifer species - focus on just deciduous that have leaf trait data

# want to remove the Abies alba, Picea abies, all Pinus, Pseudotsuga menziesii

###########################################################################

#7. Subsetting the species to those that have the most trait data
# how many species remain in this subset dataset?
length(unique(trysub$SpeciesName)) 

# What traits?
unique(trysub$Traits)

library(dplyr)

table<- trysub1 %>%
  group_by(SpeciesName,Traits) %>%
  summarise(no_rows = length(Traits))
table

# At minimum, I think we want species with SLA, LDMC, LNC, LCC, height, stem.diameter/DBH
trysubtrait<-subset(trydat, Traits == "Leaf_nitrogen_.N._content_per_leaf_dry_mass"| Traits == "Leaf_nitrogen_.N._content_per_leaf_dry_mass" | Traits == "Specific_leaf_area"| Traits == "Plant_height_vegetative"| Traits == "Leaf_dry_matter_content" | Traits == "Stem_diameter" | Traits == "Stem_specific_density" | Traits == "Leaf_photosynthesis_rate_per_leaf_area") 

table<- trysubtrait %>%
  group_by(SpeciesName,Traits) %>%
  summarise(no_rows = length(Traits))
table

table<- trysubtrait %>%
  group_by(Traits, SpeciesName) %>%
  summarise(no_rows = length(Traits))
table

length(unique(trysubtrait$SpeciesName))
unique(trysubtrait$TraitValue_std)





#Faith's subsetting code - The end goal might be to move thsi to teh try cleaning code?
#----------------------------------------

#Remove columns with NA only in them
data2 <- trydat[colSums(!is.na(trydat)) > 0]
data[1:2,2:5]

#Get rid of rows of experimental data
unique(data$Exposition) # I think we should subset out all experiments
expList <- c( "Open Top", "open-top chamber",  "open-sided growth chamber", "forest fertilization",
 "Climate Chamber", "FACE",  "mini-ecosystem")#We dont know what mini-ecosystem or face is so removing them 

unique(data$Exposition[!data$Exposition %in% expList]) 
data3 <- data2[!data2$Exposition %in% expList,]
# Start by subsetting out studies that are growth chamber studies or experiments

unique(data3$Exposition.temperature) 
data4 <- data3[is.na(data3$Exposition.temperature),] # Remove studies with an exposition temperature

unique(data4$Exposition..position.of.plant.in.the.canopy) # 
plnList <- c("7"  ,"6"  , "5", "bottom", "middle" , "top" ) # maybe these are experiments?
data5 <- data4[!data4$Exposition..position.of.plant.in.the.canopy %in% plnList,]

unique(data$Treatment.water.supply) # All NA #Intermediate, high, low. Just chose NA values. 
data6 <- data5[is.na(data5$Treatment.water.supply),] 

unique(data$Treatment.ozon) # high, low. chose na rows 
data7 <- data6[is.na(data6$Treatment.ozon),]

table(data8$DatasetID)

unique(data7$Treatment.conditions) #by this point in the cleaning there are only NAs

unique(data7$Treatment.CO2) 
table(data7$Treatment.CO2)#Remove all rows where they specify elevation 
data8 <- data7[is.na(data7$Treatment.CO2),]

unique(data8$Leaf.exposition) #Not an issue after previous cleaning

unique(data8$Treatment.relative.humidity..Relative.humidity..) # All NA

unique(data8$Treatment.plant.growth.temperature.during.night)  #Not an issue after previous cleaning

unique(data8$Treatment.daylength) 

unique(data8$Treatment.nutrient.solution.per.week) #Not an issue after previous cleaning

unique(data8$Treatment.K..potassium..supply) # All NA

unique(data8$Treatment.P..phosphorus..supply) # All N

unique(data8$Treatment.nutrient.supply) # intermediate, low, high

unique(data8$Treatment.light) # 2,1,0 no indication of what this represents

unique(data8$Treatment.growth.medium...substrat...soil) # All NA



#Get rid of climate columns 


#Columns to remove
#----------------------------

#climate columsn - precipitation 
names(data8)

precipNames <- grep( "precip", names(data2),  value = TRUE)
evapotransNames <-  grep( "evapotranspiration", names(data2),  value = TRUE)
radNames <-  grep( "radiation", names(data2),  value = TRUE)
tempNames <-  grep( "temperature", names(data2),  value = TRUE)
soilNames <-  grep( "oil", names(data2),  value = TRUE)
herbNames <-  grep( "erbivory", names(data2),  value = TRUE)
vpdNames <- grep("VPD", names(data2), value = TRUE)
seedNames <- grep("Seed", names(data2), value= TRUE)
dateNames <- grep("Measurement.date", names(data2), value= TRUE)
ageNames <- grep("age", names(data2), value= TRUE)
expNames <- grep("xposition", names(data2), value= TRUE)
treatNames <- grep("Treatment.", names(data2), value= TRUE)
litterNames <- grep("litter", names(data2), value = TRUE)

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


namesRemove <- c(expNames,treatNames, dateNames, ageNames, seedNames, otherNames, litterNames, vpdNames, evapotransNames, precipNames,radNames, tempNames, soilNames, herbNames)


colNamesNoClim <- names(data2)[!names(data2) %in% namesRemove]



dataNoExpshort <- data8 [,colNamesNoClim]

